{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Api where

import Import
import Handler.Api.Shared
import qualified Data.Char as Char
import qualified Data.Aeson.KeyMap as KM
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Database.Persist.Sql (fromSqlKey)
import Data.Time (addUTCTime)
import Yesod.Auth (Creds (..), clearCreds, setCreds)
import Yesod.Auth.HashDB (setPassword, validatePass)

postApiAuthLoginR :: Handler Value
postApiAuthLoginR = do
    identInput <- runInputPost $ fromMaybe "" <$> iopt textField "ident"
    usernameInput <- runInputPost $ fromMaybe "" <$> iopt textField "username"
    password <- runInputPost $ fromMaybe "" <$> iopt textField "password"
    let ident = T.strip $ if T.strip identInput /= "" then identInput else usernameInput
    when (ident == "") $
        apiError status400 "Username is required."
    when (T.strip password == "") $
        apiError status400 "Password is required."
    Entity userId user <- runDB $ getBy404 $ UniqueUser ident
    case validatePass user password of
        Just True -> do
            setCreds False $ Creds "api" ident []
            returnJson $ object
                [ "authenticated" .= True
                , "user" .= userValue user
                , "userId" .= fromSqlKey userId
                ]
        _ ->
            apiError status400 "Invalid username or password."

postApiAuthRegisterR :: Handler Value
postApiAuthRegisterR = do
    ident <- runInputPost $ ireq textField "ident"
    password <- runInputPost $ ireq textField "password"
    passwordConfirm <- runInputPost $ ireq textField "passwordConfirm"
    rawDisplayName <- runInputPost $ fromMaybe "" <$> iopt textField "displayName"
    rawBio <- runInputPost $ fromMaybe "" <$> iopt textField "bio"
    existingUser <- runDB $ getBy $ UniqueUser ident
    let trimmedIdent = T.strip ident
        trimmedDisplayName = T.strip rawDisplayName
        trimmedBio = T.strip rawBio
    when (trimmedIdent == "") $
        apiError status400 "Username is required."
    when (isJust existingUser) $
        apiError status400 "That username is already taken."
    when (length password < 8) $
        apiError status400 "Password must be at least 8 characters."
    when (password /= passwordConfirm) $
        apiError status400 "Passwords do not match."
    let baseUser =
            User
                { userIdent = trimmedIdent
                , userPassword = Nothing
                , userDisplayName =
                    if trimmedDisplayName == ""
                        then Nothing
                        else Just trimmedDisplayName
                , userBio =
                    if trimmedBio == ""
                        then Nothing
                        else Just (Textarea trimmedBio)
                , userIsAdmin = False
                , userPlan = "free"
                , userPlanExpiresAt = Nothing
                , userTheme = Nothing
                , userThemeOverrides = Nothing
                }
    userWithPassword <- liftIO $ setPassword password baseUser
    userId <- runDB $ insert userWithPassword
    createdUser <- runDB $ get404 userId
    setCreds False $ Creds "api" trimmedIdent []
    returnJson $ object
        [ "authenticated" .= True
        , "user" .= userValue createdUser
        , "userId" .= fromSqlKey userId
        ]

postApiAuthLogoutR :: Handler Value
postApiAuthLogoutR = do
    clearCreds False
    returnJson $ object
        [ "authenticated" .= False
        ]

getApiSessionR :: Handler Value
getApiSessionR = do
    muser <- maybeAuth
    returnJson $ object
        [ "user" .= fmap (userValue . entityVal) muser
        , "authenticated" .= isJust muser
        ]

getApiMeR :: Handler Value
getApiMeR = do
    (userId, user) <- requireAuthPair
    userTheme' <- loadUserTheme user
    domains <- runDB $ selectList [CustomDomainUser ==. userId] [Desc CustomDomainUpdatedAt]
    totalArticles <- runDB $ count [ArticleAuthor ==. userId]
    draftCount <- runDB $ count [ArticleAuthor ==. userId, ArticleDraft ==. True]
    publishedCount <- runDB $ count [ArticleAuthor ==. userId, ArticleDraft !=. True]
    returnJson $ object
        [ "user" .= userValueWithTheme user userTheme'
        , "domains" .= map customDomainValue domains
        , "meta" .= object
            [ "totalCount" .= totalArticles
            , "draftCount" .= draftCount
            , "publishedCount" .= publishedCount
            ]
        ]

postApiMePlanR :: Handler Value
postApiMePlanR = do
    (userId, user) <- requireAuthPair
    requestedPlan <- normalizeUserPlan <$> runInputPost (fromMaybe "free" <$> iopt textField "plan")
    now <- liftIO getCurrentTime
    let expiresAt =
            if requestedPlan == "free"
                then Nothing
                else Just $ addUTCTime (60 * 60 * 24 * 30) now
    runDB $ update userId
        [ UserPlan =. requestedPlan
        , UserPlanExpiresAt =. expiresAt
        ]
    updatedUser <- runDB $ get404 userId
    updatedTheme <- loadUserTheme updatedUser
    returnJson $ object
        [ "user" .= userValueWithTheme updatedUser updatedTheme
        , "changedFrom" .= userPlan user
        ]

getApiMeDomainsR :: Handler Value
getApiMeDomainsR = do
    (userId, _user) <- requireAuthPair
    domains <- runDB $ selectList [CustomDomainUser ==. userId] [Desc CustomDomainUpdatedAt]
    returnJson $ object ["items" .= map customDomainValue domains]

postApiMeDomainsR :: Handler Value
postApiMeDomainsR = do
    (userId, user) <- requireAuthPair
    unless (userHasWriterPro user) $
        apiError status400 "Writer Pro is required for custom domains."
    domainInput <- T.toLower . T.strip <$> runInputPost (fromMaybe "" <$> iopt textField "domain")
    mode <- T.toLower . T.strip <$> runInputPost (fromMaybe "save" <$> iopt textField "mode")
    now <- liftIO getCurrentTime
    case mode of
        "delete" -> do
            runDB $ deleteWhere [CustomDomainUser ==. userId]
            returnJson $ object ["items" .= ([] :: [Value])]
        _ -> do
            when (domainInput == "") $
                apiError status400 "Domain is required."
            when (T.any Char.isSpace domainInput || not ("." `T.isInfixOf` domainInput)) $
                apiError status400 "Enter a valid domain."
            existing <- runDB $ getBy $ UniqueCustomDomain domainInput
            case existing of
                Just (Entity existingId existingDomain)
                    | customDomainUser existingDomain /= userId ->
                        apiError status400 "That domain is already claimed."
                    | otherwise -> do
                        let verificationToken = buildVerificationToken userId domainInput now
                        runDB $ update existingId
                            [ CustomDomainVerificationToken =. verificationToken
                            , CustomDomainStatus =. "pending"
                            , CustomDomainUpdatedAt =. now
                            ]
                Nothing -> do
                    runDB $ deleteWhere [CustomDomainUser ==. userId]
                    runDB $ insert_ CustomDomain
                        { customDomainUser = userId
                        , customDomainDomain = domainInput
                        , customDomainVerificationToken = buildVerificationToken userId domainInput now
                        , customDomainStatus = "pending"
                        , customDomainCreatedAt = now
                        , customDomainUpdatedAt = now
                        }
            domains <- runDB $ selectList [CustomDomainUser ==. userId] [Desc CustomDomainUpdatedAt]
            returnJson $ object ["items" .= map customDomainValue domains]

postApiMeUpdateR :: Handler Value
postApiMeUpdateR = do
    (userId, _user) <- requireAuthPair
    ident <- runInputPost $ ireq textField "ident"
    rawDisplayName <- runInputPost $ fromMaybe "" <$> iopt textField "displayName"
    rawBio <- runInputPost $ fromMaybe "" <$> iopt textField "bio"
    let trimmedIdent = T.strip ident
        trimmedDisplayName = T.strip rawDisplayName
        trimmedBio = T.strip rawBio
    when (trimmedIdent == "") $
        apiError status400 "Username is required."
    when (length trimmedIdent < 3) $
        apiError status400 "Username must be at least 3 characters."
    existingUser <- runDB $ getBy $ UniqueUser trimmedIdent
    case existingUser of
        Just (Entity existingUserId _)
            | existingUserId /= userId ->
                apiError status400 "That username is already taken."
        _ -> pure ()
    runDB $ update userId
        [ UserIdent =. trimmedIdent
        , UserDisplayName =.
            (if trimmedDisplayName == "" then Nothing else Just trimmedDisplayName)
        , UserBio =.
            (if trimmedBio == "" then Nothing else Just (Textarea trimmedBio))
        ]
    updatedUser <- runDB $ get404 userId
    returnJson $ object
        [ "user" .= userValue updatedUser
        ]

postApiMeThemeR :: Handler Value
postApiMeThemeR = do
    (userId, _user) <- requireAuthPair
    rawThemeId <- runInputPost $ fromMaybe "" <$> iopt textField "themeId"
    rawOverrides <- runInputPost $ fromMaybe "" <$> iopt textField "themeOverrides"
    mThemeId <- parseOptionalThemeId rawThemeId
    case mThemeId of
        Nothing -> pure ()
        Just themeId -> do
            mTheme <- runDB $ get themeId
            case mTheme of
                Just theme | themeActive theme -> do
                    canUse <- canUseTheme userId themeId theme
                    unless canUse $
                        apiError status400 "Purchase this theme before using it."
                _ -> apiError status400 "Select an active theme."
    runDB $ update userId
        [ UserTheme =. mThemeId
        , UserThemeOverrides =. cleanOptionalText (Just rawOverrides)
        ]
    updatedUser <- runDB $ get404 userId
    updatedTheme <- loadUserTheme updatedUser
    returnJson $ object ["user" .= userValueWithTheme updatedUser updatedTheme]

getApiMeThemesR :: Handler Value
getApiMeThemesR = do
    (userId, _user) <- requireAuthPair
    ownedThemeIds <- loadOwnedThemeIds userId
    themes <- runDB $ selectList [] [Desc ThemeUpdatedAt]
    returnJson $ object
        [ "items" .= map (themeMarketplaceValue (Just userId) ownedThemeIds) themes
        ]

getApiMeThemeOrdersR :: Handler Value
getApiMeThemeOrdersR = do
    (userId, _user) <- requireAuthPair
    orders <- runDB $ selectList [ThemeOrderUser ==. userId] [Desc ThemeOrderCreatedAt]
    themeMap <- loadThemeMap $ map (themeOrderTheme . entityVal) orders
    returnJson $ object
        [ "items" .= map (themeOrderValue themeMap) orders
        ]

getApiMeThemeStatsR :: Handler Value
getApiMeThemeStatsR = do
    (userId, _user) <- requireAuthPair
    authoredThemes <- runDB $ selectList [ThemeAuthor ==. Just userId] [Desc ThemeUpdatedAt]
    let themeIds = map entityKey authoredThemes
    orders <- if null themeIds
        then pure []
        else runDB $ selectList [ThemeOrderTheme <-. themeIds] [Desc ThemeOrderCreatedAt]
    let orderGroups =
            foldl'
                (\acc (Entity _ order) -> M.insertWith (<>) (themeOrderTheme order) [order] acc)
                M.empty
                orders
        statItems =
            map
                (\themeEntity@(Entity themeId _theme) ->
                    let themeOrders = fromMaybe [] (M.lookup themeId orderGroups)
                        orderCount = length themeOrders
                        pendingCount = length $ filter (\order -> themeOrderStatus order == "pending") themeOrders
                        paidCount = length $ filter (\order -> themeOrderStatus order == "paid") themeOrders
                        failedCount = length $ filter (\order -> themeOrderStatus order == "failed") themeOrders
                        revenueCents =
                            sum $ map themeOrderAmountCents $ filter (\order -> themeOrderStatus order == "paid") themeOrders
                    in object
                        [ "theme" .= themeValue themeEntity
                        , "orderCount" .= orderCount
                        , "pendingCount" .= pendingCount
                        , "paidCount" .= paidCount
                        , "failedCount" .= failedCount
                        , "revenueCents" .= revenueCents
                        ]
                )
                authoredThemes
        totalOrderCount = length orders
        totalPendingCount = length $ filter (\order -> themeOrderStatus (entityVal order) == "pending") orders
        totalPaidCount = length $ filter (\order -> themeOrderStatus (entityVal order) == "paid") orders
        totalFailedCount = length $ filter (\order -> themeOrderStatus (entityVal order) == "failed") orders
        totalRevenueCents =
            sum $ map (themeOrderAmountCents . entityVal) $ filter (\order -> themeOrderStatus (entityVal order) == "paid") orders
    returnJson $ object
        [ "summary" .= object
            [ "themeCount" .= length authoredThemes
            , "orderCount" .= totalOrderCount
            , "pendingCount" .= totalPendingCount
            , "paidCount" .= totalPaidCount
            , "failedCount" .= totalFailedCount
            , "revenueCents" .= totalRevenueCents
            ]
        , "items" .= statItems
        ]

getApiMeThemePayoutsR :: Handler Value
getApiMeThemePayoutsR = do
    (userId, _user) <- requireAuthPair
    payoutData <- buildThemePayoutData userId
    payoutRequests <- runDB $ selectList [ThemePayoutUser ==. userId] [Desc ThemePayoutRequestedAt]
    let reservedRequestedCents =
            sum $
                map (themePayoutAmountCents . entityVal) $
                    filter (\payout -> themePayoutStatus (entityVal payout) == "requested") payoutRequests
        alreadyPaidCents =
            sum $
                map (themePayoutAmountCents . entityVal) $
                    filter (\payout -> themePayoutStatus (entityVal payout) == "paid") payoutRequests
    returnJson $ object
        [ "summary" .= payoutSummaryValue payoutData reservedRequestedCents alreadyPaidCents
        , "items" .= payoutItemValues payoutData
        , "requests" .= map (themePayoutValue M.empty) payoutRequests
        ]

postApiMeThemePayoutRequestR :: Handler Value
postApiMeThemePayoutRequestR = do
    (userId, _user) <- requireAuthPair
    payoutData <- buildThemePayoutData userId
    existingRequested <- runDB $ selectFirst [ThemePayoutUser ==. userId, ThemePayoutStatus ==. "requested"] [Desc ThemePayoutRequestedAt]
    when (isJust existingRequested) $
        apiError status400 "There is already an open payout request."
    existingPayouts <- runDB $ selectList [ThemePayoutUser ==. userId] []
    let reservedRequestedCents =
            sum $
                map (themePayoutAmountCents . entityVal) $
                    filter (\payout -> themePayoutStatus (entityVal payout) == "requested") existingPayouts
        alreadyPaidCents =
            sum $
                map (themePayoutAmountCents . entityVal) $
                    filter (\payout -> themePayoutStatus (entityVal payout) == "paid") existingPayouts
        amountCents = availablePayoutCents payoutData reservedRequestedCents alreadyPaidCents
    when (amountCents <= 0) $
        apiError status400 "There is no payout-ready balance yet."
    note <- runInputPost $ fromMaybe "" <$> iopt textField "note"
    now <- liftIO getCurrentTime
    payoutId <- runDB $ insert ThemePayout
        { themePayoutUser = userId
        , themePayoutAmountCents = amountCents
        , themePayoutStatus = "requested"
        , themePayoutSellerNote = normalizeOptionalTextarea $ Just note
        , themePayoutAdminNote = Nothing
        , themePayoutRequestedAt = now
        , themePayoutProcessedAt = Nothing
        }
    createdPayout <- Entity payoutId <$> runDB (get404 payoutId)
    returnJson $ object ["payout" .= themePayoutValue M.empty createdPayout]

getApiThemesR :: Handler Value
getApiThemesR = do
    (userId, _user) <- requireAuthPair
    ownedThemeIds <- loadOwnedThemeIds userId
    themes <- runDB $ selectList [ThemeActive ==. True, ThemeStatus ==. "published"] [Asc ThemeName]
    let usableThemes =
            filter
                (\(Entity themeId theme) ->
                    themePriceCents theme == 0 ||
                    themeId `elem` ownedThemeIds ||
                    themeAuthor theme == Just userId
                )
                themes
    returnJson $ object ["items" .= map themeValue usableThemes]

getApiThemeMarketplaceR :: Handler Value
getApiThemeMarketplaceR = do
    mUser <- maybeAuth
    let mUserId = entityKey <$> mUser
    ownedThemeIds <- maybe (pure []) loadOwnedThemeIds mUserId
    themes <- runDB $ selectList [ThemeActive ==. True, ThemeStatus ==. "published"] [Desc ThemeUpdatedAt]
    ratingSummaryMap <- loadThemeRatingSummaryMap (map entityKey themes)
    myRatingMap <- maybe (pure M.empty) (\userId -> loadUserThemeRatingMap userId (map entityKey themes)) mUserId
    myReportThemeIds <- maybe (pure []) (\userId -> loadUserThemeReportIds userId (map entityKey themes)) mUserId
    returnJson $ object
        [ "items" .=
            map
                (themeMarketplaceDetailValue mUserId ownedThemeIds ratingSummaryMap myRatingMap myReportThemeIds)
                themes
        ]

postApiThemePurchaseR :: ThemeId -> Handler Value
postApiThemePurchaseR themeId = do
    (userId, _user) <- requireAuthPair
    theme <- runDB $ get404 themeId
    unless (themeActive theme && themeStatus theme == "published") $
        apiError status400 "This theme is not available for purchase."
    when (themeAuthor theme == Just userId) $
        apiError status400 "You already own this theme."
    existingPurchase <- runDB $ getBy $ UniqueThemePurchase userId themeId
    case existingPurchase of
        Just _ -> returnJson $ object ["theme" .= themeValue (Entity themeId theme), "alreadyOwned" .= True]
        Nothing -> do
            now <- liftIO getCurrentTime
            if themePriceCents theme == 0
                then do
                    orderEntity <- runDB $ createPaidThemeOrder userId themeId theme now
                    returnJson $ object
                        [ "theme" .= themeValue (Entity themeId theme)
                        , "order" .= themeOrderValue (M.singleton themeId theme) orderEntity
                        , "alreadyOwned" .= False
                        , "paymentMode" .= ("free" :: Text)
                        , "requiresConfirmation" .= False
                        ]
                else do
                    existingPendingOrder <- runDB $
                        selectFirst
                            [ ThemeOrderUser ==. userId
                            , ThemeOrderTheme ==. themeId
                            , ThemeOrderStatus ==. "pending"
                            ]
                            [Desc ThemeOrderCreatedAt]
                    orderEntity <- case existingPendingOrder of
                        Just orderEntity -> pure orderEntity
                        Nothing -> do
                            orderId <- runDB $ insert ThemeOrder
                                { themeOrderUser = userId
                                , themeOrderTheme = themeId
                                , themeOrderAmountCents = themePriceCents theme
                                , themeOrderStatus = "pending"
                                , themeOrderProvider = Just "manual-record"
                                , themeOrderProviderOrderId = Nothing
                                , themeOrderCreatedAt = now
                                , themeOrderPaidAt = Nothing
                                }
                            Entity orderId <$> runDB (get404 orderId)
                    returnJson $ object
                        [ "theme" .= themeValue (Entity themeId theme)
                        , "order" .= themeOrderValue (M.singleton themeId theme) orderEntity
                        , "alreadyOwned" .= False
                        , "paymentMode" .= ("manual-record" :: Text)
                        , "requiresConfirmation" .= True
                        ]

postApiThemePurchaseConfirmR :: ThemeId -> Handler Value
postApiThemePurchaseConfirmR themeId = do
    (userId, _user) <- requireAuthPair
    desiredStatus <- normalizeThemeOrderStatus <$> runInputPost (fromMaybe "paid" <$> iopt textField "status")
    theme <- runDB $ get404 themeId
    pendingOrderEntity <- runDB $
        selectFirst
            [ ThemeOrderUser ==. userId
            , ThemeOrderTheme ==. themeId
            , ThemeOrderStatus ==. "pending"
            ]
            [Desc ThemeOrderCreatedAt]
    case pendingOrderEntity of
        Nothing -> apiError status400 "There is no pending order to confirm."
        Just (Entity orderId order) -> do
            now <- liftIO getCurrentTime
            runDB $ do
                update orderId
                    [ ThemeOrderStatus =. desiredStatus
                    , ThemeOrderPaidAt =. if desiredStatus == "paid" then Just now else Nothing
                    ]
                when (desiredStatus == "paid") $ do
                    existingPurchase <- getBy $ UniqueThemePurchase userId themeId
                    case existingPurchase of
                        Nothing ->
                            insert_ ThemePurchase
                                { themePurchaseUser = userId
                                , themePurchaseTheme = themeId
                                , themePurchasePriceCents = themeOrderAmountCents order
                                , themePurchasePurchasedAt = now
                                }
                        Just _ -> pure ()
            updatedOrder <- Entity orderId <$> runDB (get404 orderId)
            returnJson $ object
                [ "theme" .= themeValue (Entity themeId theme)
                , "order" .= themeOrderValue (M.singleton themeId theme) updatedOrder
                , "status" .= desiredStatus
                ]

postApiThemeCreateR :: Handler Value
postApiThemeCreateR = do
    (userId, user) <- requireAuthPair
    now <- liftIO getCurrentTime
    theme <- readUserThemeInput userId Nothing now
    authoredThemeCount <- runDB $ count [ThemeAuthor ==. Just userId]
    when (not (userHasDesignerPro user) && authoredThemeCount >= 3) $
        apiError status400 "Designer Pro is required to publish more than three themes."
    when (themePriceCents theme > 0 && not (userHasDesignerPro user)) $
        apiError status400 "Designer Pro is required for paid themes."
    ensureThemeSlugAvailable Nothing (themeSlug theme)
    themeId <- runDB $ insert theme
    runDB $ upsertThemeReview themeId Nothing "pending" Nothing now
    createdTheme <- Entity themeId <$> runDB (get404 themeId)
    returnJson $ object ["theme" .= themeValue createdTheme]

postApiThemeUpdateR :: ThemeId -> Handler Value
postApiThemeUpdateR themeId = do
    (userId, existingTheme) <- requireThemeAuthor themeId
    currentUser <- runDB $ get404 userId
    now <- liftIO getCurrentTime
    submittedTheme <- readUserThemeInput userId (themeParent existingTheme) now
    when (themePriceCents submittedTheme > 0 && not (userHasDesignerPro currentUser)) $
        apiError status400 "Designer Pro is required for paid themes."
    ensureThemeSlugAvailable (Just themeId) (themeSlug submittedTheme)
    let updatedTheme =
            submittedTheme
                { themeAuthor = themeAuthor existingTheme
                , themeParent = themeParent existingTheme
                , themeStatus = "review"
                , themeLicense = themeLicense existingTheme <|> themeLicense submittedTheme
                , themeCreatedAt = themeCreatedAt existingTheme
                , themeUpdatedAt = now
                }
    runDB $ do
        replace themeId updatedTheme
        upsertThemeReview themeId Nothing "pending" (Just "Updated by author and resubmitted for review.") now
    refreshedTheme <- Entity themeId <$> runDB (get404 themeId)
    returnJson $ object ["theme" .= themeValue refreshedTheme]

postApiThemeDeleteR :: ThemeId -> Handler Value
postApiThemeDeleteR themeId = do
    _ <- requireThemeAuthor themeId
    runDB $ do
        updateWhere [UserTheme ==. Just themeId]
            [ UserTheme =. Nothing
            , UserThemeOverrides =. Nothing
            ]
        updateWhere [ThemeParent ==. Just themeId] [ThemeParent =. Nothing]
        deleteWhere [ThemeReviewTheme ==. themeId]
        deleteWhere [ThemeOrderTheme ==. themeId]
        deleteWhere [ThemePurchaseTheme ==. themeId]
        delete themeId
    returnJson $ object ["deletedThemeId" .= fromSqlKey themeId]

postApiThemeForkR :: ThemeId -> Handler Value
postApiThemeForkR sourceThemeId = do
    (userId, user) <- requireAuthPair
    sourceTheme <- runDB $ get404 sourceThemeId
    canUse <- canUseTheme userId sourceThemeId sourceTheme
    unless canUse $
        apiError status400 "Purchase this theme before remixing it."
    now <- liftIO getCurrentTime
    rawName <- runInputPost $ fromMaybe (themeName sourceTheme <> " Remix") <$> iopt textField "name"
    rawSlug <- runInputPost $ fromMaybe (themeSlug sourceTheme <> "-" <> userIdent user <> "-remix") <$> iopt textField "slug"
    rawPriceCents <- runInputPost $ fromMaybe "0" <$> iopt textField "priceCents"
    let name = T.strip rawName
        slug = normalizeThemeSlug rawSlug
        priceCents = parseNonNegativeInt rawPriceCents
        forkTheme = sourceTheme
            { themeName = name
            , themeSlug = slug
            , themeAuthor = Just userId
            , themeParent = Just sourceThemeId
            , themePriceCents = priceCents
            , themeStatus = "review"
            , themeCreatedAt = now
            , themeUpdatedAt = now
            }
    when (priceCents > 0 && not (userHasDesignerPro user)) $
        apiError status400 "Designer Pro is required for paid remix themes."
    when (name == "") $
        apiError status400 "Theme name is required."
    when (slug == "") $
        apiError status400 "Theme slug is required."
    ensureThemeSlugAvailable Nothing slug
    themeId <- runDB $ insert forkTheme
    runDB $ upsertThemeReview themeId Nothing "pending" Nothing now
    createdTheme <- Entity themeId <$> runDB (get404 themeId)
    returnJson $ object ["theme" .= themeValue createdTheme]

postApiThemeReviewUpsertR :: ThemeId -> Handler Value
postApiThemeReviewUpsertR themeId = do
    (userId, _user) <- requireAuthPair
    rating <- min 5 . max 1 <$> runInputPost (fromMaybe 5 <$> iopt intField "rating")
    reviewText <- T.strip <$> runInputPost (fromMaybe "" <$> iopt textField "review")
    theme <- runDB $ get404 themeId
    unless (themeActive theme && themeStatus theme == "published") $
        apiError status400 "Only published themes can be reviewed."
    canUse <- canUseTheme userId themeId theme
    unless (canUse || themeAuthor theme == Just userId) $
        apiError status400 "Use this theme before reviewing it."
    now <- liftIO getCurrentTime
    existing <- runDB $ getBy $ UniqueThemeRating userId themeId
    case existing of
        Nothing ->
            runDB $ insert_ ThemeRating
                { themeRatingUser = userId
                , themeRatingTheme = themeId
                , themeRatingRating = rating
                , themeRatingReview = normalizeOptionalTextarea (Just reviewText)
                , themeRatingCreatedAt = now
                , themeRatingUpdatedAt = now
                }
        Just (Entity ratingId savedRating) ->
            runDB $ replace ratingId savedRating
                { themeRatingRating = rating
                , themeRatingReview = normalizeOptionalTextarea (Just reviewText)
                , themeRatingUpdatedAt = now
                }
    ratingSummaryMap <- loadThemeRatingSummaryMap [themeId]
    myRatingMap <- loadUserThemeRatingMap userId [themeId]
    returnJson $ object
        [ "summary" .= fromMaybe (object ["averageRating" .= (0 :: Double), "ratingCount" .= (0 :: Int)]) (M.lookup themeId ratingSummaryMap)
        , "myReview" .= fromMaybe Null (M.lookup themeId myRatingMap)
        ]

postApiThemeReportR :: ThemeId -> Handler Value
postApiThemeReportR themeId = do
    (userId, _user) <- requireAuthPair
    reason <- T.strip <$> runInputPost (fromMaybe "" <$> iopt textField "reason")
    details <- T.strip <$> runInputPost (fromMaybe "" <$> iopt textField "details")
    when (reason == "") $
        apiError status400 "Report reason is required."
    _ <- runDB $ get404 themeId
    now <- liftIO getCurrentTime
    runDB $ insert_ ThemeReport
        { themeReportUser = userId
        , themeReportTheme = themeId
        , themeReportReason = reason
        , themeReportDetails = normalizeOptionalTextarea (Just details)
        , themeReportStatus = "open"
        , themeReportCreatedAt = now
        }
    returnJson $ object ["reported" .= True]

parseOptionalThemeId :: Text -> Handler (Maybe ThemeId)
parseOptionalThemeId rawValue =
    let trimmed = T.strip rawValue
    in if trimmed == ""
        then pure Nothing
        else case fromPathPiece trimmed of
            Just themeId -> pure $ Just themeId
            Nothing -> apiError status400 "Invalid theme id."

loadOwnedThemeIds :: UserId -> Handler [ThemeId]
loadOwnedThemeIds userId =
    map (themePurchaseTheme . entityVal) <$> runDB (selectList [ThemePurchaseUser ==. userId] [])

requireThemeAuthor :: ThemeId -> Handler (UserId, Theme)
requireThemeAuthor themeId = do
    (userId, _user) <- requireAuthPair
    theme <- runDB $ get404 themeId
    unless (themeAuthor theme == Just userId) $
        apiError status400 "You can manage only your own themes."
    pure (userId, theme)

loadThemeMap :: [ThemeId] -> Handler (M.Map ThemeId Theme)
loadThemeMap themeIds
    | null themeIds = pure M.empty
    | otherwise = do
        themes <- runDB $ selectList [ThemeId <-. L.nub themeIds] []
        pure $ M.fromList $ map (\(Entity themeId theme) -> (themeId, theme)) themes

upsertThemeReview :: ThemeId -> Maybe UserId -> Text -> Maybe Text -> UTCTime -> ReaderT SqlBackend Handler ()
upsertThemeReview themeId mReviewerId status mNote now = do
    existingReview <- getBy $ UniqueThemeReview themeId
    let reviewNote = fmap Textarea (cleanOptionalText mNote)
    case existingReview of
        Nothing ->
            insert_ ThemeReview
                { themeReviewTheme = themeId
                , themeReviewReviewer = mReviewerId
                , themeReviewStatus = status
                , themeReviewNote = reviewNote
                , themeReviewCreatedAt = now
                , themeReviewUpdatedAt = now
                }
        Just (Entity reviewId review) ->
            replace reviewId review
                { themeReviewReviewer = mReviewerId
                , themeReviewStatus = status
                , themeReviewNote = reviewNote
                , themeReviewUpdatedAt = now
                }

createPaidThemeOrder :: UserId -> ThemeId -> Theme -> UTCTime -> ReaderT SqlBackend Handler (Entity ThemeOrder)
createPaidThemeOrder userId themeId theme now = do
    orderId <- insert ThemeOrder
        { themeOrderUser = userId
        , themeOrderTheme = themeId
        , themeOrderAmountCents = themePriceCents theme
        , themeOrderStatus = "paid"
        , themeOrderProvider = Just "manual-record"
        , themeOrderProviderOrderId = Nothing
        , themeOrderCreatedAt = now
        , themeOrderPaidAt = Just now
        }
    insert_ ThemePurchase
        { themePurchaseUser = userId
        , themePurchaseTheme = themeId
        , themePurchasePriceCents = themePriceCents theme
        , themePurchasePurchasedAt = now
        }
    Entity orderId <$> get404 orderId

normalizeThemeOrderStatus :: Text -> Text
normalizeThemeOrderStatus rawStatus =
    let value = T.toLower $ T.strip rawStatus
    in if value `elem` ["paid", "failed", "cancelled"]
        then value
        else "paid"

canUseTheme :: UserId -> ThemeId -> Theme -> Handler Bool
canUseTheme userId themeId theme
    | themePriceCents theme == 0 = pure True
    | themeAuthor theme == Just userId = pure True
    | otherwise = isJust <$> runDB (getBy $ UniqueThemePurchase userId themeId)

platformFeeShareCents :: Int -> Int
platformFeeShareCents amountCents = percentageShareCents 1500 amountCents

royaltyShareCents :: Int -> Int
royaltyShareCents amountCents = percentageShareCents 1000 amountCents

percentageShareCents :: Int -> Int -> Int
percentageShareCents basisPoints amountCents = amountCents * basisPoints `div` 10000

data ThemePayoutSummaryData = ThemePayoutSummaryData
    { payoutThemeCount :: Int
    , payoutPaidCount :: Int
    , payoutGrossRevenue :: Int
    , payoutPlatformFee :: Int
    , payoutDownstreamRoyalty :: Int
    , payoutUpstreamRoyalty :: Int
    , payoutNetRevenue :: Int
    }

data ThemePayoutItemData = ThemePayoutItemData
    { payoutItemTheme :: Value
    , payoutItemPaidCount :: Int
    , payoutItemGrossRevenue :: Int
    , payoutItemPlatformFee :: Int
    , payoutItemParentRoyalty :: Int
    , payoutItemUpstreamRoyalty :: Int
    , payoutItemChildOrderCount :: Int
    , payoutItemNetRevenue :: Int
    }

data ThemePayoutData = ThemePayoutData
    { payoutSummaryData :: ThemePayoutSummaryData
    , payoutItemsData :: [ThemePayoutItemData]
    }

buildThemePayoutData :: UserId -> Handler ThemePayoutData
buildThemePayoutData userId = do
    authoredThemes <- runDB $ selectList [ThemeAuthor ==. Just userId] [Desc ThemeUpdatedAt]
    let authoredThemeIds = map entityKey authoredThemes
    paidOrders <- if null authoredThemeIds
        then pure []
        else runDB $ selectList [ThemeOrderTheme <-. authoredThemeIds, ThemeOrderStatus ==. "paid"] []
    childThemes <- if null authoredThemeIds
        then pure []
        else runDB $ selectList [ThemeParent <-. map Just authoredThemeIds] []
    parentThemes <- do
        let parentIds = L.nub $ mapMaybe (themeParent . entityVal) authoredThemes
        if null parentIds
            then pure []
            else runDB $ selectList [ThemeId <-. parentIds] []
    let childThemeIds = map entityKey childThemes
    childPaidOrders <- if null childThemeIds
        then pure []
        else runDB $ selectList [ThemeOrderTheme <-. childThemeIds, ThemeOrderStatus ==. "paid"] []
    let orderGroups =
            foldl'
                (\acc (Entity _ order) -> M.insertWith (<>) (themeOrderTheme order) [order] acc)
                M.empty
                paidOrders
        childThemeMap = M.fromList $ map (\(Entity themeId theme) -> (themeId, theme)) childThemes
        parentThemeMap = M.fromList $ map (\(Entity themeId theme) -> (themeId, theme)) parentThemes
        upstreamRoyaltyByParent =
            foldl'
                (\acc (Entity _ order) ->
                    case M.lookup (themeOrderTheme order) childThemeMap of
                        Just childTheme ->
                            case (themeParent childTheme, themeAuthor childTheme) of
                                (Just parentThemeId, childAuthorId)
                                    | childAuthorId /= Just userId ->
                                        M.insertWith (+) parentThemeId (royaltyShareCents $ themeOrderAmountCents order) acc
                                _ -> acc
                        Nothing -> acc
                )
                M.empty
                childPaidOrders
        childOrderCountByParent :: M.Map ThemeId Int
        childOrderCountByParent =
            foldl'
                (\acc (Entity _ order) ->
                    case M.lookup (themeOrderTheme order) childThemeMap of
                        Just childTheme ->
                            case (themeParent childTheme, themeAuthor childTheme) of
                                (Just parentThemeId, childAuthorId)
                                    | childAuthorId /= Just userId ->
                                        M.insertWith (+) parentThemeId (1 :: Int) acc
                                _ -> acc
                        Nothing -> acc
                )
                M.empty
                childPaidOrders
        itemData =
            map
                (\themeEntity@(Entity themeId theme) ->
                    let themeOrders = fromMaybe [] (M.lookup themeId orderGroups)
                        paidCount = length themeOrders
                        grossRevenueCents = sum $ map themeOrderAmountCents themeOrders
                        platformFeeCents = platformFeeShareCents grossRevenueCents
                        parentRoyaltyCents =
                            case themeParent theme of
                                Just parentThemeId ->
                                    case M.lookup parentThemeId parentThemeMap of
                                        Just parentTheme
                                            | themeAuthor parentTheme /= Just userId -> royaltyShareCents grossRevenueCents
                                        _ -> 0
                                Nothing -> 0
                        upstreamRoyaltyEarnedCents = fromMaybe 0 (M.lookup themeId upstreamRoyaltyByParent)
                        childOrderCount = fromMaybe (0 :: Int) (M.lookup themeId childOrderCountByParent)
                        netRevenueCents = grossRevenueCents - platformFeeCents - parentRoyaltyCents
                    in ThemePayoutItemData
                        { payoutItemTheme = themeValue themeEntity
                        , payoutItemPaidCount = paidCount
                        , payoutItemGrossRevenue = grossRevenueCents
                        , payoutItemPlatformFee = platformFeeCents
                        , payoutItemParentRoyalty = parentRoyaltyCents
                        , payoutItemUpstreamRoyalty = upstreamRoyaltyEarnedCents
                        , payoutItemChildOrderCount = childOrderCount
                        , payoutItemNetRevenue = netRevenueCents
                        }
                )
                authoredThemes
        totalGrossRevenueCents = sum $ map payoutItemGrossRevenue itemData
        totalPlatformFeeCents = sum $ map payoutItemPlatformFee itemData
        totalDownstreamRoyaltyCents = sum $ map payoutItemParentRoyalty itemData
        totalUpstreamRoyaltyEarnedCents = sum $ map payoutItemUpstreamRoyalty itemData
        totalNetRevenueCents = sum $ map payoutItemNetRevenue itemData
    pure ThemePayoutData
        { payoutSummaryData = ThemePayoutSummaryData
            { payoutThemeCount = length authoredThemes
            , payoutPaidCount = length paidOrders
            , payoutGrossRevenue = totalGrossRevenueCents
            , payoutPlatformFee = totalPlatformFeeCents
            , payoutDownstreamRoyalty = totalDownstreamRoyaltyCents
            , payoutUpstreamRoyalty = totalUpstreamRoyaltyEarnedCents
            , payoutNetRevenue = totalNetRevenueCents
            }
        , payoutItemsData = itemData
        }

payoutReadyCentsFromData :: ThemePayoutData -> Int
payoutReadyCentsFromData payoutData =
    payoutNetRevenue (payoutSummaryData payoutData) + payoutUpstreamRoyalty (payoutSummaryData payoutData)

availablePayoutCents :: ThemePayoutData -> Int -> Int -> Int
availablePayoutCents payoutData requestedCents paidCents =
    max 0 (payoutReadyCentsFromData payoutData - requestedCents - paidCents)

payoutSummaryValue :: ThemePayoutData -> Int -> Int -> Value
payoutSummaryValue payoutData requestedCents paidCents =
    let summary = payoutSummaryData payoutData
    in object
        [ "themeCount" .= payoutThemeCount summary
        , "paidCount" .= payoutPaidCount summary
        , "grossRevenueCents" .= payoutGrossRevenue summary
        , "platformFeeCents" .= payoutPlatformFee summary
        , "downstreamRoyaltyCents" .= payoutDownstreamRoyalty summary
        , "upstreamRoyaltyEarnedCents" .= payoutUpstreamRoyalty summary
        , "netRevenueCents" .= payoutNetRevenue summary
        , "payoutReadyCents" .= payoutReadyCentsFromData payoutData
        , "requestedCents" .= requestedCents
        , "paidOutCents" .= paidCents
        , "availableCents" .= availablePayoutCents payoutData requestedCents paidCents
        ]

payoutItemValues :: ThemePayoutData -> [Value]
payoutItemValues payoutData =
    map
        (\item ->
            object
                [ "theme" .= payoutItemTheme item
                , "paidCount" .= payoutItemPaidCount item
                , "grossRevenueCents" .= payoutItemGrossRevenue item
                , "platformFeeCents" .= payoutItemPlatformFee item
                , "parentRoyaltyCents" .= payoutItemParentRoyalty item
                , "upstreamRoyaltyEarnedCents" .= payoutItemUpstreamRoyalty item
                , "childOrderCount" .= payoutItemChildOrderCount item
                , "netRevenueCents" .= payoutItemNetRevenue item
                , "payoutReadyCents" .= (payoutItemNetRevenue item + payoutItemUpstreamRoyalty item)
                ]
        )
        (payoutItemsData payoutData)

readUserThemeInput :: UserId -> Maybe ThemeId -> UTCTime -> Handler Theme
readUserThemeInput userId mParentThemeId now = do
    name <- T.strip <$> runInputPost (ireq textField "name")
    rawSlug <- T.strip <$> runInputPost (ireq textField "slug")
    rawDescription <- runInputPost $ fromMaybe "" <$> iopt textField "description"
    backgroundColor <- normalizeColorInput =<< runInputPost (ireq textField "backgroundColor")
    surfaceColor <- normalizeColorInput =<< runInputPost (ireq textField "surfaceColor")
    textColor <- normalizeColorInput =<< runInputPost (ireq textField "textColor")
    accentColor <- normalizeColorInput =<< runInputPost (ireq textField "accentColor")
    rawHeadingFont <- runInputPost $ fromMaybe "" <$> iopt textField "headingFont"
    rawBodyFont <- runInputPost $ fromMaybe "" <$> iopt textField "bodyFont"
    rawHeaderTemplate <- runInputPost $ fromMaybe "" <$> iopt textField "headerTemplate"
    rawBodyTemplate <- runInputPost $ fromMaybe "" <$> iopt textField "bodyTemplate"
    rawFooterTemplate <- runInputPost $ fromMaybe "" <$> iopt textField "footerTemplate"
    rawCustomCss <- runInputPost $ fromMaybe "" <$> iopt textField "customCss"
    rawPriceCents <- runInputPost $ fromMaybe "0" <$> iopt textField "priceCents"
    let slug = normalizeThemeSlug rawSlug
    when (name == "") $
        apiError status400 "Theme name is required."
    when (slug == "") $
        apiError status400 "Theme slug is required."
    pure Theme
        { themeName = name
        , themeSlug = slug
        , themeDescription = normalizeOptionalTextarea $ Just rawDescription
        , themeAuthor = Just userId
        , themeParent = mParentThemeId
        , themeBackgroundColor = backgroundColor
        , themeSurfaceColor = surfaceColor
        , themeTextColor = textColor
        , themeAccentColor = accentColor
        , themeHeadingFont = nonEmptyText rawHeadingFont
        , themeBodyFont = nonEmptyText rawBodyFont
        , themeHeaderTemplate = normalizeOptionalTextarea $ Just rawHeaderTemplate
        , themeBodyTemplate = normalizeOptionalTextarea $ Just rawBodyTemplate
        , themeFooterTemplate = normalizeOptionalTextarea $ Just rawFooterTemplate
        , themeCustomCss = normalizeOptionalTextarea $ Just rawCustomCss
        , themePriceCents = parseNonNegativeInt rawPriceCents
        , themeStatus = "review"
        , themeLicense = Just "marketplace-remix"
        , themeActive = True
        , themeCreatedAt = now
        , themeUpdatedAt = now
        }

ensureThemeSlugAvailable :: Maybe ThemeId -> Text -> Handler ()
ensureThemeSlugAvailable mThemeId slug = do
    existingTheme <- runDB $ getBy $ UniqueThemeSlug slug
    case existingTheme of
        Just (Entity existingThemeId _)
            | Just existingThemeId /= mThemeId ->
                apiError status400 "That theme slug is already used."
        _ -> pure ()

normalizeColorInput :: Text -> Handler Text
normalizeColorInput rawValue = do
    let value = T.strip rawValue
    when (value == "") $
        apiError status400 "Theme colors are required."
    pure value

normalizeThemeSlug :: Text -> Text
normalizeThemeSlug source =
    T.intercalate "-" $
        filter (/= "") $
            map
                (T.filter (\char -> Char.isAlphaNum char || char == '-'))
                (T.words $ T.map (\char -> if Char.isAlphaNum char then char else ' ') $ T.toLower source)

parseNonNegativeInt :: Text -> Int
parseNonNegativeInt rawValue =
    case TR.decimal (T.strip rawValue) of
        Right (value, _) -> max 0 value
        Left _ -> 0

customDomainValue :: Entity CustomDomain -> Value
customDomainValue (Entity domainId domainRecord) =
    object
        [ "id" .= fromSqlKey domainId
        , "domain" .= customDomainDomain domainRecord
        , "verificationToken" .= customDomainVerificationToken domainRecord
        , "status" .= customDomainStatus domainRecord
        , "createdAt" .= isoTime (customDomainCreatedAt domainRecord)
        , "updatedAt" .= isoTime (customDomainUpdatedAt domainRecord)
        ]

buildVerificationToken :: UserId -> Text -> UTCTime -> Text
buildVerificationToken userId domain now =
    "yesblog-" <> tshow (fromSqlKey userId) <> "-" <> tshow (floor (utctDayTime now) :: Int) <> "-" <> T.take 12 (T.filter Char.isAlphaNum domain)

themeMarketplaceDetailValue
    :: Maybe UserId
    -> [ThemeId]
    -> M.Map ThemeId Value
    -> M.Map ThemeId Value
    -> [ThemeId]
    -> Entity Theme
    -> Value
themeMarketplaceDetailValue mUserId ownedThemeIds ratingSummaryMap myRatingMap myReportThemeIds themeEntity@(Entity themeId _) =
    case themeMarketplaceValue mUserId ownedThemeIds themeEntity of
        Object baseObject ->
            Object $
                KM.insert "reportedByMe" (toJSON $ themeId `elem` myReportThemeIds) $
                    KM.insert "myReview" (fromMaybe Null $ M.lookup themeId myRatingMap) $
                        KM.insert
                            "rating"
                            (fromMaybe (object ["averageRating" .= (0 :: Double), "ratingCount" .= (0 :: Int)]) $ M.lookup themeId ratingSummaryMap)
                            baseObject
        other -> other

loadThemeRatingSummaryMap :: [ThemeId] -> Handler (M.Map ThemeId Value)
loadThemeRatingSummaryMap themeIds
    | null themeIds = pure M.empty
    | otherwise = do
        ratings <- runDB $ selectList [ThemeRatingTheme <-. L.nub themeIds] []
        let grouped =
                foldl'
                    (\acc (Entity _ rating) ->
                        M.insertWith
                            (\(sumB, countB) (sumA, countA) -> (sumA + sumB, countA + countB))
                            (themeRatingTheme rating)
                            (themeRatingRating rating, 1 :: Int)
                            acc
                    )
                    M.empty
                    ratings
        pure $
            fmap
                (\(ratingSum, ratingCount) ->
                    object
                        [ "averageRating" .= (if ratingCount == 0 then 0 else (fromIntegral ratingSum :: Double) / fromIntegral ratingCount)
                        , "ratingCount" .= ratingCount
                        ]
                )
                grouped

loadUserThemeRatingMap :: UserId -> [ThemeId] -> Handler (M.Map ThemeId Value)
loadUserThemeRatingMap userId themeIds
    | null themeIds = pure M.empty
    | otherwise = do
        ratings <- runDB $ selectList [ThemeRatingUser ==. userId, ThemeRatingTheme <-. L.nub themeIds] []
        pure $
            M.fromList $
                map
                    (\(Entity _ rating) ->
                        ( themeRatingTheme rating
                        , object
                            [ "rating" .= themeRatingRating rating
                            , "review" .= fmap unTextarea (themeRatingReview rating)
                            , "updatedAt" .= isoTime (themeRatingUpdatedAt rating)
                            ]
                        )
                    )
                    ratings

loadUserThemeReportIds :: UserId -> [ThemeId] -> Handler [ThemeId]
loadUserThemeReportIds userId themeIds
    | null themeIds = pure []
    | otherwise =
        map (themeReportTheme . entityVal) <$> runDB (selectList [ThemeReportUser ==. userId, ThemeReportTheme <-. L.nub themeIds] [])
