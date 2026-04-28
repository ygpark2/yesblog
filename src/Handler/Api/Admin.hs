{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Api.Admin where

import Import
import Handler.Api.Shared
import Handler.Api.ThemeSupport
import Database.Persist.Sql (fromSqlKey)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Time (addUTCTime)

getApiAdminDashboardR :: Handler Value
getApiAdminDashboardR = do
    _ <- requireAdminAuth
    recentArticles <- runDB $ selectList [] [Desc ArticleUpdatedAt, LimitTo 10]
    recentComments <- runDB $ selectList [] [Desc CommentPosted, LimitTo 10]
    recentUsers <- runDB $ selectList [] [Asc UserIdent, LimitTo 10]
    articleTags <- loadTagMap (map entityKey recentArticles)
    articleAuthors <- loadAuthorMap recentArticles
    articleCommentCounts <- loadCommentCountMap (map entityKey recentArticles)
    commentArticleMap <- loadCommentArticleMap recentComments
    userArticleStats <- loadUserArticleStats recentUsers
    articleCount <- runDB $ count ([] :: [Filter Article])
    commentCount <- runDB $ count ([] :: [Filter Comment])
    userCount <- runDB $ count ([] :: [Filter User])
    imageCount <- runDB $ count ([] :: [Filter Image])
    themeCount <- runDB $ count ([] :: [Filter Theme])
    themes <- runDB $ selectList [] [Asc ThemeName]
    returnJson $ object
        [ "meta" .= object
            [ "articleCount" .= articleCount
            , "commentCount" .= commentCount
            , "userCount" .= userCount
            , "imageCount" .= imageCount
            , "themeCount" .= themeCount
            ]
        , "articles" .= map (adminArticleValue articleTags articleAuthors articleCommentCounts) recentArticles
        , "comments" .= map (adminCommentValue commentArticleMap) recentComments
        , "users" .= map (adminUserValue userArticleStats) recentUsers
        , "themes" .= map themeValue themes
        ]

getApiAdminThemeReviewR :: Handler Value
getApiAdminThemeReviewR = do
    _ <- requireAdminAuth
    reviews <- runDB $ selectList [ThemeReviewStatus ==. "pending"] [Desc ThemeReviewUpdatedAt]
    let themeIds = map (themeReviewTheme . entityVal) reviews
        reviewerIds = mapMaybe (themeReviewReviewer . entityVal) reviews
    themeMap <- loadThemeMap themeIds
    reviewerMap <- loadReviewerMap reviewerIds
    returnJson $ object
        [ "items" .= map (themeReviewValue themeMap reviewerMap) reviews
        ]

getApiAdminThemeOrdersR :: Handler Value
getApiAdminThemeOrdersR = do
    _ <- requireAdminAuth
    orders <- runDB $ selectList [] [Desc ThemeOrderCreatedAt]
    let themeIds = map (themeOrderTheme . entityVal) orders
        userIds = map (themeOrderUser . entityVal) orders
    themeMap <- loadThemeMap themeIds
    userMap <- loadReviewerMap userIds
    returnJson $ object
        [ "items" .= map (themeOrderAdminValue themeMap userMap) orders
        ]

getApiAdminMembershipOrdersR :: Handler Value
getApiAdminMembershipOrdersR = do
    _ <- requireAdminAuth
    orders <- runDB $ selectList [] [Desc MembershipOrderCreatedAt]
    membershipMap <- loadMembershipMap $ map (membershipOrderMembership . entityVal) orders
    userMap <- loadReviewerMap $
        map (membershipOrderCreator . entityVal) orders <>
        map (membershipOrderMember . entityVal) orders
    returnJson $ object
        [ "items" .= map (membershipOrderValue membershipMap userMap) orders
        ]

getApiAdminThemePayoutsR :: Handler Value
getApiAdminThemePayoutsR = do
    _ <- requireAdminAuth
    payouts <- runDB $ selectList [] [Desc ThemePayoutRequestedAt]
    let userIds = map (themePayoutUser . entityVal) payouts
    userMap <- loadReviewerMap userIds
    let sellerGroups =
            foldl'
                (\acc payoutEntity@(Entity _ payout) ->
                    M.insertWith (<>) (themePayoutUser payout) [payoutEntity] acc
                )
                M.empty
                payouts
        sellerItems =
            map
                (\(userId, sellerPayouts) ->
                    let requestedCents =
                            sum $
                                map (themePayoutAmountCents . entityVal) $
                                    filter (\item -> themePayoutStatus (entityVal item) == "requested") sellerPayouts
                        paidCents =
                            sum $
                                map (themePayoutAmountCents . entityVal) $
                                    filter (\item -> themePayoutStatus (entityVal item) == "paid") sellerPayouts
                        rejectedCents =
                            sum $
                                map (themePayoutAmountCents . entityVal) $
                                    filter (\item -> themePayoutStatus (entityVal item) == "rejected") sellerPayouts
                        openRequestedCount =
                            length $ filter (\item -> themePayoutStatus (entityVal item) == "requested") sellerPayouts
                    in object
                        [ "user" .= fmap userValue (M.lookup userId userMap)
                        , "requestCount" .= length sellerPayouts
                        , "requestedCents" .= requestedCents
                        , "paidCents" .= paidCents
                        , "rejectedCents" .= rejectedCents
                        , "openRequestedCount" .= openRequestedCount
                        ]
                )
                (M.toList sellerGroups)
        requestedCentsTotal =
            sum $
                map (themePayoutAmountCents . entityVal) $
                    filter (\item -> themePayoutStatus (entityVal item) == "requested") payouts
        paidCentsTotal =
            sum $
                map (themePayoutAmountCents . entityVal) $
                    filter (\item -> themePayoutStatus (entityVal item) == "paid") payouts
        rejectedCentsTotal =
            sum $
                map (themePayoutAmountCents . entityVal) $
                    filter (\item -> themePayoutStatus (entityVal item) == "rejected") payouts
        openRequestedCountTotal =
            length $ filter (\item -> themePayoutStatus (entityVal item) == "requested") payouts
    returnJson $ object
        [ "summary" .= object
            [ "requestCount" .= length payouts
            , "requestedCents" .= requestedCentsTotal
            , "paidCents" .= paidCentsTotal
            , "rejectedCents" .= rejectedCentsTotal
            , "openRequestedCount" .= openRequestedCountTotal
            ]
        , "sellers" .= sellerItems
        , "items" .= map (themePayoutValue userMap) payouts
        ]

getApiAdminThemesR :: Handler Value
getApiAdminThemesR = do
    _ <- requireAdminAuth
    themes <- runDB $ selectList [] [Asc ThemeName]
    returnJson $ object ["items" .= map themeValue themes]

postApiAdminThemesR :: Handler Value
postApiAdminThemesR = do
    _ <- requireAdminAuth
    theme <- readThemeInput Nothing
    existingTheme <- runDB $ getBy $ UniqueThemeSlug $ themeSlug theme
    when (isJust existingTheme) $
        apiError status400 "That theme slug is already used."
    themeId <- runDB $ insert theme
    createdTheme <- Entity themeId <$> runDB (get404 themeId)
    returnJson $ object ["theme" .= themeValue createdTheme]

postApiAdminThemeUpdateR :: ThemeId -> Handler Value
postApiAdminThemeUpdateR themeId = do
    _ <- requireAdminAuth
    _ <- runDB $ get404 themeId
    theme <- readThemeInput (Just themeId)
    existingTheme <- runDB $ getBy $ UniqueThemeSlug $ themeSlug theme
    case existingTheme of
        Just (Entity existingThemeId _)
            | existingThemeId /= themeId ->
                apiError status400 "That theme slug is already used."
        _ -> pure ()
    now <- liftIO getCurrentTime
    runDB $ do
        replace themeId theme
        syncThemeReview themeId Nothing (themeStatus theme) Nothing now
    updatedTheme <- Entity themeId <$> runDB (get404 themeId)
    returnJson $ object ["theme" .= themeValue updatedTheme]

postApiAdminThemeApproveR :: ThemeId -> Handler Value
postApiAdminThemeApproveR themeId = do
    Entity adminId _ <- requireAdminAuth
    theme <- runDB $ get404 themeId
    now <- liftIO getCurrentTime
    runDB $ do
        update themeId
            [ ThemeStatus =. "published"
            , ThemeActive =. True
            , ThemeUpdatedAt =. now
            ]
        syncThemeReview themeId (Just adminId) "approved" (Just "Approved from review queue.") now
    returnJson $ object
        [ "theme" .= themeValue (Entity themeId (theme { themeStatus = "published", themeActive = True, themeUpdatedAt = now }))
        ]

postApiAdminThemeRejectR :: ThemeId -> Handler Value
postApiAdminThemeRejectR themeId = do
    Entity adminId _ <- requireAdminAuth
    _ <- runDB $ get404 themeId
    note <- T.strip <$> runInputPost (fromMaybe "" <$> iopt textField "note")
    when (note == "") $
        apiError status400 "Rejection note is required."
    now <- liftIO getCurrentTime
    runDB $ do
        update themeId
            [ ThemeStatus =. "suspended"
            , ThemeActive =. False
            , ThemeUpdatedAt =. now
            ]
        syncThemeReview themeId (Just adminId) "rejected" (Just note) now
    updatedTheme <- Entity themeId <$> runDB (get404 themeId)
    returnJson $ object ["theme" .= themeValue updatedTheme]

postApiAdminThemeDeleteR :: ThemeId -> Handler Value
postApiAdminThemeDeleteR themeId = do
    _ <- requireAdminAuth
    _ <- runDB $ get404 themeId
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

postApiAdminThemeOrderUpdateR :: ThemeOrderId -> Handler Value
postApiAdminThemeOrderUpdateR orderId = do
    _ <- requireAdminAuth
    desiredStatus <- normalizeOrderStatus <$> runInputPost (fromMaybe "pending" <$> iopt textField "status")
    now <- liftIO getCurrentTime
    order <- runDB $ get404 orderId
    let themeId = themeOrderTheme order
        userId = themeOrderUser order
    runDB $ do
        update orderId
            [ ThemeOrderStatus =. desiredStatus
            , ThemeOrderPaidAt =. if desiredStatus == "paid" then Just now else Nothing
            ]
        case desiredStatus of
            "paid" -> do
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
            _ ->
                deleteWhere [ThemePurchaseUser ==. userId, ThemePurchaseTheme ==. themeId]
    updatedOrder <- Entity orderId <$> runDB (get404 orderId)
    themeMap <- loadThemeMap [themeId]
    userMap <- loadReviewerMap [userId]
    returnJson $ object
        [ "order" .= themeOrderAdminValue themeMap userMap updatedOrder
        ]

postApiAdminMembershipOrderUpdateR :: MembershipOrderId -> Handler Value
postApiAdminMembershipOrderUpdateR orderId = do
    _ <- requireAdminAuth
    desiredStatus <- normalizeMembershipOrderStatus <$> runInputPost (fromMaybe "pending" <$> iopt textField "status")
    note <- T.strip <$> runInputPost (fromMaybe "" <$> iopt textField "note")
    when (desiredStatus `elem` ["failed", "cancelled"] && note == "") $
        apiError status400 "Failure or cancellation note is required."
    now <- liftIO getCurrentTime
    order <- runDB $ get404 orderId
    let membershipId = membershipOrderMembership order
        creatorId = membershipOrderCreator order
        memberId = membershipOrderMember order
        nextAdminNote =
            if note == ""
                then if desiredStatus `elem` ["pending", "paid"] then Nothing else membershipOrderAdminNote order
                else normalizeOptionalTextarea (Just note)
    runDB $ do
        update orderId
            [ MembershipOrderStatus =. desiredStatus
            , MembershipOrderPaidAt =. if desiredStatus == "paid" then Just now else Nothing
            , MembershipOrderAdminNote =. nextAdminNote
            ]
        case desiredStatus of
            "paid" ->
                update membershipId
                    [ MembershipStatus =. "active"
                    , MembershipStartedAt =. Just now
                    , MembershipExpiresAt =. Just (addUTCTime (60 * 60 * 24 * 30) now)
                    , MembershipUpdatedAt =. now
                    ]
            "failed" ->
                update membershipId
                    [ MembershipStatus =. "pending"
                    , MembershipUpdatedAt =. now
                    ]
            "cancelled" ->
                update membershipId
                    [ MembershipStatus =. "cancelled"
                    , MembershipUpdatedAt =. now
                    ]
            _ -> pure ()
    updatedOrder <- Entity orderId <$> runDB (get404 orderId)
    membershipMap <- loadMembershipMap [membershipId]
    userMap <- loadReviewerMap [creatorId, memberId]
    returnJson $ object
        [ "order" .= membershipOrderValue membershipMap userMap updatedOrder
        ]

postApiAdminThemePayoutUpdateR :: ThemePayoutId -> Handler Value
postApiAdminThemePayoutUpdateR payoutId = do
    _ <- requireAdminAuth
    desiredStatus <- normalizePayoutStatus <$> runInputPost (fromMaybe "requested" <$> iopt textField "status")
    note <- T.strip <$> runInputPost (fromMaybe "" <$> iopt textField "note")
    when (desiredStatus == "rejected" && note == "") $
        apiError status400 "Rejection note is required."
    now <- liftIO getCurrentTime
    payout <- runDB $ get404 payoutId
    let nextAdminNote =
            if note == ""
                then themePayoutAdminNote payout
                else normalizeOptionalTextarea (Just note)
    runDB $ update payoutId
        [ ThemePayoutStatus =. desiredStatus
        , ThemePayoutAdminNote =. nextAdminNote
        , ThemePayoutProcessedAt =. if desiredStatus `elem` ["paid", "rejected"] then Just now else Nothing
        ]
    updatedPayout <- Entity payoutId <$> runDB (get404 payoutId)
    let payoutUserId = themePayoutUser (entityVal updatedPayout)
    userMap <- loadReviewerMap [payoutUserId]
    returnJson $ object
        [ "payout" .= themePayoutValue userMap updatedPayout
        ]

loadThemeMap :: [ThemeId] -> Handler (M.Map ThemeId Theme)
loadThemeMap themeIds
    | null themeIds = pure M.empty
    | otherwise = do
        themes <- runDB $ selectList [ThemeId <-. L.nub themeIds] []
        pure $ M.fromList $ map (\(Entity themeId theme) -> (themeId, theme)) themes

loadReviewerMap :: [UserId] -> Handler (M.Map UserId User)
loadReviewerMap reviewerIds
    | null reviewerIds = pure M.empty
    | otherwise = do
        reviewers <- runDB $ selectList [UserId <-. L.nub reviewerIds] []
        pure $ M.fromList $ map (\(Entity userId user) -> (userId, user)) reviewers

syncThemeReview :: ThemeId -> Maybe UserId -> Text -> Maybe Text -> UTCTime -> ReaderT SqlBackend Handler ()
syncThemeReview _ _ "draft" _ _ = pure ()
syncThemeReview themeId mReviewerId status mNote now = do
    let reviewStatus =
            case status of
                "published" -> "approved"
                "suspended" -> "rejected"
                "review" -> "pending"
                other -> other
        reviewNote = fmap Textarea (cleanOptionalText mNote)
    existingReview <- getBy $ UniqueThemeReview themeId
    case existingReview of
        Nothing ->
            insert_ ThemeReview
                { themeReviewTheme = themeId
                , themeReviewReviewer = mReviewerId
                , themeReviewStatus = reviewStatus
                , themeReviewNote = reviewNote
                , themeReviewCreatedAt = now
                , themeReviewUpdatedAt = now
                }
        Just (Entity reviewId review) ->
            replace reviewId review
                { themeReviewReviewer = mReviewerId
                , themeReviewStatus = reviewStatus
                , themeReviewNote = reviewNote
                , themeReviewUpdatedAt = now
                }

normalizeOrderStatus :: Text -> Text
normalizeOrderStatus rawValue =
    let value = T.toLower $ T.strip rawValue
    in if value `elem` ["pending", "paid", "failed", "cancelled"]
        then value
        else "pending"

normalizePayoutStatus :: Text -> Text
normalizePayoutStatus rawValue =
    let value = T.toLower $ T.strip rawValue
    in if value `elem` ["requested", "paid", "rejected"]
        then value
        else "requested"

readThemeInput :: Maybe ThemeId -> Handler Theme
readThemeInput mThemeId = do
    existingTheme <- case mThemeId of
        Nothing -> pure Nothing
        Just themeId -> runDB $ get themeId
    now <- liftIO getCurrentTime
    draft <- readThemeDraftInput
    adminOptions <- readThemeAdminOptions
    pure $ buildAdminTheme existingTheme now draft adminOptions

normalizeMembershipOrderStatus :: Text -> Text
normalizeMembershipOrderStatus rawStatus =
    let status = T.toLower $ T.strip rawStatus
    in if status `elem` ["pending", "paid", "failed", "cancelled"]
        then status
        else "pending"

loadMembershipMap :: [MembershipId] -> Handler (M.Map MembershipId Membership)
loadMembershipMap membershipIds
    | null membershipIds = pure M.empty
    | otherwise = do
        memberships <- runDB $ selectList [MembershipId <-. L.nub membershipIds] []
        pure $ M.fromList $ map (\(Entity membershipId membership) -> (membershipId, membership)) memberships

postApiAdminArticleDeleteR :: ArticleId -> Handler Value
postApiAdminArticleDeleteR articleId = do
    _ <- requireAdminAuth
    deletedArticle <- runDB $ deleteArticleCascade articleId
    returnJson $ object
        [ "deletedArticleId" .= fromSqlKey articleId
        , "slug" .= articleSlug deletedArticle
        ]

postApiAdminCommentDeleteR :: CommentId -> Handler Value
postApiAdminCommentDeleteR commentId = do
    _ <- requireAdminAuth
    _ <- runDB $ get404 commentId
    runDB $ delete commentId
    returnJson $ object ["deletedCommentId" .= fromSqlKey commentId]

postApiAdminUserDeleteR :: UserId -> Handler Value
postApiAdminUserDeleteR userId = do
    Entity currentAdminId _ <- requireAdminAuth
    when (userId == currentAdminId) $
        apiError status400 "You cannot delete your own admin account."
    deletedUser <- runDB $ deleteUserCascade userId
    returnJson $ object
        [ "deletedUserId" .= fromSqlKey userId
        , "ident" .= userIdent deletedUser
        ]
