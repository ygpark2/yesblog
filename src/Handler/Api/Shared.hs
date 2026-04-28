{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Api.Shared where

import Import
import Helper.MakeBrief
import Helper.ImageForm (uploadDirectory, uploadSubDirectory)
import Database.Persist.Sql (fromSqlKey)
import qualified Data.Aeson.KeyMap as KM
import qualified Control.Monad as CM
import qualified Data.Char as Char
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Set as Set
import Data.Time (NominalDiffTime, defaultTimeLocale, diffUTCTime, formatTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Network.HTTP.Types.Status (Status, status400, status413, status415, status429)
import Network.Wai (Request (requestBodyLength), RequestBodyLength (KnownLength))
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.FilePath (takeExtension, (</>))

commentCooldownSeconds :: NominalDiffTime
commentCooldownSeconds = 15

commentCooldownSessionKey :: Text
commentCooldownSessionKey = "yesblog-last-comment-at"

maxUploadBytes :: Word64
maxUploadBytes = 8 * 1024 * 1024

allowedImageExtensions :: [String]
allowedImageExtensions = [".jpg", ".jpeg", ".png", ".gif", ".webp", ".svg"]

allowedImageContentTypes :: [Text]
allowedImageContentTypes =
    [ "image/jpeg"
    , "image/png"
    , "image/gif"
    , "image/webp"
    , "image/svg+xml"
    ]

normalizeUserPlan :: Text -> Text
normalizeUserPlan rawPlan =
    let value = T.toLower $ T.strip rawPlan
    in if value `elem` ["writer-pro", "designer-pro"]
        then value
        else "free"

isWriterProPlan :: Text -> Bool
isWriterProPlan plan = normalizeUserPlan plan `elem` ["writer-pro", "designer-pro"]

isDesignerProPlan :: Text -> Bool
isDesignerProPlan plan = normalizeUserPlan plan == "designer-pro"

userHasWriterPro :: User -> Bool
userHasWriterPro user = isWriterProPlan (userPlan user) || userIsAdmin user

userHasDesignerPro :: User -> Bool
userHasDesignerPro user = isDesignerProPlan (userPlan user) || userIsAdmin user

effectiveUserPlanAt :: UTCTime -> User -> Text
effectiveUserPlanAt now user
    | userIsAdmin user = "designer-pro"
    | otherwise =
        case userPlanExpiresAt user of
            Just expiresAt | expiresAt <= now -> "free"
            _ -> normalizeUserPlan (userPlan user)

userHasWriterProAt :: UTCTime -> User -> Bool
userHasWriterProAt now user = isWriterProPlan (effectiveUserPlanAt now user)

userHasDesignerProAt :: UTCTime -> User -> Bool
userHasDesignerProAt now user = isDesignerProPlan (effectiveUserPlanAt now user)

membershipStatusActiveAt :: UTCTime -> Membership -> Bool
membershipStatusActiveAt now membership =
    effectiveMembershipStatusAt now membership == "active" &&
    case membershipExpiresAt membership of
        Nothing -> True
        Just expiresAt -> expiresAt > now

effectiveMembershipStatusAt :: UTCTime -> Membership -> Text
effectiveMembershipStatusAt now membership
    | membershipStatus membership /= "active" = membershipStatus membership
    | maybe False (<= now) (membershipExpiresAt membership) =
        if membershipAutoRenew membership then "pending" else "expired"
    | otherwise = "active"

lookupMembershipFor :: UTCTime -> UserId -> UserId -> Handler (Maybe (Entity Membership))
lookupMembershipFor now creatorId memberId = do
    mMembership <- runDB $ getBy $ UniqueMembership creatorId memberId
    pure $
        case mMembership of
            Just (Entity membershipId membership) ->
                Just (Entity membershipId (membership { membershipStatus = effectiveMembershipStatusAt now membership }))
            Nothing -> Nothing

hasActiveMembershipFor :: UTCTime -> UserId -> UserId -> Handler Bool
hasActiveMembershipFor now creatorId memberId = do
    mMembership <- runDB $ getBy $ UniqueMembership creatorId memberId
    pure $ maybe False (membershipStatusActiveAt now . entityVal) mMembership

selectPublishedArticles :: Maybe Text -> Maybe Text -> Maybe Text -> Handler [Entity Article]
selectPublishedArticles mTag mAuthorIdent mQuery = do
    now <- liftIO getCurrentTime
    mAuthorId <- case mAuthorIdent of
        Nothing -> pure Nothing
        Just ident -> entityKey <$> runDB (getBy404 $ UniqueUser ident) >>= pure . Just
    let authorFilters =
            [ArticleDraft !=. True] <>
            maybe [] (\authorId -> [ArticleAuthor ==. authorId]) mAuthorId
    baseArticles <- runDB $ selectList authorFilters [Desc ArticleUpdatedAt]
    let publishFilteredArticles = filter (isArticlePubliclyVisible now . entityVal) baseArticles
        queryFilteredArticles = applyQueryFilter mQuery publishFilteredArticles
    case mTag of
        Nothing -> pure queryFilteredArticles
        Just tagName' -> do
            taggedArticleIds <- runDB $
                map (tagArticle . entityVal) <$> selectList [TagName ==. tagName'] []
            pure $ filter (\entity -> entityKey entity `elem` taggedArticleIds) queryFilteredArticles

selectPublishedArticlesPage :: Maybe Text -> Maybe Text -> Maybe Text -> Int -> Int -> Handler ([Entity Article], Int)
selectPublishedArticlesPage mTag mAuthorIdent mQuery page limit = do
    now <- liftIO getCurrentTime
    mAuthorId <- case mAuthorIdent of
        Nothing -> pure Nothing
        Just ident -> entityKey <$> runDB (getBy404 $ UniqueUser ident) >>= pure . Just
    taggedArticleIds <- case mTag of
        Nothing -> pure Nothing
        Just tagName' -> do
            articleIds <- runDB $ map (tagArticle . entityVal) <$> selectList [TagName ==. tagName'] []
            pure $ Just (Set.fromList articleIds)
    let dbFilters =
            [ArticleDraft !=. True] <>
            maybe [] (\authorId -> [ArticleAuthor ==. authorId]) mAuthorId
        targetOffset = (page - 1) * limit
        chunkSize = max 50 (limit * 3)
        isVisible entity@(Entity articleId article) =
            isArticlePubliclyVisible now article &&
            maybe True (Set.member articleId) taggedArticleIds
    collectPage dbFilters isVisible mQuery targetOffset limit chunkSize 0 0 []

collectPage
    :: [Filter Article]
    -> (Entity Article -> Bool)
    -> Maybe Text
    -> Int
    -> Int
    -> Int
    -> Int
    -> Int
    -> [Entity Article]
    -> Handler ([Entity Article], Int)
collectPage dbFilters includeEntity mQuery targetOffset limit chunkSize dbOffset matchedCount collected = do
    chunk <- runDB $ selectList dbFilters [Desc ArticleUpdatedAt, OffsetBy dbOffset, LimitTo chunkSize]
    if null chunk
        then pure (collected, matchedCount)
        else do
            let filteredChunk = applyQueryFilter mQuery $ filter includeEntity chunk
                matchedCount' = matchedCount + length filteredChunk
                offsetWithinChunk = max 0 (targetOffset - matchedCount)
                pageSlice = take (limit - length collected) $ drop offsetWithinChunk filteredChunk
                collected' = collected <> pageSlice
            collectPage dbFilters includeEntity mQuery targetOffset limit chunkSize (dbOffset + chunkSize) matchedCount' collected'

applyQueryFilter :: Maybe Text -> [Entity Article] -> [Entity Article]
applyQueryFilter Nothing articles = articles
applyQueryFilter (Just query) articles =
    let lowered = T.toLower query
    in filter
        (\(Entity _ article) ->
            let titleMatches = lowered `T.isInfixOf` T.toLower (articleTitle article)
                contentMatches = lowered `T.isInfixOf` T.toLower (markdownToText $ articleContent article)
            in titleMatches || contentMatches
        )
        articles

cleanOptionalText :: Maybe Text -> Maybe Text
cleanOptionalText = fmap T.strip >=> \value ->
    if value == "" then Nothing else Just value

loadTagMap :: [ArticleId] -> Handler (M.Map ArticleId [Text])
loadTagMap articleIds
    | null articleIds = pure M.empty
    | otherwise = do
        tagEntities <- runDB $ selectList [TagArticle <-. articleIds] [Asc TagName]
        pure $
            foldl'
                (\acc (Entity _ tag) -> M.insertWith (<>) (tagArticle tag) [tagName tag] acc)
                M.empty
                tagEntities

loadAuthorMap :: [Entity Article] -> Handler (M.Map UserId User)
loadAuthorMap articles
    | null articles = pure M.empty
    | otherwise = do
        let authorIds = L.nub $ map (articleAuthor . entityVal) articles
        authorEntities <- runDB $ selectList [UserId <-. authorIds] []
        pure $ M.fromList $ map (\(Entity key user) -> (key, user)) authorEntities

loadCommentCountMap :: [ArticleId] -> Handler (M.Map ArticleId Int)
loadCommentCountMap articleIds
    | null articleIds = pure M.empty
    | otherwise = do
        commentEntities <- runDB $ selectList [CommentArticle <-. articleIds] []
        pure $
            foldl'
                (\acc (Entity _ comment) -> M.insertWith (+) (commentArticle comment) 1 acc)
                M.empty
                commentEntities

loadCommentArticleMap :: [Entity Comment] -> Handler (M.Map ArticleId Article)
loadCommentArticleMap comments
    | null comments = pure M.empty
    | otherwise = do
        let articleIds = L.nub $ map (commentArticle . entityVal) comments
        articleEntities <- runDB $ selectList [ArticleId <-. articleIds] []
        pure $ M.fromList $ map (\(Entity articleId article) -> (articleId, article)) articleEntities

loadUserArticleStats :: [Entity User] -> Handler (M.Map UserId (Int, Int))
loadUserArticleStats users
    | null users = pure M.empty
    | otherwise = do
        let userIds = map entityKey users
        articles <- runDB $ selectList [ArticleAuthor <-. userIds] []
        pure $
            foldl'
                (\acc (Entity _ article) ->
                    let nextValue =
                            case M.lookup (articleAuthor article) acc of
                                Nothing ->
                                    if articleDraft article then (0, 1) else (1, 0)
                                Just (publishedCount, draftCount) ->
                                    if articleDraft article
                                        then (publishedCount, draftCount + 1)
                                        else (publishedCount + 1, draftCount)
                    in M.insert (articleAuthor article) nextValue acc
                )
                M.empty
                articles

articleSummaryValue :: M.Map ArticleId [Text] -> M.Map UserId User -> Entity Article -> Value
articleSummaryValue tagMap authorMap (Entity articleId article) =
    object
        [ "id" .= fromSqlKey articleId
        , "title" .= articleTitle article
        , "slug" .= articleSlug article
        , "excerpt" .= makeBrief 220 (markdownToText $ articleContent article)
        , "tags" .= fromMaybe [] (M.lookup articleId tagMap)
        , "createdAt" .= isoTime (articleCreatedAt article)
        , "updatedAt" .= isoTime (articleUpdatedAt article)
        , "readingMinutes" .= readingMinutes article
        , "visibility" .= articleVisibility article
        , "publishAt" .= fmap isoTime (articlePublishAt article)
        , "author" .= maybe Null userValue (M.lookup (articleAuthor article) authorMap)
        ]

articleDetailValue :: Maybe (Entity User) -> Entity Article -> User -> [Text] -> [Entity Comment] -> Value
articleDetailValue mviewer (Entity articleId article) author tags comments =
    object
        [ "id" .= fromSqlKey articleId
        , "title" .= articleTitle article
        , "slug" .= articleSlug article
        , "content" .= markdownToText (articleContent article)
        , "excerpt" .= makeBrief 220 (markdownToText $ articleContent article)
        , "tags" .= tags
        , "createdAt" .= isoTime (articleCreatedAt article)
        , "updatedAt" .= isoTime (articleUpdatedAt article)
        , "readingMinutes" .= readingMinutes article
        , "visibility" .= articleVisibility article
        , "publishAt" .= fmap isoTime (articlePublishAt article)
        , "author" .= userValue author
        , "viewer" .= fmap (userValue . entityVal) mviewer
        , "comments" .= map (commentValue (userIdent . entityVal <$> mviewer)) comments
        ]

articleDetailValueWithTheme :: Maybe (Entity User) -> Entity Article -> User -> Maybe (Entity Theme) -> [Text] -> [Entity Comment] -> Value
articleDetailValueWithTheme mviewer (Entity articleId article) author mTheme tags comments =
    object
        [ "id" .= fromSqlKey articleId
        , "title" .= articleTitle article
        , "slug" .= articleSlug article
        , "content" .= markdownToText (articleContent article)
        , "excerpt" .= makeBrief 220 (markdownToText $ articleContent article)
        , "tags" .= tags
        , "createdAt" .= isoTime (articleCreatedAt article)
        , "updatedAt" .= isoTime (articleUpdatedAt article)
        , "readingMinutes" .= readingMinutes article
        , "visibility" .= articleVisibility article
        , "publishAt" .= fmap isoTime (articlePublishAt article)
        , "author" .= userValueWithTheme author mTheme
        , "viewer" .= fmap (userValue . entityVal) mviewer
        , "comments" .= map (commentValue (userIdent . entityVal <$> mviewer)) comments
        ]

userValue :: User -> Value
userValue user =
    object
        [ "ident" .= userIdent user
        , "displayName" .= fromMaybe (userIdent user) (userDisplayName user)
        , "bio" .= fmap unTextarea (userBio user)
        , "isAdmin" .= userIsAdmin user
        , "plan" .= userPlan user
        , "planExpiresAt" .= fmap isoTime (userPlanExpiresAt user)
        , "membershipPriceCents" .= userMembershipPriceCents user
        , "themeId" .= fmap fromSqlKey (userTheme user)
        , "themeOverrides" .= userThemeOverrides user
        ]

userValueWithTheme :: User -> Maybe (Entity Theme) -> Value
userValueWithTheme user mTheme =
    object
        [ "ident" .= userIdent user
        , "displayName" .= fromMaybe (userIdent user) (userDisplayName user)
        , "bio" .= fmap unTextarea (userBio user)
        , "isAdmin" .= userIsAdmin user
        , "plan" .= userPlan user
        , "planExpiresAt" .= fmap isoTime (userPlanExpiresAt user)
        , "membershipPriceCents" .= userMembershipPriceCents user
        , "themeId" .= fmap fromSqlKey (userTheme user)
        , "themeOverrides" .= userThemeOverrides user
        , "theme" .= fmap themeValue mTheme
        ]

membershipValue :: Maybe User -> Maybe User -> Entity Membership -> Value
membershipValue mCreator mMember (Entity membershipId membership) =
    object
        [ "id" .= fromSqlKey membershipId
        , "status" .= membershipStatus membership
        , "priceCents" .= membershipPriceCents membership
        , "autoRenew" .= membershipAutoRenew membership
        , "startedAt" .= fmap isoTime (membershipStartedAt membership)
        , "expiresAt" .= fmap isoTime (membershipExpiresAt membership)
        , "createdAt" .= isoTime (membershipCreatedAt membership)
        , "updatedAt" .= isoTime (membershipUpdatedAt membership)
        , "creator" .= fmap userValue mCreator
        , "member" .= fmap userValue mMember
        ]

membershipOrderValue :: M.Map MembershipId Membership -> M.Map UserId User -> Entity MembershipOrder -> Value
membershipOrderValue membershipMap userMap (Entity orderId order) =
    object
        [ "id" .= fromSqlKey orderId
        , "amountCents" .= membershipOrderAmountCents order
        , "status" .= membershipOrderStatus order
        , "provider" .= membershipOrderProvider order
        , "providerOrderId" .= membershipOrderProviderOrderId order
        , "adminNote" .= fmap unTextarea (membershipOrderAdminNote order)
        , "createdAt" .= isoTime (membershipOrderCreatedAt order)
        , "paidAt" .= fmap isoTime (membershipOrderPaidAt order)
        , "membershipId" .= fromSqlKey (membershipOrderMembership order)
        , "membership" .= fmap (\membership -> membershipValue (M.lookup (membershipCreator membership) userMap) (M.lookup (membershipMember membership) userMap) (Entity (membershipOrderMembership order) membership)) (M.lookup (membershipOrderMembership order) membershipMap)
        , "creator" .= fmap userValue (M.lookup (membershipOrderCreator order) userMap)
        , "member" .= fmap userValue (M.lookup (membershipOrderMember order) userMap)
        ]

membershipAccessValue :: User -> Maybe (Entity Membership) -> UTCTime -> Value
membershipAccessValue creator mMembership now =
    object
        [ "enabled" .= (userMembershipPriceCents creator > 0)
        , "priceCents" .= userMembershipPriceCents creator
        , "viewerStatus" .= fmap (effectiveMembershipStatusAt now . entityVal) mMembership
        , "active" .= maybe False (membershipStatusActiveAt now . entityVal) mMembership
        , "startedAt" .= (fmap isoTime =<< (membershipStartedAt . entityVal <$> mMembership))
        , "expiresAt" .= (fmap isoTime =<< (membershipExpiresAt . entityVal <$> mMembership))
        ]

themeValue :: Entity Theme -> Value
themeValue (Entity themeId theme) =
    object
        [ "id" .= fromSqlKey themeId
        , "name" .= themeName theme
        , "slug" .= themeSlug theme
        , "description" .= fmap unTextarea (themeDescription theme)
        , "backgroundColor" .= themeBackgroundColor theme
        , "surfaceColor" .= themeSurfaceColor theme
        , "textColor" .= themeTextColor theme
        , "accentColor" .= themeAccentColor theme
        , "headingFont" .= themeHeadingFont theme
        , "bodyFont" .= themeBodyFont theme
        , "headerTemplate" .= fmap unTextarea (themeHeaderTemplate theme)
        , "bodyTemplate" .= fmap unTextarea (themeBodyTemplate theme)
        , "footerTemplate" .= fmap unTextarea (themeFooterTemplate theme)
        , "customCss" .= fmap unTextarea (themeCustomCss theme)
        , "authorId" .= fmap fromSqlKey (themeAuthor theme)
        , "parentId" .= fmap fromSqlKey (themeParent theme)
        , "priceCents" .= themePriceCents theme
        , "status" .= themeStatus theme
        , "license" .= themeLicense theme
        , "active" .= themeActive theme
        , "createdAt" .= isoTime (themeCreatedAt theme)
        , "updatedAt" .= isoTime (themeUpdatedAt theme)
        ]

themeMarketplaceValue :: Maybe UserId -> [ThemeId] -> Entity Theme -> Value
themeMarketplaceValue mUserId ownedThemeIds entity@(Entity themeId theme) =
    let isOwnTheme = isJust mUserId && themeAuthor theme == mUserId
        owned = themePriceCents theme == 0 || themeId `elem` ownedThemeIds || isOwnTheme
    in object
        [ "theme" .= themeValue entity
        , "owned" .= owned
        , "canUse" .= owned
        , "canFork" .= owned
        , "isAuthor" .= isOwnTheme
        , "canEdit" .= isOwnTheme
        , "canDelete" .= isOwnTheme
        ]

themeOrderValue :: M.Map ThemeId Theme -> Entity ThemeOrder -> Value
themeOrderValue themeMap (Entity orderId order) =
    object
        [ "id" .= fromSqlKey orderId
        , "amountCents" .= themeOrderAmountCents order
        , "status" .= themeOrderStatus order
        , "provider" .= themeOrderProvider order
        , "providerOrderId" .= themeOrderProviderOrderId order
        , "createdAt" .= isoTime (themeOrderCreatedAt order)
        , "paidAt" .= fmap isoTime (themeOrderPaidAt order)
        , "theme" .= fmap (\theme -> themeValue (Entity (themeOrderTheme order) theme)) (M.lookup (themeOrderTheme order) themeMap)
        ]

themeOrderAdminValue :: M.Map ThemeId Theme -> M.Map UserId User -> Entity ThemeOrder -> Value
themeOrderAdminValue themeMap userMap orderEntity@(Entity _ order) =
    case themeOrderValue themeMap orderEntity of
        Object baseObject ->
            Object $ KM.insert "user" (toJSON $ fmap userValue (M.lookup (themeOrderUser order) userMap)) baseObject
        other -> other

themeReviewValue :: M.Map ThemeId Theme -> M.Map UserId User -> Entity ThemeReview -> Value
themeReviewValue themeMap reviewerMap (Entity reviewId review) =
    object
        [ "id" .= fromSqlKey reviewId
        , "status" .= themeReviewStatus review
        , "note" .= fmap unTextarea (themeReviewNote review)
        , "createdAt" .= isoTime (themeReviewCreatedAt review)
        , "updatedAt" .= isoTime (themeReviewUpdatedAt review)
        , "theme" .= fmap (\theme -> themeValue (Entity (themeReviewTheme review) theme)) (M.lookup (themeReviewTheme review) themeMap)
        , "reviewer" .= fmap userValue (themeReviewReviewer review >>= (`M.lookup` reviewerMap))
        ]

themePayoutValue :: M.Map UserId User -> Entity ThemePayout -> Value
themePayoutValue userMap (Entity payoutId payout) =
    object
        [ "id" .= fromSqlKey payoutId
        , "amountCents" .= themePayoutAmountCents payout
        , "status" .= themePayoutStatus payout
        , "sellerNote" .= fmap unTextarea (themePayoutSellerNote payout)
        , "adminNote" .= fmap unTextarea (themePayoutAdminNote payout)
        , "requestedAt" .= isoTime (themePayoutRequestedAt payout)
        , "processedAt" .= fmap isoTime (themePayoutProcessedAt payout)
        , "user" .= fmap userValue (M.lookup (themePayoutUser payout) userMap)
        ]

loadUserTheme :: User -> Handler (Maybe (Entity Theme))
loadUserTheme user =
    case userTheme user of
        Nothing -> pure Nothing
        Just themeId -> runDB $ getEntity themeId

adminArticleValue :: M.Map ArticleId [Text] -> M.Map UserId User -> M.Map ArticleId Int -> Entity Article -> Value
adminArticleValue tagMap authorMap commentCountMap (Entity articleId article) =
    object
        [ "id" .= fromSqlKey articleId
        , "title" .= articleTitle article
        , "slug" .= articleSlug article
        , "draft" .= articleDraft article
        , "updatedAt" .= isoTime (articleUpdatedAt article)
        , "createdAt" .= isoTime (articleCreatedAt article)
        , "tags" .= fromMaybe [] (M.lookup articleId tagMap)
        , "commentCount" .= fromMaybe 0 (M.lookup articleId commentCountMap)
        , "author" .= maybe Null userValue (M.lookup (articleAuthor article) authorMap)
        ]

adminCommentValue :: M.Map ArticleId Article -> Entity Comment -> Value
adminCommentValue articleMap (Entity commentId comment) =
    object
        [ "id" .= fromSqlKey commentId
        , "name" .= commentName comment
        , "content" .= commentContent comment
        , "posted" .= isoTime (commentPosted comment)
        , "article" .= fmap adminCommentArticleValue (M.lookup (commentArticle comment) articleMap)
        ]

adminCommentArticleValue :: Article -> Value
adminCommentArticleValue article =
    object
        [ "title" .= articleTitle article
        , "slug" .= articleSlug article
        , "draft" .= articleDraft article
        ]

adminUserValue :: M.Map UserId (Int, Int) -> Entity User -> Value
adminUserValue statsMap (Entity userId user) =
    let (publishedCount, draftCount) = fromMaybe (0, 0) (M.lookup userId statsMap)
    in object
        [ "id" .= fromSqlKey userId
        , "ident" .= userIdent user
        , "displayName" .= fromMaybe (userIdent user) (userDisplayName user)
        , "bio" .= fmap unTextarea (userBio user)
        , "isAdmin" .= userIsAdmin user
        , "publishedCount" .= publishedCount
        , "draftCount" .= draftCount
        ]

commentValue :: Maybe Text -> Entity Comment -> Value
commentValue mViewerIdent (Entity commentId comment) =
    object
        [ "id" .= fromSqlKey commentId
        , "name" .= commentName comment
        , "content" .= commentContent comment
        , "posted" .= isoTime (commentPosted comment)
        , "canManage" .= maybe False (== commentName comment) mViewerIdent
        ]

requireCommentManager :: CommentId -> Handler Text
requireCommentManager commentId = do
    Entity _ currentUser <- requireAuth
    comment <- runDB $ get404 commentId
    if userIsAdmin currentUser || userIdent currentUser == commentName comment
        then pure (userIdent currentUser)
        else permissionDenied "You can manage only your own comments."

requireAdminAuth :: Handler (Entity User)
requireAdminAuth = do
    entity@(Entity _ currentUser) <- requireAuth
    if userIsAdmin currentUser
        then pure entity
        else permissionDenied "Admin access required."

requireAdmin :: Handler (Entity User)
requireAdmin = do
    entity@(Entity _ user) <- requireAuth
    if userIsAdmin user
        then pure entity
        else permissionDenied "Admin access required"

apiError :: Status -> Text -> Handler a
apiError status message =
    sendResponseStatus status $ object ["message" .= message]

enforceCommentCooldown :: UTCTime -> Handler ()
enforceCommentCooldown now = do
    mLastPostedAt <- lookupSession commentCooldownSessionKey
    case mLastPostedAt >>= parseCommentTimestamp of
        Just lastPostedAt
            | diffUTCTime now lastPostedAt < commentCooldownSeconds ->
                apiError status429 "Please wait a few seconds before posting another comment."
        _ -> pure ()

rememberCommentTimestamp :: UTCTime -> Handler ()
rememberCommentTimestamp now =
    setSession commentCooldownSessionKey $
        tshow (floor (utcTimeToPOSIXSeconds now) :: Integer)

parseCommentTimestamp :: Text -> Maybe UTCTime
parseCommentTimestamp rawValue = do
    (seconds, rest) <- either (const Nothing) Just (TR.decimal rawValue)
    guard (T.null rest)
    pure $ posixSecondsToUTCTime (fromIntegral (seconds :: Integer))

validateUploadRequestSize :: Handler ()
validateUploadRequestSize = do
    request <- waiRequest
    case requestBodyLength request of
        KnownLength bytes
            | bytes > maxUploadBytes ->
                apiError status413 "Images must be 8 MB or smaller."
        _ -> pure ()

validateUploadedImage :: FileInfo -> Handler ()
validateUploadedImage uploadedFile = do
    let originalName = T.unpack $ fileName uploadedFile
        extension = map Char.toLower (takeExtension originalName)
        contentType = T.toLower (fileContentType uploadedFile)
    when (extension `notElem` allowedImageExtensions) $
        apiError status415 "Unsupported image format. Use jpg, png, gif, webp, or svg."
    when (contentType `notElem` allowedImageContentTypes) $
        apiError status415 "Unsupported image content type."

loadRecentImages :: Handler [Entity Image]
loadRecentImages = runDB $ selectList [ImageFilename !=. ""] [Desc ImageDate, LimitTo 12]

imagePublicPath :: String -> Text
imagePublicPath filename = pack ("/static/files/" <> filename)

editorImageSnippet :: String -> Text
editorImageSnippet filename = T.concat ["![](\"", imagePublicPath filename, "\")"]

previewableImage :: String -> Bool
previewableImage filename =
    takeExtension filename `elem` [".jpg", ".jpeg", ".png", ".gif", ".webp", ".svg"]

lookupAutosaveArticleId :: Handler (Maybe ArticleId)
lookupAutosaveArticleId = do
    mArticleIdText <- runInputPost $ iopt textField "autosaveArticleId"
    pure $ mArticleIdText >>= fromPathPiece

defaultDraftTitle :: Text -> Text
defaultDraftTitle inputTitle =
    if T.strip inputTitle == ""
        then "Untitled draft"
        else inputTitle

normalizeSlug :: Text -> Text
normalizeSlug source =
    let normalized =
            T.intercalate "-" $
                filter (/= "") $
                    map
                        (T.filter Char.isAlphaNum)
                        (T.words $ T.map (\char -> if Char.isAlphaNum char then char else ' ') $ T.toLower source)
    in if normalized == "" then "draft" else normalized

ensureUniqueSlug :: Maybe ArticleId -> Text -> Handler Text
ensureUniqueSlug mCurrentArticleId sourceSlug = do
    let baseSlug = normalizeSlug sourceSlug
    findAvailableSlug baseSlug 1
  where
    findAvailableSlug :: Text -> Int -> Handler Text
    findAvailableSlug baseSlug attempt = do
        let candidateSlug =
                if attempt <= 1
                    then baseSlug
                    else baseSlug <> "-" <> tshow attempt
        mExisting <- runDB $ getBy $ UniqueSlug candidateSlug
        case mExisting of
            Nothing -> pure candidateSlug
            Just (Entity existingArticleId _) ->
                if Just existingArticleId == mCurrentArticleId
                    then pure candidateSlug
                    else findAvailableSlug baseSlug (attempt + 1)

loadOwnedArticle :: ArticleId -> Handler (Entity User, Article, [Text])
loadOwnedArticle articleId = do
    entity@(Entity currentUserId _) <- requireAuth
    (article, oldTags) <- runDB $ do
        post <- get404 articleId
        tags <- map (\(Entity _ tag) -> tagName tag) <$> selectList [TagArticle ==. articleId] [Asc TagName]
        pure (post, tags)
    when (articleAuthor article /= currentUserId) $
        permissionDenied "You can only edit your own articles."
    pure (entity, article, oldTags)

requireOwnedArticle :: ArticleId -> Handler Article
requireOwnedArticle articleId = do
    (Entity currentUserId _) <- requireAuth
    article <- runDB $ get404 articleId
    when (articleAuthor article /= currentUserId) $
        permissionDenied "You can only edit your own articles."
    pure article

getPageParam :: Handler Int
getPageParam = do
    page <- runInputGet $ fromMaybe 1 <$> iopt intField ("page" :: Text)
    pure $ max 1 page

redirectFrontendApp :: [Text] -> [(Text, Text)] -> Handler a
redirectFrontendApp pathPieces params = do
    render <- getUrlRenderParams
    redirect (render (FrontendAppPathR pathPieces) params)

readingMinutes :: Article -> Int
readingMinutes article =
    max 1 $
        ceiling ((fromIntegral (length (T.words (markdownToText $ articleContent article))) :: Double) / 220.0)

isoTime :: UTCTime -> Text
isoTime = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

lookupOptionalArticleIdParam :: Text -> Handler (Maybe ArticleId)
lookupOptionalArticleIdParam paramName = do
    mArticleIdText <- runInputPost $ iopt textField paramName
    pure $ mArticleIdText >>= fromPathPiece

normalizeTags :: Text -> [Text]
normalizeTags =
    filter (/= "") . T.words . T.map (\char -> if char == ',' then ' ' else char)

parseBoolText :: Text -> Bool
parseBoolText rawValue =
    T.toLower (T.strip rawValue) `elem` ["1", "true", "yes", "on"]

normalizeOptionalTextarea :: Maybe Text -> Maybe Textarea
normalizeOptionalTextarea =
    fmap Textarea >=> \value ->
        if T.strip (unTextarea value) == ""
            then Nothing
            else Just value

nonEmptyText :: Text -> Maybe Text
nonEmptyText value =
    let stripped = T.strip value
    in if stripped == "" then Nothing else Just stripped

validateThemeHtmlTemplate :: Text -> Handler ()
validateThemeHtmlTemplate rawTemplate = do
    let template = T.strip rawTemplate
    unless (template == "") $ do
        let lowered = T.toLower template
        when (containsAny lowered ["<script", "<iframe", "<object", "<embed", "<form", "<input", "<button", "<textarea", "<select", "<option", "<link", "<meta", "<base", "<img", "<svg", "<math"]) $
            apiError status400 "Theme templates may only use safe structural HTML."
        when ("style=" `T.isInfixOf` lowered || "src=" `T.isInfixOf` lowered) $
            apiError status400 "Theme templates cannot use inline styles or source attributes."
        when ("javascript:" `T.isInfixOf` lowered || "data:text/html" `T.isInfixOf` lowered) $
            apiError status400 "Theme templates cannot embed executable URLs."
        let tags = extractHtmlTags template
            invalidTags = filter (`Set.notMember` allowedThemeTags) tags
        unless (null invalidTags) $
            apiError status400 "Theme templates include unsupported HTML tags."
        when (any (hasUnsafeHref template) tags) $
            apiError status400 "Theme template links must be relative or http/https/mailto URLs."

validateThemeCss :: Text -> Handler ()
validateThemeCss rawCss = do
    let css = T.toLower $ T.strip rawCss
    unless (css == "") $
        when (containsAny css ["@import", "url(", "expression(", "javascript:", "-moz-binding", "behavior:", "</style", "<style"]) $
            apiError status400 "Theme CSS contains unsupported constructs."

allowedThemeTags :: Set.Set Text
allowedThemeTags =
    Set.fromList
        [ "a", "article", "blockquote", "code", "div", "em", "footer", "h1", "h2", "h3", "h4"
        , "header", "hr", "li", "main", "nav", "ol", "p", "pre", "section", "small", "span"
        , "strong", "time", "ul"
        ]

extractHtmlTags :: Text -> [Text]
extractHtmlTags template =
    mapMaybe extractTagName $ T.splitOn "<" template

extractTagName :: Text -> Maybe Text
extractTagName chunk =
    let stripped = T.strip chunk
    in if stripped == "" || T.isPrefixOf "!" stripped || T.isPrefixOf "?" stripped || T.isPrefixOf "/" stripped
        then if T.isPrefixOf "/" stripped
            then extractTagName (T.drop 1 stripped)
            else Nothing
        else
            let name = T.takeWhile (\char -> Char.isAlphaNum char || char == '-') stripped
            in nonEmptyText name

hasUnsafeHref :: Text -> Text -> Bool
hasUnsafeHref _ tagName
    | tagName /= "a" = False
hasUnsafeHref template _ =
    any isUnsafeHref $ mapMaybe extractHrefValue (T.splitOn "<a" template)

extractHrefValue :: Text -> Maybe Text
extractHrefValue chunk = do
    let lowered = T.toLower chunk
    hrefStart <- T.stripPrefix "href=" =<< findAttributeStart lowered "href="
    case T.uncons hrefStart of
        Just ('"', rest) -> Just $ T.takeWhile (/= '"') rest
        Just ('\'', rest) -> Just $ T.takeWhile (/= '\'') rest
        _ -> Just $ T.takeWhile (\char -> not (Char.isSpace char) && char /= '>') hrefStart

findAttributeStart :: Text -> Text -> Maybe Text
findAttributeStart haystack needle =
    snd <$> T.breakOnAll needle haystack L.!? 0

isUnsafeHref :: Text -> Bool
isUnsafeHref rawHref =
    let href = T.toLower $ T.strip rawHref
    in href /= "" &&
        not
            ( "/" `T.isPrefixOf` href
            || "#" `T.isPrefixOf` href
            || "http://" `T.isPrefixOf` href
            || "https://" `T.isPrefixOf` href
            || "mailto:" `T.isPrefixOf` href
            )

containsAny :: Text -> [Text] -> Bool
containsAny haystack = any (`T.isInfixOf` haystack)

writeUploadedImage :: FileInfo -> Handler String
writeUploadedImage uploadedFile = do
    now <- liftIO getCurrentTime
    let originalName = T.unpack $ fileName uploadedFile
        extension = map Char.toLower (takeExtension originalName)
        baseName = sanitizeFilename originalName
        timestamp = formatTime defaultTimeLocale "%Y%m%d%H%M%S%q" now
        finalName = baseName <> "-" <> timestamp <> extension
        uploadPath = uploadDirectory </> uploadSubDirectory
    liftIO $ createDirectoryIfMissing True uploadPath
    liftIO $ fileMove uploadedFile (uploadPath </> finalName)
    pure finalName

deleteArticleCascade articleId = do
    article <- get404 articleId
    deleteWhere [CommentArticle ==. articleId]
    deleteWhere [TagArticle ==. articleId]
    delete articleId
    pure article

deleteUserCascade userId = do
    user <- get404 userId
    articleIds <- map entityKey <$> selectList [ArticleAuthor ==. userId] []
    CM.forM_ articleIds $ \articleId -> do
        deleteWhere [CommentArticle ==. articleId]
        deleteWhere [TagArticle ==. articleId]
    deleteWhere [ArticleAuthor ==. userId]
    delete userId
    pure user

sanitizeFilename :: String -> String
sanitizeFilename rawName =
    let stem = reverse $ dropWhile (/= '.') $ reverse rawName
        cleaned =
            map
                (\char -> if Char.isAlphaNum char then Char.toLower char else '-')
                (take (max 0 (length rawName - length stem)) rawName)
        compacted = collapseDashes cleaned
        trimmed = trimDashes compacted
    in if null trimmed then "image" else trimmed

collapseDashes :: String -> String
collapseDashes [] = []
collapseDashes ('-':'-':rest) = collapseDashes ('-':rest)
collapseDashes (char:rest) = char : collapseDashes rest

trimDashes :: String -> String
trimDashes = reverse . dropWhile (== '-') . reverse . dropWhile (== '-')

imageValue :: Entity Image -> Value
imageValue (Entity imageId image) =
    object
        [ "id" .= fromSqlKey imageId
        , "filename" .= imageFilename image
        , "publicUrl" .= imagePublicPath (imageFilename image)
        , "markdown" .= editorImageSnippet (imageFilename image)
        , "description" .= fmap unTextarea (imageDescription image)
        , "previewable" .= previewableImage (imageFilename image)
        ]
isArticlePubliclyVisible :: UTCTime -> Article -> Bool
isArticlePubliclyVisible now article =
    articleVisibility article == "public" &&
    case articlePublishAt article of
        Nothing -> True
        Just publishAt -> publishAt <= now

isArticleVisibleToViewer :: UTCTime -> Maybe UserId -> Bool -> Article -> Bool
isArticleVisibleToViewer now mViewerId isAdminViewer article
    | isAdminViewer = True
    | maybe False (== articleAuthor article) mViewerId = True
    | otherwise =
        case articleVisibility article of
            "private" -> False
            "members" -> False
            _ -> scheduledOk
  where
    scheduledOk =
        case articlePublishAt article of
            Nothing -> True
            Just publishAt -> publishAt <= now
