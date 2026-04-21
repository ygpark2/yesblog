{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Api.Shared where

import Import
import Helper.MakeBrief
import Helper.ImageForm (uploadDirectory, uploadSubDirectory)
import Database.Persist.Sql (fromSqlKey)
import qualified Control.Monad as CM
import qualified Data.Char as Char
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Read as TR
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

selectPublishedArticles :: Maybe Text -> Maybe Text -> Maybe Text -> Handler [Entity Article]
selectPublishedArticles mTag mAuthorIdent mQuery = do
    mAuthorId <- case mAuthorIdent of
        Nothing -> pure Nothing
        Just ident -> entityKey <$> runDB (getBy404 $ UniqueUser ident) >>= pure . Just
    let authorFilters =
            [ArticleDraft !=. True] <>
            maybe [] (\authorId -> [ArticleAuthor ==. authorId]) mAuthorId
    baseArticles <- runDB $ selectList authorFilters [Desc ArticleUpdatedAt]
    let queryFilteredArticles = applyQueryFilter mQuery baseArticles
    case mTag of
        Nothing -> pure queryFilteredArticles
        Just tagName' -> do
            taggedArticleIds <- runDB $
                map (tagArticle . entityVal) <$> selectList [TagName ==. tagName'] []
            pure $ filter (\entity -> entityKey entity `elem` taggedArticleIds) queryFilteredArticles

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
        , "content" .= markdownToText (articleContent article)
        , "tags" .= fromMaybe [] (M.lookup articleId tagMap)
        , "createdAt" .= isoTime (articleCreatedAt article)
        , "updatedAt" .= isoTime (articleUpdatedAt article)
        , "readingMinutes" .= readingMinutes article
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
        , "author" .= userValue author
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
        ]

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
