{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Api.Editor where

import Import
import Handler.Api.Shared
import Helper.ImageForm (uploadDirectory, uploadSubDirectory)
import Helper.MakeBrief (markdownToText)
import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Database.Persist.Sql (fromSqlKey)
import Data.Time (defaultTimeLocale)
import Data.Time.Format (parseTimeM)
import System.Directory (doesFileExist, removeFile)
import System.FilePath ((</>))
import Yesod.Markdown (Markdown (..))

getApiEditorBootstrapR :: Handler Value
getApiEditorBootstrapR = do
    (userId, user) <- requireAuthPair
    images <- loadRecentImages
    tagEntities <- runDB $ selectList [] [Asc TagName]
    draftCount <- runDB $ count [ArticleAuthor ==. userId, ArticleDraft ==. True]
    publishedCount <- runDB $ count [ArticleAuthor ==. userId, ArticleDraft !=. True]
    let existingTags = take 12 $ L.nub $ sort $ map (tagName . entityVal) tagEntities
    returnJson $ object
        [ "user" .= userValue user
        , "meta" .= object
            [ "draftCount" .= draftCount
            , "publishedCount" .= publishedCount
            ]
        , "tagSuggestions" .= existingTags
        , "images" .= map imageValue images
        ]

getApiEditorMineR :: Handler Value
getApiEditorMineR = do
    (userId, user) <- requireAuthPair
    draftArticles <- runDB $ selectList [ArticleAuthor ==. userId, ArticleDraft ==. True] [Desc ArticleUpdatedAt, LimitTo 12]
    publishedArticles <- runDB $ selectList [ArticleAuthor ==. userId, ArticleDraft !=. True] [Desc ArticleUpdatedAt, LimitTo 12]
    draftTags <- loadTagMap (map entityKey draftArticles)
    publishedTags <- loadTagMap (map entityKey publishedArticles)
    let authorMap = M.singleton userId user
    returnJson $ object
        [ "drafts" .= map (articleSummaryValue draftTags authorMap) draftArticles
        , "published" .= map (articleSummaryValue publishedTags authorMap) publishedArticles
        ]

getApiEditorArticleR :: ArticleId -> Handler Value
getApiEditorArticleR articleId = do
    (_, article, oldTags) <- loadOwnedArticle articleId
    returnJson $ object
        [ "item" .= object
            [ "id" .= fromSqlKey articleId
            , "title" .= articleTitle article
            , "content" .= markdownToText (articleContent article)
            , "slug" .= articleSlug article
            , "draft" .= articleDraft article
            , "visibility" .= articleVisibility article
            , "publishAt" .= fmap isoTime (articlePublishAt article)
            , "tags" .= oldTags
            , "createdAt" .= isoTime (articleCreatedAt article)
            , "updatedAt" .= isoTime (articleUpdatedAt article)
            ]
        ]

postApiEditorDeleteR :: ArticleId -> Handler Value
postApiEditorDeleteR articleId = do
    _ <- requireAuth
    _ <- requireOwnedArticle articleId
    runDB $ do
        deleteWhere [CommentArticle ==. articleId]
        deleteWhere [TagArticle ==. articleId]
        delete articleId
    returnJson $ object ["deletedArticleId" .= toPathPiece articleId]

postApiEditorUploadR :: Handler Value
postApiEditorUploadR = do
    _ <- requireAuth
    validateUploadRequestSize
    (params, files) <- runRequestBody
    fileInfo <- case files of
        [] -> apiError status400 "Choose an image file to upload."
        ((_, uploadedFile):_) -> pure uploadedFile
    validateUploadedImage fileInfo
    now <- liftIO getCurrentTime
    let description = normalizeOptionalTextarea $ lookup "description" params >>= nonEmptyText
    filename <- writeUploadedImage fileInfo
    imageId <- runDB $ insert $ Image filename description now
    image <- runDB $ get404 imageId
    returnJson $ object ["image" .= imageValue (Entity imageId image)]

postApiEditorImageUpdateR :: ImageId -> Handler Value
postApiEditorImageUpdateR imageId = do
    _ <- requireAuth
    rawDescription <- runInputPost $ fromMaybe "" <$> iopt textField "description"
    let description = normalizeOptionalTextarea (nonEmptyText rawDescription)
    runDB $ update imageId [ImageDescription =. description]
    image <- runDB $ get404 imageId
    returnJson $ object ["image" .= imageValue (Entity imageId image)]

postApiEditorImageDeleteR :: ImageId -> Handler Value
postApiEditorImageDeleteR imageId = do
    _ <- requireAuth
    image <- runDB $ get404 imageId
    let filePath = uploadDirectory </> uploadSubDirectory </> imageFilename image
    fileExists <- liftIO $ doesFileExist filePath
    when fileExists $ liftIO $ removeFile filePath
    runDB $ delete imageId
    returnJson $ object ["deletedImageId" .= fromSqlKey imageId]

postApiEditorSaveR :: Handler Value
postApiEditorSaveR = do
    Entity currentUserId currentUser <- requireAuth
    mArticleId <- lookupOptionalArticleIdParam "articleId"
    title <- runInputPost $ fromMaybe "" <$> iopt textField "title"
    content <- runInputPost $ fromMaybe "" <$> iopt textField "content"
    slugInput <- runInputPost $ fromMaybe "" <$> iopt textField "slug"
    tagsText <- runInputPost $ fromMaybe "" <$> iopt textField "tags"
    draftInput <- runInputPost $ fromMaybe "false" <$> iopt textField "draft"
    visibilityInput <- runInputPost $ fromMaybe "public" <$> iopt textField "visibility"
    publishAtInput <- runInputPost $ fromMaybe "" <$> iopt textField "publishAt"
    now <- liftIO getCurrentTime
    let tags = normalizeTags tagsText
        isDraft = parseBoolText draftInput
        slugSource = if T.strip slugInput == "" then title else slugInput
        visibility = normalizeArticleVisibility visibilityInput
    publishAt <- parseOptionalPublishAt publishAtInput
    when ((visibility /= "public" || isJust publishAt) && not (userHasWriterPro currentUser)) $
        apiError status400 "Writer Pro is required for scheduling and member/private visibility."
    resolvedSlug <- ensureUniqueSlug mArticleId slugSource
    articleId <- case mArticleId of
        Just articleId -> do
            _ <- requireOwnedArticle articleId
            runDB $ do
                update articleId
                    [ ArticleTitle =. defaultDraftTitle title
                    , ArticleContent =. Markdown content
                    , ArticleSlug =. resolvedSlug
                    , ArticleDraft =. isDraft
                    , ArticleVisibility =. visibility
                    , ArticlePublishAt =. publishAt
                    , ArticleUpdatedAt =. now
                    ]
                deleteWhere [TagArticle ==. articleId]
                CM.forM_ tags $ \tag -> insert $ Tag tag articleId
                pure articleId
        Nothing -> runDB $ do
            createdArticleId <- insert $ Article currentUserId (defaultDraftTitle title) (Markdown content) resolvedSlug isDraft visibility publishAt now now
            CM.forM_ tags $ \tag -> insert $ Tag tag createdArticleId
            pure createdArticleId
    savedArticle <- runDB $ get404 articleId
    returnJson $ object
        [ "articleId" .= toPathPiece articleId
        , "slug" .= articleSlug savedArticle
        , "draft" .= articleDraft savedArticle
        , "permalink" .= ("/app/posts/" <> articleSlug savedArticle)
        ]

postApiEditorAutosaveR :: Handler Value
postApiEditorAutosaveR = do
    Entity currentUserId currentUser <- requireAuth
    mAutosaveArticleId <- lookupOptionalArticleIdParam "articleId"
    title <- runInputPost $ fromMaybe "" <$> iopt textField "title"
    content <- runInputPost $ fromMaybe "" <$> iopt textField "content"
    slugInput <- runInputPost $ fromMaybe "" <$> iopt textField "slug"
    tagsText <- runInputPost $ fromMaybe "" <$> iopt textField "tags"
    visibilityInput <- runInputPost $ fromMaybe "public" <$> iopt textField "visibility"
    publishAtInput <- runInputPost $ fromMaybe "" <$> iopt textField "publishAt"
    now <- liftIO getCurrentTime
    let tags = normalizeTags tagsText
        slugSource = if T.strip slugInput == "" then title else slugInput
        visibility = normalizeArticleVisibility visibilityInput
    publishAt <- parseOptionalPublishAt publishAtInput
    when ((visibility /= "public" || isJust publishAt) && not (userHasWriterPro currentUser)) $
        apiError status400 "Writer Pro is required for scheduling and member/private visibility."
    resolvedSlug <- ensureUniqueSlug mAutosaveArticleId slugSource
    articleId <- case mAutosaveArticleId of
        Just autosaveArticleId -> do
            article <- requireOwnedArticle autosaveArticleId
            when (not $ articleDraft article) $
                permissionDenied "Server autosave is available only for drafts."
            runDB $ do
                update autosaveArticleId
                    [ ArticleTitle =. defaultDraftTitle title
                    , ArticleContent =. Markdown content
                    , ArticleSlug =. resolvedSlug
                    , ArticleDraft =. True
                    , ArticleVisibility =. visibility
                    , ArticlePublishAt =. publishAt
                    , ArticleUpdatedAt =. now
                    ]
                deleteWhere [TagArticle ==. autosaveArticleId]
                CM.forM_ tags $ \tag -> insert $ Tag tag autosaveArticleId
                pure autosaveArticleId
        Nothing -> runDB $ do
            createdArticleId <- insert $ Article currentUserId (defaultDraftTitle title) (Markdown content) resolvedSlug True visibility publishAt now now
            CM.forM_ tags $ \tag -> insert $ Tag tag createdArticleId
            pure createdArticleId
    returnJson $ object
        [ "articleId" .= toPathPiece articleId
        , "slug" .= resolvedSlug
        ]

normalizeArticleVisibility :: Text -> Text
normalizeArticleVisibility rawValue =
    let value = T.toLower $ T.strip rawValue
    in if value `elem` ["private", "members"]
        then value
        else "public"

parseOptionalPublishAt :: Text -> Handler (Maybe UTCTime)
parseOptionalPublishAt rawValue =
    let trimmed = T.strip rawValue
    in if trimmed == ""
        then pure Nothing
        else case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M" (T.unpack trimmed) <|> parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S" (T.unpack trimmed) of
            Just parsed -> pure $ Just parsed
            Nothing -> apiError status400 "Invalid publish time."
