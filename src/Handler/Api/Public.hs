{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Api.Public where

import Import
import Handler.Api.Shared
import Helper.MakeBrief
import Database.Persist.Sql (fromSqlKey)
import qualified Data.Text as T

getApiPostsR :: Handler Value
getApiPostsR = do
    page <- max 1 <$> runInputGet (fromMaybe 1 <$> iopt intField ("page" :: Text))
    limit <- min 30 . max 1 <$> runInputGet (fromMaybe 12 <$> iopt intField ("limit" :: Text))
    mTag <- cleanOptionalText <$> lookupGetParam "tag"
    mAuthorIdent <- cleanOptionalText <$> lookupGetParam "author"
    mQuery <- cleanOptionalText <$> lookupGetParam "q"
    filteredArticles <- selectPublishedArticles mTag mAuthorIdent mQuery
    let total = length filteredArticles
        offset = (page - 1) * limit
        pagedArticles = take limit $ drop offset filteredArticles
    articleTags <- loadTagMap (map entityKey pagedArticles)
    authors <- loadAuthorMap pagedArticles
    returnJson $ object
        [ "items" .= map (articleSummaryValue articleTags authors) pagedArticles
        , "meta" .= object
            [ "page" .= page
            , "limit" .= limit
            , "total" .= total
            , "hasNext" .= (offset + length pagedArticles < total)
            , "tag" .= mTag
            , "author" .= mAuthorIdent
            , "q" .= mQuery
            ]
        ]

getApiPostR :: Text -> Handler Value
getApiPostR slug = do
    Entity articleId article <- runDB $ getBy404 $ UniqueSlug slug
    mviewer <- maybeAuth
    now <- liftIO getCurrentTime
    visibleToViewer <- canViewerAccessArticle now mviewer article
    when (articleDraft article || not visibleToViewer) notFound
    author <- runDB $ get404 $ articleAuthor article
    authorTheme <- loadUserTheme author
    tags <- runDB $ map (tagName . entityVal) <$> selectList [TagArticle ==. articleId] [Asc TagName]
    comments <- runDB $ selectList [CommentArticle ==. articleId] [Asc CommentPosted]
    relatedArticles <- runDB $ selectList
        [ ArticleDraft !=. True
        , ArticleId !=. articleId
        , ArticleAuthor ==. articleAuthor article
        ]
        [Desc ArticleUpdatedAt, LimitTo 3]
    visibleRelatedArticles <- filterM (canViewerAccessArticle now mviewer . entityVal) relatedArticles
    relatedTags <- loadTagMap (map entityKey visibleRelatedArticles)
    relatedAuthors <- loadAuthorMap visibleRelatedArticles
    returnJson $ object
        [ "item" .= articleDetailValueWithTheme mviewer (Entity articleId article) author authorTheme tags comments
        , "related" .= map (articleSummaryValue relatedTags relatedAuthors) visibleRelatedArticles
        ]

postApiPostCommentR :: Text -> Handler Value
postApiPostCommentR slug = do
    Entity articleId article <- runDB $ getBy404 $ UniqueSlug slug
    rawName <- runInputPost $ fromMaybe "" <$> iopt textField "name"
    rawContent <- runInputPost $ fromMaybe "" <$> iopt textField "content"
    muser <- maybeAuth
    now <- liftIO getCurrentTime
    visibleToViewer <- canViewerAccessArticle now muser article
    when (articleDraft article || not visibleToViewer) notFound
    enforceCommentCooldown now
    let normalizedContent = T.strip rawContent
        fallbackName = maybe "Anonymous" (\entity -> userIdent (entityVal entity)) muser
        normalizedName = if T.strip rawName == "" then fallbackName else T.strip rawName
    when (normalizedContent == "") $ apiError status400 "Write a comment before posting."
    when (length normalizedName > 80) $ apiError status400 "Comment name must be 80 characters or fewer."
    when (length normalizedContent > 3000) $ apiError status400 "Comments must be 3000 characters or fewer."
    commentId <- runDB $ insert $ Comment normalizedName normalizedContent articleId now
    comment <- runDB $ get404 commentId
    rememberCommentTimestamp now
    returnJson $ object ["comment" .= commentValue (Just normalizedName) (Entity commentId comment)]

postApiCommentUpdateR :: CommentId -> Handler Value
postApiCommentUpdateR commentId = do
    managerIdent <- requireCommentManager commentId
    rawContent <- runInputPost $ fromMaybe "" <$> iopt textField "content"
    let normalizedContent = T.strip rawContent
    when (normalizedContent == "") $ apiError status400 "Comment cannot be empty."
    when (length normalizedContent > 3000) $ apiError status400 "Comments must be 3000 characters or fewer."
    runDB $ update commentId [CommentContent =. normalizedContent]
    comment <- runDB $ get404 commentId
    returnJson $ object ["comment" .= commentValue (Just managerIdent) (Entity commentId comment)]

postApiCommentDeleteR :: CommentId -> Handler Value
postApiCommentDeleteR commentId = do
    _ <- requireCommentManager commentId
    runDB $ delete commentId
    returnJson $ object ["deletedCommentId" .= fromSqlKey commentId]

getApiUserR :: Text -> Handler Value
getApiUserR ident = do
    Entity userId user <- runDB $ getBy404 $ UniqueUser ident
    mviewer <- maybeAuth
    now <- liftIO getCurrentTime
    userTheme' <- loadUserTheme user
    articles <- runDB $ selectList [ArticleAuthor ==. userId, ArticleDraft !=. True] [Desc ArticleUpdatedAt]
    let visibleArticles = filter (isArticlePubliclyVisible now . entityVal) articles
    articleTags <- loadTagMap (map entityKey visibleArticles)
    authors <- loadAuthorMap visibleArticles
    mMembership <- case mviewer of
        Just (Entity viewerId _) | viewerId /= userId -> lookupMembershipFor now userId viewerId
        _ -> pure Nothing
    returnJson $ object
        [ "user" .= userValueWithTheme user userTheme'
        , "items" .= map (articleSummaryValue articleTags authors) visibleArticles
        , "meta" .= object ["publishedCount" .= length visibleArticles]
        , "membership" .= membershipAccessValue user mMembership now
        ]

canViewerAccessArticle :: UTCTime -> Maybe (Entity User) -> Article -> Handler Bool
canViewerAccessArticle now mviewer article
    | maybe False (userIsAdmin . entityVal) mviewer = pure True
    | maybe False ((== articleAuthor article) . entityKey) mviewer = pure True
    | otherwise =
        case articleVisibility article of
            "private" -> pure False
            "members" ->
                case mviewer of
                    Just (Entity viewerId _) -> do
                        membershipActive <- hasActiveMembershipFor now (articleAuthor article) viewerId
                        pure $ membershipActive && scheduledOk
                    Nothing -> pure False
            _ -> pure scheduledOk
  where
    scheduledOk =
        case articlePublishAt article of
            Nothing -> True
            Just publishAt -> publishAt <= now
