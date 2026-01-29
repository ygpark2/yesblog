{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Article where

import Import
import Helper.Form
import Helper.Sidebar
import Data.Time
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Data.List as L
import Yesod.Auth

getPermalinkR :: Text -> Handler Html
getPermalinkR slug = do
  maid <- maybeAuthId
  Entity articleId articleEntity <- runDB $ getBy404 $ UniqueSlug slug
  let articleTitleText = articleTitle articleEntity
      articleContentText = articleContent articleEntity
      articleAuthorId = articleAuthor articleEntity
      createdAt = articleCreatedAt articleEntity
  tagEntities <- runDB (selectList [TagArticle ==. articleId] [])
  let tags = L.sort (L.nub (Prelude.map (tagName Prelude.. entityVal) tagEntities))
  (comments, author) <- runDB $ do
    comments' <- Prelude.map entityVal <$>
                selectList [CommentArticle ==. articleId] [Asc CommentPosted]
    author' <- get404 articleAuthorId
    return (comments', author')
  let published = formatTime defaultTimeLocale "%Y-%m-%d" createdAt
  let screenAuthor = userIdent author
  let hasTags = not (Prelude.null tags)
  let hasComments = not (Prelude.null comments)
  ((_, commentWidget), enctype) <- runFormPost $ commentForm articleId
  defaultLayout $ do
    setTitle $ toHtml $ articleTitleText
    $(widgetFile "permalink")

postPermalinkR :: Text -> Handler Html
postPermalinkR slug = do
  Entity articleId _ <- runDB $ getBy404 $ UniqueSlug slug
  ((res, _), _) <- runFormPost (commentForm articleId)
  case res of
    FormSuccess comment -> do
      _ <- runDB $ insert comment
      setMessage "Comment posted."
      redirect $ PermalinkR slug
    _ -> do
      setMessage "Could not post comment."
      redirect $ PermalinkR slug

getArchiveR :: Handler Html
getArchiveR = do
  now <- liftIO $ getCurrentTime
  archives <- runDB $ selectList [ArticleDraft !=. True] [Desc ArticleCreatedAt]
  defaultLayout $ do
    let formatDate = formatTime defaultTimeLocale "%Y-%m-%d"
    setTitle "Archive"
    $(widgetFile "archive")
