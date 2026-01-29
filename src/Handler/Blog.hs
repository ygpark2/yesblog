{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Blog where

import Import
import Data.Time
import Data.Maybe
import Data.List (head, sortBy)
import Data.Function
import Control.Monad
import qualified Data.Text as T (concat, append)
import Text.Shakespeare.Text (st)
import Database.Persist.Sql
import Yesod.RssFeed
import Helper.Sidebar
import Helper.ArticleInfo
import Helper.MakeBrief

getBlogFeedR :: Handler RepRss
getBlogFeedR = do
  articles <- runDB $ selectList [ArticleDraft !=. True][Desc ArticleCreatedAt, LimitTo 10]
  title <- getBlogTitle
  let entries = Prelude.map (\(Entity _ article) ->
        FeedEntry {
            feedEntryLink = PermalinkR $ articleSlug article
          , feedEntryUpdated = articleCreatedAt article
          , feedEntryTitle = articleTitle article
          , feedEntryContent = toHtml $ makeBrief 500 $ markdownToText $ articleContent article
          , feedEntryEnclosure = Nothing
          }
        ) articles
  case articles of
   [] -> notFound
   (Entity _ firstArticle:_) -> do
    let feed = Feed {
          feedTitle = "Blog - " `T.append` title
        , feedLinkSelf = BlogFeedR
        , feedLinkHome = BlogViewR
        , feedAuthor = "cosmo__"
        , feedDescription = toHtml ("http://cosmo0920.github.com/Ahblog" :: Text)
        , feedLanguage = "ja"
        , feedUpdated = articleCreatedAt firstArticle
        , feedEntries = entries
        , feedLogo = Nothing
        }
    rssFeed feed

getBlogViewR :: Handler Html
getBlogViewR = do
  -- Get the list of articles inside the database
  let page = 10
  (articles, articleArchives) <- runDB $ do
    articles <- selectList [ArticleDraft !=. True] [Desc ArticleCreatedAt, LimitTo page]
    articleArchives <- selectList [ArticleDraft !=. True] [Desc ArticleCreatedAt, LimitTo 10]
    return (articles, articleArchives)
  title <- getBlogTitle
  -- We'll need the two "objects": articleWidget and enctype
  -- to construct the form (see templates/articles.hamlet).
  defaultLayout $ do
    let hasArticles = not (Prelude.null articles)
    setTitle $ toHtml title
    $(widgetFile "view")

getSearchR :: Handler Html
getSearchR = do
    searchString <- runInputGet $ fromMaybe ("" :: Text) <$> iopt (searchField True) ("q" :: Text)
    articles <-
       if searchString /= ""
       then selectArticles searchString
       else return (mempty)

    now <- liftIO $ getCurrentTime
    defaultLayout $ do
      let hasArticles = not (Prelude.null articles)
      $(widgetFile "search")
  where
    selectArticles :: Text -> Handler [Entity Article]
    selectArticles t =
      runDB $ rawSql [st| SELECT ?? FROM article
                          WHERE draft = ?
                          AND (content LIKE ? OR title LIKE ?)
                          ORDER BY created_at DESC|]
                     [ toPersistValue False
                     , toPersistValue $ T.concat ["%", t, "%"]
                     , toPersistValue $ T.concat ["%", t, "%"]]

getTagR :: Text -> Handler Html
getTagR tag = do
  articles <- runDB $ do
    mapM (\e -> get404 (tagArticle (entityVal e))) =<< selectList [TagName ==. tag] []
  when (Prelude.null articles) notFound
  defaultLayout $ do
    let hasArticles = not (Prelude.null articles)
    setTitle "Tagged articles"
    $(widgetFile "inline/tag")

getBlogTitle :: Handler Text
getBlogTitle = pure "YesBlog"
