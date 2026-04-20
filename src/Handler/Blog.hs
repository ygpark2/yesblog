{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Blog where

import Import
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
          , feedEntryCategories = []
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
  currentPage <- getPageParam
  let pageSize = 10
      offset = pageOffset currentPage pageSize
  articles <- runDB $ selectList [ArticleDraft !=. True] [Desc ArticleCreatedAt, OffsetBy offset, LimitTo pageSize]
  totalArticles <- runDB $ count [ArticleDraft !=. True]
  title <- getBlogTitle
  renderer <- getUrlRenderParams
  defaultLayout $ do
    let hasArticles = not (Prelude.null articles)
    let prevPageUrl =
          if currentPage > 1
          then Just $ renderer BlogViewR [("page", tshow (currentPage - 1))]
          else Nothing
    let nextPageUrl =
          if totalArticles > offset + Prelude.length articles
          then Just $ renderer BlogViewR [("page", tshow (currentPage + 1))]
          else Nothing
    setTitle $ toHtml title
    $(widgetFile "view")

getSearchR :: Handler Html
getSearchR = do
    searchString <- runInputGet $ fromMaybe ("" :: Text) <$> iopt (searchField True) ("q" :: Text)
    currentPage <- getPageParam
    let pageSize = 10
        offset = pageOffset currentPage pageSize
    (articles, totalArticles) <-
       if searchString /= ""
       then selectArticles searchString offset pageSize
       else return (mempty, 0)

    renderer <- getUrlRenderParams
    defaultLayout $ do
      let hasArticles = not (Prelude.null articles)
      let showingFrom =
            if hasArticles
            then offset + 1
            else 0
      let showingTo = offset + Prelude.length articles
      let searchSummary =
            if searchString == ""
            then "Enter a title or body keyword to search published articles."
            else if totalArticles == 0
              then T.concat ["No published articles matched \"", searchString, "\"."]
              else T.concat
                [ "Showing "
                , tshow showingFrom
                , "-"
                , tshow showingTo
                , " of "
                , tshow totalArticles
                , " results for \""
                , searchString
                , "\"."
                ]
      let prevPageUrl =
            if currentPage > 1 && searchString /= ""
            then Just $ renderer SearchR [("q", searchString), ("page", tshow (currentPage - 1))]
            else Nothing
      let nextPageUrl =
            if totalArticles > offset + Prelude.length articles
            then Just $ renderer SearchR [("q", searchString), ("page", tshow (currentPage + 1))]
            else Nothing
      $(widgetFile "search")
  where
    selectArticles :: Text -> Int -> Int -> Handler ([Entity Article], Int)
    selectArticles t offset limit =
      runDB $ do
        let keyword = T.concat ["%", t, "%"]
        articles <- rawSql [st| SELECT ?? FROM article
                                WHERE draft = ?
                                AND (content LIKE ? OR title LIKE ?)
                                ORDER BY created_at DESC
                                LIMIT ? OFFSET ?|]
                           [ toPersistValue False
                           , toPersistValue keyword
                           , toPersistValue keyword
                           , toPersistValue limit
                           , toPersistValue offset
                           ]
        total <- rawSql [st| SELECT COUNT(*) FROM article
                             WHERE draft = ?
                             AND (content LIKE ? OR title LIKE ?)|]
                        [ toPersistValue False
                        , toPersistValue keyword
                        , toPersistValue keyword
                        ]
        pure (articles, maybe 0 unSingle (listToMaybe total))

getTagR :: Text -> Handler Html
getTagR tag = do
  articles <- runDB $ do
    mapM (\e -> get404 (tagArticle (entityVal e))) =<< selectList [TagName ==. tag] []
  when (Prelude.null articles) notFound
  defaultLayout $ do
    let hasArticles = not (Prelude.null articles)
    let articleCount = Prelude.length articles
    setTitle "Tagged articles"
    $(widgetFile "inline/tag")

getBlogTitle :: Handler Text
getBlogTitle = pure "YesBlog"

getPageParam :: Handler Int
getPageParam = do
  page <- runInputGet $ fromMaybe 1 <$> iopt intField ("page" :: Text)
  pure $ max 1 page

pageOffset :: Int -> Int -> Int
pageOffset page pageSize = (page - 1) * pageSize
