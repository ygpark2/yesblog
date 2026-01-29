{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Helper.ArticleInfo where

import Import
import Data.Time (defaultTimeLocale, formatTime)
import qualified Data.Text as T

articleInfo :: Article -> Widget
articleInfo article = do
  (author, tags) <- handlerToWidget $ runDB $ do
    Entity key articleEntity <- getBy404 $ UniqueSlug (articleSlug article)
    let authorId = articleAuthor articleEntity
    author <- get404 authorId
    tags <- Prelude.map (tagName Prelude.. entityVal) <$> selectList [TagArticle ==. key] [Asc TagName]
    return (author, tags)
  let authorName = userIdent author
      createdText = T.pack (formatTime defaultTimeLocale "%Y-%m-%d" (articleCreatedAt article))
      hasTags = not (Prelude.null tags)
  [whamlet|
    <p .article-info>
      Posted #{createdText} by #{authorName}
      $if hasTags
        \- Tags:
        $forall tag <- tags
          <a href=@{TagR tag}>#{tag}
  |]
