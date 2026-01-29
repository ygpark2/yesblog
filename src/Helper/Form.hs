{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Helper.Form where

import Import
-- to use Html into forms
import Data.Time
import Yesod.Markdown
import Yesod.Auth
import Data.Maybe
import qualified Data.Text as T

entryForm :: Form (Article, [Text])
entryForm html = postForm Nothing Nothing html

modifyForm :: Article -> [Text] -> Form (Article, [Text])
modifyForm art oldTags = postForm (Just art) (Just oldTags)

postForm :: Maybe Article -> Maybe [Text] -> Form (Article, [Text])
postForm mart mtags html = do
  Entity userId _ <- lift requireAuth
  (r,widget) <- renderDivs
    (
    let art = Article <$> pure userId
                      <*> areq textField     fsTitle   (articleTitle   <$> mart)
                      <*> areq markdownField fsContent (articleContent <$> mart)
                      <*> areq textField     fsSlug    (articleSlug    <$> mart)
                      <*> areq checkBoxField fsDraft   (articleDraft   <$> mart)
                      <*> lift (liftIO getCurrentTime)
        defaultTags = Just (fmap T.unwords mtags)
        tags = (\mt -> T.words (fromMaybe "" mt)) <$> aopt textField fsTag defaultTags
    in (,) <$> art <*> tags
    )
    html
  return (r,widget)
      where
        fsTitle   = (fieldSettingsLabel ("Title" :: Text))   { fsAttrs = [("class", "col-md-12")] }
        fsContent = (fieldSettingsLabel ("Content" :: Text)) { fsAttrs = [("class", "col-md-12")] }
        fsSlug    = (fieldSettingsLabel ("Slug" :: Text))    { fsAttrs = [("class", "col-md-12")] }
        fsTag     = (fieldSettingsLabel ("Tags" :: Text))    { fsAttrs = [("class", "col-md-12")] }
        fsDraft   = (fieldSettingsLabel ("Draft" :: Text))   { fsAttrs = [("class", "col-md-12")] }

commentForm :: ArticleId -> Form Comment
commentForm articleId extra = do
  muser <- lift maybeAuth
  let mname = case muser of
        Just entity -> userIdent $ entityVal entity
        Nothing -> "Anonymous"
  renderDivs (commentAForm mname) extra
  where commentAForm mname = Comment
          <$> areq textField fsName (Just mname)
          <*> (unTextarea <$> areq textareaField fsContent Nothing)
          <*> pure articleId
          <*> lift (liftIO getCurrentTime)
            where
              fsName    = (fieldSettingsLabel ("Name" :: Text))    { fsAttrs = [("class", "col-md-12")] }
              fsContent = (fieldSettingsLabel ("Comment" :: Text)) { fsAttrs = [("class", "col-md-12")] }
