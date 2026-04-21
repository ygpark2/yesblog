{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Helper.Form where

import Import
import Yesod.Markdown
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
    let art now = Article <$> pure userId
                          <*> areq textField     fsTitle   (articleTitle   <$> mart)
                          <*> areq markdownField fsContent (articleContent <$> mart)
                          <*> areq textField     fsSlug    (articleSlug    <$> mart)
                          <*> areq checkBoxField fsDraft   (articleDraft   <$> mart)
                          <*> pure (fromMaybe now $ articleCreatedAt <$> mart)
                          <*> pure now
        defaultTags = Just (fmap T.unwords mtags)
        tags = (\mt -> T.words (fromMaybe "" mt)) <$> aopt textField fsTag defaultTags
    in (,) <$> (lift (liftIO getCurrentTime) >>= art) <*> tags
    )
    html
  return (r,widget)
      where
        fsTitle   = (fieldSettingsLabel ("Title" :: Text))
          { fsAttrs =
              [ ("class", "col-md-12")
              , ("id", "write-title")
              , ("placeholder", "Write a strong title")
              ]
          }
        fsContent = (fieldSettingsLabel ("Content" :: Text))
          { fsAttrs =
              [ ("class", "col-md-12")
              , ("id", "write-content")
              , ("placeholder", "Start writing...")
              ]
          }
        fsSlug    = (fieldSettingsLabel ("Slug" :: Text))
          { fsAttrs =
              [ ("class", "col-md-12")
              , ("id", "write-slug")
              , ("placeholder", "auto-generated-from-title")
              ]
          }
        fsTag     = (fieldSettingsLabel ("Tags" :: Text))
          { fsAttrs =
              [ ("class", "col-md-12")
              , ("id", "write-tags")
              , ("placeholder", "essay devlog notes")
              ]
          }
        fsDraft   = (fieldSettingsLabel ("Draft" :: Text))
          { fsAttrs =
              [ ("class", "col-md-12")
              , ("id", "write-draft")
              ]
          }

commentForm :: ArticleId -> Form Comment
commentForm articleId extra = do
  muser <- lift maybeAuth
  let mname = case muser of
        Just entity -> userIdent $ entityVal entity
        Nothing -> "Anonymous"
  renderDivs (commentAForm mname) extra
  where commentAForm authorName = Comment
          <$> areq textField commentNameField (Just authorName)
          <*> (unTextarea <$> areq textareaField fsContent Nothing)
          <*> pure articleId
          <*> lift (liftIO getCurrentTime)
            where
              commentNameField = (fieldSettingsLabel ("Name" :: Text))    { fsAttrs = [("class", "col-md-12")] }
              fsContent        = (fieldSettingsLabel ("Comment" :: Text)) { fsAttrs = [("class", "col-md-12")] }
