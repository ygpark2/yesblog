{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.User where

import Import
import Yesod.Auth
import Helper.UserForm

getUserSettingR :: Handler Html
getUserSettingR = do
  Entity key user <- requireAuth
  (widget, enctype) <- generateFormPost userForm
  defaultLayout $ do
    setTitle "User Settings"
    $(widgetFile "user-setting")

postUserSettingR :: Handler Html
postUserSettingR = do
  Entity key _ <- requireAuth
  ((result, _), _) <- runFormPost userForm
  case result of
    FormSuccess newUser -> do
         runDB $ replace key newUser
         redirect UserSettingR
    _ -> permissionDenied "Not authorized"

postLangR :: Handler ()
postLangR = do
    lang <- runInputPost $ ireq textField "lang"
    setLanguage lang
    redirect UserSettingR
