{-# LANGUAGE OverloadedStrings #-}
module Helper.UserForm where

import Import
import Yesod.Auth

userForm :: Form User
userForm html = do
  Entity _ user <- lift requireAuth
  let ident      = userIdent user
      password   = userPassword user
      displayName = userDisplayName user
      bio         = userBio user
      isAdmin    = userIsAdmin user
      theme       = userTheme user
      themeOverrides = userThemeOverrides user
  renderDivs
    (User
      <$> areq textField (fieldSettingsLabel ("Username" :: Text)) (Just ident)
      <*> pure password
      <*> aopt textField (fieldSettingsLabel ("Display name" :: Text)) (Just displayName)
      <*> aopt textareaField (fieldSettingsLabel ("Bio" :: Text)) (Just bio)
      <*> pure isAdmin
      <*> pure (userPlan user)
      <*> pure (userPlanExpiresAt user)
      <*> pure theme
      <*> pure themeOverrides
    )
    html
