{-# LANGUAGE OverloadedStrings #-}
module Helper.UserForm where

import Import
import Yesod.Auth

userForm :: Form User
userForm html = do
  Entity _ user <- lift requireAuth
  let ident      = userIdent user
      password   = userPassword user
  renderDivs
    (User
      <$> areq textField (fieldSettingsLabel ("Username" :: Text)) (Just ident)
      <*> pure password
    )
    html
