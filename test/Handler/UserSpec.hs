{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.UserSpec (spec) where

import TestImport
import Yesod.Form.Fields (Textarea (..))

spec :: Spec
spec = withApp $ do

    describe "User settings" $ do
        it "updates display name and bio" $ do
            userEntity@(Entity userId user) <- createUser "settings-user"
            authenticateAs userEntity

            get UserSettingR
            statusIs 200

            request $ do
                setMethod "POST"
                setUrl UserSettingR
                addToken
                byLabelExact "Username" (userIdent user)
                byLabelExact "Display name" ("Settings User" :: Text)
                byLabelExact "Bio" ("Updated profile bio" :: Text)

            statusIs 303

            users <- runDB $ selectList [UserId ==. userId] []
            let [Entity _ updatedUser] = users
            assertEq "display name updated" (userDisplayName updatedUser) (Just "Settings User")
            assertEq "bio updated" (fmap unTextarea $ userBio updatedUser) (Just "Updated profile bio")
