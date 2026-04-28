{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.ApiAuthSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "Auth API" $ do
        it "registers a user and starts a session" $ do
            requestWithCsrf $ do
                setMethod "POST"
                setUrl ApiAuthRegisterR
                addPostParam "ident" "new-writer"
                addPostParam "password" "password123"
                addPostParam "passwordConfirm" "password123"
                addPostParam "displayName" "New Writer"
                addPostParam "bio" "Writes API-first posts"

            statusIs 200
            bodyContains "\"authenticated\":true"
            bodyContains "\"ident\":\"new-writer\""

            get ApiSessionR
            statusIs 200
            bodyContains "\"authenticated\":true"
            bodyContains "\"ident\":\"new-writer\""

            mUser <- runDB $ getBy $ UniqueUser "new-writer"
            case mUser of
                Nothing -> liftIO $ expectationFailure "expected registered user to exist"
                Just (Entity _ user) -> do
                    assertEq "display name" (userDisplayName user) (Just "New Writer")
                    assertEq "bio" (userBio user) (Just (Textarea "Writes API-first posts"))
                    liftIO $ userPassword user `shouldSatisfy` isJust

        it "logs in and logs out an existing user" $ do
            _ <- createUserWithPassword "login-user" "password123"

            requestWithCsrf $ do
                setMethod "POST"
                setUrl ApiAuthLoginR
                addPostParam "ident" "login-user"
                addPostParam "password" "password123"

            statusIs 200
            bodyContains "\"authenticated\":true"

            get ApiSessionR
            statusIs 200
            bodyContains "\"authenticated\":true"
            bodyContains "\"ident\":\"login-user\""

            requestWithCsrf $ do
                setMethod "POST"
                setUrl ApiAuthLogoutR

            statusIs 200
            bodyContains "\"authenticated\":false"

            get ApiSessionR
            statusIs 200
            bodyContains "\"authenticated\":false"

        it "rejects duplicate usernames during registration" $ do
            _ <- createUser "taken-user"

            requestWithCsrf $ do
                setMethod "POST"
                setUrl ApiAuthRegisterR
                addPostParam "ident" "taken-user"
                addPostParam "password" "password123"
                addPostParam "passwordConfirm" "password123"

            statusIs 400
            bodyContains "already taken"

        it "rejects invalid passwords during login" $ do
            _ <- createUserWithPassword "member" "password123"

            requestWithCsrf $ do
                setMethod "POST"
                setUrl ApiAuthLoginR
                addPostParam "ident" "member"
                addPostParam "password" "wrong-password"

            statusIs 400
            bodyContains "Invalid username or password."

        it "blocks self-service plan upgrades" $ do
            userEntity <- createUser "plan-member"
            authenticateAs userEntity

            requestWithCsrf $ do
                setMethod "POST"
                setUrl ApiMePlanR
                addPostParam "plan" "writer-pro"

            statusIs 400
            bodyContains "not self-service"
