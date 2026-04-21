{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Api where

import Import
import Handler.Api.Shared
import qualified Data.Text as T
import Database.Persist.Sql (fromSqlKey)
import Yesod.Auth (Creds (..), clearCreds, setCreds)
import Yesod.Auth.HashDB (setPassword, validatePass)

postApiAuthLoginR :: Handler Value
postApiAuthLoginR = do
    identInput <- runInputPost $ fromMaybe "" <$> iopt textField "ident"
    usernameInput <- runInputPost $ fromMaybe "" <$> iopt textField "username"
    password <- runInputPost $ fromMaybe "" <$> iopt textField "password"
    let ident = T.strip $ if T.strip identInput /= "" then identInput else usernameInput
    when (ident == "") $
        apiError status400 "Username is required."
    when (T.strip password == "") $
        apiError status400 "Password is required."
    Entity userId user <- runDB $ getBy404 $ UniqueUser ident
    case validatePass user password of
        Just True -> do
            setCreds False $ Creds "api" ident []
            returnJson $ object
                [ "authenticated" .= True
                , "user" .= userValue user
                , "userId" .= fromSqlKey userId
                ]
        _ ->
            apiError status400 "Invalid username or password."

postApiAuthRegisterR :: Handler Value
postApiAuthRegisterR = do
    ident <- runInputPost $ ireq textField "ident"
    password <- runInputPost $ ireq textField "password"
    passwordConfirm <- runInputPost $ ireq textField "passwordConfirm"
    rawDisplayName <- runInputPost $ fromMaybe "" <$> iopt textField "displayName"
    rawBio <- runInputPost $ fromMaybe "" <$> iopt textField "bio"
    existingUser <- runDB $ getBy $ UniqueUser ident
    let trimmedIdent = T.strip ident
        trimmedDisplayName = T.strip rawDisplayName
        trimmedBio = T.strip rawBio
    when (trimmedIdent == "") $
        apiError status400 "Username is required."
    when (isJust existingUser) $
        apiError status400 "That username is already taken."
    when (length password < 8) $
        apiError status400 "Password must be at least 8 characters."
    when (password /= passwordConfirm) $
        apiError status400 "Passwords do not match."
    let baseUser =
            User
                { userIdent = trimmedIdent
                , userPassword = Nothing
                , userDisplayName =
                    if trimmedDisplayName == ""
                        then Nothing
                        else Just trimmedDisplayName
                , userBio =
                    if trimmedBio == ""
                        then Nothing
                        else Just (Textarea trimmedBio)
                , userIsAdmin = False
                }
    userWithPassword <- liftIO $ setPassword password baseUser
    userId <- runDB $ insert userWithPassword
    createdUser <- runDB $ get404 userId
    setCreds False $ Creds "api" trimmedIdent []
    returnJson $ object
        [ "authenticated" .= True
        , "user" .= userValue createdUser
        , "userId" .= fromSqlKey userId
        ]

postApiAuthLogoutR :: Handler Value
postApiAuthLogoutR = do
    clearCreds False
    returnJson $ object
        [ "authenticated" .= False
        ]

getApiSessionR :: Handler Value
getApiSessionR = do
    muser <- maybeAuth
    returnJson $ object
        [ "user" .= fmap (userValue . entityVal) muser
        , "authenticated" .= isJust muser
        ]

getApiMeR :: Handler Value
getApiMeR = do
    (userId, user) <- requireAuthPair
    totalArticles <- runDB $ count [ArticleAuthor ==. userId]
    draftCount <- runDB $ count [ArticleAuthor ==. userId, ArticleDraft ==. True]
    publishedCount <- runDB $ count [ArticleAuthor ==. userId, ArticleDraft !=. True]
    returnJson $ object
        [ "user" .= userValue user
        , "meta" .= object
            [ "totalCount" .= totalArticles
            , "draftCount" .= draftCount
            , "publishedCount" .= publishedCount
            ]
        ]

postApiMeUpdateR :: Handler Value
postApiMeUpdateR = do
    (userId, _user) <- requireAuthPair
    ident <- runInputPost $ ireq textField "ident"
    rawDisplayName <- runInputPost $ fromMaybe "" <$> iopt textField "displayName"
    rawBio <- runInputPost $ fromMaybe "" <$> iopt textField "bio"
    let trimmedIdent = T.strip ident
        trimmedDisplayName = T.strip rawDisplayName
        trimmedBio = T.strip rawBio
    when (trimmedIdent == "") $
        apiError status400 "Username is required."
    when (length trimmedIdent < 3) $
        apiError status400 "Username must be at least 3 characters."
    existingUser <- runDB $ getBy $ UniqueUser trimmedIdent
    case existingUser of
        Just (Entity existingUserId _)
            | existingUserId /= userId ->
                apiError status400 "That username is already taken."
        _ -> pure ()
    runDB $ update userId
        [ UserIdent =. trimmedIdent
        , UserDisplayName =.
            (if trimmedDisplayName == "" then Nothing else Just trimmedDisplayName)
        , UserBio =.
            (if trimmedBio == "" then Nothing else Just (Textarea trimmedBio))
        ]
    updatedUser <- runDB $ get404 userId
    returnJson $ object
        [ "user" .= userValue updatedUser
        ]
