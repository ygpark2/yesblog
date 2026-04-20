{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Profile where

import Import

getProfileR :: Handler Html
getProfileR = do
    (userId, user) <- requireAuthPair
    articleCount <- runDB $ count [ArticleAuthor ==. userId]
    let screenName = fromMaybe (userIdent user) (userDisplayName user)
        hasBio = isJust (userBio user)
    defaultLayout $ do
        setTitle . toHtml $ screenName <> "'s Profile"
        $(widgetFile "profile")
