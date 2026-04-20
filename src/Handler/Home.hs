{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Home where

import Import
import Text.Julius (RawJS (..))

getHomeR :: Handler Html
getHomeR = do
    allComments <- runDB $ getAllComments
    muser <- maybeAuth

    defaultLayout $ do
        let title :: Text
            title = "YesBlog"
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        let currentCommenter = maybe "Anonymous" (userIdent . entityVal) muser
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    allComments <- runDB $ getAllComments
    muser <- maybeAuth

    defaultLayout $ do
        let title :: Text
            title = "YesBlog"
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        let currentCommenter = maybe "Anonymous" (userIdent . entityVal) muser
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")

getAllComments :: DB [Entity HomeComment]
getAllComments = selectList [] [Desc HomeCommentPosted]
