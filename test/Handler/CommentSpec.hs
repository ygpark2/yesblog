{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.CommentSpec (spec) where

import TestImport
import Data.Aeson

spec :: Spec
spec = withApp $ do
    describe "valid request" $ do
        it "gives a 200" $ do
            get HomeR
            statusIs 200

            let message = "My message" :: Text
                body = object [ "message" .= message ]
                encoded = encode body

            request $ do
                setMethod "POST"
                setUrl CommentR
                setRequestBody encoded
                addRequestHeader ("Content-Type", "application/json")

            statusIs 200

            comments <- runDB $ selectList [HomeCommentContent ==. message] []
            let [Entity _id comment] = comments
            assertEq "comment content" (homeCommentContent comment) message
            assertEq "comment author" (homeCommentName comment) ("Anonymous" :: Text)
            bodyContains "\"message\":\"My message\""

    describe "invalid requests" $ do
        it "400s when the JSON body is invalid" $ do
            get HomeR

            let body = object [ "foo" .= ("My message" :: Value) ]

            request $ do
                setMethod "POST"
                setUrl CommentR
                setRequestBody $ encode body
                addRequestHeader ("Content-Type", "application/json")

            statusIs 400

        it "400s when the message is blank" $ do
            get HomeR

            let body = object [ "message" .= ("   " :: Text) ]

            request $ do
                setMethod "POST"
                setUrl CommentR
                setRequestBody $ encode body
                addRequestHeader ("Content-Type", "application/json")

            statusIs 400
