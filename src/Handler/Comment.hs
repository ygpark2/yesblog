{-# LANGUAGE OverloadedStrings #-}
module Handler.Comment where

import Import
import Data.Aeson ((.:), FromJSON (..), object, withObject, (.=))
import qualified Data.Text as T
import Data.Time (getCurrentTime)

data IncomingComment = IncomingComment
    { incomingMessage :: Text
    }

instance FromJSON IncomingComment where
    parseJSON = withObject "IncomingComment" $ \obj ->
        IncomingComment <$> obj .: "message"

postCommentR :: Handler Value
postCommentR = do
    IncomingComment rawMessage <- requireCheckJsonBody
    let message = T.strip rawMessage
    when (T.null message) $ invalidArgs ["message"]

    muser <- maybeAuth
    now <- liftIO getCurrentTime
    let authorName = maybe "Anonymous" (\entity -> userIdent (entityVal entity)) muser

    _ <- runDB $ insert $ HomeComment authorName message now
    returnJson $ object
        [ "name" .= authorName
        , "message" .= message
        ]
