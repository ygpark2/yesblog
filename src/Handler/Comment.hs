{-# LANGUAGE OverloadedStrings #-}
module Handler.Comment where

import Import

postCommentR :: Handler Value
postCommentR = do
    returnJson ("ok" :: Text)
