{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Api.Legacy where

import Import
import Data.FileEmbed (embedFile)

getFaviconR :: Handler TypedContent
getFaviconR = do
    cacheSeconds $ 60 * 60 * 24 * 30
    return $ TypedContent "image/x-icon" $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR =
    return $ TypedContent typePlain $ toContent $(embedFile "config/robots.txt")

getHomeR :: Handler Html
getHomeR = redirect (FrontendAppPathR [])

postHomeR :: Handler Html
postHomeR = redirect (FrontendAppPathR [])
