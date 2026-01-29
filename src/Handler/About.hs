{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.About where

import Import
import Yesod.Markdown (markdownFromFile, markdownToHtml)
import System.FilePath

getAboutR :: Handler Html
getAboutR = do
  fileData <- liftIO $ markdownFromFile aboutPage
  defaultLayout $ do
    setTitle "About"
    case markdownToHtml fileData of
      Left _ -> toWidget [whamlet|<p>Failed to render the about page.|]
      Right html -> toWidget html

aboutPage :: String
aboutPage = "page" </> "about.md"
