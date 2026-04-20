{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Helper.ImageForm where
import Data.Time (UTCTime, getCurrentTime)
import Data.Maybe (fromMaybe)
import Import

uploadDirectory :: FilePath
uploadDirectory = "static"

uploadSubDirectory :: FilePath
uploadSubDirectory = "files"

uploadForm :: Form (FileInfo, Maybe Textarea, UTCTime)
uploadForm = renderDivs $ (,,)
    <$> fileAFormReq fsFile
    <*> aopt textareaField fsFileDescr Nothing
    <*> lift (liftIO getCurrentTime)
        where
          fsFile      = (fieldSettingsLabel ("File upload" :: Text)) { fsAttrs = [("class", "col-md-12")]}
          fsFileDescr = (fieldSettingsLabel ("Description" :: Text)) { fsAttrs = [("class", "col-md-12")]}

imageDescriptionForm :: Maybe Textarea -> Form Textarea
imageDescriptionForm description = renderDivs $
    areq textareaField fsImageDescr (Just (fromMaybe (Textarea "") description))
  where
    fsImageDescr = (fieldSettingsLabel ("Image description" :: Text)) { fsAttrs = [("class", "col-md-12")] }
