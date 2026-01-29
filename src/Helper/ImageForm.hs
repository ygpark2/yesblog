{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Helper.ImageForm where
import Data.Time (UTCTime, getCurrentTime)
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
