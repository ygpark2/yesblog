{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Image where

import Import
import System.FilePath
import qualified Data.Text as T
import System.Directory (removeFile, doesFileExist)
import Data.Time
import Helper.ImageForm

getImagesR :: Handler Html
getImagesR = do
    ((_, widget), enctype) <- runFormPost uploadForm
    images <- runDB $ selectList [ImageFilename !=. ""] [Desc ImageDate]
    mmsg <- getMessage
    now <- liftIO $ getCurrentTime
    let hasImages = not (Prelude.null images)
    defaultLayout $ do
      $(widgetFile "admin/image")

postImagesR :: Handler Html
postImagesR = do
    ((result, _), _) <- runFormPost uploadForm
    case result of
        FormSuccess (file, info, date) -> do
            -- DONE: check if image already exists
            -- use "insertBy" function and add UniqueImage filename constraint to config/models
            -- save to image directory
            filename <- writeToServer file
            success <- runDB $ insertBy $ Image filename info date
            case success of
              Right _key -> do
                setMessage "File saved."
                redirect ImagesR
              Left _ -> do
                setMessage "Could not register image."
                redirect ImagesR
        _ -> do
            setMessage "Something went wrong."
            redirect ImagesR

deleteImageR :: ImageId -> Handler ()
deleteImageR imageId = do
    image <- runDB $ get404 imageId
    let filename = imageFilename image
        path = imageFilePath filename
    liftIO $ removeFile path
    -- only delete from database if file has been removed from server
    stillExists <- liftIO $ doesFileExist path

    case (not stillExists) of
        False -> do
            setMessage "Could not register image."
            redirect ImagesR
        True  -> do
            runDB $ delete imageId
            setMessage "File deleted."
            redirect ImagesR

writeToServer :: FileInfo -> Handler FilePath
writeToServer file = do
    let filename = T.unpack $ fileName file
        path = imageFilePath filename
    liftIO $ fileMove file path
    return filename

imageFilePath :: String -> FilePath
imageFilePath f = uploadDirectory </> uploadSubDirectory </> f

imageFilePath' :: String -> FilePath
imageFilePath' f = "/" </> uploadDirectory </> uploadSubDirectory </> f
