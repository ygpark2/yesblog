{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Image where

import Import
import System.FilePath
import qualified Data.Text as T
import System.Directory (removeFile, doesFileExist)
import Helper.ImageForm

getImagesR :: Handler Html
getImagesR = do
    _ <- requireAdmin
    ((_, uploadWidget), uploadEnctype) <- runFormPost uploadForm
    images <- runDB $ selectList [ImageFilename !=. ""] [Desc ImageDate]
    imageCards <- forM images $ \(Entity imageId image) -> do
        (descriptionWidget, descriptionEnctype) <- generateFormPost $ imageDescriptionForm (imageDescription image)
        pure (imageId, image, descriptionWidget, descriptionEnctype)
    mmsg <- getMessage
    let hasImages = not (Prelude.null images)
    defaultLayout $ do
      $(widgetFile "admin/image")

postImagesR :: Handler Html
postImagesR = do
    _ <- requireAdmin
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
    _ <- requireAdmin
    image <- runDB $ get404 imageId
    let filename = imageFilename image
        filePath = imageFilePath filename
    liftIO $ removeFile filePath
    -- only delete from database if file has been removed from server
    stillExists <- liftIO $ doesFileExist filePath

    case (not stillExists) of
        False -> do
            setMessage "Could not register image."
            redirect ImagesR
        True  -> do
            runDB $ delete imageId
            setMessage "File deleted."
            redirect ImagesR

postImageUpdateR :: ImageId -> Handler Html
postImageUpdateR imageId = do
    _ <- requireAdmin
    image <- runDB $ get404 imageId
    ((result, _), _) <- runFormPost $ imageDescriptionForm (imageDescription image)
    case result of
        FormSuccess description -> do
            runDB $ update imageId [ImageDescription =. normalizeDescription description]
            setMessage "Image description updated."
            redirect ImagesR
        _ -> do
            setMessage "Could not update image description."
            redirect ImagesR

writeToServer :: FileInfo -> Handler FilePath
writeToServer file = do
    let filename = T.unpack $ fileName file
        filePath = imageFilePath filename
    liftIO $ fileMove file filePath
    return filename

imageFilePath :: String -> FilePath
imageFilePath f = uploadDirectory </> uploadSubDirectory </> f

imageFilePath' :: String -> FilePath
imageFilePath' f = "/" </> uploadDirectory </> uploadSubDirectory </> f

markdownImageSnippet :: String -> Text
markdownImageSnippet filename =
    T.concat ["![](\"", T.pack (imageFilePath' filename), "\")"]

normalizeDescription :: Textarea -> Maybe Textarea
normalizeDescription description
    | T.strip (unTextarea description) == "" = Nothing
    | otherwise = Just description

isPreviewableImage :: String -> Bool
isPreviewableImage filename =
    takeExtension filename `Prelude.elem` [".jpg", ".jpeg", ".png", ".gif", ".webp", ".svg"]

requireAdmin :: Handler (Entity User)
requireAdmin = do
    entity@(Entity _ user) <- requireAuth
    if userIsAdmin user
        then pure entity
        else permissionDenied "Admin access required"
