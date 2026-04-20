{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.ImageSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "Image management" $ do
        it "shows preview and reusable paths for uploaded images" $ do
            adminEntity <- createAdmin "image-admin"
            _ <- createImage "example.png" (Just "Preview image")
            authenticateAs adminEntity

            get ImagesR
            statusIs 200
            htmlAnyContain "img" "example.png"
            htmlAnyContain "input" "/static/files/example.png"
            htmlAnyContain "input" "![](\"/static/files/example.png\")"

        it "shows description edit controls on the management page" $ do
            adminEntity <- createAdmin "image-admin-update"
            _ <- createImage "example.png" (Just "Old description")
            authenticateAs adminEntity

            get ImagesR
            statusIs 200
            htmlAnyContain "label" "Image description"
            htmlAnyContain "button" "Save description"
