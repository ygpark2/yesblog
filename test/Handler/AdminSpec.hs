{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.AdminSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "Admin routes" $ do
        it "rejects non-admin users from admin dashboard" $ do
            userEntity <- createUser "member"
            authenticateAs userEntity

            get AdminR
            statusIs 403

        it "allows admin users into admin dashboard" $ do
            adminEntity <- createAdmin "admin"
            authenticateAs adminEntity

            get AdminR
            statusIs 200

        it "shows dashboard sections for recent content" $ do
            adminEntity@(Entity adminId _) <- createAdmin "dashboard-admin"
            _ <- createArticle adminId "Dashboard post" "dashboard-post"
            _ <- createImage "dashboard.png" (Just "Dashboard image")
            _ <- createUser "dashboard-member"
            authenticateAs adminEntity

            get AdminR
            statusIs 200
            htmlAnyContain "h1" "Admin dashboard"
            htmlAnyContain "h2" "Latest articles"
            htmlAnyContain "h2" "Latest comments"
            htmlAnyContain "h2" "Recent users"
            htmlAnyContain "h2" "Recent images"
            htmlAnyContain "a" "Write a new article"

        it "rejects non-admin users from image management" $ do
            userEntity <- createUser "member-image"
            authenticateAs userEntity

            get ImagesR
            statusIs 403

        it "allows admin users into image management" $ do
            adminEntity <- createAdmin "admin-image"
            authenticateAs adminEntity

            get ImagesR
            statusIs 200

        it "shows uploaded image snippets on the new article page" $ do
            adminEntity <- createAdmin "admin-new"
            _ <- createImage "editor.png" (Just "Editor image")
            authenticateAs adminEntity

            get NewBlogR
            statusIs 200
            htmlAnyContain "h2" "Image library"
            htmlAnyContain "input" "/static/files/editor.png"
            htmlAnyContain "input" "![](\"/static/files/editor.png\")"
            htmlAnyContain "button" "Insert markdown"

        it "shows uploaded image snippets on the edit article page" $ do
            adminEntity@(Entity adminId _) <- createAdmin "admin-edit"
            articleEntity <- createArticle adminId "Editable post" "editable-post"
            _ <- createImage "edit.png" (Just "Edit image")
            authenticateAs adminEntity

            get $ ArticleEditR (entityKey articleEntity)
            statusIs 200
            htmlAnyContain "h2" "Image library"
            htmlAnyContain "input" "/static/files/edit.png"
            htmlAnyContain "input" "![](\"/static/files/edit.png\")"
            htmlAnyContain "button" "Insert markdown"
