{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.ApiAdminSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "Admin API" $ do
        it "returns dashboard data for admins" $ do
            adminEntity <- createAdmin "site-admin"
            Entity authorId _ <- createUser "member-author"
            Entity articleId _ <- createArticle authorId "Admin Visible Post" "admin-visible-post"
            _ <- createComment articleId "reader" "Admin sees this"
            _ <- createImage "admin.png" (Just "Admin image")
            authenticateAs adminEntity

            get ApiAdminDashboardR
            statusIs 200
            bodyContains "\"articleCount\":1"
            bodyContains "\"commentCount\":1"
            bodyContains "\"userCount\":2"
            bodyContains "\"imageCount\":1"
            bodyContains "\"title\":\"Admin Visible Post\""
            bodyContains "\"ident\":\"member-author\""

        it "deletes articles with dependent tags and comments" $ do
            adminEntity <- createAdmin "article-admin"
            Entity authorId _ <- createUser "article-owner"
            Entity articleId _ <- createArticleWithContent authorId "Delete Target" "body" "delete-target" False ["cleanup"]
            _ <- createComment articleId "reader" "delete comment"
            authenticateAs adminEntity

            request $ do
                setMethod "POST"
                setUrl $ ApiAdminArticleDeleteR articleId

            statusIs 200

            deletedArticle <- getRecord articleId
            assertEq "deleted article flag" (isNothing deletedArticle) True
            remainingTags <- runDB $ selectList [TagArticle ==. articleId] []
            remainingComments <- runDB $ selectList [CommentArticle ==. articleId] []
            assertEq "remaining tags" (length remainingTags) 0
            assertEq "remaining comments" (length remainingComments) 0

        it "deletes users and their authored articles but blocks self-delete" $ do
            adminEntity@(Entity adminId _) <- createAdmin "user-admin"
            Entity memberId _ <- createUser "delete-member"
            Entity articleId _ <- createArticle memberId "Member Post" "member-post"
            _ <- createComment articleId "reader" "member comment"
            authenticateAs adminEntity

            request $ do
                setMethod "POST"
                setUrl $ ApiAdminUserDeleteR memberId

            statusIs 200

            deletedUser <- getRecord memberId
            deletedArticle <- getRecord articleId
            assertEq "deleted user flag" (isNothing deletedUser) True
            assertEq "deleted article flag" (isNothing deletedArticle) True

            request $ do
                setMethod "POST"
                setUrl $ ApiAdminUserDeleteR adminId

            statusIs 400
            bodyContains "cannot delete your own admin account"
