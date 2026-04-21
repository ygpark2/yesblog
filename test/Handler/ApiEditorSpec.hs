{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.ApiEditorSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "Editor API" $ do
        it "returns bootstrap and mine data for the authenticated writer" $ do
            writerEntity@(Entity writerId _) <- createUser "studio-writer"
            _ <- createArticleWithContent writerId "Draft Entry" "draft body" "draft-entry" True ["draft-tag"]
            _ <- createArticleWithContent writerId "Published Entry" "published body" "published-entry" False ["published-tag"]
            _ <- createImage "studio.png" (Just "Studio image")
            authenticateAs writerEntity

            get ApiEditorBootstrapR
            statusIs 200
            bodyContains "\"ident\":\"studio-writer\""
            bodyContains "\"draftCount\":1"
            bodyContains "\"publishedCount\":1"
            bodyContains "\"filename\":\"studio.png\""

            get ApiEditorMineR
            statusIs 200
            bodyContains "\"title\":\"Draft Entry\""
            bodyContains "\"title\":\"Published Entry\""

        it "saves new articles and resolves slug collisions" $ do
            writerEntity@(Entity writerId _) <- createUser "save-writer"
            _ <- createArticleWithContent writerId "Existing Post" "existing body" "same-slug" False []
            authenticateAs writerEntity

            request $ do
                setMethod "POST"
                setUrl ApiEditorSaveR
                addPostParam "title" "New Saved Post"
                addPostParam "content" "Editor content"
                addPostParam "slug" "same-slug"
                addPostParam "tags" "svelte, studio"
                addPostParam "draft" "false"

            statusIs 200
            bodyContains "\"slug\":\"same-slug-2\""
            bodyContains "\"permalink\":\"/app/posts/same-slug-2\""

            savedArticle <- runDB $ getBy $ UniqueSlug "same-slug-2"
            case savedArticle of
                Nothing -> liftIO $ expectationFailure "expected saved article with unique slug"
                Just (Entity articleId article) -> do
                    assertEq "saved draft flag" (articleDraft article) False
                    savedTags <- runDB $ selectList [TagArticle ==. articleId] [Asc TagName]
                    assertEq "saved tags" (map (tagName . entityVal) savedTags) ["studio", "svelte"]

        it "autosaves and updates an existing draft" $ do
            writerEntity@(Entity writerId _) <- createUser "autosave-writer"
            Entity articleId _ <- createArticleWithContent writerId "Old Draft" "old body" "old-draft" True ["old"]
            authenticateAs writerEntity

            request $ do
                setMethod "POST"
                setUrl ApiEditorAutosaveR
                addPostParam "articleId" (tshow $ fromSqlKey articleId)
                addPostParam "title" "Updated Draft"
                addPostParam "content" "updated body"
                addPostParam "slug" "updated-draft"
                addPostParam "tags" "refined, updated"

            statusIs 200
            bodyContains "\"slug\":\"updated-draft\""

            mUpdatedArticle <- getRecord articleId
            case mUpdatedArticle of
                Nothing -> liftIO $ expectationFailure "expected updated article to exist"
                Just updatedArticle -> do
                    assertEq "updated title" (articleTitle updatedArticle) "Updated Draft"
                    assertEq "updated draft flag" (articleDraft updatedArticle) True
                    assertEq "updated slug" (articleSlug updatedArticle) "updated-draft"

            updatedTags <- runDB $ selectList [TagArticle ==. articleId] [Asc TagName]
            assertEq "updated tags" (map (tagName . entityVal) updatedTags) ["refined", "updated"]

        it "deletes owned articles together with tags and comments" $ do
            writerEntity@(Entity writerId _) <- createUser "delete-writer"
            Entity articleId _ <- createArticleWithContent writerId "Delete Me" "bye" "delete-me" False ["cleanup"]
            _ <- createComment articleId "reader" "remove me"
            authenticateAs writerEntity

            request $ do
                setMethod "POST"
                setUrl $ ApiEditorDeleteR articleId

            statusIs 200

            deletedArticle <- getRecord articleId
            assertEq "deleted article flag" (isNothing deletedArticle) True
            remainingTags <- runDB $ selectList [TagArticle ==. articleId] []
            remainingComments <- runDB $ selectList [CommentArticle ==. articleId] []
            assertEq "remaining tags" (length remainingTags) 0
            assertEq "remaining comments" (length remainingComments) 0
