{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.ApiPublicSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "Public API" $ do
        it "lists published posts and supports filters" $ do
            Entity authorId _ <- createUser "public-author"
            _ <- createArticleWithContent authorId "Visible Svelte Post" "SvelteKit body text" "visible-svelte-post" False ["svelte", "frontend"]
            _ <- createArticleWithContent authorId "Draft Svelte Post" "should stay hidden" "draft-svelte-post" True ["svelte"]

            request $ do
                setMethod "GET"
                setUrl ApiPostsR
                addGetParam "author" "public-author"
                addGetParam "tag" "svelte"
                addGetParam "q" "SvelteKit"

            statusIs 200
            bodyContains "\"title\":\"Visible Svelte Post\""
            bodyContains "\"author\":\"public-author\""

        it "returns published post details and hides drafts" $ do
            Entity authorId _ <- createUser "detail-author"
            Entity articleId _ <- createArticleWithContent authorId "Readable Article" "Long form content" "readable-article" False ["essay"]
            _ <- createArticleWithContent authorId "Related Article" "Next read" "related-article" False ["essay"]
            _ <- createComment articleId "reader" "Great article"
            _ <- createArticleWithContent authorId "Hidden Draft" "Nope" "hidden-draft" True []

            get $ ApiPostR "readable-article"
            statusIs 200
            bodyContains "\"title\":\"Readable Article\""
            bodyContains "\"content\":\"Long form content\""
            bodyContains "\"name\":\"reader\""
            bodyContains "\"title\":\"Related Article\""

            get $ ApiPostR "hidden-draft"
            statusIs 404

        it "creates, updates, and deletes comments for the signed-in author" $ do
            Entity authorId _ <- createUser "comment-author"
            Entity articleId _ <- createArticle authorId "Commentable Post" "commentable-post"
            userEntity <- createUser "commenter"
            authenticateAs userEntity

            request $ do
                setMethod "POST"
                setUrl $ ApiPostCommentR "commentable-post"
                addPostParam "content" "First comment"

            statusIs 200
            bodyContains "\"name\":\"commenter\""

            comments <- runDB $ selectList [CommentArticle ==. articleId] []
            assertEq "comment count" (length comments) 1
            let [Entity commentId _] = comments

            request $ do
                setMethod "POST"
                setUrl $ ApiCommentUpdateR commentId
                addPostParam "content" "Updated comment"

            statusIs 200
            bodyContains "\"content\":\"Updated comment\""

            mUpdatedComment <- getRecord commentId
            case mUpdatedComment of
                Nothing -> liftIO $ expectationFailure "expected updated comment to exist"
                Just updatedComment ->
                    assertEq "updated comment content" (commentContent updatedComment) "Updated comment"

            request $ do
                setMethod "POST"
                setUrl $ ApiCommentDeleteR commentId

            statusIs 200

            deletedComment <- getRecord commentId
            assertEq "deleted comment flag" (isNothing deletedComment) True

        it "returns a user's published posts" $ do
            Entity authorId _ <- createUserWithProfile "profile-author" (Just "Profile Author") (Just "Readable bio")
            _ <- createArticle authorId "Profile Post" "profile-post"

            get $ ApiUserR "profile-author"
            statusIs 200
            bodyContains "\"displayName\":\"Profile Author\""
            bodyContains "\"title\":\"Profile Post\""
