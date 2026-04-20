{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.BlogSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "Blog pagination" $ do
        it "shows older posts link when more than one page exists" $ do
            Entity userId _ <- createUser "author"
            mapM_ (\n -> void $ createArticle userId ("Article " <> tshow n) ("article-" <> tshow n)) [1..11 :: Int]

            get BlogViewR
            statusIs 200
            htmlAnyContain ".pager .next a" "Older posts"

        it "shows newer posts link on later blog pages" $ do
            Entity userId _ <- createUser "author-two"
            mapM_ (\n -> void $ createArticle userId ("Post " <> tshow n) ("post-" <> tshow n)) [1..11 :: Int]

            request $ do
                setMethod "GET"
                setUrl BlogViewR
                addGetParam "page" "2"

            statusIs 200
            htmlAnyContain ".pager .previous a" "Newer posts"

    describe "Search pagination" $ do
        it "shows next link for multi-page search results" $ do
            Entity userId _ <- createUser "search-author"
            mapM_ (\n -> void $ createArticle userId ("Searchable " <> tshow n) ("searchable-" <> tshow n)) [1..11 :: Int]

            request $ do
                setMethod "GET"
                setUrl SearchR
                addGetParam "q" "Searchable"

            statusIs 200
            htmlAnyContain ".pager .next a" "Next"

        it "shows previous link on later search pages" $ do
            Entity userId _ <- createUser "search-author-two"
            mapM_ (\n -> void $ createArticle userId ("Query " <> tshow n) ("query-" <> tshow n)) [1..11 :: Int]

            request $ do
                setMethod "GET"
                setUrl SearchR
                addGetParam "q" "Query"
                addGetParam "page" "2"

            statusIs 200
            htmlAnyContain ".pager .previous a" "Previous"

        it "shows a result summary for matching searches" $ do
            Entity userId _ <- createUser "search-summary-author"
            mapM_ (\n -> void $ createArticle userId ("Summary " <> tshow n) ("summary-" <> tshow n)) [1..3 :: Int]

            request $ do
                setMethod "GET"
                setUrl SearchR
                addGetParam "q" "Summary"

            statusIs 200
            htmlAnyContain ".search-summary" "Showing 1-3 of 3 results for \"Summary\"."

    describe "Archive view" $ do
        it "groups published posts by month" $ do
            Entity userId _ <- createUser "archive-author"
            _ <- createArticle userId "Archive one" "archive-one"
            _ <- createArticle userId "Archive two" "archive-two"

            get ArchiveR
            statusIs 200
            htmlAnyContain "h1" "Archive"
            htmlAnyContain "h2" "-"
            htmlAnyContain "a" "Archive one"

    describe "Tag view" $ do
        it "shows tagged articles with a summary count" $ do
            Entity userId _ <- createUser "tag-author"
            Entity articleId _ <- createArticle userId "Tagged post" "tagged-post"
            _ <- runDB $ insert $ Tag "haskell" articleId

            get $ TagR "haskell"
            statusIs 200
            htmlAnyContain "h1" "Tag: haskell"
            htmlAnyContain "p" "1 published posts"
            htmlAnyContain "a" "Tagged post"
