{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Api.Admin where

import Import
import Handler.Api.Shared
import Database.Persist.Sql (fromSqlKey)

getApiAdminDashboardR :: Handler Value
getApiAdminDashboardR = do
    _ <- requireAdminAuth
    recentArticles <- runDB $ selectList [] [Desc ArticleUpdatedAt, LimitTo 10]
    recentComments <- runDB $ selectList [] [Desc CommentPosted, LimitTo 10]
    recentUsers <- runDB $ selectList [] [Asc UserIdent, LimitTo 10]
    articleTags <- loadTagMap (map entityKey recentArticles)
    articleAuthors <- loadAuthorMap recentArticles
    articleCommentCounts <- loadCommentCountMap (map entityKey recentArticles)
    commentArticleMap <- loadCommentArticleMap recentComments
    userArticleStats <- loadUserArticleStats recentUsers
    articleCount <- runDB $ count ([] :: [Filter Article])
    commentCount <- runDB $ count ([] :: [Filter Comment])
    userCount <- runDB $ count ([] :: [Filter User])
    imageCount <- runDB $ count ([] :: [Filter Image])
    returnJson $ object
        [ "meta" .= object
            [ "articleCount" .= articleCount
            , "commentCount" .= commentCount
            , "userCount" .= userCount
            , "imageCount" .= imageCount
            ]
        , "articles" .= map (adminArticleValue articleTags articleAuthors articleCommentCounts) recentArticles
        , "comments" .= map (adminCommentValue commentArticleMap) recentComments
        , "users" .= map (adminUserValue userArticleStats) recentUsers
        ]

postApiAdminArticleDeleteR :: ArticleId -> Handler Value
postApiAdminArticleDeleteR articleId = do
    _ <- requireAdminAuth
    deletedArticle <- runDB $ deleteArticleCascade articleId
    returnJson $ object
        [ "deletedArticleId" .= fromSqlKey articleId
        , "slug" .= articleSlug deletedArticle
        ]

postApiAdminCommentDeleteR :: CommentId -> Handler Value
postApiAdminCommentDeleteR commentId = do
    _ <- requireAdminAuth
    _ <- runDB $ get404 commentId
    runDB $ delete commentId
    returnJson $ object ["deletedCommentId" .= fromSqlKey commentId]

postApiAdminUserDeleteR :: UserId -> Handler Value
postApiAdminUserDeleteR userId = do
    Entity currentAdminId _ <- requireAdminAuth
    when (userId == currentAdminId) $
        apiError status400 "You cannot delete your own admin account."
    deletedUser <- runDB $ deleteUserCascade userId
    returnJson $ object
        [ "deletedUserId" .= fromSqlKey userId
        , "ident" .= userIdent deletedUser
        ]
