{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Admin where


import Import
import qualified Control.Monad as CM
import qualified Data.Text as T
import System.FilePath (takeExtension)
import Helper.Form

loadRecentImages :: Handler [Entity Image]
loadRecentImages = runDB $ selectList [ImageFilename !=. ""] [Desc ImageDate, LimitTo 12]

imagePublicPath :: String -> Text
imagePublicPath filename = T.pack ("/static/files/" <> filename)

editorImageSnippet :: String -> Text
editorImageSnippet filename = T.concat ["![](\"", imagePublicPath filename, "\")"]

previewableImage :: String -> Bool
previewableImage filename =
  takeExtension filename `Prelude.elem` [".jpg", ".jpeg", ".png", ".gif", ".webp", ".svg"]

getAdminR :: Handler Html
getAdminR = do
  _ <- requireAdmin
  let page = 10
  Entity currentUserId user <- requireAuth
  let username = userIdent user
  (articles, users, comments, images) <- runDB $ do
     articles <- selectList [] [Desc ArticleCreatedAt, LimitTo page]
     users <- selectList [] [Asc UserIdent, LimitTo page]
     comments <- selectList [] [Desc CommentPosted, LimitTo page]
     images <- selectList [ImageFilename !=. ""] [Desc ImageDate, LimitTo page]
     return (articles, users, comments, images)
  let articleCount = Prelude.length articles
      userCount = Prelude.length users
      commentCount = Prelude.length comments
      imageCount = Prelude.length images
  (articleWidget, enctype) <- generateFormPost entryForm
  mmsg <- getMessage
  defaultLayout $ do
    setTitle "Admin"
    $(widgetFile "admin/index")

postAdminR :: Handler Html
postAdminR = do
  _ <- requireAdmin
  ((res,_),_) <- runFormPost entryForm
  case res of
    FormSuccess (article, tags) -> do
      articleId <- runDB $ do
        _article <- insert article
        CM.forM_ tags $ \tag -> insert $ Tag tag _article
        return _article
      renderer <- getUrlRenderParams
      let html = [hamlet|<span .notice><h4>#{articleTitle article}</h4>
                                       <p> created
                                       <a href=@{AdminR}>Back Admin</a>|]
      setMessage $ toHtml $ html renderer
      redirect $ ArticleR articleId
    _ -> defaultLayout $ do
            setTitle "Post failed"
            $(widgetFile "admin/articleAddError")

getNewBlogR :: Handler Html
getNewBlogR = do
  _ <- requireAdmin
  images <- loadRecentImages
  (articleWidget, enctype) <- generateFormPost entryForm
  defaultLayout $ do
    setTitle "Admin"
    $(widgetFile "admin/new")

getArticleR :: ArticleId -> Handler Html
getArticleR articleId = do
  _ <- requireAdmin
  (comments, article, author, tags) <- runDB $ do
    comments <- Prelude.map entityVal <$>
                selectList [CommentArticle ==. articleId] [Asc CommentPosted]
    article  <- get404 articleId
    Entity key articleEntity <- getBy404 $ UniqueSlug (articleSlug article)
    let authorId = articleAuthor articleEntity
    author <- get404 authorId
    tags <- Prelude.map (\e -> tagName (entityVal e)) <$> selectList [TagArticle ==. key] []
    return (comments, article, author, tags)
  let screenAuthor = userIdent author
  let hasTags = not (Prelude.null tags)
  let hasComments = not (Prelude.null comments)
  ((_, commentWidget), enctype) <- runFormPost $ commentForm articleId
  defaultLayout $ do
    setTitle $ toHtml $ articleTitle article
    $(widgetFile "admin/article")

postArticleR :: ArticleId -> Handler Html
postArticleR articleId = do
  _ <- requireAdmin
  ((res,_), _) <- runFormPost $ commentForm articleId
  case res of
    FormSuccess comment -> do
      _ <- runDB $ insert comment
      setMessage "Comment posted."
      redirect $ ArticleR articleId
    _ -> do
      setMessage "Could not post comment."
      redirect $ ArticleR articleId

postNewBlogR :: Handler Html
postNewBlogR = do
  _ <- requireAdmin
  ((res,_),_) <- runFormPost entryForm
  case res of
    FormSuccess (article, tags) -> do
      articleId <- runDB $ do
        _article <- insert article
        CM.forM_ tags $ \tag -> insert $ Tag tag _article
        return _article
      renderer <- getUrlRenderParams
      let html = [hamlet|<span .notice><h4>#{articleTitle article}</h4>
                                       <p> created
                                       <a href=@{AdminR}>Back Admin</a>|]
      setMessage $ toHtml $ html renderer
      redirect $ ArticleR articleId
    _ -> defaultLayout $ do
           setTitle "Post failed"
           $(widgetFile "admin/articleAddError")

getArticleEditR :: ArticleId -> Handler Html
getArticleEditR articleId = do
  _ <- requireAdmin
  (post, oldTags) <- runDB $ do
    post <- get404 articleId
    oldTags <- Prelude.map (\(Entity _ t) -> tagName t)
               <$> (selectList [TagArticle ==. articleId] [])
    return (post, oldTags)
  images <- loadRecentImages
  (postWidget, enctype) <- generateFormPost $ (postForm (Just post) (Just oldTags))
  defaultLayout $ do
    setTitle "Edit article"
    $(widgetFile "admin/edit")

postPreviewR :: Handler Html
postPreviewR = do
  _ <- requireAdmin
  ((res,previewWidget),_) <- runFormPost entryForm
  case res of
    FormSuccess _ -> do
      defaultLayout $ do
        $(widgetFile "admin/preview")
    _ -> do
      setMessage "Something went wrong."
      redirect NewBlogR

postArticleEditR :: ArticleId -> Handler Html
postArticleEditR articleId = do
  _ <- requireAdmin
  ((res, postWidget), enctype) <- runFormPost entryForm
  case res of
       FormSuccess (post, tags) -> do
         runDB $ do
           update articleId [ ArticleTitle   =. articleTitle   post
                            , ArticleContent =. articleContent post
                            , ArticleSlug    =. articleSlug    post
                            , ArticleDraft   =. articleDraft   post]
           deleteWhere [TagArticle ==. articleId]
           CM.forM_ tags $ \tag -> insert $ Tag tag articleId
         renderer <- getUrlRenderParams
         let html = [hamlet|<span .notice><h4>#{articleTitle post}</h4>
                                          <p> updated
                                          <a href=@{ArticleEditR articleId}>Back Edit</a>|]
         setMessage $ toHtml $ html renderer
         redirect $ ArticleR articleId
       _ -> do
         images <- loadRecentImages
         defaultLayout $ do
           setTitle "Post failed"
           $(widgetFile "admin/edit")

getArticleDeleteR :: ArticleId -> Handler Html
getArticleDeleteR articleId = do
  _ <- requireAdmin
  _ <- runDB $ get404 articleId
  defaultLayout $ do
    setTitle "Delete article"
    $(widgetFile "admin/delete")

postArticleDeleteR :: ArticleId -> Handler Html
postArticleDeleteR articleId = do
  _ <- requireAdmin
  article <- runDB $ do
    _post <- get404 articleId
    delete articleId
    deleteWhere [CommentArticle ==. articleId]
    deleteWhere [TagArticle ==. articleId]
    return _post
  renderer <- getUrlRenderParams
  let html = [hamlet|<span .notice><h4>#{articleTitle article}</h4>
                                   <p> deleted|]
  setMessage $ toHtml $ html renderer
  redirect $ AdminR

getCommentDeleteR :: CommentId -> Handler Html
getCommentDeleteR commentId = do
  _ <- requireAdmin
  comment <- runDB $ do
    _post <- get404 commentId
    delete commentId
    return _post
  renderer <- getUrlRenderParams
  let html = [hamlet|<span .notice><h4>#{commentName comment}</h4>
                                   <p> deleted|]
  setMessage $ toHtml $ html renderer
  redirect $ AdminR

getUserDeleteR :: UserId -> Handler Html
getUserDeleteR usrId = do
  _ <- requireAdmin
  (Entity userId _) <- requireAuth
  if usrId /= userId
    then do
      deleteuser <- runDB $ do
        _user <- get404 usrId
        delete usrId
        return _user
      renderer <- getUrlRenderParams
      let html = [hamlet|<span .notice><h4>User:#{userIdent deleteuser}</h4>
                                       <p> deleted|]
      setMessage $ toHtml $ html renderer
      redirect $ AdminR
   else do
     setMessage "You cannot delete yourself."
     redirect $ AdminR

requireAdmin :: Handler (Entity User)
requireAdmin = do
  entity@(Entity _ user) <- requireAuth
  if userIsAdmin user
    then pure entity
    else permissionDenied "Admin access required"
