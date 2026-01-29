{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Admin where


import Import
import Yesod.Auth
import Data.Time
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Control.Monad as CM
import Helper.Form
import Helper.MakeBrief
import Helper.Sidebar
import Handler.Image

getAdminR :: Handler Html
getAdminR = do
  -- Get the list of articles inside the database
  let page = 10
  Entity _ user <- requireAuth
  let username = userIdent user
  (articles, users, comments) <- runDB $ do
     articles <- selectList [] [Desc ArticleCreatedAt, LimitTo page]
     users <- selectList [] [Desc UserIdent]
     comments <- selectList [] [Desc CommentPosted]
     return (articles, users, comments)
  -- We'll need the two "objects": articleWidget and enctype
  -- to construct the form (see templates/articles.hamlet).
  (articleWidget, enctype) <- generateFormPost entryForm
  maid <- maybeAuthId
  defaultLayout $ do
    setTitle "Admin"
    $(widgetFile "admin/index")

postAdminR :: Handler Html
postAdminR = do
  ((res,articleWidget),enctype) <- runFormPost entryForm
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
  Entity _ user <- requireAuth
  let username = userIdent user
  -- Get the list of articles inside the database
  (articles, images) <- runDB $ do
    articles <- selectList [][Desc ArticleCreatedAt]
    images   <- selectList [ImageFilename !=. ""] [Desc ImageDate]
    return (articles, images)
  now <- liftIO $ getCurrentTime
  -- We'll need the two "objects": articleWidget and enctype
  -- to construct the form (see templates/articles.hamlet).
  (articleWidget, enctype) <- generateFormPost entryForm
  maid <- maybeAuthId
  defaultLayout $ do
    setTitle "Admin"
    $(widgetFile "admin/new")

getArticleR :: ArticleId -> Handler Html
getArticleR articleId = do
  now <- liftIO $ getCurrentTime
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
  (Entity _ user) <- requireAuth
  let username = userIdent user
  ((res,articleWidget),enctype) <- runFormPost entryForm
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
  (Entity _ user) <- requireAuth
  (post, oldTags) <- runDB $ do
    post <- get404 articleId
    oldTags <- Prelude.map (\(Entity _ t) -> tagName t)
               <$> (selectList [TagArticle ==. articleId] [])
    return (post, oldTags)
  let username = userIdent user
  maid <- maybeAuthId
  (postWidget, enctype) <- generateFormPost $ (postForm (Just post) (Just oldTags))
  defaultLayout $ do
    setTitle "Edit article"
    $(widgetFile "admin/edit")

postPreviewR :: Handler Html
postPreviewR = do
  (Entity _ user) <- requireAuth
  let username = userIdent user
  ((res,previewWidget),enctype) <- runFormPost entryForm
  case res of
    FormSuccess (article, tags) -> do
      now <- liftIO $ getCurrentTime
      defaultLayout $ do
        $(widgetFile "admin/preview")
    _ -> do
      setMessage "Something went wrong."
      redirect NewBlogR

postArticleEditR :: ArticleId -> Handler Html
postArticleEditR articleId = do
  maid <- maybeAuthId
  (Entity _ user) <- requireAuth
  let username = userIdent user
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
       _ -> defaultLayout $ do
         setTitle "Post failed"
         $(widgetFile "admin/edit")

getArticleDeleteR :: ArticleId -> Handler Html
getArticleDeleteR articleId = do
  (article, oldTags) <- runDB $ do
    _article <- get404 articleId
    oldTags <- Prelude.map (\(Entity _ t) -> tagName t)
                 <$> (selectList [TagArticle ==. articleId] [])
    return (_article, oldTags)
  (postWidget, enctype) <- generateFormPost $ (postForm (Just article) (Just oldTags))
  defaultLayout $ do
    setTitle "Delete article"
    $(widgetFile "admin/delete")

postArticleDeleteR :: ArticleId -> Handler Html
postArticleDeleteR articleId = do
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
