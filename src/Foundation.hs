{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamlet)
import Text.Jasmine         (minifym)
import Control.Monad.Logger (LogSource)

-- Used only when in "auth-dummy-login" setting is enabled.
import Yesod.Auth.Dummy

import Yesod.Auth.HashDB    (HashDBUser (..))
import Yesod.Auth.Message   (AuthMessage (InvalidUsernamePass))
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import Data.Kind            (Type)

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a = forall (m :: Type -> Type).
    (MonadIO m) => ReaderT SqlBackend m a

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot :: Approot App
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
    yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware

    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        pc <- widgetToPageContent widget
        withUrlRenderer [hamlet|
$doctype 5
<html lang="en">
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    ^{pageHead pc}
  <body>
    ^{pageBody pc}
|]

    -- The page to be redirected to when authentication is required.
    authRoute
        :: App
        -> Maybe (Route App)
    authRoute _ = Just $ FrontendAppPathR ["login"]

    isAuthorized
        :: Route App  -- ^ The route the user is visiting.
        -> Bool       -- ^ Whether or not this is a "write" request.
        -> Handler AuthResult
    -- Routes not requiring authentication.
    isAuthorized HomeR _ = return Authorized
    isAuthorized (FrontendAppPathR _) _ = return Authorized
    isAuthorized ApiPostsR _ = return Authorized
    isAuthorized (ApiPostR _) _ = return Authorized
    isAuthorized (ApiPostCommentR _) _ = return Authorized
    isAuthorized (ApiCommentUpdateR _) _ = isAuthenticated
    isAuthorized (ApiCommentDeleteR _) _ = isAuthenticated
    isAuthorized (ApiUserR _) _ = return Authorized
    isAuthorized (ApiUserMembershipR _) _ = isAuthenticated
    isAuthorized ApiAuthLoginR _ = return Authorized
    isAuthorized ApiAuthRegisterR _ = return Authorized
    isAuthorized ApiAuthLogoutR _ = return Authorized
    isAuthorized ApiSessionR _ = return Authorized
    isAuthorized ApiMeR _ = isAuthenticated
    isAuthorized ApiMeUpdateR _ = isAuthenticated
    isAuthorized ApiMePlanR _ = isAuthenticated
    isAuthorized ApiMeDomainsR _ = isAuthenticated
    isAuthorized ApiMeMembershipsR _ = isAuthenticated
    isAuthorized ApiMeMembershipOrdersR _ = isAuthenticated
    isAuthorized ApiMeMembershipRefreshR _ = isAuthenticated
    isAuthorized (ApiMeMembershipOrderUpdateR _) _ = isAuthenticated
    isAuthorized (ApiMeMembershipUpdateR _) _ = isAuthenticated
    isAuthorized ApiMeThemeR _ = isAuthenticated
    isAuthorized ApiMeThemesR _ = isAuthenticated
    isAuthorized ApiMeThemeOrdersR _ = isAuthenticated
    isAuthorized ApiMeThemeStatsR _ = isAuthenticated
    isAuthorized ApiMeThemePayoutsR _ = isAuthenticated
    isAuthorized ApiMeThemePayoutRequestR _ = isAuthenticated
    isAuthorized ApiThemesR _ = isAuthenticated
    isAuthorized ApiThemeMarketplaceR _ = return Authorized
    isAuthorized ApiThemeCreateR _ = isAuthenticated
    isAuthorized (ApiThemeUpdateR _) _ = isAuthenticated
    isAuthorized (ApiThemeDeleteR _) _ = isAuthenticated
    isAuthorized (ApiThemeReviewUpsertR _) _ = isAuthenticated
    isAuthorized (ApiThemeReportR _) _ = isAuthenticated
    isAuthorized (ApiThemePurchaseR _) _ = isAuthenticated
    isAuthorized (ApiThemePurchaseConfirmR _) _ = isAdminUser
    isAuthorized (ApiThemeForkR _) _ = isAuthenticated
    isAuthorized ApiAdminDashboardR _ = isAdminUser
    isAuthorized ApiAdminMembershipOrdersR _ = isAdminUser
    isAuthorized ApiAdminThemeOrdersR _ = isAdminUser
    isAuthorized ApiAdminThemePayoutsR _ = isAdminUser
    isAuthorized ApiAdminThemeReviewR _ = isAdminUser
    isAuthorized ApiAdminThemesR _ = isAdminUser
    isAuthorized (ApiAdminThemeOrderUpdateR _) _ = isAdminUser
    isAuthorized (ApiAdminThemePayoutUpdateR _) _ = isAdminUser
    isAuthorized (ApiAdminThemeApproveR _) _ = isAdminUser
    isAuthorized (ApiAdminThemeRejectR _) _ = isAdminUser
    isAuthorized (ApiAdminThemeUpdateR _) _ = isAdminUser
    isAuthorized (ApiAdminThemeDeleteR _) _ = isAdminUser
    isAuthorized (ApiAdminArticleDeleteR _) _ = isAdminUser
    isAuthorized (ApiAdminCommentDeleteR _) _ = isAdminUser
    isAuthorized (ApiAdminUserDeleteR _) _ = isAdminUser
    isAuthorized (ApiAdminMembershipOrderUpdateR _) _ = isAdminUser
    isAuthorized ApiEditorBootstrapR _ = isAuthenticated
    isAuthorized ApiEditorMineR _ = isAuthenticated
    isAuthorized (ApiEditorArticleR _) _ = isAuthenticated
    isAuthorized (ApiEditorDeleteR _) _ = isAuthenticated
    isAuthorized ApiEditorUploadR _ = isAuthenticated
    isAuthorized (ApiEditorImageUpdateR _) _ = isAuthenticated
    isAuthorized (ApiEditorImageDeleteR _) _ = isAuthenticated
    isAuthorized ApiEditorSaveR _ = isAuthenticated
    isAuthorized ApiEditorAutosaveR _ = isAuthenticated
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized


    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent
        :: Text  -- ^ The file extension
        -> Text -- ^ The MIME content type
        -> LByteString -- ^ The contents of the file
        -> Handler (Maybe (Either Text (Route App, [(Text, Text)])))
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger :: App -> IO Logger
    makeLogger = return . appLogger

-- Define breadcrumbs.
instance YesodBreadcrumbs App where
    -- Takes the route that the user is currently on, and returns a tuple
    -- of the 'Text' that you want the label to display, and a previous
    -- breadcrumb route.
    breadcrumb
        :: Route App  -- ^ The route the user is visiting currently.
        -> Handler (Text, Maybe (Route App))
    breadcrumb HomeR = return ("Home", Nothing)
    breadcrumb  _ = return ("home", Nothing)

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest :: App -> Route App
    loginDest _ = FrontendAppPathR []
    -- Where to send a user after logout
    logoutDest :: App -> Route App
    logoutDest _ = FrontendAppPathR []
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer :: App -> Bool
    redirectToReferer _ = True

    authenticate :: (MonadHandler m, HandlerSite m ~ App)
                 => Creds App -> m (AuthenticationResult App)
    authenticate creds = liftHandler $ runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Authenticated uid
            Nothing -> return $ UserError InvalidUsernamePass

    -- You can add other plugins like Google Email, email or OAuth here
    authPlugins :: App -> [AuthPlugin App]
    authPlugins app = extraAuthPlugins
        -- Enable authDummy login if enabled.
        where extraAuthPlugins = [authDummy | appAuthDummyLogin $ appSettings app]

    loginHandler :: AuthHandler App Html
    loginHandler = do
        liftHandler $ redirect (FrontendAppPathR ["login"])

-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    return $ case muid of
        Nothing -> Unauthorized "You must login to access this page"
        Just _ -> Authorized

isAdminUser :: Handler AuthResult
isAdminUser = do
    muser <- maybeAuth
    return $ case muser of
        Nothing -> Unauthorized "You must login to access this page"
        Just (Entity _ user)
            | userIsAdmin user -> Authorized
            | otherwise -> Unauthorized "Admin access required"

instance YesodAuthPersist App

instance HashDBUser User where
    userPasswordHash = userPassword
    setPasswordHash passwordHash user = user { userPassword = Just passwordHash }

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage :: App -> [Lang] -> FormMessage -> Text
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
