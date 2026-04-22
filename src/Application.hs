{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( getApplicationDev
    , appMain
    , develMain
    , makeFoundation
    , makeLogWare
    -- * for DevelMain
    , getApplicationRepl
    , shutdownApp
    -- * for GHCI
    , handler
    , db
    ) where

import Control.Monad.Logger                 (liftLoc, runLoggingT)
import Database.Persist.Sql                 (ConnectionPool, Single (..),
                                             rawExecute, rawSql)
import Database.Persist.Postgresql          (createPostgresqlPool)
import Database.Persist.Sqlite              (createSqlitePool, runSqlPool,
                                             sqlDatabase, sqlPoolSize)
import Import
import Language.Haskell.TH.Syntax           (qLocation)
import Network.HTTP.Client.TLS              (getGlobalManager)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp             (Settings, defaultSettings,
                                             defaultShouldDisplayException,
                                             runSettings, setHost,
                                             setOnException, setPort, getPort)
import Network.Wai.Middleware.RequestLogger (Destination (Logger),
                                             IPAddrSource (..),
                                             OutputFormat (..), destination,
                                             mkRequestLogger, outputFormat)
import System.Directory                     (doesFileExist)
import qualified System.FilePath            as FP
import System.FilePath                      ((</>), takeExtension)
import System.Log.FastLogger                (defaultBufSize, newStdoutLoggerSet,
                                             toLogStr)
import Yesod.Auth.HashDB                    (setPassword)

import Handler.Api
import Handler.Api.Admin
import Handler.Api.Editor
import Handler.Api.Legacy
import Handler.Api.Public
import Settings                             (DatabaseConfig (..),
                                             databasePoolSize,
                                             postgresqlConnectionString)

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- | This function allocates resources (such as a database connection pool),
-- performs initialization and returns a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
    -- Some basic initializations: HTTP connection manager, logger, and static
    -- subsite.
    appHttpManager <- getGlobalManager
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    appStatic <-
        (if appMutableStatic appSettings then staticDevel else static)
        (appStaticDir appSettings)

    -- We need a log function to create a connection pool. We need a connection
    -- pool to create our foundation. And we need our foundation to get a
    -- logging function. To get out of this loop, we initially create a
    -- temporary foundation without a real connection pool, get a log function
    -- from there, and then create the real foundation.
    let mkFoundation appConnPool = App {..}
        -- The App {..} syntax is an example of record wild cards. For more
        -- information, see:
        -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html
        tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation appLogger

    -- Create the database connection pool
    pool <- flip runLoggingT logFunc $
        case appDatabaseConf appSettings of
            SqliteDatabase sqliteConf ->
                createSqlitePool
                    (sqlDatabase sqliteConf)
                    (sqlPoolSize sqliteConf)
            PostgresqlDatabase postgresConf ->
                createPostgresqlPool
                    (postgresqlConnectionString postgresConf)
                    (databasePoolSize $ appDatabaseConf appSettings)

    -- Perform database migration using our application's logging settings.
    runLoggingT (runSqlPool (migrateSchema $ appDatabaseConf appSettings) pool) logFunc
    seedAdminAccount appSettings pool

    -- Return the foundation
    return $ mkFoundation pool

migrateSchema :: forall m. MonadIO m => DatabaseConfig -> ReaderT SqlBackend m ()
migrateSchema (PostgresqlDatabase _) =
    runMigration migrateAll
migrateSchema (SqliteDatabase _) = do
    tables <- fmap unSingle <$> (rawSql
        "SELECT name FROM sqlite_master WHERE type = 'table'"
        [] :: ReaderT SqlBackend m [Single Text])
    if "article" `elem` tables
        then ensureExistingSqliteSchema
        else runMigration migrateAll

ensureExistingSqliteSchema :: forall m. MonadIO m => ReaderT SqlBackend m ()
ensureExistingSqliteSchema =
    ensureArticleUpdatedAtColumn

ensureArticleUpdatedAtColumn :: forall m. MonadIO m => ReaderT SqlBackend m ()
ensureArticleUpdatedAtColumn = do
    columnNames <- fmap unSingle <$> (rawSql
        "SELECT name FROM pragma_table_info('article') ORDER BY cid"
        [] :: ReaderT SqlBackend m [Single Text])
    unless ("updated_at" `elem` columnNames) $ do
        rawExecute "ALTER TABLE article ADD COLUMN updated_at TIMESTAMP" []
        rawExecute
            "UPDATE article SET updated_at = created_at WHERE updated_at IS NULL"
            []

seedAdminAccount :: AppSettings -> ConnectionPool -> IO ()
seedAdminAccount settings pool = do
    mAdmin <- runSqlPool (getBy $ UniqueUser $ appSeedAdminIdent settings) pool
    case mAdmin of
        Nothing -> do
            seededAdmin <- setPassword (appSeedAdminPassword settings) User
                { userIdent = appSeedAdminIdent settings
                , userPassword = Nothing
                , userDisplayName = Just $ appSeedAdminDisplayName settings
                , userBio = Just $ Textarea "Default administrator account"
                , userIsAdmin = True
                }
            void $ runSqlPool (insert seededAdmin) pool
        Just (Entity adminId adminUser) ->
            unless (userIsAdmin adminUser) $
                runSqlPool (update adminId [UserIsAdmin =. True]) pool

-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applying some additional middlewares.
makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- makeLogWare foundation
    -- Create the WAI application and apply middlewares
    appPlain <- toWaiAppPlain foundation
    return $ logWare $ defaultMiddlewaresNoLogging appPlain

makeLogWare :: App -> IO Middleware
makeLogWare foundation =
    mkRequestLogger def
        { outputFormat =
            if appDetailedRequestLogging $ appSettings foundation
                then Detailed True
                else Apache
                        (if appIpFromHeader $ appSettings foundation
                            then FromFallback
                            else FromSocket)
        , destination = Logger $ loggerSet $ appLogger foundation
        }


-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
      setPort (appPort $ appSettings foundation)
    $ setHost (appHost $ appSettings foundation)
    $ setOnException (\_req e ->
        when (defaultShouldDisplayException e) $ messageLoggerSource
            foundation
            (appLogger foundation)
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e))
      defaultSettings

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app <- makeApplication foundation
    return (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    -- Get the settings from all relevant sources
    settings <- loadYamlSettingsArgs
        -- fall back to compile-time values, set to [] to require values at runtime
        [configSettingsYmlValue]

        -- allow environment variables to override
        useEnv

    -- Generate the foundation from the settings
    foundation <- makeFoundation settings

    -- Generate a WAI Application from the foundation
    app <- makeApplication foundation

    -- Run the application with Warp
    runSettings (warpSettings foundation) app


--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app1 <- makeApplication foundation
    return (getPort wsettings, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp _ = return ()


---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------

-- | Run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h

-- | Run DB queries
db :: ReaderT SqlBackend Handler a -> IO a
db = handler . runDB

getFrontendAppPathR :: [Text] -> Handler TypedContent
getFrontendAppPathR pieces = serveFrontendPath pieces

serveFrontendPath :: [Text] -> Handler TypedContent
serveFrontendPath pieces = do
    app <- getYesod
    let staticRoot = appStaticDir $ appSettings app
        frontendRoot = staticRoot </> "app"
        relativePath = FP.joinPath $ map unpack pieces
        requestedPath =
            if null pieces
                then frontendRoot </> "index.html"
                else frontendRoot </> relativePath
        fallbackPath = frontendRoot </> "index.html"
    requestedExists <- liftIO $ doesFileExist requestedPath
    let targetPath = if requestedExists then requestedPath else fallbackPath
        contentType = frontendContentType targetPath
    sendFile contentType targetPath

frontendContentType :: FilePath -> ContentType
frontendContentType path =
    case takeExtension path of
        ".html" -> "text/html; charset=utf-8"
        ".js" -> "application/javascript; charset=utf-8"
        ".css" -> "text/css; charset=utf-8"
        ".json" -> "application/json; charset=utf-8"
        ".svg" -> "image/svg+xml"
        ".png" -> "image/png"
        ".jpg" -> "image/jpeg"
        ".jpeg" -> "image/jpeg"
        ".webp" -> "image/webp"
        ".woff" -> "font/woff"
        ".woff2" -> "font/woff2"
        _ -> "application/octet-stream"
