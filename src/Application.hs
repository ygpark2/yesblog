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
    seedDefaultThemes pool
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
ensureExistingSqliteSchema = do
    ensureArticleUpdatedAtColumn
    ensureArticlePublishingColumns
    ensureThemeTable
    ensureThemeMarketplaceColumns
    ensureThemePurchaseTable
    ensureThemeOrderTable
    ensureThemeReviewTable
    ensureThemePayoutTable
    ensureThemeRatingTable
    ensureThemeReportTable
    ensureCustomDomainTable
    ensureMembershipTable
    ensureMembershipOrderTable
    ensureUserThemeColumns
    ensureUserPlanColumns

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

ensureArticlePublishingColumns :: forall m. MonadIO m => ReaderT SqlBackend m ()
ensureArticlePublishingColumns = do
    columnNames <- fmap unSingle <$> (rawSql
        "SELECT name FROM pragma_table_info('article') ORDER BY cid"
        [] :: ReaderT SqlBackend m [Single Text])
    unless ("visibility" `elem` columnNames) $
        rawExecute "ALTER TABLE article ADD COLUMN visibility VARCHAR NOT NULL DEFAULT 'public'" []
    unless ("publish_at" `elem` columnNames) $
        rawExecute "ALTER TABLE article ADD COLUMN publish_at TIMESTAMP NULL" []

ensureThemeTable :: forall m. MonadIO m => ReaderT SqlBackend m ()
ensureThemeTable = do
    rawExecute
        "CREATE TABLE IF NOT EXISTS theme (id INTEGER PRIMARY KEY, name VARCHAR NOT NULL, slug VARCHAR NOT NULL, description VARCHAR NULL, author INTEGER NULL REFERENCES \"user\"(id), parent INTEGER NULL REFERENCES theme(id), background_color VARCHAR NOT NULL, surface_color VARCHAR NOT NULL, text_color VARCHAR NOT NULL, accent_color VARCHAR NOT NULL, heading_font VARCHAR NULL, body_font VARCHAR NULL, header_template VARCHAR NULL, body_template VARCHAR NULL, footer_template VARCHAR NULL, custom_css VARCHAR NULL, price_cents INTEGER NOT NULL DEFAULT 0, status VARCHAR NOT NULL DEFAULT 'published', license VARCHAR NULL, active BOOLEAN NOT NULL DEFAULT 1, created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, CONSTRAINT unique_theme_slug UNIQUE (slug))"
        []

ensureThemeMarketplaceColumns :: forall m. MonadIO m => ReaderT SqlBackend m ()
ensureThemeMarketplaceColumns = do
    columnNames <- fmap unSingle <$> (rawSql
        "SELECT name FROM pragma_table_info('theme') ORDER BY cid"
        [] :: ReaderT SqlBackend m [Single Text])
    unless ("author" `elem` columnNames) $
        rawExecute "ALTER TABLE theme ADD COLUMN author INTEGER NULL REFERENCES \"user\"(id)" []
    unless ("parent" `elem` columnNames) $
        rawExecute "ALTER TABLE theme ADD COLUMN parent INTEGER NULL REFERENCES theme(id)" []
    unless ("price_cents" `elem` columnNames) $
        rawExecute "ALTER TABLE theme ADD COLUMN price_cents INTEGER NOT NULL DEFAULT 0" []
    unless ("header_template" `elem` columnNames) $
        rawExecute "ALTER TABLE theme ADD COLUMN header_template VARCHAR NULL" []
    unless ("body_template" `elem` columnNames) $
        rawExecute "ALTER TABLE theme ADD COLUMN body_template VARCHAR NULL" []
    unless ("footer_template" `elem` columnNames) $
        rawExecute "ALTER TABLE theme ADD COLUMN footer_template VARCHAR NULL" []
    unless ("custom_css" `elem` columnNames) $
        rawExecute "ALTER TABLE theme ADD COLUMN custom_css VARCHAR NULL" []
    unless ("status" `elem` columnNames) $
        rawExecute "ALTER TABLE theme ADD COLUMN status VARCHAR NOT NULL DEFAULT 'published'" []
    unless ("license" `elem` columnNames) $
        rawExecute "ALTER TABLE theme ADD COLUMN license VARCHAR NULL" []
    unless ("created_at" `elem` columnNames) $
        rawExecute "ALTER TABLE theme ADD COLUMN created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP" []
    unless ("updated_at" `elem` columnNames) $
        rawExecute "ALTER TABLE theme ADD COLUMN updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP" []

ensureThemePurchaseTable :: forall m. MonadIO m => ReaderT SqlBackend m ()
ensureThemePurchaseTable =
    rawExecute
        "CREATE TABLE IF NOT EXISTS theme_purchase (id INTEGER PRIMARY KEY, user INTEGER NOT NULL REFERENCES \"user\"(id), theme INTEGER NOT NULL REFERENCES theme(id), price_cents INTEGER NOT NULL DEFAULT 0, purchased_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, CONSTRAINT unique_theme_purchase UNIQUE (user, theme))"
        []

ensureThemeOrderTable :: forall m. MonadIO m => ReaderT SqlBackend m ()
ensureThemeOrderTable =
    rawExecute
        "CREATE TABLE IF NOT EXISTS theme_order (id INTEGER PRIMARY KEY, user INTEGER NOT NULL REFERENCES \"user\"(id), theme INTEGER NOT NULL REFERENCES theme(id), amount_cents INTEGER NOT NULL DEFAULT 0, status VARCHAR NOT NULL DEFAULT 'pending', provider VARCHAR NULL, provider_order_id VARCHAR NULL, created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, paid_at TIMESTAMP NULL)"
        []

ensureThemeReviewTable :: forall m. MonadIO m => ReaderT SqlBackend m ()
ensureThemeReviewTable =
    rawExecute
        "CREATE TABLE IF NOT EXISTS theme_review (id INTEGER PRIMARY KEY, theme INTEGER NOT NULL REFERENCES theme(id), reviewer INTEGER NULL REFERENCES \"user\"(id), status VARCHAR NOT NULL DEFAULT 'pending', note VARCHAR NULL, created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, CONSTRAINT unique_theme_review UNIQUE (theme))"
        []

ensureThemePayoutTable :: forall m. MonadIO m => ReaderT SqlBackend m ()
ensureThemePayoutTable = do
    rawExecute
        "CREATE TABLE IF NOT EXISTS theme_payout (id INTEGER PRIMARY KEY, user INTEGER NOT NULL REFERENCES \"user\"(id), amount_cents INTEGER NOT NULL DEFAULT 0, status VARCHAR NOT NULL DEFAULT 'requested', seller_note VARCHAR NULL, admin_note VARCHAR NULL, requested_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, processed_at TIMESTAMP NULL)"
        []
    columnNames <- fmap unSingle <$> (rawSql
        "SELECT name FROM pragma_table_info('theme_payout') ORDER BY cid"
        [] :: ReaderT SqlBackend m [Single Text])
    unless ("seller_note" `elem` columnNames) $
        rawExecute "ALTER TABLE theme_payout ADD COLUMN seller_note VARCHAR NULL" []
    unless ("admin_note" `elem` columnNames) $
        rawExecute "ALTER TABLE theme_payout ADD COLUMN admin_note VARCHAR NULL" []
    when ("note" `elem` columnNames) $ do
        rawExecute
            "UPDATE theme_payout SET seller_note = note WHERE seller_note IS NULL AND status <> 'rejected' AND note IS NOT NULL"
            []
        rawExecute
            "UPDATE theme_payout SET admin_note = note WHERE admin_note IS NULL AND status = 'rejected' AND note IS NOT NULL"
            []

ensureThemeRatingTable :: forall m. MonadIO m => ReaderT SqlBackend m ()
ensureThemeRatingTable =
    rawExecute
        "CREATE TABLE IF NOT EXISTS theme_rating (id INTEGER PRIMARY KEY, user INTEGER NOT NULL REFERENCES \"user\"(id), theme INTEGER NOT NULL REFERENCES theme(id), rating INTEGER NOT NULL, review VARCHAR NULL, created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, CONSTRAINT unique_theme_rating UNIQUE (user, theme))"
        []

ensureThemeReportTable :: forall m. MonadIO m => ReaderT SqlBackend m ()
ensureThemeReportTable =
    rawExecute
        "CREATE TABLE IF NOT EXISTS theme_report (id INTEGER PRIMARY KEY, user INTEGER NOT NULL REFERENCES \"user\"(id), theme INTEGER NOT NULL REFERENCES theme(id), reason VARCHAR NOT NULL, details VARCHAR NULL, status VARCHAR NOT NULL DEFAULT 'open', created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP)"
        []

ensureCustomDomainTable :: forall m. MonadIO m => ReaderT SqlBackend m ()
ensureCustomDomainTable =
    rawExecute
        "CREATE TABLE IF NOT EXISTS custom_domain (id INTEGER PRIMARY KEY, user INTEGER NOT NULL REFERENCES \"user\"(id), domain VARCHAR NOT NULL, verification_token VARCHAR NOT NULL, status VARCHAR NOT NULL DEFAULT 'pending', created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, CONSTRAINT unique_custom_domain UNIQUE (domain))"
        []

ensureMembershipTable :: forall m. MonadIO m => ReaderT SqlBackend m ()
ensureMembershipTable =
    do
        rawExecute
            "CREATE TABLE IF NOT EXISTS membership (id INTEGER PRIMARY KEY, creator INTEGER NOT NULL REFERENCES \"user\"(id), member INTEGER NOT NULL REFERENCES \"user\"(id), price_cents INTEGER NOT NULL DEFAULT 0, status VARCHAR NOT NULL DEFAULT 'pending', auto_renew BOOLEAN NOT NULL DEFAULT 0, started_at TIMESTAMP NULL, expires_at TIMESTAMP NULL, created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, CONSTRAINT unique_membership UNIQUE (creator, member))"
            []
        columnNames <- fmap unSingle <$> (rawSql
            "SELECT name FROM pragma_table_info('membership') ORDER BY cid"
            [] :: ReaderT SqlBackend m [Single Text])
        unless ("auto_renew" `elem` columnNames) $
            rawExecute "ALTER TABLE membership ADD COLUMN auto_renew BOOLEAN NOT NULL DEFAULT 0" []

ensureMembershipOrderTable :: forall m. MonadIO m => ReaderT SqlBackend m ()
ensureMembershipOrderTable = do
    rawExecute
        "CREATE TABLE IF NOT EXISTS membership_order (id INTEGER PRIMARY KEY, membership INTEGER NOT NULL REFERENCES membership(id), creator INTEGER NOT NULL REFERENCES \"user\"(id), member INTEGER NOT NULL REFERENCES \"user\"(id), amount_cents INTEGER NOT NULL DEFAULT 0, status VARCHAR NOT NULL DEFAULT 'pending', provider VARCHAR NULL, provider_order_id VARCHAR NULL, admin_note TEXT NULL, created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, paid_at TIMESTAMP NULL)"
        []
    columnNames <- fmap unSingle <$> (rawSql
        "SELECT name FROM pragma_table_info('membership_order') ORDER BY cid"
        [] :: ReaderT SqlBackend m [Single Text])
    unless ("admin_note" `elem` columnNames) $
        rawExecute "ALTER TABLE membership_order ADD COLUMN admin_note TEXT NULL" []

ensureUserThemeColumns :: forall m. MonadIO m => ReaderT SqlBackend m ()
ensureUserThemeColumns = do
    columnNames <- fmap unSingle <$> (rawSql
        "SELECT name FROM pragma_table_info('user') ORDER BY cid"
        [] :: ReaderT SqlBackend m [Single Text])
    unless ("theme" `elem` columnNames) $
        rawExecute "ALTER TABLE \"user\" ADD COLUMN theme INTEGER NULL REFERENCES theme(id)" []
    unless ("theme_overrides" `elem` columnNames) $
        rawExecute "ALTER TABLE \"user\" ADD COLUMN theme_overrides VARCHAR NULL" []

ensureUserPlanColumns :: forall m. MonadIO m => ReaderT SqlBackend m ()
ensureUserPlanColumns = do
    columnNames <- fmap unSingle <$> (rawSql
        "SELECT name FROM pragma_table_info('user') ORDER BY cid"
        [] :: ReaderT SqlBackend m [Single Text])
    unless ("plan" `elem` columnNames) $
        rawExecute "ALTER TABLE \"user\" ADD COLUMN plan VARCHAR NOT NULL DEFAULT 'free'" []
    unless ("plan_expires_at" `elem` columnNames) $
        rawExecute "ALTER TABLE \"user\" ADD COLUMN plan_expires_at TIMESTAMP NULL" []
    unless ("membership_price_cents" `elem` columnNames) $
        rawExecute "ALTER TABLE \"user\" ADD COLUMN membership_price_cents INTEGER NOT NULL DEFAULT 0" []

seedDefaultThemes :: ConnectionPool -> IO ()
seedDefaultThemes pool = do
    now <- getCurrentTime
    let themes =
            [ Theme
                { themeName = "Editorial Ink"
                , themeSlug = "editorial-ink"
                , themeDescription = Just $ Textarea "Warm paper, dark ink, and classic editorial typography."
                , themeAuthor = Nothing
                , themeParent = Nothing
                , themeBackgroundColor = "#f8f0dc"
                , themeSurfaceColor = "#fffef7"
                , themeTextColor = "#111111"
                , themeAccentColor = "#ffe11a"
                , themeHeadingFont = Just "Cormorant Garamond"
                , themeBodyFont = Just "Space Grotesk"
                , themeHeaderTemplate = Nothing
                , themeBodyTemplate = Nothing
                , themeFooterTemplate = Nothing
                , themeCustomCss = Nothing
                , themePriceCents = 0
                , themeStatus = "published"
                , themeLicense = Just "free-remix"
                , themeActive = True
                , themeCreatedAt = now
                , themeUpdatedAt = now
                }
            , Theme
                { themeName = "Ocean Desk"
                , themeSlug = "ocean-desk"
                , themeDescription = Just $ Textarea "Cool blue writing surface with crisp high-contrast panels."
                , themeAuthor = Nothing
                , themeParent = Nothing
                , themeBackgroundColor = "#dff6fb"
                , themeSurfaceColor = "#ffffff"
                , themeTextColor = "#102027"
                , themeAccentColor = "#57d5e5"
                , themeHeadingFont = Just "Space Grotesk"
                , themeBodyFont = Just "Space Grotesk"
                , themeHeaderTemplate = Nothing
                , themeBodyTemplate = Nothing
                , themeFooterTemplate = Nothing
                , themeCustomCss = Nothing
                , themePriceCents = 0
                , themeStatus = "published"
                , themeLicense = Just "free-remix"
                , themeActive = True
                , themeCreatedAt = now
                , themeUpdatedAt = now
                }
            , Theme
                { themeName = "Garden Notes"
                , themeSlug = "garden-notes"
                , themeDescription = Just $ Textarea "Soft green accents for personal journals and essays."
                , themeAuthor = Nothing
                , themeParent = Nothing
                , themeBackgroundColor = "#eef8de"
                , themeSurfaceColor = "#fffdf6"
                , themeTextColor = "#172016"
                , themeAccentColor = "#7ef0b2"
                , themeHeadingFont = Just "Cormorant Garamond"
                , themeBodyFont = Just "Space Grotesk"
                , themeHeaderTemplate = Nothing
                , themeBodyTemplate = Nothing
                , themeFooterTemplate = Nothing
                , themeCustomCss = Nothing
                , themePriceCents = 0
                , themeStatus = "published"
                , themeLicense = Just "free-remix"
                , themeActive = True
                , themeCreatedAt = now
                , themeUpdatedAt = now
                }
            ]
    forM_ themes $ \theme -> do
        existingTheme <- runSqlPool (getBy $ UniqueThemeSlug $ themeSlug theme) pool
        when (isNothing existingTheme) $
            void $ runSqlPool (insert theme) pool

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
                , userPlan = "designer-pro"
                , userPlanExpiresAt = Nothing
                , userMembershipPriceCents = 0
                , userTheme = Nothing
                , userThemeOverrides = Nothing
                }
            void $ runSqlPool (insert seededAdmin) pool
        Just (Entity adminId adminUser) ->
            unless (userIsAdmin adminUser && userPlan adminUser == "designer-pro") $
                runSqlPool (update adminId [UserIsAdmin =. True, UserPlan =. "designer-pro"]) pool

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
