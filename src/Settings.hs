{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

import ClassyPrelude.Yesod
import Control.Monad.Fail          (fail)
import qualified Control.Exception as Exception
import Data.Aeson                  (Object, Result (..), Value (Object),
                                    fromJSON, withObject, (.!=), (.:?))
import Data.Aeson.Types            (Parser)
import qualified Data.Char as Char
import Data.FileEmbed              (embedFile)
import qualified Data.Text as T
import Data.Yaml                   (decodeEither')
import Database.Persist.Postgresql (ConnectionString)
import Database.Persist.Sqlite     (SqliteConf, sqlPoolSize)
import Language.Haskell.TH.Syntax  (Exp, Name, Q)
import Network.Wai.Handler.Warp    (HostPreference)
import Yesod.Default.Config2       (applyEnvValue, configSettingsYml)

-- | Runtime settings to configure this application. These settings can be
-- loaded from various sources: defaults, environment variables, config files,
-- theoretically even a database.
data AppSettings = AppSettings
    { appStaticDir              :: String
    -- ^ Directory from which to serve static files.
    , appDatabaseConf           :: DatabaseConfig
    -- ^ Configuration settings for accessing the database.
    , appRoot                   :: Maybe Text
    -- ^ Base for all generated URLs. If @Nothing@, determined
    -- from the request headers.
    , appHost                   :: HostPreference
    -- ^ Host/interface the server should bind to.
    , appPort                   :: Int
    -- ^ Port to listen on
    , appIpFromHeader           :: Bool
    -- ^ Get the IP address from the header when logging. Useful when sitting
    -- behind a reverse proxy.

    , appDetailedRequestLogging :: Bool
    -- ^ Use detailed request logging system
    , appShouldLogAll           :: Bool
    -- ^ Should all log messages be displayed?
    , appReloadTemplates        :: Bool
    -- ^ Use the reload version of templates
    , appMutableStatic          :: Bool
    -- ^ Assume that files in the static dir may change after compilation
    , appSkipCombining          :: Bool
    -- ^ Perform no stylesheet/script combining

    -- Example app-specific configuration values.
    , appCopyright              :: Text
    -- ^ Copyright text to appear in the footer of the page
    , appAnalytics              :: Maybe Text
    -- ^ Google Analytics code

    , appAuthDummyLogin         :: Bool
    -- ^ Indicate if auth dummy login should be enabled.
    , appSeedAdminIdent         :: Text
    -- ^ Default admin username created during startup seeding.
    , appSeedAdminPassword      :: Text
    -- ^ Default admin password created during startup seeding.
    , appSeedAdminDisplayName   :: Text
    -- ^ Default admin display name created during startup seeding.
    }

data DatabaseConfig
    = SqliteDatabase SqliteConf
    | PostgresqlDatabase PostgresqlConf

data PostgresqlConf = PostgresqlConf
    { pgConnStrText :: Text
    , pgPoolSize    :: Int
    }

postgresqlConnectionString :: PostgresqlConf -> ConnectionString
postgresqlConnectionString = encodeUtf8 . pgConnStrText

databasePoolSize :: DatabaseConfig -> Int
databasePoolSize (SqliteDatabase conf) = sqlPoolSize conf
databasePoolSize (PostgresqlDatabase conf) = pgPoolSize conf

instance FromJSON DatabaseConfig where
    parseJSON = withObject "DatabaseConfig" $ \o -> do
        mKind <- fmap (fmap normalizeDatabaseKind) $ o .:? "kind"
        case mKind of
            Just "sqlite" -> SqliteDatabase <$> parseJSON (Object o)
            Just "postgres" -> PostgresqlDatabase <$> parsePostgresqlConf o
            Just "postgresql" -> PostgresqlDatabase <$> parsePostgresqlConf o
            Just other -> fail $ "Unsupported database kind: " <> unpack other
            Nothing -> do
                mConnStr <- o .:? "connstr"
                mHost <- o .:? "host"
                mUser <- o .:? "user"
                mPort <- o .:? "port" :: Parser (Maybe Int)
                case (mConnStr :: Maybe Text, mHost :: Maybe Text, mUser :: Maybe Text, mPort) of
                    (Just _, _, _, _) -> PostgresqlDatabase <$> parsePostgresqlConf o
                    (_, Just _, _, _) -> PostgresqlDatabase <$> parsePostgresqlConf o
                    (_, _, Just _, _) -> PostgresqlDatabase <$> parsePostgresqlConf o
                    (_, _, _, Just _) -> PostgresqlDatabase <$> parsePostgresqlConf o
                    _ -> SqliteDatabase <$> parseJSON (Object o)

normalizeDatabaseKind :: Text -> Text
normalizeDatabaseKind = toLower

parsePostgresqlConf :: Object -> Parser PostgresqlConf
parsePostgresqlConf o = do
    poolSize <- o .:? "poolsize" .!= 10
    mConnStr <- o .:? "connstr"
    connStr <- case mConnStr of
        Just rawConnStr -> pure $ T.strip rawConnStr
        Nothing -> buildPostgresqlConnStr o
    pure PostgresqlConf
        { pgConnStrText = connStr
        , pgPoolSize = poolSize
        }

buildPostgresqlConnStr :: Object -> Parser Text
buildPostgresqlConnStr o = do
    databaseName <- o .: "database"
    host <- o .:? "host" .!= "127.0.0.1"
    port <- o .:? "port" .!= (5432 :: Int)
    user <- o .:? "user" .!= "postgres"
    password <- o .:? "password" .!= ""
    sslmode <- o .:? "sslmode"
    let baseParts =
            [ ("host", host)
            , ("port", tshow port)
            , ("user", user)
            , ("dbname", databaseName)
            ]
        passwordPart =
            if password == ""
                then []
                else [("password", password)]
        sslmodePart =
            maybe [] (\mode -> [("sslmode", mode)]) sslmode
    pure $ unwords $ map renderConnPart $ baseParts <> passwordPart <> sslmodePart

renderConnPart :: (Text, Text) -> Text
renderConnPart (key, value) = key <> "=" <> quoteConnValue value

quoteConnValue :: Text -> Text
quoteConnValue value
    | needsQuoting value = "'" <> T.replace "'" "\\'" value <> "'"
    | otherwise = value
  where
    needsQuoting = any (\char -> Char.isSpace char || char == '\'' || char == '\\')

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        let defaultDev =
#ifdef DEVELOPMENT
                True
#else
                False
#endif
        appStaticDir              <- o .: "static-dir"
        appDatabaseConf           <- o .: "database"
        appRoot                   <- o .:? "approot"
        appHost                   <- fromString <$> o .: "host"
        appPort                   <- o .: "port"
        appIpFromHeader           <- o .: "ip-from-header"

        dev                       <- o .:? "development"      .!= defaultDev

        appDetailedRequestLogging <- o .:? "detailed-logging" .!= dev
        appShouldLogAll           <- o .:? "should-log-all"   .!= dev
        appReloadTemplates        <- o .:? "reload-templates" .!= dev
        appMutableStatic          <- o .:? "mutable-static"   .!= dev
        appSkipCombining          <- o .:? "skip-combining"   .!= dev

        appCopyright              <- o .:  "copyright"
        appAnalytics              <- o .:? "analytics"

        appAuthDummyLogin         <- o .:? "auth-dummy-login"      .!= dev
        appSeedAdminIdent         <- o .:? "seed-admin-ident"      .!= "admin"
        appSeedAdminPassword      <- o .:? "seed-admin-password"   .!= "admin123456"
        appSeedAdminDisplayName   <- o .:? "seed-admin-display-name" .!= "YesBlog Admin"

        return AppSettings {..}

-- | How static files should be combined.
combineSettings :: CombineSettings
combineSettings = def

-- | Raw bytes at compile time of @config/settings.yml@
configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

-- | @config/settings.yml@, parsed to a @Value@.
configSettingsYmlValue :: Value
configSettingsYmlValue = either Exception.throw id
                       $ decodeEither' configSettingsYmlBS

-- | A version of @AppSettings@ parsed at compile time from @config/settings.yml@.
compileTimeAppSettings :: AppSettings
compileTimeAppSettings =
    case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
        Error e -> error e
        Success settings -> settings

-- The following two functions can be used to combine multiple CSS or JS files
-- at compile time to decrease the number of http requests.
-- Sample usage (inside a Widget):
--
-- > $(combineStylesheets 'StaticR [style1_css, style2_css])

combineStylesheets :: Name -> [Route Static] -> Q Exp
combineStylesheets = combineStylesheets'
    (appSkipCombining compileTimeAppSettings)
    combineSettings

combineScripts :: Name -> [Route Static] -> Q Exp
combineScripts = combineScripts'
    (appSkipCombining compileTimeAppSettings)
    combineSettings
