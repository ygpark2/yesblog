{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module TestImport
    ( module TestImport
    , module X
    ) where

import Application           (makeFoundation, makeLogWare)
import ClassyPrelude         as X hiding (delete, deleteBy, Handler)
import Database.Persist      as X hiding (get)
import qualified Database.Persist as Persist
import Database.Persist.Sql  as X (SqlBackend, SqlPersistM, runSqlPersistMPool, rawExecute, rawSql, unSingle, fromSqlKey)
import Foundation            as X
import Model                 as X
import Test.Hspec            as X
import Yesod.Default.Config2 (useEnv, loadYamlSettings)
import Yesod.Auth            as X
import Yesod.Auth.HashDB    (setPassword)
import Yesod.Test            as X
import Yesod.Core.Unsafe     (fakeHandlerGetLogger)

-- Wiping the database
import Database.Persist.Sqlite              (sqlDatabase, mkSqliteConnectionInfo, fkEnabled, createSqlitePoolFromInfo)
import Control.Monad.Logger                 (runLoggingT)
import Lens.Micro                           (set)
import Settings (DatabaseConfig (..), appDatabaseConf)
import Yesod.Core (messageLoggerSource)
import Yesod.Form.Fields as X (Textarea (..))
import Yesod.Markdown (Markdown (..))

defaultTestPassword :: Text
defaultTestPassword = "password123"

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    pool <- fmap appConnPool getTestYesod
    liftIO $ runSqlPersistMPool query pool

runHandler :: Handler a -> YesodExample App a
runHandler handler = do
    app <- getTestYesod
    fakeHandlerGetLogger appLogger app handler

getRecord :: Persist.PersistRecordBackend record SqlBackend => Key record -> YesodExample App (Maybe record)
getRecord = runDB . Persist.get

withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
    settings <- loadYamlSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        useEnv
    foundation <- makeFoundation settings
    wipeDB foundation
    logWare <- liftIO $ makeLogWare foundation
    return (foundation, logWare)

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app = do
    case appDatabaseConf $ appSettings app of
        SqliteDatabase sqliteConf -> do
            -- In order to wipe the database, we need to use a connection which has
            -- foreign key checks disabled. Foreign key checks are enabled or disabled
            -- per connection, so this won't affect queries outside this function.
            let logFunc = messageLoggerSource app (appLogger app)
                dbName = sqlDatabase sqliteConf
                connInfo = set fkEnabled False $ mkSqliteConnectionInfo dbName

            pool <- runLoggingT (createSqlitePoolFromInfo connInfo 1) logFunc

            flip runSqlPersistMPool pool $ do
                tables <- getSqliteTables
                let queries = map (\t -> "DELETE FROM \"" ++ t ++ "\"") tables
                forM_ queries (\q -> rawExecute q [])
        PostgresqlDatabase _ ->
            flip runSqlPersistMPool (appConnPool app) $ do
                tables <- getPostgresqlTables
                let queries = map (\t -> "TRUNCATE TABLE \"" ++ t ++ "\" RESTART IDENTITY CASCADE") tables
                forM_ queries (\q -> rawExecute q [])

getSqliteTables :: DB [Text]
getSqliteTables = do
    tables <- rawSql "SELECT name FROM sqlite_master WHERE type = 'table';" []
    pure $ fmap unSingle tables

getPostgresqlTables :: DB [Text]
getPostgresqlTables = do
    tables <- rawSql
        "SELECT tablename FROM pg_tables WHERE schemaname = 'public'"
        []
    pure $ fmap unSingle tables

authenticateAs :: Entity User -> YesodExample App ()
authenticateAs (Entity _ u) = do
    request $ do
        setMethod "POST"
        addPostParam "ident" $ userIdent u
        addPostParam "password" defaultTestPassword
        setUrl ApiAuthLoginR
    statusIs 200

-- | Create a user.  The dummy email entry helps to confirm that foreign-key
-- checking is switched off in wipeDB for those database backends which need it.
createUser :: Text -> YesodExample App (Entity User)
createUser ident = createUserWithPassword ident defaultTestPassword

createAdmin :: Text -> YesodExample App (Entity User)
createAdmin ident = createAdminWithPassword ident defaultTestPassword

createUserWithPassword :: Text -> Text -> YesodExample App (Entity User)
createUserWithPassword ident password = runDB $ do
    user <- liftIO $ setPassword password User
        { userIdent = ident
        , userPassword = Nothing
        , userDisplayName = Nothing
        , userBio = Nothing
        , userIsAdmin = False
        }
    insertEntity user

createAdminWithPassword :: Text -> Text -> YesodExample App (Entity User)
createAdminWithPassword ident password = runDB $ do
    user <- liftIO $ setPassword password User
        { userIdent = ident
        , userPassword = Nothing
        , userDisplayName = Nothing
        , userBio = Nothing
        , userIsAdmin = True
        }
    insertEntity user

createUserWithProfile :: Text -> Maybe Text -> Maybe Text -> YesodExample App (Entity User)
createUserWithProfile ident displayName bio = runDB $ do
    user <- liftIO $ setPassword defaultTestPassword User
        { userIdent = ident
        , userPassword = Nothing
        , userDisplayName = displayName
        , userBio = Textarea <$> bio
        , userIsAdmin = False
        }
    insertEntity user

createArticle :: UserId -> Text -> Text -> YesodExample App (Entity Article)
createArticle authorId title slug =
    createArticleWithContent authorId title ("content for " <> title) slug False []

createArticleWithContent :: UserId -> Text -> Text -> Text -> Bool -> [Text] -> YesodExample App (Entity Article)
createArticleWithContent authorId title content slug isDraft tags = runDB $ do
    now <- liftIO getCurrentTime
    articleEntity@(Entity articleId _) <- insertEntity Article
        { articleAuthor = authorId
        , articleTitle = title
        , articleContent = Markdown content
        , articleSlug = slug
        , articleDraft = isDraft
        , articleCreatedAt = now
        , articleUpdatedAt = now
        }
    forM_ tags $ \tagName ->
        insert_ $ Tag tagName articleId
    pure articleEntity

createComment :: ArticleId -> Text -> Text -> YesodExample App (Entity Comment)
createComment articleId authorName content = runDB $ do
    now <- liftIO getCurrentTime
    insertEntity $ Comment authorName content articleId now

createImage :: String -> Maybe Text -> YesodExample App (Entity Image)
createImage filename description = runDB $ do
    now <- liftIO getCurrentTime
    insertEntity Image
        { imageFilename = filename
        , imageDescription = Textarea <$> description
        , imageDate = now
        }
