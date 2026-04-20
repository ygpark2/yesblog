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
import Database.Persist.Sql  (SqlPersistM, runSqlPersistMPool, rawExecute, rawSql, unSingle)
import Foundation            as X
import Model                 as X
import Test.Hspec            as X
import Yesod.Default.Config2 (useEnv, loadYamlSettings)
import Yesod.Auth            as X
import Yesod.Test            as X
import Yesod.Core.Unsafe     (fakeHandlerGetLogger)

-- Wiping the database
import Database.Persist.Sqlite              (sqlDatabase, mkSqliteConnectionInfo, fkEnabled, createSqlitePoolFromInfo)
import Control.Monad.Logger                 (runLoggingT)
import Lens.Micro                           (set)
import Settings (appDatabaseConf)
import Yesod.Core (messageLoggerSource)
import Yesod.Form.Fields (Textarea (..))
import Yesod.Markdown (Markdown (..))

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    pool <- fmap appConnPool getTestYesod
    liftIO $ runSqlPersistMPool query pool

runHandler :: Handler a -> YesodExample App a
runHandler handler = do
    app <- getTestYesod
    fakeHandlerGetLogger appLogger app handler

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
    -- In order to wipe the database, we need to use a connection which has
    -- foreign key checks disabled.  Foreign key checks are enabled or disabled
    -- per connection, so this won't effect queries outside this function.
    --
    -- Aside: foreign key checks are enabled by persistent-sqlite, as of
    -- version 2.6.2, unless they are explicitly disabled in the
    -- SqliteConnectionInfo.

    let logFunc = messageLoggerSource app (appLogger app)

    let dbName = sqlDatabase $ appDatabaseConf $ appSettings app
        connInfo = set fkEnabled False $ mkSqliteConnectionInfo dbName

    pool <- runLoggingT (createSqlitePoolFromInfo connInfo 1) logFunc

    flip runSqlPersistMPool pool $ do
        tables <- getTables
        let queries = map (\t -> "DELETE FROM \"" ++ t ++ "\"") tables
        forM_ queries (\q -> rawExecute q [])

getTables :: DB [Text]
getTables = do
    tables <- rawSql "SELECT name FROM sqlite_master WHERE type = 'table';" []
    return (fmap unSingle tables)

-- | Authenticate as a user. This relies on the `auth-dummy-login: true` flag
-- being set in test-settings.yaml, which enables dummy authentication in
-- Foundation.hs
authenticateAs :: Entity User -> YesodExample App ()
authenticateAs (Entity _ u) = do
    request $ do
        setMethod "POST"
        addPostParam "ident" $ userIdent u
        setUrl $ AuthR $ PluginR "dummy" []

-- | Create a user.  The dummy email entry helps to confirm that foreign-key
-- checking is switched off in wipeDB for those database backends which need it.
createUser :: Text -> YesodExample App (Entity User)
createUser ident = runDB $ do
    insertEntity User
        { userIdent = ident
        , userPassword = Nothing
        , userDisplayName = Nothing
        , userBio = Nothing
        , userIsAdmin = False
        }

createAdmin :: Text -> YesodExample App (Entity User)
createAdmin ident = runDB $ do
    insertEntity User
        { userIdent = ident
        , userPassword = Nothing
        , userDisplayName = Nothing
        , userBio = Nothing
        , userIsAdmin = True
        }

createUserWithProfile :: Text -> Maybe Text -> Maybe Text -> YesodExample App (Entity User)
createUserWithProfile ident displayName bio = runDB $ do
    insertEntity User
        { userIdent = ident
        , userPassword = Nothing
        , userDisplayName = displayName
        , userBio = Textarea <$> bio
        , userIsAdmin = False
        }

createArticle :: UserId -> Text -> Text -> YesodExample App (Entity Article)
createArticle authorId title slug = runDB $ do
    now <- liftIO getCurrentTime
    insertEntity Article
        { articleAuthor = authorId
        , articleTitle = title
        , articleContent = Markdown ("content for " <> title)
        , articleSlug = slug
        , articleDraft = False
        , articleCreatedAt = now
        }

createImage :: String -> Maybe Text -> YesodExample App (Entity Image)
createImage filename description = runDB $ do
    now <- liftIO getCurrentTime
    insertEntity Image
        { imageFilename = filename
        , imageDescription = Textarea <$> description
        , imageDate = now
        }
