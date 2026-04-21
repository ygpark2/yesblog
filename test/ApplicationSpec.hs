{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module ApplicationSpec (spec) where

import Application (makeFoundation)
import Database.Persist.Sql (runSqlPersistMPool)
import TestImport
import Settings (appSeedAdminIdent)
import Yesod.Default.Config2 (loadYamlSettings, useEnv)

spec :: Spec
spec = describe "Admin seed" $
    it "creates the default admin account during foundation setup" $ do
        settings <- loadYamlSettings
            ["config/test-settings.yml", "config/settings.yml"]
            []
            useEnv
        foundation <- makeFoundation settings
        wipeDB foundation

        seededFoundation <- makeFoundation settings
        mAdmin <- runSqlPersistMPool
            (getBy $ UniqueUser $ appSeedAdminIdent $ appSettings seededFoundation)
            (appConnPool seededFoundation)

        case mAdmin of
            Nothing -> expectationFailure "expected seeded admin user to exist"
            Just (Entity _ adminUser) -> do
                userIdent adminUser `shouldBe` appSeedAdminIdent (appSettings seededFoundation)
                userIsAdmin adminUser `shouldBe` True
                userPassword adminUser `shouldSatisfy` isJust

        wipeDB seededFoundation
