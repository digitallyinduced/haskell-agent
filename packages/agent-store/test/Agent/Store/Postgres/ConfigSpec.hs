{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Agent.Store.Postgres.ConfigSpec (spec) where

import qualified Data.Text as Text
import Test.Hspec

import Agent.Store.Postgres.Config

spec :: Spec
spec = do
    describe "defaultManagedPostgresConfig" do
        it "keeps all cluster state below the agent state directory" do
            let config = defaultManagedPostgresConfig "/tmp/agent" "/pg/bin"
            config.postgresPaths.postgresDataDirectory
                `shouldBe` "/tmp/agent/postgres/data"
            config.postgresPaths.postgresSocketDirectory
                `shouldBe` "/tmp/agent/postgres/run"
            postgresExecutable config "initdb"
                `shouldBe` "/pg/bin/initdb"

        it "configures a private Unix socket without TCP listeners" do
            let rendered = postgresqlConf $
                    defaultManagedPostgresConfig "/tmp/agent" ""
            rendered `shouldSatisfy` Text.isInfixOf "listen_addresses = ''"
            rendered `shouldSatisfy`
                Text.isInfixOf "unix_socket_permissions = 0700"
