{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Agent.Store.Postgres.TenantSpec (spec) where

import Agent.Store.Postgres
    ( ManagedPostgresConfig(..)
    , defaultManagedPostgresConfig
    , trustedPool
    )
import Agent.Store.Postgres.Connection
    ( closeStorePool
    , defaultPoolConfig
    , openRoleStorePool
    , withSession
    )
import Agent.Store.Postgres.Managed (stopManagedPostgres)
import Agent.Store.Postgres.Tenant
    ( acquireTenantStore
    , closeTenantStoreManager
    , openTenantStoreManager
    , tenantDatabase
    )
import Control.Exception.Safe (finally)
import Data.Text (Text)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Session as Session
import Hasql.Statement (Statement)
import qualified Hasql.Statement as Statement
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec =
    describe "tenant PostgreSQL isolation" do
        it "uses distinct databases and denies a tenant role cross-database access" $
            withSystemTempDirectory "ha" \stateDirectory -> do
                let config =
                        defaultManagedPostgresConfig stateDirectory ""
                    cleanup = do
                        _ <- stopManagedPostgres config
                        pure ()
                (openTenantStoreManager config 2 >>= \case
                    Left err ->
                        expectationFailure
                            ("could not open tenant store manager: " <> show err)
                    Right manager ->
                        finally
                            (do
                                databaseA <-
                                    either (fail . show) pure $
                                        tenantDatabase
                                            "ha_t_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                                            "ha_rt_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                                databaseB <-
                                    either (fail . show) pure $
                                        tenantDatabase
                                            "ha_t_bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
                                            "ha_rt_bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
                                storeA <-
                                    acquireTenantStore manager databaseA
                                        >>= either (fail . show) pure
                                storeB <-
                                    acquireTenantStore manager databaseB
                                        >>= either (fail . show) pure
                                withSession
                                    (trustedPool storeA)
                                    (Session.statement () identityStatement)
                                    `shouldReturn`
                                        Right
                                            ( "ha_t_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                                            , "ha_rt_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                                            )
                                withSession
                                    (trustedPool storeB)
                                    (Session.statement () identityStatement)
                                    `shouldReturn`
                                        Right
                                            ( "ha_t_bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
                                            , "ha_rt_bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
                                            )
                                crossed <-
                                    openRoleStorePool
                                        config
                                            { postgresDatabase =
                                                "ha_t_bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
                                            }
                                        "ha_rt_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                                        defaultPoolConfig
                                case crossed of
                                    Left _ -> pure ()
                                    Right pool -> do
                                        closeStorePool pool
                                        expectationFailure
                                            "tenant A connected to tenant B's database"
                            )
                            (closeTenantStoreManager manager)
                    ) `finally` cleanup

identityStatement :: Statement () (Text, Text)
identityStatement = Statement.preparable
    "SELECT current_database()::text, current_user::text"
    Encoders.noParams
    (Decoders.singleRow $
        (,)
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text))
