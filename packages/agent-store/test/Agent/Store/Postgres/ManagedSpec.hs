{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Agent.Store.Postgres.ManagedSpec (spec) where

import Control.Exception.Safe (finally)
import Data.ByteString (ByteString)
import Data.Either (isLeft, isRight)
import Data.Text (Text)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Session as Session
import Hasql.Statement (Statement)
import qualified Hasql.Statement as Statement
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Agent.Store.Postgres
import Agent.Store.Postgres.Connection
    ( closeStorePool
    , defaultPoolConfig
    , openStorePool
    , withSession
    )
import Agent.Store.Postgres.Managed
    ( ensureManagedPostgres
    , stopManagedPostgres
    )
import Agent.Store.Postgres.Migrations
    ( Migration(..)
    , runMigrations
    )
import Agent.Store.Postgres.Scope (customSchemaStatements)

spec :: Spec
spec =
    describe "managed PostgreSQL" do
        it "starts on a private socket and applies the harness migrations" $
            -- Keep the prefix short because Darwin's Unix socket path limit
            -- also includes PostgreSQL's generated socket filename.
            withSystemTempDirectory "ha" \stateDirectory -> do
                let config =
                        defaultManagedPostgresConfig stateDirectory ""
                    cleanup = do
                        _ <- stopManagedPostgres config
                        pure ()
                ((withStore config \store -> do
                    withSession
                        (trustedPool store)
                        (Session.statement () serverStatement)
                        `shouldReturn`
                            Right
                                ( "haskell_agent"
                                , "ha_runtime"
                                , True
                                , True
                                , True
                                )
                    forbiddenResult <- withSession
                        (trustedPool store)
                        (Session.script
                            "CREATE SCHEMA runtime_must_not_create")
                    forbiddenResult `shouldSatisfy` isLeft
                    ) >>= \case
                        Left err ->
                            expectationFailure
                                ("could not open managed store: " <> show err)
                        Right () -> pure ())
                    `finally` cleanup

        it "upgrades an empty normalized session schema in place" $
            withSystemTempDirectory "ha" \stateDirectory -> do
                let
                    config = defaultManagedPostgresConfig stateDirectory ""
                    cleanup = do
                        _ <- stopManagedPostgres config
                        pure ()
                (do
                    ensureManagedPostgres config
                        >>= (`shouldSatisfy` isRight)
                    openStorePool config defaultPoolConfig >>= \case
                        Left err ->
                            expectationFailure
                                ("could not open bootstrap pool: " <> show err)
                        Right ownerPool ->
                            finally
                                (runMigrations ownerPool legacyMigrations
                                    `shouldReturn` Right ())
                                (closeStorePool ownerPool)
                    (withStore config \store ->
                        withSession
                            (provisioningPool store)
                            (Session.statement () upgradedSchemaStatement)
                            `shouldReturn` Right (True, True, True)
                        ) >>= \case
                            Left err ->
                                expectationFailure
                                    ("could not upgrade managed store: " <> show err)
                            Right () -> pure ()
                    ) `finally` cleanup

legacyMigrations :: [Migration]
legacyMigrations =
    [ Migration
        { migrationVersion = 1
        , migrationName = "initial harness storage"
        , migrationStatements =
            customSchemaStatements
            <> legacySessionSchemaStatements
        }
    , Migration
        { migrationVersion = 2
        , migrationName = "restricted harness runtime role"
        , migrationStatements =
            [ "CREATE ROLE ha_runtime LOGIN\
              \ NOSUPERUSER NOCREATEDB NOCREATEROLE\
              \ NOINHERIT NOREPLICATION NOBYPASSRLS"
            , "DO $ha$\
              \ BEGIN\
              \   EXECUTE format(\
              \     'GRANT CONNECT ON DATABASE %I TO ha_runtime',\
              \     current_database()\
              \   );\
              \ END\
              \ $ha$"
            , "GRANT USAGE ON SCHEMA harness TO ha_runtime"
            , "GRANT SELECT ON harness.schema_migrations TO ha_runtime"
            ]
        }
    ]

legacySessionSchemaStatements :: [ByteString]
legacySessionSchemaStatements =
    [ "CREATE OR REPLACE FUNCTION harness.raise_normalized_value_error(\
      \ error_message text\
      \ ) RETURNS boolean\
      \ LANGUAGE plpgsql\
      \ AS $$ BEGIN RAISE EXCEPTION '%', error_message; END $$"
    , "CREATE TABLE harness.structured_values (\
      \ value_id uuid PRIMARY KEY DEFAULT pg_catalog.uuidv7()\
      \ )"
    , "CREATE TABLE harness.sessions (\
      \ session_id uuid PRIMARY KEY DEFAULT pg_catalog.uuidv7(),\
      \ session_key text NOT NULL UNIQUE,\
      \ created_at timestamptz NOT NULL,\
      \ updated_at timestamptz NOT NULL,\
      \ metadata_value_id uuid NOT NULL\
      \   REFERENCES harness.structured_values(value_id),\
      \ next_event_sequence bigint NOT NULL DEFAULT 1,\
      \ next_turn_index bigint NOT NULL DEFAULT 0,\
      \ deleted_at timestamptz\
      \ )"
    ]

serverStatement :: Statement () (Text, Text, Bool, Bool, Bool)
serverStatement = Statement.preparable
    "SELECT current_database()::text, current_user::text,\
    \ inet_server_addr() IS NULL,\
    \ to_regclass('harness.sessions') IS NOT NULL,\
    \ current_setting('server_version_num')::integer >= 180000\
    \   AND substring(pg_catalog.uuidv7()::text, 15, 1) = '7'"
    Encoders.noParams
    (Decoders.singleRow $
        (,,,,)
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.bool)
            <*> Decoders.column (Decoders.nonNullable Decoders.bool)
            <*> Decoders.column (Decoders.nonNullable Decoders.bool))

upgradedSchemaStatement :: Statement () (Bool, Bool, Bool)
upgradedSchemaStatement = Statement.preparable
    "SELECT\
    \ EXISTS (\
    \   SELECT 1 FROM information_schema.columns\
    \   WHERE table_schema = 'harness'\
    \     AND table_name = 'sessions'\
    \     AND column_name = 'session_schema_version'\
    \ ),\
    \ NOT EXISTS (\
    \   SELECT 1 FROM information_schema.columns\
    \   WHERE table_schema = 'harness'\
    \     AND table_name = 'sessions'\
    \     AND column_name = 'metadata_value_id'\
    \ ),\
    \ to_regclass('harness.structured_values') IS NULL"
    Encoders.noParams
    (Decoders.singleRow $
        (,,)
            <$> Decoders.column (Decoders.nonNullable Decoders.bool)
            <*> Decoders.column (Decoders.nonNullable Decoders.bool)
            <*> Decoders.column (Decoders.nonNullable Decoders.bool))
