{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FieldSelectors #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Agent.Store.Postgres.ManagedSpec (spec) where

import Control.Exception.Safe (finally)
import Data.ByteString (ByteString)
import Data.Either (isLeft, isRight)
import Data.Text (Text)
import Data.Time.Clock (addUTCTime, getCurrentTime)
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
    , coreMigrations
    , runMigrations
    )
import Agent.Store.Postgres.Scope (customSchemaStatements)
import Agent.Store.Postgres.UsageCache
    ( AccountUsageCacheEntry(..)
    , loadAccountUsageCache
    , upsertAccountUsageCache
    )

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
                    fetchedAt <- getCurrentTime
                    let usageEntry = AccountUsageCacheEntry
                            { accountUsageCacheProvider = "openai"
                            , accountUsageCacheAccountId = "account-1"
                            , accountUsageCachePayload = "{\"remaining\":42}"
                            , accountUsageCacheFetchedAt = fetchedAt
                            , accountUsageCacheExpiresAt =
                                addUTCTime 300 fetchedAt
                            }
                    upsertAccountUsageCache (trustedPool store) usageEntry
                        `shouldReturn` Right ()
                    loadAccountUsageCache
                        (trustedPool store)
                        "openai"
                        "account-1"
                        `shouldReturn` Right (Just usageEntry)
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

        it "migrates legacy tool outputs to typed text fields" $
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
                                ("could not open migration pool: " <> show err)
                        Right ownerPool ->
                            finally
                                (do
                                    runMigrations ownerPool
                                        legacyTypedToolOutputMigrations
                                        `shouldReturn` Right ()
                                    runMigrations ownerPool coreMigrations
                                        `shouldReturn` Right ()
                                    withSession ownerPool
                                        (Session.statement ()
                                            migratedToolOutputsStatement)
                                        `shouldReturn`
                                            Right (True, True, True, True)
                                )
                                (closeStorePool ownerPool)
                    ) `finally` cleanup

        it "backfills transcript effects and restores turn immutability" $
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
                                ("could not open migration pool: " <> show err)
                        Right ownerPool ->
                            finally
                                (do
                                    runMigrations ownerPool
                                        legacyTranscriptEffectMigrations
                                        `shouldReturn` Right ()
                                    runMigrations ownerPool coreMigrations
                                        `shouldReturn` Right ()
                                    withSession ownerPool
                                        (Session.statement ()
                                            migratedTranscriptEffectsStatement)
                                        `shouldReturn`
                                            Right
                                                [ "append"
                                                , "replace"
                                                , "replace"
                                                , "replace"
                                                , "reset"
                                                ]
                                    immutable <- withSession ownerPool
                                        (Session.script
                                            "UPDATE harness.session_turns\
                                            \ SET user_text = 'mutated'\
                                            \ WHERE turn_index = 0")
                                    immutable `shouldSatisfy` isLeft
                                )
                                (closeStorePool ownerPool)
                    ) `finally` cleanup

        it "migrates opaque response fields to text columns" $
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
                                ("could not open migration pool: " <> show err)
                        Right ownerPool ->
                            finally
                                (do
                                    runMigrations ownerPool
                                        legacyOpaqueFieldMigrations
                                        `shouldReturn` Right ()
                                    runMigrations ownerPool coreMigrations
                                        `shouldReturn` Right ()
                                    withSession ownerPool
                                        (Session.statement ()
                                            migratedOpaqueFieldsStatement)
                                        `shouldReturn`
                                            Right (True, True, True)
                                )
                                (closeStorePool ownerPool)
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

legacyTranscriptEffectMigrations :: [Migration]
legacyTranscriptEffectMigrations =
    [ Migration
        { migrationVersion = 1
        , migrationName = "initial harness storage"
        , migrationStatements =
            [ "CREATE TABLE harness.session_turns (\
              \ turn_id uuid PRIMARY KEY,\
              \ session_id uuid NOT NULL,\
              \ turn_index bigint NOT NULL,\
              \ user_text text NOT NULL\
              \ )"
            , "CREATE TABLE harness.session_response_items (\
              \ response_item_id uuid PRIMARY KEY,\
              \ turn_id uuid NOT NULL,\
              \ item_type text NOT NULL\
              \ )"
            , "CREATE TABLE harness.session_messages (\
              \ response_item_id uuid PRIMARY KEY,\
              \ role_name text NOT NULL,\
              \ content_text text\
              \ )"
            , "CREATE TABLE harness.session_response_content_parts (\
              \ content_part_id uuid PRIMARY KEY,\
              \ response_item_id uuid NOT NULL,\
              \ text_value text\
              \ )"
            , "CREATE OR REPLACE FUNCTION\
              \ harness.reject_session_fact_mutation()\
              \ RETURNS trigger\
              \ LANGUAGE plpgsql\
              \ AS $$ BEGIN\
              \ RAISE EXCEPTION 'session events and turns are immutable';\
              \ END $$"
            , "CREATE TRIGGER session_turns_immutable\
              \ BEFORE UPDATE OR DELETE ON harness.session_turns\
              \ FOR EACH ROW EXECUTE FUNCTION\
              \ harness.reject_session_fact_mutation()"
            , "INSERT INTO harness.session_turns\
              \ (turn_id, session_id, turn_index, user_text) VALUES\
              \ ('10000000-0000-0000-0000-000000000001',\
              \  '20000000-0000-0000-0000-000000000001', 0, 'hello'),\
              \ ('10000000-0000-0000-0000-000000000002',\
              \  '20000000-0000-0000-0000-000000000001', 1, '/compact'),\
              \ ('10000000-0000-0000-0000-000000000003',\
              \  '20000000-0000-0000-0000-000000000001', 2, 'automatic'),\
              \ ('10000000-0000-0000-0000-000000000004',\
              \  '20000000-0000-0000-0000-000000000001', 3, 'summary'),\
              \ ('10000000-0000-0000-0000-000000000005',\
              \  '20000000-0000-0000-0000-000000000001', 4, '/clear')"
            , "INSERT INTO harness.session_response_items\
              \ (response_item_id, turn_id, item_type) VALUES\
              \ ('30000000-0000-0000-0000-000000000001',\
              \  '10000000-0000-0000-0000-000000000003', 'compaction'),\
              \ ('30000000-0000-0000-0000-000000000002',\
              \  '10000000-0000-0000-0000-000000000004', 'message'),\
              \ ('30000000-0000-0000-0000-000000000003',\
              \  '10000000-0000-0000-0000-000000000005', 'compaction')"
            , "INSERT INTO harness.session_messages\
              \ (response_item_id, role_name, content_text) VALUES\
              \ ('30000000-0000-0000-0000-000000000002', 'assistant',\
              \  'Compacted conversation summary: retained facts')"
            ]
        }
    , Migration
        { migrationVersion = 2
        , migrationName = "restricted harness runtime role"
        , migrationStatements = []
        }
    , Migration
        { migrationVersion = 3
        , migrationName = "typed relational session storage"
        , migrationStatements = []
        }
    , Migration
        { migrationVersion = 4
        , migrationName = "text tool outputs"
        , migrationStatements = []
        }
    , Migration
        { migrationVersion = 5
        , migrationName = "versioned learned skills"
        , migrationStatements = []
        }
    , Migration
        { migrationVersion = 6
        , migrationName = "typed response item fields"
        , migrationStatements = []
        }
    ]

legacyOpaqueFieldMigrations :: [Migration]
legacyOpaqueFieldMigrations =
    [ Migration
        { migrationVersion = 1
        , migrationName = "initial harness storage"
        , migrationStatements =
            [ "CREATE TABLE harness.session_messages (\
              \ response_item_id uuid PRIMARY KEY,\
              \ extra_fields jsonb NOT NULL DEFAULT '{}'::jsonb\
              \   CONSTRAINT provider_fields_are_objects\
              \   CHECK (jsonb_typeof(extra_fields) = 'object')\
              \ )"
            , "INSERT INTO harness.session_messages\
              \ (response_item_id, extra_fields) VALUES\
              \ ('00000000-0000-0000-0000-000000000001',\
              \ '{\"provider\":true}')"
            , "CREATE TABLE harness.session_tagged_items (\
              \ response_item_id uuid PRIMARY KEY,\
              \ fields jsonb NOT NULL DEFAULT '{}'::jsonb\
              \   CHECK (jsonb_typeof(fields) = 'object')\
              \ )"
            , "INSERT INTO harness.session_tagged_items\
              \ (response_item_id, fields) VALUES\
              \ ('00000000-0000-0000-0000-000000000002',\
              \ '{\"tagged\":true}')"
            , "CREATE TABLE harness.session_response_content_parts (\
              \ content_part_id uuid PRIMARY KEY,\
              \ input_audio jsonb,\
              \ prompt_cache_breakpoint jsonb,\
              \ annotations jsonb,\
              \ logprobs jsonb,\
              \ extra_fields jsonb NOT NULL DEFAULT '{}'::jsonb\
              \   CHECK (jsonb_typeof(extra_fields) = 'object')\
              \ )"
            , "INSERT INTO harness.session_response_content_parts\
              \ (content_part_id, input_audio, prompt_cache_breakpoint,\
              \ annotations, logprobs, extra_fields) VALUES\
              \ ('00000000-0000-0000-0000-000000000003',\
              \ '{\"data\":\"abc\"}', '{\"scope\":\"turn\"}',\
              \ '[{\"type\":\"citation\"}]', '[{\"token\":\"ok\"}]',\
              \ '{\"leaf\":true}')"
            ]
        }
    , Migration
        { migrationVersion = 2
        , migrationName = "restricted harness runtime role"
        , migrationStatements =
            [ "CREATE ROLE ha_runtime LOGIN\
              \ NOSUPERUSER NOCREATEDB NOCREATEROLE\
              \ NOINHERIT NOREPLICATION NOBYPASSRLS"
            ]
        }
    , Migration
        { migrationVersion = 3
        , migrationName = "typed relational session storage"
        , migrationStatements = []
        }
    , Migration
        { migrationVersion = 4
        , migrationName = "text tool outputs"
        , migrationStatements = []
        }
    , Migration
        { migrationVersion = 5
        , migrationName = "versioned learned skills"
        , migrationStatements = []
        }
    ]

legacyTypedToolOutputMigrations :: [Migration]
legacyTypedToolOutputMigrations =
    [ Migration
        { migrationVersion = 1
        , migrationName = "initial harness storage"
        , migrationStatements =
            [ "CREATE TABLE harness.session_function_call_outputs (\
              \ response_item_id uuid PRIMARY KEY,\
              \ output jsonb NOT NULL\
              \ )"
            , "INSERT INTO harness.session_function_call_outputs\
              \ (response_item_id, output) VALUES\
              \ ('00000000-0000-0000-0000-000000000001', '\"plain\"')"
            , "CREATE TABLE harness.session_custom_tool_call_outputs (\
              \ response_item_id uuid PRIMARY KEY,\
              \ output jsonb NOT NULL\
              \ )"
            , "INSERT INTO harness.session_custom_tool_call_outputs\
              \ (response_item_id, output) VALUES\
              \ ('00000000-0000-0000-0000-000000000002',\
              \ '{\"nested\":true}')"
            ]
        }
    , Migration
        { migrationVersion = 2
        , migrationName = "restricted harness runtime role"
        , migrationStatements =
            [ "CREATE ROLE ha_runtime LOGIN\
              \ NOSUPERUSER NOCREATEDB NOCREATEROLE\
              \ NOINHERIT NOREPLICATION NOBYPASSRLS"
            ]
        }
    , Migration
        { migrationVersion = 3
        , migrationName = "typed relational session storage"
        , migrationStatements = []
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

migratedOpaqueFieldsStatement :: Statement () (Bool, Bool, Bool)
migratedOpaqueFieldsStatement = Statement.preparable
    "SELECT\
    \ (SELECT count(*) = 7\
    \   FROM information_schema.columns\
    \   WHERE table_schema = 'harness'\
    \     AND data_type = 'text'\
    \     AND (table_name, column_name) IN (\
    \       ('session_messages', 'extra_fields_text'),\
    \       ('session_tagged_items', 'fields_text'),\
    \       ('session_response_content_parts', 'input_audio_text'),\
    \       ('session_response_content_parts',\
    \         'prompt_cache_breakpoint_text'),\
    \       ('session_response_content_parts', 'annotations_text'),\
    \       ('session_response_content_parts', 'logprobs_text'),\
    \       ('session_response_content_parts', 'extra_fields_text')\
    \     )),\
    \ (SELECT extra_fields_text LIKE '%\"provider\"%'\
    \   FROM harness.session_messages),\
    \ (SELECT input_audio_text LIKE '%\"data\"%'\
    \     AND prompt_cache_breakpoint_text LIKE '%\"scope\"%'\
    \     AND annotations_text LIKE '%\"citation\"%'\
    \     AND logprobs_text LIKE '%\"token\"%'\
    \     AND extra_fields_text LIKE '%\"leaf\"%'\
    \   FROM harness.session_response_content_parts)"
    Encoders.noParams
    (Decoders.singleRow $
        (,,)
            <$> Decoders.column (Decoders.nonNullable Decoders.bool)
            <*> Decoders.column (Decoders.nonNullable Decoders.bool)
            <*> Decoders.column (Decoders.nonNullable Decoders.bool))

migratedToolOutputsStatement :: Statement () (Bool, Bool, Bool, Bool)
migratedToolOutputsStatement = Statement.preparable
    "SELECT\
    \ EXISTS (\
    \   SELECT 1 FROM information_schema.columns\
    \   WHERE table_schema = 'harness'\
    \     AND table_name = 'session_function_call_outputs'\
    \     AND column_name = 'output_text'\
    \     AND data_type = 'text'\
    \ ) AND EXISTS (\
    \   SELECT 1 FROM information_schema.columns\
    \   WHERE table_schema = 'harness'\
    \     AND table_name = 'session_function_call_outputs'\
    \     AND column_name = 'output_kind'\
    \     AND data_type = 'text'\
    \ ),\
    \ (SELECT output_text = '\"plain\"' AND output_kind = 'encoded'\
    \   FROM harness.session_function_call_outputs),\
    \ EXISTS (\
    \   SELECT 1 FROM information_schema.columns\
    \   WHERE table_schema = 'harness'\
    \     AND table_name = 'session_custom_tool_call_outputs'\
    \     AND column_name = 'output_text'\
    \     AND data_type = 'text'\
    \ ) AND EXISTS (\
    \   SELECT 1 FROM information_schema.columns\
    \   WHERE table_schema = 'harness'\
    \     AND table_name = 'session_custom_tool_call_outputs'\
    \     AND column_name = 'output_kind'\
    \     AND data_type = 'text'\
    \ ),\
    \ (SELECT output_text = '{\"nested\": true}'\
    \     AND output_kind = 'encoded'\
    \   FROM harness.session_custom_tool_call_outputs)"
    Encoders.noParams
    (Decoders.singleRow $
        (,,,)
            <$> Decoders.column (Decoders.nonNullable Decoders.bool)
            <*> Decoders.column (Decoders.nonNullable Decoders.bool)
            <*> Decoders.column (Decoders.nonNullable Decoders.bool)
            <*> Decoders.column (Decoders.nonNullable Decoders.bool))

migratedTranscriptEffectsStatement :: Statement () [Text]
migratedTranscriptEffectsStatement = Statement.preparable
    "SELECT transcript_effect\
    \ FROM harness.session_turns\
    \ ORDER BY turn_index"
    Encoders.noParams
    (Decoders.rowList $
        Decoders.column (Decoders.nonNullable Decoders.text))

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
