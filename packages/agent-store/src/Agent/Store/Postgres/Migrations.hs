{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Ordered, transactional migrations for harness-owned schemas.
module Agent.Store.Postgres.Migrations
    ( Migration(..)
    , coreMigrations
    , runMigrations
    , runCoreMigrations
    ) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int64)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement)
import qualified Hasql.Transaction as Transaction
import qualified Hasql.Transaction.Sessions as Transactions

import Agent.Store.Postgres.Hasql (mkStatement)

import Agent.Store.Postgres.Connection
import Agent.Store.Postgres.Scope (customSchemaStatements)
import Agent.Store.Postgres.Session (sessionSchemaStatements)
import Agent.Store.Types

data Migration = Migration
    { migrationVersion :: !Int64
    , migrationName :: !Text
    , migrationStatements :: ![ByteString]
    }
    deriving (Eq, Show)

coreMigrations :: [Migration]
coreMigrations =
    [ Migration
        { migrationVersion = 1
        , migrationName = "initial harness storage"
        , migrationStatements =
            customSchemaStatements
            <> sessionSchemaStatements
        }
    , Migration
        { migrationVersion = 2
        , migrationName = "restricted harness runtime role"
        , migrationStatements =
            [ "DO $ha$\
              \ BEGIN\
              \   IF NOT EXISTS (\
              \     SELECT 1 FROM pg_catalog.pg_roles\
              \     WHERE rolname = 'ha_runtime'\
              \   ) THEN\
              \     CREATE ROLE ha_runtime LOGIN\
              \       NOSUPERUSER NOCREATEDB NOCREATEROLE\
              \       NOINHERIT NOREPLICATION NOBYPASSRLS;\
              \   END IF;\
              \ END\
              \ $ha$"
            , "ALTER ROLE ha_runtime LOGIN\
              \ NOSUPERUSER NOCREATEDB NOCREATEROLE\
              \ NOINHERIT NOREPLICATION NOBYPASSRLS"
            , "ALTER ROLE ha_runtime SET search_path TO pg_catalog"
            , "ALTER ROLE ha_runtime SET statement_timeout TO '30s'"
            , "ALTER ROLE ha_runtime SET lock_timeout TO '5s'"
            , "ALTER ROLE ha_runtime\
              \ SET idle_in_transaction_session_timeout TO '30s'"
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
            <> sessionRuntimeGrantStatements
            <> [ "GRANT SELECT, INSERT, UPDATE\
              \ ON harness.custom_scopes TO ha_runtime"
            , "GRANT SELECT, INSERT, UPDATE\
              \ ON harness.custom_sql_audit TO ha_runtime"
            , "GRANT SELECT, INSERT, DELETE\
              \ ON harness.custom_catalog_snapshots TO ha_runtime"
            , "GRANT SELECT, INSERT\
              \ ON harness.custom_catalog_objects TO ha_runtime"
            , "GRANT SELECT, INSERT\
              \ ON harness.custom_catalog_columns TO ha_runtime"
            , "GRANT SELECT, INSERT\
              \ ON harness.custom_catalog_constraints TO ha_runtime"
            , "GRANT SELECT, INSERT\
              \ ON harness.custom_catalog_indexes TO ha_runtime"
            ]
        }
    , Migration
        { migrationVersion = 3
        , migrationName = "typed relational session storage"
        , migrationStatements =
            [ prepareTypedSessionSchemaStatement ]
            <> sessionSchemaStatements
            <> sessionRuntimeGrantStatements
        }
    , Migration
        { migrationVersion = 4
        , migrationName = "text tool outputs"
        , migrationStatements = [migrateToolOutputsToTextStatement]
        }
    ]

-- Version 1 shipped only on the in-development PostgreSQL branch. Empty
-- clusters can be upgraded in place; non-empty normalized session stores fail
-- explicitly rather than discarding data that cannot be converted losslessly
-- without retaining the removed generic value codec.
prepareTypedSessionSchemaStatement :: ByteString
prepareTypedSessionSchemaStatement =
    "DO $ha$\
    \ BEGIN\
    \   IF EXISTS (\
    \     SELECT 1 FROM information_schema.columns\
    \     WHERE table_schema = 'harness'\
    \       AND table_name = 'sessions'\
    \       AND column_name = 'metadata_value_id'\
    \   ) THEN\
    \     IF EXISTS (SELECT 1 FROM harness.sessions) THEN\
    \       RAISE EXCEPTION USING\
    \         ERRCODE = '55000',\
    \         MESSAGE =\
    \           'cannot automatically migrate non-empty normalized session storage',\
    \         HINT =\
    \           'export or reset the private PostgreSQL session store before upgrading';\
    \     END IF;\
    \     DROP TABLE IF EXISTS harness.legacy_session_imports;\
    \     DROP TABLE IF EXISTS harness.session_turns;\
    \     DROP TABLE IF EXISTS harness.session_events;\
    \     DROP TABLE IF EXISTS harness.sessions;\
    \     DROP TABLE IF EXISTS harness.structured_values CASCADE;\
    \     DROP FUNCTION IF EXISTS\
    \       harness.raise_normalized_value_error(text);\
    \   END IF;\
    \ END\
    \ $ha$"

migrateToolOutputsToTextStatement :: ByteString
migrateToolOutputsToTextStatement =
    "DO $ha$\
    \ BEGIN\
    \   IF EXISTS (\
    \     SELECT 1 FROM information_schema.columns\
    \     WHERE table_schema = 'harness'\
    \       AND table_name = 'session_function_call_outputs'\
    \       AND column_name = 'output'\
    \   ) THEN\
    \     ALTER TABLE harness.session_function_call_outputs\
    \       ALTER COLUMN output TYPE text USING\
    \         CASE WHEN jsonb_typeof(output) = 'string'\
    \           THEN output #>> '{}' ELSE output::text END;\
    \     ALTER TABLE harness.session_function_call_outputs\
    \       RENAME COLUMN output TO output_text;\
    \   END IF;\
    \   IF EXISTS (\
    \     SELECT 1 FROM information_schema.columns\
    \     WHERE table_schema = 'harness'\
    \       AND table_name = 'session_custom_tool_call_outputs'\
    \       AND column_name = 'output'\
    \   ) THEN\
    \     ALTER TABLE harness.session_custom_tool_call_outputs\
    \       ALTER COLUMN output TYPE text USING\
    \         CASE WHEN jsonb_typeof(output) = 'string'\
    \           THEN output #>> '{}' ELSE output::text END;\
    \     ALTER TABLE harness.session_custom_tool_call_outputs\
    \       RENAME COLUMN output TO output_text;\
    \   END IF;\
    \ END\
    \ $ha$"

sessionRuntimeGrantStatements :: [ByteString]
sessionRuntimeGrantStatements =
    [ "GRANT SELECT, INSERT, UPDATE\
      \ ON harness.sessions TO ha_runtime"
    , "GRANT SELECT, INSERT\
      \ ON harness.session_events TO ha_runtime"
    , "GRANT SELECT, INSERT\
      \ ON harness.session_turns TO ha_runtime"
    , "GRANT SELECT, INSERT\
      \ ON harness.legacy_session_imports TO ha_runtime"
    , "GRANT SELECT, INSERT\
      \ ON harness.session_response_items TO ha_runtime"
    , "GRANT SELECT, INSERT\
      \ ON harness.session_messages TO ha_runtime"
    , "GRANT SELECT, INSERT\
      \ ON harness.session_function_calls TO ha_runtime"
    , "GRANT SELECT, INSERT\
      \ ON harness.session_function_call_outputs TO ha_runtime"
    , "GRANT SELECT, INSERT\
      \ ON harness.session_custom_tool_calls TO ha_runtime"
    , "GRANT SELECT, INSERT\
      \ ON harness.session_custom_tool_call_outputs TO ha_runtime"
    , "GRANT SELECT, INSERT\
      \ ON harness.session_reasoning_items TO ha_runtime"
    , "GRANT SELECT, INSERT\
      \ ON harness.session_reasoning_summaries TO ha_runtime"
    , "GRANT SELECT, INSERT\
      \ ON harness.session_item_references TO ha_runtime"
    , "GRANT SELECT, INSERT\
      \ ON harness.session_tagged_items TO ha_runtime"
    , "GRANT SELECT, INSERT\
      \ ON harness.session_response_content_parts TO ha_runtime"
    ]

runCoreMigrations :: StorePool -> IO (Either StoreError ())
runCoreMigrations pool =
    runMigrations pool coreMigrations

-- | Apply all pending migrations in one serializable transaction.
--
-- A transaction-scoped advisory lock serializes application instances. The
-- migration table is bootstrapped by the same transaction.
runMigrations
    :: StorePool
    -> [Migration]
    -> IO (Either StoreError ())
runMigrations pool migrations =
    case validateMigrations migrations of
        Left err -> pure (Left err)
        Right ordered ->
            runTransaction pool Transactions.Serializable Transactions.Write do
                Transaction.sql
                    "DO $$ BEGIN\
                    \ PERFORM pg_advisory_xact_lock(684412850144245674);\
                    \ END $$"
                Transaction.sql
                    "CREATE SCHEMA IF NOT EXISTS harness"
                Transaction.sql
                    "CREATE TABLE IF NOT EXISTS harness.schema_migrations (\
                    \ migration_id uuid PRIMARY KEY DEFAULT pg_catalog.uuidv7(),\
                    \ version bigint NOT NULL UNIQUE,\
                    \ name text NOT NULL,\
                    \ applied_at timestamptz NOT NULL DEFAULT now()\
                    \ )"
                applied <- Map.fromList <$> Transaction.statement
                    ()
                    appliedMigrationsStatement
                case verifyNames applied ordered of
                    Left err -> pure (Left err)
                    Right () -> do
                        forM_ ordered \migration ->
                            case Map.lookup migration.migrationVersion applied of
                                Just _ -> pure ()
                                Nothing -> do
                                    forM_ migration.migrationStatements
                                        Transaction.sql
                                    Transaction.statement
                                        ( migration.migrationVersion
                                        , migration.migrationName
                                        )
                                        recordMigrationStatement
                        pure (Right ())
            >>= \case
                Left err -> pure (Left err)
                Right result -> pure result

validateMigrations :: [Migration] -> Either StoreError [Migration]
validateMigrations migrations
    | any ((<= 0) . (.migrationVersion)) migrations =
        Left $ StoreMigrationError "Migration versions must be positive"
    | hasDuplicates versions =
        Left $ StoreMigrationError "Migration versions must be unique"
    | otherwise = Right (List.sortOn (.migrationVersion) migrations)
  where
    versions = fmap (.migrationVersion) migrations

verifyNames
    :: Map.Map Int64 Text
    -> [Migration]
    -> Either StoreError ()
verifyNames applied =
    mapM_ \migration ->
        case Map.lookup migration.migrationVersion applied of
            Just storedName
                | storedName /= migration.migrationName ->
                    Left $ StoreMigrationError $
                        "Migration " <> Text.pack (show migration.migrationVersion)
                            <> " was previously recorded as "
                            <> storedName <> ", not " <> migration.migrationName
            _ -> Right ()

hasDuplicates :: Ord a => [a] -> Bool
hasDuplicates values =
    length values /= Map.size (Map.fromList (fmap (\value -> (value, ())) values))

appliedMigrationsStatement :: Statement () [(Int64, Text)]
appliedMigrationsStatement = mkStatement
    "SELECT version, name FROM harness.schema_migrations ORDER BY version"
    Encoders.noParams
    (Decoders.rowList $
        (,)
            <$> Decoders.column (Decoders.nonNullable Decoders.int8)
            <*> Decoders.column (Decoders.nonNullable Decoders.text))
    True

recordMigrationStatement :: Statement (Int64, Text) ()
recordMigrationStatement = mkStatement
    "INSERT INTO harness.schema_migrations (version, name) VALUES ($1, $2)"
    ( (fst >$< Encoders.param (Encoders.nonNullable Encoders.int8))
        <> (snd >$< Encoders.param (Encoders.nonNullable Encoders.text))
    )
    Decoders.noResult
    True
