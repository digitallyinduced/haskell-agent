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
import Agent.Store.Postgres.Skill
    ( learnedSkillRuntimeGrantStatements
    , learnedSkillSchemaStatements
    )
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
    , Migration
        { migrationVersion = 5
        , migrationName = "versioned learned skills"
        , migrationStatements =
            learnedSkillSchemaStatements
            <> learnedSkillRuntimeGrantStatements
        }
    , Migration
        { migrationVersion = 6
        , migrationName = "typed response item fields"
        , migrationStatements =
            [migrateOpaqueSessionFieldsToTextStatement]
        }
    , Migration
        { migrationVersion = 7
        , migrationName = "session recap summaries"
        , migrationStatements =
            [ "ALTER TABLE IF EXISTS harness.sessions\
              \ ADD COLUMN IF NOT EXISTS last_recap text"
            , "ALTER TABLE IF EXISTS harness.sessions\
              \ ADD COLUMN IF NOT EXISTS last_turn_summary text"
            , "ALTER TABLE IF EXISTS harness.sessions\
              \ ADD COLUMN IF NOT EXISTS last_recap_main_turns\
              \ bigint NOT NULL DEFAULT 0"
            ]
        }
    , Migration
        { migrationVersion = 8
        , migrationName = "session transcript effects and paging"
        , migrationStatements =
            [migrateSessionTranscriptEffectsStatement]
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

migrateSessionTranscriptEffectsStatement :: ByteString
migrateSessionTranscriptEffectsStatement =
    "DO $ha$\
    \ BEGIN\
    \   IF to_regclass('harness.session_turns') IS NULL THEN\
    \     RETURN;\
    \   END IF;\
    \   ALTER TABLE harness.session_turns\
    \     ADD COLUMN IF NOT EXISTS transcript_effect text;\
    \   DROP TRIGGER IF EXISTS session_turns_immutable\
    \     ON harness.session_turns;\
    \   UPDATE harness.session_turns\
    \     SET transcript_effect = 'append'\
    \     WHERE transcript_effect IS NULL;\
    \   UPDATE harness.session_turns\
    \     SET transcript_effect = 'replace'\
    \     WHERE btrim(user_text) = '/compact'\
    \       OR btrim(user_text) LIKE '/compact %';\
    \   IF to_regclass('harness.session_response_items') IS NOT NULL THEN\
    \     UPDATE harness.session_turns turn_row\
    \       SET transcript_effect = 'replace'\
    \       WHERE EXISTS (\
    \         SELECT 1\
    \         FROM harness.session_response_items item\
    \         WHERE item.turn_id = turn_row.turn_id\
    \           AND item.item_type = 'compaction'\
    \       );\
    \   END IF;\
    \   IF to_regclass('harness.session_response_items') IS NOT NULL\
    \     AND to_regclass('harness.session_messages') IS NOT NULL\
    \     AND to_regclass('harness.session_response_content_parts')\
    \       IS NOT NULL THEN\
    \     UPDATE harness.session_turns turn_row\
    \       SET transcript_effect = 'replace'\
    \       WHERE EXISTS (\
    \         SELECT 1\
    \         FROM harness.session_response_items item\
    \         JOIN harness.session_messages message\
    \           ON message.response_item_id = item.response_item_id\
    \         LEFT JOIN harness.session_response_content_parts part\
    \           ON part.response_item_id = item.response_item_id\
    \         WHERE item.turn_id = turn_row.turn_id\
    \           AND message.role_name = 'assistant'\
    \           AND btrim(coalesce(\
    \             message.content_text, part.text_value, ''))\
    \             LIKE 'Compacted conversation summary:%'\
    \       );\
    \   END IF;\
    \   UPDATE harness.session_turns\
    \     SET transcript_effect = 'reset'\
    \     WHERE btrim(user_text) IN ('/clear', '/new');\
    \   ALTER TABLE harness.session_turns\
    \     ALTER COLUMN transcript_effect SET DEFAULT 'append';\
    \   ALTER TABLE harness.session_turns\
    \     ALTER COLUMN transcript_effect SET NOT NULL;\
    \   IF NOT EXISTS (\
    \     SELECT 1\
    \     FROM pg_catalog.pg_constraint constraint_row\
    \     JOIN pg_catalog.pg_class relation\
    \       ON relation.oid = constraint_row.conrelid\
    \     JOIN pg_catalog.pg_namespace schema_row\
    \       ON schema_row.oid = relation.relnamespace\
    \     WHERE schema_row.nspname = 'harness'\
    \       AND relation.relname = 'session_turns'\
    \       AND constraint_row.conname =\
    \         'session_turns_transcript_effect_check'\
    \   ) THEN\
    \     ALTER TABLE harness.session_turns\
    \       ADD CONSTRAINT session_turns_transcript_effect_check\
    \       CHECK (transcript_effect IN ('append', 'replace', 'reset'));\
    \   END IF;\
    \   CREATE INDEX IF NOT EXISTS session_turns_session_index_idx\
    \     ON harness.session_turns (session_id, turn_index);\
    \   CREATE INDEX IF NOT EXISTS session_turns_checkpoint_idx\
    \     ON harness.session_turns (session_id, turn_index DESC)\
    \     WHERE transcript_effect <> 'append';\
    \   IF to_regprocedure(\
    \     'harness.reject_session_fact_mutation()') IS NOT NULL THEN\
    \     CREATE TRIGGER session_turns_immutable\
    \       BEFORE UPDATE OR DELETE ON harness.session_turns\
    \       FOR EACH ROW EXECUTE FUNCTION\
    \         harness.reject_session_fact_mutation();\
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
    \       ALTER COLUMN output TYPE text USING output::text;\
    \     ALTER TABLE harness.session_function_call_outputs\
    \       RENAME COLUMN output TO output_text;\
    \     ALTER TABLE harness.session_function_call_outputs\
    \       ADD COLUMN IF NOT EXISTS\
    \         output_kind text NOT NULL DEFAULT 'encoded'\
    \       CHECK (output_kind IN ('text', 'encoded'));\
    \   END IF;\
    \   IF EXISTS (\
    \     SELECT 1 FROM information_schema.columns\
    \     WHERE table_schema = 'harness'\
    \       AND table_name = 'session_custom_tool_call_outputs'\
    \       AND column_name = 'output'\
    \   ) THEN\
    \     ALTER TABLE harness.session_custom_tool_call_outputs\
    \       ALTER COLUMN output TYPE text USING output::text;\
    \     ALTER TABLE harness.session_custom_tool_call_outputs\
    \       RENAME COLUMN output TO output_text;\
    \     ALTER TABLE harness.session_custom_tool_call_outputs\
    \       ADD COLUMN IF NOT EXISTS\
    \         output_kind text NOT NULL DEFAULT 'encoded'\
    \       CHECK (output_kind IN ('text', 'encoded'));\
    \   END IF;\
    \ END\
    \ $ha$"

-- Stores that already ran the original version-4 migration have no shape
-- marker for older output rows. Defaulting those rows to plain text preserves
-- the behavior of the previous loader; new rows retain their explicit kind.
migrateOpaqueSessionFieldsToTextStatement :: ByteString
migrateOpaqueSessionFieldsToTextStatement =
    "DO $ha$\
    \ DECLARE\
    \   target record;\
    \   constraint_name text;\
    \ BEGIN\
    \   FOR target IN\
    \     SELECT * FROM (VALUES\
    \       ('session_messages', 'extra_fields', 'extra_fields_text', true),\
    \       ('session_function_calls', 'extra_fields',\
    \         'extra_fields_text', true),\
    \       ('session_function_call_outputs', 'extra_fields',\
    \         'extra_fields_text', true),\
    \       ('session_custom_tool_calls', 'extra_fields',\
    \         'extra_fields_text', true),\
    \       ('session_custom_tool_call_outputs', 'extra_fields',\
    \         'extra_fields_text', true),\
    \       ('session_reasoning_items', 'extra_fields',\
    \         'extra_fields_text', true),\
    \       ('session_reasoning_summaries', 'extra_fields',\
    \         'extra_fields_text', true),\
    \       ('session_item_references', 'extra_fields',\
    \         'extra_fields_text', true),\
    \       ('session_tagged_items', 'fields', 'fields_text', true),\
    \       ('session_response_content_parts', 'input_audio',\
    \         'input_audio_text', false),\
    \       ('session_response_content_parts', 'prompt_cache_breakpoint',\
    \         'prompt_cache_breakpoint_text', false),\
    \       ('session_response_content_parts', 'annotations',\
    \         'annotations_text', false),\
    \       ('session_response_content_parts', 'logprobs',\
    \         'logprobs_text', false),\
    \       ('session_response_content_parts', 'extra_fields',\
    \         'extra_fields_text', true)\
    \     ) AS fields(table_name, old_name, new_name, has_default)\
    \   LOOP\
    \     IF EXISTS (\
    \       SELECT 1 FROM information_schema.columns\
    \       WHERE table_schema = 'harness'\
    \         AND table_name = target.table_name\
    \         AND column_name = target.old_name\
    \     ) THEN\
    \       FOR constraint_name IN\
    \         SELECT constraint_row.conname\
    \         FROM pg_catalog.pg_constraint constraint_row\
    \         JOIN pg_catalog.pg_class relation\
    \           ON relation.oid = constraint_row.conrelid\
    \         JOIN pg_catalog.pg_namespace schema_row\
    \           ON schema_row.oid = relation.relnamespace\
    \         WHERE schema_row.nspname = 'harness'\
    \           AND relation.relname = target.table_name\
    \           AND constraint_row.contype = 'c'\
    \           AND position(\
    \             target.old_name\
    \             IN pg_catalog.pg_get_constraintdef(constraint_row.oid)\
    \           ) > 0\
    \       LOOP\
    \         EXECUTE format(\
    \           'ALTER TABLE harness.%I DROP CONSTRAINT %I',\
    \           target.table_name, constraint_name\
    \         );\
    \       END LOOP;\
    \       EXECUTE format(\
    \         'ALTER TABLE harness.%I ALTER COLUMN %I DROP DEFAULT',\
    \         target.table_name, target.old_name\
    \       );\
    \       EXECUTE format(\
    \         'ALTER TABLE harness.%I ALTER COLUMN %I TYPE text USING %I::text',\
    \         target.table_name, target.old_name, target.old_name\
    \       );\
    \       IF target.has_default THEN\
    \         EXECUTE format(\
    \           'ALTER TABLE harness.%I ALTER COLUMN %I SET DEFAULT %L',\
    \           target.table_name, target.old_name, '{}'\
    \         );\
    \       END IF;\
    \       EXECUTE format(\
    \         'ALTER TABLE harness.%I RENAME COLUMN %I TO %I',\
    \         target.table_name, target.old_name, target.new_name\
    \       );\
    \     END IF;\
    \   END LOOP;\
    \   IF to_regclass('harness.session_function_call_outputs')\
    \       IS NOT NULL THEN\
    \     ALTER TABLE harness.session_function_call_outputs\
    \       ADD COLUMN IF NOT EXISTS output_kind text NOT NULL DEFAULT 'text';\
    \     IF NOT EXISTS (\
    \       SELECT 1 FROM pg_catalog.pg_constraint\
    \       WHERE conrelid =\
    \         'harness.session_function_call_outputs'::regclass\
    \         AND conname =\
    \           'session_function_call_outputs_output_kind_check'\
    \     ) THEN\
    \       ALTER TABLE harness.session_function_call_outputs\
    \         ADD CONSTRAINT\
    \           session_function_call_outputs_output_kind_check\
    \         CHECK (output_kind IN ('text', 'encoded'));\
    \     END IF;\
    \   END IF;\
    \   IF to_regclass('harness.session_custom_tool_call_outputs')\
    \       IS NOT NULL THEN\
    \     ALTER TABLE harness.session_custom_tool_call_outputs\
    \       ADD COLUMN IF NOT EXISTS output_kind text NOT NULL DEFAULT 'text';\
    \     IF NOT EXISTS (\
    \       SELECT 1 FROM pg_catalog.pg_constraint\
    \       WHERE conrelid =\
    \         'harness.session_custom_tool_call_outputs'::regclass\
    \         AND conname =\
    \           'session_custom_tool_call_outputs_output_kind_check'\
    \     ) THEN\
    \       ALTER TABLE harness.session_custom_tool_call_outputs\
    \         ADD CONSTRAINT\
    \           session_custom_tool_call_outputs_output_kind_check\
    \         CHECK (output_kind IN ('text', 'encoded'));\
    \     END IF;\
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
