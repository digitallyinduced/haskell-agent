{-# LANGUAGE OverloadedStrings #-}

module Agent.Store.Postgres.Session.Schema
    ( sessionSchemaStatements
    , sessionSearchIndexStatements
    ) where

import Data.ByteString (ByteString)

import Agent.Store.Postgres.Interaction
    ( interactionSchemaStatements
    )
import Agent.Store.Postgres.SessionItem
    ( sessionItemSchemaStatements
    )

sessionSchemaStatements :: [ByteString]
sessionSchemaStatements =
    [ "CREATE TABLE IF NOT EXISTS harness.sessions (\
      \ session_id uuid PRIMARY KEY DEFAULT pg_catalog.uuidv7(),\
      \ session_key text NOT NULL UNIQUE,\
      \ session_schema_version integer NOT NULL CHECK (session_schema_version > 0),\
      \ created_at timestamptz NOT NULL,\
      \ updated_at timestamptz NOT NULL,\
      \ provider text NOT NULL CHECK (length(btrim(provider)) > 0),\
      \ connection_id text NOT NULL CHECK (length(btrim(connection_id)) > 0),\
      \ model_id text NOT NULL CHECK (length(btrim(model_id)) > 0),\
      \ transport_model_id text,\
      \ dialect text NOT NULL CHECK (length(btrim(dialect)) > 0),\
      \ legacy_target_provider text,\
      \ legacy_target_connection text,\
      \ legacy_target_effective_model text,\
      \ legacy_target_dialect text,\
      \ cwd text NOT NULL,\
      \ effort text NOT NULL,\
      \ title text NOT NULL,\
      \ title_is_manual boolean NOT NULL,\
      \ title_refresh_index bigint NOT NULL CHECK (title_refresh_index >= 0),\
      \ title_user_turns bigint NOT NULL CHECK (title_user_turns >= 0),\
      \ last_response_id text,\
      \ input_tokens bigint NOT NULL CHECK (input_tokens >= 0),\
      \ output_tokens bigint NOT NULL CHECK (output_tokens >= 0),\
      \ cached_tokens bigint NOT NULL CHECK (cached_tokens >= 0),\
      \ last_recap text,\
      \ last_turn_summary text,\
      \ last_recap_main_turns bigint NOT NULL DEFAULT 0 CHECK (last_recap_main_turns >= 0),\
      \ next_event_sequence bigint NOT NULL DEFAULT 1,\
      \ next_turn_index bigint NOT NULL DEFAULT 0,\
      \ deleted_at timestamptz,\
      \ CHECK (updated_at >= created_at),\
      \ CHECK (next_event_sequence >= 1),\
      \ CHECK (next_turn_index >= 0),\
      \ CHECK (\
      \   (legacy_target_provider IS NULL\
      \     AND legacy_target_connection IS NULL\
      \     AND legacy_target_effective_model IS NULL\
      \     AND legacy_target_dialect IS NULL)\
      \   OR\
      \   (legacy_target_provider IS NOT NULL\
      \     AND legacy_target_connection IS NOT NULL\
      \     AND legacy_target_effective_model IS NOT NULL\
      \     AND legacy_target_dialect IS NOT NULL\
      \     AND length(btrim(legacy_target_provider)) > 0\
      \     AND length(btrim(legacy_target_connection)) > 0\
      \     AND length(btrim(legacy_target_effective_model)) > 0\
      \     AND length(btrim(legacy_target_dialect)) > 0)\
      \ )\
      \ )"
    , "CREATE INDEX IF NOT EXISTS sessions_updated_at_idx\
      \ ON harness.sessions (updated_at DESC)\
      \ WHERE deleted_at IS NULL"
    , "CREATE TABLE IF NOT EXISTS harness.session_events (\
      \ event_id uuid PRIMARY KEY DEFAULT pg_catalog.uuidv7(),\
      \ session_id uuid NOT NULL\
      \   REFERENCES harness.sessions(session_id),\
      \ sequence bigint NOT NULL,\
      \ event_kind text NOT NULL,\
      \ occurred_at timestamptz NOT NULL,\
      \ UNIQUE (session_id, sequence)\
      \ )"
    , "CREATE INDEX IF NOT EXISTS session_events_session_time_idx\
      \ ON harness.session_events (session_id, occurred_at DESC)"
    , "CREATE TABLE IF NOT EXISTS harness.session_turns (\
      \ turn_id uuid PRIMARY KEY DEFAULT pg_catalog.uuidv7(),\
      \ session_id uuid NOT NULL\
      \   REFERENCES harness.sessions(session_id),\
      \ event_id uuid NOT NULL UNIQUE\
      \   REFERENCES harness.session_events(event_id),\
      \ turn_index bigint NOT NULL,\
      \ event_sequence bigint NOT NULL,\
      \ occurred_at timestamptz NOT NULL,\
      \ user_text text NOT NULL,\
      \ assistant_text text,\
      \ error_text text,\
      \ response_id text,\
      \ transcript_effect text NOT NULL DEFAULT 'append'\
      \   CHECK (transcript_effect IN ('append', 'replace', 'reset')),\
      \ usage_input_tokens bigint,\
      \ usage_output_tokens bigint,\
      \ usage_cached_tokens bigint,\
      \ search_vector tsvector GENERATED ALWAYS AS (\
      \   setweight(to_tsvector('english', coalesce(user_text, '')), 'A') ||\
      \   setweight(to_tsvector('english', coalesce(assistant_text, '')), 'B')\
      \ ) STORED,\
      \ UNIQUE (session_id, turn_index),\
      \ UNIQUE (session_id, event_sequence),\
      \ FOREIGN KEY (session_id, event_sequence)\
      \   REFERENCES harness.session_events(session_id, sequence),\
      \ CHECK (\
      \   (usage_input_tokens IS NULL\
      \     AND usage_output_tokens IS NULL\
      \     AND usage_cached_tokens IS NULL)\
      \   OR\
      \   (usage_input_tokens >= 0\
      \     AND usage_output_tokens >= 0\
      \     AND usage_cached_tokens >= 0)\
      \ )\
      \ )"
    , "CREATE INDEX IF NOT EXISTS session_turns_search_idx\
      \ ON harness.session_turns USING gin (search_vector)"
    , "CREATE INDEX IF NOT EXISTS session_turns_session_time_idx\
      \ ON harness.session_turns (session_id, occurred_at DESC)"
    , "CREATE INDEX IF NOT EXISTS session_turns_session_index_idx\
      \ ON harness.session_turns (session_id, turn_index)"
    , "CREATE INDEX IF NOT EXISTS session_turns_checkpoint_idx\
      \ ON harness.session_turns (session_id, turn_index DESC)\
      \ WHERE transcript_effect <> 'append'"
    ]
    <> sessionSearchIndexStatements
    <> sessionItemSchemaStatements
    <> [ "CREATE TABLE IF NOT EXISTS harness.legacy_session_imports (\
       \ import_id uuid PRIMARY KEY DEFAULT pg_catalog.uuidv7(),\
       \ source_path text NOT NULL,\
       \ content_hash text NOT NULL,\
       \ session_id uuid NOT NULL REFERENCES harness.sessions(session_id),\
       \ imported_at timestamptz NOT NULL DEFAULT now(),\
       \ UNIQUE (source_path, content_hash),\
       \ UNIQUE (session_id, content_hash)\
       \ )"
       , "CREATE OR REPLACE FUNCTION harness.reject_session_fact_mutation()\
       \ RETURNS trigger\
       \ LANGUAGE plpgsql\
       \ AS $$ BEGIN\
       \ RAISE EXCEPTION 'session events and turns are immutable';\
       \ END $$"
       , "DROP TRIGGER IF EXISTS session_events_immutable\
       \ ON harness.session_events"
       , "CREATE TRIGGER session_events_immutable\
       \ BEFORE UPDATE OR DELETE ON harness.session_events\
       \ FOR EACH ROW EXECUTE FUNCTION harness.reject_session_fact_mutation()"
       , "DROP TRIGGER IF EXISTS session_turns_immutable\
       \ ON harness.session_turns"
       , "CREATE TRIGGER session_turns_immutable\
       \ BEFORE UPDATE OR DELETE ON harness.session_turns\
       \ FOR EACH ROW EXECUTE FUNCTION harness.reject_session_fact_mutation()"
       ]
    <> interactionSchemaStatements

-- | Trigram indexes that make the substring branches of conversation search
-- indexable. Without them the @ILIKE@ fallbacks in 'searchTurnsStatement'
-- force a sequential scan of @session_turns@ even though the tsvector branch
-- has a GIN index: an @OR@ can only use a bitmap scan when every branch is
-- indexable. With @pg_trgm@ indexes on the raw text columns the planner can
-- combine all three branches with a BitmapOr.
-- The column-existence guards keep the shared statements safe as a
-- catch-up migration on partially-shaped legacy stores; on freshly created
-- schemas they are trivially true.
sessionSearchIndexStatements :: [ByteString]
sessionSearchIndexStatements =
    [ "CREATE EXTENSION IF NOT EXISTS pg_trgm"
    , "DO $ha$\
      \ BEGIN\
      \   IF EXISTS (\
      \     SELECT 1 FROM information_schema.columns\
      \     WHERE table_schema = 'harness'\
      \       AND table_name = 'session_turns'\
      \       AND column_name = 'user_text'\
      \   ) THEN\
      \     CREATE INDEX IF NOT EXISTS session_turns_user_text_trgm_idx\
      \       ON harness.session_turns USING gin (user_text gin_trgm_ops);\
      \   END IF;\
      \   IF EXISTS (\
      \     SELECT 1 FROM information_schema.columns\
      \     WHERE table_schema = 'harness'\
      \       AND table_name = 'session_turns'\
      \       AND column_name = 'assistant_text'\
      \   ) THEN\
      \     CREATE INDEX IF NOT EXISTS session_turns_assistant_text_trgm_idx\
      \       ON harness.session_turns USING gin (assistant_text gin_trgm_ops);\
      \   END IF;\
      \ END\
      \ $ha$"
    ]
