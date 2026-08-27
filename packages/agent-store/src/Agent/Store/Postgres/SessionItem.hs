{-# LANGUAGE OverloadedStrings #-}

-- | Relational persistence for the response items attached to a session turn.
--
-- Every known field is passed to PostgreSQL as a typed parameter. Open
-- provider-defined leaves are already encoded as opaque text by the caller;
-- this module neither parses nor renders their wire representation.
module Agent.Store.Postgres.SessionItem
    ( sessionItemSchemaStatements
    , insertResponseItems
    , loadResponseItems
    ) where

import qualified Data.ByteString as ByteString

import Agent.Store.Postgres.SessionItem.Read (loadResponseItems)
import Agent.Store.Postgres.SessionItem.Write (insertResponseItems)

sessionItemSchemaStatements :: [ByteString.ByteString]
sessionItemSchemaStatements =
    [ "CREATE TABLE IF NOT EXISTS harness.session_response_items (\
      \ response_item_id uuid PRIMARY KEY DEFAULT pg_catalog.uuidv7(),\
      \ turn_id uuid NOT NULL REFERENCES harness.session_turns(turn_id)\
      \   ON DELETE CASCADE,\
      \ item_index integer NOT NULL CHECK (item_index >= 0),\
      \ storage_kind text NOT NULL CHECK (storage_kind IN (\
      \   'message', 'function_call', 'function_call_output',\
      \   'custom_tool_call', 'custom_tool_call_output', 'reasoning',\
      \   'item_reference', 'tagged')),\
      \ item_type text NOT NULL CHECK (length(item_type) > 0),\
      \ representation text NOT NULL\
      \   CHECK (representation IN ('core', 'known', 'unknown')),\
      \ UNIQUE (turn_id, item_index)\
      \ )"
    , "CREATE INDEX IF NOT EXISTS session_response_items_type_idx\
      \ ON harness.session_response_items (item_type)"
    , "CREATE TABLE IF NOT EXISTS harness.session_messages (\
      \ response_item_id uuid PRIMARY KEY REFERENCES\
      \   harness.session_response_items(response_item_id) ON DELETE CASCADE,\
      \ provider_item_id text,\
      \ role_name text NOT NULL,\
      \ status_name text,\
      \ phase text,\
      \ content_kind text NOT NULL CHECK (content_kind IN ('text', 'parts')),\
      \ content_text text,\
      \ extra_fields_text text NOT NULL DEFAULT '{}',\
      \ CHECK ((content_kind = 'text' AND content_text IS NOT NULL)\
      \   OR (content_kind = 'parts' AND content_text IS NULL))\
      \ )"
    , "CREATE TABLE IF NOT EXISTS harness.session_function_calls (\
      \ response_item_id uuid PRIMARY KEY REFERENCES\
      \   harness.session_response_items(response_item_id) ON DELETE CASCADE,\
      \ provider_item_id text,\
      \ call_id text NOT NULL,\
      \ function_name text NOT NULL,\
      \ arguments text NOT NULL,\
      \ status_name text,\
      \ extra_fields_text text NOT NULL DEFAULT '{}'\
      \ )"
    , "CREATE INDEX IF NOT EXISTS session_function_calls_call_idx\
      \ ON harness.session_function_calls (call_id)"
    , "CREATE INDEX IF NOT EXISTS session_function_calls_name_idx\
      \ ON harness.session_function_calls (function_name)"
    , "CREATE TABLE IF NOT EXISTS harness.session_function_call_outputs (\
      \ response_item_id uuid PRIMARY KEY REFERENCES\
      \   harness.session_response_items(response_item_id) ON DELETE CASCADE,\
      \ provider_item_id text,\
      \ call_id text NOT NULL,\
      \ output_kind text NOT NULL CHECK (output_kind IN ('text', 'encoded')),\
      \ output_text text NOT NULL,\
      \ status_name text,\
      \ extra_fields_text text NOT NULL DEFAULT '{}'\
      \ )"
    , "CREATE INDEX IF NOT EXISTS session_function_call_outputs_call_idx\
      \ ON harness.session_function_call_outputs (call_id)"
    , "CREATE TABLE IF NOT EXISTS harness.session_custom_tool_calls (\
      \ response_item_id uuid PRIMARY KEY REFERENCES\
      \   harness.session_response_items(response_item_id) ON DELETE CASCADE,\
      \ provider_item_id text,\
      \ call_id text NOT NULL,\
      \ tool_name text NOT NULL,\
      \ input_text text NOT NULL,\
      \ status_name text,\
      \ extra_fields_text text NOT NULL DEFAULT '{}'\
      \ )"
    , "CREATE INDEX IF NOT EXISTS session_custom_tool_calls_call_idx\
      \ ON harness.session_custom_tool_calls (call_id)"
    , "CREATE INDEX IF NOT EXISTS session_custom_tool_calls_name_idx\
      \ ON harness.session_custom_tool_calls (tool_name)"
    , "CREATE TABLE IF NOT EXISTS harness.session_custom_tool_call_outputs (\
      \ response_item_id uuid PRIMARY KEY REFERENCES\
      \   harness.session_response_items(response_item_id) ON DELETE CASCADE,\
      \ provider_item_id text,\
      \ call_id text NOT NULL,\
      \ tool_name text,\
      \ output_kind text NOT NULL CHECK (output_kind IN ('text', 'encoded')),\
      \ output_text text NOT NULL,\
      \ status_name text,\
      \ extra_fields_text text NOT NULL DEFAULT '{}'\
      \ )"
    , "CREATE INDEX IF NOT EXISTS session_custom_tool_call_outputs_call_idx\
      \ ON harness.session_custom_tool_call_outputs (call_id)"
    , "CREATE TABLE IF NOT EXISTS harness.session_reasoning_items (\
      \ response_item_id uuid PRIMARY KEY REFERENCES\
      \   harness.session_response_items(response_item_id) ON DELETE CASCADE,\
      \ provider_item_id text,\
      \ has_content boolean NOT NULL,\
      \ encrypted_content text,\
      \ status_name text,\
      \ extra_fields_text text NOT NULL DEFAULT '{}'\
      \ )"
    , "CREATE TABLE IF NOT EXISTS harness.session_reasoning_summaries (\
      \ summary_part_id uuid PRIMARY KEY DEFAULT pg_catalog.uuidv7(),\
      \ response_item_id uuid NOT NULL REFERENCES\
      \   harness.session_reasoning_items(response_item_id) ON DELETE CASCADE,\
      \ part_index integer NOT NULL CHECK (part_index >= 0),\
      \ part_type text NOT NULL,\
      \ text_value text,\
      \ extra_fields_text text NOT NULL DEFAULT '{}',\
      \ UNIQUE (response_item_id, part_index)\
      \ )"
    , "CREATE TABLE IF NOT EXISTS harness.session_item_references (\
      \ response_item_id uuid PRIMARY KEY REFERENCES\
      \   harness.session_response_items(response_item_id) ON DELETE CASCADE,\
      \ provider_item_id text NOT NULL,\
      \ extra_fields_text text NOT NULL DEFAULT '{}'\
      \ )"
    , "CREATE TABLE IF NOT EXISTS harness.session_tagged_items (\
      \ response_item_id uuid PRIMARY KEY REFERENCES\
      \   harness.session_response_items(response_item_id) ON DELETE CASCADE,\
      \ wire_tag text NOT NULL,\
      \ fields_text text NOT NULL DEFAULT '{}'\
      \ )"
    , "CREATE TABLE IF NOT EXISTS harness.session_response_content_parts (\
      \ content_part_id uuid PRIMARY KEY DEFAULT pg_catalog.uuidv7(),\
      \ response_item_id uuid NOT NULL REFERENCES\
      \   harness.session_response_items(response_item_id) ON DELETE CASCADE,\
      \ part_index integer NOT NULL CHECK (part_index >= 0),\
      \ part_type text NOT NULL,\
      \ text_value text,\
      \ refusal_text text,\
      \ detail text,\
      \ file_data text,\
      \ file_id text,\
      \ file_url text,\
      \ filename text,\
      \ image_url text,\
      \ file_data_mime_type text,\
      \ file_data_bytes bytea,\
      \ image_mime_type text,\
      \ image_bytes bytea,\
      \ input_audio_text text,\
      \ prompt_cache_breakpoint_text text,\
      \ annotations_text text,\
      \ logprobs_text text,\
      \ extra_fields_text text NOT NULL DEFAULT '{}',\
      \ UNIQUE (response_item_id, part_index)\
      \ )"
    ]
