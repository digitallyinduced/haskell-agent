{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Relational persistence for the response items attached to a session turn.
--
-- Stable fields have dedicated columns. JSONB is used only where the wire
-- type is intentionally open: provider extension fields and content-part
-- leaves such as annotations.
module Agent.Store.Postgres.SessionItem
    ( sessionItemSchemaStatements
    , insertResponseItems
    , loadResponseItems
    ) where

import Control.Monad (forM_)
import qualified Data.ByteString as ByteString
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int32)
import Data.Text (Text)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement)
import qualified Hasql.Transaction as Transaction

import Agent.Store.Postgres.Hasql (mkStatement)
import Agent.Store.SessionItem (StoredResponseItem(..))

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
      \ extra_fields jsonb NOT NULL DEFAULT '{}'::jsonb\
      \   CHECK (jsonb_typeof(extra_fields) = 'object'),\
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
      \ extra_fields jsonb NOT NULL DEFAULT '{}'::jsonb\
      \   CHECK (jsonb_typeof(extra_fields) = 'object')\
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
      \ output_text text NOT NULL,\
      \ status_name text,\
      \ extra_fields jsonb NOT NULL DEFAULT '{}'::jsonb\
      \   CHECK (jsonb_typeof(extra_fields) = 'object')\
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
      \ extra_fields jsonb NOT NULL DEFAULT '{}'::jsonb\
      \   CHECK (jsonb_typeof(extra_fields) = 'object')\
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
      \ output_text text NOT NULL,\
      \ status_name text,\
      \ extra_fields jsonb NOT NULL DEFAULT '{}'::jsonb\
      \   CHECK (jsonb_typeof(extra_fields) = 'object')\
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
      \ extra_fields jsonb NOT NULL DEFAULT '{}'::jsonb\
      \   CHECK (jsonb_typeof(extra_fields) = 'object')\
      \ )"
    , "CREATE TABLE IF NOT EXISTS harness.session_reasoning_summaries (\
      \ summary_part_id uuid PRIMARY KEY DEFAULT pg_catalog.uuidv7(),\
      \ response_item_id uuid NOT NULL REFERENCES\
      \   harness.session_reasoning_items(response_item_id) ON DELETE CASCADE,\
      \ part_index integer NOT NULL CHECK (part_index >= 0),\
      \ part_type text NOT NULL,\
      \ text_value text,\
      \ extra_fields jsonb NOT NULL DEFAULT '{}'::jsonb\
      \   CHECK (jsonb_typeof(extra_fields) = 'object'),\
      \ UNIQUE (response_item_id, part_index)\
      \ )"
    , "CREATE TABLE IF NOT EXISTS harness.session_item_references (\
      \ response_item_id uuid PRIMARY KEY REFERENCES\
      \   harness.session_response_items(response_item_id) ON DELETE CASCADE,\
      \ provider_item_id text NOT NULL,\
      \ extra_fields jsonb NOT NULL DEFAULT '{}'::jsonb\
      \   CHECK (jsonb_typeof(extra_fields) = 'object')\
      \ )"
    , "CREATE TABLE IF NOT EXISTS harness.session_tagged_items (\
      \ response_item_id uuid PRIMARY KEY REFERENCES\
      \   harness.session_response_items(response_item_id) ON DELETE CASCADE,\
      \ wire_tag text NOT NULL,\
      \ fields jsonb NOT NULL DEFAULT '{}'::jsonb\
      \   CHECK (jsonb_typeof(fields) = 'object')\
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
      \ input_audio jsonb,\
      \ prompt_cache_breakpoint jsonb,\
      \ annotations jsonb,\
      \ logprobs jsonb,\
      \ extra_fields jsonb NOT NULL DEFAULT '{}'::jsonb\
      \   CHECK (jsonb_typeof(extra_fields) = 'object'),\
      \ UNIQUE (response_item_id, part_index)\
      \ )"
    ]

data BaseParams = BaseParams
    { baseTurnId :: !Text
    , baseIndex :: !Int32
    , baseStorageKind :: !Text
    , baseItemType :: !Text
    , baseRepresentation :: !Text
    }

insertResponseItems
    :: Text
    -> [StoredResponseItem]
    -> Transaction.Transaction ()
insertResponseItems turnId items =
    forM_ (zip [0..] items) \(index, item) -> do
        let kind = storageKind item
        itemId <- Transaction.statement
            BaseParams
                { baseTurnId = turnId
                , baseIndex = fromIntegral (index :: Int)
                , baseStorageKind = kind
                , baseItemType = item.storedResponseItemType
                , baseRepresentation =
                    item.storedResponseItemRepresentation
                }
            insertBaseStatement
        insertItemPayload kind itemId item.storedResponseItemPayload

storageKind :: StoredResponseItem -> Text
storageKind item
    | item.storedResponseItemRepresentation /= "core" = "tagged"
    | otherwise =
        case item.storedResponseItemType of
            "message" -> "message"
            "function_call" -> "function_call"
            "function_call_output" -> "function_call_output"
            "custom_tool_call" -> "custom_tool_call"
            "custom_tool_call_output" -> "custom_tool_call_output"
            "reasoning" -> "reasoning"
            "item_reference" -> "item_reference"
            _ -> "tagged"

insertItemPayload
    :: Text
    -> Text
    -> Text
    -> Transaction.Transaction ()
insertItemPayload kind itemId payload =
    case kind of
        "message" -> do
            Transaction.statement (itemId, payload) insertMessagePayloadStatement
            Transaction.statement (itemId, payload) insertContentPayloadStatement
        "function_call" ->
            Transaction.statement
                (itemId, payload)
                insertFunctionCallPayloadStatement
        "function_call_output" ->
            Transaction.statement
                (itemId, payload)
                insertFunctionOutputPayloadStatement
        "custom_tool_call" ->
            Transaction.statement
                (itemId, payload)
                insertCustomCallPayloadStatement
        "custom_tool_call_output" ->
            Transaction.statement
                (itemId, payload)
                insertCustomOutputPayloadStatement
        "reasoning" -> do
            Transaction.statement
                (itemId, payload)
                insertReasoningPayloadStatement
            Transaction.statement
                (itemId, payload)
                insertSummaryPayloadStatement
            Transaction.statement
                (itemId, payload)
                insertContentPayloadStatement
        "item_reference" ->
            Transaction.statement
                (itemId, payload)
                insertReferencePayloadStatement
        _ ->
            Transaction.statement
                (itemId, payload)
                insertTaggedPayloadStatement

data LoadedItem = LoadedItem
    { loadedRepresentation :: !Text
    , loadedItemType :: !Text
    , loadedPayload :: !Text
    }

loadResponseItems
    :: Text
    -> Transaction.Transaction (Either Text [StoredResponseItem])
loadResponseItems turnId = do
    rows <- Transaction.statement turnId loadItemsStatement
    pure (traverse storedItem rows)
  where
    storedItem :: LoadedItem -> Either Text StoredResponseItem
    storedItem row =
        case row.loadedRepresentation of
            "core" -> Right (toStored row)
            "known" -> Right (toStored row)
            "unknown" -> Right (toStored row)
            value -> Left ("unknown response item representation: " <> value)
    toStored :: LoadedItem -> StoredResponseItem
    toStored row = StoredResponseItem
        { storedResponseItemType = row.loadedItemType
        , storedResponseItemRepresentation = row.loadedRepresentation
        , storedResponseItemPayload = row.loadedPayload
        }

insertBaseStatement :: Statement BaseParams Text
insertBaseStatement = mkStatement
    "INSERT INTO harness.session_response_items\
    \ (turn_id, item_index, storage_kind, item_type, representation)\
    \ VALUES ($1::uuid, $2, $3, $4, $5)\
    \ RETURNING response_item_id::text"
    ( fieldParam (.baseTurnId) textParam
        <> fieldParam (.baseIndex) int32Param
        <> fieldParam (.baseStorageKind) textParam
        <> fieldParam (.baseItemType) textParam
        <> fieldParam (.baseRepresentation) textParam
    )
    textSingleResult
    True

insertMessagePayloadStatement :: Statement (Text, Text) ()
insertMessagePayloadStatement = mkStatement
    "WITH payload AS (SELECT $2::jsonb AS value)\
    \ INSERT INTO harness.session_messages\
    \ (response_item_id, provider_item_id, role_name, status_name, phase,\
    \ content_kind, content_text, extra_fields)\
    \ SELECT $1::uuid, value->>'id', value->>'role', value->>'status',\
    \ value->>'phase',\
    \ CASE jsonb_typeof(value->'content')\
    \   WHEN 'string' THEN 'text' ELSE 'parts' END,\
    \ CASE jsonb_typeof(value->'content')\
    \   WHEN 'string' THEN value->>'content' ELSE NULL END,\
    \ value - ARRAY['type','id','content','role','status','phase']::text[]\
    \ FROM payload"
    itemPayloadParams
    Decoders.noResult
    True

insertFunctionCallPayloadStatement :: Statement (Text, Text) ()
insertFunctionCallPayloadStatement = mkStatement
    "WITH payload AS (SELECT $2::jsonb AS value)\
    \ INSERT INTO harness.session_function_calls\
    \ (response_item_id, provider_item_id, call_id, function_name,\
    \ arguments, status_name, extra_fields)\
    \ SELECT $1::uuid, value->>'id', value->>'call_id', value->>'name',\
    \ value->>'arguments', value->>'status',\
    \ value - ARRAY['type','id','call_id','name','arguments','status']::text[]\
    \ FROM payload"
    itemPayloadParams
    Decoders.noResult
    True

insertCustomCallPayloadStatement :: Statement (Text, Text) ()
insertCustomCallPayloadStatement = mkStatement
    "WITH payload AS (SELECT $2::jsonb AS value)\
    \ INSERT INTO harness.session_custom_tool_calls\
    \ (response_item_id, provider_item_id, call_id, tool_name,\
    \ input_text, status_name, extra_fields)\
    \ SELECT $1::uuid, value->>'id', value->>'call_id', value->>'name',\
    \ value->>'input', value->>'status',\
    \ value - ARRAY['type','id','call_id','name','input','status']::text[]\
    \ FROM payload"
    itemPayloadParams
    Decoders.noResult
    True

insertFunctionOutputPayloadStatement :: Statement (Text, Text) ()
insertFunctionOutputPayloadStatement = mkStatement
    "WITH payload AS (SELECT $2::jsonb AS value)\
    \ INSERT INTO harness.session_function_call_outputs\
    \ (response_item_id, provider_item_id, call_id, output_text,\
    \ status_name, extra_fields)\
    \ SELECT $1::uuid, value->>'id', value->>'call_id',\
    \ CASE jsonb_typeof(value->'output')\
    \   WHEN 'string' THEN value->>'output' ELSE (value->'output')::text END,\
    \ value->>'status',\
    \ value - ARRAY['type','id','call_id','output','status']::text[]\
    \ FROM payload"
    itemPayloadParams
    Decoders.noResult
    True

insertCustomOutputPayloadStatement :: Statement (Text, Text) ()
insertCustomOutputPayloadStatement = mkStatement
    "WITH payload AS (SELECT $2::jsonb AS value)\
    \ INSERT INTO harness.session_custom_tool_call_outputs\
    \ (response_item_id, provider_item_id, call_id, tool_name, output_text,\
    \ status_name, extra_fields)\
    \ SELECT $1::uuid, value->>'id', value->>'call_id', value->>'name',\
    \ CASE jsonb_typeof(value->'output')\
    \   WHEN 'string' THEN value->>'output' ELSE (value->'output')::text END,\
    \ value->>'status',\
    \ value - ARRAY['type','id','call_id','name','output','status']::text[]\
    \ FROM payload"
    itemPayloadParams
    Decoders.noResult
    True

insertReasoningPayloadStatement :: Statement (Text, Text) ()
insertReasoningPayloadStatement = mkStatement
    "WITH payload AS (SELECT $2::jsonb AS value)\
    \ INSERT INTO harness.session_reasoning_items\
    \ (response_item_id, provider_item_id, has_content, encrypted_content,\
    \ status_name, extra_fields)\
    \ SELECT $1::uuid, value->>'id', value ? 'content',\
    \ value->>'encrypted_content', value->>'status',\
    \ value - ARRAY[\
    \   'type','id','summary','content','encrypted_content','status']::text[]\
    \ FROM payload"
    itemPayloadParams
    Decoders.noResult
    True

insertSummaryPayloadStatement :: Statement (Text, Text) ()
insertSummaryPayloadStatement = mkStatement
    "WITH payload AS (SELECT $2::jsonb AS value)\
    \ INSERT INTO harness.session_reasoning_summaries\
    \ (response_item_id, part_index, part_type, text_value, extra_fields)\
    \ SELECT $1::uuid, (part.ordinality - 1)::integer,\
    \ part.value->>'type', part.value->>'text',\
    \ part.value - ARRAY['type','text']::text[]\
    \ FROM payload\
    \ CROSS JOIN LATERAL jsonb_array_elements(\
    \   COALESCE(payload.value->'summary', '[]'::jsonb))\
    \   WITH ORDINALITY AS part(value, ordinality)"
    itemPayloadParams
    Decoders.noResult
    True

insertReferencePayloadStatement :: Statement (Text, Text) ()
insertReferencePayloadStatement = mkStatement
    "WITH payload AS (SELECT $2::jsonb AS value)\
    \ INSERT INTO harness.session_item_references\
    \ (response_item_id, provider_item_id, extra_fields)\
    \ SELECT $1::uuid, value->>'id', value - ARRAY['type','id']::text[]\
    \ FROM payload"
    itemPayloadParams
    Decoders.noResult
    True

insertTaggedPayloadStatement :: Statement (Text, Text) ()
insertTaggedPayloadStatement = mkStatement
    "WITH payload AS (SELECT $2::jsonb AS value)\
    \ INSERT INTO harness.session_tagged_items\
    \ (response_item_id, wire_tag, fields)\
    \ SELECT $1::uuid, value->>'type', value - 'type'\
    \ FROM payload"
    itemPayloadParams
    Decoders.noResult
    True

insertContentPayloadStatement :: Statement (Text, Text) ()
insertContentPayloadStatement = mkStatement
    "WITH payload AS (SELECT $2::jsonb AS value)\
    \ INSERT INTO harness.session_response_content_parts\
    \ (response_item_id, part_index, part_type, text_value, refusal_text,\
    \ detail, file_data, file_id, file_url, filename, image_url,\
    \ input_audio, prompt_cache_breakpoint, annotations, logprobs,\
    \ extra_fields)\
    \ SELECT $1::uuid, (part.ordinality - 1)::integer,\
    \ part.value->>'type', part.value->>'text', part.value->>'refusal',\
    \ part.value->>'detail', part.value->>'file_data', part.value->>'file_id',\
    \ part.value->>'file_url', part.value->>'filename',\
    \ part.value->>'image_url', part.value->'input_audio',\
    \ part.value->'prompt_cache_breakpoint', part.value->'annotations',\
    \ part.value->'logprobs',\
    \ part.value - ARRAY[\
    \   'type','text','refusal','detail','file_data','file_id','file_url',\
    \   'filename','image_url','input_audio','prompt_cache_breakpoint',\
    \   'annotations','logprobs']::text[]\
    \ FROM payload\
    \ CROSS JOIN LATERAL jsonb_array_elements(\
    \   CASE WHEN jsonb_typeof(payload.value->'content') = 'array'\
    \     THEN payload.value->'content' ELSE '[]'::jsonb END)\
    \   WITH ORDINALITY AS part(value, ordinality)"
    itemPayloadParams
    Decoders.noResult
    True

itemPayloadParams :: Encoders.Params (Text, Text)
itemPayloadParams =
    (fst >$< textParam)
        <> (snd >$< textParam)

loadItemsStatement :: Statement Text [LoadedItem]
loadItemsStatement = mkStatement loadItemsSql textParam
    (Decoders.rowList $
        LoadedItem <$> textColumn <*> textColumn <*> textColumn)
    True

loadItemsSql :: Text
loadItemsSql =
    "SELECT i.representation, i.item_type,\
    \ (CASE i.storage_kind\
    \ WHEN 'message' THEN\
    \   m.extra_fields\
    \   || jsonb_build_object(\
    \     'type', 'message',\
    \     'role', m.role_name,\
    \     'content', CASE m.content_kind\
    \       WHEN 'text' THEN to_jsonb(m.content_text)\
    \       ELSE COALESCE((\
    \         SELECT jsonb_agg(\
    \           cp.extra_fields\
    \           || jsonb_build_object('type', cp.part_type)\
    \           || CASE cp.part_type\
    \             WHEN 'input_text' THEN jsonb_build_object('text', cp.text_value)\
    \             WHEN 'input_audio' THEN jsonb_build_object('input_audio', cp.input_audio)\
    \             WHEN 'output_text' THEN jsonb_build_object('text', cp.text_value)\
    \             WHEN 'refusal' THEN jsonb_build_object('refusal', cp.refusal_text)\
    \             WHEN 'reasoning_text' THEN jsonb_build_object('text', cp.text_value)\
    \             WHEN 'summary_text' THEN jsonb_build_object('text', cp.text_value)\
    \             ELSE '{}'::jsonb END\
    \           || jsonb_strip_nulls(jsonb_build_object(\
    \             'detail', cp.detail, 'file_data', cp.file_data,\
    \             'file_id', cp.file_id, 'file_url', cp.file_url,\
    \             'filename', cp.filename, 'image_url', cp.image_url,\
    \             'prompt_cache_breakpoint', cp.prompt_cache_breakpoint,\
    \             'annotations', cp.annotations, 'logprobs', cp.logprobs))\
    \           ORDER BY cp.part_index)\
    \         FROM harness.session_response_content_parts cp\
    \         WHERE cp.response_item_id = i.response_item_id\
    \       ), '[]'::jsonb) END)\
    \   || jsonb_strip_nulls(jsonb_build_object(\
    \     'id', m.provider_item_id, 'status', m.status_name, 'phase', m.phase))\
    \ WHEN 'function_call' THEN\
    \   fc.extra_fields\
    \   || jsonb_build_object('type', 'function_call',\
    \     'call_id', fc.call_id, 'name', fc.function_name,\
    \     'arguments', fc.arguments)\
    \   || jsonb_strip_nulls(jsonb_build_object(\
    \     'id', fc.provider_item_id, 'status', fc.status_name))\
    \ WHEN 'function_call_output' THEN\
    \   fo.extra_fields\
    \   || jsonb_build_object('type', 'function_call_output',\
    \     'call_id', fo.call_id, 'output', fo.output_text)\
    \   || jsonb_strip_nulls(jsonb_build_object(\
    \     'id', fo.provider_item_id, 'status', fo.status_name))\
    \ WHEN 'custom_tool_call' THEN\
    \   cc.extra_fields\
    \   || jsonb_build_object('type', 'custom_tool_call',\
    \     'call_id', cc.call_id, 'name', cc.tool_name, 'input', cc.input_text)\
    \   || jsonb_strip_nulls(jsonb_build_object(\
    \     'id', cc.provider_item_id, 'status', cc.status_name))\
    \ WHEN 'custom_tool_call_output' THEN\
    \   co.extra_fields\
    \   || jsonb_build_object('type', 'custom_tool_call_output',\
    \     'call_id', co.call_id, 'output', co.output_text)\
    \   || jsonb_strip_nulls(jsonb_build_object(\
    \     'id', co.provider_item_id, 'name', co.tool_name,\
    \     'status', co.status_name))\
    \ WHEN 'reasoning' THEN\
    \   r.extra_fields\
    \   || jsonb_build_object(\
    \     'type', 'reasoning',\
    \     'summary', COALESCE((\
    \       SELECT jsonb_agg(\
    \         rs.extra_fields || jsonb_build_object('type', rs.part_type)\
    \         || jsonb_strip_nulls(jsonb_build_object('text', rs.text_value))\
    \         ORDER BY rs.part_index)\
    \       FROM harness.session_reasoning_summaries rs\
    \       WHERE rs.response_item_id = i.response_item_id\
    \     ), '[]'::jsonb))\
    \   || CASE WHEN r.has_content THEN jsonb_build_object('content',\
    \       COALESCE((SELECT jsonb_agg(\
    \         cp.extra_fields || jsonb_build_object('type', cp.part_type)\
    \         || CASE cp.part_type\
    \           WHEN 'input_text' THEN jsonb_build_object('text', cp.text_value)\
    \           WHEN 'input_audio' THEN jsonb_build_object('input_audio', cp.input_audio)\
    \           WHEN 'output_text' THEN jsonb_build_object('text', cp.text_value)\
    \           WHEN 'refusal' THEN jsonb_build_object('refusal', cp.refusal_text)\
    \           WHEN 'reasoning_text' THEN jsonb_build_object('text', cp.text_value)\
    \           WHEN 'summary_text' THEN jsonb_build_object('text', cp.text_value)\
    \           ELSE '{}'::jsonb END\
    \         || jsonb_strip_nulls(jsonb_build_object(\
    \           'detail', cp.detail, 'file_data', cp.file_data,\
    \           'file_id', cp.file_id, 'file_url', cp.file_url,\
    \           'filename', cp.filename, 'image_url', cp.image_url,\
    \           'prompt_cache_breakpoint', cp.prompt_cache_breakpoint,\
    \           'annotations', cp.annotations, 'logprobs', cp.logprobs))\
    \         ORDER BY cp.part_index)\
    \       FROM harness.session_response_content_parts cp\
    \       WHERE cp.response_item_id = i.response_item_id), '[]'::jsonb))\
    \     ELSE '{}'::jsonb END\
    \   || jsonb_strip_nulls(jsonb_build_object(\
    \     'id', r.provider_item_id, 'encrypted_content', r.encrypted_content,\
    \     'status', r.status_name))\
    \ WHEN 'item_reference' THEN\
    \   ir.extra_fields || jsonb_build_object(\
    \     'type', 'item_reference', 'id', ir.provider_item_id)\
    \ WHEN 'tagged' THEN\
    \   ti.fields || jsonb_build_object('type', ti.wire_tag)\
    \ END)::text AS payload\
    \ FROM harness.session_response_items i\
    \ LEFT JOIN harness.session_messages m USING (response_item_id)\
    \ LEFT JOIN harness.session_function_calls fc USING (response_item_id)\
    \ LEFT JOIN harness.session_function_call_outputs fo USING (response_item_id)\
    \ LEFT JOIN harness.session_custom_tool_calls cc USING (response_item_id)\
    \ LEFT JOIN harness.session_custom_tool_call_outputs co USING (response_item_id)\
    \ LEFT JOIN harness.session_reasoning_items r USING (response_item_id)\
    \ LEFT JOIN harness.session_item_references ir USING (response_item_id)\
    \ LEFT JOIN harness.session_tagged_items ti USING (response_item_id)\
    \ WHERE i.turn_id = $1::uuid\
    \ ORDER BY i.item_index ASC"

fieldParam :: (a -> b) -> Encoders.Params b -> Encoders.Params a
fieldParam field encoder = field >$< encoder

textParam :: Encoders.Params Text
textParam = Encoders.param (Encoders.nonNullable Encoders.text)

int32Param :: Encoders.Params Int32
int32Param = Encoders.param (Encoders.nonNullable Encoders.int4)

textColumn :: Decoders.Row Text
textColumn = Decoders.column (Decoders.nonNullable Decoders.text)

textSingleResult :: Decoders.Result Text
textSingleResult = Decoders.singleRow textColumn
