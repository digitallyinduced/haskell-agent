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
import Data.Aeson (FromJSON, ToJSON, Value(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement)
import qualified Hasql.Transaction as Transaction

import Agent.Responses.Types
    ( CustomToolCall(..)
    , CustomToolCallOutput(..)
    , FunctionCall(..)
    , FunctionCallOutput(..)
    , ItemReference(..)
    , MessageContent(..)
    , ReasoningItem(..)
    , ReasoningSummaryPart(..)
    , ResponseContentPart(..)
    , ResponseItem(..)
    , ResponseMessage(..)
    , TaggedObject(..)
    , parseResponseItemType
    , responseItemTypeText
    )
import Agent.Store.Postgres.Hasql (mkStatement)

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

data MessageParams = MessageParams
    { messageItemId :: !Text
    , messageProviderId :: !(Maybe Text)
    , messageRole :: !Text
    , messageStatus :: !(Maybe Text)
    , messagePhase :: !(Maybe Text)
    , messageContentKind :: !Text
    , messageContentText :: !(Maybe Text)
    , messageExtras :: !Value
    }

data CallParams = CallParams
    { callItemId :: !Text
    , callProviderId :: !(Maybe Text)
    , callId :: !Text
    , callName :: !Text
    , callInput :: !Text
    , callStatus :: !(Maybe Text)
    , callExtras :: !Value
    }

data OutputParams = OutputParams
    { outputItemId :: !Text
    , outputProviderId :: !(Maybe Text)
    , outputCallId :: !Text
    , outputName :: !(Maybe Text)
    , outputText :: !Text
    , outputStatus :: !(Maybe Text)
    , outputExtras :: !Value
    }

data ReasoningParams = ReasoningParams
    { reasoningItemId :: !Text
    , reasoningProviderId :: !(Maybe Text)
    , reasoningHasContent :: !Bool
    , reasoningEncrypted :: !(Maybe Text)
    , reasoningStatus :: !(Maybe Text)
    , reasoningExtras :: !Value
    }

data SummaryParams = SummaryParams
    { summaryItemId :: !Text
    , summaryIndex :: !Int32
    , summaryType :: !Text
    , summaryText :: !(Maybe Text)
    , summaryExtras :: !Value
    }

data ContentParams = ContentParams
    { contentItemId :: !Text
    , contentIndex :: !Int32
    , contentType :: !Text
    , contentText :: !(Maybe Text)
    , contentRefusal :: !(Maybe Text)
    , contentDetail :: !(Maybe Text)
    , contentFileData :: !(Maybe Text)
    , contentFileId :: !(Maybe Text)
    , contentFileUrl :: !(Maybe Text)
    , contentFilename :: !(Maybe Text)
    , contentImageUrl :: !(Maybe Text)
    , contentInputAudio :: !(Maybe Value)
    , contentCacheBreakpoint :: !(Maybe Value)
    , contentAnnotations :: !(Maybe Value)
    , contentLogprobs :: !(Maybe Value)
    , contentExtras :: !Value
    }

insertResponseItems :: Text -> [ResponseItem] -> Transaction.Transaction ()
insertResponseItems turnId items =
    forM_ (zip [0..] items) \(index, item) -> do
        itemId <- Transaction.statement
            BaseParams
                { baseTurnId = turnId
                , baseIndex = fromIntegral (index :: Int)
                , baseStorageKind = storageKind item
                , baseItemType = itemType item
                , baseRepresentation = representation item
                }
            insertBaseStatement
        insertItem itemId item

storageKind :: ResponseItem -> Text
storageKind = \case
    MessageItem{} -> "message"
    FunctionCallItem{} -> "function_call"
    FunctionCallOutputItem{} -> "function_call_output"
    CustomToolCallItem{} -> "custom_tool_call"
    CustomToolCallOutputItem{} -> "custom_tool_call_output"
    ReasoningItemValue{} -> "reasoning"
    ItemReferenceValue{} -> "item_reference"
    KnownResponseItem{} -> "tagged"
    UnknownResponseItem{} -> "tagged"

itemType :: ResponseItem -> Text
itemType = \case
    MessageItem{} -> "message"
    FunctionCallItem{} -> "function_call"
    FunctionCallOutputItem{} -> "function_call_output"
    CustomToolCallItem{} -> "custom_tool_call"
    CustomToolCallOutputItem{} -> "custom_tool_call_output"
    ReasoningItemValue{} -> "reasoning"
    ItemReferenceValue{} -> "item_reference"
    KnownResponseItem kind _ -> responseItemTypeText kind
    UnknownResponseItem tagged -> tagged.tag

representation :: ResponseItem -> Text
representation = \case
    KnownResponseItem{} -> "known"
    UnknownResponseItem{} -> "unknown"
    _ -> "core"

insertItem :: Text -> ResponseItem -> Transaction.Transaction ()
insertItem itemId = \case
    MessageItem value -> do
        let (contentKind, contentText, parts) = case value.content of
                MessageContentText text -> ("text", Just text, [])
                MessageContentParts values -> ("parts", Nothing, values)
        Transaction.statement
            MessageParams
                { messageItemId = itemId
                , messageProviderId = value.messageId
                , messageRole = enumText value.role
                , messageStatus = fmap enumText value.status
                , messagePhase = value.phase
                , messageContentKind = contentKind
                , messageContentText = contentText
                , messageExtras = Object value.extraFields
                }
            insertMessageStatement
        insertContentParts itemId parts
    FunctionCallItem value ->
        Transaction.statement
            CallParams
                { callItemId = itemId
                , callProviderId = value.itemId
                , callId = value.callId
                , callName = value.name
                , callInput = value.arguments
                , callStatus = fmap enumText value.status
                , callExtras = Object value.extraFields
                }
            insertFunctionCallStatement
    FunctionCallOutputItem value ->
        Transaction.statement
            OutputParams
                { outputItemId = itemId
                , outputProviderId = value.itemId
                , outputCallId = value.callId
                , outputName = Nothing
                , outputText = renderToolOutput value.output
                , outputStatus = fmap enumText value.status
                , outputExtras = Object value.extraFields
                }
            insertFunctionOutputStatement
    CustomToolCallItem value ->
        Transaction.statement
            CallParams
                { callItemId = itemId
                , callProviderId = value.itemId
                , callId = value.callId
                , callName = value.name
                , callInput = value.input
                , callStatus = fmap enumText value.status
                , callExtras = Object value.extraFields
                }
            insertCustomCallStatement
    CustomToolCallOutputItem value ->
        Transaction.statement
            OutputParams
                { outputItemId = itemId
                , outputProviderId = value.itemId
                , outputCallId = value.callId
                , outputName = value.name
                , outputText = renderToolOutput value.output
                , outputStatus = fmap enumText value.status
                , outputExtras = Object value.extraFields
                }
            insertCustomOutputStatement
    ReasoningItemValue value -> do
        Transaction.statement
            ReasoningParams
                { reasoningItemId = itemId
                , reasoningProviderId = value.itemId
                , reasoningHasContent = maybe False (const True) value.content
                , reasoningEncrypted = value.encryptedContent
                , reasoningStatus = fmap enumText value.status
                , reasoningExtras = Object value.extraFields
                }
            insertReasoningStatement
        forM_ (zip [0..] value.summary) \(index, part) ->
            Transaction.statement
                SummaryParams
                    { summaryItemId = itemId
                    , summaryIndex = fromIntegral (index :: Int)
                    , summaryType = part.partType
                    , summaryText = part.text
                    , summaryExtras = Object part.extraFields
                    }
                insertSummaryStatement
        insertContentParts itemId (maybe [] id value.content)
    ItemReferenceValue value ->
        Transaction.statement
            (itemId, value.itemId, Object value.extraFields)
            insertReferenceStatement
    KnownResponseItem _ tagged ->
        Transaction.statement
            (itemId, tagged.tag, Object tagged.fields)
            insertTaggedStatement
    UnknownResponseItem tagged ->
        Transaction.statement
            (itemId, tagged.tag, Object tagged.fields)
            insertTaggedStatement

insertContentParts :: Text -> [ResponseContentPart] -> Transaction.Transaction ()
insertContentParts itemId parts =
    forM_ (zip [0..] parts) \(index, part) ->
        Transaction.statement
            (contentParams itemId (fromIntegral (index :: Int)) part)
            insertContentStatement

contentParams :: Text -> Int32 -> ResponseContentPart -> ContentParams
contentParams itemId index = \case
    InputTextPart { text, promptCacheBreakpoint, extraFields } ->
        (emptyContent itemId index "input_text")
            { contentText = Just text
            , contentCacheBreakpoint = promptCacheBreakpoint
            , contentExtras = Object extraFields
            }
    InputImagePart
        { detail, fileId, imageUrl, promptCacheBreakpoint, extraFields } ->
        (emptyContent itemId index "input_image")
            { contentDetail = detail
            , contentFileId = fileId
            , contentImageUrl = imageUrl
            , contentCacheBreakpoint = promptCacheBreakpoint
            , contentExtras = Object extraFields
            }
    InputFilePart
        { detail, fileData, fileId, fileUrl, filename
        , promptCacheBreakpoint, extraFields
        } ->
        (emptyContent itemId index "input_file")
            { contentDetail = detail
            , contentFileData = fileData
            , contentFileId = fileId
            , contentFileUrl = fileUrl
            , contentFilename = filename
            , contentCacheBreakpoint = promptCacheBreakpoint
            , contentExtras = Object extraFields
            }
    InputAudioPart { inputAudio, extraFields } ->
        (emptyContent itemId index "input_audio")
            { contentInputAudio = Just inputAudio
            , contentExtras = Object extraFields
            }
    OutputTextPart { text, annotations, logprobs, extraFields } ->
        (emptyContent itemId index "output_text")
            { contentText = Just text
            , contentAnnotations = fmap Aeson.toJSON annotations
            , contentLogprobs = fmap Aeson.toJSON logprobs
            , contentExtras = Object extraFields
            }
    RefusalPart { refusal, extraFields } ->
        (emptyContent itemId index "refusal")
            { contentRefusal = Just refusal
            , contentExtras = Object extraFields
            }
    ReasoningTextPart { text, extraFields } ->
        (emptyContent itemId index "reasoning_text")
            { contentText = Just text
            , contentExtras = Object extraFields
            }
    SummaryTextPart { text, extraFields } ->
        (emptyContent itemId index "summary_text")
            { contentText = Just text
            , contentExtras = Object extraFields
            }
    UnknownContentPart tagged ->
        (emptyContent itemId index tagged.tag)
            { contentExtras = Object tagged.fields }

emptyContent :: Text -> Int32 -> Text -> ContentParams
emptyContent itemId index kind = ContentParams
    { contentItemId = itemId
    , contentIndex = index
    , contentType = kind
    , contentText = Nothing
    , contentRefusal = Nothing
    , contentDetail = Nothing
    , contentFileData = Nothing
    , contentFileId = Nothing
    , contentFileUrl = Nothing
    , contentFilename = Nothing
    , contentImageUrl = Nothing
    , contentInputAudio = Nothing
    , contentCacheBreakpoint = Nothing
    , contentAnnotations = Nothing
    , contentLogprobs = Nothing
    , contentExtras = Object KeyMap.empty
    }

data LoadedItem = LoadedItem
    { loadedRepresentation :: !Text
    , loadedItemType :: !Text
    , loadedValue :: !Value
    }

loadResponseItems
    :: Text
    -> Transaction.Transaction (Either Text [ResponseItem])
loadResponseItems turnId = do
    rows <- Transaction.statement turnId loadItemsStatement
    pure (traverse decodeLoaded rows)

decodeLoaded :: LoadedItem -> Either Text ResponseItem
decodeLoaded row =
    case row.loadedRepresentation of
        "core" -> decodeValue "response item" row.loadedValue
        "known" -> do
            tagged <- decodeValue "known tagged response item" row.loadedValue
            pure $ KnownResponseItem
                (parseResponseItemType row.loadedItemType)
                tagged
        "unknown" ->
            UnknownResponseItem
                <$> decodeValue "unknown tagged response item" row.loadedValue
        value -> Left ("unknown response item representation: " <> value)

decodeValue :: FromJSON a => Text -> Value -> Either Text a
decodeValue label value =
    case Aeson.fromJSON value of
        Aeson.Success result -> Right result
        Aeson.Error err -> Left (label <> ": " <> Text.pack err)

enumText :: ToJSON a => a -> Text
enumText value = case Aeson.toJSON value of
    String text -> text
    _ -> ""

-- Tool execution produces text. Keep compatibility with older/provider wire
-- values by storing their JSON rendering rather than retaining a JSONB column.
renderToolOutput :: Value -> Text
renderToolOutput = \case
    String text -> text
    value ->
        TextEncoding.decodeUtf8
            (LazyByteString.toStrict (Aeson.encode value))

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

insertMessageStatement :: Statement MessageParams ()
insertMessageStatement = mkStatement
    "INSERT INTO harness.session_messages\
    \ (response_item_id, provider_item_id, role_name, status_name, phase,\
    \ content_kind, content_text, extra_fields)\
    \ VALUES ($1::uuid, $2, $3, $4, $5, $6, $7, $8)"
    ( fieldParam (.messageItemId) textParam
        <> fieldParam (.messageProviderId) nullableTextParam
        <> fieldParam (.messageRole) textParam
        <> fieldParam (.messageStatus) nullableTextParam
        <> fieldParam (.messagePhase) nullableTextParam
        <> fieldParam (.messageContentKind) textParam
        <> fieldParam (.messageContentText) nullableTextParam
        <> fieldParam (.messageExtras) jsonbParam
    )
    Decoders.noResult
    True

insertFunctionCallStatement :: Statement CallParams ()
insertFunctionCallStatement = mkStatement
    "INSERT INTO harness.session_function_calls\
    \ (response_item_id, provider_item_id, call_id, function_name,\
    \ arguments, status_name, extra_fields)\
    \ VALUES ($1::uuid, $2, $3, $4, $5, $6, $7)"
    callEncoder
    Decoders.noResult
    True

insertCustomCallStatement :: Statement CallParams ()
insertCustomCallStatement = mkStatement
    "INSERT INTO harness.session_custom_tool_calls\
    \ (response_item_id, provider_item_id, call_id, tool_name,\
    \ input_text, status_name, extra_fields)\
    \ VALUES ($1::uuid, $2, $3, $4, $5, $6, $7)"
    callEncoder
    Decoders.noResult
    True

callEncoder :: Encoders.Params CallParams
callEncoder =
    fieldParam (.callItemId) textParam
        <> fieldParam (.callProviderId) nullableTextParam
        <> fieldParam (.callId) textParam
        <> fieldParam (.callName) textParam
        <> fieldParam (.callInput) textParam
        <> fieldParam (.callStatus) nullableTextParam
        <> fieldParam (.callExtras) jsonbParam

insertFunctionOutputStatement :: Statement OutputParams ()
insertFunctionOutputStatement = mkStatement
    "INSERT INTO harness.session_function_call_outputs\
    \ (response_item_id, provider_item_id, call_id, output_text,\
    \ status_name, extra_fields)\
    \ VALUES ($1::uuid, $2, $3, $4, $5, $6)"
    ( fieldParam (.outputItemId) textParam
        <> fieldParam (.outputProviderId) nullableTextParam
        <> fieldParam (.outputCallId) textParam
        <> fieldParam (.outputText) textParam
        <> fieldParam (.outputStatus) nullableTextParam
        <> fieldParam (.outputExtras) jsonbParam
    )
    Decoders.noResult
    True

insertCustomOutputStatement :: Statement OutputParams ()
insertCustomOutputStatement = mkStatement
    "INSERT INTO harness.session_custom_tool_call_outputs\
    \ (response_item_id, provider_item_id, call_id, tool_name, output_text,\
    \ status_name, extra_fields)\
    \ VALUES ($1::uuid, $2, $3, $4, $5, $6, $7)"
    outputEncoder
    Decoders.noResult
    True

outputEncoder :: Encoders.Params OutputParams
outputEncoder =
    fieldParam (.outputItemId) textParam
        <> fieldParam (.outputProviderId) nullableTextParam
        <> fieldParam (.outputCallId) textParam
        <> fieldParam (.outputName) nullableTextParam
        <> fieldParam (.outputText) textParam
        <> fieldParam (.outputStatus) nullableTextParam
        <> fieldParam (.outputExtras) jsonbParam

insertReasoningStatement :: Statement ReasoningParams ()
insertReasoningStatement = mkStatement
    "INSERT INTO harness.session_reasoning_items\
    \ (response_item_id, provider_item_id, has_content, encrypted_content,\
    \ status_name, extra_fields)\
    \ VALUES ($1::uuid, $2, $3, $4, $5, $6)"
    ( fieldParam (.reasoningItemId) textParam
        <> fieldParam (.reasoningProviderId) nullableTextParam
        <> fieldParam (.reasoningHasContent) boolParam
        <> fieldParam (.reasoningEncrypted) nullableTextParam
        <> fieldParam (.reasoningStatus) nullableTextParam
        <> fieldParam (.reasoningExtras) jsonbParam
    )
    Decoders.noResult
    True

insertSummaryStatement :: Statement SummaryParams ()
insertSummaryStatement = mkStatement
    "INSERT INTO harness.session_reasoning_summaries\
    \ (response_item_id, part_index, part_type, text_value, extra_fields)\
    \ VALUES ($1::uuid, $2, $3, $4, $5)"
    ( fieldParam (.summaryItemId) textParam
        <> fieldParam (.summaryIndex) int32Param
        <> fieldParam (.summaryType) textParam
        <> fieldParam (.summaryText) nullableTextParam
        <> fieldParam (.summaryExtras) jsonbParam
    )
    Decoders.noResult
    True

insertReferenceStatement :: Statement (Text, Text, Value) ()
insertReferenceStatement = mkStatement
    "INSERT INTO harness.session_item_references\
    \ (response_item_id, provider_item_id, extra_fields)\
    \ VALUES ($1::uuid, $2, $3)"
    ( ((\(a, _, _) -> a) >$< textParam)
        <> ((\(_, b, _) -> b) >$< textParam)
        <> ((\(_, _, c) -> c) >$< jsonbParam)
    )
    Decoders.noResult
    True

insertTaggedStatement :: Statement (Text, Text, Value) ()
insertTaggedStatement = mkStatement
    "INSERT INTO harness.session_tagged_items\
    \ (response_item_id, wire_tag, fields)\
    \ VALUES ($1::uuid, $2, $3)"
    ( ((\(a, _, _) -> a) >$< textParam)
        <> ((\(_, b, _) -> b) >$< textParam)
        <> ((\(_, _, c) -> c) >$< jsonbParam)
    )
    Decoders.noResult
    True

insertContentStatement :: Statement ContentParams ()
insertContentStatement = mkStatement
    "INSERT INTO harness.session_response_content_parts\
    \ (response_item_id, part_index, part_type, text_value, refusal_text,\
    \ detail, file_data, file_id, file_url, filename, image_url,\
    \ input_audio, prompt_cache_breakpoint, annotations, logprobs,\
    \ extra_fields)\
    \ VALUES ($1::uuid, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11,\
    \ $12, $13, $14, $15, $16)"
    ( fieldParam (.contentItemId) textParam
        <> fieldParam (.contentIndex) int32Param
        <> fieldParam (.contentType) textParam
        <> fieldParam (.contentText) nullableTextParam
        <> fieldParam (.contentRefusal) nullableTextParam
        <> fieldParam (.contentDetail) nullableTextParam
        <> fieldParam (.contentFileData) nullableTextParam
        <> fieldParam (.contentFileId) nullableTextParam
        <> fieldParam (.contentFileUrl) nullableTextParam
        <> fieldParam (.contentFilename) nullableTextParam
        <> fieldParam (.contentImageUrl) nullableTextParam
        <> fieldParam (.contentInputAudio) nullableJsonbParam
        <> fieldParam (.contentCacheBreakpoint) nullableJsonbParam
        <> fieldParam (.contentAnnotations) nullableJsonbParam
        <> fieldParam (.contentLogprobs) nullableJsonbParam
        <> fieldParam (.contentExtras) jsonbParam
    )
    Decoders.noResult
    True

loadItemsStatement :: Statement Text [LoadedItem]
loadItemsStatement = mkStatement loadItemsSql textParam
    (Decoders.rowList $
        LoadedItem <$> textColumn <*> textColumn <*> jsonbColumn)
    True

loadItemsSql :: Text
loadItemsSql =
    "SELECT i.representation, i.item_type,\
    \ CASE i.storage_kind\
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
    \ END AS payload\
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

nullableTextParam :: Encoders.Params (Maybe Text)
nullableTextParam = Encoders.param (Encoders.nullable Encoders.text)

int32Param :: Encoders.Params Int32
int32Param = Encoders.param (Encoders.nonNullable Encoders.int4)

boolParam :: Encoders.Params Bool
boolParam = Encoders.param (Encoders.nonNullable Encoders.bool)

jsonbParam :: Encoders.Params Value
jsonbParam = Encoders.param (Encoders.nonNullable Encoders.jsonb)

nullableJsonbParam :: Encoders.Params (Maybe Value)
nullableJsonbParam = Encoders.param (Encoders.nullable Encoders.jsonb)

textColumn :: Decoders.Row Text
textColumn = Decoders.column (Decoders.nonNullable Decoders.text)

jsonbColumn :: Decoders.Row Value
jsonbColumn = Decoders.column (Decoders.nonNullable Decoders.jsonb)

textSingleResult :: Decoders.Result Text
textSingleResult = Decoders.singleRow textColumn
