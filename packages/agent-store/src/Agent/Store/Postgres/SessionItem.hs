{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
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

import Control.Monad (forM, forM_)
import qualified Data.ByteString as ByteString
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int32)
import Data.Text (Text)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement)
import qualified Hasql.Transaction as Transaction

import Agent.Store.Postgres.Hasql (mkStatement)
import Agent.Store.SessionItem

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
      \ input_audio_text text,\
      \ prompt_cache_breakpoint_text text,\
      \ annotations_text text,\
      \ logprobs_text text,\
      \ extra_fields_text text NOT NULL DEFAULT '{}',\
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
    { messageResponseItemId :: !Text
    , messageProviderItemId :: !(Maybe Text)
    , messageRole :: !Text
    , messageStatus :: !(Maybe Text)
    , messagePhase :: !(Maybe Text)
    , messageContentKind :: !Text
    , messageContentText :: !(Maybe Text)
    , messageExtraFields :: !Text
    }

data FunctionCallParams = FunctionCallParams
    { functionCallResponseItemId :: !Text
    , functionCallProviderItemId :: !(Maybe Text)
    , functionCallCallId :: !Text
    , functionCallName :: !Text
    , functionCallArguments :: !Text
    , functionCallStatus :: !(Maybe Text)
    , functionCallExtraFields :: !Text
    }

data FunctionOutputParams = FunctionOutputParams
    { functionOutputResponseItemId :: !Text
    , functionOutputProviderItemId :: !(Maybe Text)
    , functionOutputCallId :: !Text
    , functionOutputKind :: !Text
    , functionOutputText :: !Text
    , functionOutputStatus :: !(Maybe Text)
    , functionOutputExtraFields :: !Text
    }

data CustomCallParams = CustomCallParams
    { customCallResponseItemId :: !Text
    , customCallProviderItemId :: !(Maybe Text)
    , customCallCallId :: !Text
    , customCallName :: !Text
    , customCallInput :: !Text
    , customCallStatus :: !(Maybe Text)
    , customCallExtraFields :: !Text
    }

data CustomOutputParams = CustomOutputParams
    { customOutputResponseItemId :: !Text
    , customOutputProviderItemId :: !(Maybe Text)
    , customOutputCallId :: !Text
    , customOutputName :: !(Maybe Text)
    , customOutputKind :: !Text
    , customOutputText :: !Text
    , customOutputStatus :: !(Maybe Text)
    , customOutputExtraFields :: !Text
    }

data ReasoningParams = ReasoningParams
    { reasoningResponseItemId :: !Text
    , reasoningProviderItemId :: !(Maybe Text)
    , reasoningHasContent :: !Bool
    , reasoningEncryptedContent :: !(Maybe Text)
    , reasoningStatus :: !(Maybe Text)
    , reasoningExtraFields :: !Text
    }

data SummaryParams = SummaryParams
    { summaryResponseItemId :: !Text
    , summaryIndex :: !Int32
    , summaryType :: !Text
    , summaryText :: !(Maybe Text)
    , summaryExtraFields :: !Text
    }

data ReferenceParams = ReferenceParams
    { referenceResponseItemId :: !Text
    , referenceProviderItemId :: !Text
    , referenceExtraFields :: !Text
    }

data TaggedParams = TaggedParams
    { taggedResponseItemId :: !Text
    , taggedWireTag :: !Text
    , taggedFields :: !Text
    }

data ContentPartParams = ContentPartParams
    { contentPartResponseItemId :: !Text
    , contentPartIndex :: !Int32
    , contentPartType :: !Text
    , contentPartText :: !(Maybe Text)
    , contentPartRefusal :: !(Maybe Text)
    , contentPartDetail :: !(Maybe Text)
    , contentPartFileData :: !(Maybe Text)
    , contentPartFileId :: !(Maybe Text)
    , contentPartFileUrl :: !(Maybe Text)
    , contentPartFilename :: !(Maybe Text)
    , contentPartImageUrl :: !(Maybe Text)
    , contentPartInputAudio :: !(Maybe Text)
    , contentPartPromptCacheBreakpoint :: !(Maybe Text)
    , contentPartAnnotations :: !(Maybe Text)
    , contentPartLogprobs :: !(Maybe Text)
    , contentPartExtraFields :: !Text
    }

insertResponseItems
    :: Text
    -> [StoredResponseItem]
    -> Transaction.Transaction ()
insertResponseItems turnId items =
    forM_ (zip [0..] items) \(index, item) -> do
        itemId <- Transaction.statement
            BaseParams
                { baseTurnId = turnId
                , baseIndex = fromIntegral (index :: Int)
                , baseStorageKind = storageKind item
                , baseItemType = storedResponseItemType item
                , baseRepresentation =
                    representationText
                        (storedResponseItemRepresentation item)
                }
            insertBaseStatement
        insertItem itemId item

insertItem :: Text -> StoredResponseItem -> Transaction.Transaction ()
insertItem itemId = \case
    StoredMessageItem message -> do
        let (contentKind, contentText, contentParts) =
                case message.storedMessageContent of
                    StoredMessageText value -> ("text", Just value, [])
                    StoredMessageParts parts -> ("parts", Nothing, parts)
        Transaction.statement
            MessageParams
                { messageResponseItemId = itemId
                , messageProviderItemId =
                    message.storedMessageProviderItemId
                , messageRole = message.storedMessageRole
                , messageStatus = message.storedMessageStatus
                , messagePhase = message.storedMessagePhase
                , messageContentKind = contentKind
                , messageContentText = contentText
                , messageExtraFields =
                    message.storedMessageExtraFields.storedOpaqueObjectText
                }
            insertMessageStatement
        insertContentParts itemId contentParts
    StoredFunctionCallItem call ->
        Transaction.statement
            FunctionCallParams
                { functionCallResponseItemId = itemId
                , functionCallProviderItemId =
                    call.storedFunctionCallProviderItemId
                , functionCallCallId = call.storedFunctionCallCallId
                , functionCallName = call.storedFunctionCallName
                , functionCallArguments = call.storedFunctionCallArguments
                , functionCallStatus = call.storedFunctionCallStatus
                , functionCallExtraFields =
                    call.storedFunctionCallExtraFields.storedOpaqueObjectText
                }
            insertFunctionCallStatement
    StoredFunctionCallOutputItem output ->
        let value = output.storedFunctionCallOutputValue
        in Transaction.statement
            FunctionOutputParams
                { functionOutputResponseItemId = itemId
                , functionOutputProviderItemId =
                    output.storedFunctionCallOutputProviderItemId
                , functionOutputCallId =
                    output.storedFunctionCallOutputCallId
                , functionOutputKind =
                    toolOutputKindText value.storedToolOutputKind
                , functionOutputText = value.storedToolOutputText
                , functionOutputStatus =
                    output.storedFunctionCallOutputStatus
                , functionOutputExtraFields =
                    opaqueObjectText
                        output.storedFunctionCallOutputExtraFields
                }
            insertFunctionOutputStatement
    StoredCustomToolCallItem call ->
        Transaction.statement
            CustomCallParams
                { customCallResponseItemId = itemId
                , customCallProviderItemId =
                    call.storedCustomToolCallProviderItemId
                , customCallCallId = call.storedCustomToolCallCallId
                , customCallName = call.storedCustomToolCallName
                , customCallInput = call.storedCustomToolCallInput
                , customCallStatus = call.storedCustomToolCallStatus
                , customCallExtraFields =
                    call.storedCustomToolCallExtraFields.storedOpaqueObjectText
                }
            insertCustomCallStatement
    StoredCustomToolCallOutputItem output ->
        let value = output.storedCustomToolCallOutputValue
        in Transaction.statement
            CustomOutputParams
                { customOutputResponseItemId = itemId
                , customOutputProviderItemId =
                    output.storedCustomToolCallOutputProviderItemId
                , customOutputCallId =
                    output.storedCustomToolCallOutputCallId
                , customOutputName = output.storedCustomToolCallOutputName
                , customOutputKind =
                    toolOutputKindText value.storedToolOutputKind
                , customOutputText = value.storedToolOutputText
                , customOutputStatus =
                    output.storedCustomToolCallOutputStatus
                , customOutputExtraFields =
                    opaqueObjectText
                        output.storedCustomToolCallOutputExtraFields
                }
            insertCustomOutputStatement
    StoredReasoningItem reasoning -> do
        Transaction.statement
            ReasoningParams
                { reasoningResponseItemId = itemId
                , reasoningProviderItemId =
                    reasoning.storedReasoningProviderItemId
                , reasoningHasContent =
                    case reasoning.storedReasoningContent of
                        Nothing -> False
                        Just _ -> True
                , reasoningEncryptedContent =
                    reasoning.storedReasoningEncryptedContent
                , reasoningStatus = reasoning.storedReasoningStatus
                , reasoningExtraFields =
                    reasoning.storedReasoningExtraFields.storedOpaqueObjectText
                }
            insertReasoningStatement
        insertSummaries itemId reasoning.storedReasoningSummary
        forM_ reasoning.storedReasoningContent (insertContentParts itemId)
    StoredItemReferenceItem reference ->
        Transaction.statement
            ReferenceParams
                { referenceResponseItemId = itemId
                , referenceProviderItemId =
                    reference.storedItemReferenceProviderItemId
                , referenceExtraFields =
                    opaqueObjectText reference.storedItemReferenceExtraFields
                }
            insertReferenceStatement
    StoredTaggedResponseItem tagged ->
        Transaction.statement
            TaggedParams
                { taggedResponseItemId = itemId
                , taggedWireTag = tagged.storedTaggedItemWireTag
                , taggedFields =
                    tagged.storedTaggedItemFields.storedOpaqueObjectText
                }
            insertTaggedStatement

insertSummaries
    :: Text
    -> [StoredReasoningSummaryPart]
    -> Transaction.Transaction ()
insertSummaries itemId parts =
    forM_ (zip [0..] parts) \(index, part) ->
        Transaction.statement
            SummaryParams
                { summaryResponseItemId = itemId
                , summaryIndex = fromIntegral (index :: Int)
                , summaryType = part.storedReasoningSummaryPartType
                , summaryText = part.storedReasoningSummaryPartText
                , summaryExtraFields =
                    opaqueObjectText
                        part.storedReasoningSummaryPartExtraFields
                }
            insertSummaryStatement

insertContentParts
    :: Text
    -> [StoredContentPart]
    -> Transaction.Transaction ()
insertContentParts itemId parts =
    forM_ (zip [0..] parts) \(index, part) ->
        Transaction.statement
            ContentPartParams
                { contentPartResponseItemId = itemId
                , contentPartIndex = fromIntegral (index :: Int)
                , contentPartType = part.storedContentPartType
                , contentPartText = part.storedContentPartText
                , contentPartRefusal = part.storedContentPartRefusal
                , contentPartDetail = part.storedContentPartDetail
                , contentPartFileData = part.storedContentPartFileData
                , contentPartFileId = part.storedContentPartFileId
                , contentPartFileUrl = part.storedContentPartFileUrl
                , contentPartFilename = part.storedContentPartFilename
                , contentPartImageUrl = part.storedContentPartImageUrl
                , contentPartInputAudio =
                    opaqueValueText <$> part.storedContentPartInputAudio
                , contentPartPromptCacheBreakpoint =
                    opaqueValueText
                        <$> part.storedContentPartPromptCacheBreakpoint
                , contentPartAnnotations =
                    opaqueValueText <$> part.storedContentPartAnnotations
                , contentPartLogprobs =
                    opaqueValueText <$> part.storedContentPartLogprobs
                , contentPartExtraFields =
                    part.storedContentPartExtraFields.storedOpaqueObjectText
                }
            insertContentPartStatement

storageKind :: StoredResponseItem -> Text
storageKind = \case
    StoredMessageItem{} -> "message"
    StoredFunctionCallItem{} -> "function_call"
    StoredFunctionCallOutputItem{} -> "function_call_output"
    StoredCustomToolCallItem{} -> "custom_tool_call"
    StoredCustomToolCallOutputItem{} -> "custom_tool_call_output"
    StoredReasoningItem{} -> "reasoning"
    StoredItemReferenceItem{} -> "item_reference"
    StoredTaggedResponseItem{} -> "tagged"

representationText :: StoredResponseItemRepresentation -> Text
representationText = \case
    StoredCoreRepresentation -> "core"
    StoredKnownRepresentation -> "known"
    StoredUnknownRepresentation -> "unknown"

representationFromText :: Text -> Either Text StoredResponseItemRepresentation
representationFromText = \case
    "core" -> Right StoredCoreRepresentation
    "known" -> Right StoredKnownRepresentation
    "unknown" -> Right StoredUnknownRepresentation
    value -> Left ("unknown response item representation: " <> value)

toolOutputKindText :: StoredToolOutputKind -> Text
toolOutputKindText = \case
    StoredToolOutputText -> "text"
    StoredToolOutputEncoded -> "encoded"

toolOutputKindFromText :: Text -> Either Text StoredToolOutputKind
toolOutputKindFromText = \case
    "text" -> Right StoredToolOutputText
    "encoded" -> Right StoredToolOutputEncoded
    value -> Left ("unknown stored tool output kind: " <> value)

opaqueValueText :: StoredOpaqueValue -> Text
opaqueValueText value = value.storedOpaqueValueText

opaqueObjectText :: StoredOpaqueObject -> Text
opaqueObjectText value = value.storedOpaqueObjectText

data BaseRow = BaseRow
    { baseRowId :: !Text
    , baseRowStorageKind :: !Text
    , baseRowItemType :: !Text
    , baseRowRepresentation :: !Text
    }

data MessageRow = MessageRow
    { messageRowProviderItemId :: !(Maybe Text)
    , messageRowRole :: !Text
    , messageRowStatus :: !(Maybe Text)
    , messageRowPhase :: !(Maybe Text)
    , messageRowContentKind :: !Text
    , messageRowContentText :: !(Maybe Text)
    , messageRowExtraFields :: !Text
    }

data FunctionOutputRow = FunctionOutputRow
    { functionOutputRowProviderItemId :: !(Maybe Text)
    , functionOutputRowCallId :: !Text
    , functionOutputRowKind :: !Text
    , functionOutputRowText :: !Text
    , functionOutputRowStatus :: !(Maybe Text)
    , functionOutputRowExtraFields :: !Text
    }

data CustomOutputRow = CustomOutputRow
    { customOutputRowProviderItemId :: !(Maybe Text)
    , customOutputRowCallId :: !Text
    , customOutputRowName :: !(Maybe Text)
    , customOutputRowKind :: !Text
    , customOutputRowText :: !Text
    , customOutputRowStatus :: !(Maybe Text)
    , customOutputRowExtraFields :: !Text
    }

data ReasoningRow = ReasoningRow
    { reasoningRowProviderItemId :: !(Maybe Text)
    , reasoningRowHasContent :: !Bool
    , reasoningRowEncryptedContent :: !(Maybe Text)
    , reasoningRowStatus :: !(Maybe Text)
    , reasoningRowExtraFields :: !Text
    }

data TaggedRow = TaggedRow
    { taggedRowWireTag :: !Text
    , taggedRowFields :: !Text
    }

loadResponseItems
    :: Text
    -> Transaction.Transaction (Either Text [StoredResponseItem])
loadResponseItems turnId = do
    rows <- Transaction.statement turnId loadBaseRowsStatement
    sequence <$> forM rows loadResponseItem

loadResponseItem
    :: BaseRow
    -> Transaction.Transaction (Either Text StoredResponseItem)
loadResponseItem base =
    case base.baseRowStorageKind of
        "message" -> loadMessage base
        "function_call" -> loadFunctionCall base
        "function_call_output" -> loadFunctionOutput base
        "custom_tool_call" -> loadCustomCall base
        "custom_tool_call_output" -> loadCustomOutput base
        "reasoning" -> loadReasoning base
        "item_reference" -> loadItemReference base
        "tagged" -> loadTagged base
        value ->
            pure (Left ("unknown response item storage kind: " <> value))

loadMessage
    :: BaseRow
    -> Transaction.Transaction (Either Text StoredResponseItem)
loadMessage base =
    case validateCoreBase "message" base of
        Left err -> pure (Left err)
        Right () -> do
            stored <- Transaction.statement
                base.baseRowId
                loadMessageStatement
            case stored of
                Nothing -> pure (missingChild "message" base)
                Just row -> case row.messageRowContentKind of
                    "text" ->
                        pure do
                            content <- maybe
                                (Left "stored text message has no content text")
                                (Right . StoredMessageText)
                                row.messageRowContentText
                            Right $ StoredMessageItem $
                                messageFromRow row content
                    "parts" -> do
                        parts <- Transaction.statement
                            base.baseRowId
                            loadContentPartsStatement
                        pure $ Right $ StoredMessageItem $
                            messageFromRow row (StoredMessageParts parts)
                    value ->
                        pure $ Left $
                            "unknown stored message content kind: " <> value

messageFromRow :: MessageRow -> StoredMessageContent -> StoredMessage
messageFromRow row content = StoredMessage
    { storedMessageProviderItemId = row.messageRowProviderItemId
    , storedMessageContent = content
    , storedMessageRole = row.messageRowRole
    , storedMessageStatus = row.messageRowStatus
    , storedMessagePhase = row.messageRowPhase
    , storedMessageExtraFields =
        StoredOpaqueObject row.messageRowExtraFields
    }

loadFunctionCall
    :: BaseRow
    -> Transaction.Transaction (Either Text StoredResponseItem)
loadFunctionCall base =
    loadCoreChild
        "function_call"
        base
        loadFunctionCallStatement
        StoredFunctionCallItem

loadFunctionOutput
    :: BaseRow
    -> Transaction.Transaction (Either Text StoredResponseItem)
loadFunctionOutput base =
    case validateCoreBase "function_call_output" base of
        Left err -> pure (Left err)
        Right () -> do
            stored <- Transaction.statement
                base.baseRowId
                loadFunctionOutputStatement
            pure do
                row <- maybe
                    (missingChild "function_call_output" base)
                    Right
                    stored
                kind <- toolOutputKindFromText row.functionOutputRowKind
                Right $ StoredFunctionCallOutputItem StoredFunctionCallOutput
                    { storedFunctionCallOutputProviderItemId =
                        row.functionOutputRowProviderItemId
                    , storedFunctionCallOutputCallId =
                        row.functionOutputRowCallId
                    , storedFunctionCallOutputValue = StoredToolOutput
                        { storedToolOutputKind = kind
                        , storedToolOutputText = row.functionOutputRowText
                        }
                    , storedFunctionCallOutputStatus =
                        row.functionOutputRowStatus
                    , storedFunctionCallOutputExtraFields =
                        StoredOpaqueObject row.functionOutputRowExtraFields
                    }

loadCustomCall
    :: BaseRow
    -> Transaction.Transaction (Either Text StoredResponseItem)
loadCustomCall base =
    loadCoreChild
        "custom_tool_call"
        base
        loadCustomCallStatement
        StoredCustomToolCallItem

loadCustomOutput
    :: BaseRow
    -> Transaction.Transaction (Either Text StoredResponseItem)
loadCustomOutput base =
    case validateCoreBase "custom_tool_call_output" base of
        Left err -> pure (Left err)
        Right () -> do
            stored <- Transaction.statement
                base.baseRowId
                loadCustomOutputStatement
            pure do
                row <- maybe
                    (missingChild "custom_tool_call_output" base)
                    Right
                    stored
                kind <- toolOutputKindFromText row.customOutputRowKind
                Right $
                    StoredCustomToolCallOutputItem StoredCustomToolCallOutput
                        { storedCustomToolCallOutputProviderItemId =
                            row.customOutputRowProviderItemId
                        , storedCustomToolCallOutputCallId =
                            row.customOutputRowCallId
                        , storedCustomToolCallOutputName =
                            row.customOutputRowName
                        , storedCustomToolCallOutputValue = StoredToolOutput
                            { storedToolOutputKind = kind
                            , storedToolOutputText = row.customOutputRowText
                            }
                        , storedCustomToolCallOutputStatus =
                            row.customOutputRowStatus
                        , storedCustomToolCallOutputExtraFields =
                            StoredOpaqueObject row.customOutputRowExtraFields
                        }

loadReasoning
    :: BaseRow
    -> Transaction.Transaction (Either Text StoredResponseItem)
loadReasoning base =
    case validateCoreBase "reasoning" base of
        Left err -> pure (Left err)
        Right () -> do
            stored <- Transaction.statement
                base.baseRowId
                loadReasoningStatement
            case stored of
                Nothing -> pure (missingChild "reasoning" base)
                Just row -> do
                    summaries <- Transaction.statement
                        base.baseRowId
                        loadSummariesStatement
                    content <-
                        if row.reasoningRowHasContent
                            then Just <$> Transaction.statement
                                base.baseRowId
                                loadContentPartsStatement
                            else pure Nothing
                    pure $ Right $ StoredReasoningItem StoredReasoning
                        { storedReasoningProviderItemId =
                            row.reasoningRowProviderItemId
                        , storedReasoningSummary = summaries
                        , storedReasoningContent = content
                        , storedReasoningEncryptedContent =
                            row.reasoningRowEncryptedContent
                        , storedReasoningStatus = row.reasoningRowStatus
                        , storedReasoningExtraFields =
                            StoredOpaqueObject row.reasoningRowExtraFields
                        }

loadItemReference
    :: BaseRow
    -> Transaction.Transaction (Either Text StoredResponseItem)
loadItemReference base =
    loadCoreChild
        "item_reference"
        base
        loadReferenceStatement
        StoredItemReferenceItem

loadTagged
    :: BaseRow
    -> Transaction.Transaction (Either Text StoredResponseItem)
loadTagged base = do
    stored <- Transaction.statement base.baseRowId loadTaggedStatement
    pure do
        representation <-
            representationFromText base.baseRowRepresentation
        case representation of
            StoredCoreRepresentation ->
                Left "tagged response item has a core representation"
            StoredKnownRepresentation -> Right ()
            StoredUnknownRepresentation -> Right ()
        row <- maybe (missingChild "tagged" base) Right stored
        Right $ StoredTaggedResponseItem StoredTaggedItem
            { storedTaggedItemRepresentation = representation
            , storedTaggedItemWireTag = row.taggedRowWireTag
            , storedTaggedItemFields =
                StoredOpaqueObject row.taggedRowFields
            }

loadCoreChild
    :: Text
    -> BaseRow
    -> Statement Text (Maybe a)
    -> (a -> StoredResponseItem)
    -> Transaction.Transaction (Either Text StoredResponseItem)
loadCoreChild expectedType base statement wrap =
    case validateCoreBase expectedType base of
        Left err -> pure (Left err)
        Right () -> do
            stored <- Transaction.statement base.baseRowId statement
            pure $
                maybe
                    (missingChild expectedType base)
                    (Right . wrap)
                    stored

validateCoreBase :: Text -> BaseRow -> Either Text ()
validateCoreBase expectedType base = do
    if base.baseRowItemType == expectedType
        then Right ()
        else Left $
            "response item storage kind "
                <> expectedType
                <> " has item type "
                <> base.baseRowItemType
    representation <- representationFromText base.baseRowRepresentation
    if representation == StoredCoreRepresentation
        then Right ()
        else Left $
            "response item "
                <> expectedType
                <> " has non-core representation "
                <> base.baseRowRepresentation

missingChild :: Text -> BaseRow -> Either Text a
missingChild kind base = Left (missingChildMessage kind base)

missingChildMessage :: Text -> BaseRow -> Text
missingChildMessage kind base =
    "response item "
        <> base.baseRowId
        <> " is missing its "
        <> kind
        <> " row"

insertBaseStatement :: Statement BaseParams Text
insertBaseStatement = mkStatement
    "INSERT INTO harness.session_response_items\
    \ (turn_id, item_index, storage_kind, item_type, representation)\
    \ VALUES ($1::uuid, $2, $3, $4, $5)\
    \ RETURNING response_item_id::text"
    ( fieldParam (.baseTurnId) (Encoders.param (Encoders.nonNullable Encoders.text))
        <> fieldParam (.baseIndex) (Encoders.param (Encoders.nonNullable Encoders.int4))
        <> fieldParam (.baseStorageKind) (Encoders.param (Encoders.nonNullable Encoders.text))
        <> fieldParam (.baseItemType) (Encoders.param (Encoders.nonNullable Encoders.text))
        <> fieldParam (.baseRepresentation) (Encoders.param (Encoders.nonNullable Encoders.text))
    )
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text)))
    True

insertMessageStatement :: Statement MessageParams ()
insertMessageStatement = mkStatement
    "INSERT INTO harness.session_messages\
    \ (response_item_id, provider_item_id, role_name, status_name, phase,\
    \ content_kind, content_text, extra_fields_text)\
    \ VALUES ($1::uuid, $2, $3, $4, $5, $6, $7, $8)"
    ( fieldParam (.messageResponseItemId) (Encoders.param (Encoders.nonNullable Encoders.text))
        <> fieldParam (.messageProviderItemId) (Encoders.param (Encoders.nullable Encoders.text))
        <> fieldParam (.messageRole) (Encoders.param (Encoders.nonNullable Encoders.text))
        <> fieldParam (.messageStatus) (Encoders.param (Encoders.nullable Encoders.text))
        <> fieldParam (.messagePhase) (Encoders.param (Encoders.nullable Encoders.text))
        <> fieldParam (.messageContentKind) (Encoders.param (Encoders.nonNullable Encoders.text))
        <> fieldParam (.messageContentText) (Encoders.param (Encoders.nullable Encoders.text))
        <> fieldParam (.messageExtraFields) (Encoders.param (Encoders.nonNullable Encoders.text))
    )
    Decoders.noResult
    True

insertFunctionCallStatement :: Statement FunctionCallParams ()
insertFunctionCallStatement = mkStatement
    "INSERT INTO harness.session_function_calls\
    \ (response_item_id, provider_item_id, call_id, function_name,\
    \ arguments, status_name, extra_fields_text)\
    \ VALUES ($1::uuid, $2, $3, $4, $5, $6, $7)"
    ( fieldParam (.functionCallResponseItemId) (Encoders.param (Encoders.nonNullable Encoders.text))
        <> fieldParam (.functionCallProviderItemId) (Encoders.param (Encoders.nullable Encoders.text))
        <> fieldParam (.functionCallCallId) (Encoders.param (Encoders.nonNullable Encoders.text))
        <> fieldParam (.functionCallName) (Encoders.param (Encoders.nonNullable Encoders.text))
        <> fieldParam (.functionCallArguments) (Encoders.param (Encoders.nonNullable Encoders.text))
        <> fieldParam (.functionCallStatus) (Encoders.param (Encoders.nullable Encoders.text))
        <> fieldParam (.functionCallExtraFields) (Encoders.param (Encoders.nonNullable Encoders.text))
    )
    Decoders.noResult
    True

insertFunctionOutputStatement :: Statement FunctionOutputParams ()
insertFunctionOutputStatement = mkStatement
    "INSERT INTO harness.session_function_call_outputs\
    \ (response_item_id, provider_item_id, call_id, output_kind, output_text,\
    \ status_name, extra_fields_text)\
    \ VALUES ($1::uuid, $2, $3, $4, $5, $6, $7)"
    ( fieldParam (.functionOutputResponseItemId) (Encoders.param (Encoders.nonNullable Encoders.text))
        <> fieldParam (.functionOutputProviderItemId) (Encoders.param (Encoders.nullable Encoders.text))
        <> fieldParam (.functionOutputCallId) (Encoders.param (Encoders.nonNullable Encoders.text))
        <> fieldParam (.functionOutputKind) (Encoders.param (Encoders.nonNullable Encoders.text))
        <> fieldParam (.functionOutputText) (Encoders.param (Encoders.nonNullable Encoders.text))
        <> fieldParam (.functionOutputStatus) (Encoders.param (Encoders.nullable Encoders.text))
        <> fieldParam (.functionOutputExtraFields) (Encoders.param (Encoders.nonNullable Encoders.text))
    )
    Decoders.noResult
    True

insertCustomCallStatement :: Statement CustomCallParams ()
insertCustomCallStatement = mkStatement
    "INSERT INTO harness.session_custom_tool_calls\
    \ (response_item_id, provider_item_id, call_id, tool_name, input_text,\
    \ status_name, extra_fields_text)\
    \ VALUES ($1::uuid, $2, $3, $4, $5, $6, $7)"
    ( fieldParam (.customCallResponseItemId) (Encoders.param (Encoders.nonNullable Encoders.text))
        <> fieldParam (.customCallProviderItemId) (Encoders.param (Encoders.nullable Encoders.text))
        <> fieldParam (.customCallCallId) (Encoders.param (Encoders.nonNullable Encoders.text))
        <> fieldParam (.customCallName) (Encoders.param (Encoders.nonNullable Encoders.text))
        <> fieldParam (.customCallInput) (Encoders.param (Encoders.nonNullable Encoders.text))
        <> fieldParam (.customCallStatus) (Encoders.param (Encoders.nullable Encoders.text))
        <> fieldParam (.customCallExtraFields) (Encoders.param (Encoders.nonNullable Encoders.text))
    )
    Decoders.noResult
    True

insertCustomOutputStatement :: Statement CustomOutputParams ()
insertCustomOutputStatement = mkStatement
    "INSERT INTO harness.session_custom_tool_call_outputs\
    \ (response_item_id, provider_item_id, call_id, tool_name, output_kind,\
    \ output_text, status_name, extra_fields_text)\
    \ VALUES ($1::uuid, $2, $3, $4, $5, $6, $7, $8)"
    ( fieldParam (.customOutputResponseItemId) (Encoders.param (Encoders.nonNullable Encoders.text))
        <> fieldParam (.customOutputProviderItemId) (Encoders.param (Encoders.nullable Encoders.text))
        <> fieldParam (.customOutputCallId) (Encoders.param (Encoders.nonNullable Encoders.text))
        <> fieldParam (.customOutputName) (Encoders.param (Encoders.nullable Encoders.text))
        <> fieldParam (.customOutputKind) (Encoders.param (Encoders.nonNullable Encoders.text))
        <> fieldParam (.customOutputText) (Encoders.param (Encoders.nonNullable Encoders.text))
        <> fieldParam (.customOutputStatus) (Encoders.param (Encoders.nullable Encoders.text))
        <> fieldParam (.customOutputExtraFields) (Encoders.param (Encoders.nonNullable Encoders.text))
    )
    Decoders.noResult
    True

insertReasoningStatement :: Statement ReasoningParams ()
insertReasoningStatement = mkStatement
    "INSERT INTO harness.session_reasoning_items\
    \ (response_item_id, provider_item_id, has_content, encrypted_content,\
    \ status_name, extra_fields_text)\
    \ VALUES ($1::uuid, $2, $3, $4, $5, $6)"
    ( fieldParam (.reasoningResponseItemId) (Encoders.param (Encoders.nonNullable Encoders.text))
        <> fieldParam (.reasoningProviderItemId) (Encoders.param (Encoders.nullable Encoders.text))
        <> fieldParam (.reasoningHasContent) (Encoders.param (Encoders.nonNullable Encoders.bool))
        <> fieldParam (.reasoningEncryptedContent) (Encoders.param (Encoders.nullable Encoders.text))
        <> fieldParam (.reasoningStatus) (Encoders.param (Encoders.nullable Encoders.text))
        <> fieldParam (.reasoningExtraFields) (Encoders.param (Encoders.nonNullable Encoders.text))
    )
    Decoders.noResult
    True

insertSummaryStatement :: Statement SummaryParams ()
insertSummaryStatement = mkStatement
    "INSERT INTO harness.session_reasoning_summaries\
    \ (response_item_id, part_index, part_type, text_value,\
    \ extra_fields_text)\
    \ VALUES ($1::uuid, $2, $3, $4, $5)"
    ( fieldParam (.summaryResponseItemId) (Encoders.param (Encoders.nonNullable Encoders.text))
        <> fieldParam (.summaryIndex) (Encoders.param (Encoders.nonNullable Encoders.int4))
        <> fieldParam (.summaryType) (Encoders.param (Encoders.nonNullable Encoders.text))
        <> fieldParam (.summaryText) (Encoders.param (Encoders.nullable Encoders.text))
        <> fieldParam (.summaryExtraFields) (Encoders.param (Encoders.nonNullable Encoders.text))
    )
    Decoders.noResult
    True

insertReferenceStatement :: Statement ReferenceParams ()
insertReferenceStatement = mkStatement
    "INSERT INTO harness.session_item_references\
    \ (response_item_id, provider_item_id, extra_fields_text)\
    \ VALUES ($1::uuid, $2, $3)"
    ( fieldParam (.referenceResponseItemId) (Encoders.param (Encoders.nonNullable Encoders.text))
        <> fieldParam (.referenceProviderItemId) (Encoders.param (Encoders.nonNullable Encoders.text))
        <> fieldParam (.referenceExtraFields) (Encoders.param (Encoders.nonNullable Encoders.text))
    )
    Decoders.noResult
    True

insertTaggedStatement :: Statement TaggedParams ()
insertTaggedStatement = mkStatement
    "INSERT INTO harness.session_tagged_items\
    \ (response_item_id, wire_tag, fields_text)\
    \ VALUES ($1::uuid, $2, $3)"
    ( fieldParam (.taggedResponseItemId) (Encoders.param (Encoders.nonNullable Encoders.text))
        <> fieldParam (.taggedWireTag) (Encoders.param (Encoders.nonNullable Encoders.text))
        <> fieldParam (.taggedFields) (Encoders.param (Encoders.nonNullable Encoders.text))
    )
    Decoders.noResult
    True

insertContentPartStatement :: Statement ContentPartParams ()
insertContentPartStatement = mkStatement
    "INSERT INTO harness.session_response_content_parts\
    \ (response_item_id, part_index, part_type, text_value, refusal_text,\
    \ detail, file_data, file_id, file_url, filename, image_url,\
    \ input_audio_text, prompt_cache_breakpoint_text, annotations_text,\
    \ logprobs_text, extra_fields_text)\
    \ VALUES ($1::uuid, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11,\
    \ $12, $13, $14, $15, $16)"
    ( fieldParam (.contentPartResponseItemId) (Encoders.param (Encoders.nonNullable Encoders.text))
        <> fieldParam (.contentPartIndex) (Encoders.param (Encoders.nonNullable Encoders.int4))
        <> fieldParam (.contentPartType) (Encoders.param (Encoders.nonNullable Encoders.text))
        <> fieldParam (.contentPartText) (Encoders.param (Encoders.nullable Encoders.text))
        <> fieldParam (.contentPartRefusal) (Encoders.param (Encoders.nullable Encoders.text))
        <> fieldParam (.contentPartDetail) (Encoders.param (Encoders.nullable Encoders.text))
        <> fieldParam (.contentPartFileData) (Encoders.param (Encoders.nullable Encoders.text))
        <> fieldParam (.contentPartFileId) (Encoders.param (Encoders.nullable Encoders.text))
        <> fieldParam (.contentPartFileUrl) (Encoders.param (Encoders.nullable Encoders.text))
        <> fieldParam (.contentPartFilename) (Encoders.param (Encoders.nullable Encoders.text))
        <> fieldParam (.contentPartImageUrl) (Encoders.param (Encoders.nullable Encoders.text))
        <> fieldParam (.contentPartInputAudio) (Encoders.param (Encoders.nullable Encoders.text))
        <> fieldParam (.contentPartPromptCacheBreakpoint) (Encoders.param (Encoders.nullable Encoders.text))
        <> fieldParam (.contentPartAnnotations) (Encoders.param (Encoders.nullable Encoders.text))
        <> fieldParam (.contentPartLogprobs) (Encoders.param (Encoders.nullable Encoders.text))
        <> fieldParam (.contentPartExtraFields) (Encoders.param (Encoders.nonNullable Encoders.text))
    )
    Decoders.noResult
    True

loadBaseRowsStatement :: Statement Text [BaseRow]
loadBaseRowsStatement = mkStatement
    "SELECT response_item_id::text, storage_kind, item_type, representation\
    \ FROM harness.session_response_items\
    \ WHERE turn_id = $1::uuid\
    \ ORDER BY item_index"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowList $
        BaseRow
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text))
    True

loadMessageStatement :: Statement Text (Maybe MessageRow)
loadMessageStatement = mkStatement
    "SELECT provider_item_id, role_name, status_name, phase, content_kind,\
    \ content_text, extra_fields_text\
    \ FROM harness.session_messages\
    \ WHERE response_item_id = $1::uuid"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowMaybe $
        MessageRow
            <$> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text))
    True

loadFunctionCallStatement :: Statement Text (Maybe StoredFunctionCall)
loadFunctionCallStatement = mkStatement
    "SELECT provider_item_id, call_id, function_name, arguments, status_name,\
    \ extra_fields_text\
    \ FROM harness.session_function_calls\
    \ WHERE response_item_id = $1::uuid"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowMaybe $
        StoredFunctionCall
            <$> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> (StoredOpaqueObject <$> Decoders.column (Decoders.nonNullable Decoders.text)))
    True

loadFunctionOutputStatement :: Statement Text (Maybe FunctionOutputRow)
loadFunctionOutputStatement = mkStatement
    "SELECT provider_item_id, call_id, output_kind, output_text, status_name,\
    \ extra_fields_text\
    \ FROM harness.session_function_call_outputs\
    \ WHERE response_item_id = $1::uuid"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowMaybe $
        FunctionOutputRow
            <$> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text))
    True

loadCustomCallStatement :: Statement Text (Maybe StoredCustomToolCall)
loadCustomCallStatement = mkStatement
    "SELECT provider_item_id, call_id, tool_name, input_text, status_name,\
    \ extra_fields_text\
    \ FROM harness.session_custom_tool_calls\
    \ WHERE response_item_id = $1::uuid"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowMaybe $
        StoredCustomToolCall
            <$> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> (StoredOpaqueObject <$> Decoders.column (Decoders.nonNullable Decoders.text)))
    True

loadCustomOutputStatement :: Statement Text (Maybe CustomOutputRow)
loadCustomOutputStatement = mkStatement
    "SELECT provider_item_id, call_id, tool_name, output_kind, output_text,\
    \ status_name, extra_fields_text\
    \ FROM harness.session_custom_tool_call_outputs\
    \ WHERE response_item_id = $1::uuid"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowMaybe $
        CustomOutputRow
            <$> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text))
    True

loadReasoningStatement :: Statement Text (Maybe ReasoningRow)
loadReasoningStatement = mkStatement
    "SELECT provider_item_id, has_content, encrypted_content, status_name,\
    \ extra_fields_text\
    \ FROM harness.session_reasoning_items\
    \ WHERE response_item_id = $1::uuid"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowMaybe $
        ReasoningRow
            <$> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.bool)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text))
    True

loadSummariesStatement :: Statement Text [StoredReasoningSummaryPart]
loadSummariesStatement = mkStatement
    "SELECT part_type, text_value, extra_fields_text\
    \ FROM harness.session_reasoning_summaries\
    \ WHERE response_item_id = $1::uuid\
    \ ORDER BY part_index"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowList $
        StoredReasoningSummaryPart
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> (StoredOpaqueObject <$> Decoders.column (Decoders.nonNullable Decoders.text)))
    True

loadReferenceStatement :: Statement Text (Maybe StoredItemReference)
loadReferenceStatement = mkStatement
    "SELECT provider_item_id, extra_fields_text\
    \ FROM harness.session_item_references\
    \ WHERE response_item_id = $1::uuid"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowMaybe $
        StoredItemReference
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> (StoredOpaqueObject <$> Decoders.column (Decoders.nonNullable Decoders.text)))
    True

loadTaggedStatement :: Statement Text (Maybe TaggedRow)
loadTaggedStatement = mkStatement
    "SELECT wire_tag, fields_text\
    \ FROM harness.session_tagged_items\
    \ WHERE response_item_id = $1::uuid"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowMaybe $
        TaggedRow <$> Decoders.column (Decoders.nonNullable Decoders.text) <*> Decoders.column (Decoders.nonNullable Decoders.text))
    True

loadContentPartsStatement :: Statement Text [StoredContentPart]
loadContentPartsStatement = mkStatement
    "SELECT part_type, text_value, refusal_text, detail, file_data, file_id,\
    \ file_url, filename, image_url, input_audio_text,\
    \ prompt_cache_breakpoint_text, annotations_text, logprobs_text,\
    \ extra_fields_text\
    \ FROM harness.session_response_content_parts\
    \ WHERE response_item_id = $1::uuid\
    \ ORDER BY part_index"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowList $
        StoredContentPart
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> nullableOpaqueValueColumn
            <*> nullableOpaqueValueColumn
            <*> nullableOpaqueValueColumn
            <*> nullableOpaqueValueColumn
            <*> (StoredOpaqueObject <$> Decoders.column (Decoders.nonNullable Decoders.text)))
    True

fieldParam :: (a -> b) -> Encoders.Params b -> Encoders.Params a
fieldParam field encoder = field >$< encoder

nullableOpaqueValueColumn :: Decoders.Row (Maybe StoredOpaqueValue)
nullableOpaqueValueColumn =
    fmap StoredOpaqueValue <$> Decoders.column (Decoders.nullable Decoders.text)
