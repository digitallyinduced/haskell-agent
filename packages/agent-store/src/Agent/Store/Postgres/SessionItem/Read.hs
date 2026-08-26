{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Agent.Store.Postgres.SessionItem.Read
    ( loadResponseItems
    ) where

import Control.Monad (forM)
import Data.Text (Text)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement)
import qualified Hasql.Transaction as Transaction

import Agent.Store.Postgres.Hasql (mkStatement)
import Agent.Store.SessionItem

representationFromText :: Text -> Either Text StoredResponseItemRepresentation
representationFromText = \case
    "core" -> Right StoredCoreRepresentation
    "known" -> Right StoredKnownRepresentation
    "unknown" -> Right StoredUnknownRepresentation
    value -> Left ("unknown response item representation: " <> value)

toolOutputKindFromText :: Text -> Either Text StoredToolOutputKind
toolOutputKindFromText = \case
    "text" -> Right StoredToolOutputText
    "encoded" -> Right StoredToolOutputEncoded
    value -> Left ("unknown stored tool output kind: " <> value)

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

nullableOpaqueValueColumn :: Decoders.Row (Maybe StoredOpaqueValue)
nullableOpaqueValueColumn =
    fmap StoredOpaqueValue <$> Decoders.column (Decoders.nullable Decoders.text)
