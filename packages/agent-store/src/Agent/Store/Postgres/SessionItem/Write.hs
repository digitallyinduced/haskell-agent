{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Relational writes for response items attached to a session turn.
module Agent.Store.Postgres.SessionItem.Write
    ( insertResponseItems
    ) where

import Control.Monad (forM_)
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int32)
import Data.Text (Text)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement)
import qualified Hasql.Transaction as Transaction

import Agent.Store.Postgres.Hasql (mkStatement)
import Agent.Store.SessionItem

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

toolOutputKindText :: StoredToolOutputKind -> Text
toolOutputKindText = \case
    StoredToolOutputText -> "text"
    StoredToolOutputEncoded -> "encoded"

opaqueValueText :: StoredOpaqueValue -> Text
opaqueValueText value = value.storedOpaqueValueText

opaqueObjectText :: StoredOpaqueObject -> Text
opaqueObjectText value = value.storedOpaqueObjectText

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

fieldParam :: (a -> b) -> Encoders.Params b -> Encoders.Params a
fieldParam field encoder = field >$< encoder
