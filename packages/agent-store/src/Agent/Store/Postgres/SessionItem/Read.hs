{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Agent.Store.Postgres.SessionItem.Read
    ( loadResponseItems
    ) where

import Control.Monad (forM)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Hasql.Statement (Statement)
import qualified Hasql.Transaction as Transaction

import Agent.Store.Postgres.SessionItem.Mapping.Statements.BaseRows
import Agent.Store.Postgres.SessionItem.Mapping.Statements.ContentParts
import Agent.Store.Postgres.SessionItem.Mapping.Statements.CustomCall
import Agent.Store.Postgres.SessionItem.Mapping.Statements.CustomOutput
import Agent.Store.Postgres.SessionItem.Mapping.Statements.FunctionCall
import Agent.Store.Postgres.SessionItem.Mapping.Statements.FunctionOutput
import Agent.Store.Postgres.SessionItem.Mapping.Statements.Message
import Agent.Store.Postgres.SessionItem.Mapping.Statements.Reasoning
import Agent.Store.Postgres.SessionItem.Mapping.Statements.Reference
import Agent.Store.Postgres.SessionItem.Mapping.Statements.Summaries
import Agent.Store.Postgres.SessionItem.Mapping.Statements.Tagged
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

loadResponseItems
    :: Text
    -> Transaction.Transaction (Either Text [StoredResponseItem])
loadResponseItems turnId = do
    rows <- Transaction.statement turnId loadBaseRowsStatement
    let messagesBatched = shouldBatch "message" rows
        functionCallsBatched = shouldBatch "function_call" rows
        functionOutputsBatched =
            shouldBatch "function_call_output" rows
        reasoningBatched = shouldBatch "reasoning" rows
    messages <- loadIf messagesBatched loadMessagesStatement
    functionCalls <- loadIf functionCallsBatched loadFunctionCallsStatement
    functionOutputs <- loadIf
        functionOutputsBatched
        loadFunctionOutputsStatement
    reasoningItems <- loadIf reasoningBatched loadReasoningItemsStatement
    contentParts <- loadIf
        ( (messagesBatched
                && any ((== "parts") . (.messageRowContentKind) . snd) messages)
            || (reasoningBatched
                && any ((.reasoningRowHasContent) . snd) reasoningItems)
        )
        loadTurnContentPartsStatement
    summaries <- loadIf
        reasoningBatched
        loadTurnSummariesStatement
    sequence <$> forM rows
        (loadResponseItem
            messagesBatched
            functionCallsBatched
            functionOutputsBatched
            reasoningBatched
            (Map.fromList messages)
            (Map.fromList functionCalls)
            (Map.fromList functionOutputs)
            (Map.fromList reasoningItems)
            (groupChildren contentParts)
            (groupChildren summaries))
  where
    loadIf
        :: Bool
        -> Statement Text [a]
        -> Transaction.Transaction [a]
    loadIf condition statement
        | condition = Transaction.statement turnId statement
        | otherwise = pure []

    groupChildren :: [(Text, a)] -> Map.Map Text [a]
    groupChildren =
        Map.map reverse
            . Map.fromListWith (++)
            . map (\(itemId, child) -> (itemId, [child]))

    shouldBatch :: Text -> [BaseRow] -> Bool
    shouldBatch kind = go 0
      where
        go :: Int -> [BaseRow] -> Bool
        go count _
            | count > batchChildThreshold = True
        go _ [] = False
        go count (row : rest) =
            go
                (if row.baseRowStorageKind == kind then count + 1 else count)
                rest

-- Point lookups remain the lower-latency path for tiny turns. The real-session
-- benchmark showed the batched statements win once a child kind has meaningful
-- fan-out, while this cutoff keeps message-only sessions on their old path.
batchChildThreshold :: Int
batchChildThreshold = 8

loadResponseItem
    :: Bool
    -> Bool
    -> Bool
    -> Bool
    -> Map.Map Text MessageRow
    -> Map.Map Text FunctionCallRow
    -> Map.Map Text FunctionOutputRow
    -> Map.Map Text ReasoningRow
    -> Map.Map Text [ContentPartRow]
    -> Map.Map Text [SummaryRow]
    -> BaseRow
    -> Transaction.Transaction (Either Text StoredResponseItem)
loadResponseItem
        messagesBatched
        functionCallsBatched
        functionOutputsBatched
        reasoningBatched
        messages
        functionCalls
        functionOutputs
        reasoningItems
        contentParts
        summaries
        base =
    case base.baseRowStorageKind of
        "message" ->
            if messagesBatched
                then pure $
                    loadMessageValue
                        (Map.lookup base.baseRowId messages)
                        (Map.findWithDefault [] base.baseRowId contentParts)
                        base
                else loadMessage base
        "function_call" ->
            if functionCallsBatched
                then pure $
                    loadFunctionCallValue
                        (Map.lookup base.baseRowId functionCalls)
                        base
                else loadFunctionCall base
        "function_call_output" ->
            if functionOutputsBatched
                then pure $
                    loadFunctionOutputValue
                        (Map.lookup base.baseRowId functionOutputs)
                        base
                else loadFunctionOutput base
        "custom_tool_call" -> loadCustomCall base
        "custom_tool_call_output" -> loadCustomOutput base
        "reasoning" ->
            if reasoningBatched
                then pure $
                    loadReasoningValue
                        (Map.lookup base.baseRowId reasoningItems)
                        (Map.findWithDefault [] base.baseRowId contentParts)
                        (Map.findWithDefault [] base.baseRowId summaries)
                        base
                else loadReasoning base
        "item_reference" -> loadItemReference base
        "tagged" -> loadTagged base
        value ->
            pure (Left ("unknown response item storage kind: " <> value))

loadMessageValue
    :: Maybe MessageRow
    -> [ContentPartRow]
    -> BaseRow
    -> Either Text StoredResponseItem
loadMessageValue stored parts base =
    case validateCoreBase "message" base of
        Left err -> Left err
        Right () -> do
            row <- maybe (missingChild "message" base) Right stored
            content <- case row.messageRowContentKind of
                "text" ->
                    maybe
                        (Left "stored text message has no content text")
                        (Right . StoredMessageText)
                        row.messageRowContentText
                "parts" ->
                    Right $
                        StoredMessageParts (map contentPartFromRow parts)
                value ->
                    Left ("unknown stored message content kind: " <> value)
            Right $ StoredMessageItem $ messageFromRow row content

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
                            messageFromRow row $
                                StoredMessageParts (map contentPartFromRow parts)
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

loadFunctionCallValue
    :: Maybe FunctionCallRow
    -> BaseRow
    -> Either Text StoredResponseItem
loadFunctionCallValue stored base =
    loadCoreChildValue
        "function_call"
        base
        stored
        (StoredFunctionCallItem . functionCallFromRow)

loadFunctionCall
    :: BaseRow
    -> Transaction.Transaction (Either Text StoredResponseItem)
loadFunctionCall base =
    loadCoreChild
        "function_call"
        base
        loadFunctionCallStatement
        (StoredFunctionCallItem . functionCallFromRow)

loadFunctionOutputValue
    :: Maybe FunctionOutputRow
    -> BaseRow
    -> Either Text StoredResponseItem
loadFunctionOutputValue stored base =
    case validateCoreBase "function_call_output" base of
        Left err -> Left err
        Right () -> do
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
            pure (loadFunctionOutputValue stored base)

loadCustomCall
    :: BaseRow
    -> Transaction.Transaction (Either Text StoredResponseItem)
loadCustomCall base =
    loadCoreChild
        "custom_tool_call"
        base
        loadCustomCallStatement
        (StoredCustomToolCallItem . customCallFromRow)

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

loadReasoningValue
    :: Maybe ReasoningRow
    -> [ContentPartRow]
    -> [SummaryRow]
    -> BaseRow
    -> Either Text StoredResponseItem
loadReasoningValue stored contentParts summaries base =
    case validateCoreBase "reasoning" base of
        Left err -> Left err
        Right () -> do
            row <- maybe (missingChild "reasoning" base) Right stored
            Right $ StoredReasoningItem StoredReasoning
                { storedReasoningProviderItemId =
                    row.reasoningRowProviderItemId
                , storedReasoningSummary =
                    map summaryFromRow summaries
                , storedReasoningContent =
                    if row.reasoningRowHasContent
                        then Just (map contentPartFromRow contentParts)
                        else Nothing
                , storedReasoningEncryptedContent =
                    row.reasoningRowEncryptedContent
                , storedReasoningStatus = row.reasoningRowStatus
                , storedReasoningExtraFields =
                    StoredOpaqueObject row.reasoningRowExtraFields
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
                            then Just . map contentPartFromRow
                                <$> Transaction.statement
                                    base.baseRowId
                                    loadContentPartsStatement
                            else pure Nothing
                    pure $ Right $ StoredReasoningItem StoredReasoning
                        { storedReasoningProviderItemId =
                            row.reasoningRowProviderItemId
                        , storedReasoningSummary =
                            map summaryFromRow summaries
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
        (StoredItemReferenceItem . referenceFromRow)

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

loadCoreChildValue
    :: Text
    -> BaseRow
    -> Maybe a
    -> (a -> StoredResponseItem)
    -> Either Text StoredResponseItem
loadCoreChildValue expectedType base stored wrap = do
    validateCoreBase expectedType base
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

functionCallFromRow :: FunctionCallRow -> StoredFunctionCall
functionCallFromRow row = StoredFunctionCall
    { storedFunctionCallProviderItemId = row.functionCallRowProviderItemId
    , storedFunctionCallCallId = row.functionCallRowCallId
    , storedFunctionCallName = row.functionCallRowName
    , storedFunctionCallArguments = row.functionCallRowArguments
    , storedFunctionCallStatus = row.functionCallRowStatus
    , storedFunctionCallExtraFields =
        StoredOpaqueObject row.functionCallRowExtraFields
    }

customCallFromRow :: CustomCallRow -> StoredCustomToolCall
customCallFromRow row = StoredCustomToolCall
    { storedCustomToolCallProviderItemId = row.customCallRowProviderItemId
    , storedCustomToolCallCallId = row.customCallRowCallId
    , storedCustomToolCallName = row.customCallRowName
    , storedCustomToolCallInput = row.customCallRowInput
    , storedCustomToolCallStatus = row.customCallRowStatus
    , storedCustomToolCallExtraFields =
        StoredOpaqueObject row.customCallRowExtraFields
    }

referenceFromRow :: ReferenceRow -> StoredItemReference
referenceFromRow row = StoredItemReference
    { storedItemReferenceProviderItemId = row.referenceRowProviderItemId
    , storedItemReferenceExtraFields =
        StoredOpaqueObject row.referenceRowExtraFields
    }

summaryFromRow :: SummaryRow -> StoredReasoningSummaryPart
summaryFromRow row = StoredReasoningSummaryPart
    { storedReasoningSummaryPartType = row.summaryRowType
    , storedReasoningSummaryPartText = row.summaryRowText
    , storedReasoningSummaryPartExtraFields =
        StoredOpaqueObject row.summaryRowExtraFields
    }

contentPartFromRow :: ContentPartRow -> StoredContentPart
contentPartFromRow row = StoredContentPart
    { storedContentPartType = row.contentPartRowType
    , storedContentPartText = row.contentPartRowText
    , storedContentPartRefusal = row.contentPartRowRefusal
    , storedContentPartDetail = row.contentPartRowDetail
    , storedContentPartFileData = row.contentPartRowFileData
    , storedContentPartFileId = row.contentPartRowFileId
    , storedContentPartFileUrl = row.contentPartRowFileUrl
    , storedContentPartFilename = row.contentPartRowFilename
    , storedContentPartImageUrl = row.contentPartRowImageUrl
    , storedContentPartFileBinary =
        StoredBinaryData
            <$> row.contentPartRowFileDataMimeType
            <*> row.contentPartRowFileDataBytes
    , storedContentPartImageBinary =
        StoredBinaryData
            <$> row.contentPartRowImageMimeType
            <*> row.contentPartRowImageBytes
    , storedContentPartInputAudio =
        StoredOpaqueValue <$> row.contentPartRowInputAudio
    , storedContentPartPromptCacheBreakpoint =
        StoredOpaqueValue <$> row.contentPartRowPromptCacheBreakpoint
    , storedContentPartAnnotations =
        StoredOpaqueValue <$> row.contentPartRowAnnotations
    , storedContentPartLogprobs =
        StoredOpaqueValue <$> row.contentPartRowLogprobs
    , storedContentPartExtraFields =
        StoredOpaqueObject row.contentPartRowExtraFields
    }
