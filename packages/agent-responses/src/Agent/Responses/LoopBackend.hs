-- | Provider-neutral loop adapters for Responses-compatible transports.
module Agent.Responses.LoopBackend
    ( statelessResponsesBackend
    , statelessResponsesBackendPreservingCheckpointHistory
    , statelessResponsesBackendPreservingHistory
    , statelessResponsesBackendWithRawReasoning
    , tokenProviderStatelessResponsesBackend
    , tokenProviderStatelessResponsesBackendPreservingCheckpointHistory
    , tokenProviderStatelessResponsesBackendPreservingHistory
    , turnInputsToItems
    , responseToTurnOutput
    , responseItemToToolCall
    , responseTokenUsage
    , streamEventToLoopEvent
    , streamEventToLoopEventWithRawReasoning
    , StreamProjectionState
    , emptyStreamProjectionState
    , streamEventToLoopEventsStep
    , newStreamEventToLoopEvents
    , toolArgumentActivityChunkChars
    , runawayToolArgumentWarningChars
    , streamOutputObserved
    , hasRecoverableIncompleteOutput
    , responseNeedsLoopContinuation
    , assistantTextFromResponse
    , toolResultToItem
    , withRequestInput
    , normalizeResponseInputItems
    , isServerCompactionCheckpoint
    ) where

import Agent.Error (ApiError)
import Agent.InterAgentMessage
    ( InterAgentMessage(..)
    , InterAgentMessageContent(..)
    , renderInterAgentMessage
    , renderInterAgentMessageHeader
    )
import Agent.Json (RawJson, rawJsonBytes, rawJsonFromEncoding)
import Agent.JsonText (jsonTextFieldPartial)
import Agent.Loop
    ( Backend(..)
    , BackendCallbacks(..)
    , BackendResult(..)
    , BackendSnapshot(..)
    , FileAttachment(..)
    , ImageAttachment(..)
    , LoopEvent(..)
    , TokenUsage(..)
    , TurnAttachment(..)
    , TurnCompletion(..)
    , TurnInput(..)
    , TurnOutput(..)
    , emptyTokenUsage
    , advanceBackendSnapshot
    , backendWithCallbacks
    )
import Agent.Provider
    ( Credential
    , TokenProvider
    , runWithTokenProvider
    )
import Agent.Responses.Request
    ( filterCompactionCheckpointsByOrigin
    , isServerCompactionCheckpoint
    , stripReplayedItemStatus
    )
import Agent.Responses.Types
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallKind(..)
    , ToolCallMode(..)
    , ToolCallResult(..)
    , ToolResultImage(..)
    , canonicalToolName
    , isComputerToolCallKind
    , toolCallMode
    , toolCallResultImages
    , toolCallResultMode
    , setToolCallArguments
    , withToolCallMode
    )
import Control.Applicative ((<|>))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Hermes as Hermes
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified "base64-bytestring" Data.ByteString.Base64 as Base64
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IORef (atomicModifyIORef', newIORef)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe, maybeToList)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

-- | Adapt a stateless Responses transport to the provider-neutral loop.
statelessResponsesBackend
    :: (ResponseCreateParams
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> Backend
statelessResponsesBackend send getParams =
    statelessResponsesBackendWithRawReasoning True send getParams

-- | Adapt a stateless transport whose opaque checkpoints are replayable, but
-- only by that same provider. Keep the complete pre-checkpoint history in host
-- state for later provider switches; the provider's wire projection must trim
-- that portable prefix when it replays its checkpoint.
statelessResponsesBackendPreservingCheckpointHistory
    :: (ResponseCreateParams
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> Backend
statelessResponsesBackendPreservingCheckpointHistory send getParams =
    statelessResponsesBackendWithMode
        PreservePreCheckpointHistoryAndCheckpoint
        True
        send
        getParams

-- | Adapt a stateless transport that cannot safely replay opaque server
-- checkpoints. Keep the complete request history even when a response
-- contains a checkpoint, and omit the unusable checkpoint itself from the
-- next snapshot so a later provider cannot mistake it for compatible state.
statelessResponsesBackendPreservingHistory
    :: (ResponseCreateParams
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> Backend
statelessResponsesBackendPreservingHistory send getParams =
    statelessResponsesBackendWithMode
        PreservePreCheckpointHistory
        True
        send
        getParams

-- | Adapt a stateless Responses transport while optionally exposing raw
-- reasoning text. Reasoning summaries remain visible in either mode.
statelessResponsesBackendWithRawReasoning
    :: Bool
    -> (ResponseCreateParams
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> Backend
statelessResponsesBackendWithRawReasoning showRawReasoning send getParams =
    statelessResponsesBackendWithMode
        ReplacePreCheckpointHistory
        showRawReasoning
        send
        getParams

data ServerCheckpointMode
    = ReplacePreCheckpointHistory
    | PreservePreCheckpointHistory
    | PreservePreCheckpointHistoryAndCheckpoint

statelessResponsesBackendWithMode
    :: ServerCheckpointMode
    -> Bool
    -> (ResponseCreateParams
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> Backend
statelessResponsesBackendWithMode
        checkpointMode
        showRawReasoning
        send
        getParams =
    backendWithCallbacks
        \snapshot _legacyPreviousResponseId inputs callbacks -> do
        baseParams <- getParams
        projectEvent <- newStreamEventToLoopEvents showRawReasoning
        let newItems = turnInputsToItems inputs
            requestItems = snapshot.backendItems <> newItems
            request = withRequestInput baseParams requestItems
        result <- send request \event -> do
            projectEvent event >>= mapM_ callbacks.onLoopEvent
            case completedAsyncToolCall event of
                Just call -> callbacks.onAsyncToolCall call
                Nothing -> pure ()
        case result of
            Left err -> pure (Left err)
            Right response ->
                let normalizedRequestItems =
                        normalizeResponseInputItems requestItems
                    completedItems =
                        case checkpointMode of
                            ReplacePreCheckpointHistory ->
                                fromMaybe
                                    (normalizedRequestItems <> response.output)
                                    (latestServerCheckpointSuffix response.output)
                            PreservePreCheckpointHistory ->
                                normalizedRequestItems
                                    <> filterCompactionCheckpointsByOrigin
                                        (const False)
                                        response.output
                            PreservePreCheckpointHistoryAndCheckpoint ->
                                normalizedRequestItems <> response.output
                in
                pure $ Right BackendResult
                    { backendOutput = responseToTurnOutput response
                    , backendState =
                        advanceBackendSnapshot snapshot
                            completedItems
                            Nothing
                    }

-- Server compaction checkpoints replace everything that preceded them. Keep
-- the checkpoint and later output in the stateless snapshot so subsequent
-- requests do not replay the obsolete pre-compaction transcript.
latestServerCheckpointSuffix
    :: [ResponseItem]
    -> Maybe [ResponseItem]
latestServerCheckpointSuffix = go [] . reverse
  where
    go _ [] = Nothing
    go after (item : before)
        | isServerCompactionCheckpoint item = Just (item : after)
        | otherwise = go (item : after) before

-- | Adapt a credentialed stateless Responses transport to the loop.
--
-- Credential acquisition and account failover are shared across providers;
-- the transport remains responsible only for one request with one credential.
tokenProviderStatelessResponsesBackend
    :: TokenProvider
    -> (Credential
        -> ResponseCreateParams
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> Backend
tokenProviderStatelessResponsesBackend provider send =
    statelessResponsesBackend \params onEvent ->
        runWithTokenProvider provider \credential ->
            send credential params onEvent

-- | Credentialed counterpart to
-- 'statelessResponsesBackendPreservingCheckpointHistory'.
tokenProviderStatelessResponsesBackendPreservingCheckpointHistory
    :: TokenProvider
    -> (Credential
        -> ResponseCreateParams
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> Backend
tokenProviderStatelessResponsesBackendPreservingCheckpointHistory
        provider
        send =
    statelessResponsesBackendPreservingCheckpointHistory \params onEvent ->
        runWithTokenProvider provider \credential ->
            send credential params onEvent

-- | Credentialed counterpart to
-- 'statelessResponsesBackendPreservingHistory'.
tokenProviderStatelessResponsesBackendPreservingHistory
    :: TokenProvider
    -> (Credential
        -> ResponseCreateParams
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> Backend
tokenProviderStatelessResponsesBackendPreservingHistory provider send =
    statelessResponsesBackendPreservingHistory \params onEvent ->
        runWithTokenProvider provider \credential ->
            send credential params onEvent

-- | 'input' is also a field on 'CustomToolCall', so a record update is
-- ambiguous. Rebuild from the constructor instead.
withRequestInput :: ResponseCreateParams -> [ResponseItem] -> ResponseCreateParams
withRequestInput ResponseCreateParams{..} items =
    let prefix = requestInputPrefix input
        -- Replayed transcript items are provider output. Keep checkpoint
        -- provenance intact until the target provider can decide whether the
        -- adjacent opaque checkpoint is compatible, then drop provider
        -- lifecycle status (see 'stripReplayedItemStatus').
        normalizedItems =
            map stripReplayedItemStatus
                (normalizeResponseInputItems items)
        requestItems
            | any isAdditionalTools prefix =
                ensureReasoningHasFollowingItem
                    (map stripResponsesLiteImageDetails normalizedItems)
            | otherwise =
                ensureReasoningHasFollowingItem normalizedItems
    in
    ResponseCreateParams
        { input = Just
            (ResponseInputItems
                (prefix <> requestItems))
        , ..
        }

requestInputPrefix :: Maybe ResponseInput -> [ResponseItem]
requestInputPrefix = \case
    Just (ResponseInputItems (firstItem : rest))
        | isAdditionalTools firstItem ->
            firstItem : takeWhile isBaseInstructions rest
    Just ResponseInputItems{} -> []
    Just ResponseInputText{} -> []
    Nothing -> []

isAdditionalTools :: ResponseItem -> Bool
isAdditionalTools = \case
    AdditionalToolsItemValue{} -> True
    UnknownResponseItem TaggedObject { tag = "additional_tools" } -> True
    _ -> False

isBaseInstructions :: ResponseItem -> Bool
isBaseInstructions = \case
    MessageItem ResponseMessage { role = RoleDeveloper, passthrough } ->
        case passthrough of
            Just metadata ->
                maybe False
                    ("model.base_instructions" `elem`)
                    metadata.contentItemKinds
            Nothing -> False
    _ -> False

-- Responses Lite rejects image-detail hints. Match Codex by removing them
-- from user messages and from image content embedded in tool outputs while
-- preserving every other item field.
stripResponsesLiteImageDetails :: ResponseItem -> ResponseItem
stripResponsesLiteImageDetails = \case
    MessageItem message ->
        MessageItem ResponseMessage
            { messageId = message.messageId
            , content = case message.content of
                MessageContentText text -> MessageContentText text
                MessageContentParts parts ->
                    MessageContentParts (map stripContentPart parts)
            , role = message.role
            , status = message.status
            , phase = message.phase
            , passthrough = message.passthrough

            }
    FunctionCallOutputItem callOutput ->
        FunctionCallOutputItem FunctionCallOutput
            { itemId = callOutput.itemId
            , callId = callOutput.callId
            , name = callOutput.name
            , namespace = callOutput.namespace
            , provider = callOutput.provider
            , output = stripRawJsonImageDetails callOutput.output
            , status = callOutput.status
            , async = callOutput.async
            }
    CustomToolCallOutputItem callOutput ->
        CustomToolCallOutputItem CustomToolCallOutput
            { itemId = callOutput.itemId
            , callId = callOutput.callId
            , name = callOutput.name
            , output = stripRawJsonImageDetails callOutput.output
            , status = callOutput.status
            , async = callOutput.async
            }
    item -> item
  where
    stripContentPart = \case
        InputImagePart{..} -> InputImagePart { detail = Nothing, .. }
        part -> part

stripRawJsonImageDetails :: RawJson -> RawJson
stripRawJsonImageDetails raw =
    case Aeson.decodeStrict' (rawJsonBytes raw) of
        Just value ->
            let cleaned = stripInputImageDetailValue value
            in if cleaned == value
                then raw
                else rawJsonFromEncoding (Aeson.toEncoding cleaned)
        Nothing -> raw

stripInputImageDetailValue :: Aeson.Value -> Aeson.Value
stripInputImageDetailValue = \case
    Aeson.Object object ->
        let nested = KeyMap.map stripInputImageDetailValue object
        in Aeson.Object $
            if KeyMap.lookup "type" nested
                    == Just (Aeson.String "input_image")
                then KeyMap.delete "detail" nested
                else nested
    Aeson.Array values ->
        Aeson.Array (fmap stripInputImageDetailValue values)
    value -> value

-- Repair persisted compatibility shapes at the wire boundary without
-- rewriting session files. Older assistant summaries used input_text, and
-- older computer sessions used provider-native call/output items that Codex
-- does not accept.
normalizeResponseInputItems :: [ResponseItem] -> [ResponseItem]
normalizeResponseInputItems = go Set.empty Nothing
  where
    go legacyFunctionCalls pendingScreenshot = \case
        [] -> computerObservationItems pendingScreenshot
        FunctionCallItem call : items
            | isLegacyComputerFunctionCall call ->
                computerObservationItems pendingScreenshot
                    <> [FunctionCallItem
                        (normalizeLegacyComputerFunctionCall call)]
                    <> go
                        (Set.insert call.callId legacyFunctionCalls)
                        Nothing
                        items
        FunctionCallOutputItem output : items
            | Set.member output.callId legacyFunctionCalls ->
                let screenshot = legacyFunctionScreenshot output
                in normalizeLegacyComputerFunctionOutput output screenshot
                    : go
                        (Set.delete output.callId legacyFunctionCalls)
                        screenshot
                        items
        item : items
            | isToolOutputItem item ->
                normalizeRequestItem item
                    <> go
                        legacyFunctionCalls
                        (updatedComputerScreenshot pendingScreenshot item)
                        items
            | otherwise ->
                computerObservationItems pendingScreenshot
                    <> normalizeRequestItem item
                    <> go legacyFunctionCalls Nothing items

    computerObservationItems =
        maybe [] (pure . legacyComputerScreenshotObservation)

-- Keep a legacy screenshot behind the complete run of tool outputs. A later
-- incomplete computer output invalidates an earlier image rather than
-- presenting stale pixels as the latest desktop state.
updatedComputerScreenshot
    :: Maybe Text
    -> ResponseItem
    -> Maybe Text
updatedComputerScreenshot current = \case
    ComputerCallOutputItem output -> legacyComputerScreenshot output
    _ -> current

isToolOutputItem :: ResponseItem -> Bool
isToolOutputItem = \case
    FunctionCallOutputItem{} -> True
    CustomToolCallOutputItem{} -> True
    ComputerCallOutputItem{} -> True
    _ -> False

normalizeRequestItem :: ResponseItem -> [ResponseItem]
normalizeRequestItem = \case
    ComputerCallItem call ->
        [FunctionCallItem (legacyComputerFunctionCall call)]
    ComputerCallOutputItem output ->
        [legacyComputerFunctionOutput output]
    MessageItem message
        | message.role == RoleAssistant ->
            [ MessageItem ResponseMessage
                { messageId = message.messageId
                , content = case message.content of
                    MessageContentText text ->
                        MessageContentParts
                            [OutputTextPart text Nothing Nothing]
                    MessageContentParts parts ->
                        MessageContentParts (map normalizeAssistantPart parts)
                , role = message.role
                , status = message.status
                , phase = message.phase
                , passthrough = message.passthrough

                }
            ]
    item -> [item]

legacyComputerFunctionCall :: ComputerCall -> FunctionCall
legacyComputerFunctionCall call = FunctionCall
    { itemId = Nothing
    , callId = call.computerCallId
    , name = computerFunctionName
    , namespace = Nothing
    , provider = Nothing
    , arguments =
        Text.decodeUtf8 . LBS.toStrict . Aeson.encode $
            Aeson.object ["actions" Aeson..= call.computerActions]
    , encryptedFunctionArgs = Nothing
    , status = call.computerCallStatus
    , async = Nothing
    }

legacyComputerFunctionOutput :: ComputerCallOutput -> ResponseItem
legacyComputerFunctionOutput output =
    FunctionCallOutputItem FunctionCallOutput
        { itemId = Nothing
        , callId = output.computerOutputCallId
        , name = Nothing
        , namespace = Nothing
        , provider = Nothing
        , output = rawJsonFromEncoding . Aeson.toEncoding $
            if legacyComputerOutputCompleted output
                then ("Computer action completed." :: Text)
                else "Computer action did not complete."
        , status = output.computerOutputStatus
        , async = Nothing
        }

legacyComputerScreenshot :: ComputerCallOutput -> Maybe Text
legacyComputerScreenshot output
    | legacyComputerOutputCompleted output =
        Just output.screenshotDataUrl
    | otherwise = Nothing

legacyComputerOutputCompleted :: ComputerCallOutput -> Bool
legacyComputerOutputCompleted output =
    output.computerOutputStatus `elem` [Nothing, Just ItemCompleted]

normalizeLegacyComputerFunctionCall :: FunctionCall -> FunctionCall
normalizeLegacyComputerFunctionCall call = FunctionCall
    { itemId = Nothing
    , callId = call.callId
    , name = computerFunctionName
    , namespace = Nothing
    , provider = Nothing
    , arguments = call.arguments
    , encryptedFunctionArgs = call.encryptedFunctionArgs
    , status = call.status
    , async = call.async
    }

normalizeLegacyComputerFunctionOutput
    :: FunctionCallOutput
    -> Maybe Text
    -> ResponseItem
normalizeLegacyComputerFunctionOutput output screenshot =
    FunctionCallOutputItem FunctionCallOutput
        { itemId = Nothing
        , callId = output.callId
        , name = Nothing
        , namespace = Nothing
        , provider = Nothing
        , output =
            if output.status == Just ItemIncomplete
                then rawJsonFromEncoding
                    (Aeson.toEncoding ("Computer action did not complete." :: Text))
                else case screenshot of
                    Just _ -> rawJsonFromEncoding
                        (Aeson.toEncoding ("Computer action completed." :: Text))
                    Nothing -> output.output
        , status = output.status
        , async = output.async
        }

legacyFunctionScreenshot :: FunctionCallOutput -> Maybe Text
legacyFunctionScreenshot output
    | output.status == Just ItemIncomplete = Nothing
    | otherwise =
        case Aeson.decodeStrict' (rawJsonBytes output.output) of
            Just (Aeson.Array parts) ->
                lastMaybe
                    [ imageUrl
                    | Aeson.Object part <- foldr (:) [] parts
                    , KeyMap.lookup "type" part
                        == Just (Aeson.String "input_image")
                    , Just (Aeson.String imageUrl) <-
                        [KeyMap.lookup "image_url" part]
                    ]
            _ -> Nothing

normalizeAssistantPart :: ResponseContentPart -> ResponseContentPart
normalizeAssistantPart = \case
    InputTextPart { text } ->
        OutputTextPart
            { text
            , annotations = Nothing
            , logprobs = Nothing

            }
    part -> part

-- | Responses rejects a trailing reasoning item with @missing_following_item@.
-- Stateless backends resend the local transcript, so splice an empty assistant
-- message when the last item is reasoning. Backends that send only deltas plus
-- @previous_response_id@ never include that trailing reasoning in @input@.
ensureReasoningHasFollowingItem :: [ResponseItem] -> [ResponseItem]
ensureReasoningHasFollowingItem items =
    case reverse items of
        ReasoningItemValue{} : _ ->
            items <> [emptyAssistantFollowupItem]
        _ -> items

emptyAssistantFollowupItem :: ResponseItem
emptyAssistantFollowupItem = MessageItem ResponseMessage
    { messageId = Nothing
    , content = MessageContentParts
        [OutputTextPart "" Nothing Nothing]
    , role = RoleAssistant
    , status = Nothing
    , phase = Nothing
    , passthrough = Nothing

    }

turnInputsToItems :: [TurnInput] -> [ResponseItem]
turnInputsToItems inputs =
    map turnInputToItem inputs
        <> maybeToList
            (computerScreenshotObservation <$> latestComputerScreenshot inputs)

turnInputToItem :: TurnInput -> ResponseItem
turnInputToItem = \case
    UserMessage text -> userMessageItem text
    AgentMessage message -> agentMessageItem message
    UserMessageWithAttachments text attachments ->
        userMessageWithAttachmentsItem text attachments
    CompletedTool result -> toolResultToItem result

userMessageItem :: Text -> ResponseItem
userMessageItem text = MessageItem ResponseMessage
    { messageId = Nothing
    , content = MessageContentParts [InputTextPart text Nothing]
    , role = RoleUser
    , status = Nothing
    , phase = Nothing
    , passthrough = Nothing

    }

userMessageWithAttachmentsItem
    :: Text
    -> NonEmpty.NonEmpty TurnAttachment
    -> ResponseItem
userMessageWithAttachmentsItem text attachments = MessageItem ResponseMessage
    { messageId = Nothing
    , content = MessageContentParts
        ( InputTextPart text Nothing
        : map attachmentPart (NonEmpty.toList attachments)
        )
    , role = RoleUser
    , status = Nothing
    , phase = Nothing
    , passthrough = Nothing

    }

agentMessageItem :: InterAgentMessage -> ResponseItem
agentMessageItem message = AgentMessageItem ResponseAgentMessage
    { messageId = Nothing
    , author = Just message.messageAuthor
    , recipient = Just message.messageRecipient
    , content = agentMessageContent message
    , passthrough = Nothing

    }

agentMessageContent :: InterAgentMessage -> [ResponseContentPart]
agentMessageContent message = case message.messageContent of
    PlainInterAgentContent _ ->
        [InputTextPart (renderInterAgentMessage message) Nothing]
    EncryptedInterAgentContent encrypted ->
        [ InputTextPart
            (renderInterAgentMessageHeader message)
            Nothing
        , EncryptedContentPart encrypted
        ]
attachmentPart :: TurnAttachment -> ResponseContentPart
attachmentPart = \case
    ImageAttachmentItem image -> imageAttachmentPart image
    FileAttachmentItem file -> fileAttachmentPart file

imageAttachmentPart :: ImageAttachment -> ResponseContentPart
imageAttachmentPart ImageAttachment{imageMime, imageBytes} =
    InputImagePart
        { detail = Just "auto"
        , fileId = Nothing
        , imageUrl = Just (imageDataUrl imageMime imageBytes)
        , promptCacheBreakpoint = Nothing

        }

fileAttachmentPart :: FileAttachment -> ResponseContentPart
fileAttachmentPart FileAttachment{fileName, fileMime, fileBytes} =
    InputFilePart
        { detail = Just "auto"
        , fileData = Just (imageDataUrl fileMime fileBytes)
        , fileId = Nothing
        , fileUrl = Nothing
        , filename = fileName
        , promptCacheBreakpoint = Nothing

        }

imageDataUrl :: Text -> ByteString -> Text
imageDataUrl mime bytes =
    "data:" <> mime <> ";base64," <> Text.decodeUtf8 (Base64.encode bytes)

toolResultToItem :: ToolCallResult -> ResponseItem
toolResultToItem result = case result.callKind of
    FunctionCallKind -> FunctionCallOutputItem FunctionCallOutput
        { itemId = Nothing
        , callId = result.callId
        , name = Nothing
        , namespace = Nothing
        , provider = Nothing
        , output = toolResultOutput result
        , status = Nothing
        , async = asyncResultField result
        }
    CustomCallKind -> CustomToolCallOutputItem CustomToolCallOutput
        { itemId = Nothing
        , callId = result.callId
        , name = Nothing
        , output = toolResultOutput result
        , status = Nothing
        , async = asyncResultField result
        }
    ComputerCallKind ->
        FunctionCallOutputItem FunctionCallOutput
            { itemId = Nothing
            , callId = result.callId
            , name = Nothing
            , namespace = Nothing
            , provider = Nothing
            , output = rawJsonFromEncoding . Aeson.toEncoding $
                computerFunctionTextOutput result.output
            , status = Nothing
            , async = Nothing
            }
    ComputerFunctionCallKind ->
        FunctionCallOutputItem FunctionCallOutput
            { itemId = Nothing
            , callId = result.callId
            , name = Nothing
            , namespace = Nothing
            , provider = Nothing
            , output = rawJsonFromEncoding . Aeson.toEncoding $
                computerFunctionTextOutput result.output
            , status = Nothing
            , async = Nothing
            }

toolResultOutput :: ToolCallResult -> RawJson
toolResultOutput result =
    case toolCallResultImages result of
        [] -> rawJsonFromEncoding (Aeson.toEncoding result.output)
        images ->
            rawJsonFromEncoding . Aeson.toEncoding $
                map imagePart images
                    <> [ InputTextPart result.output Nothing
                       | not (Text.null (Text.strip result.output))
                       ]
  where
    imagePart image =
        InputImagePart
            { detail = image.imageDetail
            , fileId = Nothing
            , imageUrl = Just image.imageUrl
            , promptCacheBreakpoint = Nothing
            }

computerFunctionTextOutput :: Text -> Text
computerFunctionTextOutput rawOutput =
    case Hermes.decodeEither computerCallOutputDecoder
            (Text.encodeUtf8 rawOutput) of
        Right output ->
            case KeyMap.lookup
                    "accessibility_state"
                    output.computerOutputExtra of
                Just (Aeson.String state)
                    | not (Text.null (Text.strip state)) ->
                        "Computer action completed.\n\n"
                            <> "Current macOS accessibility state:\n"
                            <> state
                _ -> "Computer action completed."
        Left _ -> rawOutput

latestComputerScreenshot :: [TurnInput] -> Maybe Text
latestComputerScreenshot inputs =
    lastMaybe
        [ result
        | CompletedTool result <- inputs
        , isComputerToolCallKind result.callKind
        ]
        >>= \result ->
            case Hermes.decodeEither computerCallOutputDecoder
                    (Text.encodeUtf8 result.output) of
                Right ComputerCallOutput{screenshotDataUrl} ->
                    Just screenshotDataUrl
                Left _ -> Nothing

computerScreenshotObservation :: Text -> ResponseItem
computerScreenshotObservation screenshotDataUrl =
    computerScreenshotObservationWith
        "Current macOS desktop after the completed computer action:"
        screenshotDataUrl

legacyComputerScreenshotObservation :: Text -> ResponseItem
legacyComputerScreenshotObservation screenshotDataUrl =
    computerScreenshotObservationWith
        "macOS desktop observed after the completed computer action:"
        screenshotDataUrl

computerScreenshotObservationWith :: Text -> Text -> ResponseItem
computerScreenshotObservationWith observationText screenshotDataUrl =
    MessageItem ResponseMessage
        { messageId = Nothing
        , content = MessageContentParts
            [ InputTextPart
                observationText
                Nothing
            , InputImagePart
                { detail = Just "auto"
                , fileId = Nothing
                , imageUrl = Just screenshotDataUrl
                , promptCacheBreakpoint = Nothing
                }
            ]
        , role = RoleUser
        , status = Nothing
        , phase = Nothing
        , passthrough = Nothing
        }

lastMaybe :: [value] -> Maybe value
lastMaybe = \case
    [] -> Nothing
    values -> Just (last values)

responseToTurnOutput :: Response -> TurnOutput
responseToTurnOutput response = TurnOutput
    { responseId = response.responseId
    , toolCalls = mapMaybe responseItemToToolCall response.output
    , assistantText = assistantTextFromResponse response
    , tokenUsage = responseTokenUsage response
    , providerTelemetry = Nothing
    , completion = case response.status of
        ResponseIncomplete
            | hasContinuableReasoningOnlyOutput response -> TurnCompleted
            | otherwise -> TurnIncomplete
                { incompleteReason =
                    maybe "unknown" (.reason) response.incompleteDetails
                , incompleteReasoningTokens =
                    response.usage
                        >>= (.outputTokensDetails)
                        >>= (.reasoningTokens)
                }
        _ -> TurnCompleted
    }

-- | Whether this successful response requires an empty continuation on its
-- committed response chain.
responseNeedsLoopContinuation :: Response -> Bool
responseNeedsLoopContinuation response = case response.status of
    ResponseCompleted ->
        not (responseHasToolCalls response)
            && not (responseHasVisibleAssistantText response)
    ResponseIncomplete -> hasContinuableReasoningOnlyOutput response
    _ -> False

-- | Whether the transport should retain an incomplete response instead of
-- converting it to an 'ApiError'. Partial tool/text output is retained so the
-- committed response can be reported without replaying it, but remains
-- 'TurnIncomplete'. A reasoning-only @max_output_tokens@ stop instead becomes
-- an empty completion so the loop can continue the response chain. Reasons
-- such as @content_filter@ stay transport failures, as do completely empty
-- incomplete responses, so a replay-safe fallback can still run.
hasRecoverableIncompleteOutput :: Response -> Bool
hasRecoverableIncompleteOutput response =
    responseHasToolCalls response
        || responseHasVisibleAssistantText response
        || hasContinuableReasoningOnlyOutput response

-- A reasoning-only max-output stop is an intermediate model sample. Mark it
-- completed at the loop boundary so the core loop continues from its committed
-- response id. Partial text or tool calls remain terminal incomplete output:
-- executing either could act on a truncated response.
hasContinuableReasoningOnlyOutput :: Response -> Bool
hasContinuableReasoningOnlyOutput response =
    not (null response.output)
        && all isReasoningOutput response.output
        && isContinuableIncompleteReason response

responseHasToolCalls :: Response -> Bool
responseHasToolCalls =
    not . null . mapMaybe responseItemToToolCall . (.output)

responseHasVisibleAssistantText :: Response -> Bool
responseHasVisibleAssistantText =
    maybe False (not . Text.null . Text.strip) . assistantTextFromResponse

isReasoningOutput :: ResponseItem -> Bool
isReasoningOutput = \case
    ReasoningItemValue{} -> True
    _ -> False

isContinuableIncompleteReason :: Response -> Bool
isContinuableIncompleteReason response =
    maybe False ((`elem` continuableIncompleteReasons) . (.reason))
        response.incompleteDetails

-- | Incomplete reasons where the model can still produce tools or text on a
-- follow-up sample. Safety/filter stops are not continuable.
continuableIncompleteReasons :: [Text]
continuableIncompleteReasons =
    ["max_output_tokens"]

responseTokenUsage :: Response -> TokenUsage
responseTokenUsage response =
    tokenUsageFromResponse response.usage

tokenUsageFromResponse :: Maybe ResponseUsage -> TokenUsage
tokenUsageFromResponse = maybe emptyTokenUsage \usage ->
    TokenUsage
        { inputTokens = usage.inputTokens
        , outputTokens = usage.outputTokens
        , cachedTokens = fromMaybe 0 (usage.inputTokensDetails >>= (.cachedTokens))
        }

responseItemToToolCall :: ResponseItem -> Maybe ToolCall
responseItemToToolCall = \case
    FunctionCallItem call
        | isComputerFunctionCall call ->
            Just $ withToolCallMode (callModeFromField call.async) ToolCall
                { callId = call.callId
                , name = "computer"
                , arguments = call.arguments
                , callKind = ComputerFunctionCallKind
                -- Desktop input may contain typed secrets. Conservatively
                -- redact every reserved computer-function payload.
                , argumentsEncrypted = True
                }
    FunctionCallItem call ->
        let toolName = namespacedToolName call.namespace call.name
        in Just $ withToolCallMode (callModeFromField call.async) ToolCall
            { callId = call.callId
            , name = toolName
            , arguments = call.arguments
            , callKind = FunctionCallKind
            , argumentsEncrypted =
                encryptedCollaborationArguments
                    toolName
                    call.encryptedFunctionArgs
            }
    CustomToolCallItem call ->
        Just $ withToolCallMode (callModeFromField call.async) ToolCall
        { callId = call.callId
        , name = namespacedToolName call.namespace call.name
        , arguments = call.input
        , callKind = CustomCallKind
        , argumentsEncrypted = False
        }
    ComputerCallItem call -> Just ToolCall
        { callId = call.computerCallId
        , name = "computer"
        , arguments = Text.decodeUtf8 (LBS.toStrict (Aeson.encode call))
        , callKind = ComputerCallKind
        , argumentsEncrypted = any isSensitiveComputerAction call.computerActions
        }
    _ -> Nothing

callModeFromField :: Maybe Bool -> ToolCallMode
callModeFromField = \case
    Just True -> AsyncToolCall
    _ -> BlockingToolCall

asyncResultField :: ToolCallResult -> Maybe Bool
asyncResultField result =
    case toolCallResultMode result of
        AsyncToolCall -> Just True
        BlockingToolCall -> Nothing

completedAsyncToolCall :: ResponseStreamEvent -> Maybe ToolCall
completedAsyncToolCall = \case
    ResponseOutputItemDoneEvent { item } -> do
        call <- responseItemToToolCall item
        case toolCallMode call of
            AsyncToolCall -> Just call
            BlockingToolCall -> Nothing
    _ -> Nothing

isComputerFunctionCall :: FunctionCall -> Bool
isComputerFunctionCall call =
    ( call.name == computerFunctionName
        && call.namespace `elem` [Nothing, Just "functions"]
    )
        || isLegacyComputerFunctionCall call

isLegacyComputerFunctionCall :: FunctionCall -> Bool
isLegacyComputerFunctionCall call =
    call.name == legacyComputerFunctionName
        && call.namespace == Just computerFunctionNamespace

isSensitiveComputerAction :: ComputerAction -> Bool
isSensitiveComputerAction = \case
    TypeAction{} -> True
    KeypressAction{} -> True
    _ -> False

encryptedCollaborationArguments :: Text -> Maybe [Text] -> Bool
encryptedCollaborationArguments toolName encryptedFunctionArgs =
    toolName `elem`
        [ "collaboration.spawn_agent"
        , "collaboration.send_message"
        , "collaboration.followup_task"
        ]
        && encryptedFunctionArgs /= Just []

namespacedToolName :: Maybe Text -> Text -> Text
namespacedToolName namespace name = case namespace of
    Just value
        | not (Text.null value) ->
            if Text.isSuffixOf "." value || Text.isSuffixOf "::" value
                then value <> name
                else value <> "." <> name
    _ -> name

assistantTextFromResponse :: Response -> Maybe Text
assistantTextFromResponse response = case
    [ value
    | MessageItem message <- response.output
    , message.role == RoleAssistant
    , value <- case message.content of
        MessageContentText text -> [text]
        MessageContentParts parts -> [text | OutputTextPart { text } <- parts]
    ] of
        [] -> Nothing
        values -> Just (Text.intercalate "\n" values)

streamEventToLoopEvent :: ResponseStreamEvent -> Maybe LoopEvent
streamEventToLoopEvent = streamEventToLoopEventWithRawReasoning True

-- | Convert a Responses stream event, optionally suppressing raw
-- @response.reasoning_text.delta@ events. Summary deltas are always exposed.
streamEventToLoopEventWithRawReasoning
    :: Bool
    -> ResponseStreamEvent
    -> Maybe LoopEvent
streamEventToLoopEventWithRawReasoning showRawReasoning = \case
    -- Publish a tool block as soon as the provider announces the output item.
    -- The loop will still execute the call only after the complete response
    -- has been assembled; this event is purely a live UI projection.
    ResponseOutputItemAddedEvent { item }
        | Just call <- responseItemToToolCall item ->
            Just (ToolStarted call)
    -- Providers commonly send the call arguments in deltas and include the
    -- complete call in the corresponding done event. Replace the placeholder
    -- block's metadata/body before the core loop starts executing it.
    ResponseOutputItemDoneEvent { item }
        | Just call <- responseItemToToolCall item ->
            Just (ToolUpdated call)
    ResponseReasoningSummaryPartAddedEvent
        { summaryIndex = Just index }
        | index > 0 ->
            Just (ReasoningDelta "\n\n")
    ResponseCodexRateLimitsEvent { rateLimits = limits } ->
            codexRateLimitsWarning limits
    OtherResponseStreamEvent
        { otherEventType = StreamEventUnknown eventType } ->
            Just (ActivityUpdated
                ("Warning: unsupported provider event " <> eventType))
    OtherResponseStreamEvent { otherEventType, eventDelta } ->
        case eventDelta of
            Just text | Text.null text -> Nothing
            Just text -> case otherEventType of
                EventOutputTextDelta -> Just (TextDelta text)
                EventReasoningTextDelta
                    | showRawReasoning -> Just (ReasoningDelta text)
                    | otherwise -> Nothing
                EventReasoningSummaryTextDelta -> Just (ReasoningDelta text)
                _ -> Nothing
            Nothing -> Nothing
    _ -> Nothing

codexRateLimitsWarning :: CodexRateLimits -> Maybe LoopEvent
codexRateLimitsWarning limits =
    if reached || not (null lowWindows)
        then Just (WarningRaised
            (headline <> foldMap formatWindows (nonEmpty lowWindows)))
        else Nothing
  where
    windows =
        [ ("primary", used)
        | used <- maybeToList limits.primaryUsedPercent
        ]
        <> [ ("secondary", used)
           | used <- maybeToList limits.secondaryUsedPercent
           ]
    lowWindows = filter ((>= 90) . snd) windows
    reached =
        limits.limitReached == Just True
            || limits.allowed == Just False
            || any ((>= 100) . snd) windows
    headline
        | reached =
            "Codex usage limit reached. Check /usage for reset details."
        | otherwise = "Codex usage is low"
    nonEmpty [] = Nothing
    nonEmpty values = Just values
    formatWindows values =
        ": " <> Text.intercalate " · "
            [ label <> " " <> formatRemaining (max 0 (100 - used))
                <> "% left"
            | (label, used) <- values
            ]
            <> ". Check /usage for reset details."
    formatRemaining value
        | value == fromIntegral (round value :: Int) =
            Text.pack (show (round value :: Int))
        | otherwise = Text.pack (show value)

-- | Stateful projection of one streamed response attempt into loop events.
--
-- On top of 'streamEventToLoopEventWithRawReasoning' this surfaces safe
-- streamed arguments as repaintable tool previews and reports coarse activity
-- for encrypted or computer-use calls. It also warns when a model gets stuck
-- in a degenerate repetition loop inside one call (observed as multi-minute
-- 128k-output-token samples whose arguments repeat @\\u0000@ or a hallucinated
-- path segment).
--
-- Build one projector per response attempt so counters describe a single
-- provider sample.
newStreamEventToLoopEvents
    :: Bool
    -> IO (ResponseStreamEvent -> IO [LoopEvent])
newStreamEventToLoopEvents showRawReasoning = do
    stateRef <- newIORef emptyStreamProjectionState
    pure \event ->
        atomicModifyIORef' stateRef \state ->
            streamEventToLoopEventsStep showRawReasoning state event

-- | Immutable state for projecting one response attempt.
--
-- The constructor is intentionally private so callers cannot accidentally
-- carry only part of the projection state across an attempt boundary.
data StreamProjectionState = StreamProjectionState
    { streamToolArguments :: !ToolArgumentStreamState
    }

emptyStreamProjectionState :: StreamProjectionState
emptyStreamProjectionState =
    StreamProjectionState emptyToolArgumentStreamState

-- | Pure projection of one provider event. The returned state belongs to the
-- same response attempt; start from 'emptyStreamProjectionState' when a retry
-- or reconnect begins a new sample.
streamEventToLoopEventsStep
    :: Bool
    -> StreamProjectionState
    -> ResponseStreamEvent
    -> (StreamProjectionState, [LoopEvent])
streamEventToLoopEventsStep showRawReasoning state event =
    ( StreamProjectionState nextArguments
    , maybeToList
        (statefulStreamEventToLoopEvent showRawReasoning event)
        <> argumentEvents
    )
  where
    (nextArguments, argumentEvents) =
        toolArgumentStreamStep event state.streamToolArguments

-- Function and custom-tool done items are projected by
-- 'toolArgumentStreamStep' so a sparse provider item can be reconciled with
-- arguments accumulated from deltas. Every other event retains the pure
-- projection used by stateless callers.
statefulStreamEventToLoopEvent
    :: Bool
    -> ResponseStreamEvent
    -> Maybe LoopEvent
statefulStreamEventToLoopEvent showRawReasoning event = case event of
    ResponseOutputItemDoneEvent { item = FunctionCallItem _ } -> Nothing
    ResponseOutputItemDoneEvent { item = CustomToolCallItem _ } -> Nothing
    _ -> streamEventToLoopEventWithRawReasoning showRawReasoning event

-- | Emit an updated argument-streaming activity after this many additional
-- streamed argument characters.
toolArgumentActivityChunkChars :: Int
toolArgumentActivityChunkChars = 8192

-- | Warn after every additional this many streamed argument characters in a
-- single response. The largest legitimate call observed in practice is well
-- under half of this; degenerate repetition loops run to the provider's
-- output-token cap (hundreds of thousands of characters).
runawayToolArgumentWarningChars :: Int
runawayToolArgumentWarningChars = 100000

data ToolArgumentStreamState = ToolArgumentStreamState
    { toolNamesById :: !(Map ToolStreamIdentity Text)
    , toolCallsById :: !(Map ToolStreamIdentity ToolCall)
    , toolNamesByOutputIndex :: !(IntMap Text)
    , toolCallsByOutputIndex :: !(IntMap ToolCall)
    , currentToolCall :: !(Maybe ToolCall)
    , shellPreviewsByCallId :: !(Map ToolPreviewKey Text)
    , rawPreviewsByCallId :: !(Map ToolPreviewKey RawArgumentPreview)
    , currentToolName :: !(Maybe Text)
    , streamedArgumentChars :: !Int
    , announcedArgumentChars :: !Int
    , warnedArgumentChars :: !Int
    }

data ToolStreamKind
    = FunctionToolStream
    | CustomToolStream
    deriving (Eq, Ord)

data ToolStreamIdentity
    = ToolStreamItemId !ToolStreamKind !Text
    | ToolStreamCallId !ToolStreamKind !Text
    deriving (Eq, Ord)

data ToolPreviewKey = ToolPreviewKey !ToolStreamKind !Text
    deriving (Eq, Ord)

data RawArgumentPreview = RawArgumentPreview
    { publishedRawArguments :: !Text
    , pendingRawArgumentChunks :: ![Text]
    , pendingRawArgumentChars :: !Int
    , retainedRawArgumentChars :: !Int
    }

emptyToolArgumentStreamState :: ToolArgumentStreamState
emptyToolArgumentStreamState = ToolArgumentStreamState
    { toolNamesById = Map.empty
    , toolCallsById = Map.empty
    , toolNamesByOutputIndex = IntMap.empty
    , toolCallsByOutputIndex = IntMap.empty
    , currentToolCall = Nothing
    , shellPreviewsByCallId = Map.empty
    , rawPreviewsByCallId = Map.empty
    , currentToolName = Nothing
    , streamedArgumentChars = 0
    , announcedArgumentChars = 0
    , warnedArgumentChars = 0
    }

toolArgumentStreamStep
    :: ResponseStreamEvent
    -> ToolArgumentStreamState
    -> (ToolArgumentStreamState, [LoopEvent])
toolArgumentStreamStep event state = case event of
    ResponseOutputItemAddedEvent
        { item = FunctionCallItem call, outputIndex } ->
        announceToolCall
            (responseItemToToolCall (FunctionCallItem call))
            (namespacedToolName call.namespace call.name)
            outputIndex
            ( maybeToList
                (ToolStreamItemId FunctionToolStream <$> call.itemId)
                <> [ToolStreamCallId FunctionToolStream call.callId]
            )
            state
    ResponseOutputItemAddedEvent
        { item = CustomToolCallItem call, outputIndex } ->
        announceToolCall
            (responseItemToToolCall (CustomToolCallItem call))
            (namespacedToolName call.namespace call.name)
            outputIndex
            ( maybeToList
                (ToolStreamItemId CustomToolStream <$> call.itemId)
                <> [ToolStreamCallId CustomToolStream call.callId]
            )
            state
    ResponseOutputItemDoneEvent
        { item = FunctionCallItem responseCall, outputIndex }
        | Just call <- responseItemToToolCall (FunctionCallItem responseCall) ->
            finishOutputItemToolCall
                outputIndex
                [ ToolStreamItemId FunctionToolStream <$> responseCall.itemId
                , Just
                    (ToolStreamCallId
                        FunctionToolStream
                        responseCall.callId)
                ]
                call
                state
    ResponseOutputItemDoneEvent
        { item = CustomToolCallItem responseCall, outputIndex }
        | Just call <- responseItemToToolCall
            (CustomToolCallItem responseCall) ->
            finishOutputItemToolCall
                outputIndex
                [ ToolStreamItemId CustomToolStream <$> responseCall.itemId
                , Just
                    (ToolStreamCallId
                        CustomToolStream
                        responseCall.callId)
                ]
                call
                state
    ResponseFunctionCallArgumentsDeltaEvent
        { delta = Just deltaText, streamItemId, streamOutputIndex } ->
        updateToolArguments
            streamOutputIndex
            [ToolStreamItemId FunctionToolStream <$> streamItemId]
            deltaText
            state
    ResponseFunctionCallArgumentsDoneEvent
        { arguments, streamItemId, streamOutputIndex } ->
            finishToolArguments
                streamOutputIndex
                [ToolStreamItemId FunctionToolStream <$> streamItemId]
                arguments
                state
    ResponseCustomToolInputDeltaEvent
        { delta = Just deltaText, streamItemId, streamCallId
        , streamOutputIndex } ->
            updateToolArguments
                streamOutputIndex
                [ ToolStreamItemId CustomToolStream <$> streamItemId
                , ToolStreamCallId CustomToolStream <$> streamCallId
                ]
                deltaText
                state
    ResponseCustomToolInputDoneEvent
        { inputText, streamItemId, streamCallId, streamOutputIndex } ->
            finishToolArguments
                streamOutputIndex
                [ ToolStreamItemId CustomToolStream <$> streamItemId
                , ToolStreamCallId CustomToolStream <$> streamCallId
                ]
                inputText
                state
    _ -> (state, [])

announceToolCall
    :: Maybe ToolCall
    -> Text
    -> Maybe Int
    -> [ToolStreamIdentity]
    -> ToolArgumentStreamState
    -> (ToolArgumentStreamState, [LoopEvent])
announceToolCall maybeCall name outputIndex identities state =
    ( state
        { toolNamesById =
            foldr (\identity -> Map.insert identity name)
                state.toolNamesById
                identities
        , toolCallsById =
            case maybeCall of
                Nothing -> state.toolCallsById
                Just call ->
                    foldr (\identity -> Map.insert identity call)
                        state.toolCallsById
                        identities
        , toolNamesByOutputIndex =
            maybe state.toolNamesByOutputIndex
                (\index -> IntMap.insert index name
                    state.toolNamesByOutputIndex)
                outputIndex
        , toolCallsByOutputIndex =
            case (outputIndex, maybeCall) of
                (Just index, Just call) ->
                    IntMap.insert index call state.toolCallsByOutputIndex
                _ -> state.toolCallsByOutputIndex
        , currentToolCall = maybeCall <|> state.currentToolCall
        , currentToolName = Just name
        }
    , [ ActivityUpdated (writingToolCallActivity name Nothing)
      | maybe True (not . supportsLiveArgumentPreview) maybeCall
      ]
    )

finishOutputItemToolCall
    :: Maybe Int
    -> [Maybe ToolStreamIdentity]
    -> ToolCall
    -> ToolArgumentStreamState
    -> (ToolArgumentStreamState, [LoopEvent])
finishOutputItemToolCall outputIndex identities call state =
    let previousCall = resolveToolCall outputIndex identities state
        canRecover =
            supportsLiveArgumentPreview call
                && maybe True supportsLiveArgumentPreview previousCall
        baseCall
            | canRecover
            , Text.null call.arguments
            , Just previous <- previousCall =
                withToolArguments call previous.arguments
            | otherwise = call
        (next, completedCall)
            | canRecover
            , Text.null call.arguments =
                let prefixChars
                        | isLiveShellTool call.name =
                            liveShellArgumentPrefixChars
                        | otherwise = liveRawArgumentPrefixChars
                    (finished, recovered, _) =
                        finishBufferedLiveCall
                            prefixChars
                            baseCall
                            Nothing
                            state
                in (finished, recovered)
            | otherwise = (trackUpdatedToolCall call state, call)
    in (next, [ToolUpdated completedCall])

resolveToolName
    :: Maybe Int
    -> [Maybe ToolStreamIdentity]
    -> ToolArgumentStreamState
    -> Text
resolveToolName outputIndex identities state =
    fromMaybe "tool" $
        resolveToolValue
            outputIndex
            identities
            state.toolNamesByOutputIndex
            state.toolNamesById
            state.currentToolName

resolveToolCall
    :: Maybe Int
    -> [Maybe ToolStreamIdentity]
    -> ToolArgumentStreamState
    -> Maybe ToolCall
resolveToolCall outputIndex identities state =
    resolveToolValue
        outputIndex
        identities
        state.toolCallsByOutputIndex
        state.toolCallsById
        state.currentToolCall

resolveToolValue
    :: Maybe Int
    -> [Maybe ToolStreamIdentity]
    -> IntMap value
    -> Map ToolStreamIdentity value
    -> Maybe value
    -> Maybe value
resolveToolValue outputIndex identities byOutputIndex byIdentity fallback =
    (outputIndex >>= (`IntMap.lookup` byOutputIndex))
        <|> firstJust
            [ Map.lookup identity byIdentity
            | Just identity <- identities
            ]
        <|> if hasLocator then Nothing else fallback
  where
    hasLocator = isJust outputIndex || any isJust identities
    firstJust = foldr (<|>) Nothing

-- Keep live previews useful without retaining unbounded repeated strict Text
-- values for runaway calls. Shell previews only need one command line, while
-- raw calls need enough source to show a representative diff or structured
-- argument preview.
liveShellArgumentPrefixChars :: Int
liveShellArgumentPrefixChars = 4096

liveRawArgumentPrefixChars :: Int
liveRawArgumentPrefixChars = 64 * 1024

-- Structured JSON calls normally reveal their useful path, pattern, or query
-- near the start. Publish that prefix eagerly, then batch tiny provider deltas.
-- apply_patch remains batched after its first fragment because patch bodies can
-- be large and are useful even when the first fragment already has a header.
liveStructuredArgumentEagerChars :: Int
liveStructuredArgumentEagerChars = 128

-- Shell commands remain eager while their useful prefix is forming, then use
-- smaller batches than source-shaped arguments so the command still feels
-- live without reparsing and repainting a long prefix for every tiny delta.
liveShellArgumentPublishChunkChars :: Int
liveShellArgumentPublishChunkChars = 64

liveRawArgumentPublishChunkChars :: Int
liveRawArgumentPublishChunkChars = 256

updateToolArguments
    :: Maybe Int
    -> [Maybe ToolStreamIdentity]
    -> Text
    -> ToolArgumentStreamState
    -> (ToolArgumentStreamState, [LoopEvent])
updateToolArguments outputIndex identities delta state =
    let name = resolveToolName outputIndex identities state
        maybeCall = resolveToolCall outputIndex identities state
        (withDraft, previewEvents) = case maybeCall of
            Just call
                | not (supportsLiveArgumentPreview call) ->
                    (state, [])
                | isLiveShellTool call.name ->
                    updateLiveShellCall call delta state
                | otherwise ->
                    updateLiveRawCall
                        (if isApplyPatchTool call.name
                            then 0
                            else liveStructuredArgumentEagerChars)
                        call
                        delta
                        state
            _ -> (state, [])
        (counted, activityEvents) =
            countToolArgumentChars
                name
                (maybe False supportsLiveArgumentPreview maybeCall)
                (Text.length delta)
                withDraft
    in (counted, previewEvents <> activityEvents)

finishToolArguments
    :: Maybe Int
    -> [Maybe ToolStreamIdentity]
    -> Maybe Text
    -> ToolArgumentStreamState
    -> (ToolArgumentStreamState, [LoopEvent])
finishToolArguments outputIndex identities completeArguments state =
    case resolveToolCall outputIndex identities state of
        Just call
            | not (supportsLiveArgumentPreview call) -> (state, [])
            | isLiveShellTool call.name ->
                finishLiveShellCall call completeArguments state
            | otherwise ->
                finishLiveRawCall call completeArguments state
        Nothing -> (state, [])

updateLiveShellCall
    :: ToolCall
    -> Text
    -> ToolArgumentStreamState
    -> (ToolArgumentStreamState, [LoopEvent])
updateLiveShellCall call delta state =
    case updateBufferedLiveCall
            liveStructuredArgumentEagerChars
            liveShellArgumentPublishChunkChars
            liveShellArgumentPrefixChars
            call
            delta
            state of
        (next, Nothing) -> (next, [])
        (next, Just updatedCall) ->
            publishLiveShellPreview updatedCall next

finishLiveShellCall
    :: ToolCall
    -> Maybe Text
    -> ToolArgumentStreamState
    -> (ToolArgumentStreamState, [LoopEvent])
finishLiveShellCall call completeArguments state =
    let (next, updatedCall, _) =
            finishBufferedLiveCall
                liveShellArgumentPrefixChars
                call
                completeArguments
                state
    in publishLiveShellPreview updatedCall next

publishLiveShellPreview
    :: ToolCall
    -> ToolArgumentStreamState
    -> (ToolArgumentStreamState, [LoopEvent])
publishLiveShellPreview call state =
    let key = toolPreviewKey call
        maybeCommand = jsonTextFieldPartial "command" call.arguments
        preview = Text.takeWhile (/= '\n') <$> maybeCommand
        previousPreview = Map.lookup key state.shellPreviewsByCallId
        changed = maybe False
            (\value -> not (Text.null value) && Just value /= previousPreview)
            preview
        displayCall command =
            withToolArguments call $
                Text.decodeUtf8
                    (LBS.toStrict
                        (Aeson.encode (Aeson.object ["command" Aeson..= command])))
        next = state
            { shellPreviewsByCallId =
                maybe state.shellPreviewsByCallId
                    (\value -> Map.insert key value
                        state.shellPreviewsByCallId)
                    preview
            }
    in
    ( next
    , [ToolArgumentsUpdated (displayCall command)
      | changed
      , command <- maybeToList preview
      ]
    )

updateLiveRawCall
    :: Int
    -> ToolCall
    -> Text
    -> ToolArgumentStreamState
    -> (ToolArgumentStreamState, [LoopEvent])
updateLiveRawCall eagerChars call delta state =
    case updateBufferedLiveCall
            eagerChars
            liveRawArgumentPublishChunkChars
            liveRawArgumentPrefixChars
            call
            delta
            state of
        (next, Nothing) -> (next, [])
        (next, Just updatedCall) ->
            (next, [ToolArgumentsUpdated updatedCall])

finishLiveRawCall
    :: ToolCall
    -> Maybe Text
    -> ToolArgumentStreamState
    -> (ToolArgumentStreamState, [LoopEvent])
finishLiveRawCall call completeArguments state =
    let (next, updatedCall, changed) =
            finishBufferedLiveCall
                liveRawArgumentPrefixChars
                call
                completeArguments
                state
    in
    ( next
    , [ToolArgumentsUpdated updatedCall | changed]
    )

updateBufferedLiveCall
    :: Int
    -> Int
    -> Int
    -> ToolCall
    -> Text
    -> ToolArgumentStreamState
    -> (ToolArgumentStreamState, Maybe ToolCall)
updateBufferedLiveCall
    eagerChars
    publishChunkChars
    prefixChars
    call
    delta
    state =
    let key = toolPreviewKey call
        preview = Map.findWithDefault
            (initialRawArgumentPreview prefixChars call)
            key
            state.rawPreviewsByCallId
        room = prefixChars - preview.retainedRawArgumentChars
        retainedDelta
            | room <= 0 = ""
            | Text.length delta <= room = delta
            | otherwise = Text.copy (Text.take room delta)
        retainedDeltaChars = Text.length retainedDelta
        pendingChars =
            preview.pendingRawArgumentChars + retainedDeltaChars
        withDelta = preview
            { pendingRawArgumentChunks =
                if retainedDeltaChars > 0
                    then retainedDelta : preview.pendingRawArgumentChunks
                    else preview.pendingRawArgumentChunks
            , pendingRawArgumentChars = pendingChars
            , retainedRawArgumentChars =
                preview.retainedRawArgumentChars + retainedDeltaChars
            }
        shouldPublish =
            retainedDeltaChars > 0
                && ( Text.null preview.publishedRawArguments
                    || withDelta.retainedRawArgumentChars <= eagerChars
                    || ( preview.retainedRawArgumentChars < eagerChars
                        && withDelta.retainedRawArgumentChars > eagerChars
                       )
                    || pendingChars >= publishChunkChars
                    || withDelta.retainedRawArgumentChars == prefixChars
                   )
        rawArguments =
            preview.publishedRawArguments
                <> Text.concat (reverse withDelta.pendingRawArgumentChunks)
        published = withDelta
            { publishedRawArguments = rawArguments
            , pendingRawArgumentChunks = []
            , pendingRawArgumentChars = 0
            }
        nextPreview = if shouldPublish then published else withDelta
        withPreview = state
            { rawPreviewsByCallId =
                Map.insert key nextPreview state.rawPreviewsByCallId
            }
        updatedCall = withToolArguments call rawArguments
    in if shouldPublish
        then
            ( trackUpdatedToolCall updatedCall withPreview
            , Just updatedCall
            )
        else (withPreview, Nothing)

finishBufferedLiveCall
    :: Int
    -> ToolCall
    -> Maybe Text
    -> ToolArgumentStreamState
    -> (ToolArgumentStreamState, ToolCall, Bool)
finishBufferedLiveCall prefixChars call completeArguments state =
    let key = toolPreviewKey call
        preview = Map.findWithDefault
            (initialRawArgumentPreview prefixChars call)
            key
            state.rawPreviewsByCallId
        accumulated =
            preview.publishedRawArguments
                <> Text.concat (reverse preview.pendingRawArgumentChunks)
        rawArguments =
            Text.copy
                (Text.take prefixChars
                    (fromMaybe accumulated completeArguments))
        finalPreview = RawArgumentPreview
            { publishedRawArguments = rawArguments
            , pendingRawArgumentChunks = []
            , pendingRawArgumentChars = 0
            , retainedRawArgumentChars = Text.length rawArguments
            }
        updatedCall = withToolArguments call rawArguments
        next =
            trackUpdatedToolCall updatedCall state
                { rawPreviewsByCallId =
                    Map.insert key finalPreview state.rawPreviewsByCallId
                }
    in (next, updatedCall, rawArguments /= call.arguments)

initialRawArgumentPreview :: Int -> ToolCall -> RawArgumentPreview
initialRawArgumentPreview prefixChars call =
    let initial =
            Text.copy
                (Text.take prefixChars call.arguments)
    in RawArgumentPreview
        { publishedRawArguments = initial
        , pendingRawArgumentChunks = []
        , pendingRawArgumentChars = 0
        , retainedRawArgumentChars = Text.length initial
        }

trackUpdatedToolCall
    :: ToolCall
    -> ToolArgumentStreamState
    -> ToolArgumentStreamState
trackUpdatedToolCall updatedCall state =
    state
        { toolCallsById =
            Map.map
                (\known ->
                    if sameStreamedToolCall known updatedCall
                        then updatedCall
                        else known)
                state.toolCallsById
        , toolCallsByOutputIndex =
            IntMap.map
                (\known ->
                    if sameStreamedToolCall known updatedCall
                        then updatedCall
                        else known)
                state.toolCallsByOutputIndex
        , currentToolCall =
            fmap
                (\known ->
                    if sameStreamedToolCall known updatedCall
                        then updatedCall
                        else known)
                state.currentToolCall
        }

sameStreamedToolCall :: ToolCall -> ToolCall -> Bool
sameStreamedToolCall left right =
    left.callId == right.callId
        && toolStreamKind left == toolStreamKind right

toolPreviewKey :: ToolCall -> ToolPreviewKey
toolPreviewKey call = ToolPreviewKey (toolStreamKind call) call.callId

toolStreamKind :: ToolCall -> ToolStreamKind
toolStreamKind call = case call.callKind of
    CustomCallKind -> CustomToolStream
    _ -> FunctionToolStream

isApplyPatchTool :: Text -> Bool
isApplyPatchTool name =
    canonicalToolName name == "apply_patch"

isLiveShellTool :: Text -> Bool
isLiveShellTool name =
    canonicalToolName name `elem` ["shell_command", "run_terminal_cmd"]

supportsLiveArgumentPreview :: ToolCall -> Bool
supportsLiveArgumentPreview call =
    not call.argumentsEncrypted
        && not (isComputerToolCallKind call.callKind)
        && canonicalToolName call.name /= "computer"

withToolArguments :: ToolCall -> Text -> ToolCall
withToolArguments call arguments = setToolCallArguments arguments call

countToolArgumentChars
    :: Text
    -> Bool
    -> Int
    -> ToolArgumentStreamState
    -> (ToolArgumentStreamState, [LoopEvent])
countToolArgumentChars name hasLivePreview deltaChars state =
    let total = state.streamedArgumentChars + deltaChars
        announce =
            total - state.announcedArgumentChars
                >= toolArgumentActivityChunkChars
        warn =
            total - state.warnedArgumentChars
                >= runawayToolArgumentWarningChars
    in
    ( state
        { streamedArgumentChars = total
        , announcedArgumentChars =
            if announce then total else state.announcedArgumentChars
        , warnedArgumentChars =
            if warn then total else state.warnedArgumentChars
        }
    , [ ActivityUpdated (writingToolCallActivity name (Just total))
      | announce
      , not hasLivePreview
      ]
        <> [ WarningRaised (runawayToolArgumentWarning name total)
           | warn
           ]
    )

writingToolCallActivity :: Text -> Maybe Int -> Text
writingToolCallActivity name total =
    "Writing " <> name <> " call…"
        <> foldMap
            (\chars -> " (" <> formatCharCount chars <> ")")
            total

runawayToolArgumentWarning :: Text -> Int -> Text
runawayToolArgumentWarning name total =
    "The model has streamed "
        <> formatCharCount total
        <> " of "
        <> name
        <> " arguments in one response; it may be stuck in a repetition loop."

formatCharCount :: Int -> Text
formatCharCount chars
    | chars >= 10000 =
        Text.pack (show (chars `div` 1000)) <> "k chars"
    | otherwise = Text.pack (show chars) <> " chars"

-- | Whether a stream event proves the provider has begun producing response
-- output. These events make replay unsafe even when they do not map to a
-- visible loop delta.
streamOutputObserved :: ResponseStreamEvent -> Bool
streamOutputObserved event = case event of
    ResponseCompletedEvent{} -> True
    ResponseDoneEvent{} -> True
    ResponseIncompleteEvent { responseValue } ->
        not (null responseValue.output)
    ResponseFailedEvent { responseValue } ->
        not (null responseValue.output)
    ResponseOutputItemAddedEvent{} -> True
    ResponseOutputItemDoneEvent{} -> True
    ResponseFunctionCallArgumentsDeltaEvent{} -> True
    ResponseFunctionCallArgumentsDoneEvent{} -> True
    ResponseCustomToolInputDeltaEvent{} -> True
    ResponseCustomToolInputDoneEvent{} -> True
    ResponseReasoningSummaryPartAddedEvent{} -> True
    ResponseReasoningSummaryTextDoneEvent{} -> True
    OtherResponseStreamEvent { otherEventType }
        | streamEventTypeText otherEventType == unparsedStreamEventTypeText ->
            False
    _ ->
        responseStreamEventType event /= EventCodexRateLimits
            && isJust (streamEventToLoopEvent event)
