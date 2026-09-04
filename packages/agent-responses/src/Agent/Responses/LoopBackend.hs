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
    , ToolCallResult(..)
    , ToolResultImage(..)
    , canonicalToolName
    , isComputerToolCallKind
    , toolCallResultImages
    )
import Control.Applicative ((<|>))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Hermes as Hermes
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified "base64-bytestring" Data.ByteString.Base64 as Base64
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
    Backend \snapshot _legacyPreviousResponseId inputs onEvent -> do
        baseParams <- getParams
        projectEvent <- newStreamEventToLoopEvents showRawReasoning
        let newItems = turnInputsToItems inputs
            requestItems = snapshot.backendItems <> newItems
            request = withRequestInput baseParams requestItems
        result <- send request \event ->
            projectEvent event >>= mapM_ onEvent
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

            }
    CustomToolCallOutputItem callOutput ->
        CustomToolCallOutputItem CustomToolCallOutput
            { itemId = callOutput.itemId
            , callId = callOutput.callId
            , name = callOutput.name
            , output = stripRawJsonImageDetails callOutput.output
            , status = callOutput.status

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

        }
    CustomCallKind -> CustomToolCallOutputItem CustomToolCallOutput
        { itemId = Nothing
        , callId = result.callId
        , name = Nothing
        , output = toolResultOutput result
        , status = Nothing
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
        Right ComputerCallOutput{} ->
            "Computer action completed."
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
            Just ToolCall
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
        in Just ToolCall
            { callId = call.callId
            , name = toolName
            , arguments = call.arguments
            , callKind = FunctionCallKind
            , argumentsEncrypted =
                encryptedCollaborationArguments
                    toolName
                    call.encryptedFunctionArgs
            }
    CustomToolCallItem call -> Just ToolCall
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
-- On top of 'streamEventToLoopEventWithRawReasoning' this surfaces selected
-- streamed arguments as repaintable tool previews and reports coarse activity
-- for other tools. It also warns when a model gets stuck in a degenerate
-- repetition loop inside one call (observed as multi-minute 128k-output-token
-- samples whose arguments repeat @\\u0000@ or a hallucinated path segment).
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
        (streamEventToLoopEventWithRawReasoning showRawReasoning event)
        <> maybeToList (codexRateLimitsUpdate event)
        <> argumentEvents
    )
  where
    (nextArguments, argumentEvents) =
        toolArgumentStreamStep event state.streamToolArguments

codexRateLimitsUpdate :: ResponseStreamEvent -> Maybe LoopEvent
codexRateLimitsUpdate = \case
    ResponseCodexRateLimitsEvent { rateLimits = limits } ->
        case limits.secondaryUsedPercent of
            Just used -> Just (limitUpdate "Weekly limit left" used)
            Nothing ->
                limitUpdate "5h limit left" <$> limits.primaryUsedPercent
    _ -> Nothing
  where
    limitUpdate label used =
        let remaining :: Int
            remaining = max 0 (min 100 (round (100 - used)))
        in ProviderLimitUpdated
            { providerLimitText =
                label <> ": " <> Text.pack (show remaining) <> "%"
            , providerLimitWarning = remaining <= 10
            }

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
    { toolNamesById :: !(Map Text Text)
    , toolCallsById :: !(Map Text ToolCall)
    , currentToolCall :: !(Maybe ToolCall)
    , shellPreviewsByCallId :: !(Map Text Text)
    , rawPreviewsByCallId :: !(Map Text RawArgumentPreview)
    , currentToolName :: !(Maybe Text)
    , streamedArgumentChars :: !Int
    , announcedArgumentChars :: !Int
    , warnedArgumentChars :: !Int
    }

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
    ResponseOutputItemAddedEvent { item = FunctionCallItem call } ->
        announceToolCall
            (responseItemToToolCall (FunctionCallItem call))
            (namespacedToolName call.namespace call.name)
            (maybeToList call.itemId <> [call.callId])
            state
    ResponseOutputItemAddedEvent { item = CustomToolCallItem call } ->
        announceToolCall
            (responseItemToToolCall (CustomToolCallItem call))
            (namespacedToolName call.namespace call.name)
            (maybeToList call.itemId <> [call.callId])
            state
    ResponseFunctionCallArgumentsDeltaEvent { delta = Just deltaText, streamItemId } ->
        updateToolArguments
            [streamItemId]
            deltaText
            state
    ResponseCustomToolInputDeltaEvent
        { delta = Just deltaText, streamItemId, streamCallId } ->
            updateToolArguments
                [streamItemId, streamCallId]
                deltaText
                state
    _ -> (state, [])

announceToolCall
    :: Maybe ToolCall
    -> Text
    -> [Text]
    -> ToolArgumentStreamState
    -> (ToolArgumentStreamState, [LoopEvent])
announceToolCall maybeCall name identities state =
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
        , currentToolCall = maybeCall <|> state.currentToolCall
        , currentToolName = Just name
        }
    , [ ActivityUpdated (writingToolCallActivity name Nothing)
      | not (isLiveArgumentTool name)
      ]
    )

resolveToolName :: [Maybe Text] -> ToolArgumentStreamState -> Text
resolveToolName identities state =
    fromMaybe (fromMaybe "tool" state.currentToolName) $
        firstJust
            [ Map.lookup identity state.toolNamesById
            | Just identity <- identities
            ]
  where
    firstJust = foldr (<|>) Nothing

resolveToolCall
    :: [Maybe Text]
    -> ToolArgumentStreamState
    -> Maybe ToolCall
resolveToolCall identities state =
    firstJust
        [ Map.lookup identity state.toolCallsById
        | Just identity <- identities
        ]
        <|> state.currentToolCall
  where
    firstJust = foldr (<|>) Nothing

-- Keep live previews useful without retaining unbounded repeated strict Text
-- values for runaway calls. Shell previews only need one command line, while
-- apply_patch needs enough source to show a representative multi-file diff.
liveShellArgumentPrefixChars :: Int
liveShellArgumentPrefixChars = 4096

liveRawArgumentPrefixChars :: Int
liveRawArgumentPrefixChars = 64 * 1024

-- Publish the first raw fragment immediately, then batch very small provider
-- deltas. Rebuilding a cumulative strict Text for every token is quadratic
-- and can also make terminal repainting dominate a long patch stream.
liveRawArgumentPublishChunkChars :: Int
liveRawArgumentPublishChunkChars = 256

updateToolArguments
    :: [Maybe Text]
    -> Text
    -> ToolArgumentStreamState
    -> (ToolArgumentStreamState, [LoopEvent])
updateToolArguments identities delta state =
    let name = resolveToolName identities state
        maybeCall = resolveToolCall identities state
        (withDraft, previewEvents) = case maybeCall of
            Just call
                | isLiveShellTool call.name ->
                    updateLiveShellCall call delta state
                | isLiveRawArgumentTool call.name ->
                    updateLiveRawCall call delta state
            _ -> (state, [])
        (counted, activityEvents) =
            countToolArgumentChars name (Text.length delta) withDraft
    in (counted, previewEvents <> activityEvents)

updateLiveShellCall
    :: ToolCall
    -> Text
    -> ToolArgumentStreamState
    -> (ToolArgumentStreamState, [LoopEvent])
updateLiveShellCall call delta state =
    let rawArguments =
            appendArgumentPrefix
                liveShellArgumentPrefixChars
                call.arguments
                delta
        updatedCall = withToolArguments call rawArguments
        maybeCommand = jsonTextFieldPartial "command" rawArguments
        preview = Text.takeWhile (/= '\n') <$> maybeCommand
        previousPreview = Map.lookup call.callId state.shellPreviewsByCallId
        changed = maybe False
            (\value -> not (Text.null value) && Just value /= previousPreview)
            preview
        displayCall command =
            withToolArguments updatedCall $
                Text.decodeUtf8
                    (LBS.toStrict
                        (Aeson.encode (Aeson.object ["command" Aeson..= command])))
        next = (trackUpdatedToolCall updatedCall state)
            { shellPreviewsByCallId =
                maybe state.shellPreviewsByCallId
                    (\value -> Map.insert call.callId value
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
    :: ToolCall
    -> Text
    -> ToolArgumentStreamState
    -> (ToolArgumentStreamState, [LoopEvent])
updateLiveRawCall call delta state =
    let preview = Map.findWithDefault
            (initialRawArgumentPreview call)
            call.callId
            state.rawPreviewsByCallId
        room =
            liveRawArgumentPrefixChars - preview.retainedRawArgumentChars
        retainedDelta = Text.copy (Text.take room delta)
        retainedDeltaChars = Text.length retainedDelta
        pendingChars =
            preview.pendingRawArgumentChars + retainedDeltaChars
        withDelta = preview
            { pendingRawArgumentChunks =
                [retainedDelta | retainedDeltaChars > 0]
                    <> preview.pendingRawArgumentChunks
            , pendingRawArgumentChars = pendingChars
            , retainedRawArgumentChars =
                preview.retainedRawArgumentChars + retainedDeltaChars
            }
        shouldPublish =
            retainedDeltaChars > 0
                && ( Text.null preview.publishedRawArguments
                    || pendingChars >= liveRawArgumentPublishChunkChars
                    || withDelta.retainedRawArgumentChars
                        == liveRawArgumentPrefixChars
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
                Map.insert call.callId nextPreview state.rawPreviewsByCallId
            }
        updatedCall = withToolArguments call rawArguments
    in if shouldPublish
        then
            ( trackUpdatedToolCall updatedCall withPreview
            , [ToolArgumentsUpdated updatedCall]
            )
        else (withPreview, [])

initialRawArgumentPreview :: ToolCall -> RawArgumentPreview
initialRawArgumentPreview call =
    let initial =
            Text.copy
                (Text.take liveRawArgumentPrefixChars call.arguments)
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
                    if known.callId == updatedCall.callId
                        then updatedCall
                        else known)
                state.toolCallsById
        , currentToolCall = Just updatedCall
        }

appendArgumentPrefix :: Int -> Text -> Text -> Text
appendArgumentPrefix limit previous delta
    | room <= 0 = previous
    | Text.null previous = retainedDelta
    | otherwise = previous <> retainedDelta
  where
    room = limit - Text.length previous
    retainedDelta = Text.copy (Text.take room delta)

isLiveArgumentTool :: Text -> Bool
isLiveArgumentTool name =
    isLiveShellTool name || isLiveRawArgumentTool name

isLiveRawArgumentTool :: Text -> Bool
isLiveRawArgumentTool name =
    canonicalToolName name `elem` ["apply_patch"]

isLiveShellTool :: Text -> Bool
isLiveShellTool name =
    canonicalToolName name `elem` ["shell_command", "run_terminal_cmd"]

withToolArguments :: ToolCall -> Text -> ToolCall
withToolArguments ToolCall
    { callId
    , name
    , callKind
    , argumentsEncrypted
    } arguments =
        ToolCall
            { callId
            , name
            , arguments
            , callKind
            , argumentsEncrypted
            }

countToolArgumentChars
    :: Text
    -> Int
    -> ToolArgumentStreamState
    -> (ToolArgumentStreamState, [LoopEvent])
countToolArgumentChars name deltaChars state =
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
      , not (isLiveArgumentTool name)
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
