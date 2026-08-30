-- | Provider-neutral loop adapters for Responses-compatible transports.
module Agent.Responses.LoopBackend
    ( statelessResponsesBackend
    , statelessResponsesBackendWithRawReasoning
    , tokenProviderStatelessResponsesBackend
    , turnInputsToItems
    , responseToTurnOutput
    , responseItemToToolCall
    , responseTokenUsage
    , streamEventToLoopEvent
    , streamEventToLoopEventWithRawReasoning
    , newStreamEventToLoopEvents
    , toolArgumentActivityChunkChars
    , runawayToolArgumentWarningChars
    , streamOutputObserved
    , hasRecoverableIncompleteOutput
    , responseNeedsLoopContinuation
    , assistantTextFromResponse
    , toolResultToItem
    , withRequestInput
    ) where

import Agent.Error (ApiError)
import Agent.InterAgentMessage
    ( InterAgentMessage(..)
    , InterAgentMessageContent(..)
    , renderInterAgentMessage
    , renderInterAgentMessageHeader
    )
import Agent.Json (RawJson, rawJsonFromEncoding)
import Agent.JsonText (jsonTextFieldPartial)
import Agent.Loop
    ( Backend(..)
    , BackendResult(..)
    , FileAttachment(..)
    , ImageAttachment(..)
    , LoopEvent(..)
    , TokenUsage(..)
    , TurnAttachment(..)
    , TurnCompletion(..)
    , TurnInput(..)
    , TurnOutput(..)
    , emptyTokenUsage
    )
import Agent.Provider
    ( Credential
    , TokenProvider
    , runWithTokenProvider
    )
import Agent.Responses.Request (stripReplayedItemStatus)
import Agent.Responses.Types
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallKind(..)
    , ToolCallResult(..)
    , ToolResultImage(..)
    , canonicalToolName
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
    Backend \history _previousResponseId inputs onEvent -> do
        baseParams <- getParams
        projectEvent <- newStreamEventToLoopEvents showRawReasoning
        let newItems = turnInputsToItems inputs
            requestItems = history <> newItems
            request = withRequestInput baseParams requestItems
        result <- send request \event ->
            projectEvent event >>= mapM_ onEvent
        case result of
            Left err -> pure (Left err)
            Right response ->
                pure $ Right BackendResult
                    { backendOutput = responseToTurnOutput response
                    , backendState = requestItems <> response.output
                    }

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

-- | 'input' is also a field on 'CustomToolCall', so a record update is
-- ambiguous. Rebuild from the constructor instead.
withRequestInput :: ResponseCreateParams -> [ResponseItem] -> ResponseCreateParams
withRequestInput ResponseCreateParams{..} items =
    let prefix = requestInputPrefix input
        -- Replayed transcript items are provider output; drop their lifecycle
        -- status before they become input (see 'stripReplayedItemStatus').
        normalizedItems =
            map (normalizeRequestItem . stripReplayedItemStatus) items
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
            , output = callOutput.output
            , status = callOutput.status

            }
    CustomToolCallOutputItem callOutput ->
        CustomToolCallOutputItem CustomToolCallOutput
            { itemId = callOutput.itemId
            , callId = callOutput.callId
            , name = callOutput.name
            , output = callOutput.output
            , status = callOutput.status

            }
    item -> item
  where
    stripContentPart = \case
        InputImagePart{..} -> InputImagePart { detail = Nothing, .. }
        part -> part

-- Older local compaction snapshots accidentally persisted assistant summaries
-- as input_text. Responses input accepts assistant history, but its content
-- parts must use output_text (or refusal). Repair those snapshots at the wire
-- boundary so resumed sessions recover without rewriting their session files.
normalizeRequestItem :: ResponseItem -> ResponseItem
normalizeRequestItem = \case
    MessageItem message
        | message.role == RoleAssistant ->
            MessageItem ResponseMessage
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
    item -> item

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
turnInputsToItems = map turnInputToItem

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
    ComputerCallKind -> case Hermes.decodeEither computerCallOutputDecoder
            (Text.encodeUtf8 result.output) of
        Right output -> ComputerCallOutputItem output
            { computerOutputCallId = result.callId }
        Left _ -> ComputerCallOutputItem ComputerCallOutput
            { computerOutputItemId = Nothing
            , computerOutputCallId = result.callId
            , screenshotDataUrl = transparentPixelDataUrl
            , acknowledgedChecks = []
            , computerOutputStatus = Nothing
            , computerOutputExtra = KeyMap.empty
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

transparentPixelDataUrl :: Text
transparentPixelDataUrl =
    "data:image/png;base64,"
        <> "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVQ"
        <> "IHWP4z8DwHwAFgAI/ScL7WQAAAABJRU5ErkJggg=="
responseToTurnOutput :: Response -> TurnOutput
responseToTurnOutput response = TurnOutput
    { responseId = response.responseId
    , toolCalls = mapMaybe responseItemToToolCall response.output
    , assistantText = assistantTextFromResponse response
    , tokenUsage = responseTokenUsage response
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
-- On top of 'streamEventToLoopEventWithRawReasoning' this surfaces streamed
-- shell arguments as a repaintable command preview and reports coarse activity
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
    stateRef <- newIORef emptyToolArgumentStreamState
    pure \event -> do
        argumentEvents <- atomicModifyIORef' stateRef \state ->
            toolArgumentStreamStep event state
        pure $
            maybeToList
                (streamEventToLoopEventWithRawReasoning showRawReasoning event)
                <> maybeToList (codexRateLimitsUpdate event)
                <> argumentEvents

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
    , currentToolName :: !(Maybe Text)
    , streamedArgumentChars :: !Int
    , announcedArgumentChars :: !Int
    , warnedArgumentChars :: !Int
    }

emptyToolArgumentStreamState :: ToolArgumentStreamState
emptyToolArgumentStreamState = ToolArgumentStreamState
    { toolNamesById = Map.empty
    , toolCallsById = Map.empty
    , currentToolCall = Nothing
    , shellPreviewsByCallId = Map.empty
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
      | not (isLiveShellTool name)
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

-- Keep enough raw arguments to render a useful one-line shell preview without
-- accumulating an unbounded repeated strict Text value for runaway calls.
liveToolArgumentPrefixChars :: Int
liveToolArgumentPrefixChars = 4096

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
            Text.take liveToolArgumentPrefixChars (call.arguments <> delta)
        updatedCall = withToolArguments call rawArguments
        updatedCalls =
            Map.map
                (\known ->
                    if known.callId == call.callId then updatedCall else known)
                state.toolCallsById
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
        next = state
            { toolCallsById = updatedCalls
            , currentToolCall = Just updatedCall
            , shellPreviewsByCallId =
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
      , not (isLiveShellTool name)
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
