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
import Agent.Loop
    ( Backend(..)
    , BackendResult(..)
    , FileAttachment(..)
    , ImageAttachment(..)
    , LoopEvent(..)
    , TokenUsage(..)
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
import Agent.Responses.StreamAssembly (responseFragmentHasOutput)
import Agent.Responses.Types
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallKind(..)
    , ToolCallResult(..)
    )
import Control.Applicative ((<|>))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified "base64-bytestring" Data.ByteString.Base64 as Base64
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Text.Printf (printf)

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
        normalizedItems = map normalizeRequestItem items
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
            , extraFields = message.extraFields
            }
    FunctionCallOutputItem callOutput ->
        FunctionCallOutputItem FunctionCallOutput
            { itemId = callOutput.itemId
            , callId = callOutput.callId
            , name = callOutput.name
            , namespace = callOutput.namespace
            , output = stripInputImageDetailValue callOutput.output
            , status = callOutput.status
            , extraFields = callOutput.extraFields
            }
    CustomToolCallOutputItem callOutput ->
        CustomToolCallOutputItem CustomToolCallOutput
            { itemId = callOutput.itemId
            , callId = callOutput.callId
            , name = callOutput.name
            , output = stripInputImageDetailValue callOutput.output
            , status = callOutput.status
            , extraFields = callOutput.extraFields
            }
    item -> item
  where
    stripContentPart = \case
        InputImagePart{..} -> InputImagePart { detail = Nothing, .. }
        part -> part

stripInputImageDetailValue :: Aeson.Value -> Aeson.Value
stripInputImageDetailValue = \case
    Aeson.Object object ->
        let nested = KeyMap.map stripInputImageDetailValue object
        in Aeson.Object $ case KeyMap.lookup (Key.fromText "type") nested of
            Just (Aeson.String "input_image") ->
                KeyMap.delete (Key.fromText "detail") nested
            _ -> nested
    Aeson.Array values ->
        Aeson.Array (fmap stripInputImageDetailValue values)
    value -> value

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
                            [OutputTextPart text Nothing Nothing KeyMap.empty]
                    MessageContentParts parts ->
                        MessageContentParts (map normalizeAssistantPart parts)
                , role = message.role
                , status = message.status
                , phase = message.phase
                , passthrough = message.passthrough
                , extraFields = message.extraFields
                }
    item -> item

normalizeAssistantPart :: ResponseContentPart -> ResponseContentPart
normalizeAssistantPart = \case
    InputTextPart { text, extraFields } ->
        OutputTextPart
            { text
            , annotations = Nothing
            , logprobs = Nothing
            , extraFields
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
        [OutputTextPart "" Nothing Nothing KeyMap.empty]
    , role = RoleAssistant
    , status = Nothing
    , phase = Nothing
    , passthrough = Nothing
    , extraFields = KeyMap.empty
    }

turnInputsToItems :: [TurnInput] -> [ResponseItem]
turnInputsToItems = map turnInputToItem

turnInputToItem :: TurnInput -> ResponseItem
turnInputToItem = \case
    UserMessage text -> userMessageItem text
    AgentMessage message -> agentMessageItem message
    UserMultimodal{userText, userImages} -> multimodalUserItem userText userImages
    UserMultimodalFiles{userText, userImages, userFiles} ->
        multimodalFilesItem userText userImages userFiles
    CompletedTool result -> toolResultToItem result

userMessageItem :: Text -> ResponseItem
userMessageItem text = MessageItem ResponseMessage
    { messageId = Nothing
    , content = MessageContentParts [InputTextPart text Nothing KeyMap.empty]
    , role = RoleUser
    , status = Nothing
    , phase = Nothing
    , passthrough = Nothing
    , extraFields = KeyMap.empty
    }

multimodalFilesItem :: Text -> [ImageAttachment] -> [FileAttachment] -> ResponseItem
multimodalFilesItem text images files = MessageItem ResponseMessage
    { messageId = Nothing
    , content = MessageContentParts
        ( InputTextPart text Nothing KeyMap.empty
        : map imageAttachmentPart images
        <> map fileAttachmentPart files
        )
    , role = RoleUser
    , status = Nothing
    , phase = Nothing
    , passthrough = Nothing
    , extraFields = KeyMap.empty
    }

agentMessageItem :: InterAgentMessage -> ResponseItem
agentMessageItem message = AgentMessageItem ResponseAgentMessage
    { messageId = Nothing
    , author = Just message.messageAuthor
    , recipient = Just message.messageRecipient
    , content = agentMessageContent message
    , passthrough = Nothing
    , extraFields = KeyMap.empty
    }

agentMessageContent :: InterAgentMessage -> [ResponseContentPart]
agentMessageContent message = case message.messageContent of
    PlainInterAgentContent _ ->
        [InputTextPart (renderInterAgentMessage message) Nothing KeyMap.empty]
    EncryptedInterAgentContent encrypted ->
        [ InputTextPart
            (renderInterAgentMessageHeader message)
            Nothing
            KeyMap.empty
        , EncryptedContentPart encrypted KeyMap.empty
        ]

multimodalUserItem :: Text -> [ImageAttachment] -> ResponseItem
multimodalUserItem text images = MessageItem ResponseMessage
    { messageId = Nothing
    , content = MessageContentParts
        ( InputTextPart text Nothing KeyMap.empty
        : map imageAttachmentPart images
        )
    , role = RoleUser
    , status = Nothing
    , phase = Nothing
    , passthrough = Nothing
    , extraFields = KeyMap.empty
    }

imageAttachmentPart :: ImageAttachment -> ResponseContentPart
imageAttachmentPart ImageAttachment{imageMime, imageBytes} =
    InputImagePart
        { detail = Just "auto"
        , fileId = Nothing
        , imageUrl = Just (imageDataUrl imageMime imageBytes)
        , promptCacheBreakpoint = Nothing
        , extraFields = KeyMap.empty
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
        , extraFields = KeyMap.empty
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
        , output = Aeson.String result.output
        , status = Nothing
        , extraFields = KeyMap.empty
        }
    CustomCallKind -> CustomToolCallOutputItem CustomToolCallOutput
        { itemId = Nothing
        , callId = result.callId
        , name = Nothing
        , output = Aeson.String result.output
        , status = Nothing
        , extraFields = KeyMap.empty
        }

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
    _ -> Nothing

data CodexRateLimitsPayload = CodexRateLimitsPayload
    { rateLimits :: !(Maybe CodexRateLimitDetails) }

data CodexRateLimitDetails = CodexRateLimitDetails
    { allowed :: !(Maybe Bool)
    , limitReached :: !(Maybe Bool)
    , primary :: !(Maybe CodexRateLimitWindow)
    , secondary :: !(Maybe CodexRateLimitWindow)
    }

data CodexRateLimitWindow = CodexRateLimitWindow
    { usedPercent :: !Double }

codexRateLimitsWarning :: Aeson.Object -> Maybe Text
codexRateLimitsWarning fields = do
    payload <- codexRateLimitsPayload fields
    do
            details <- payload.rateLimits
            let reportedWindows =
                    [ ("primary", window)
                    | window <- maybeToList details.primary
                    ]
                    <> [ ("secondary", window)
                       | window <- maybeToList details.secondary
                       ]
                lowWindows =
                    [ (label, window)
                    | (label, window) <- reportedWindows
                    , window.usedPercent >= 90
                    ]
                reached =
                    details.limitReached == Just True
                        || details.allowed == Just False
                        || any ((>= 100) . (.usedPercent) . snd) reportedWindows
            if not reached && null lowWindows
                then Nothing
                else
                    let headline
                            | reached = "Codex usage limit reached"
                            | otherwise = "Codex usage is low"
                        windows =
                            if null lowWindows && reached
                                then reportedWindows
                                else lowWindows
                        detail = case windows of
                            [] -> ""
                            values ->
                                ": "
                                    <> Text.intercalate " · "
                                        (map formatRateLimitWindow values)
                    in Just
                        (headline <> detail
                            <> ". Check /usage for reset details.")

codexRateLimitsPayload :: Aeson.Object -> Maybe CodexRateLimitsPayload
codexRateLimitsPayload object =
    pure (CodexRateLimitsPayload (objectField "rate_limits" object >>= details))
  where
    details fields = Just CodexRateLimitDetails
        { allowed = boolField "allowed" fields
        , limitReached = boolField "limit_reached" fields
        , primary = objectField "primary" fields >>= window
        , secondary = objectField "secondary" fields >>= window
        }
    window fields =
        CodexRateLimitWindow <$> numberField "used_percent" fields

objectField :: Text -> Aeson.Object -> Maybe Aeson.Object
objectField name object = case KeyMap.lookup (Key.fromText name) object of
    Just (Aeson.Object value) -> Just value
    _ -> Nothing

boolField :: Text -> Aeson.Object -> Maybe Bool
boolField name object = case KeyMap.lookup (Key.fromText name) object of
    Just (Aeson.Bool value) -> Just value
    _ -> Nothing

numberField :: Text -> Aeson.Object -> Maybe Double
numberField name object = case KeyMap.lookup (Key.fromText name) object of
    Just (Aeson.Number value) -> Just (realToFrac value)
    _ -> Nothing

formatRateLimitWindow :: (Text, CodexRateLimitWindow) -> Text
formatRateLimitWindow (label, window) =
    label <> " " <> formatPercent remaining <> "% left"
  where
    remaining = max 0 (min 100 (100 - window.usedPercent))

formatPercent :: Double -> Text
formatPercent value
    | abs (value - fromIntegral (round value :: Int)) < 0.05 =
        Text.pack (show (round value :: Int))
    | otherwise = Text.pack (printf "%.1f" value)

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
    ResponseReasoningSummaryPartAddedEvent
        { summaryIndex = Just index }
        | index > 0 ->
            Just (ReasoningDelta "\n\n")
    OtherResponseStreamEvent
        { otherEventType
        , eventExtraFields
        }
        | streamEventTypeText otherEventType == unparsedStreamEventTypeText ->
            Just (WarningRaised (unparsedStreamFrameWarning eventExtraFields))
    OtherResponseStreamEvent
        { otherEventType = StreamEventUnknown eventType
        , eventExtraFields
        } ->
            Just
                (ActivityUpdated
                    (unknownProviderEventWarning eventType eventExtraFields))
    OtherResponseStreamEvent
        { otherEventType = EventCodexRateLimits
        , eventExtraFields
        } ->
            WarningRaised <$> codexRateLimitsWarning eventExtraFields
    OtherResponseStreamEvent { otherEventType, eventExtraFields } ->
        case extraDeltaText eventExtraFields of
            Just text -> case otherEventType of
                EventOutputTextDelta -> Just (TextDelta text)
                EventReasoningTextDelta
                    | showRawReasoning -> Just (ReasoningDelta text)
                    | otherwise -> Nothing
                EventReasoningSummaryTextDelta -> Just (ReasoningDelta text)
                _ -> Nothing
            Nothing -> Nothing
    _ -> Nothing

-- | Stateful projection of one streamed response attempt into loop events.
--
-- On top of 'streamEventToLoopEventWithRawReasoning' this surfaces streamed
-- tool-call arguments as live activity. Argument deltas map to no visible
-- loop event on their own, so a model writing a large tool call — or stuck in
-- a degenerate repetition loop inside one (observed as multi-minute
-- 128k-output-token samples whose arguments repeat @\\u0000@ or a hallucinated
-- path segment) — previously looked like endless silent reasoning until the
-- provider's output-token cap ended the turn.
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
                <> argumentEvents

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
    , currentToolName :: !(Maybe Text)
    , streamedArgumentChars :: !Int
    , announcedArgumentChars :: !Int
    , warnedArgumentChars :: !Int
    }

emptyToolArgumentStreamState :: ToolArgumentStreamState
emptyToolArgumentStreamState = ToolArgumentStreamState
    { toolNamesById = Map.empty
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
        announceToolCall call.name
            (maybeToList call.itemId <> [call.callId])
            state
    ResponseOutputItemAddedEvent { item = CustomToolCallItem call } ->
        announceToolCall call.name
            (maybeToList call.itemId <> [call.callId])
            state
    ResponseFunctionCallArgumentsDeltaEvent { delta = Just deltaText, streamItemId } ->
        countToolArgumentChars
            (resolveToolName [streamItemId] state)
            (Text.length deltaText)
            state
    ResponseCustomToolInputDeltaEvent
        { delta = Just deltaText, streamItemId, streamCallId } ->
            countToolArgumentChars
                (resolveToolName [streamItemId, streamCallId] state)
                (Text.length deltaText)
                state
    _ -> (state, [])

announceToolCall
    :: Text
    -> [Text]
    -> ToolArgumentStreamState
    -> (ToolArgumentStreamState, [LoopEvent])
announceToolCall name identities state =
    ( state
        { toolNamesById =
            foldr (\identity -> Map.insert identity name)
                state.toolNamesById
                identities
        , currentToolName = Just name
        }
    , [ActivityUpdated (writingToolCallActivity name Nothing)]
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
        responseFragmentHasOutput responseValue
    ResponseFailedEvent { responseValue } ->
        responseFragmentHasOutput responseValue
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

unknownProviderEventWarning :: Text -> Aeson.Object -> Text
unknownProviderEventWarning eventType extras =
    "Warning: unsupported provider event "
        <> eventType
        <> foldMap (": " <>) (objectPreview extras)

unparsedStreamFrameWarning :: Aeson.Object -> Text
unparsedStreamFrameWarning extras =
    "Codex websocket dropped an unparsed frame"
        <> foldMap (": " <>) (nonEmptyText extras "error")
        <> foldMap (" payload=" <>) (nonEmptyText extras "payload")

objectPreview :: Aeson.Object -> Maybe Text
objectPreview extras
    | KeyMap.null extras = Nothing
    | otherwise =
        Just
            . Text.take 500
            . Text.decodeUtf8Lenient
            . LBS.toStrict
            . Aeson.encode
            $ Aeson.Object extras

extraDeltaText :: Aeson.Object -> Maybe Text
extraDeltaText extras = nonEmptyText extras "delta" <|> nonEmptyText extras "text"

nonEmptyText :: Aeson.Object -> Text -> Maybe Text
nonEmptyText extras key = case KeyMap.lookup (Key.fromText key) extras of
    Just (Aeson.String text) | not (Text.null text) -> Just text
    _ -> Nothing
