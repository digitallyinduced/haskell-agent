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
    , streamOutputObserved
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
import qualified "base64-bytestring" Data.ByteString.Base64 as Base64
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
        let newItems = turnInputsToItems inputs
            requestItems = history <> newItems
            request = withRequestInput baseParams requestItems
        result <- send request \event ->
            mapM_ onEvent
                (streamEventToLoopEventWithRawReasoning showRawReasoning event)
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
                map stripResponsesLiteImageDetails normalizedItems
            | otherwise = normalizedItems
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
    UnknownResponseItem TaggedObject { tag = "additional_tools" } -> True
    _ -> False

isBaseInstructions :: ResponseItem -> Bool
isBaseInstructions = \case
    MessageItem ResponseMessage { role = RoleDeveloper, extraFields } ->
        case KeyMap.lookup
                (Key.fromText "internal_chat_message_metadata_passthrough")
                extraFields of
            Just (Aeson.Object metadata) ->
                case KeyMap.lookup
                        (Key.fromText "content_item_kinds")
                        metadata of
                    Just (Aeson.Array kinds) ->
                        Aeson.String "model.base_instructions" `elem` kinds
                    _ -> False
            _ -> False
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
            , extraFields = message.extraFields
            }
    FunctionCallOutputItem callOutput ->
        FunctionCallOutputItem FunctionCallOutput
            { itemId = callOutput.itemId
            , callId = callOutput.callId
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
    , extraFields = KeyMap.empty
    }

agentMessageItem :: InterAgentMessage -> ResponseItem
agentMessageItem message = KnownResponseItem ItemAgentMessage TaggedObject
    { tag = "agent_message"
    , fields = KeyMap.fromList
        [ (Key.fromText "author", Aeson.String message.messageAuthor)
        , (Key.fromText "recipient", Aeson.String message.messageRecipient)
        , (Key.fromText "content", Aeson.toJSON (agentMessageContent message))
        ]
    }

agentMessageContent :: InterAgentMessage -> [Aeson.Value]
agentMessageContent message = case message.messageContent of
    PlainInterAgentContent _ ->
        [inputTextValue (renderInterAgentMessage message)]
    EncryptedInterAgentContent encrypted ->
        [ inputTextValue (renderInterAgentMessageHeader message)
        , Aeson.object
            [ "type" Aeson..= ("encrypted_content" :: Text)
            , "encrypted_content" Aeson..= encrypted
            ]
        ]
  where
    inputTextValue text = Aeson.object
        [ "type" Aeson..= ("input_text" :: Text)
        , "text" Aeson..= text
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
    }

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
        let toolName = namespacedToolName call.extraFields call.name
        in Just ToolCall
            { callId = call.callId
            , name = toolName
            , arguments = call.arguments
            , callKind = FunctionCallKind
            , argumentsEncrypted =
                encryptedCollaborationArguments toolName call.extraFields
            }
    CustomToolCallItem call -> Just ToolCall
        { callId = call.callId
        , name = namespacedToolName call.extraFields call.name
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

instance Aeson.FromJSON CodexRateLimitsPayload where
    parseJSON = Aeson.withObject "Codex rate limits payload" \object ->
        CodexRateLimitsPayload
            <$> object Aeson..:? "rate_limits"

instance Aeson.FromJSON CodexRateLimitDetails where
    parseJSON = Aeson.withObject "Codex rate limit details" \object ->
        CodexRateLimitDetails
            <$> object Aeson..:? "allowed"
            <*> object Aeson..:? "limit_reached"
            <*> object Aeson..:? "primary"
            <*> object Aeson..:? "secondary"

instance Aeson.FromJSON CodexRateLimitWindow where
    parseJSON = Aeson.withObject "Codex rate limit window" \object ->
        CodexRateLimitWindow
            <$> object Aeson..: "used_percent"

codexRateLimitsWarning :: Aeson.Object -> Maybe Text
codexRateLimitsWarning fields =
    case Aeson.fromJSON (Aeson.Object fields)
        :: Aeson.Result CodexRateLimitsPayload of
        Aeson.Error _ -> Nothing
        Aeson.Success payload -> do
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

encryptedCollaborationArguments :: Text -> Aeson.Object -> Bool
encryptedCollaborationArguments toolName extras =
    toolName `elem`
        [ "collaboration.spawn_agent"
        , "collaboration.send_message"
        , "collaboration.followup_task"
        ]
        && not plaintextOverride
  where
    plaintextOverride =
        case KeyMap.lookup (Key.fromText "encrypted_function_args") extras of
            Just (Aeson.Array values) -> null values
            _ -> False

namespacedToolName :: Aeson.Object -> Text -> Text
namespacedToolName extras name = case KeyMap.lookup (Key.fromText "namespace") extras of
    Just (Aeson.String namespace)
        | not (Text.null namespace) ->
            if Text.isSuffixOf "." namespace || Text.isSuffixOf "::" namespace
                then namespace <> name
                else namespace <> "." <> name
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
        { otherEventType = StreamEventUnknown eventType } ->
            Just
                (ActivityUpdated
                    ("Warning: unsupported provider event " <> eventType))
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

-- | Whether a stream event proves the provider has begun producing response
-- output. These events make replay unsafe even when they do not map to a
-- visible loop delta.
streamOutputObserved :: ResponseStreamEvent -> Bool
streamOutputObserved event = case event of
    ResponseCompletedEvent{} -> True
    ResponseDoneEvent{} -> True
    ResponseIncompleteEvent{} -> True
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
    _ ->
        responseStreamEventType event /= EventCodexRateLimits
            && isJust (streamEventToLoopEvent event)

extraDeltaText :: Aeson.Object -> Maybe Text
extraDeltaText extras = nonEmptyText extras "delta" <|> nonEmptyText extras "text"

nonEmptyText :: Aeson.Object -> Text -> Maybe Text
nonEmptyText extras key = case KeyMap.lookup (Key.fromText key) extras of
    Just (Aeson.String text) | not (Text.null text) -> Just text
    _ -> Nothing
