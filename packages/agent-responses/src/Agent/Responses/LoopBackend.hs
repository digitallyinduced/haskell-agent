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
    , hasRecoverableIncompleteOutput
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
import Agent.Json
    ( Extensions
    , RawJson
    , emptyExtensions
    , extensionsFromList
    , extensionsToList
    , deleteExtension
    , lookupExtension
    , rawJsonBytes
    )
import qualified Agent.Json.Decoder as JsonDecoder
import qualified Agent.Json.Encoder as JsonEncoder
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallKind(..)
    , ToolCallResult(..)
    )
import Control.Applicative ((<|>))
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

stripInputImageDetailValue :: RawJson -> RawJson
stripInputImageDetailValue raw =
    case JsonDecoder.decode rawContainerDecoder (rawJsonBytes raw) of
        Right (Just (Left fields)) ->
            let transformed =
                    extensionsFromList
                        [ (key, stripInputImageDetailValue value)
                        | (key, value) <- extensionsToList fields
                        ]
                withoutDetail =
                    case lookupExtension "type" transformed
                        >>= decodeRaw JsonDecoder.text of
                        Just "input_image" ->
                            deleteExtension "detail" transformed
                        _ -> transformed
            in encodeRaw
                (JsonEncoder.objectWithExtensions id [])
                withoutDetail
        Right (Just (Right values)) ->
            encodeRaw
                (JsonEncoder.list JsonEncoder.rawJson)
                (map stripInputImageDetailValue values)
        _ -> raw
  where
    rawContainerDecoder =
        JsonDecoder.byType \case
            JsonDecoder.JsonObject ->
                Just . Left
                    <$> JsonDecoder.objectFields
                        JsonDecoder.extensionFields
            JsonDecoder.JsonArray ->
                Just . Right
                    <$> JsonDecoder.array JsonDecoder.rawJson
            _ -> Nothing <$ JsonDecoder.skip

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
                            [OutputTextPart text Nothing Nothing emptyExtensions]
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
        [OutputTextPart "" Nothing Nothing emptyExtensions]
    , role = RoleAssistant
    , status = Nothing
    , phase = Nothing
    , passthrough = Nothing
    , extraFields = emptyExtensions
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
    , content = MessageContentParts [InputTextPart text Nothing emptyExtensions]
    , role = RoleUser
    , status = Nothing
    , phase = Nothing
    , passthrough = Nothing
    , extraFields = emptyExtensions
    }

multimodalFilesItem :: Text -> [ImageAttachment] -> [FileAttachment] -> ResponseItem
multimodalFilesItem text images files = MessageItem ResponseMessage
    { messageId = Nothing
    , content = MessageContentParts
        ( InputTextPart text Nothing emptyExtensions
        : map imageAttachmentPart images
        <> map fileAttachmentPart files
        )
    , role = RoleUser
    , status = Nothing
    , phase = Nothing
    , passthrough = Nothing
    , extraFields = emptyExtensions
    }

agentMessageItem :: InterAgentMessage -> ResponseItem
agentMessageItem message = AgentMessageItem ResponseAgentMessage
    { messageId = Nothing
    , author = Just message.messageAuthor
    , recipient = Just message.messageRecipient
    , content = agentMessageContent message
    , passthrough = Nothing
    , extraFields = emptyExtensions
    }

agentMessageContent :: InterAgentMessage -> [ResponseContentPart]
agentMessageContent message = case message.messageContent of
    PlainInterAgentContent _ ->
        [InputTextPart (renderInterAgentMessage message) Nothing emptyExtensions]
    EncryptedInterAgentContent encrypted ->
        [ InputTextPart
            (renderInterAgentMessageHeader message)
            Nothing
            emptyExtensions
        , EncryptedContentPart encrypted emptyExtensions
        ]

multimodalUserItem :: Text -> [ImageAttachment] -> ResponseItem
multimodalUserItem text images = MessageItem ResponseMessage
    { messageId = Nothing
    , content = MessageContentParts
        ( InputTextPart text Nothing emptyExtensions
        : map imageAttachmentPart images
        )
    , role = RoleUser
    , status = Nothing
    , phase = Nothing
    , passthrough = Nothing
    , extraFields = emptyExtensions
    }

imageAttachmentPart :: ImageAttachment -> ResponseContentPart
imageAttachmentPart ImageAttachment{imageMime, imageBytes} =
    InputImagePart
        { detail = Just "auto"
        , fileId = Nothing
        , imageUrl = Just (imageDataUrl imageMime imageBytes)
        , promptCacheBreakpoint = Nothing
        , extraFields = emptyExtensions
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
        , extraFields = emptyExtensions
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
        , output = encodeRaw JsonEncoder.text result.output
        , status = Nothing
        , extraFields = emptyExtensions
        }
    CustomCallKind -> CustomToolCallOutputItem CustomToolCallOutput
        { itemId = Nothing
        , callId = result.callId
        , name = Nothing
        , output = encodeRaw JsonEncoder.text result.output
        , status = Nothing
        , extraFields = emptyExtensions
        }

responseToTurnOutput :: Response -> TurnOutput
responseToTurnOutput response = TurnOutput
    { responseId = response.responseId
    , toolCalls = mapMaybe responseItemToToolCall response.output
    , assistantText = assistantTextFromResponse response
    , tokenUsage = responseTokenUsage response
    , completion = case response.status of
        ResponseIncomplete -> TurnIncomplete
            { incompleteReason =
                maybe "unknown" (.reason) response.incompleteDetails
            , incompleteReasoningTokens =
                response.usage
                    >>= (.outputTokensDetails)
                    >>= (.reasoningTokens)
            }
        _ -> TurnCompleted
    }

-- | An incomplete response can still finish the turn when it already contains
-- executable tool calls or assistant text, or when it is a continuable
-- reasoning-only stop. @max_output_tokens@ during reasoning is handed to the
-- loop as an empty completion so it can continue the chain. Reasons such as
-- @content_filter@ stay transport failures, as do completely empty incomplete
-- responses, so a replay-safe fallback can still run.
hasRecoverableIncompleteOutput :: Response -> Bool
hasRecoverableIncompleteOutput response =
    not (null (mapMaybe responseItemToToolCall response.output))
        || maybe False (not . Text.null . Text.strip)
            (assistantTextFromResponse response)
        || (any isReasoningOutput response.output
            && isContinuableIncompleteReason response)

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

data CodexRateLimitDetails = CodexRateLimitDetails
    { allowed :: !(Maybe Bool)
    , limitReached :: !(Maybe Bool)
    , primaryUsedPercent :: !(Maybe Double)
    , secondaryUsedPercent :: !(Maybe Double)
    }

codexRateLimitsWarning :: Extensions -> Maybe Text
codexRateLimitsWarning fields = do
    detailsRaw <- lookupExtension "rate_limits" fields
    details <- decodeRaw rateLimitDetailsDecoder detailsRaw
    let reportedWindows =
            [ ("primary", value)
            | value <- maybeToList details.primaryUsedPercent
            ]
            <> [ ("secondary", value)
               | value <- maybeToList details.secondaryUsedPercent
               ]
        lowWindows =
            [ (label, value)
            | (label, value) <- reportedWindows
            , value >= 90
            ]
        reached =
            details.limitReached == Just True
                || details.allowed == Just False
                || any ((>= 100) . snd) reportedWindows
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

rateLimitDetailsDecoder :: JsonDecoder.Decoder CodexRateLimitDetails
rateLimitDetailsDecoder =
    JsonDecoder.objectFields $
        CodexRateLimitDetails
            <$> JsonDecoder.optionalField "allowed" JsonDecoder.bool
            <*> JsonDecoder.optionalField
                "limit_reached"
                JsonDecoder.bool
            <*> JsonDecoder.optionalField
                "primary"
                rateLimitWindowDecoder
            <*> JsonDecoder.optionalField
                "secondary"
                rateLimitWindowDecoder

rateLimitWindowDecoder :: JsonDecoder.Decoder Double
rateLimitWindowDecoder =
    JsonDecoder.objectFields $
        JsonDecoder.requiredField "used_percent" JsonDecoder.double

formatRateLimitWindow :: (Text, Double) -> Text
formatRateLimitWindow (label, usedPercent) =
    label <> " " <> formatPercent remaining <> "% left"
  where
    remaining = max 0 (min 100 (100 - usedPercent))

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
    ResponseOutputTextDeltaEvent { delta = Just text } ->
        Just (TextDelta text)
    ResponseReasoningTextDeltaEvent { delta = Just text }
        | showRawReasoning ->
            Just (ReasoningDelta text)
        | otherwise -> Nothing
    ResponseReasoningSummaryTextDeltaEvent { delta = Just text } ->
        Just (ReasoningDelta text)
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
    ResponseOutputTextDeltaEvent{} -> True
    ResponseReasoningTextDeltaEvent{} -> True
    ResponseReasoningSummaryTextDeltaEvent{} -> True
    ResponseReasoningSummaryTextDoneEvent{} -> True
    OtherResponseStreamEvent { otherEventType }
        | streamEventTypeText otherEventType == unparsedStreamEventTypeText ->
            False
    _ ->
        responseStreamEventType event /= EventCodexRateLimits
            && isJust (streamEventToLoopEvent event)

unknownProviderEventWarning :: Text -> Extensions -> Text
unknownProviderEventWarning eventType extras =
    "Warning: unsupported provider event "
        <> eventType
        <> foldMap (": " <>) (objectPreview extras)

unparsedStreamFrameWarning :: Extensions -> Text
unparsedStreamFrameWarning extras =
    "Codex websocket dropped an unparsed frame"
        <> foldMap (": " <>) (nonEmptyText extras "error")
        <> foldMap (" payload=" <>) (nonEmptyText extras "payload")

objectPreview :: Extensions -> Maybe Text
objectPreview extras
    | null (extensionsToList extras) = Nothing
    | otherwise =
        Just
            . Text.take 500
            . Text.decodeUtf8Lenient
            $ JsonEncoder.encode
                (JsonEncoder.objectWithExtensions id [])
                extras

extraDeltaText :: Extensions -> Maybe Text
extraDeltaText extras = nonEmptyText extras "delta" <|> nonEmptyText extras "text"

nonEmptyText :: Extensions -> Text -> Maybe Text
nonEmptyText extras key = do
    raw <- lookupExtension key extras
    text <- decodeRaw JsonDecoder.text raw
    if Text.null text then Nothing else Just text

decodeRaw :: JsonDecoder.Decoder value -> RawJson -> Maybe value
decodeRaw decoder raw =
    either (const Nothing) Just $
        JsonDecoder.decode decoder (rawJsonBytes raw)

encodeRaw :: JsonEncoder.Encoder value -> value -> RawJson
encodeRaw encoder value =
    case JsonDecoder.validateRawJson (JsonEncoder.encode encoder value) of
        Left err ->
            error (Text.unpack (JsonDecoder.renderDecodeError err))
        Right raw -> raw
