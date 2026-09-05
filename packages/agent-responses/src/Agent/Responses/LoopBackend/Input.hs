-- | Prepare portable loop inputs and replayed history for the Responses wire.
-- Compatibility repairs belong here, not in persisted session history.
module Agent.Responses.LoopBackend.Input
    ( withRequestInput
    , normalizeResponseInputItems
    , turnInputsToItems
    , toolResultToItem
    , isLegacyComputerFunctionCall
    ) where

import Agent.InterAgentMessage
    ( InterAgentMessage(..)
    , InterAgentMessageContent(..)
    , renderInterAgentMessage
    , renderInterAgentMessageHeader
    )
import Agent.Json (RawJson, rawJsonBytes, rawJsonFromEncoding)
import Agent.Loop
    ( FileAttachment(..)
    , ImageAttachment(..)
    , TurnAttachment(..)
    , TurnInput(..)
    )
import Agent.Responses.Request (stripReplayedItemStatus)
import Agent.Responses.Types
import Agent.ToolDispatch
    ( toolCallResultOutcome
    , ToolCallKind(..)
    , ToolCallMode(..)
    , ToolCallResult(..)
    , ToolResultImage(..)
    , isComputerToolCallKind
    , toolCallResultImages
    , toolCallResultMode
    )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Hermes as Hermes
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified "base64-bytestring" Data.ByteString.Base64 as Base64
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (maybeToList)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

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
            { localOutcome = callOutput.localOutcome
            , itemId = callOutput.itemId
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
            { localOutcome = callOutput.localOutcome
            , itemId = callOutput.itemId
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
        { localOutcome = Nothing
        , itemId = Nothing
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
        { localOutcome = output.localOutcome
        , itemId = Nothing
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
        { localOutcome = toolCallResultOutcome result
        , itemId = Nothing
        , callId = result.callId
        , name = Nothing
        , namespace = Nothing
        , provider = Nothing
        , output = toolResultOutput result
        , status = Nothing
        , async = asyncResultField result
        }
    CustomCallKind -> CustomToolCallOutputItem CustomToolCallOutput
        { localOutcome = toolCallResultOutcome result
        , itemId = Nothing
        , callId = result.callId
        , name = Nothing
        , output = toolResultOutput result
        , status = Nothing
        , async = asyncResultField result
        }
    ComputerCallKind ->
        FunctionCallOutputItem FunctionCallOutput
            { localOutcome = toolCallResultOutcome result
            , itemId = Nothing
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
            { localOutcome = toolCallResultOutcome result
            , itemId = Nothing
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

asyncResultField :: ToolCallResult -> Maybe Bool
asyncResultField result =
    case toolCallResultMode result of
        AsyncToolCall -> Just True
        BlockingToolCall -> Nothing

isLegacyComputerFunctionCall :: FunctionCall -> Bool
isLegacyComputerFunctionCall call =
    call.name == legacyComputerFunctionName
        && call.namespace == Just computerFunctionNamespace
