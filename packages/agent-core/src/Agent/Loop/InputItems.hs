-- | Canonical, provider-neutral representation of accepted loop inputs.
-- Interruption recovery and successful submissions must encode inputs alike.
module Agent.Loop.InputItems
    ( turnInputsToItems
    , toolResultToItem
    , computerScreenshotObservationWith
    , computerFunctionTextOutput
    ) where

import Agent.InterAgentMessage
    ( InterAgentMessage(..)
    , InterAgentMessageContent(..)
    , renderInterAgentMessage
    , renderInterAgentMessageHeader
    )
import Agent.ComputerUse.Protocol
    ( ComputerUseEffect(..)
    , ComputerUseVerdict(..)
    , ComputerUseVerdictDecision(..)
    , computerUseVerdictField
    )
import Agent.Json (RawJson, rawJsonFromEncoding)
import Agent.Loop.Input
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
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Agent.Json.Decode as Json
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

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
        [ InputTextPart (renderInterAgentMessageHeader message) Nothing
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
            , output = computerFunctionToolResultOutput result
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
            , output = computerFunctionToolResultOutput result
            , status = Nothing
            , async = Nothing
            }

toolResultOutput :: ToolCallResult -> RawJson
toolResultOutput result =
    richToolResultOutput result.output (toolCallResultImages result)

computerFunctionToolResultOutput :: ToolCallResult -> RawJson
computerFunctionToolResultOutput result =
    richToolResultOutput
        (computerFunctionTextOutput result.output)
        (toolCallResultImages result)

richToolResultOutput :: Text -> [ToolResultImage] -> RawJson
richToolResultOutput output images =
    case images of
        [] -> rawJsonFromEncoding (Aeson.toEncoding output)
        nonEmptyImages ->
            rawJsonFromEncoding . Aeson.toEncoding $
                map imagePart nonEmptyImages
                    <> [ InputTextPart output Nothing
                       | not (Text.null (Text.strip output))
                       ]
  where
    imagePart :: ToolResultImage -> ResponseContentPart
    imagePart image =
        InputImagePart
            { detail = image.imageDetail
            , fileId = Nothing
            , imageUrl = Just image.imageUrl
            , promptCacheBreakpoint = Nothing
            }

computerFunctionTextOutput :: Text -> Text
computerFunctionTextOutput rawOutput =
    case Json.decodeEither computerCallOutputDecoder (Text.encodeUtf8 rawOutput) of
        Right output ->
            renderComputerOutput
                (verdictFromObject output.computerOutputExtra)
                ( KeyMap.lookup
                    "accessibility_state"
                    output.computerOutputExtra
                    >>= computerAccessibilityFromValue
                )
        Left _ ->
            case Json.decodeEither
                    nativeComputerAccessibilityDecoder
                    (Text.encodeUtf8 rawOutput) of
                Right accessibility ->
                    renderComputerOutput
                        (verdictFromRawOutput rawOutput)
                        (Just accessibility)
                Left _ -> rawOutput

renderComputerOutput
    :: Maybe ComputerUseVerdict
    -> Maybe ComputerAccessibility
    -> Text
renderComputerOutput verdict accessibility =
    verdictText verdict
        <> maybe "" ("\n\n" <>) (renderComputerAccessibility <$> accessibility)

verdictText :: Maybe ComputerUseVerdict -> Text
verdictText Nothing = "Computer action completed."
verdictText (Just verdict) =
    case
        ( verdict.computerUseVerdictEffect
        , verdict.computerUseVerdictDecision
        ) of
        (ComputerUseObservation, ComputerUseDone) ->
            "Computer observation completed."
        (ComputerUseUnverifiable, ComputerUseInspectFreshState) ->
            "Computer input was delivered, but its intended UI effect is "
                <> "unverified. Inspect the fresh observation before "
                <> "retrying; do not repeat the input blindly."
        (ComputerUseUnverifiable, ComputerUseVerifyFreshState) ->
            "Computer input was delivered, but its intended UI effect is "
                <> "unverified. Capture fresh state before retrying; do not "
                <> "repeat the input blindly."
        (ComputerUseSuspectedNoop, ComputerUseInspectFreshState) ->
            "The computer host reported that the input may not have taken "
                <> "effect. Inspect the fresh observation before retrying; "
                <> "do not repeat the input blindly."
        (ComputerUseSuspectedNoop, ComputerUseVerifyFreshState) ->
            "The computer host reported that the input may not have taken "
                <> "effect. Re-observe or rebind before retrying; do not "
                <> "repeat the input blindly."
        _ -> verdict.computerUseVerdictHint

verdictFromObject :: Aeson.Object -> Maybe ComputerUseVerdict
verdictFromObject object =
    KeyMap.lookup (Key.fromText computerUseVerdictField) object
        >>= decodeVerdict

verdictFromRawOutput :: Text -> Maybe ComputerUseVerdict
verdictFromRawOutput raw =
    case Aeson.eitherDecodeStrict' (Text.encodeUtf8 raw) of
        Right (Aeson.Object object) -> verdictFromObject object
        _ -> Nothing

decodeVerdict :: Aeson.Value -> Maybe ComputerUseVerdict
decodeVerdict value =
    case Aeson.fromJSON value of
        Aeson.Success verdict -> Just verdict
        Aeson.Error _ -> Nothing

data ComputerAccessibility
    = TextComputerAccessibility !Text
    | StructuredComputerAccessibility !Aeson.Object

renderComputerAccessibility :: ComputerAccessibility -> Text
renderComputerAccessibility accessibility =
    case accessibility of
        TextComputerAccessibility state ->
            "Current macOS accessibility state:\n" <> state
        StructuredComputerAccessibility fields ->
            accessibilityStateHeading fields
                <> "\n"
                <> jsonValueText (Aeson.Object fields)

computerAccessibilityFromValue
    :: Aeson.Value
    -> Maybe ComputerAccessibility
computerAccessibilityFromValue = \case
    Aeson.String state
        | not (Text.null (Text.strip state)) ->
            Just (TextComputerAccessibility state)
    Aeson.Object fields ->
        Just (StructuredComputerAccessibility fields)
    _ -> Nothing

-- Native hosts return their result object directly, without the provider's
-- @computer_call_output.call_id@ envelope. Only claim such an object when it
-- carries the accessibility projection; list-target results must stay intact.
nativeComputerAccessibilityDecoder :: Json.Decoder ComputerAccessibility
nativeComputerAccessibilityDecoder = do
    accessibility <- Json.objectFold Nothing decodeField
    maybe
        (fail "native computer result has no accessibility_state")
        pure
        accessibility
  where
    decodeField key current
        | key /= "accessibility_state" =
            current <$ Json.withOwnedRawJson (const (pure ()))
        | Just _ <- current =
            fail "duplicate native computer accessibility_state"
        | otherwise =
            Just <$> computerAccessibilityDecoder

computerAccessibilityDecoder :: Json.Decoder ComputerAccessibility
computerAccessibilityDecoder =
    Json.getType >>= \case
        Json.VString -> do
            state <- Json.text
            if Text.null (Text.strip state)
                then fail "native computer accessibility_state is empty"
                else pure (TextComputerAccessibility state)
        Json.VObject ->
            Json.withOwnedRawJson \raw ->
                case Aeson.eitherDecodeStrict' raw of
                    Right (Aeson.Object fields) ->
                        pure (StructuredComputerAccessibility fields)
                    Right _ ->
                        fail "native computer accessibility_state is not an object"
                    Left err -> fail err
        _ ->
            fail "native computer accessibility_state must be text or an object"

accessibilityStateHeading :: Aeson.Object -> Text
accessibilityStateHeading fields =
    case KeyMap.lookup "kind" fields of
        Just (Aeson.String "full") ->
            "Full macOS accessibility snapshot"
                <> revisionSuffix "revision"
                <> ":"
        Just (Aeson.String "delta") ->
            "macOS accessibility changes"
                <> case (fieldText "base_revision", fieldText "revision") of
                    (Just baseRevision, Just revision) ->
                        ", revision " <> baseRevision <> " -> " <> revision
                    _ -> ""
                <> ":"
        Just (Aeson.String "unavailable") ->
            "macOS accessibility state unavailable"
                <> revisionSuffix "revision"
                <> ":"
        _ -> "Current macOS accessibility state:"
  where
    revisionSuffix key =
        maybe "" (", revision " <>) (fieldText key)
    fieldText key =
        jsonValueText <$> KeyMap.lookup key fields

jsonValueText :: Aeson.Value -> Text
jsonValueText =
    Text.decodeUtf8
        . LBS.toStrict
        . Aeson.encode

latestComputerScreenshot :: [TurnInput] -> Maybe Text
latestComputerScreenshot inputs =
    lastMaybe
        [ result
        | CompletedTool result <- inputs
        , isComputerToolCallKind result.callKind
        ]
        >>= \result ->
            case lastMaybe (toolCallResultImages result)
                    >>= nonEmptyImageUrl . (.imageUrl) of
                Just imageUrl -> Just imageUrl
                Nothing -> legacyComputerScreenshot result.output
  where
    legacyComputerScreenshot output =
        case Json.decodeEither computerCallOutputDecoder
                (Text.encodeUtf8 output) of
            Right ComputerCallOutput{screenshotDataUrl} ->
                nonEmptyImageUrl screenshotDataUrl
            Left _ -> Nothing
    nonEmptyImageUrl imageUrl
        | Text.null (Text.strip imageUrl) = Nothing
        | otherwise = Just imageUrl
    lastMaybe [] = Nothing
    lastMaybe values = Just (last values)

computerScreenshotObservation :: Text -> ResponseItem
computerScreenshotObservation screenshotDataUrl =
    computerScreenshotObservationWith
        "Current macOS desktop after the completed computer action:"
        screenshotDataUrl

computerScreenshotObservationWith :: Text -> Text -> ResponseItem
computerScreenshotObservationWith observationText screenshotDataUrl =
    MessageItem ResponseMessage
        { messageId = Nothing
        , content = MessageContentParts
            [ InputTextPart observationText Nothing
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

asyncResultField :: ToolCallResult -> Maybe Bool
asyncResultField result =
    case toolCallResultMode result of
        AsyncToolCall -> Just True
        BlockingToolCall -> Nothing
