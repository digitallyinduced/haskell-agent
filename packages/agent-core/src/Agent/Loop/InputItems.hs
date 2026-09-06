-- | Canonical, provider-neutral representation of accepted loop inputs.
-- Interruption recovery and successful submissions must encode inputs alike.
module Agent.Loop.InputItems
    ( turnInputsToItems
    , toolResultToItem
    , computerScreenshotObservationWith
    ) where

import Agent.InterAgentMessage
    ( InterAgentMessage(..)
    , InterAgentMessageContent(..)
    , renderInterAgentMessage
    , renderInterAgentMessageHeader
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
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Agent.Json.Decode as Json
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
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
            case KeyMap.lookup "accessibility_state" output.computerOutputExtra of
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
            case Json.decodeEither computerCallOutputDecoder
                    (Text.encodeUtf8 result.output) of
                Right ComputerCallOutput{screenshotDataUrl} ->
                    Just screenshotDataUrl
                Left _ -> Nothing
  where
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
