-- | Provider-neutral turn inputs and prompt-image normalization.
module Agent.Loop.Input
    ( ImageAttachment(..)
    , FileAttachment(..)
    , TurnAttachment(..)
    , TurnInput(..)
    , userMessageWithAttachments
    , turnInputImages
    , turnInputFiles
    , mapTurnInputUserText
    , normalizeTurnInputs
    , normalizeResponseItemImages
    ) where

import Agent.Image.Normalize
    ( NormalizedImage(..)
    , normalizeImageDataUrl
    , normalizeImageForPrompt
    )
import Agent.InterAgentMessage (InterAgentMessage)
import Agent.Json (RawJson, rawJsonBytes, rawJsonFromEncoding)
import Agent.Responses.Types
    ( ComputerCallOutput(..)
    , CustomToolCallOutput(..)
    , FunctionCallOutput(..)
    , MessageContent(..)
    , ResponseContentPart(..)
    , ResponseAgentMessage(..)
    , ResponseItem(..)
    , ResponseMessage(..)
    , ReasoningItem(..)
    )
import Agent.ToolDispatch (ToolCallResult(..), ToolResultImage(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)

-- | Image bytes attached to a user turn (PNG/JPEG/…).
data ImageAttachment = ImageAttachment
    { imageMime :: !Text
    , imageBytes :: !ByteString
    } deriving (Eq)

instance Show ImageAttachment where
    show image =
        "ImageAttachment { imageMime = " <> show image.imageMime
            <> ", imageBytes = <redacted>"
            <> ", imageByteLength = " <> show (ByteString.length image.imageBytes)
            <> " }"

-- | File bytes attached to a user turn. Providers that cannot ingest files
-- natively should fall back to a local path or text summary.
data FileAttachment = FileAttachment
    { fileName :: !(Maybe Text)
    , fileMime :: !Text
    , fileBytes :: !ByteString
    } deriving (Eq)

instance Show FileAttachment where
    show file =
        "FileAttachment { fileName = " <> show file.fileName
            <> ", fileMime = " <> show file.fileMime
            <> ", fileBytes = <redacted>"
            <> ", fileByteLength = " <> show (ByteString.length file.fileBytes)
            <> " }"

-- | A provider-neutral user attachment, retained in source order.
data TurnAttachment
    = ImageAttachmentItem !ImageAttachment
    | FileAttachmentItem !FileAttachment
    deriving (Eq, Show)

-- | One input supplied to the provider-neutral agent loop.
data TurnInput
    = UserMessage Text
    | AgentMessage InterAgentMessage
    | UserMessageWithAttachments !Text !(NonEmpty TurnAttachment)
    | CompletedTool ToolCallResult
    deriving (Eq, Show)

-- | Build a user message, using the text-only representation when the
-- attachment list is empty.
userMessageWithAttachments :: Text -> [TurnAttachment] -> TurnInput
userMessageWithAttachments text =
    maybe (UserMessage text) (UserMessageWithAttachments text)
        . NonEmpty.nonEmpty

-- | Images attached to a user input, in source order.
turnInputImages :: TurnInput -> [ImageAttachment]
turnInputImages = \case
    UserMessageWithAttachments _ attachments ->
        [ image
        | ImageAttachmentItem image <- NonEmpty.toList attachments
        ]
    _ -> []

-- | Files attached to a user input, in source order.
turnInputFiles :: TurnInput -> [FileAttachment]
turnInputFiles = \case
    UserMessageWithAttachments _ attachments ->
        [ file
        | FileAttachmentItem file <- NonEmpty.toList attachments
        ]
    _ -> []

-- | Transform user-authored text without changing attachments or other input
-- variants.
mapTurnInputUserText :: (Text -> Text) -> TurnInput -> TurnInput
mapTurnInputUserText transform = \case
    UserMessage text ->
        UserMessage (transform text)
    UserMessageWithAttachments text attachments ->
        UserMessageWithAttachments (transform text) attachments
    other -> other

normalizeTurnInputImages :: TurnInput -> TurnInput
normalizeTurnInputImages = \case
    UserMessageWithAttachments text attachments ->
        UserMessageWithAttachments text
            (fmap normalizeAttachment attachments)
    CompletedTool result ->
        CompletedTool (normalizeToolResultImages result)
    other -> other
  where
    normalizeAttachment = \case
        ImageAttachmentItem ImageAttachment{imageMime, imageBytes} ->
            case normalizeImageForPrompt imageMime imageBytes of
                NormalizedImage{normalizedImageMime, normalizedImageBytes} ->
                    ImageAttachmentItem ImageAttachment
                        { imageMime = normalizedImageMime
                        , imageBytes = normalizedImageBytes
                        }
        file@FileAttachmentItem{} -> file

    normalizeToolResultImages result@ToolCallResult{} = result
    normalizeToolResultImages result@ToolCallResultWithImages{
        toolResultImages
    } =
        result
            { toolResultImages =
                fmap normalizeToolResultImage toolResultImages
            }
    normalizeToolResultImages result@AsyncToolCallResult{} = result
    normalizeToolResultImages result@AsyncToolCallResultWithImages{
        toolResultImages
    } =
        result
            { toolResultImages =
                fmap normalizeToolResultImage toolResultImages
            }

    normalizeToolResultImages result@ToolCallResultWithOutcome{toolResultImages} =
        result { toolResultImages = fmap normalizeToolResultImage toolResultImages }
    normalizeToolResultImages result@AsyncToolCallResultWithOutcome{toolResultImages} =
        result { toolResultImages = fmap normalizeToolResultImage toolResultImages }

    normalizeToolResultImage :: ToolResultImage -> ToolResultImage
    normalizeToolResultImage image@ToolResultImage{imageUrl} =
        image { imageUrl = normalizeImageDataUrl imageUrl }

normalizeTurnInputs :: [TurnInput] -> [TurnInput]
normalizeTurnInputs = map normalizeTurnInputImages

normalizeResponseItemImages :: ResponseItem -> ResponseItem
normalizeResponseItemImages = \case
    MessageItem message ->
        MessageItem
            message
                { content =
                    normalizeMessageContentImages message.content
                }
    AgentMessageItem message ->
        AgentMessageItem
            message
                { content =
                    map normalizeResponseContentPartImage message.content
                }
    ReasoningItemValue reasoning ->
        ReasoningItemValue
            reasoning
                { content =
                    fmap
                        (map normalizeResponseContentPartImage)
                        reasoning.content
                }
    FunctionCallOutputItem callOutput ->
        FunctionCallOutputItem
            callOutput
                { output = normalizeRawJsonImages callOutput.output
                }
    CustomToolCallOutputItem callOutput ->
        CustomToolCallOutputItem
            callOutput
                { output = normalizeRawJsonImages callOutput.output
                }
    ComputerCallOutputItem callOutput ->
        ComputerCallOutputItem
            callOutput
                { screenshotDataUrl =
                    normalizeImageDataUrl callOutput.screenshotDataUrl
                }
    item -> item

normalizeMessageContentImages :: MessageContent -> MessageContent
normalizeMessageContentImages = \case
    MessageContentText text -> MessageContentText text
    MessageContentParts parts ->
        MessageContentParts (map normalizeResponseContentPartImage parts)

normalizeResponseContentPartImage
    :: ResponseContentPart
    -> ResponseContentPart
normalizeResponseContentPartImage part@InputImagePart{imageUrl} =
    part
        { imageUrl = fmap normalizeImageDataUrl imageUrl
        }
normalizeResponseContentPartImage part = part

normalizeRawJsonImages :: RawJson -> RawJson
normalizeRawJsonImages raw =
    case Aeson.decodeStrict' (rawJsonBytes raw) of
        Nothing -> raw
        Just value ->
            let normalized = normalizeJsonImageValues value
            in if normalized == value
                then raw
                else rawJsonFromEncoding (Aeson.toEncoding normalized)

normalizeJsonImageValues :: Aeson.Value -> Aeson.Value
normalizeJsonImageValues = \case
    Aeson.Object object ->
        Aeson.Object (normalizeObjectImageUrl recursivelyNormalized)
      where
        recursivelyNormalized = KeyMap.map normalizeJsonImageValues object
    Aeson.Array values ->
        Aeson.Array (fmap normalizeJsonImageValues values)
    value -> value
  where
    normalizeObjectImageUrl object =
        case
            ( KeyMap.lookup "type" object
            , KeyMap.lookup "image_url" object
            )
        of
            (Just (Aeson.String kind), Just (Aeson.String imageUrl))
                | kind == "input_image"
                    || kind == "computer_screenshot" ->
                        KeyMap.insert
                            "image_url"
                            (Aeson.String (normalizeImageDataUrl imageUrl))
                            object
            _ -> object
