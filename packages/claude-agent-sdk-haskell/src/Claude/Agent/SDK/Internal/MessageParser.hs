-- | Typed parser for Claude Code's stream-json records.
module Claude.Agent.SDK.Internal.MessageParser
    ( decodeMessageLine
    , parseMessageValue
    ) where

import Claude.Agent.SDK.Errors (ClaudeSDKError(..))
import Claude.Agent.SDK.Types
    ( AssistantMessage(..)
    , ContentBlock(..)
    , ConversationResetMessage(..)
    , Message(..)
    , MessageOrigin(..)
    , ModelUsage(..)
    , ResultMessage(..)
    , StreamEvent(..)
    , SystemMessage(..)
    , Usage(..)
    , UserMessage(..)
    , emptyUsage
    )
import qualified Data.Aeson as Aeson
import Data.Aeson (Object, Value)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.UUID.Types as UUID

decodeMessageLine :: ByteString -> Either ClaudeSDKError Message
decodeMessageLine bytes =
    case Aeson.eitherDecodeStrict' bytes of
        Left message ->
            Left CLIJSONDecodeError
                { decodeError = Text.pack message
                , rawBody =
                    Text.take 2_000
                        (TextEncoding.decodeUtf8With lenientDecode bytes)
                }
        Right value ->
            parseMessageValue value

parseMessageValue :: Value -> Either ClaudeSDKError Message
parseMessageValue value =
    case value of
        Aeson.Object object ->
            parseMessageObject object
        _ ->
            Left MessageParseError
                { parseError = "expected a JSON object"
                , rawMessage = Just value
                }

parseMessageObject :: Object -> Either ClaudeSDKError Message
parseMessageObject object =
    case textAt "type" object of
        Nothing ->
            Left MessageParseError
                { parseError = "message is missing a string `type` field"
                , rawMessage = Just (Aeson.Object object)
                }
        Just "user" ->
            MessageUser <$> parseUserMessage object
        Just "assistant" ->
            MessageAssistant <$> parseAssistantMessage object
        Just "system" ->
            MessageSystem <$> parseSystemMessage object
        Just "result" ->
            MessageResult <$> parseResultMessage object
        Just "stream_event" ->
            MessageStreamEvent <$> parseStreamEvent object
        Just "conversation_reset" ->
            pure $
                MessageConversationReset ConversationResetMessage
                    { newConversationId =
                        nonEmptyTextAt "new_conversation_id" object
                    , uuid = nonEmptyTextAt "uuid" object
                    , sessionId = nonEmptyTextAt "session_id" object
                    , raw = object
                    }
        Just "control_request" ->
            pure (MessageControlRequest object)
        Just _ ->
            pure (MessageUnknown (Aeson.Object object))

parseUserMessage :: Object -> Either ClaudeSDKError UserMessage
parseUserMessage object = do
    message <- requireObjectAt "message" object
    content <- case KeyMap.lookup "content" message of
        Just (Aeson.String text) ->
            pure [TextBlock text]
        Just (Aeson.Array values) ->
            traverse parseContentBlock (toList values)
        Just value ->
            Left $
                messageParseError
                    "user message content must be text or an array"
                    value
        Nothing ->
            Left $
                messageParseError
                    "user message is missing `message.content`"
                    (Aeson.Object object)
    pure UserMessage
        { content
        , uuid = nonEmptyTextAt "uuid" object
        , parentToolUseId =
            nonEmptyTextAt "parent_tool_use_id" object
        , origin = messageOriginAt "origin" object
        , raw = object
        }

parseAssistantMessage
    :: Object
    -> Either ClaudeSDKError AssistantMessage
parseAssistantMessage object = do
    message <- requireObjectAt "message" object
    contentValue <-
        maybe
            (Left $
                messageParseError
                    "assistant message is missing `message.content`"
                    (Aeson.Object object))
            Right
            (KeyMap.lookup "content" message)
    content <- case contentValue of
        Aeson.Array values ->
            traverse parseContentBlock (toList values)
        _ ->
            Left $
                messageParseError
                    "assistant message content must be an array"
                    contentValue
    supersedes <-
        strictOptionalStringArrayAt "supersedes" object
    pure AssistantMessage
        { content
        , model = nonEmptyTextAt "model" message
        , parentToolUseId =
            nonEmptyTextAt "parent_tool_use_id" object
        , error = nonEmptyTextAt "error" object
        , usage = usageFromObject <$> objectAt "usage" message
        , messageId = nonEmptyTextAt "id" message
        , stopReason = nonEmptyTextAt "stop_reason" message
        , sessionId = nonEmptyTextAt "session_id" object
        , uuid = nonEmptyTextAt "uuid" object
        , supersedes
        , raw = object
        }

parseSystemMessage
    :: Object
    -> Either ClaudeSDKError SystemMessage
parseSystemMessage object = do
    subtype <-
        requireTextAt "subtype" object
            "system message is missing a string `subtype`"
    retractedMessageUuids <-
        strictOptionalStringArrayAt
            "retracted_message_uuids"
            object
    pure SystemMessage
        { subtype
        , sessionId = nonEmptyTextAt "session_id" object
        , uuid = nonEmptyTextAt "uuid" object
        , apiKeySource = nonEmptyTextAt "apiKeySource" object
        , retractedMessageUuids
        , raw = object
        }

parseResultMessage
    :: Object
    -> Either ClaudeSDKError ResultMessage
parseResultMessage object = do
    subtype <-
        requireTextAt "subtype" object
            "result message is missing a string `subtype`"
    isError <- case KeyMap.lookup "is_error" object of
        Just (Aeson.Bool value) -> Right value
        Just value ->
            Left $
                messageParseError
                    "result `is_error` must be a boolean"
                    value
        Nothing ->
            Left $
                messageParseError
                    "result message is missing `is_error`"
                    (Aeson.Object object)
    sessionId <-
        requireSessionIdAt "session_id" object
            "result message is missing a non-empty string `session_id`"
    let modelUsage = modelUsageFromResult object
    pure ResultMessage
        { subtype
        , durationMs = optionalIntAt "duration_ms" object
        , durationApiMs = optionalIntAt "duration_api_ms" object
        , isError
        , numTurns = optionalIntAt "num_turns" object
        , sessionId
        , stopReason = nonEmptyTextAt "stop_reason" object
        , totalCostUsd = optionalDoubleAt "total_cost_usd" object
        , usage = maybe emptyUsage usageFromObject (objectAt "usage" object)
        , result = textAt "result" object
        , structuredOutput = KeyMap.lookup "structured_output" object
        , modelUsage
        , errors = stringArrayAt "errors" object
        , apiErrorStatus = optionalIntAt "api_error_status" object
        , origin = messageOriginAt "origin" object
        , uuid = nonEmptyTextAt "uuid" object
        , raw = object
        }

parseStreamEvent :: Object -> Either ClaudeSDKError StreamEvent
parseStreamEvent object =
    case KeyMap.lookup "event" object of
        Nothing ->
            Left $
                messageParseError
                    "stream event is missing `event`"
                    (Aeson.Object object)
        Just event ->
            pure StreamEvent
                { uuid = nonEmptyTextAt "uuid" object
                , sessionId = nonEmptyTextAt "session_id" object
                , event
                , parentToolUseId =
                    nonEmptyTextAt "parent_tool_use_id" object
                , raw = object
                }

parseContentBlock :: Value -> Either ClaudeSDKError ContentBlock
parseContentBlock value =
    case value of
        Aeson.Object object ->
            case textAt "type" object of
                Just "text" ->
                    TextBlock
                        <$> requireTextAt "text" object
                            "text block is missing `text`"
                Just "thinking" ->
                    ThinkingBlock
                        <$> requireTextAt "thinking" object
                            "thinking block is missing `thinking`"
                        <*> pure (nonEmptyTextAt "signature" object)
                Just "tool_use" ->
                    ToolUseBlock
                        <$> requireNonEmptyTextAt "id" object
                            "tool_use block is missing `id`"
                        <*> requireNonEmptyTextAt "name" object
                            "tool_use block is missing `name`"
                        <*> pure
                            ( maybe
                                (Aeson.Object KeyMap.empty)
                                id
                                (KeyMap.lookup "input" object)
                            )
                Just "tool_result" ->
                    ToolResultBlock
                        <$> requireNonEmptyTextAt "tool_use_id" object
                            "tool_result block is missing `tool_use_id`"
                        <*> pure (KeyMap.lookup "content" object)
                        <*> pure (boolAt "is_error" object)
                Just "server_tool_use" ->
                    ServerToolUseBlock
                        <$> requireNonEmptyTextAt "id" object
                            "server_tool_use block is missing `id`"
                        <*> requireNonEmptyTextAt "name" object
                            "server_tool_use block is missing `name`"
                        <*> pure
                            ( maybe
                                (Aeson.Object KeyMap.empty)
                                id
                                (KeyMap.lookup "input" object)
                            )
                Just "advisor_tool_result" ->
                    ServerToolResultBlock
                        <$> requireNonEmptyTextAt "tool_use_id" object
                            "server tool result is missing `tool_use_id`"
                        <*> pure (KeyMap.lookup "content" object)
                _ ->
                    pure (UnknownContentBlock value)
        _ ->
            Left $
                messageParseError
                    "content block must be a JSON object"
                    value

usageFromObject :: Object -> Usage
usageFromObject object =
    let directInput = intAt "input_tokens" object
        cacheCreation = intAt "cache_creation_input_tokens" object
        cacheRead = intAt "cache_read_input_tokens" object
    in Usage
        { inputTokens = directInput + cacheCreation + cacheRead
        , outputTokens = intAt "output_tokens" object
        , cachedTokens = cacheRead
        }

modelUsageFromResult :: Object -> Map Text ModelUsage
modelUsageFromResult object =
    case objectAt "modelUsage" object of
        Nothing -> Map.empty
        Just models ->
            maybe Map.empty Map.fromList $
                traverse parseEntry
                    [ (Key.toText key, value)
                    | (key, value) <- KeyMap.toList models
                    ]
  where
    parseEntry (modelName, Aeson.Object usageObject) = do
        directInput <- nonNegativeIntAt "inputTokens" usageObject
        output <- nonNegativeIntAt "outputTokens" usageObject
        cacheRead <-
            optionalNonNegativeIntAt "cacheReadInputTokens" usageObject
        cacheCreation <-
            optionalNonNegativeIntAt
                "cacheCreationInputTokens"
                usageObject
        pure
            ( modelName
            , ModelUsage
                { inputTokens = directInput
                , outputTokens = output
                , cacheReadInputTokens = cacheRead
                , cacheCreationInputTokens = cacheCreation
                , costUSD = optionalDoubleAt "costUSD" usageObject
                , raw = usageObject
                }
            )
    parseEntry _ = Nothing

messageParseError :: Text -> Value -> ClaudeSDKError
messageParseError parseError raw =
    MessageParseError
        { parseError
        , rawMessage = Just raw
        }

requireObjectAt
    :: Key.Key
    -> Object
    -> Either ClaudeSDKError Object
requireObjectAt key object =
    case KeyMap.lookup key object of
        Just (Aeson.Object nested) -> Right nested
        Just value ->
            Left $
                messageParseError
                    ("`" <> Key.toText key <> "` must be an object")
                    value
        Nothing ->
            Left $
                messageParseError
                    ("message is missing `" <> Key.toText key <> "`")
                    (Aeson.Object object)

requireTextAt
    :: Key.Key
    -> Object
    -> Text
    -> Either ClaudeSDKError Text
requireTextAt key object err =
    case textAt key object of
        Just value -> Right value
        Nothing ->
            Left $
                messageParseError err (Aeson.Object object)

requireNonEmptyTextAt
    :: Key.Key
    -> Object
    -> Text
    -> Either ClaudeSDKError Text
requireNonEmptyTextAt key object err =
    case nonEmptyTextAt key object of
        Just value -> Right value
        Nothing ->
            Left $
                messageParseError err (Aeson.Object object)

requireSessionIdAt
    :: Key.Key
    -> Object
    -> Text
    -> Either ClaudeSDKError Text
requireSessionIdAt key object err =
    normalizeSessionId <$> requireNonEmptyTextAt key object err

objectAt :: Key.Key -> Object -> Maybe Object
objectAt key object =
    case KeyMap.lookup key object of
        Just (Aeson.Object nested) -> Just nested
        _ -> Nothing

messageOriginAt :: Key.Key -> Object -> Maybe MessageOrigin
messageOriginAt key object =
    case KeyMap.lookup key object of
        Nothing -> Nothing
        Just Aeson.Null -> Nothing
        Just (Aeson.Object originObject) ->
            Just MessageOrigin
                { kind =
                    maybe "unclassified" id
                        (nonEmptyTextAt "kind" originObject)
                , raw = originObject
                }
        Just _ ->
            Just MessageOrigin
                { kind = "unclassified"
                , raw = KeyMap.empty
                }

stringArrayAt :: Key.Key -> Object -> [Text]
stringArrayAt key object =
    case KeyMap.lookup key object of
        Just (Aeson.Array values) ->
            [text | Aeson.String text <- toList values]
        _ -> []

strictOptionalStringArrayAt
    :: Key.Key
    -> Object
    -> Either ClaudeSDKError [Text]
strictOptionalStringArrayAt key object =
    case KeyMap.lookup key object of
        Nothing ->
            Right []
        Just Aeson.Null ->
            Right []
        Just (Aeson.Array values) ->
            traverse parseElement (toList values)
        Just value ->
            Left $
                messageParseError
                    ( "`"
                        <> Key.toText key
                        <> "` must be an array of non-empty strings"
                    )
                    value
  where
    parseElement = \case
        Aeson.String raw ->
            let stripped = Text.strip raw
            in if Text.null stripped
                then invalidElement (Aeson.String raw)
                else Right stripped
        value ->
            invalidElement value

    invalidElement value =
        Left $
            messageParseError
                ( "`"
                    <> Key.toText key
                    <> "` must contain only non-empty strings"
                )
                value

textAt :: Key.Key -> Object -> Maybe Text
textAt key object =
    case KeyMap.lookup key object of
        Just (Aeson.String text) -> Just text
        _ -> Nothing

nonEmptyTextAt :: Key.Key -> Object -> Maybe Text
nonEmptyTextAt key object = do
    text <- textAt key object
    let stripped = Text.strip text
    if Text.null stripped then Nothing else Just stripped

normalizeSessionId :: Text -> Text
normalizeSessionId value =
    maybe value UUID.toText (UUID.fromText value)

boolAt :: Key.Key -> Object -> Maybe Bool
boolAt key object =
    case KeyMap.lookup key object of
        Just (Aeson.Bool value) -> Just value
        _ -> Nothing

intAt :: Key.Key -> Object -> Int
intAt key object =
    case optionalIntAt key object of
        Just value -> max 0 value
        Nothing -> 0

optionalIntAt :: Key.Key -> Object -> Maybe Int
optionalIntAt key object = do
    value <- KeyMap.lookup key object
    case Aeson.fromJSON value of
        Aeson.Success number -> Just number
        Aeson.Error _ -> Nothing

nonNegativeIntAt :: Key.Key -> Object -> Maybe Int
nonNegativeIntAt key object = do
    value <- optionalIntAt key object
    if value >= 0 then Just value else Nothing

optionalNonNegativeIntAt :: Key.Key -> Object -> Maybe Int
optionalNonNegativeIntAt key object =
    case KeyMap.lookup key object of
        Nothing -> Just 0
        Just Aeson.Null -> Just 0
        Just _ -> nonNegativeIntAt key object

optionalDoubleAt :: Key.Key -> Object -> Maybe Double
optionalDoubleAt key object =
    case KeyMap.lookup key object of
        Just value ->
            case Aeson.fromJSON value of
                Aeson.Success number -> Just number
                Aeson.Error _ -> Nothing
        _ -> Nothing
