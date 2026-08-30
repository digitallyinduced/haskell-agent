-- | Native Hermes decoders for Claude Code's stream-json records.
module Claude.Agent.SDK.Internal.MessageParser
    ( decodeMessageLine
    ) where

import Agent.Json
    ( RawJson
    , rawJsonBytes
    , rawJsonDecoder
    , rawJsonFromEncoding
    )
import qualified Agent.Json.Decode as Json
import Claude.Agent.SDK.Errors (ClaudeSDKError(..))
import Claude.Agent.SDK.Types
import Control.Monad (join)
import qualified Data.Aeson.Encoding as Aeson
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.UUID.Types as UUID

decodeMessageLine :: ByteString -> Either ClaudeSDKError Message
decodeMessageLine bytes =
    case Json.decodeEither messageDecoder bytes of
        Right message -> Right message
        Left typedError ->
            case Json.decodeEither rawJsonDecoder bytes of
                Left syntaxError ->
                    Left CLIJSONDecodeError
                        { decodeError = syntaxError.jsonErrorMessage
                        , rawBody = displayBytes bytes
                        }
                Right raw ->
                    Left MessageParseError
                        { parseError =
                            conciseDecodeError typedError.jsonErrorMessage
                        , rawMessage = Just raw
                        }

messageDecoder :: Json.Decoder Message
messageDecoder = Json.withType \case
    Json.VObject -> Json.object do
        messageType <- requiredNonEmptyText
            "type"
            "message is missing a string `type` field"
        Json.liftObjectDecoder case messageType of
            "user" -> MessageUser <$> userMessageDecoder
            "assistant" -> MessageAssistant <$> assistantMessageDecoder
            "system" -> MessageSystem <$> systemMessageDecoder
            "result" -> MessageResult <$> resultMessageDecoder
            "stream_event" -> MessageStreamEvent <$> streamEventDecoder
            "conversation_reset" ->
                MessageConversationReset <$> conversationResetDecoder
            "control_request" ->
                MessageControlRequest <$> opaqueMessageDecoder
            _ ->
                MessageUnknown <$> opaqueMessageDecoder
    _ -> fail "expected a JSON object"

userMessageDecoder :: Json.Decoder UserMessage
userMessageDecoder = Json.object do
    content <- Json.atKeyOptional "message" userContentEnvelopeDecoder
        >>= maybe
            (fail "user message is missing `message.content`")
            pure
    parentToolUseId <- optionalNonEmptyText "parent_tool_use_id"
    hasParentToolUseId <- parentFieldPresent
    sessionId <- optionalNonEmptyText "session_id"
    origin <- optionalOrigin "origin"
    uuid <- optionalNonEmptyText "uuid"
    pure UserMessage{..}

userContentEnvelopeDecoder :: Json.Decoder [ContentBlock]
userContentEnvelopeDecoder = Json.object do
    content <- Json.atKeyOptional "content" userContentDecoder
    maybe (fail "user message is missing `message.content`") pure content

userContentDecoder :: Json.Decoder [ContentBlock]
userContentDecoder = Json.withType \case
    Json.VString -> pure . (: []) . TextBlock =<< Json.text
    Json.VArray -> Json.list contentBlockDecoder
    _ -> fail "user message content must be text or an array"

assistantMessageDecoder :: Json.Decoder AssistantMessage
assistantMessageDecoder = Json.object do
    details <- Json.atKeyOptional "message" assistantDetailsDecoder
        >>= maybe
            (fail "assistant message is missing `message.content`")
            pure
    parentToolUseId <- optionalNonEmptyText "parent_tool_use_id"
    hasParentToolUseId <- parentFieldPresent
    error <- optionalNonEmptyText "error"
    sessionId <- optionalNonEmptyText "session_id"
    uuid <- optionalNonEmptyText "uuid"
    supersedes <- strictOptionalNonEmptyTextList
        "supersedes"
        "`supersedes` must be an array of non-empty strings"
    pure AssistantMessage
        { content = details.assistantContent
        , model = details.assistantModel
        , usage = details.assistantUsage
        , messageId = details.assistantMessageId
        , stopReason = details.assistantStopReason
        , ..
        }

data AssistantDetails = AssistantDetails
    { assistantContent :: ![ContentBlock]
    , assistantModel :: !(Maybe Text)
    , assistantUsage :: !(Maybe Usage)
    , assistantMessageId :: !(Maybe Text)
    , assistantStopReason :: !(Maybe Text)
    }

assistantDetailsDecoder :: Json.Decoder AssistantDetails
assistantDetailsDecoder = Json.object do
    assistantContent <- Json.atKeyOptional
        "content"
        (Json.withType \case
            Json.VArray -> Json.list contentBlockDecoder
            _ -> fail "assistant message content must be an array")
        >>= maybe
            (fail "assistant message is missing `message.content`")
            pure
    assistantModel <- optionalNonEmptyText "model"
    assistantUsage <- optionalTyped "usage" usageDecoder
    assistantMessageId <- optionalNonEmptyText "id"
    assistantStopReason <- optionalNonEmptyText "stop_reason"
    pure AssistantDetails{..}

systemMessageDecoder :: Json.Decoder SystemMessage
systemMessageDecoder = Json.object do
    subtype <- requiredText
        "subtype"
        "system message is missing a string `subtype`"
    sessionId <- optionalNonEmptyText "session_id"
    uuid <- optionalNonEmptyText "uuid"
    apiKeySource <- optionalNonEmptyText "apiKeySource"
    parentToolUseId <- optionalNonEmptyText "parent_tool_use_id"
    hasParentToolUseId <- parentFieldPresent
    retractedMessageUuids <- strictOptionalNonEmptyTextList
        "retracted_message_uuids"
        "`retracted_message_uuids` must be an array of non-empty strings"
    pure SystemMessage{..}

resultMessageDecoder :: Json.Decoder ResultMessage
resultMessageDecoder = Json.object do
    subtype <- requiredText
        "subtype"
        "result message is missing a string `subtype`"
    isError <- requiredBool
        "is_error"
        "result message is missing `is_error`"
    sessionId <- normalizeSessionId <$> requiredNonEmptyText
        "session_id"
        "result message is missing a non-empty string `session_id`"
    durationMs <- optionalNumber "duration_ms" Json.int
    durationApiMs <- optionalNumber "duration_api_ms" Json.int
    numTurns <- optionalNumber "num_turns" Json.int
    stopReason <- optionalNonEmptyText "stop_reason"
    totalCostUsd <- optionalNumber "total_cost_usd" Json.double
    usage <- fromMaybe emptyUsage <$> optionalTyped "usage" usageDecoder
    result <- optionalText "result"
    structuredOutput <- optionalRaw "structured_output"
    modelUsage <- fromMaybe Map.empty
        <$> optionalTyped "modelUsage" modelUsageDecoder
    errors <- catMaybes . fromMaybe [] <$> optionalTyped
        "errors"
        (Json.list tolerantOptionalText)
    apiErrorStatus <- optionalNumber "api_error_status" Json.int
    origin <- optionalOrigin "origin"
    uuid <- optionalNonEmptyText "uuid"
    parentToolUseId <- optionalNonEmptyText "parent_tool_use_id"
    hasParentToolUseId <- parentFieldPresent
    pure ResultMessage{..}

streamEventDecoder :: Json.Decoder StreamEvent
streamEventDecoder = Json.object do
    uuid <- optionalNonEmptyText "uuid"
    sessionId <- optionalNonEmptyText "session_id"
    event <- Json.atKeyOptional "event" rawJsonDecoder
        >>= maybe (fail "stream event is missing `event`") pure
    streamToolUse <- Json.atKeyOptional "event" streamToolUseDecoder
        >>= pure . (>>= id)
    parentToolUseId <- optionalNonEmptyText "parent_tool_use_id"
    hasParentToolUseId <- parentFieldPresent
    pure StreamEvent{..}

streamToolUseDecoder :: Json.Decoder (Maybe StreamToolUse)
streamToolUseDecoder = Json.withType \case
    Json.VObject -> Json.object do
        eventType <- optionalText "type"
        case eventType of
            Just "content_block_start" ->
                Json.atKeyOptional "content_block" streamToolBlockDecoder
                    >>= pure . (>>= id)
            _ -> pure Nothing
    _ -> pure Nothing

streamToolBlockDecoder :: Json.Decoder (Maybe StreamToolUse)
streamToolBlockDecoder = Json.withType \case
    Json.VObject -> Json.object do
        blockType <- optionalText "type"
        case blockType of
            Just kind | kind `elem` ["tool_use", "server_tool_use"] -> do
                toolUseId <- requiredNonEmptyText
                    "id"
                    "stream tool block is missing `id`"
                name <- requiredNonEmptyText
                    "name"
                    "stream tool block is missing `name`"
                input <- fromMaybe emptyObjectJson <$> optionalRaw "input"
                pure (Just StreamToolUse{..})
            _ -> pure Nothing
    _ -> pure Nothing

conversationResetDecoder :: Json.Decoder ConversationResetMessage
conversationResetDecoder = Json.object do
    newConversationId <-
        fmap normalizeSessionId
            <$> optionalNonEmptyText "new_conversation_id"
    uuid <- optionalNonEmptyText "uuid"
    sessionId <-
        fmap normalizeSessionId
            <$> optionalNonEmptyText "session_id"
    parentToolUseId <- optionalNonEmptyText "parent_tool_use_id"
    hasParentToolUseId <- parentFieldPresent
    pure ConversationResetMessage{..}

opaqueMessageDecoder :: Json.Decoder OpaqueMessage
opaqueMessageDecoder = Json.object do
    uuid <- optionalNonEmptyText "uuid"
    sessionId <- optionalNonEmptyText "session_id"
    parentToolUseId <- optionalNonEmptyText "parent_tool_use_id"
    hasParentToolUseId <- parentFieldPresent
    raw <- Json.liftObjectDecoder rawJsonDecoder
    pure OpaqueMessage{..}

contentBlockDecoder :: Json.Decoder ContentBlock
contentBlockDecoder = Json.withType \case
    Json.VObject -> Json.object do
        contentType <- optionalText "type"
        Json.liftObjectDecoder $ Json.object $ case contentType of
            Just "text" ->
                TextBlock <$> requiredText
                    "text"
                    "text block is missing `text`"
            Just "thinking" ->
                ThinkingBlock
                    <$> requiredText
                        "thinking"
                        "thinking block is missing `thinking`"
                    <*> optionalNonEmptyText "signature"
            Just "tool_use" ->
                ToolUseBlock
                    <$> requiredNonEmptyText
                        "id"
                        "tool_use block is missing `id`"
                    <*> requiredNonEmptyText
                        "name"
                        "tool_use block is missing `name`"
                    <*> (fromMaybe emptyObjectJson <$> optionalRaw "input")
            Just "tool_result" ->
                ToolResultBlock
                    <$> requiredNonEmptyText
                        "tool_use_id"
                        "tool_result block is missing `tool_use_id`"
                    <*> optionalTyped "content" toolResultContentDecoder
                    <*> optionalBool "is_error"
            Just "server_tool_use" ->
                ServerToolUseBlock
                    <$> requiredNonEmptyText
                        "id"
                        "server_tool_use block is missing `id`"
                    <*> requiredNonEmptyText
                        "name"
                        "server_tool_use block is missing `name`"
                    <*> (fromMaybe emptyObjectJson <$> optionalRaw "input")
            Just "advisor_tool_result" ->
                ServerToolResultBlock
                    <$> requiredNonEmptyText
                        "tool_use_id"
                        "server tool result is missing `tool_use_id`"
                    <*> optionalTyped "content" toolResultContentDecoder
            _ ->
                UnknownContentBlock contentType
                    <$> Json.liftObjectDecoder rawJsonDecoder
    _ -> fail "content block must be a JSON object"

-- | Capture the complete @content@ value once, then render the retained
-- bytes in a separate decode pass.
--
-- Hermes values are forward-only simdjson On-Demand iterators. Capturing the
-- raw bytes of an array or object consumes it, so a second decoder must not
-- run on the same value; iterating the consumed array reads past its tape and
-- fails with an opaque SIMD error. Scalars tolerate a re-read, which is why
-- string content never exhibited the problem.
toolResultContentDecoder :: Json.Decoder ToolResultContent
toolResultContentDecoder = do
    raw <- rawJsonDecoder
    pure ToolResultContent
        { raw
        , renderedText = renderToolResultBytes (rawJsonBytes raw)
        }

renderToolResultBytes :: ByteString -> Text
renderToolResultBytes bytes =
    either
        (const (displayBytes bytes))
        id
        (Json.decodeEither renderedToolResultDecoder bytes)

-- | Render tool result content as text. Arrays join their rendered elements,
-- objects render as structured blocks, and anything else falls back to the
-- raw JSON. Every branch consumes the current value exactly once.
renderedToolResultDecoder :: Json.Decoder Text
renderedToolResultDecoder =
    Json.withType \case
        Json.VString -> Json.text
        Json.VArray ->
            Text.intercalate "\n" <$> Json.list renderedToolResultDecoder
        Json.VObject ->
            Json.withOwnedRawJson (pure . renderToolResultBlock)
        Json.VNull -> pure ""
        _ -> Json.withOwnedRawJson (pure . displayBytes)

-- | Render one structured tool-result block. Text blocks contribute their
-- text, tool references and images get compact labels, and anything else
-- falls back to its raw JSON.
renderToolResultBlock :: ByteString -> Text
renderToolResultBlock raw =
    case Json.decodeEither toolResultBlockDecoder raw of
        Right (Just rendered) -> rendered
        _ -> displayBytes raw

toolResultBlockDecoder :: Json.Decoder (Maybe Text)
toolResultBlockDecoder = Json.object do
    blockType <- optionalText "type"
    textValue <- optionalText "text"
    toolName <- optionalNonEmptyText "tool_name"
    mediaType <- join <$> optionalTyped "source" imageSourceMediaTypeDecoder
    pure case (blockType, textValue) of
        (_, Just text) -> Just text
        (Just "tool_reference", Nothing) ->
            ("Tool reference: " <>) <$> toolName
        (Just "image", Nothing) ->
            Just ("[image" <> maybe "" (" " <>) mediaType <> "]")
        _ -> Nothing

imageSourceMediaTypeDecoder :: Json.Decoder (Maybe Text)
imageSourceMediaTypeDecoder = Json.withType \case
    Json.VObject -> Json.object (optionalNonEmptyText "media_type")
    _ -> pure Nothing

usageDecoder :: Json.Decoder Usage
usageDecoder = Json.object do
    directInput <- nonNegativeNumber "input_tokens"
    cacheCreation <- nonNegativeNumber "cache_creation_input_tokens"
    cacheRead <- nonNegativeNumber "cache_read_input_tokens"
    outputTokens <- nonNegativeNumber "output_tokens"
    pure Usage
        { inputTokens = directInput + cacheCreation + cacheRead
        , cachedTokens = cacheRead
        , ..
        }

modelUsageDecoder :: Json.Decoder (Map Text ModelUsage)
modelUsageDecoder = Json.withType \case
    Json.VObject -> do
        entries <- Json.objectAsKeyValues
            (\key -> pure key)
            tolerantModelUsageDecoder
        pure $ case traverse sequenceEntry entries of
            Just valid -> Map.fromList valid
            Nothing -> Map.empty
    _ -> pure Map.empty
  where
    sequenceEntry (key, usage) = (key,) <$> usage

tolerantModelUsageDecoder :: Json.Decoder (Maybe ModelUsage)
tolerantModelUsageDecoder = Json.withType \case
    Json.VObject -> Json.object do
        inputTokens <- optionalNonNegativeNumber "inputTokens"
        outputTokens <- optionalNonNegativeNumber "outputTokens"
        cacheReadInputTokens <- optionalNonNegativeNumberDefault
            "cacheReadInputTokens"
        cacheCreationInputTokens <- optionalNonNegativeNumberDefault
            "cacheCreationInputTokens"
        webSearchRequests <- optionalNonNegativeNumberDefault
            "webSearchRequests"
        costUSD <- optionalNumber "costUSD" Json.double
        contextWindow <- optionalNonNegativeNumber "contextWindow"
        maxOutputTokens <- optionalNonNegativeNumber "maxOutputTokens"
        canonicalModel <- optionalNonEmptyText "canonicalModel"
        provider <- optionalNonEmptyText "provider"
        pure do
            input <- inputTokens
            output <- outputTokens
            pure ModelUsage
                { inputTokens = input
                , outputTokens = output
                , ..
                }
    _ -> pure Nothing

optionalOrigin
    :: Text
    -> Json.FieldsDecoder (Maybe MessageOrigin)
optionalOrigin key =
    (Json.atKeyOptional key $
        Json.withType \case
            Json.VObject -> do
                raw <- rawJsonDecoder
                case Json.decodeEither
                        (originDecoder raw)
                        (rawJsonBytes raw)
                    of
                    Left err ->
                        fail (Text.unpack err.jsonErrorMessage)
                    Right origin ->
                        pure (Just origin)
            _ -> pure Nothing)
        >>= pure . (>>= id)

originDecoder :: RawJson -> Json.Decoder MessageOrigin
originDecoder raw = Json.object do
    kind <- requiredNonEmptyText
        "kind"
        "message origin is missing a non-empty string `kind`"
    server <- optionalNonEmptyText "server"
    from <- optionalNonEmptyText "from"
    name <- optionalNonEmptyText "name"
    fromSession <- optionalNonEmptyText "fromSession"
    senderTaskId <- optionalNonEmptyText "senderTaskId"
    body <- optionalText "body"
    verifiedPeerPid <- optionalNumber "verifiedPeerPid" Json.int
    subkind <- optionalNonEmptyText "subkind"
    pure MessageOrigin{..}

requiredText
    :: Text
    -> Text
    -> Json.FieldsDecoder Text
requiredText key err =
    Json.atKeyOptional key tolerantOptionalText >>= \value ->
        maybe (fail (Text.unpack err)) pure (value >>= id)

requiredNonEmptyText
    :: Text
    -> Text
    -> Json.FieldsDecoder Text
requiredNonEmptyText key err =
    Json.atKeyOptional key tolerantOptionalText >>= \value ->
        maybe (fail (Text.unpack err)) pure
            (value >>= id >>= nonEmptyText)

requiredBool
    :: Text
    -> Text
    -> Json.FieldsDecoder Bool
requiredBool key err =
    Json.atKeyOptional key tolerantOptionalBool >>= \value ->
        maybe (fail (Text.unpack err)) pure (value >>= id)

optionalText :: Text -> Json.FieldsDecoder (Maybe Text)
optionalText key =
    (Json.atKeyOptional key tolerantOptionalText)
        >>= pure . (>>= id)

optionalNonEmptyText :: Text -> Json.FieldsDecoder (Maybe Text)
optionalNonEmptyText key =
    Json.atKeyOptional key tolerantOptionalText >>= \value ->
        pure (value >>= id >>= nonEmptyText)

optionalBool :: Text -> Json.FieldsDecoder (Maybe Bool)
optionalBool key =
    (Json.atKeyOptional key tolerantOptionalBool)
        >>= pure . (>>= id)

optionalRaw :: Text -> Json.FieldsDecoder (Maybe RawJson)
optionalRaw key =
    (Json.atKeyOptional key $
        Json.withType \case
            Json.VNull -> pure Nothing
            _ -> Just <$> rawJsonDecoder)
        >>= pure . (>>= id)

optionalTyped
    :: Text
    -> Json.Decoder a
    -> Json.FieldsDecoder (Maybe a)
optionalTyped key decoder =
    (Json.atKeyOptional key $
        Json.withType \case
            Json.VNull -> pure Nothing
            _ -> Just <$> decoder)
        >>= pure . (>>= id)

optionalNumber
    :: Text
    -> Json.Decoder number
    -> Json.FieldsDecoder (Maybe number)
optionalNumber key decoder =
    (Json.atKeyOptional key $
        Json.withType \case
            Json.VNumber -> Just <$> decoder
            _ -> pure Nothing)
        >>= pure . (>>= id)

nonNegativeNumber :: Text -> Json.FieldsDecoder Int
nonNegativeNumber key =
    max 0 . fromMaybe 0 <$> optionalNumber key Json.int

optionalNonNegativeNumber :: Text -> Json.FieldsDecoder (Maybe Int)
optionalNonNegativeNumber key =
    fmap (\value -> if value >= 0 then Just value else Nothing)
        <$> optionalNumber key Json.int
        >>= pure . (>>= id)

optionalNonNegativeNumberDefault :: Text -> Json.FieldsDecoder Int
optionalNonNegativeNumberDefault key =
    fromMaybe 0 <$> optionalNonNegativeNumber key

strictOptionalNonEmptyTextList
    :: Text
    -> Text
    -> Json.FieldsDecoder [Text]
strictOptionalNonEmptyTextList key err = do
    value <- Json.atKeyOptional key $
        Json.withType \case
            Json.VNull -> pure (Just [])
            Json.VArray -> Just <$> Json.list
                (Json.withType \case
                    Json.VString -> nonEmptyText <$> Json.text
                    _ -> pure Nothing)
            _ -> pure Nothing
    case value of
        Nothing -> pure []
        Just (Just elements)
            | Just texts <- sequence elements -> pure texts
        _ -> fail (Text.unpack err)

parentFieldPresent :: Json.FieldsDecoder Bool
parentFieldPresent =
    maybe False id <$> Json.atKeyOptional
        "parent_tool_use_id"
        (Json.withType \case
            Json.VNull -> pure False
            _ -> pure True)

tolerantOptionalText :: Json.Decoder (Maybe Text)
tolerantOptionalText = Json.withType \case
    Json.VString -> Just <$> Json.text
    _ -> pure Nothing

tolerantOptionalBool :: Json.Decoder (Maybe Bool)
tolerantOptionalBool = Json.withType \case
    Json.VBoolean -> Just <$> Json.bool
    _ -> pure Nothing

nonEmptyText :: Text -> Maybe Text
nonEmptyText value =
    let stripped = Text.strip value
    in if Text.null stripped then Nothing else Just stripped

normalizeSessionId :: Text -> Text
normalizeSessionId value =
    maybe value UUID.toText (UUID.fromText value)

displayBytes :: ByteString -> Text
displayBytes =
    Text.take 2_000 . TextEncoding.decodeUtf8With lenientDecode

conciseDecodeError :: Text -> Text
conciseDecodeError message
    | "expected a JSON object" `Text.isInfixOf` message =
        "expected a JSON object"
    | otherwise = message

emptyObjectJson :: RawJson
emptyObjectJson = rawJsonFromEncoding (Aeson.pairs mempty)
