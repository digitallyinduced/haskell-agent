-- | Stateless OpenAI Chat Completions adapter for local tool-capable models.
--
-- The harness keeps its canonical transcript as Responses items. This module
-- projects that transcript to Chat Completions messages, then normalizes each
-- completion back into a Responses-shaped result so the normal tool loop,
-- persistence, and compaction machinery remain shared.
module Agent.Responses.ChatCompletions
    ( ChatCompletionsOptions(..)
    , buildChatRequest
    , createChatCompletionWith
    , createChatCompletionWithEvents
    , normalizeChatCompletion
    , reinforceFunctionSchemas
    ) where

import Agent.Error (ApiError(..), ErrorType(InvalidRequestError))
import Agent.Responses.GenericClient
    ( classifyFailure
    , retryTransientResultWithPolicy
    )
import qualified Agent.Responses.HttpSSE as Http
import Agent.Responses.LoopBackend (assistantTextFromResponse)
import Agent.Responses.Types
import Control.Exception.Safe (tryAny)
import Control.Monad (foldM, unless)
import Control.Retry
    ( RetryPolicyM
    , exponentialBackoff
    , limitRetries
    )
import qualified Data.Aeson as Aeson
import Data.Aeson
    ( (.:)
    , (.:?)
    , (.!=)
    , withObject
    )
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe, isNothing, mapMaybe, maybeToList)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Network.HTTP.Simple hiding (Response)

data ChatCompletionsOptions = ChatCompletionsOptions
    { chatBaseUrl :: !String
      -- ^ API prefix including any version segment, but not
      -- @/chat/completions@.
    , chatModel :: !Text
      -- ^ Exact model identifier sent to the endpoint.
    , chatBearerToken :: !(Maybe Text)
      -- ^ Optional bearer token. 'Nothing' omits the Authorization header.
    , chatRequestTimeoutSeconds :: !Int
      -- ^ Full non-streaming request timeout.
    }
    deriving (Eq)

-- | Project a canonical Responses request to the Chat Completions subset
-- understood by apfel and similar local servers.
buildChatRequest
    :: ChatCompletionsOptions
    -> ResponseCreateParams
    -> Either ApiError Aeson.Value
buildChatRequest options rawRequest = do
    let request = reinforceFunctionSchemas rawRequest
    let responseTools = fromMaybe [] request.tools
    chatTools <- traverse chatTool responseTools
    messages <- responseInputMessages request.instructions request.input
    pure $ Aeson.object $
        [ "model" Aeson..= options.chatModel
        , "messages" Aeson..= messages
        , "stream" Aeson..= False
        -- apfel executes one Foundation Models request at a time and reports
        -- a single tool-call sequence, so make that contract explicit.
        , "parallel_tool_calls" Aeson..= False
        ]
            <> maybeToList
                (("max_tokens" Aeson..=) <$> request.maxOutputTokens)
            <> maybeToList
                (("temperature" Aeson..=) <$> request.temperature)
            <> maybeToList
                (("top_p" Aeson..=) <$> request.topP)
            <> maybeToList
                (("tool_choice" Aeson..=) . chatToolChoice
                    <$> request.toolChoice)
            <> ["tools" Aeson..= chatTools | not (null chatTools)]
  where
    responseInputMessages instructionText inputItems =
        (maybeToList (systemMessage <$> instructionText) <>)
            <$> projectResponseInput inputItems

-- | Apple Intelligence is substantially more reliable at producing object
-- arguments when the OpenAI function schemas also appear in its instruction
-- text. The native Foundation Models tool definitions alone can otherwise
-- yield positional payloads such as @[]@ even for object schemas.
--
-- Apply this to the canonical params before context sizing. 'buildChatRequest'
-- also applies it defensively for direct callers.
reinforceFunctionSchemas :: ResponseCreateParams -> ResponseCreateParams
reinforceFunctionSchemas request =
    case functionSchemaInstructions (fromMaybe [] request.tools) of
        Nothing -> request
        Just schemas -> case request of
            ResponseCreateParams{..} ->
                ResponseCreateParams
                { instructions =
                    appendInstructionsOnce instructions schemas
                , ..
                }

functionSchemaInstructions :: [ResponseTool] -> Maybe Text
functionSchemaInstructions tools =
    case mapMaybe functionSchema tools of
        [] -> Nothing
        schemas ->
            Just $
                "## Function Argument Schemas\n\
                \When calling a function, function.arguments must be a \
                \JSON-encoded object matching that function's schema. Never \
                \use an array as the top-level arguments value. Include every \
                \required field.\n"
                    <> Text.decodeUtf8
                        (LBS.toStrict (Aeson.encode schemas))
  where
    functionSchema = \case
        FunctionToolValue function ->
            Just $ Aeson.object $
                [ "name" Aeson..= function.name ]
                    <> maybeToList
                        (("parameters" Aeson..=) <$> function.parameters)
        _ -> Nothing

appendInstructionsOnce :: Maybe Text -> Text -> Maybe Text
appendInstructionsOnce base addition =
    case base of
        Nothing -> Just addition
        Just current
            | Text.null (Text.strip current) -> Just addition
            | addition `Text.isSuffixOf` current -> Just current
            | otherwise -> Just (current <> "\n\n" <> addition)

createChatCompletionWith
    :: ChatCompletionsOptions
    -> ResponseCreateParams
    -> IO (Either ApiError Response)
createChatCompletionWith options request =
    createChatCompletionWithEvents options request (const (pure ()))

-- | Send one non-streaming completion while adapting final assistant text to
-- the usual Responses delta callback consumed by the UI.
createChatCompletionWithEvents
    :: ChatCompletionsOptions
    -> ResponseCreateParams
    -> (ResponseStreamEvent -> IO ())
    -> IO (Either ApiError Response)
createChatCompletionWithEvents options request onEvent =
    retryTransientResultWithPolicy
        transientResultPolicy
        performOnce
        onEvent
  where
    performOnce emit = do
        case buildChatRequest options request of
            Left err -> pure (Left err)
            Right requestBody -> do
                result <- Http.performChatCompletionsHttpJson
                    Http.HttpJsonConfig
                        { Http.jsonExceptionPrefix =
                            "Chat Completions request failed"
                        , Http.jsonClassifyFailure = classifyFailure
                        }
                    options.chatBaseUrl
                    options.chatRequestTimeoutSeconds
                    (Aeson.encode requestBody)
                    configureRequest
                case result >>= chatCompletionToResponse of
                    Left err -> pure (Left err)
                    Right response ->
                        emitNonStreamingText emit response >>= \case
                            Left err -> pure (Left err)
                            Right () -> pure (Right response)

    configureRequest =
        maybe
            id
            (\token ->
                setRequestHeader
                    "Authorization"
                    ["Bearer " <> Text.encodeUtf8 token])
            (nonEmptyText options.chatBearerToken)
            . setRequestHeader "User-Agent" ["haskell-agent"]

data ChatCompletion = ChatCompletion
    { chatCompletionId :: !Text
    , chatCompletionCreated :: !Aeson.Value
    , chatCompletionModel :: !Text
    , chatCompletionChoices :: ![ChatChoice]
    , chatCompletionUsage :: !(Maybe ChatUsage)
    }

instance Aeson.FromJSON ChatCompletion where
    parseJSON = withObject "ChatCompletion" \object ->
        ChatCompletion
            <$> object .: "id"
            <*> object .: "created"
            <*> object .: "model"
            <*> object .: "choices"
            <*> object .:? "usage"

data ChatChoice = ChatChoice
    { chatChoiceMessage :: !ChatMessage
    , chatChoiceFinishReason :: !(Maybe Text)
    }

instance Aeson.FromJSON ChatChoice where
    parseJSON = withObject "ChatChoice" \object ->
        ChatChoice
            <$> object .: "message"
            <*> object .:? "finish_reason"

data ChatMessage = ChatMessage
    { chatMessageRole :: !Text
    , chatMessageContent :: !(Maybe Text)
    , chatMessageToolCalls :: ![ChatToolCall]
    }

instance Aeson.FromJSON ChatMessage where
    parseJSON = withObject "ChatMessage" \object ->
        ChatMessage
            <$> object .: "role"
            <*> object .:? "content"
            <*> object .:? "tool_calls" .!= []

data ChatToolCall = ChatToolCall
    { chatToolCallId :: !Text
    , chatToolCallType :: !Text
    , chatToolCallFunction :: !ChatFunction
    }

instance Aeson.FromJSON ChatToolCall where
    parseJSON = withObject "ChatToolCall" \object ->
        ChatToolCall
            <$> object .: "id"
            <*> object .: "type"
            <*> object .: "function"

data ChatFunction = ChatFunction
    { chatFunctionName :: !Text
    , chatFunctionArguments :: !Text
    }

instance Aeson.FromJSON ChatFunction where
    parseJSON = withObject "ChatFunction" \object ->
        ChatFunction
            <$> object .: "name"
            <*> object .:? "arguments" .!= "{}"

data ChatUsage = ChatUsage
    { chatPromptTokens :: !(Maybe Int)
    , chatCompletionTokens :: !(Maybe Int)
    , chatTotalTokens :: !(Maybe Int)
    }

instance Aeson.FromJSON ChatUsage where
    parseJSON = withObject "ChatUsage" \object ->
        ChatUsage
            <$> object .:? "prompt_tokens"
            <*> object .:? "completion_tokens"
            <*> object .:? "total_tokens"

chatCompletionToResponse :: ChatCompletion -> Either ApiError Response
chatCompletionToResponse completion = do
    unless (isNonEmpty completion.chatCompletionId) $
        Left (invalidRequest
            "Chat Completions response contained an empty id")
    unless (isNonEmpty completion.chatCompletionModel) $
        Left (invalidRequest
            "Chat Completions response contained an empty model")
    choice <- case completion.chatCompletionChoices of
        first : _ -> Right first
        [] -> Left (ConnectionError
            "Chat Completions response did not contain a choice")
    unless (choice.chatChoiceMessage.chatMessageRole == "assistant") $
        Left (invalidRequest
            "Chat Completions response message was not from the assistant")
    case choice.chatChoiceFinishReason of
        Nothing -> pure ()
        Just "stop" -> pure ()
        Just "tool_calls" -> pure ()
        Just "length" ->
            Left (invalidRequest
                "Apple Intelligence stopped at its output-token limit")
        Just reason ->
            Left (invalidRequest
                ("Apple Intelligence returned unsupported finish_reason "
                    <> reason))
    let message = choice.chatChoiceMessage
    validateChatToolCalls message.chatMessageToolCalls
    let
        output =
            maybeToList
                (assistantTextItem <$> nonEmptyText message.chatMessageContent)
                <> map chatToolCallItem message.chatMessageToolCalls
    if null output
        then Left (ConnectionError
            "Chat Completions response contained neither text nor tool calls")
        else decodeResponse completion output

-- | Normalize a decoded Chat Completions payload into the canonical Responses
-- value consumed by the shared loop.
normalizeChatCompletion :: Aeson.Value -> Either ApiError Response
normalizeChatCompletion value =
    case Aeson.fromJSON value of
        Aeson.Error err ->
            Left (ConnectionError
                ("Chat Completions response could not be decoded: "
                    <> Text.pack err))
        Aeson.Success completion ->
            chatCompletionToResponse completion

validateChatToolCalls :: [ChatToolCall] -> Either ApiError ()
validateChatToolCalls = go Set.empty
  where
    go _ [] = pure ()
    go seen (call : rest) = do
        unless (call.chatToolCallType == "function") $
            Left (invalidRequest
                "Chat Completions returned a non-function tool call")
        unless (isNonEmpty call.chatToolCallId) $
            Left (invalidRequest
                "Chat Completions returned a tool call with an empty id")
        unless (isNonEmpty call.chatToolCallFunction.chatFunctionName) $
            Left (invalidRequest
                "Chat Completions returned a tool call with an empty name")
        unless (Set.notMember call.chatToolCallId seen) $
            Left (invalidRequest
                "Chat Completions returned duplicate tool-call ids")
        go (Set.insert call.chatToolCallId seen) rest

decodeResponse
    :: ChatCompletion
    -> [ResponseItem]
    -> Either ApiError Response
decodeResponse completion output =
    case Aeson.fromJSON payload of
        Aeson.Error err ->
            Left (ConnectionError
                ("Chat Completions response could not be normalized: "
                    <> Text.pack err))
        Aeson.Success response -> Right response
  where
    payload = Aeson.object $
        [ "id" Aeson..= completion.chatCompletionId
        , "created_at" Aeson..= completion.chatCompletionCreated
        , "model" Aeson..= completion.chatCompletionModel
        , "status" Aeson..= ("completed" :: Text)
        , "output" Aeson..= output
        ]
            <> maybeToList
                (chatUsageField <$> completion.chatCompletionUsage)

chatUsageField usage =
    "usage" Aeson..= Aeson.object
        [ "input_tokens" Aeson..= fromMaybe 0 usage.chatPromptTokens
        , "output_tokens" Aeson..= fromMaybe 0 usage.chatCompletionTokens
        , "total_tokens" Aeson..= fromMaybe 0 usage.chatTotalTokens
        ]

assistantTextItem :: Text -> ResponseItem
assistantTextItem text = MessageItem ResponseMessage
    { messageId = Nothing
    , content = MessageContentParts
        [ OutputTextPart
            { text
            , annotations = Nothing
            , logprobs = Nothing
            , extraFields = KeyMap.empty
            }
        ]
    , role = RoleAssistant
    , status = Just ItemCompleted
    , phase = Nothing
    , passthrough = Nothing
    , extraFields = KeyMap.empty
    }

chatToolCallItem :: ChatToolCall -> ResponseItem
chatToolCallItem call = FunctionCallItem FunctionCall
    { itemId = Nothing
    , callId = call.chatToolCallId
    , name = call.chatToolCallFunction.chatFunctionName
    , namespace = Nothing
    , arguments = call.chatToolCallFunction.chatFunctionArguments
    , encryptedFunctionArgs = Nothing
    , status = Just ItemCompleted
    , extraFields = KeyMap.empty
    }

systemMessage :: Text -> Aeson.Value
systemMessage text =
    Aeson.object
        [ "role" Aeson..= ("system" :: Text)
        , "content" Aeson..= text
        ]

projectResponseInput
    :: Maybe ResponseInput
    -> Either ApiError [Aeson.Value]
projectResponseInput = \case
    Nothing -> pure []
    Just (ResponseInputText text) ->
        pure [Aeson.object
            [ "role" Aeson..= ("user" :: Text)
            , "content" Aeson..= text
            ]
        ]
    Just (ResponseInputItems items) -> responseItemsToChatMessages items

responseItemsToChatMessages
    :: [ResponseItem]
    -> Either ApiError [Aeson.Value]
responseItemsToChatMessages = go Set.empty
  where
    go outstanding = \case
        []
            | Set.null outstanding -> pure []
            | otherwise ->
                Left (invalidRequest
                    "Apple Intelligence history contains a tool call without a result")
        MessageItem message : rest
            | not (Set.null outstanding) ->
                Left (invalidRequest
                    "Apple Intelligence history contains a message before the preceding tool result")
            | message.role == RoleAssistant ->
                let (calls, remaining) = spanFunctionCalls rest
                in if null calls
                    then do
                        value <- chatMessage message
                        (value :) <$> go outstanding rest
                    else do
                        nextOutstanding <-
                            registerFunctionCalls outstanding calls
                        let value = assistantToolCallMessage
                                (nonEmptyText
                                    (Just
                                        (messageContentText
                                            message.content)))
                                calls
                        (value :) <$> go nextOutstanding remaining
            | otherwise -> do
                value <- chatMessage message
                (value :) <$> go outstanding rest
        first@(FunctionCallItem _) : rest ->
            if not (Set.null outstanding)
                then Left (invalidRequest
                    "Apple Intelligence history contains overlapping tool calls")
                else
                    let (calls, remaining) =
                            spanFunctionCalls (first : rest)
                    in do
                        nextOutstanding <-
                            registerFunctionCalls outstanding calls
                        let value =
                                assistantToolCallMessage Nothing calls
                        (value :) <$> go nextOutstanding remaining
        FunctionCallOutputItem call : rest -> do
            unless (isNonEmpty call.callId) $
                Left (invalidRequest
                    "Apple Intelligence history contains an empty tool-call id")
            unless (Set.member call.callId outstanding) $
                Left (invalidRequest
                    "Apple Intelligence history contains an orphaned tool result")
            let value =
                    Aeson.object
                        [ "role" Aeson..= ("tool" :: Text)
                        , "tool_call_id" Aeson..= call.callId
                        , "content" Aeson..=
                            nonEmptyToolOutput call.output
                        ]
            (value :) <$> go (Set.delete call.callId outstanding) rest
        _ ->
            Left (invalidRequest
                "Apple Intelligence cannot continue this session because its history contains an unsupported item")

spanFunctionCalls :: [ResponseItem] -> ([FunctionCall], [ResponseItem])
spanFunctionCalls = go []
  where
    go reversed = \case
        FunctionCallItem call : rest -> go (call : reversed) rest
        rest -> (reverse reversed, rest)

registerFunctionCalls
    :: Set.Set Text
    -> [FunctionCall]
    -> Either ApiError (Set.Set Text)
registerFunctionCalls = foldM step
  where
    step outstanding call = do
        unless (isNonEmpty call.callId) $
            Left (invalidRequest
                "Apple Intelligence history contains an empty tool-call id")
        unless (isNonEmpty call.name) $
            Left (invalidRequest
                "Apple Intelligence history contains an empty tool name")
        unless (isNothing call.namespace) $
            Left (invalidRequest
                "Apple Intelligence does not support namespaced tool history")
        unless (Set.notMember call.callId outstanding) $
            Left (invalidRequest
                "Apple Intelligence history contains duplicate tool-call ids")
        pure (Set.insert call.callId outstanding)

chatMessage :: ResponseMessage -> Either ApiError Aeson.Value
chatMessage message = do
    role <- chatRole message.role
    pure $ Aeson.object
        [ "role" Aeson..= role
        , "content" Aeson..= messageContentText message.content
        ]

assistantToolCallMessage
    :: Maybe Text
    -> [FunctionCall]
    -> Aeson.Value
assistantToolCallMessage content calls =
    Aeson.object
        [ "role" Aeson..= ("assistant" :: Text)
        , "content" Aeson..= maybe Aeson.Null Aeson.String content
        , "tool_calls" Aeson..= map functionCallValue calls
        ]

functionCallValue :: FunctionCall -> Aeson.Value
functionCallValue call =
    Aeson.object
        [ "id" Aeson..= call.callId
        , "type" Aeson..= ("function" :: Text)
        , "function" Aeson..= Aeson.object
            [ "name" Aeson..= call.name
            , "arguments" Aeson..= call.arguments
            ]
        ]

chatTool :: ResponseTool -> Either ApiError Aeson.Value
chatTool = \case
    FunctionToolValue function -> do
        unless (isNonEmpty function.name) $
            Left (invalidRequest
                "Apple Intelligence received a function tool with an empty name")
        pure $ Aeson.object
            [ "type" Aeson..= ("function" :: Text)
            , "function" Aeson..= Aeson.object
                ( [ "name" Aeson..= function.name ]
                    <> maybeToList
                        (("description" Aeson..=) <$> function.description)
                    <> maybeToList
                        (("parameters" Aeson..=) <$> function.parameters)
                    <> maybeToList
                        (("strict" Aeson..=) <$> function.strict)
                )
            ]
    _ ->
        Left (invalidRequest
            "Apple Intelligence supports function tools only")

chatToolChoice :: ToolChoice -> Aeson.Value
chatToolChoice choice = case choice of
    ToolChoiceObject tagged
        | tagged.tag == "function"
        , Just name <- KeyMap.lookup "name" tagged.fields ->
            Aeson.object
                [ "type" Aeson..= ("function" :: Text)
                , "function" Aeson..= Aeson.object ["name" Aeson..= name]
                ]
    _ -> Aeson.toJSON choice

chatRole :: ResponseRole -> Either ApiError Text
chatRole = \case
    RoleAssistant -> pure "assistant"
    RoleSystem -> pure "system"
    RoleDeveloper -> pure "system"
    RoleUser -> pure "user"
    RoleUnknown _ ->
        Left (invalidRequest
            "Apple Intelligence history contains an unsupported message role")

messageContentText :: MessageContent -> Text
messageContentText = \case
    MessageContentText text -> text
    MessageContentParts parts -> Text.concat (map contentPartText parts)

contentPartText :: ResponseContentPart -> Text
contentPartText = \case
    InputTextPart{text} -> text
    OutputTextPart{text} -> text
    PlainTextPart{text} -> text
    RefusalPart{refusal} -> refusal
    ReasoningTextPart{text} -> text
    SummaryTextPart{text} -> text
    InputImagePart{} -> "\n[image omitted by the local chat adapter]\n"
    InputFilePart{} -> "\n[file omitted by the local chat adapter]\n"
    InputAudioPart{} -> "\n[audio omitted by the local chat adapter]\n"
    EncryptedContentPart{} -> ""
    UnknownContentPart{} -> ""

renderToolOutput :: Aeson.Value -> Text
renderToolOutput = \case
    Aeson.String text -> text
    value -> Text.decodeUtf8 (LBS.toStrict (Aeson.encode value))

nonEmptyToolOutput :: Aeson.Value -> Text
nonEmptyToolOutput value =
    let rendered = renderToolOutput value
    in if Text.null (Text.strip rendered)
        then "(no output)"
        else rendered

emitNonStreamingText
    :: (ResponseStreamEvent -> IO ())
    -> Response
    -> IO (Either ApiError ())
emitNonStreamingText emit response =
    tryAny
        (mapM_
            (\text ->
                emit OtherResponseStreamEvent
                    { otherEventType = EventOutputTextDelta
                    , sequenceNumber = Nothing
                    , eventExtraFields =
                        KeyMap.singleton "delta" (Aeson.String text)
                    })
            (assistantTextFromResponse response)) >>= \case
                Left exception ->
                    pure $ Left $ ConnectionError
                        ( "Chat Completions callback failed: "
                            <> Text.pack (show exception)
                        )
                Right () -> pure (Right ())

nonEmptyText :: Maybe Text -> Maybe Text
nonEmptyText (Just value)
    | not (Text.null (Text.strip value)) = Just value
nonEmptyText _ = Nothing

isNonEmpty :: Text -> Bool
isNonEmpty = not . Text.null . Text.strip

invalidRequest :: Text -> ApiError
invalidRequest message =
    ProviderError InvalidRequestError message Nothing

transientResultPolicy :: RetryPolicyM IO
transientResultPolicy = exponentialBackoff 1_000_000 <> limitRetries 3
