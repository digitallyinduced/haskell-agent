{-# LANGUAGE ExistentialQuantification #-}

module Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallKind(..)
    , ToolCallResult(..)
    , ToolDispatchConfig(..)
    , ToolRuntime(..)
    , ToolHandler
    , typedTool
    , typedToolWithCall
    , typedToolWithRuntimeAndCall
    , typedStreamingTool
    , noArgsTool
    , functionToolCall
    , customToolCall
    , canonicalToolName
    , dispatchToolCall
    , dispatchToolHandler
    , handlerName
    , toolArgumentsValue
    , decodeToolArguments
    ) where

import Agent.Responses.Types (Response, ResponseCreateParams)
import Agent.ToolArgs (stripAesonPrefix)
import Control.Applicative ((<|>))
import Control.Exception.Safe (SomeException, tryAny)
import Data.Aeson (FromJSON, Value(..))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseEither)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

-- | How the originating model turn encoded this call. Adapters need this to
-- emit @function_call_output@ versus @custom_tool_call_output@.
data ToolCallKind
    = FunctionCallKind
    | CustomCallKind
    deriving (Eq, Show)

-- | Provider-neutral function or custom tool call emitted by a model transport.
data ToolCall = ToolCall
    { callId :: !Text
    , name :: !Text
    , arguments :: !Text
    , callKind :: !ToolCallKind
    , argumentsEncrypted :: !Bool
    } deriving (Eq)

instance Show ToolCall where
    show call =
        "ToolCall { callId = " <> show call.callId
            <> ", name = " <> show call.name
            <> ", arguments = " <> shownArguments
            <> ", callKind = " <> show call.callKind
            <> ", argumentsEncrypted = " <> show call.argumentsEncrypted
            <> " }"
      where
        shownArguments
            | call.argumentsEncrypted = "<redacted>"
            | otherwise = show call.arguments

-- | Provider-neutral result ready for a transport adapter to encode.
data ToolCallResult = ToolCallResult
    { callId :: !Text
    , output :: !Text
    , callKind :: !ToolCallKind
    } deriving (Eq, Show)

functionToolCall :: Text -> Text -> Text -> ToolCall
functionToolCall callId name arguments = ToolCall
    { callId
    , name
    , arguments
    , callKind = FunctionCallKind
    , argumentsEncrypted = False
    }

customToolCall :: Text -> Text -> Text -> ToolCall
customToolCall callId name arguments = ToolCall
    { callId
    , name
    , arguments
    , callKind = CustomCallKind
    , argumentsEncrypted = False
    }

data ToolDispatchConfig = ToolDispatchConfig
    { toolDispatchUnknownTool :: Text -> Text
    , toolDispatchFormatResult :: Either Text Text -> Text
    , toolDispatchFormatException :: Text -> SomeException -> Text
    , toolDispatchOnException :: Text -> SomeException -> IO ()
    , toolDispatchOnOutput :: ToolCall -> Text -> IO ()
    , toolDispatchRuntime :: !(Maybe ToolRuntime)
    , toolDispatchCallResponses
        :: !(Maybe
            ([ResponseCreateParams] -> IO [Either Text Response]))
    }

-- | Capabilities supplied by the active agent loop to a tool handler.
--
-- Nested calls use the same approval, event, registry, and dispatch path as
-- model-authored calls, but their results remain local to the invoking tool.
data ToolRuntime = ToolRuntime
    { invokeNestedTool :: ToolCall -> IO ToolCallResult
    , invokeNestedTools :: [ToolCall] -> IO [ToolCallResult]
    , invokeNestedResponses
        :: [ResponseCreateParams] -> IO [Either Text Response]
    }

data ToolHandler
    = forall args. FromJSON args => TypedTool Text (args -> IO (Either Text Text))
    | forall args. FromJSON args => TypedToolWithCall Text (ToolCall -> args -> IO (Either Text Text))
    | forall args. FromJSON args => TypedToolWithRuntimeAndCall Text
        (ToolRuntime -> ToolCall -> args -> IO (Either Text Text))
    | forall args. FromJSON args => TypedStreamingTool Text ((Text -> IO ()) -> args -> IO (Either Text Text))
    | NoArgsTool Text (IO (Either Text Text))

typedTool :: FromJSON args => Text -> (args -> IO (Either Text Text)) -> ToolHandler
typedTool = TypedTool

typedToolWithCall :: FromJSON args => Text -> (ToolCall -> args -> IO (Either Text Text)) -> ToolHandler
typedToolWithCall = TypedToolWithCall

typedToolWithRuntimeAndCall
    :: FromJSON args
    => Text
    -> (ToolRuntime -> ToolCall -> args -> IO (Either Text Text))
    -> ToolHandler
typedToolWithRuntimeAndCall = TypedToolWithRuntimeAndCall

-- | A typed tool that can publish accumulated output snapshots while running.
-- The final result remains authoritative.
typedStreamingTool
    :: FromJSON args
    => Text
    -> ((Text -> IO ()) -> args -> IO (Either Text Text))
    -> ToolHandler
typedStreamingTool = TypedStreamingTool

noArgsTool :: Text -> IO (Either Text Text) -> ToolHandler
noArgsTool = NoArgsTool

dispatchToolCall :: ToolDispatchConfig -> [ToolHandler] -> ToolCall -> IO ToolCallResult
dispatchToolCall config handlers call =
    dispatchToolHandler config (findHandler call.name handlers) call

-- | Dispatch with an already-resolved handler. Registries should prefer this
-- entry point so canonical-name lookup and uniqueness checks happen once.
dispatchToolHandler
    :: ToolDispatchConfig
    -> Maybe ToolHandler
    -> ToolCall
    -> IO ToolCallResult
dispatchToolHandler config maybeHandler call = do
    let callName = call.name
        input = toolArgumentsValue call.arguments
        runTool = case maybeHandler of
            Just handler ->
                runHandler
                    (config.toolDispatchOnOutput call)
                    config.toolDispatchRuntime
                    call
                    input
                    handler
            Nothing -> pure (Left (config.toolDispatchUnknownTool callName))
    result <- tryAny runTool
    resultOutput <- case result of
        Right toolResult ->
            pure (config.toolDispatchFormatResult toolResult)
        Left exception -> do
            config.toolDispatchOnException callName exception
            pure (config.toolDispatchFormatException callName exception)
    pure ToolCallResult
        { callId = call.callId
        , output = resultOutput
        , callKind = call.callKind
        }

toolArgumentsValue :: Text -> Value
toolArgumentsValue arguments =
    case Aeson.eitherDecodeStrict' (TextEncoding.encodeUtf8 arguments) of
        Right value -> value
        Left _ -> String arguments

decodeToolArguments :: FromJSON args => Value -> Either Text args
decodeToolArguments value =
    case parseEither Aeson.parseJSON value of
        Right args -> Right args
        Left err -> Left (stripAesonPrefix (Text.pack err))

findHandler :: Text -> [ToolHandler] -> Maybe ToolHandler
findHandler name handlers =
    find ((== name) . handlerName) handlers
        <|> find ((== canonicalToolName name) . handlerName) handlers

-- | Codex namespaced tools may arrive as @collaboration.spawn_agent@ or
-- legacy @multi_agent_v1.spawn_agent@ (and concatenated Display forms).
canonicalToolName :: Text -> Text
canonicalToolName name
    | Just rest <- Text.stripPrefix "functions." name = rest
    | Just rest <- Text.stripPrefix "collaboration." name = rest
    | Just rest <- Text.stripPrefix "collaboration" name
    , rest `elem` multiAgentBareNames =
        rest
    | Just rest <- Text.stripPrefix "multi_agent_v1." name = rest
    | Just rest <- Text.stripPrefix "multi_agent_v1" name
    , rest `elem` multiAgentBareNames =
        rest
    | otherwise = name

multiAgentBareNames :: [Text]
multiAgentBareNames =
    [ "spawn_agent"
    , "wait_agent"
    , "send_message"
    , "followup_task"
    , "list_agents"
    , "interrupt_agent"
    , "send_input"
    , "close_agent"
    , "resume_agent"
    ]

handlerName :: ToolHandler -> Text
handlerName = \case
    TypedTool name _ -> name
    TypedToolWithCall name _ -> name
    TypedToolWithRuntimeAndCall name _ -> name
    TypedStreamingTool name _ -> name
    NoArgsTool name _ -> name

runHandler
    :: (Text -> IO ())
    -> Maybe ToolRuntime
    -> ToolCall
    -> Value
    -> ToolHandler
    -> IO (Either Text Text)
runHandler emitOutput runtime call value = \case
    TypedTool _ run ->
        case decodeToolArguments value of
            Right args -> run args
            Left err -> pure (Left err)
    TypedToolWithCall _ run ->
        case decodeToolArguments value of
            Right args -> run call args
            Left err -> pure (Left err)
    TypedToolWithRuntimeAndCall _ run ->
        case (runtime, decodeToolArguments value) of
            (Nothing, _) ->
                pure (Left "This tool requires an active agent-loop runtime.")
            (_, Left err) -> pure (Left err)
            (Just activeRuntime, Right args) ->
                run activeRuntime call args
    TypedStreamingTool _ run ->
        case decodeToolArguments value of
            Right args -> run emitOutput args
            Left err -> pure (Left err)
    NoArgsTool _ run ->
        run
