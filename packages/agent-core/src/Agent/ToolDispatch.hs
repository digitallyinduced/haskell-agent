{-# LANGUAGE ExistentialQuantification #-}

module Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallKind(..)
    , ToolCallResult(..)
    , ToolDispatchConfig(..)
    , ToolHandler
    , typedTool
    , typedToolWithCall
    , typedStreamingTool
    , textTool
    , streamingTextTool
    , noArgsTool
    , functionToolCall
    , customToolCall
    , canonicalToolName
    , canonicalToolArguments
    , dispatchToolCall
    , dispatchToolHandler
    , handlerName
    , toolArgumentsValue
    , decodeToolArguments
    ) where

import Agent.Dialect
    ( claudeCodeCanonicalToolName
    , grokBuildCanonicalToolName
    )
import Agent.Json.Decode (Decoder)
import qualified Agent.Json.Decode as Json
import Control.Applicative ((<|>))
import Control.Exception.Safe (SomeException, tryAny)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text

-- | How the originating model turn encoded this call. Adapters need this to
-- emit @function_call_output@ versus @custom_tool_call_output@.
data ToolCallKind
    = FunctionCallKind
    | CustomCallKind
    -- | A provider-native computer call. Its arguments are the encoded
    -- action list, and its result is encoded by the provider adapter as a
    -- structured screenshot output rather than a function output string.
    | ComputerCallKind
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
    , toolDispatchFinalizeOutput :: ToolCall -> Text -> IO Text
    }

data ToolHandler
    = forall args. TypedTool Text (Decoder args) (args -> IO (Either Text Text))
    | forall args. TypedToolWithCall Text (Decoder args) (ToolCall -> args -> IO (Either Text Text))
    | forall args. TypedStreamingTool Text (Decoder args) ((Text -> IO ()) -> args -> IO (Either Text Text))
    | TextTool Text (Text -> IO (Either Text Text))
    | StreamingTextTool Text ((Text -> IO ()) -> Text -> IO (Either Text Text))
    | NoArgsTool Text (IO (Either Text Text))

typedTool :: Text -> Decoder args -> (args -> IO (Either Text Text)) -> ToolHandler
typedTool = TypedTool

typedToolWithCall :: Text -> Decoder args -> (ToolCall -> args -> IO (Either Text Text)) -> ToolHandler
typedToolWithCall = TypedToolWithCall

-- | A typed tool that can publish accumulated output snapshots while running.
-- The final result remains authoritative.
typedStreamingTool
    :: Text
    -> Decoder args
    -> ((Text -> IO ()) -> args -> IO (Either Text Text))
    -> ToolHandler
typedStreamingTool = TypedStreamingTool

-- | A freeform tool whose input is plain text rather than JSON.
textTool
    :: Text
    -> (Text -> IO (Either Text Text))
    -> ToolHandler
textTool = TextTool

-- | A streaming freeform tool. Its input is not JSON, so no JSON decoder is
-- involved.
streamingTextTool
    :: Text
    -> ((Text -> IO ()) -> Text -> IO (Either Text Text))
    -> ToolHandler
streamingTextTool = StreamingTextTool

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
        input = canonicalToolArguments call.name call.arguments
        runTool = case maybeHandler of
            Just handler ->
                runHandler
                    (config.toolDispatchOnOutput call)
                    call
                    input
                    handler
            Nothing -> pure (Left (config.toolDispatchUnknownTool callName))
    result <- tryAny runTool
    resultOutput <- case result of
        Right toolResult ->
            pure (config.toolDispatchFormatResult toolResult)
        Left exception -> do
            -- Diagnostics must not replace the original tool failure with a
            -- second exception. 'tryAny' still lets asynchronous cancellation
            -- propagate.
            _ <- tryAny (config.toolDispatchOnException callName exception)
            pure (config.toolDispatchFormatException callName exception)
    finalizedOutput <-
        tryAny (config.toolDispatchFinalizeOutput call resultOutput) >>= \case
            Right output -> pure output
            Left exception -> do
                _ <- tryAny (config.toolDispatchOnException callName exception)
                pure resultOutput
    pure ToolCallResult
        { callId = call.callId
        , output = finalizedOutput
        , callKind = call.callKind
        }

toolArgumentsValue :: Text -> Text
toolArgumentsValue = id

decodeToolArguments :: Decoder args -> Text -> Either Text args
decodeToolArguments decoder value =
    case Json.decodeText decoder value of
        Right args -> Right args
        Left originalError ->
            case Json.decodeEither decoder
                (LBS.toStrict (Aeson.encode value)) of
                Right args -> Right args
                Left _ -> Left originalError.jsonErrorMessage

findHandler :: Text -> [ToolHandler] -> Maybe ToolHandler
findHandler name handlers =
    Map.lookup name byName <|> Map.lookup (canonicalToolName name) byName
  where
    byName =
        Map.fromList
            [ (handlerName handler, handler)
            | handler <- handlers
            ]

-- | Codex namespaced tools may arrive as @collaboration.spawn_agent@ or
-- legacy @multi_agent_v1.spawn_agent@ (and concatenated Display forms).
canonicalToolName :: Text -> Text
canonicalToolName name
    | grokName /= name = grokName
    | claudeName /= name = claudeName
    | Just rest <- Text.stripPrefix "collaboration." name =
        canonicalToolName rest
    | Just rest <- Text.stripPrefix "collaboration" name
    , rest `elem` multiAgentBareNames =
        canonicalToolName rest
    | Just rest <- Text.stripPrefix "multi_agent_v1." name =
        canonicalToolName rest
    | Just rest <- Text.stripPrefix "multi_agent_v1" name
    , rest `elem` multiAgentBareNames =
        canonicalToolName rest
    | otherwise = name
  where
    grokName = grokBuildCanonicalToolName name
    claudeName = claudeCodeCanonicalToolName name

-- | Project current public Grok Build parameter names back onto the stable
-- internal handler contract. Keep this beside 'canonicalToolName' so every
-- dispatch path applies the same compatibility mapping.
canonicalToolArguments :: Text -> Text -> Text
canonicalToolArguments _ = id

multiAgentBareNames :: [Text]
multiAgentBareNames =
    [ "spawn_agent"
    , "spawn_agent_in_worktree"
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
    TypedTool name _ _ -> name
    TypedToolWithCall name _ _ -> name
    TypedStreamingTool name _ _ -> name
    TextTool name _ -> name
    StreamingTextTool name _ -> name
    NoArgsTool name _ -> name

runHandler
    :: (Text -> IO ())
    -> ToolCall
    -> Text
    -> ToolHandler
    -> IO (Either Text Text)
runHandler emitOutput call value = \case
    TypedTool _ decoder run -> decodeAndRun decoder value run
    TypedToolWithCall _ decoder run -> decodeAndRun decoder value (run call)
    TypedStreamingTool _ decoder run -> decodeAndRun decoder value (run emitOutput)
    TextTool _ run -> run value
    StreamingTextTool _ run -> run emitOutput value
    NoArgsTool _ run ->
        run

decodeAndRun
    :: Decoder args
    -> Text
    -> (args -> IO (Either Text Text))
    -> IO (Either Text Text)
decodeAndRun decoder value run =
    either (pure . Left) run (decodeToolArguments decoder value)
