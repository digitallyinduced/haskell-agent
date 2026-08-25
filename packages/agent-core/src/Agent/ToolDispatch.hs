{-# LANGUAGE ExistentialQuantification #-}

module Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallKind(..)
    , ToolCallResult(..)
    , ToolCallStreamRef(..)
    , ToolArgumentStreamEvent(..)
    , ToolArgumentUpdate(..)
    , ToolArgumentStreamItem(..)
    , ToolArgumentSource
    , PreparedToolResult
    , ToolArgumentInterpreter
    , ToolArgumentInterpreterFactory
    , ToolDispatchConfig(..)
    , ToolHandler
    , typedTool
    , typedToolWithCall
    , typedStreamingTool
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

import Agent.ToolArgs (stripAesonPrefix)
import Agent.Dialect (grokBuildCanonicalToolName)
import Control.Applicative ((<|>))
import Control.Exception.Safe (SomeException, tryAny)
import Data.Aeson (FromJSON, Value(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (parseEither)
import Data.Acquire (Acquire)
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

-- | Provider-neutral references used to correlate streamed argument events
-- with the output item they belong to. Providers may expose one or both
-- references; the speculation runtime keeps aliases together.
data ToolCallStreamRef
    = ToolCallStreamItem !Text
    | ToolCallStreamOutput !Int
    | ToolCallStreamCall !Text
    deriving (Eq, Ord, Show)

-- | Canonical lifecycle events consumed by tool argument interpreters. Provider
-- adapters translate their native stream events into this small vocabulary.
data ToolArgumentStreamEvent
    = ToolArgumentsStarted
        { argumentStreamRefs :: ![ToolCallStreamRef]
        , argumentStreamCallId :: !Text
        , argumentStreamName :: !(Maybe Text)
        , argumentStreamArguments :: !Text
        }
    | ToolArgumentsDelta
        { argumentStreamRefs :: ![ToolCallStreamRef]
        , argumentStreamDelta :: !Text
        }
    | ToolArgumentsDone
        { argumentStreamRefs :: ![ToolCallStreamRef]
        , argumentStreamName :: !(Maybe Text)
        , argumentStreamArguments :: !Text
        }
    | ToolCallStreamCompleted
        { argumentStreamRefs :: ![ToolCallStreamRef]
        , argumentStreamCall :: !ToolCall
        }
    deriving (Eq, Show)

data ToolArgumentUpdate
    = ToolArgumentDeltaUpdate !Text
    | ToolArgumentDoneUpdate !Text
    deriving (Eq, Show)

-- | Semantic input consumed by one tool-owned streamed-argument interpreter.
-- Resource shutdown is deliberately not represented here: abandoning the
-- interpreter cancels its owner scope instead of sending a synthetic event.
data ToolArgumentStreamItem
    = ToolArgumentStreamUpdate !ToolArgumentUpdate
    | ToolArgumentStreamFinal !ToolCall
    deriving (Eq, Show)

-- | Blocking source for one correlated tool call's argument stream.
type ToolArgumentSource = IO ToolArgumentStreamItem

-- | Validation/consumption deferred until normal approval and scheduling have
-- completed. The authoritative call is supplied again so the interpreter can
-- reject a stale or mismatched prepared value.
type PreparedToolResult =
    ToolCall -> IO (Maybe (Either Text Text))

-- | A scoped streamed-argument interpreter. The function owns all
-- tool-specific incremental parsing and speculative optimization. It returns
-- a prepared result action after receiving 'ToolArgumentStreamFinal'.
type ToolArgumentInterpreter =
    ToolCall
    -> ToolArgumentSource
    -> IO PreparedToolResult

-- | Session-scoped acquisition of an interpreter function. For example,
-- @read_file@ acquires its shared workspace filename index here.
type ToolArgumentInterpreterFactory =
    Acquire ToolArgumentInterpreter

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
    }

data ToolHandler
    = forall args. FromJSON args => TypedTool Text (args -> IO (Either Text Text))
    | forall args. FromJSON args => TypedToolWithCall Text (ToolCall -> args -> IO (Either Text Text))
    | forall args. FromJSON args => TypedStreamingTool Text ((Text -> IO ()) -> args -> IO (Either Text Text))
    | NoArgsTool Text (IO (Either Text Text))

typedTool :: FromJSON args => Text -> (args -> IO (Either Text Text)) -> ToolHandler
typedTool = TypedTool

typedToolWithCall :: FromJSON args => Text -> (ToolCall -> args -> IO (Either Text Text)) -> ToolHandler
typedToolWithCall = TypedToolWithCall

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
        input =
            canonicalToolArguments call.name
                (toolArgumentsValue call.arguments)
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
    | grokName /= name = grokName
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

-- | Project current public Grok Build parameter names back onto the stable
-- internal handler contract. Keep this beside 'canonicalToolName' so every
-- dispatch path applies the same compatibility mapping.
canonicalToolArguments :: Text -> Value -> Value
canonicalToolArguments name value
    | canonicalToolName name == "task" =
        renameObjectKey "background" "run_in_background" value
    | otherwise = value

renameObjectKey :: Text -> Text -> Value -> Value
renameObjectKey source target (Object object)
    | KeyMap.member targetKey object = Object object
    | Just field <- KeyMap.lookup sourceKey object =
        Object $
            KeyMap.insert targetKey field
                (KeyMap.delete sourceKey object)
    | otherwise = Object object
  where
    sourceKey = Key.fromText source
    targetKey = Key.fromText target
renameObjectKey _ _ value = value

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
    TypedStreamingTool name _ -> name
    NoArgsTool name _ -> name

runHandler
    :: (Text -> IO ())
    -> ToolCall
    -> Value
    -> ToolHandler
    -> IO (Either Text Text)
runHandler emitOutput call value = \case
    TypedTool _ run -> decodeAndRun value run
    TypedToolWithCall _ run -> decodeAndRun value (run call)
    TypedStreamingTool _ run -> decodeAndRun value (run emitOutput)
    NoArgsTool _ run ->
        run

decodeAndRun
    :: FromJSON args
    => Value
    -> (args -> IO (Either Text Text))
    -> IO (Either Text Text)
decodeAndRun value run =
    either (pure . Left) run (decodeToolArguments value)
