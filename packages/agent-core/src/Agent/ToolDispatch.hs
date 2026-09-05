module Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallKind(..)
    , isComputerToolCallKind
    , ToolCallResult(..)
    , ToolDispatchOutcome(..)
    , ToolResultImage(..)
    , ToolHandlerResult(..)
    , ToolOutcome(..)
    , toolCallResultOutcome
    , withToolCallOutcome
    , toolCallResultImages
    , ToolDispatchConfig(..)
    , ToolHandler
    , typedTool
    , typedToolWithCall
    , typedRichToolWithCall
    , typedStreamingTool
    , typedStreamingRichTool
    , textTool
    , streamingTextTool
    , streamingRichTextTool
    , passthroughTool
    , noArgsTool
    , functionToolCall
    , customToolCall
    , canonicalToolName
    , canonicalToolArguments
    , dispatchToolCall
    , dispatchToolCallDetailed
    , dispatchToolHandler
    , dispatchToolHandlerDetailed
    , handlerName
    , toolArgumentsValue
    , decodeToolArguments
    ) where

import Agent.ToolOutcome (ToolOutcome(..), toolOutcomeSucceeded)
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
    -- | A legacy provider-native computer call retained so persisted sessions
    -- can still be resumed.
    | ComputerCallKind
    -- | The reserved ordinary computer function. It uses the local executor
    -- and explicit approval rules; Responses continuations pair its text
    -- @function_call_output@ with a fresh user screenshot.
    | ComputerFunctionCallKind
    deriving (Eq, Show)

isComputerToolCallKind :: ToolCallKind -> Bool
isComputerToolCallKind = \case
    ComputerCallKind -> True
    ComputerFunctionCallKind -> True
    _ -> False

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

-- | An image returned alongside a tool's short textual output. Keeping image
-- data out of 'output' prevents large data URLs from being truncated, logged,
-- or fed back to the model as ordinary text.
data ToolResultImage = ToolResultImage
    { imageUrl :: !Text
    , imageDetail :: !(Maybe Text)
    } deriving (Eq)

instance Show ToolResultImage where
    show image =
        "ToolResultImage { imageUrl = <redacted>, imageDetail = "
            <> show image.imageDetail
            <> " }"

-- | Rich result produced by the small number of tools that return media.
-- Ordinary tool constructors continue to accept @Either Text Text@.
data ToolHandlerResult = ToolHandlerResult
    { resultText :: !Text
    , resultImages :: ![ToolResultImage]
    }
    | ToolHandlerResultWithOutcome
        { resultText :: !Text
        , resultImages :: ![ToolResultImage]
        , resultOutcome :: !ToolOutcome
        }
    deriving (Eq, Show)

-- | A dispatched result together with its protocol-neutral success bit.
--
-- Provider adapters such as an in-process MCP server must not infer failure
-- from the rendered output: callers are free to customize that rendering, and
-- successful tools may legitimately return text beginning with @"Error:"@.
data ToolDispatchOutcome = ToolDispatchOutcome
    { toolDispatchResult :: !ToolCallResult
    , toolDispatchSucceeded :: !Bool
    } deriving (Eq, Show)

-- | Provider-neutral result ready for a transport adapter to encode.
--
-- Native dispatch always supplies an outcome. The two legacy constructors
-- represent historical/imported results whose execution facts are unknown.
-- Consumers use the total image/outcome accessors across all three forms.
data ToolCallResult
    = ToolCallResult
        { callId :: !Text
        , output :: !Text
        , callKind :: !ToolCallKind
        }
    | ToolCallResultWithImages
        { callId :: !Text
        , output :: !Text
        , callKind :: !ToolCallKind
        , toolResultImages :: ![ToolResultImage]
        }
    | ToolCallResultWithOutcome
        { callId :: !Text
        , output :: !Text
        , callKind :: !ToolCallKind
        , toolResultImages :: ![ToolResultImage]
        , toolResultOutcome :: !ToolOutcome
        }
    deriving (Eq)

instance Show ToolCallResult where
    show result =
        "ToolCallResult { callId = " <> show result.callId
            <> ", output = " <> show result.output
            <> ", callKind = " <> show result.callKind
            <> ", outcome = " <> show (toolCallResultOutcome result)
            <> imageSummary
            <> " }"
      where
        imageSummary = case toolCallResultImages result of
            [] -> ""
            images -> ", images = <" <> show (length images) <> ">"

toolCallResultImages :: ToolCallResult -> [ToolResultImage]
toolCallResultImages = \case
    ToolCallResult{} -> []
    ToolCallResultWithImages{toolResultImages} -> toolResultImages
    ToolCallResultWithOutcome{toolResultImages} -> toolResultImages

toolCallResultOutcome :: ToolCallResult -> Maybe ToolOutcome
toolCallResultOutcome = \case
    ToolCallResultWithOutcome{toolResultOutcome} -> Just toolResultOutcome
    _ -> Nothing

-- | Attach execution facts at a native or durable-storage boundary. Legacy
-- imports without facts retain their compatibility representation.
withToolCallOutcome :: Maybe ToolOutcome -> ToolCallResult -> ToolCallResult
withToolCallOutcome Nothing result = result
withToolCallOutcome (Just outcome) result =
    ToolCallResultWithOutcome result.callId result.output result.callKind
        (toolCallResultImages result) outcome

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

-- | Every handler has the same execution interface. Smart constructors capture
-- argument decoding and result adaptation, leaving dispatch independent of the
-- input format, streaming support, and output richness.
--
-- The original call remains separate from the canonical argument text so
-- passthrough brokers can forward it without rewriting its payload.
data ToolHandler = ToolHandler
    { toolHandlerName :: !Text
    , toolHandlerRun
        :: (Text -> IO ())
        -> ToolCall
        -> Text
        -> IO (Either Text ToolHandlerResult)
    }

typedTool :: Text -> Decoder args -> (args -> IO (Either Text Text)) -> ToolHandler
typedTool name decoder run =
    typedToolWithCall name decoder (\_call -> run)

typedToolWithCall :: Text -> Decoder args -> (ToolCall -> args -> IO (Either Text Text)) -> ToolHandler
typedToolWithCall name decoder run =
    typedRichToolWithCall name decoder \call args ->
        plainResult <$> run call args

typedRichToolWithCall
    :: Text
    -> Decoder args
    -> (ToolCall -> args -> IO (Either Text ToolHandlerResult))
    -> ToolHandler
typedRichToolWithCall name decoder run =
    ToolHandler name \_emit call value ->
        decodeAndRun decoder value (run call)

-- | A typed tool that can publish accumulated output snapshots while running.
-- The final result remains authoritative.
typedStreamingTool
    :: Text
    -> Decoder args
    -> ((Text -> IO ()) -> args -> IO (Either Text Text))
    -> ToolHandler
typedStreamingTool name decoder run =
    typedStreamingRichTool name decoder \emit args ->
        plainResult <$> run emit args

typedStreamingRichTool
    :: Text
    -> Decoder args
    -> ((Text -> IO ()) -> args -> IO (Either Text ToolHandlerResult))
    -> ToolHandler
typedStreamingRichTool name decoder run =
    ToolHandler name \emit _call value ->
        decodeAndRun decoder value (run emit)

-- | A freeform tool whose input is plain text rather than JSON.
textTool
    :: Text
    -> (Text -> IO (Either Text Text))
    -> ToolHandler
textTool name run =
    streamingTextTool name (\_emit -> run)

-- | A streaming freeform tool. Its input is not JSON, so no JSON decoder is
-- involved.
streamingTextTool
    :: Text
    -> ((Text -> IO ()) -> Text -> IO (Either Text Text))
    -> ToolHandler
streamingTextTool name run =
    streamingRichTextTool name \emit value ->
        plainResult <$> run emit value

streamingRichTextTool
    :: Text
    -> ((Text -> IO ()) -> Text -> IO (Either Text ToolHandlerResult))
    -> ToolHandler
streamingRichTextTool name run =
    ToolHandler name \emit _call value -> run emit value

passthroughTool
    :: Text
    -> ((Text -> IO ()) -> ToolCall -> IO (Either Text ToolHandlerResult))
    -> ToolHandler
passthroughTool name run =
    ToolHandler name \emit call _value -> run emit call

noArgsTool :: Text -> IO (Either Text Text) -> ToolHandler
noArgsTool name run = textTool name (\_value -> run)

plainResult :: Either Text Text -> Either Text ToolHandlerResult
plainResult = fmap \text -> ToolHandlerResult text []

dispatchToolCall :: ToolDispatchConfig -> [ToolHandler] -> ToolCall -> IO ToolCallResult
dispatchToolCall config handlers call =
    (.toolDispatchResult) <$>
        dispatchToolCallDetailed config handlers call

dispatchToolCallDetailed
    :: ToolDispatchConfig
    -> [ToolHandler]
    -> ToolCall
    -> IO ToolDispatchOutcome
dispatchToolCallDetailed config handlers call =
    dispatchToolHandlerDetailed config (findHandler call.name handlers) call

-- | Dispatch with an already-resolved handler. Registries should prefer this
-- entry point so canonical-name lookup and uniqueness checks happen once.
dispatchToolHandler
    :: ToolDispatchConfig
    -> Maybe ToolHandler
    -> ToolCall
    -> IO ToolCallResult
dispatchToolHandler config maybeHandler call = do
    (.toolDispatchResult) <$>
        dispatchToolHandlerDetailed config maybeHandler call

-- | Detailed dispatch entry point for protocol adapters which need to preserve
-- the handler's success/failure distinction independently of output
-- formatting.
dispatchToolHandlerDetailed
    :: ToolDispatchConfig
    -> Maybe ToolHandler
    -> ToolCall
    -> IO ToolDispatchOutcome
dispatchToolHandlerDetailed config maybeHandler call = do
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
    (resultOutput, resultImages) <- case result of
        Right (Right toolResult) ->
            pure
                ( config.toolDispatchFormatResult (Right toolResult.resultText)
                , toolResult.resultImages
                )
        Right (Left err) ->
            pure (config.toolDispatchFormatResult (Left err), [])
        Left exception -> do
            -- Diagnostics must not replace the original tool failure with a
            -- second exception. 'tryAny' still lets asynchronous cancellation
            -- propagate.
            _ <- tryAny (config.toolDispatchOnException callName exception)
            pure (config.toolDispatchFormatException callName exception, [])
    finalizedOutput <-
        tryAny (config.toolDispatchFinalizeOutput call resultOutput) >>= \case
            Right output -> pure output
            Left exception -> do
                _ <- tryAny (config.toolDispatchOnException callName exception)
                pure resultOutput
    let outcome = case result of
            Right (Right ToolHandlerResult{}) -> ToolSucceeded
            Right (Right ToolHandlerResultWithOutcome{resultOutcome}) -> resultOutcome
            Right (Left _) -> ToolFailed
            Left _ -> ToolFailed
        dispatchedResult = ToolCallResultWithOutcome
            call.callId finalizedOutput call.callKind resultImages outcome
    pure ToolDispatchOutcome
        { toolDispatchResult = dispatchedResult
        , toolDispatchSucceeded = toolOutcomeSucceeded outcome
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
    | name == "image_gen__imagegen" = "imagegen"
    | Just rest <- Text.stripPrefix "image_gen." name
    , rest == "imagegen" = "imagegen"
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
handlerName handler = handler.toolHandlerName

runHandler
    :: (Text -> IO ())
    -> ToolCall
    -> Text
    -> ToolHandler
    -> IO (Either Text ToolHandlerResult)
runHandler emitOutput call value handler =
    handler.toolHandlerRun emitOutput call value

decodeAndRun
    :: Decoder args
    -> Text
    -> (args -> IO (Either Text result))
    -> IO (Either Text result)
decodeAndRun decoder value run =
    either (pure . Left) run (decodeToolArguments decoder value)
