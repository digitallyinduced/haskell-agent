{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}

module Agent.ToolDispatch
    ( ToolCall(ToolCall, callId, name, arguments, callKind, argumentsEncrypted)
    , ToolCallMode(..)
    , callMode
    , toolCallMode
    , withToolCallMode
    , setToolCallArguments
    , ToolCallKind(..)
    , isComputerToolCallKind
    , ToolCallResult(..)
    , ToolDispatchOutcome(..)
    , ToolResultImage(..)
    , ToolHandlerResult(..)
    , toolCallResultImages
    , toolCallResultMode
    , withToolCallResultMode
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
import GHC.Records (HasField(..))

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

-- | Whether the provider permits the application to complete this call after
-- the originating model turn has continued.
data ToolCallMode
    = BlockingToolCall
    | AsyncToolCall
    deriving (Eq, Show)

isComputerToolCallKind :: ToolCallKind -> Bool
isComputerToolCallKind = \case
    ComputerCallKind -> True
    ComputerFunctionCallKind -> True
    _ -> False

-- | Provider-neutral function or custom tool call emitted by a model transport.
data ToolCall =
    ToolCallInternal
        !Text
        !Text
        !Text
        !ToolCallKind
        !Bool
        !ToolCallMode
    deriving (Eq)

-- | Source-compatible blocking-call constructor. The execution mode is kept
-- outside the historical five-field surface so existing positional and record
-- construction remains valid.
pattern ToolCall
    :: Text
    -> Text
    -> Text
    -> ToolCallKind
    -> Bool
    -> ToolCall
pattern ToolCall
    { callId
    , name
    , arguments
    , callKind
    , argumentsEncrypted
    } <- ToolCallInternal
        callId
        name
        arguments
        callKind
        argumentsEncrypted
        _
  where
    ToolCall callId name arguments callKind argumentsEncrypted =
        ToolCallInternal
            callId
            name
            arguments
            callKind
            argumentsEncrypted
            BlockingToolCall

{-# COMPLETE ToolCall #-}

instance HasField "callId" ToolCall Text where
    getField (ToolCallInternal value _ _ _ _ _) = value

instance HasField "name" ToolCall Text where
    getField (ToolCallInternal _ value _ _ _ _) = value

instance HasField "arguments" ToolCall Text where
    getField (ToolCallInternal _ _ value _ _ _) = value

instance HasField "callKind" ToolCall ToolCallKind where
    getField (ToolCallInternal _ _ _ value _ _) = value

instance HasField "argumentsEncrypted" ToolCall Bool where
    getField (ToolCallInternal _ _ _ _ value _) = value

toolCallMode :: ToolCall -> ToolCallMode
toolCallMode
    (ToolCallInternal _ _ _ _ _ mode) =
        mode

-- | Execution-mode accessor named after the logical 'ToolCall' field.
-- 'toolCallMode' remains available where a less ambiguous name is useful.
callMode :: ToolCall -> ToolCallMode
callMode = toolCallMode

withToolCallMode :: ToolCallMode -> ToolCall -> ToolCall
withToolCallMode mode
    (ToolCallInternal callId name arguments callKind argumentsEncrypted _) =
        ToolCallInternal
            callId
            name
            arguments
            callKind
            argumentsEncrypted
            mode

-- | Replace streamed arguments without accidentally resetting an async call
-- to the compatibility constructor's blocking default.
setToolCallArguments :: Text -> ToolCall -> ToolCall
setToolCallArguments newArguments
    (ToolCallInternal callId name _ callKind argumentsEncrypted mode) =
        ToolCallInternal
            callId
            name
            newArguments
            callKind
            argumentsEncrypted
            mode

instance Show ToolCall where
    show call =
        "ToolCall { callId = " <> show call.callId
            <> ", name = " <> show call.name
            <> ", arguments = " <> shownArguments
            <> ", callKind = " <> show call.callKind
            <> ", argumentsEncrypted = " <> show call.argumentsEncrypted
            <> ", callMode = " <> show (toolCallMode call)
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
    } deriving (Eq, Show)

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
-- The original constructor stays unchanged for ordinary text tools. The rich
-- constructor avoids a source-compatible API break for callers that build or
-- pattern-match three-field results.
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
    | AsyncToolCallResult
        { callId :: !Text
        , output :: !Text
        , callKind :: !ToolCallKind
        }
    | AsyncToolCallResultWithImages
        { callId :: !Text
        , output :: !Text
        , callKind :: !ToolCallKind
        , toolResultImages :: ![ToolResultImage]
        }
    deriving (Eq)

instance Show ToolCallResult where
    show result =
        "ToolCallResult { callId = " <> show result.callId
            <> ", output = " <> show result.output
            <> ", callKind = " <> show result.callKind
            <> ", callMode = " <> show (toolCallResultMode result)
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
    AsyncToolCallResult{} -> []
    AsyncToolCallResultWithImages{toolResultImages} -> toolResultImages

toolCallResultMode :: ToolCallResult -> ToolCallMode
toolCallResultMode = \case
    ToolCallResult{} -> BlockingToolCall
    ToolCallResultWithImages{} -> BlockingToolCall
    AsyncToolCallResult{} -> AsyncToolCall
    AsyncToolCallResultWithImages{} -> AsyncToolCall

-- | Retag a result while preserving its payload and any attached images.
withToolCallResultMode :: ToolCallMode -> ToolCallResult -> ToolCallResult
withToolCallResultMode mode result =
    case (mode, toolCallResultImages result) of
        (BlockingToolCall, []) ->
            ToolCallResult result.callId result.output result.callKind
        (BlockingToolCall, images) ->
            ToolCallResultWithImages
                result.callId result.output result.callKind images
        (AsyncToolCall, []) ->
            AsyncToolCallResult result.callId result.output result.callKind
        (AsyncToolCall, images) ->
            AsyncToolCallResultWithImages
                result.callId result.output result.callKind images

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
    let dispatchedResult = case (toolCallMode call, resultImages) of
            (BlockingToolCall, []) ->
                ToolCallResult
                    call.callId
                    finalizedOutput
                    call.callKind
            (BlockingToolCall, images) ->
                ToolCallResultWithImages
                    call.callId
                    finalizedOutput
                    call.callKind
                    images
            (AsyncToolCall, []) ->
                AsyncToolCallResult
                    call.callId
                    finalizedOutput
                    call.callKind
            (AsyncToolCall, images) ->
                AsyncToolCallResultWithImages
                    call.callId
                    finalizedOutput
                    call.callKind
                    images
        succeeded = case result of
            Right (Right _) -> True
            Right (Left _) -> False
            Left _ -> False
    pure ToolDispatchOutcome
        { toolDispatchResult = dispatchedResult
        , toolDispatchSucceeded = succeeded
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
