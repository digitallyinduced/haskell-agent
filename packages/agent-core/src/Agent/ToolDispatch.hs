{-# LANGUAGE ExistentialQuantification #-}

module Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallKind(..)
    , isComputerToolCallKind
    , ToolCallResult(..)
    , ToolDispatchOutcome(..)
    , ToolResultImage(..)
    , ToolHandlerResult(..)
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

-- | How the originating model turn encoded this call. Adapters need this to
-- emit @function_call_output@ versus @custom_tool_call_output@.
data ToolCallKind
    = FunctionCallKind
    | CustomCallKind
    -- | A provider-native computer call. Its arguments are the encoded
    -- action list, and its result is encoded by the provider adapter as a
    -- structured screenshot output rather than a function output string.
    | ComputerCallKind
    -- | The reserved Responses Lite computer function. It uses the same local
    -- executor and approval rules as a native computer call, but its result
    -- is encoded as a multimodal function output.
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
    deriving (Eq)

instance Show ToolCallResult where
    show result =
        "ToolCallResult { callId = " <> show result.callId
            <> ", output = " <> show result.output
            <> ", callKind = " <> show result.callKind
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
    | forall args. TypedRichToolWithCall Text (Decoder args) (ToolCall -> args -> IO (Either Text ToolHandlerResult))
    | forall args. TypedStreamingTool Text (Decoder args) ((Text -> IO ()) -> args -> IO (Either Text Text))
    | forall args. TypedStreamingRichTool Text (Decoder args) ((Text -> IO ()) -> args -> IO (Either Text ToolHandlerResult))
    | TextTool Text (Text -> IO (Either Text Text))
    | StreamingTextTool Text ((Text -> IO ()) -> Text -> IO (Either Text Text))
    | StreamingRichTextTool Text ((Text -> IO ()) -> Text -> IO (Either Text ToolHandlerResult))
    | NoArgsTool Text (IO (Either Text Text))

typedTool :: Text -> Decoder args -> (args -> IO (Either Text Text)) -> ToolHandler
typedTool = TypedTool

typedToolWithCall :: Text -> Decoder args -> (ToolCall -> args -> IO (Either Text Text)) -> ToolHandler
typedToolWithCall = TypedToolWithCall

typedRichToolWithCall
    :: Text
    -> Decoder args
    -> (ToolCall -> args -> IO (Either Text ToolHandlerResult))
    -> ToolHandler
typedRichToolWithCall = TypedRichToolWithCall

-- | A typed tool that can publish accumulated output snapshots while running.
-- The final result remains authoritative.
typedStreamingTool
    :: Text
    -> Decoder args
    -> ((Text -> IO ()) -> args -> IO (Either Text Text))
    -> ToolHandler
typedStreamingTool = TypedStreamingTool

typedStreamingRichTool
    :: Text
    -> Decoder args
    -> ((Text -> IO ()) -> args -> IO (Either Text ToolHandlerResult))
    -> ToolHandler
typedStreamingRichTool = TypedStreamingRichTool

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

streamingRichTextTool
    :: Text
    -> ((Text -> IO ()) -> Text -> IO (Either Text ToolHandlerResult))
    -> ToolHandler
streamingRichTextTool = StreamingRichTextTool

noArgsTool :: Text -> IO (Either Text Text) -> ToolHandler
noArgsTool = NoArgsTool

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
    let dispatchedResult
            | null resultImages =
                ToolCallResult
                    call.callId
                    finalizedOutput
                    call.callKind
            | otherwise =
                ToolCallResultWithImages
                    call.callId
                    finalizedOutput
                    call.callKind
                    resultImages
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
handlerName = \case
    TypedTool name _ _ -> name
    TypedToolWithCall name _ _ -> name
    TypedRichToolWithCall name _ _ -> name
    TypedStreamingTool name _ _ -> name
    TypedStreamingRichTool name _ _ -> name
    TextTool name _ -> name
    StreamingTextTool name _ -> name
    StreamingRichTextTool name _ -> name
    NoArgsTool name _ -> name

runHandler
    :: (Text -> IO ())
    -> ToolCall
    -> Text
    -> ToolHandler
    -> IO (Either Text ToolHandlerResult)
runHandler emitOutput call value = \case
    TypedTool _ decoder run ->
        plainResult <$> decodeAndRun decoder value run
    TypedToolWithCall _ decoder run ->
        plainResult <$> decodeAndRun decoder value (run call)
    TypedRichToolWithCall _ decoder run ->
        decodeAndRun decoder value (run call)
    TypedStreamingTool _ decoder run ->
        plainResult <$> decodeAndRun decoder value (run emitOutput)
    TypedStreamingRichTool _ decoder run ->
        decodeAndRun decoder value (run emitOutput)
    TextTool _ run -> plainResult <$> run value
    StreamingTextTool _ run -> plainResult <$> run emitOutput value
    StreamingRichTextTool _ run -> run emitOutput value
    NoArgsTool _ run ->
        plainResult <$> run
  where
    plainResult = fmap \text -> ToolHandlerResult text []

decodeAndRun
    :: Decoder args
    -> Text
    -> (args -> IO (Either Text result))
    -> IO (Either Text result)
decodeAndRun decoder value run =
    either (pure . Left) run (decodeToolArguments decoder value)
