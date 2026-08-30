-- | Model-facing @exec@/@wait@ tools backed by the isolated Node worker.
--
-- Wire names, descriptions, grammar, and output formatting mirror the Codex
-- CLI code-mode implementation so a catalog model running @code_mode_only@
-- sees the tool surface it was trained with. Nested effects are delegated
-- through a caller-supplied invoker, so direct and code-mode calls share the
-- same registry, authorization, and safety checks.
module Agent.Tools.CodeMode.Tool
    ( CodeModeToolSet(..)
    , CodeModeNestedSpec(..)
    , CodeModeNamespace(..)
    , CodeModeNestedInvoke
    , ToolMode(..)
    , ExecPragma(..)
    , codeModeExecGrammar
    , defaultExecYieldTimeMs
    , newCodeModeToolSet
    , normalizeCodeModeIdentifier
    , parseExecSource
    , renderJsonSchemaType
    ) where

import Agent.ToolArgs
    ( objectArgsExact
    , optBoolStrict
    , optInt
    , reqText
    )
import Agent.Json.Decode (Decoder)
import qualified Agent.Json.Decode as Json
import Agent.ToolDSL
    ( PropertySchema(..)
    , PropertyType(..)
    , parametersObjectLoose
    )
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallKind(..)
    , ToolCallResult(..)
    , ToolHandlerResult(..)
    , ToolResultImage(..)
    , streamingRichTextTool
    , toolCallResultImages
    , typedStreamingRichTool
    )
import Agent.Tools.CodeMode.Host
    ( CodeModeConfig(..)
    , CodeModeError(..)
    , CodeModeHost
    , CodeModeResult(..)
    , ImageDetailVisibility(..)
    , checkCodeModeAvailability
    , closeCodeModeHost
    , defaultCodeModeConfig
    , execCodeCellWithTools
    , newCodeModeHost
    , terminateCodeCell
    , waitCodeCell
    )
import Agent.Tools.CodeMode.Protocol (CodeModeToolMetadata(..))
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRule(..)
    , ToolExecutionPolicy(..)
    , ToolSchema(..)
    , freeformGrammarAppToolWithExecution
    , jsonAppToolWithExecution
    )
import Control.Applicative ((<|>))
import Control.Exception.Safe (finally)
import Control.Monad (foldM)
import Data.Aeson (Value(..), encode)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import qualified Data.Vector as Vector
import Text.Printf (printf)
import Text.Read (readMaybe)

-- | Codex model-catalog tool-surface selector.
data ToolMode
    = ConventionalToolMode
    | CodeToolMode
    | CodeOnlyToolMode
    deriving (Eq, Show)

-- | Run one nested tool call through the host application's registry,
-- authorization, and dispatch. 'Left' rejects the nested JavaScript promise
-- with the given message; 'Right' resolves it with the tool output and any
-- supplemental media.
type CodeModeNestedInvoke = ToolCall -> IO (Either Text ToolCallResult)

-- | A namespaced nested-tool group, mirroring provider tool namespaces such
-- as @collaboration@.
data CodeModeNamespace = CodeModeNamespace
    { namespaceName :: !Text
    , namespaceDescription :: !Text
    } deriving (Eq, Show)

-- | One direct tool projected into the code-mode JavaScript surface.
data CodeModeNestedSpec = CodeModeNestedSpec
    { nestedSpecTool :: !AppTool
    , nestedSpecNamespace :: !(Maybe CodeModeNamespace)
    }

data CodeModeToolSet = CodeModeToolSet
    { codeModeTools :: ![AppTool]
      -- ^ The @exec@ and @wait@ tools, ready for registry and wire schemas.
    , codeModeNestedToolNames :: ![Text]
    , closeCodeModeToolSet :: !(IO ())
    }

data NestedTool = NestedTool
    { nestedRuntimeName :: !Text
    , nestedCallKind :: !ToolCallKind
    , nestedDescription :: !Text
    , nestedNamespace :: !(Maybe CodeModeNamespace)
    , nestedParameters :: !(Maybe Value)
      -- ^ JSON Schema for function tools; 'Nothing' for freeform tools.
    }

-- | Build the code-mode tool set for one session.
--
-- Starts a probe worker before returning so code-only sessions cannot
-- advertise @exec@ when the installed Node binary rejects our permission
-- flags, worker module, or readiness protocol.
newCodeModeToolSet
    :: ToolMode
    -> ImageDetailVisibility
    -> FilePath
    -> CodeModeNestedInvoke
    -> [CodeModeNestedSpec]
    -> IO (Either Text CodeModeToolSet)
newCodeModeToolSet mode detailVisibility workerPath invoke specs =
    case buildNestedTools specs of
        Left err -> pure (Left err)
        Right nested -> do
            nextInvocation <- newIORef (0 :: Int)
            let config =
                    (defaultCodeModeConfig workerPath
                        (runNestedTool invoke nextInvocation nested))
                        { imageDetailVisibility = detailVisibility }
            checkCodeModeAvailability config >>= \case
                Left err -> pure (Left err)
                Right () ->
                    probeCodeModeConfig config >>= \case
                        Left err -> pure (Left err)
                        Right () -> do
                            host <- newCodeModeHost config
                            let metadata =
                                    [ CodeModeToolMetadata
                                        { toolMetadataName = name
                                        , toolMetadataDescription =
                                            definition.nestedDescription
                                        }
                                    | (name, definition) <-
                                        Map.toAscList nested
                                    ]
                                names = map (.toolMetadataName) metadata
                                description =
                                    execDescription
                                        (mode == CodeOnlyToolMode)
                                        detailVisibility
                                        (Map.toAscList nested)
                                tools =
                                    [ execTool host metadata description
                                    , waitTool host
                                    ]
                            pure $ Right CodeModeToolSet
                                { codeModeTools = tools
                                , codeModeNestedToolNames = names
                                , closeCodeModeToolSet =
                                    closeCodeModeHost host
                                }

probeCodeModeConfig :: CodeModeConfig -> IO (Either Text ())
probeCodeModeConfig config = do
    host <- newCodeModeHost config
    result <-
        execCodeCellWithTools
            host
            ""
            []
            config.startupTimeoutMs
        `finally` closeCodeModeHost host
    pure $ case result of
        Right CodeModeFinished{} -> Right ()
        Right unexpected ->
            Left $
                "code-mode worker readiness probe did not complete: "
                    <> Text.pack (show unexpected)
        Left err ->
            Left $
                "code-mode worker readiness probe failed: "
                    <> renderCodeModeError err

buildNestedTools
    :: [CodeModeNestedSpec]
    -> Either Text (Map Text NestedTool)
buildNestedTools =
    foldM add Map.empty
  where
    add current spec
        | codeName `elem` ["exec", "wait"] = Right current
        | Map.member codeName current =
            -- Match Codex's stable first-wins projection when two provider
            -- names normalize to the same JS identifier.
            Right current
        | otherwise =
            Right $ Map.insert codeName
                NestedTool
                    { nestedRuntimeName = tool.appToolName
                    , nestedCallKind = case tool.appToolSchema of
                        FreeformApplyPatchSchema -> CustomCallKind
                        FreeformGrammarSchema _ _ -> CustomCallKind
                        JsonFunctionSchema _ -> FunctionCallKind
                        RawJsonFunctionSchema _ -> FunctionCallKind
                    , nestedDescription = tool.appToolDescription
                    , nestedNamespace = spec.nestedSpecNamespace
                    , nestedParameters = case tool.appToolSchema of
                        JsonFunctionSchema properties ->
                            Just (parametersObjectLoose properties)
                        RawJsonFunctionSchema value -> Just value
                        FreeformApplyPatchSchema -> Nothing
                        FreeformGrammarSchema _ _ -> Nothing
                    }
                current
      where
        tool = spec.nestedSpecTool
        codeName =
            normalizeCodeModeIdentifier (rawCodeModeName spec)

-- | Provider tool key before identifier normalization. Namespaced tools use
-- Codex's @namespace__name@ flattening.
rawCodeModeName :: CodeModeNestedSpec -> Text
rawCodeModeName spec = case spec.nestedSpecNamespace of
    Nothing -> name
    Just namespace
        | "_" `Text.isSuffixOf` namespace.namespaceName
            || "_" `Text.isPrefixOf` name ->
            namespace.namespaceName <> name
        | otherwise -> namespace.namespaceName <> "__" <> name
  where
    name = spec.nestedSpecTool.appToolName

-- | Normalize a provider tool key into a JavaScript identifier, matching the
-- Codex code-mode projection. Namespace separators are already encoded as
-- double underscores before this step.
normalizeCodeModeIdentifier :: Text -> Text
normalizeCodeModeIdentifier raw
    | Text.null normalized = "_"
    | otherwise = normalized
  where
    normalized = Text.pack
        [ if validAt index char then char else '_'
        | (index, char) <- zip [0 :: Int ..] (Text.unpack raw)
        ]
    validAt 0 char =
        char == '_' || char == '$' || isAsciiLetter char
    validAt _ char =
        char == '_' || char == '$'
            || isAsciiLetter char || isAsciiDigit char
    isAsciiLetter char =
        ('a' <= char && char <= 'z')
            || ('A' <= char && char <= 'Z')
    isAsciiDigit char = '0' <= char && char <= '9'

runNestedTool
    :: CodeModeNestedInvoke
    -> IORef Int
    -> Map Text NestedTool
    -> Text
    -> Value
    -> IO (Either Text Value)
runNestedTool invoke nextInvocation nested codeName arguments =
    case Map.lookup codeName nested of
        Nothing -> pure (Left
            ("tool is not available in this cell: " <> codeName))
        Just tool -> either (pure . Left) (invokeTool tool)
            (nestedToolArguments tool.nestedCallKind arguments)
  where
    invokeTool :: NestedTool -> Text -> IO (Either Text Value)
    invokeTool tool callArguments = do
        invocation <- atomicModifyIORef' nextInvocation
            \current ->
                let next = current + 1
                in (next, next)
        result <- invoke ToolCall
            { callId =
                "code-mode:"
                    <> Text.pack (show invocation)
                    <> ":"
                    <> codeName
            , name = tool.nestedRuntimeName
            , arguments = callArguments
            , callKind = tool.nestedCallKind
            , argumentsEncrypted = False
            }
        pure (nestedResultValue <$> result)

nestedResultValue :: ToolCallResult -> Value
nestedResultValue result =
    case toolCallResultImages result of
        [] -> String result.output
        image : _ ->
            Object . KeyMap.fromList $
                [ ("image_url", String image.imageUrl) ]
                    <> [ ("output_hint", String result.output)
                       | not (Text.null (Text.strip result.output))
                       ]

nestedToolArguments :: ToolCallKind -> Value -> Either Text Text
nestedToolArguments CustomCallKind = \case
    String input -> Right input
    _ -> Left "freeform tool calls require a string input"
nestedToolArguments _ =
    Right . TextEncoding.decodeUtf8 . LBS.toStrict . encode

newtype ExecArgs = ExecArgs { source :: Text }

data ExecPragma = ExecPragma
    { yieldTimeMs :: Maybe Int
    , maxOutputTokens :: Maybe Int
    } deriving (Eq, Show)

execPragmaDecoder :: Decoder ExecPragma
execPragmaDecoder = objectArgsExact
        ["yield_time_ms", "max_output_tokens"]
        \object_ -> ExecPragma
            <$> optInt object_ "yield_time_ms"
            <*> optInt object_ "max_output_tokens"

-- | Codex ships @code_mode.default_exec_yield_time_ms = 30000@; the exec tool
-- description advertises this value.
defaultExecYieldTimeMs :: Int
defaultExecYieldTimeMs = 30000

defaultWaitYieldTimeMs :: Int
defaultWaitYieldTimeMs = 10000

execTool :: CodeModeHost -> [CodeModeToolMetadata] -> Text -> AppTool
execTool host nestedTools description =
    freeformGrammarAppToolWithExecution
        "exec"
        description
        "lark"
        codeModeExecGrammar
        AlwaysReadOnly
        -- The JavaScript source is opaque to the outer scheduler and may
        -- invoke any projected tool, including mutating tools. Treat the
        -- wrapper as an exclusive call; concurrency requested inside the
        -- cell remains explicit in the JavaScript (for example Promise.all).
        TurnSequential
        (streamingRichTextTool "exec" \_emit source ->
            runExec host nestedTools (ExecArgs source))

runExec
    :: CodeModeHost
    -> [CodeModeToolMetadata]
    -> ExecArgs
    -> IO (Either Text ToolHandlerResult)
runExec host nestedTools args =
    case parseExecSource args.source of
        Left err -> pure (Left err)
        Right (source, pragma)
            | maybe False (< 0) pragma.yieldTimeMs ->
                pure (Left
                    "exec pragma field `yield_time_ms` must be a non-negative safe integer")
            | maybe False (not . isJavaScriptSafeInteger)
                    pragma.yieldTimeMs ->
                pure (Left
                    "exec pragma field `yield_time_ms` must be a non-negative safe integer")
            | maybe False (< 0) pragma.maxOutputTokens ->
                pure (Left
                    "exec pragma field `max_output_tokens` must be a non-negative safe integer")
            | maybe False (not . isJavaScriptSafeInteger)
                    pragma.maxOutputTokens ->
                pure (Left
                    "exec pragma field `max_output_tokens` must be a non-negative safe integer")
            | otherwise -> do
                started <- getCurrentTime
                result <- execCodeCellWithTools
                    host
                    source
                    nestedTools
                    (resolveYieldTimeoutMs
                        (fromMaybe defaultExecYieldTimeMs pragma.yieldTimeMs))
                finished <- getCurrentTime
                pure $
                    withResultImages result
                        <$> renderCodeModeResult
                            pragma.maxOutputTokens
                            (elapsedSeconds started finished)
                            result

-- | Codex grants a one-second grace period on top of yields of ten seconds or
-- more, so a script that finishes just past its yield window still returns a
-- complete result instead of a needless @wait@ round trip.
resolveYieldTimeoutMs :: Int -> Int
resolveYieldTimeoutMs requested
    | requested >= 10000 = requested + 1000
    | otherwise = max 1 requested

parseExecSource :: Text -> Either Text (Text, ExecPragma)
parseExecSource raw
    | Text.null (Text.strip raw) =
        Left
            "exec expects raw JavaScript source text (non-empty). Provide JS only, optionally with first-line `// @exec: {\"yield_time_ms\": 10000, \"max_output_tokens\": 1000}`."
    | otherwise =
        let (first, restWithNewline) = Text.breakOn "\n" raw
            trimmed = Text.stripStart first
        in case Text.stripPrefix "// @exec:" trimmed of
            Nothing -> Right (raw, ExecPragma Nothing Nothing)
            Just directive
                | Text.null restWithNewline
                    || Text.null (Text.strip (Text.drop 1 restWithNewline)) ->
                    Left
                        "exec pragma must be followed by JavaScript source on subsequent lines"
                | Text.null (Text.strip directive) ->
                    Left
                        "exec pragma must be a JSON object with supported fields \
                        \`yield_time_ms` and `max_output_tokens`"
                | otherwise ->
                    case Json.decodeText execPragmaDecoder (Text.strip directive) of
                        Left err -> Left $
                            "exec pragma must be valid JSON with supported fields \
                            \`yield_time_ms` and `max_output_tokens`: "
                                <> err.jsonErrorMessage
                        Right pragma
                            | maybe False (< 0) pragma.yieldTimeMs
                                || maybe False (< 0) pragma.maxOutputTokens ->
                                Left
                                    "exec pragma fields `yield_time_ms` and \
                                    \`max_output_tokens` must be non-negative safe integers"
                            | otherwise ->
                                Right (Text.drop 1 restWithNewline, pragma)

data WaitArgs = WaitArgs
    { cellId :: Text
    , yieldTimeMs :: Maybe Int
    , maxTokens :: Maybe Int
    , terminate :: Bool
    }

waitArgsDecoder :: Decoder WaitArgs
waitArgsDecoder = objectArgsExact
        ["cell_id", "yield_time_ms", "max_tokens", "terminate"]
        \object_ -> WaitArgs
            <$> reqText object_ "cell_id"
            <*> optInt object_ "yield_time_ms"
            <*> optInt object_ "max_tokens"
            <*> (fromMaybe False <$> optBoolStrict object_ "terminate")

waitTool :: CodeModeHost -> AppTool
waitTool host =
    jsonAppToolWithExecution
        "wait"
        waitDescription
        [ PropertySchema "cell_id" PropertyString True $ Just
            "Identifier of the running exec cell."
        , PropertySchema "max_tokens" PropertyNumber False $ Just
            "Output token budget for this wait call. Defaults to 10000 tokens."
        , PropertySchema "terminate" PropertyBoolean False $ Just
            "True stops the running exec cell; false or omitted waits for output."
        , PropertySchema "yield_time_ms" PropertyNumber False $ Just
            "Wait before yielding more output. Defaults to 10000 ms."
        ]
        AlwaysReadOnly
        ParallelSafe
        (typedStreamingRichTool "wait" waitArgsDecoder (\_emit -> runWait host))

runWait :: CodeModeHost -> WaitArgs -> IO (Either Text ToolHandlerResult)
runWait host args
    | maybe False (< 0) args.yieldTimeMs =
        pure (Left "yield_time_ms must be non-negative")
    | maybe False (< 0) args.maxTokens =
        pure (Left "max_tokens must be non-negative")
    | otherwise = do
        started <- getCurrentTime
        result <-
            if args.terminate
                then terminateCodeCell host args.cellId
                else waitCodeCell host args.cellId
                    (resolveYieldTimeoutMs
                        (fromMaybe defaultWaitYieldTimeMs args.yieldTimeMs))
        finished <- getCurrentTime
        pure $
            withResultImages result
                <$> renderCodeModeResult
                    args.maxTokens
                    (elapsedSeconds started finished)
                    result

withResultImages
    :: Either CodeModeError CodeModeResult
    -> Text
    -> ToolHandlerResult
withResultImages result text =
    ToolHandlerResult
        { resultText = text
        , resultImages = codeModeResultImages result
        }

codeModeResultImages
    :: Either CodeModeError CodeModeResult
    -> [ToolResultImage]
codeModeResultImages = \case
    Right CodeModeRunning { cellOutput } -> valueImages cellOutput
    Right CodeModeTerminated { cellValue } -> valueImages cellValue
    Right CodeModeFinished { cellValue } -> valueImages cellValue
    Right CodeModeFailed { cellValue } -> valueImages cellValue
    Left _ -> []
  where
    valueImages (Object result)
        | Just (Array content) <- KeyMap.lookup "content" result =
            mapMaybe contentImage (Vector.toList content)
    valueImages _ = []

    contentImage (Object content)
        | Just (String "image") <- KeyMap.lookup "type" content
        , Just (String imageUrl) <- KeyMap.lookup "image_url" content =
            Just ToolResultImage
                { imageUrl
                , imageDetail = case KeyMap.lookup "detail" content of
                    Just (String detail) -> Just detail
                    _ -> Nothing
                }
    contentImage _ = Nothing

renderCodeModeResult
    :: Maybe Int
    -> Double
    -> Either CodeModeError CodeModeResult
    -> Either Text Text
renderCodeModeResult budget wallTime = \case
    Left (CodeModeExecutionError err) ->
        Right $ withStatus "Script failed"
            ["Script error:\n" <> err]
    -- Codex reports an unknown cell as a failed script result rather than a
    -- protocol error, so the model recovers by rerunning exec.
    Left (CodeModeUnknownCell cellId) ->
        Right $ withStatus "Script failed"
            ["Script error:\nexec cell " <> cellId <> " not found"]
    Left err -> Left (renderCodeModeError err)
    Right CodeModeRunning { cellId, cellOutput } ->
        Right $ withStatus
            ("Script running with cell ID " <> cellId)
            (valueContents budget cellOutput)
    Right CodeModeTerminated { cellValue } ->
        Right $ withStatus "Script terminated"
            (valueContents budget cellValue)
    Right CodeModeFinished { cellValue } ->
        Right $ withStatus "Script completed"
            (valueContents budget cellValue)
    Right CodeModeFailed { cellValue, cellError } ->
        Right $ withStatus "Script failed"
            (valueContents budget cellValue
                <> ["Script error:\n" <> cellError])
  where
    withStatus status content =
        status
            <> "\nWall time "
            <> Text.pack (printf "%.1f" wallTime)
            <> " seconds\nOutput:\n"
            <> truncateOutputItems budget content

    valueContents requested value =
        case value of
            Object result
                | Just (Array content) <- KeyMap.lookup "content" result ->
                    map (contentItem requested) (Vector.toList content)
            String text -> [text]
            Null -> []
            _ -> [renderValue value]

    contentItem _requested value@(Object content)
        | Just (String "text") <- KeyMap.lookup "type" content
        , Just (String text) <- KeyMap.lookup "text" content =
            text
        | Just (String "image") <- KeyMap.lookup "type" content =
            "[image output item; call tools.show_image to display an image to the user]"
        | Just (String "audio") <- KeyMap.lookup "type" content =
            "[audio output item]"
        | otherwise = renderValue value
    contentItem _requested value = renderValue value

-- | Middle truncation with a Codex-style token budget: roughly four bytes per
-- token, keeping half the budget at each end, with the upstream warning
-- header and elision marker.
truncateOutputItems :: Maybe Int -> [Text] -> Text
truncateOutputItems requested items
    | BS.length encoded <= byteBudget = joined
    | otherwise =
        let charBudget = max 2 (byteBudget `div` 2)
            half = charBudget `div` 2
            keptFront = Text.take half joined
            keptBack = Text.takeEnd (charBudget - half) joined
            originalTokens =
                (BS.length encoded + bytesPerToken - 1) `div` bytesPerToken
            truncatedTokens =
                max 1 (originalTokens - tokenBudget)
            totalLines = length (Text.lines joined)
        in "Warning: truncated output (original token count: "
            <> Text.pack (show originalTokens)
            <> ")\nTotal output lines: "
            <> Text.pack (show totalLines)
            <> "\n\n"
            <> keptFront
            <> "…"
            <> Text.pack (show truncatedTokens)
            <> " tokens truncated…"
            <> keptBack
  where
    joined = Text.intercalate "\n" items
    bytesPerToken = 4
    tokenBudget = min 10000 (max 0 (fromMaybe 10000 requested))
    byteBudget = bytesPerToken * tokenBudget
    encoded = TextEncoding.encodeUtf8 joined

elapsedSeconds :: UTCTime -> UTCTime -> Double
elapsedSeconds started finished =
    realToFrac (diffUTCTime finished started)

renderCodeModeError :: CodeModeError -> Text
renderCodeModeError = \case
    CodeModeStartupError err -> "code-mode worker failed to start: " <> err
    CodeModeProtocolError err -> "code-mode protocol failure: " <> err
    CodeModeExecutionError err -> "JavaScript execution failed: " <> err
    CodeModeResourceError err -> "code-mode resource limit: " <> err
    CodeModeUnknownCell cellId -> "exec cell " <> cellId <> " not found"
    CodeModeBusyObserver cellId ->
        "exec cell " <> cellId <> " already has an active observer"
    CodeModeAlreadyTerminating cellId ->
        "exec cell " <> cellId <> " is already terminating"
    CodeModeClosedCell cellId ->
        "exec cell " <> cellId <> " closed unexpectedly"

renderValue :: Value -> Text
renderValue Null = ""
renderValue (String text) = text
renderValue value =
    TextEncoding.decodeUtf8 . LBS.toStrict $ Aeson.encode value

isJavaScriptSafeInteger :: Int -> Bool
isJavaScriptSafeInteger value =
    toInteger value <= 9007199254740991

-- | The upstream freeform grammar, byte-for-byte including the leading and
-- trailing newline of the Rust raw-string literal.
codeModeExecGrammar :: Text
codeModeExecGrammar =
    "\nstart: pragma_source | plain_source\n\
    \pragma_source: PRAGMA_LINE NEWLINE SOURCE\n\
    \plain_source: SOURCE\n\
    \\n\
    \PRAGMA_LINE: /[ \\t]*\\/\\/ @exec:[^\\r\\n]*/\n\
    \NEWLINE: /\\r?\\n/\n\
    \SOURCE: /[\\s\\S]+/\n"

execDescription
    :: Bool
    -> ImageDetailVisibility
    -> [(Text, NestedTool)]
    -> Text
execDescription includeDeclarations detailVisibility tools =
    Text.intercalate "\n\n" sections
  where
    sections =
        [execDescriptionTemplate detailVisibility]
            <> (if includeDeclarations && not (null tools)
                    then [renderNestedToolSections tools]
                    else [])

execDescriptionTemplate :: ImageDetailVisibility -> Text
execDescriptionTemplate detailVisibility =
    Text.intercalate "\n"
        [ "Run JavaScript code to orchestrate/compose tool calls"
        , "- Evaluates the provided JavaScript code in a fresh V8 isolate as an async module."
        , "- All nested tools are available on the global `tools` object, for example `await tools.exec_command(...)`. Tool names are exposed as normalized JavaScript identifiers, for example `await tools.mcp__ologs__get_profile(...)`."
        , "- Nested tool methods take either a string or an object as their input argument."
        , "- Nested tools return either an object or a string, based on the description."
        , "- Runs raw JavaScript -- no Node, no file system, no network access, no console."
        , "- Accepts raw JavaScript source text, not JSON, quoted strings, or markdown code fences."
        , "- You may optionally start the tool input with a first-line pragma like `// @exec: {\"yield_time_ms\": 10000, \"max_output_tokens\": 1000}`."
        , "- `yield_time_ms` asks `exec` to yield early if the script is still running. Defaults to "
            <> Text.pack (show defaultExecYieldTimeMs)
            <> " ms."
        , "- `max_output_tokens` sets the token budget for direct `exec` results. Defaults to 10000 tokens."
        , "- When the JS code is fully evaluated, the isolate's lifetime ends and unawaited promises are silently discarded."
        , ""
        , "- Global helpers:"
        , "- `exit()`: Immediately ends the current script successfully (like an early return from the top level)."
        , "- `text(value: string | number | boolean | undefined | null)`: Appends a text item. Non-string values are stringified with `JSON.stringify(...)` when possible."
        , imageHelperDescription detailVisibility
        , "- `audio(audioUrlOrItem: string | { audio_url: string } | AudioContent)`: Appends an audio item. `audio_url` should be a base64-encoded `data:` URL. To forward an MCP tool audio block, pass an individual `AudioContent` block from `result.content`, for example `audio(result.content[0])`."
        , "- `generatedImage(result: { image_url: string; output_hint?: string })`: Appends an image-generation result and its optional output hint. HTTP(S) URLs are not supported."
        , "- `store(key: string, value: any)`: stores a serializable value under a string key for later `exec` calls in the same session."
        , "- `load(key: string)`: returns the stored value for a string key, or `undefined` if it is missing."
        , "- `notify(value: string | number | boolean | undefined | null)`: immediately injects an extra `custom_tool_call_output` for the current `exec` call. Values are stringified like `text(...)`."
        , "- `setTimeout(callback: () => void, delayMs?: number)`: schedules a callback to run later and returns a timeout id. Pending timeouts do not keep `exec` alive by themselves; await an explicit promise if you need to wait for one."
        , "- `clearTimeout(timeoutId?: number)`: cancels a timeout created by `setTimeout`."
        , "- `ALL_TOOLS`: metadata for the enabled nested tools as `{ name, description }` entries."
        , "- `yield_control()`: yields the accumulated output to the model immediately while the script keeps running."
        ]

imageHelperDescription :: ImageDetailVisibility -> Text
imageHelperDescription = \case
    ImageDetailVisible ->
        "- `image(imageUrlOrItem: string | { image_url: string; detail?: \"auto\" | \"low\" | \"high\" | \"original\" | null } | ImageContent, detail?: \"auto\" | \"low\" | \"high\" | \"original\" | null)`: Appends an image item. `image_url` should be a base64-encoded `data:` URL. To forward an MCP tool image, pass an individual `ImageContent` block from `result.content`, for example `image(result.content[0])`. MCP image blocks may request detail with `_meta: { \"codex/imageDetail\": \"original\" }`. When provided, the second `detail` argument overrides any detail embedded in the first argument."
    ImageDetailHidden ->
        "- `image(imageUrlOrItem: string | { image_url: string } | ImageContent)`: Appends an image item. `image_url` should be a base64-encoded `data:` URL. To forward an MCP tool image, pass an individual `ImageContent` block from `result.content`, for example `image(result.content[0])`."

renderNestedToolSections :: [(Text, NestedTool)] -> Text
renderNestedToolSections =
    Text.intercalate "\n\n" . go Nothing . sortForDescription
  where
    sortForDescription =
        sortOn \(codeName, tool) ->
            ( maybe "" (.namespaceName) tool.nestedNamespace
            , tool.nestedRuntimeName
            , codeName
            )

    go _ [] = []
    go previousNamespace ((codeName, tool) : rest) =
        namespaceSection previousNamespace tool.nestedNamespace
            <> [toolSection codeName tool]
            <> go
                ((.namespaceName) <$> tool.nestedNamespace <|> previousNamespace)
                rest

    namespaceSection :: Maybe Text -> Maybe CodeModeNamespace -> [Text]
    namespaceSection previous = \case
        Just namespace
            | Just namespace.namespaceName /= previous
            , not (Text.null (Text.strip namespace.namespaceDescription)) ->
                [ "## "
                    <> namespace.namespaceName
                    <> "\n"
                    <> Text.strip namespace.namespaceDescription
                ]
        _ -> []

toolSection :: Text -> NestedTool -> Text
toolSection codeName tool =
    heading
        <> "\n"
        <> Text.strip tool.nestedDescription
        <> "\n\nexec tool declaration:\n```ts\n"
        <> declaration
        <> "\n```"
  where
    heading
        | tool.nestedRuntimeName == codeName = "### `" <> codeName <> "`"
        | otherwise =
            "### `" <> codeName <> "` (`" <> tool.nestedRuntimeName <> "`)"
    declaration =
        "declare const tools: { "
            <> codeName
            <> "("
            <> inputName
            <> ": "
            <> inputType
            <> "): Promise<unknown>; };"
    (inputName, inputType) =
        case tool.nestedParameters of
            Just schema -> ("args", renderJsonSchemaType schema)
            Nothing -> ("input", "string")

renderJsonSchemaType :: Value -> Text
renderJsonSchemaType schema =
    let rendered = renderSchema schema 0 schema
    in if BS.length (TextEncoding.encodeUtf8 rendered) > 16000
        then "unknown"
        else rendered

renderSchema :: Value -> Int -> Value -> Text
renderSchema _ depth _
    | depth > 12 = "unknown"
renderSchema _ _ (Bool True) = "unknown"
renderSchema _ _ (Bool False) = "never"
renderSchema root depth (Object schema)
    | Just (String reference) <- KeyMap.lookup "$ref" schema =
        maybe "unknown" (renderSchema root (depth + 1))
            (resolveLocalRef root reference)
    | Just value <- KeyMap.lookup "const" schema =
        renderLiteral value
    | Just (Array values) <- KeyMap.lookup "enum" schema
    , not (Vector.null values) =
        Text.intercalate " | "
            (map renderLiteral (Vector.toList values))
    | Just variants <- firstArray ["anyOf", "oneOf"] schema
    , not (null variants) =
        Text.intercalate " | "
            (map (renderSchema root (depth + 1)) variants)
    | Just variants <- arrayField "allOf" schema
    , not (null variants) =
        Text.intercalate " & "
            (map
                (parenthesizeUnion . renderSchema root (depth + 1))
                variants)
    | Just (String typeName) <- KeyMap.lookup "type" schema =
        renderType root depth schema typeName
    | Just (Array typeNames) <- KeyMap.lookup "type" schema =
        Text.intercalate " | "
            [ renderType root depth schema typeName
            | String typeName <- Vector.toList typeNames
            ]
    | any (`KeyMap.member` schema)
            ["properties", "required", "additionalProperties"] =
        renderObject root depth schema
    | any (`KeyMap.member` schema) ["items", "prefixItems"] =
        renderArray root depth schema
    | otherwise = "unknown"
  where
    firstArray [] _ = Nothing
    firstArray (key : keys) object_ =
        arrayField key object_ <|> firstArray keys object_
renderSchema _ _ _ = "unknown"

resolveLocalRef :: Value -> Text -> Maybe Value
resolveLocalRef root reference = do
    pointer <- Text.stripPrefix "#" reference
    if Text.null pointer
        then Just root
        else do
            path <- Text.stripPrefix "/" pointer
            foldM descend root (map decodeSegment (Text.splitOn "/" path))
  where
    decodeSegment = Text.replace "~1" "/" . Text.replace "~0" "~"
    descend (Object object_) segment =
        KeyMap.lookup (Key.fromText segment) object_
    descend (Array values) segment = do
        index <- readMaybe (Text.unpack segment)
        values Vector.!? index
    descend _ _ = Nothing

arrayField :: Key.Key -> KeyMap.KeyMap Value -> Maybe [Value]
arrayField key schema =
    case KeyMap.lookup key schema of
        Just (Array values) -> Just (Vector.toList values)
        _ -> Nothing

renderType :: Value -> Int -> KeyMap.KeyMap Value -> Text -> Text
renderType root depth schema = \case
    "string" -> "string"
    "number" -> "number"
    "integer" -> "number"
    "boolean" -> "boolean"
    "null" -> "null"
    "array" -> renderArray root depth schema
    "object" -> renderObject root depth schema
    _ -> "unknown"

renderArray :: Value -> Int -> KeyMap.KeyMap Value -> Text
renderArray root depth schema =
    case KeyMap.lookup "items" schema of
        Just item ->
            "Array<" <> renderSchema root (depth + 1) item <> ">"
        Nothing ->
            case arrayField "prefixItems" schema of
                Just items | not (null items) ->
                    "["
                        <> Text.intercalate ", "
                            (map (renderSchema root (depth + 1)) items)
                        <> "]"
                _ -> "unknown[]"

renderObject :: Value -> Int -> KeyMap.KeyMap Value -> Text
renderObject root depth schema
    | any propertyHasDescription properties =
        Text.intercalate "\n" $
            ["{"]
                <> concatMap renderDescribedProperty properties
                <> additionalPropertyLines "  "
                <> ["}"]
    | null compactLines = "{}"
    | otherwise = "{ " <> Text.intercalate " " compactLines <> " }"
  where
    requiredNames =
        case KeyMap.lookup "required" schema of
            Just (Array values) ->
                [ name
                | String name <- Vector.toList values
                ]
            _ -> []
    properties =
        case KeyMap.lookup "properties" schema of
            Just (Object values) ->
                sortOn fst
                    [ (Key.toText name, value)
                    | (name, value) <- KeyMap.toList values
                    ]
            _ -> []
    compactLines =
        map renderProperty properties <> additionalPropertyLines ""

    propertyHasDescription (_, Object property) =
        case KeyMap.lookup "description" property of
            Just (String description) ->
                not (Text.null (Text.strip description))
            _ -> False
    propertyHasDescription _ = False

    renderDescribedProperty property@(_, value) =
        [ "  // " <> Text.strip line
        | line <- descriptionLines value
        , not (Text.null (Text.strip line))
        ]
            <> ["  " <> renderProperty property]

    descriptionLines (Object property) =
        case KeyMap.lookup "description" property of
            Just (String description) -> Text.lines description
            _ -> []
    descriptionLines _ = []

    renderProperty (name, value) =
        renderPropertyName name
            <> (if name `elem` requiredNames then "" else "?")
            <> ": "
            <> renderSchema root (depth + 1) value
            <> ";"

    additionalPropertyLines prefix =
        case KeyMap.lookup "additionalProperties" schema of
            Just (Bool False) -> []
            Just (Bool True) -> [prefix <> "[key: string]: unknown;"]
            Just value ->
                [ prefix
                    <> "[key: string]: "
                    <> renderSchema root (depth + 1) value
                    <> ";"
                ]
            Nothing
                | null properties ->
                    [prefix <> "[key: string]: unknown;"]
                | otherwise -> []

renderPropertyName :: Text -> Text
renderPropertyName name
    | validIdentifier name = name
    | otherwise = renderLiteral (String name)
  where
    validIdentifier value =
        case Text.uncons value of
            Nothing -> False
            Just (first, rest) ->
                validFirst first && Text.all validRest rest
    validFirst char =
        char == '_' || char == '$'
            || ('a' <= char && char <= 'z')
            || ('A' <= char && char <= 'Z')
    validRest char =
        validFirst char || ('0' <= char && char <= '9')

renderLiteral :: Value -> Text
renderLiteral =
    TextEncoding.decodeUtf8 . LBS.toStrict . Aeson.encode

parenthesizeUnion :: Text -> Text
parenthesizeUnion rendered
    | " | " `Text.isInfixOf` rendered = "(" <> rendered <> ")"
    | otherwise = rendered

waitDescription :: Text
waitDescription =
    "Waits on a yielded `exec` cell and returns new output or completion.\n\
    \- Use `wait` only after `exec` returns `Script running with cell ID ...`.\n\
    \- `cell_id` identifies the running `exec` cell to resume.\n\
    \- `yield_time_ms` controls how long to wait for more output before yielding again. Defaults to 10000 ms.\n\
    \- `max_tokens` limits how much new output this wait call returns. Defaults to 10000 tokens.\n\
    \- `terminate: true` stops the running cell; false or omitted waits for output.\n\
    \- `wait` returns only the new output since the last yield, or the final completion or termination result for that cell.\n\
    \- If the cell is still running, `wait` may yield again with the same `cell_id`.\n\
    \- If the cell has already finished, `wait` returns the completed result and closes the cell."
