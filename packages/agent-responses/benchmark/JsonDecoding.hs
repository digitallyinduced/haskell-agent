{-# LANGUAGE BangPatterns #-}

module Main (main) where
import Agent.Json (RawJson, rawJsonFromEncoding)
import Agent.JsonText (jsonTextFieldPartial)
import Agent.Loop (LoopEvent(..))
import qualified Agent.Responses.Codec as Codec
import Agent.Responses.LoopBackend
    ( newStreamEventToLoopEvents
    , responseItemToToolCall
    , streamEventToLoopEventWithRawReasoning
    )
import Agent.Responses.StreamAssembly
    ( StreamAssemblyConfig(..)
    , applyStreamEvent
    , buildStreamResponse
    , emptyStreamAssemblyState
    , failedStreamResponseMessage
    , finishStreamResponse
    )
import Agent.Error (ApiError(..))
import Agent.Responses.Types
import Agent.ToolDispatch (ToolCall(..))
import Control.Exception (evaluate)
import Control.Monad (foldM, forM, (>=>))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Pair)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (atomicModifyIORef', newIORef)
import Data.List (sort)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (die)
import System.Mem (performGC)
import Text.Printf (printf)
data Sample = Sample
    { wallMillis :: !Double
    , cpuMillis :: !Double
    , allocatedBytes :: !Integer
    , maxLiveBytes :: !Integer
    , checksum :: !Int
    }
main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    if enabled then pure () else die "run with +RTS -T"
    getArgs >>= \case
        [mode, eventArg, deltaArg, sampleArg]
            | mode `elem` ["stream", "stream-online", "stream-list"] -> do
            eventCount <- positive "event count" eventArg
            deltaBytes <- positive "delta bytes" deltaArg
            sampleCount <- positive "sample count" sampleArg
            let payloads = streamPayloads eventCount deltaBytes
            _ <- evaluate (sum (map BS.length payloads))
            Codec.withResponseStreamEventDecoder \decode -> do
                samples <- forM [1 .. sampleCount] \_ ->
                    measure
                        (if mode == "stream-list"
                            then runStreamList decode payloads
                            else runStream decode payloads)
                report mode eventCount deltaBytes samples
        ["request", iterationArg, sampleArg] -> do
            iterations <- positive "iteration count" iterationArg
            sampleCount <- positive "sample count" sampleArg
            samples <- forM [1 .. sampleCount] \_ ->
                measure (runRequests iterations)
            report "request" iterations 0 samples
        [mode, bodyArg, deltaArg, repeatArg, sampleArg]
            | mode `elem`
                ["tool-shell-baseline", "tool-shell", "tool-json"] -> do
            bodyChars <- positive "argument body characters" bodyArg
            deltaChars <- positive "delta characters" deltaArg
            repetitions <- positive "repetition count" repeatArg
            sampleCount <- positive "sample count" sampleArg
            let events =
                    toolArgumentEvents
                        (mode /= "tool-json")
                        bodyChars
                        deltaChars
            _ <- evaluate (sum (map argumentEventSize events))
            samples <- forM [1 .. sampleCount] \_ ->
                measure $
                    if mode == "tool-shell-baseline"
                        then runLegacyShellProjection repetitions events
                        else runToolArgumentProjection repetitions events
            report
                (mode <> "-x" <> show repetitions)
                bodyChars
                deltaChars
                samples
        _ -> die $
            "usage: responses-json-bench stream-online EVENTS DELTA_BYTES SAMPLES\n"
                <> "   or: responses-json-bench stream-list EVENTS DELTA_BYTES SAMPLES\n"
                <> "   or: responses-json-bench request ITERATIONS SAMPLES\n"
                <> "   or: responses-json-bench tool-shell-baseline BODY_CHARS DELTA_CHARS REPETITIONS SAMPLES\n"
                <> "   or: responses-json-bench tool-shell BODY_CHARS DELTA_CHARS REPETITIONS SAMPLES\n"
                <> "   or: responses-json-bench tool-json BODY_CHARS DELTA_CHARS REPETITIONS SAMPLES"
positive :: String -> String -> IO Int
positive label raw = case reads raw of
    [(value, "")] | value > 0 -> pure value
    _ -> die ("invalid " <> label <> ": " <> raw)
report :: String -> Int -> Int -> [Sample] -> IO ()
report mode count bytes samples =
    printf "%s,%d,%d,%d,%.3f,%.3f,%d,%d,%d\n"
        mode count bytes (length samples)
        (median (map (.wallMillis) samples))
        (median (map (.cpuMillis) samples))
        (median (map (.allocatedBytes) samples))
        (median (map (.maxLiveBytes) samples))
        (median (map (.checksum) samples))
median :: Ord value => [value] -> value
median values = sort values !! (length values `div` 2)
runStream
    :: (BS.ByteString -> IO (Either Text.Text ResponseStreamEvent))
    -> [BS.ByteString]
    -> IO Int
runStream decode = go emptyStreamAssemblyState
  where
    go !_ [] = error "stream has no terminal event"
    go !state (payload : rest) = do
        event <- decode payload >>= either (error . Text.unpack) pure
        let !next = applyStreamEvent state event
        case event of
            ResponseCompletedEvent{} -> case finishStreamResponse Nothing next event of
                Left err -> error (show err)
                Right response -> evaluate (responseChecksum response)
            _ -> go next rest

-- Compatibility/list baseline: decode and retain every event before assembly,
-- matching the former shared HTTP transport's memory shape.
runStreamList
    :: (BS.ByteString -> IO (Either Text.Text ResponseStreamEvent))
    -> [BS.ByteString]
    -> IO Int
runStreamList decode payloads = do
    events <- mapM (decode >=> either (error . Text.unpack) pure) payloads
    case buildStreamResponse benchmarkAssemblyConfig events of
        Left err -> error (show err)
        Right response -> evaluate (responseChecksum response)

benchmarkAssemblyConfig :: StreamAssemblyConfig
benchmarkAssemblyConfig = StreamAssemblyConfig
    { missingCompletionMessage = "benchmark stream has no terminal event"
    , classifyStreamError =
        \streamError -> ConnectionError streamError.message
    , classifyFailedResponse =
        ConnectionError . failedStreamResponseMessage
    , incompleteAsFailure = False
    }
runRequests :: Int -> IO Int
runRequests count = go count checksumSeed
  where
    go 0 !checksum = pure checksum
    go remaining !checksum = do
        let bytes = Codec.encodeResponseCreateParams (requestParams remaining)
            checksum' = LBS.foldl'
                (\value byte -> value * 33 + fromIntegral byte)
                checksum bytes
        checksum' `seq` go (remaining - 1) checksum'

runToolArgumentProjection :: Int -> [ResponseStreamEvent] -> IO Int
runToolArgumentProjection repetitions events = go repetitions checksumSeed
  where
    go 0 !result = pure result
    go remaining !result = do
        project <- newStreamEventToLoopEvents False
        next <- foldM (projectOne project) result events
        go (remaining - 1) next
    projectOne project !current event = do
        projected <- project event
        pure $! foldl' loopEventChecksum current projected

-- Conservative compatibility baseline for the shell-preview hot path replaced
-- by batching. Keep this local to the benchmark: each delta extends the
-- previously published strict Text, reparses the entire prefix, and rebuilds
-- the ToolArgumentsUpdated payload through the former per-event IORef shape.
runLegacyShellProjection :: Int -> [ResponseStreamEvent] -> IO Int
runLegacyShellProjection repetitions events =
    go repetitions checksumSeed
  where
    go 0 !result = pure result
    go remaining !result = do
        project <- newLegacyShellProjector
        next <- foldM (projectOne project) result events
        go (remaining - 1) next

    projectOne project !current event = do
        projected <- project event
        pure $! foldl' loopEventChecksum current projected

newLegacyShellProjector
    :: IO (ResponseStreamEvent -> IO [LoopEvent])
newLegacyShellProjector = do
    stateRef <- newIORef emptyLegacyShellState
    pure \event -> do
        argumentEvents <- atomicModifyIORef' stateRef \state ->
            legacyShellStreamStep event state
        pure $
            maybeToList
                (streamEventToLoopEventWithRawReasoning False event)
                <> argumentEvents

data LegacyShellState = LegacyShellState
    { legacyShellCall :: !(Maybe ToolCall)
    , legacyShellPreview :: !(Maybe Text)
    }

emptyLegacyShellState :: LegacyShellState
emptyLegacyShellState = LegacyShellState
    { legacyShellCall = Nothing
    , legacyShellPreview = Nothing
    }

legacyShellStreamStep
    :: ResponseStreamEvent
    -> LegacyShellState
    -> (LegacyShellState, [LoopEvent])
legacyShellStreamStep event state = case event of
    ResponseOutputItemAddedEvent { item = FunctionCallItem call } ->
        ( state
            { legacyShellCall =
                responseItemToToolCall (FunctionCallItem call)
            }
        , []
        )
    ResponseFunctionCallArgumentsDeltaEvent { delta = Just deltaText } ->
        maybe (state, [])
            (\call -> publishLegacyShellCall
                call
                (appendLegacyArgumentPrefix call.arguments deltaText)
                state)
            state.legacyShellCall
    ResponseFunctionCallArgumentsDoneEvent { arguments } ->
        maybe (state, [])
            (\call -> publishLegacyShellCall
                call
                (Text.copy
                    (Text.take legacyShellArgumentPrefixChars
                        (maybe call.arguments id arguments)))
                state)
            state.legacyShellCall
    _ -> (state, [])

legacyShellArgumentPrefixChars :: Int
legacyShellArgumentPrefixChars = 4096

appendLegacyArgumentPrefix :: Text -> Text -> Text
appendLegacyArgumentPrefix previous delta
    | room <= 0 = previous
    | Text.null previous = retainedDelta
    | otherwise = previous <> retainedDelta
  where
    room = legacyShellArgumentPrefixChars - Text.length previous
    retainedDelta = Text.copy (Text.take room delta)

publishLegacyShellCall
    :: ToolCall
    -> Text
    -> LegacyShellState
    -> (LegacyShellState, [LoopEvent])
publishLegacyShellCall call rawArguments state =
    let updatedCall = withLegacyToolArguments call rawArguments
        maybeCommand =
            Text.takeWhile (/= '\n')
                <$> jsonTextFieldPartial "command" rawArguments
        changed = maybe False
            (\value ->
                not (Text.null value)
                    && Just value /= state.legacyShellPreview)
            maybeCommand
        displayCall command =
            withLegacyToolArguments updatedCall $
                Text.Encoding.decodeUtf8
                    (LBS.toStrict
                        (Aeson.encode
                            (Aeson.object ["command" Aeson..= command])))
        next = state
            { legacyShellCall = Just updatedCall
            , legacyShellPreview =
                maybe state.legacyShellPreview Just maybeCommand
            }
    in
    ( next
    , [ ToolArgumentsUpdated (displayCall command)
      | changed
      , command <- maybeToList maybeCommand
      ]
    )

withLegacyToolArguments :: ToolCall -> Text -> ToolCall
withLegacyToolArguments ToolCall
    { callId
    , name
    , callKind
    , argumentsEncrypted
    } arguments =
    ToolCall
        { callId
        , name
        , arguments
        , callKind
        , argumentsEncrypted
        }

toolArgumentEvents :: Bool -> Int -> Int -> [ResponseStreamEvent]
toolArgumentEvents shell bodyChars deltaChars =
    added : zipWith deltaEvent [1 ..] chunks <> [done]
  where
    name = if shell then "shell_command" else "grep"
    field = if shell then "command" else "pattern"
    arguments =
        "{\"" <> field <> "\":\""
            <> Text.replicate bodyChars "x"
            <> "\"}"
    chunks = Text.chunksOf deltaChars arguments
    call = FunctionCall
        { itemId = Just "benchmark-tool-item"
        , callId = "benchmark-tool-call"
        , name
        , namespace = Nothing
        , provider = Nothing
        , arguments = ""
        , encryptedFunctionArgs = Nothing
        , status = Nothing
        , async = Nothing
        }
    added = ResponseOutputItemAddedEvent
        { item = FunctionCallItem call
        , outputIndex = Just 0
        , sequenceNumber = Just 0
        }
    deltaEvent sequenceNumber chunk =
        ResponseFunctionCallArgumentsDeltaEvent
            { delta = Just chunk
            , streamItemId = call.itemId
            , streamOutputIndex = Just 0
            , sequenceNumber = Just sequenceNumber
            }
    done = ResponseFunctionCallArgumentsDoneEvent
        { arguments = Just arguments
        , functionName = Just name
        , streamItemId = call.itemId
        , streamOutputIndex = Just 0
        , sequenceNumber = Just (length chunks + 1)
        }

argumentEventSize :: ResponseStreamEvent -> Int
argumentEventSize = \case
    ResponseOutputItemAddedEvent
        { item = FunctionCallItem call } ->
            Text.length call.name + Text.length call.arguments
    ResponseFunctionCallArgumentsDeltaEvent { delta } ->
        maybe 0 Text.length delta
    ResponseFunctionCallArgumentsDoneEvent { arguments } ->
        maybe 0 Text.length arguments
    _ -> 0

loopEventChecksum :: Int -> LoopEvent -> Int
loopEventChecksum current event =
    current * 33 + case event of
        ToolStarted call -> callChecksum call
        ToolUpdated call -> callChecksum call
        ToolArgumentsUpdated call -> callChecksum call
        ActivityUpdated text -> Text.length text
        WarningRaised text -> Text.length text
        _ -> 1
  where
    callChecksum call =
        Text.length call.callId
            + Text.length call.name
            + Text.length call.arguments
requestParams :: Int -> ResponseCreateParams
requestParams iteration = defaultResponseCreateParams
    { model = Just "gpt-5"
    , instructions = Just "Inspect, edit, test, and report concisely."
    , input = Just (ResponseInputText
        ("Fix regression number " <> Text.pack (show iteration) <> "."))
    , tools = Just
        [ CustomToolValue CustomTool
            { name = "apply_patch"
            , description = Just "Apply a patch to files."
            , format = Just patchFormat
            , async = Nothing
            }
        , FunctionToolValue FunctionTool
            { name = "shell_command"
            , description = Just "Run a shell command."
            , parameters = Just shellSchema
            , strict = Just True
            , async = Nothing
            }
        , NamespaceToolValue NamespaceTool
            { name = "workspace"
            , description = Just "Read-only repository operations."
            , tools =
                [ FunctionToolValue FunctionTool
                    { name = "read_file"
                    , description = Just "Read a file."
                    , parameters = Just readSchema
                    , strict = Just True
                    , async = Nothing
                    }
                ]
            }
        ]
    }
shellSchema, readSchema, patchFormat :: RawJson
shellSchema = objectSchema
    [ "command" Aeson..= jsonType "string"
    , "yield_time_ms" Aeson..= jsonType "integer"
    ] ["command"]
readSchema = objectSchema
    [ "target_file" Aeson..= jsonType "string"
    , "offset" Aeson..= integerMin
    , "limit" Aeson..= integerMin
    ] ["target_file"]
patchFormat = rawJson
    [ "type" Aeson..= ("grammar" :: Text.Text)
    , "syntax" Aeson..= ("lark" :: Text.Text)
    , "definition" Aeson..=
        ("start: begin hunk+ end\nbegin: \"*** Begin Patch\"\n\
         \hunk: /[^\\n]+/ NEWLINE\nend: \"*** End Patch\"" :: Text.Text)
    ]
rawJson :: [Pair] -> RawJson
rawJson = rawJsonFromEncoding . Aeson.toEncoding . Aeson.object
objectSchema :: [Pair] -> [Text.Text] -> RawJson
objectSchema properties required = rawJson
    [ "type" Aeson..= ("object" :: Text.Text)
    , "properties" Aeson..= Aeson.object properties
    , "required" Aeson..= required
    , "additionalProperties" Aeson..= False
    ]
jsonType :: Text.Text -> Aeson.Value
jsonType value = Aeson.object ["type" Aeson..= value]
integerMin :: Aeson.Value
integerMin = Aeson.object
    ["type" Aeson..= ("integer" :: Text.Text), "minimum" Aeson..= (1 :: Int)]
streamPayloads :: Int -> Int -> [BS.ByteString]
streamPayloads eventCount deltaBytes =
    [lifecycle "response.created" "in_progress", lifecycle
        "response.in_progress" "in_progress"]
        <> added
        <> map delta [1 .. eventCount]
        <> done
        <> [lifecycle "response.completed" "completed"]
  where
    chunk = BS.replicate deltaBytes 120
    groups = eventCount `div` 10
    remainder = eventCount `mod` 10
    full count = BS.replicate (count * deltaBytes) 120
    reasoningBytes = full (groups * 5 + min remainder 4)
    outputBytes = full (groups * 3 + max 0 (min remainder 7 - 4))
    functionBytes = full (groups + if remainder >= 8 then 1 else 0)
    customBytes = full (groups + if remainder >= 9 then 1 else 0)
    lifecycle eventType status = BS.concat
        [ "{\"type\":\"", eventType, "\",\"response\":{\"id\":\"resp-bench\""
        , ",\"model\":\"gpt-5\",\"status\":\"", status, "\",\"tools\":["
        , "{\"type\":\"namespace\",\"name\":\"workspace\",\"tools\":["
        , "{\"type\":\"function\",\"name\":\"search\",\"parameters\":"
        , largeSchema, "}]}],\"future_namespace\":{\"schema\":"
        , largeSchema, "},\"usage\":{\"input_tokens\":1200,\"output_tokens\":"
        , BS8.pack (show eventCount), ",\"total_tokens\":"
        , BS8.pack (show (1200 + eventCount)), "}}}"
        ]
    largeSchema = BS.concat
        [ "{\"type\":\"object\",\"properties\":{\"query\":{\"type\":\"string\","
        , "\"description\":\"", BS.replicate 8192 115, "\"}}}"
        ]
    added =
        [ itemEvent "added" 0
            "{\"type\":\"reasoning\",\"id\":\"rs-1\",\"summary\":[]}"
        , itemEvent "added" 1
            "{\"type\":\"message\",\"id\":\"msg-1\",\"role\":\"assistant\",\
            \\"content\":[]}"
        , itemEvent "added" 2
            "{\"type\":\"function_call\",\"id\":\"fc-1\",\"call_id\":\"call-f\",\
            \\"name\":\"shell_command\",\"arguments\":\"\"}"
        , itemEvent "added" 3
            "{\"type\":\"custom_tool_call\",\"id\":\"ct-1\",\"call_id\":\"call-c\",\
            \\"name\":\"apply_patch\",\"input\":\"\"}"
        ]
    done =
        [ itemEvent "done" 0 (BS.concat
            ["{\"type\":\"reasoning\",\"id\":\"rs-1\",\"summary\":[{\"type\":\
             \\"summary_text\",\"text\":\"", reasoningBytes, "\"}]}"])
        , itemEvent "done" 1 (BS.concat
            ["{\"type\":\"message\",\"id\":\"msg-1\",\"role\":\"assistant\",\
             \\"content\":[{\"type\":\"output_text\",\"text\":\"",
             outputBytes, "\"}]}"])
        , itemEvent "done" 2 (BS.concat
            ["{\"type\":\"function_call\",\"id\":\"fc-1\",\"call_id\":\"call-f\",\
             \\"name\":\"shell_command\",\"arguments\":\"", functionBytes, "\"}"])
        , itemEvent "done" 3 (BS.concat
            ["{\"type\":\"custom_tool_call\",\"id\":\"ct-1\",\"call_id\":\"call-c\",\
             \\"name\":\"apply_patch\",\"input\":\"", customBytes, "\"}"])
        ]
    itemEvent :: BS.ByteString -> Int -> BS.ByteString -> BS.ByteString
    itemEvent phase index item = BS.concat
        [ "{\"type\":\"response.output_item.", phase, "\",\"output_index\":"
        , BS8.pack (show index), ",\"item\":", item, "}"
        ]
    delta sequenceNumber = BS.concat
        [ "{\"type\":\"", eventType, "\",\"sequence_number\":"
        , BS8.pack (show sequenceNumber), fields, ",\"delta\":\"", chunk, "\"}"
        ]
      where
        (eventType, fields) = case sequenceNumber `mod` 10 of
            value | value < 5 ->
                ("response.reasoning_summary_text.delta",
                    ",\"item_id\":\"rs-1\",\"output_index\":0,\"summary_index\":0")
            value | value < 8 ->
                ("response.output_text.delta",
                    ",\"item_id\":\"msg-1\",\"output_index\":1,\
                    \\"content_index\":0")
            8 -> ("response.function_call_arguments.delta",
                    ",\"item_id\":\"fc-1\",\"output_index\":2")
            _ -> ("response.custom_tool_call_input.delta",
                    ",\"item_id\":\"ct-1\",\"call_id\":\"call-c\",\
                    \\"output_index\":3")
responseChecksum :: Response -> Int
responseChecksum response =
    textChecksum response.responseId
        + textChecksum response.model
        + length (show response.status)
        + sum (map itemChecksum response.output)
        + maybe 0 (sum . map toolChecksum) response.tools
        + maybe 0 usageChecksum response.usage
  where
    usageChecksum usage =
        usage.inputTokens + usage.outputTokens + usage.totalTokens
    itemChecksum = \case
        MessageItem message -> case message.content of
            MessageContentText text -> textChecksum text
            MessageContentParts parts -> sum (map partChecksum parts)
        FunctionCallItem call ->
            textChecksum call.name + textChecksum call.arguments
        CustomToolCallItem call ->
            textChecksum call.name + textChecksum call.input
        ReasoningItemValue item ->
            sum [maybe 0 textChecksum part.text | part <- item.summary]
        _ -> 1
    partChecksum = \case
        OutputTextPart { text } -> textChecksum text
        _ -> 1
    toolChecksum = \case
        FunctionToolValue tool ->
            textChecksum tool.name + maybe 0 (length . show) tool.parameters
        CustomToolValue tool ->
            textChecksum tool.name + maybe 0 (length . show) tool.format
        NamespaceToolValue tool ->
            textChecksum tool.name + sum (map toolChecksum tool.tools)
        _ -> 1
textChecksum :: Text.Text -> Int
textChecksum = Text.foldl' (\value character -> value * 33 + fromEnum character)
    checksumSeed
checksumSeed :: Int
checksumSeed = 5381
measure :: IO Int -> IO Sample
measure action = do
    performGC
    beforeStats <- getRTSStats
    beforeCPU <- getCPUTime
    beforeWall <- getMonotonicTimeNSec
    result <- action >>= evaluate
    afterWall <- getMonotonicTimeNSec
    afterCPU <- getCPUTime
    -- Flush the current nursery into allocated_bytes without charging this GC
    -- to the wall/CPU sample.
    performGC
    afterStats <- getRTSStats
    pure Sample
        { wallMillis = fromIntegral (afterWall - beforeWall) / 1.0e6
        , cpuMillis = fromIntegral (afterCPU - beforeCPU) / 1.0e9
        , allocatedBytes = fromIntegral
            (afterStats.allocated_bytes - beforeStats.allocated_bytes)
        , maxLiveBytes = fromIntegral afterStats.max_live_bytes
        , checksum = result
        }
