{-# LANGUAGE BangPatterns #-}

module Main (main) where

import qualified Agent.Json.Decoder as Decoder
import qualified Agent.Json.Decoder.Hermes as Hermes
import qualified Agent.Json.Encoder as Encoder
import Agent.Json
    ( Extensions
    , extensionsSourceRaw
    , extensionsToList
    , rawJsonBytes
    )
import Agent.Error (ApiError)
import qualified Agent.Responses.Codec as ResponsesCodec
import Agent.Responses.SSE
import qualified Agent.Responses.Hermes as ResponsesHermes
import Agent.Responses.StreamAssembly
    ( applyStreamEvent
    , emptyStreamAssemblyState
    , finishStreamResponse
    )
import Agent.Responses.Types
import Control.Monad (foldM, forM)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (die)
import System.Mem (performGC)
import Text.Printf (printf)

data Workload
    = DirectSse
    | AesonSse
    | DirectSseExtensions
    | AesonSseExtensions
    | DirectRequest
    | AesonRequest
    | PortableStream
    | HermesStream
    deriving stock (Eq)

data LegacyEvent = LegacyEvent
    { legacyType :: !Text
    , legacySequence :: !Aeson.Value
    , legacyItemId :: !Text
    , legacyOutputIndex :: !Aeson.Value
    , legacyContentIndex :: !Aeson.Value
    , legacyDelta :: !Text
    }

instance Aeson.FromJSON LegacyEvent where
    parseJSON = Aeson.withObject "LegacyEvent" \objectValue ->
        LegacyEvent
            <$> objectValue Aeson..: "type"
            <*> objectValue Aeson..: "sequence_number"
            <*> objectValue Aeson..: "item_id"
            <*> objectValue Aeson..: "output_index"
            <*> objectValue Aeson..: "content_index"
            <*> objectValue Aeson..: "delta"

data Sample = Sample
    { wallMillis :: !Double
    , cpuMillis :: !Double
    , allocatedBytes :: !Integer
    }

main :: IO ()
main = do
    statsEnabled <- getRTSStatsEnabled
    if statsEnabled then pure () else die "run with +RTS -T"
    getArgs >>= \case
        [workloadArg, eventCountArg, deltaBytesArg, sampleCountArg] -> do
            workload <- parseWorkload workloadArg
            eventCount <- positive "event count" eventCountArg
            deltaBytes <- positive "delta bytes" deltaBytesArg
            sampleCount <- positive "sample count" sampleCountArg
            let chunks = fixtureChunks
                    (workload == DirectSseExtensions
                        || workload == AesonSseExtensions)
                    eventCount
                    deltaBytes
                request = fixtureRequest eventCount deltaBytes
                streamFrames =
                    fixtureStreamFrames eventCount deltaBytes
            validate workload chunks streamFrames request
            samples <- forM [1 .. sampleCount] \sampleIndex ->
                measure $
                    runWorkload
                        workload
                        chunks
                        streamFrames
                        (varyRequest sampleIndex request)
            let median project =
                    sort (map project samples) !! (sampleCount `div` 2)
                replayDivisor =
                    if isStreamWorkload workload
                        then streamReplayCount
                        else 1
            printf
                "%s,%d,%d,%d,%.3f,%.3f,%d\n"
                workloadArg eventCount deltaBytes sampleCount
                (median (.wallMillis) / fromIntegral replayDivisor)
                (median (.cpuMillis) / fromIntegral replayDivisor)
                (median (.allocatedBytes)
                    `div` fromIntegral replayDivisor)
        _ -> die $
            "usage: responses-hot-path WORKLOAD EVENTS DELTA_BYTES SAMPLES\n"
                <> "workloads: direct-sse, aeson-sse, "
                <> "direct-sse-extensions, aeson-sse-extensions, "
                <> "direct-request, aeson-request, "
                <> "portable-stream, hermes-stream"

parseWorkload :: String -> IO Workload
parseWorkload = \case
    "direct-sse" -> pure DirectSse
    "aeson-sse" -> pure AesonSse
    "direct-sse-extensions" -> pure DirectSseExtensions
    "aeson-sse-extensions" -> pure AesonSseExtensions
    "direct-request" -> pure DirectRequest
    "aeson-request" -> pure AesonRequest
    "portable-stream" -> pure PortableStream
    "hermes-stream" -> pure HermesStream
    value -> die ("unknown workload: " <> value)

isStreamWorkload :: Workload -> Bool
isStreamWorkload PortableStream = True
isStreamWorkload HermesStream = True
isStreamWorkload _ = False

positive :: String -> String -> IO Int
positive label raw = case reads raw of
    [(value, "")] | value > 0 -> pure value
    _ -> die ("invalid " <> label <> ": " <> raw)

runWorkload
    :: Workload
    -> [BS.ByteString]
    -> [BS.ByteString]
    -> ResponseCreateParams
    -> IO Int
runWorkload workload chunks streamFrames request =
    case workload of
        DirectSse -> runDirectSse chunks
        AesonSse -> runAesonSse chunks
        DirectSseExtensions -> runDirectSse chunks
        AesonSseExtensions -> runAesonSse chunks
        DirectRequest ->
            pure (checksum
                (Encoder.encode responseCreateParamsEncoder request))
        AesonRequest ->
            pure (checksum
                (LBS.toStrict
                    (Aeson.encode (legacyRequestValue request))))
        PortableStream ->
            runStreamBatch $
                runTypedStream
                    (pure . ResponsesCodec.decodeResponseStreamEvent)
                    streamFrames
        HermesStream ->
            runStreamBatch $
                ResponsesCodec.withResponseStreamEventDecoder \decodeEvent ->
                    runTypedStream decodeEvent streamFrames

runDirectSse :: [BS.ByteString] -> IO Int
runDirectSse chunks =
    Hermes.withDecoderSession \session -> do
        result <- foldM (directStep session)
            (Right (newSseDecoder, 0)) chunks
        case result of
            Left err -> error (show err)
            Right (_, value) -> pure value

directStep
    :: Hermes.DecoderSession
    -> Either ApiError (SseDecoder, Int)
    -> BS.ByteString
    -> IO (Either ApiError (SseDecoder, Int))
directStep _ left@(Left _) _ = pure left
directStep session (Right (decoder, total)) chunk = do
    decoded <- feedSseDecoderWith decodeFrame decoder chunk
    pure $ case decoded of
        Left err -> Left err
        Right (next, events) ->
            Right (next, foldl checksumEvent total events)
  where
    decodeFrame frame = do
        case frame.sseFrameEventType of
            Just eventType
                | eventType == "response.output_text.delta" -> do
                    value <- Hermes.decodeHermesIO
                        session
                        (ResponsesHermes.textDeltaEventDecoder eventType)
                        frame.sseFrameData
                    pure $ case value of
                        Left _ -> Nothing
                        Right ResponsesHermes.TextDeltaFields{..} ->
                                Just ResponseOutputTextDeltaEvent
                                    { delta
                                    , streamItemId = itemId
                                    , streamOutputIndex = outputIndex
                                    , contentIndex
                                    , logprobs
                                    , sequenceNumber
                                    , eventExtraFields = extensions
                                    }
            _ -> do
                value <- Hermes.decodeIO session
                    (responseStreamEventDecoderWithType
                        frame.sseFrameEventType)
                    frame.sseFrameData
                pure (either (const Nothing) Just value)

checksumEvent :: Int -> ResponseStreamEvent -> Int
checksumEvent current = \case
    ResponseOutputTextDeltaEvent{..} ->
        checksumDelta current delta streamItemId sequenceNumber
            + maybe 0 id streamOutputIndex
            + maybe 0 id contentIndex
    ResponseReasoningSummaryTextDeltaEvent{..} ->
        checksumDelta current delta streamItemId sequenceNumber
            + maybe 0 id streamOutputIndex
            + maybe 0 id summaryIndex
    ResponseFunctionCallArgumentsDeltaEvent{..} ->
        checksumDelta current delta streamItemId sequenceNumber
            + maybe 0 id streamOutputIndex
    ResponseCustomToolInputDeltaEvent{..} ->
        checksumDelta current delta streamItemId sequenceNumber
            + maybe 0 id streamOutputIndex
            + maybe 0 Text.length streamCallId
    ResponseCreatedEvent{responseValue} ->
        checksumResponse current responseValue
    ResponseInProgressEvent{responseValue} ->
        checksumResponse current responseValue
    ResponseCompletedEvent{responseValue} ->
        checksumResponse current responseValue
    ResponseDoneEvent{responseValue} ->
        checksumResponse current responseValue
    ResponseIncompleteEvent{responseValue} ->
        checksumResponse current responseValue
    ResponseFailedEvent{responseValue} ->
        checksumResponse current responseValue
    event ->
        current
            + maybe 0 id (responseStreamEventSequenceNumber event)

checksumDelta
    :: Int
    -> Maybe Text
    -> Maybe Text
    -> Maybe Int
    -> Int
checksumDelta current delta itemId sequenceNumber =
    current
        + maybe 0 id sequenceNumber
        + maybe 0 Text.length delta
        + maybe 0 Text.length itemId

checksumResponse :: Int -> Response -> Int
checksumResponse current response =
    current
        + Text.length response.responseId
        + Text.length response.model
        + Text.length response.object
        + sum (map checksumItem response.output)
        + maybe 0 checksumExtensions response.metadata
        + maybe 0 (sum . map checksumTool) response.tools
        + checksumExtensions response.extraFields

checksumItem :: ResponseItem -> Int
checksumItem = \case
    MessageItem message ->
        maybe 0 Text.length message.messageId
            + checksumMessageContent message.content
            + checksumExtensions message.extraFields
    ReasoningItemValue reasoning ->
        maybe 0 Text.length reasoning.itemId
            + sum
                [ Text.length part.partType
                    + maybe 0 Text.length part.text
                | part <- reasoning.summary
                ]
            + maybe 0 length reasoning.content
            + checksumExtensions reasoning.extraFields
    FunctionCallItem call ->
        maybe 0 Text.length call.itemId
            + Text.length call.callId
            + Text.length call.name
            + Text.length call.arguments
            + checksumExtensions call.extraFields
    CustomToolCallItem call ->
        maybe 0 Text.length call.itemId
            + Text.length call.callId
            + Text.length call.name
            + Text.length call.input
            + checksumExtensions call.extraFields
    _ -> 1

checksumMessageContent :: MessageContent -> Int
checksumMessageContent = \case
    MessageContentText value -> Text.length value
    MessageContentParts parts -> length parts

checksumTool :: ResponseTool -> Int
checksumTool = \case
    FunctionToolValue FunctionTool{..} ->
        Text.length name
            + maybe 0 Text.length description
            + maybe 0 (BS.length . rawJsonBytes) parameters
            + checksumExtensions extraFields
    KnownResponseTool toolType TaggedObject{..} ->
        Text.length (responseToolTypeText toolType)
            + Text.length tag
            + checksumExtensions fields
    UnknownResponseTool TaggedObject{..} ->
        Text.length tag + checksumExtensions fields

checksumExtensions :: Extensions -> Int
checksumExtensions extensions =
    maybe 0 (BS.length . rawJsonBytes)
        (extensionsSourceRaw extensions)
        + sum
            [ Text.length key + BS.length (rawJsonBytes value)
            | (key, value) <- extensionsToList extensions
            ]

runAesonSse :: [BS.ByteString] -> IO Int
runAesonSse chunks =
    pure $ either (error . show) snd $
        foldM step (newSseDecoder, 0) chunks
  where
    step
        :: (SseDecoder, Int)
        -> BS.ByteString
        -> Either ApiError (SseDecoder, Int)
    step (decoder, total) chunk = do
        (next, frames) <- feedSseFrameDecoder decoder chunk
        let values =
                [ value
                | frame <- frames
                , Right value <-
                    [ Aeson.eitherDecodeStrict' frame.sseFrameData
                        :: Either String LegacyEvent
                    ]
                ]
        pure
            ( next
            , foldl
                (\current value ->
                    current
                        + valueSize value.legacySequence
                        + Text.length value.legacyDelta
                        + Text.length value.legacyType
                        + Text.length value.legacyItemId
                        + valueSize value.legacyOutputIndex
                        + valueSize value.legacyContentIndex)
                total values
            )

    valueSize = \case
        Aeson.String value -> Text.length value
        Aeson.Number _ -> 1
        Aeson.Bool value -> if value then 1 else 0
        Aeson.Null -> 0
        Aeson.Array values -> length values
        Aeson.Object values -> length values

runTypedStream
    :: (BS.ByteString -> IO (Either String ResponseStreamEvent))
    -> [BS.ByteString]
    -> IO Int
runTypedStream decodeEvent =
    go emptyStreamAssemblyState 0
  where
    go _ _ [] =
        error "stream replay ended without a terminal response"
    go state total (frame : rest) = do
        event <- decodeEvent frame >>= either error pure
        let nextState = applyStreamEvent state event
            nextTotal = checksumEvent total event
        case event of
            ResponseCompletedEvent{} ->
                finish nextState nextTotal event rest
            ResponseDoneEvent{} ->
                finish nextState nextTotal event rest
            ResponseIncompleteEvent{} ->
                finish nextState nextTotal event rest
            ResponseFailedEvent{} ->
                finish nextState nextTotal event rest
            _ ->
                go nextState nextTotal rest

    finish state total terminal rest
        | not (null rest) =
            error "stream replay has frames after the terminal response"
        | otherwise =
            case finishStreamResponse Nothing state terminal of
                Left err -> error (show err)
                Right response ->
                    pure (checksumResponse total response)

runStreamBatch :: IO Int -> IO Int
runStreamBatch runOne =
    go streamReplayCount 0
  where
    go 0 !total = pure total
    go remaining !total = do
        value <- runOne
        go (remaining - 1) (total + value)

streamReplayCount :: Int
streamReplayCount = 100

legacyRequestValue :: ResponseCreateParams -> Aeson.Value
legacyRequestValue request =
    Aeson.object
        [ "model" Aeson..= request.model
        , "stream" Aeson..= request.stream
        , "input" Aeson..= fixtureInputText request
        , "max_output_tokens" Aeson..= request.maxOutputTokens
        ]

fixtureInputText :: ResponseCreateParams -> Maybe Text
fixtureInputText request =
    case request.input of
        Just (ResponseInputText value) -> Just value
        _ -> Nothing

fixtureChunks :: Bool -> Int -> Int -> [BS.ByteString]
fixtureChunks includeExtensions eventCount deltaBytes =
    chunksOf 4_096 $
        BS.concat
            [ "event: response.output_text.delta\n"
                <> "data: {\"type\":\"response.output_text.delta\","
                <> "\"sequence_number\":" <> ascii index
                <> ",\"item_id\":\"msg_1\",\"output_index\":0,"
                <> "\"content_index\":0,\"delta\":\""
                <> BS.replicate deltaBytes 0x78
                <> if includeExtensions
                    then "\",\"vendor\":{\"x\":1}}\n\n"
                    else "\"}\n\n"
            | index <- [1 .. eventCount]
            ]

fixtureRequest :: Int -> Int -> ResponseCreateParams
fixtureRequest eventCount deltaBytes =
    defaultResponseCreateParams
        { model = Just "gpt-5"
        , input = Just (ResponseInputText
            (Text.replicate (eventCount * deltaBytes) "x"))
        , stream = Just True
        , maxOutputTokens = Just 2_048
        }

fixtureStreamFrames :: Int -> Int -> [BS.ByteString]
fixtureStreamFrames eventCount deltaBytes =
    [lifecycleFrame "response.created" "in_progress" 0]
        <> [lifecycleFrame "response.in_progress" "in_progress" 1]
        <> zipWith (itemFrame "response.output_item.added")
            [2 ..]
            (items "in_progress")
        <> zipWith deltaFrame [6 ..] [0 .. eventCount - 1]
        <> zipWith (itemFrame "response.output_item.done")
            [eventCount + 6 ..]
            (items "completed")
        <> [ lifecycleFrame
                "response.completed"
                "completed"
                (eventCount + 10)
           ]
  where
    lifecycleFrame eventType status sequenceNumber =
        "{\"type\":\"" <> eventType
            <> "\",\"response\":"
            <> lifecycleResponse status
            <> ",\"sequence_number\":"
            <> ascii sequenceNumber
            <> "}"

    lifecycleResponse status =
        "{\"id\":\"resp_bench\",\"object\":\"response\","
            <> "\"created_at\":0,\"model\":\"gpt-5.6-luna\","
            <> "\"status\":\"" <> status <> "\","
            <> "\"output\":[],\"tools\":["
            <> namespaceTool "coding" 24_576
            <> ","
            <> namespaceTool "system" 8_192
            <> "],\"vendor\":{\"trace\":true}}"

    namespaceTool name payloadBytes =
        "{\"type\":\"namespace\",\"name\":\"" <> name
            <> "\",\"description\":\"benchmark namespace\","
            <> "\"tools\":[{\"type\":\"function\",\"name\":\"fixture\","
            <> "\"parameters\":{\"type\":\"object\",\"description\":\""
            <> BS.replicate payloadBytes 0x78
            <> "\"}}]}"

    itemFrame eventType sequenceNumber (outputIndex, item) =
        "{\"type\":\"" <> eventType
            <> "\",\"sequence_number\":" <> ascii sequenceNumber
            <> ",\"output_index\":" <> ascii outputIndex
            <> ",\"item\":" <> item <> "}"

    items status =
        [ (0, messageItem status)
        , (1, reasoningItem status)
        , (2, functionItem status)
        , (3, customItem status)
        ]

    messageItem status =
        "{\"type\":\"message\",\"id\":\"message_1\","
            <> "\"role\":\"assistant\",\"status\":\"" <> status
            <> "\",\"content\":[]}"

    reasoningItem status =
        "{\"type\":\"reasoning\",\"id\":\"reasoning_1\","
            <> "\"status\":\"" <> status <> "\",\"summary\":[]}"

    functionItem status =
        let arguments =
                if status == "in_progress" then "" else "{}"
        in
        "{\"type\":\"function_call\",\"id\":\"function_1\","
            <> "\"call_id\":\"function_1\",\"name\":\"fixture\","
            <> "\"status\":\"" <> status <> "\",\"arguments\":\""
            <> arguments <> "\"}"

    customItem status =
        let input =
                if status == "in_progress" then "" else "done"
        in
        "{\"type\":\"custom_tool_call\",\"id\":\"custom_1\","
            <> "\"call_id\":\"custom_1\",\"name\":\"fixture\","
            <> "\"status\":\"" <> status <> "\",\"input\":\""
            <> input <> "\"}"

    deltaFrame sequenceNumber index =
        let delta = BS.replicate deltaBytes 0x78
            common eventType itemId outputIndex =
                "{\"type\":\"" <> eventType
                    <> "\",\"sequence_number\":" <> ascii sequenceNumber
                    <> ",\"item_id\":\"" <> itemId
                    <> "\",\"output_index\":" <> ascii outputIndex
        in case index `mod` 10 of
            value
                | value < 5 ->
                    common
                        "response.reasoning_summary_text.delta"
                        "reasoning_1"
                        1
                        <> ",\"summary_index\":0,\"delta\":\""
                        <> delta
                        <> "\"}"
                | value < 8 ->
                    common
                        "response.output_text.delta"
                        "message_1"
                        0
                        <> ",\"content_index\":0,\"delta\":\""
                        <> delta
                        <> "\"}"
                | value == 8 ->
                    common
                        "response.function_call_arguments.delta"
                        "function_1"
                        2
                        <> ",\"delta\":\""
                        <> delta
                        <> "\"}"
                | otherwise ->
                    common
                        "response.custom_tool_call_input.delta"
                        "custom_1"
                        3
                        <> ",\"call_id\":\"custom_1\",\"delta\":\""
                        <> delta
                        <> "\"}"

varyRequest :: Int -> ResponseCreateParams -> ResponseCreateParams
varyRequest sampleIndex request@ResponseCreateParams{..} =
    ResponseCreateParams
        { input = case request.input of
            Just (ResponseInputText value) ->
                Just (ResponseInputText
                    (value <> Text.pack (show sampleIndex)))
            other -> other
        , ..
        }

ascii :: Int -> BS.ByteString
ascii = BS.pack . map (fromIntegral . fromEnum) . show

chunksOf :: Int -> BS.ByteString -> [BS.ByteString]
chunksOf size bytes
    | BS.null bytes = []
    | otherwise =
        let (chunk, rest) = BS.splitAt size bytes
        in chunk : chunksOf size rest

checksum :: BS.ByteString -> Int
checksum =
    BS.foldl' (\value byte -> value * 33 + fromIntegral byte) 5381

validate
    :: Workload
    -> [BS.ByteString]
    -> [BS.ByteString]
    -> ResponseCreateParams
    -> IO ()
validate workload chunks streamFrames request =
    case workload of
        DirectSse -> runDirectSse chunks >> pure ()
        AesonSse -> runAesonSse chunks >> pure ()
        DirectSseExtensions -> runDirectSse chunks >> pure ()
        AesonSseExtensions -> runAesonSse chunks >> pure ()
        DirectRequest ->
            either (error . show) (const (pure ())) $
                Decoder.decode responseCreateParamsDecoder
                    (Encoder.encode responseCreateParamsEncoder request)
        AesonRequest ->
            case Aeson.eitherDecodeStrict'
                (LBS.toStrict
                    (Aeson.encode (legacyRequestValue request)))
                :: Either String Aeson.Value of
                Left err -> error err
                Right _ -> pure ()
        PortableStream ->
            runTypedStream
                (pure . ResponsesCodec.decodeResponseStreamEvent)
                streamFrames
                >> pure ()
        HermesStream ->
            do
                portableEvents <-
                    decodeStreamEvents
                        (pure . ResponsesCodec.decodeResponseStreamEvent)
                        streamFrames
                hermesEvents <-
                    ResponsesCodec.withResponseStreamEventDecoder
                        \decodeEvent ->
                            decodeStreamEvents decodeEvent streamFrames
                if hermesEvents == portableEvents
                    then
                        ResponsesCodec.withResponseStreamEventDecoder
                            \decodeEvent ->
                                runTypedStream decodeEvent streamFrames
                                    >> pure ()
                    else error "Hermes stream differs from portable"

decodeStreamEvents
    :: (BS.ByteString -> IO (Either String ResponseStreamEvent))
    -> [BS.ByteString]
    -> IO [ResponseStreamEvent]
decodeStreamEvents decodeEvent =
    mapM \frame -> decodeEvent frame >>= either error pure

measure :: IO Int -> IO Sample
measure action = do
    performGC
    beforeStats <- getRTSStats
    beforeCPU <- getCPUTime
    beforeWall <- getMonotonicTimeNSec
    result <- action
    result `seq` pure ()
    afterWall <- getMonotonicTimeNSec
    afterCPU <- getCPUTime
    afterStats <- getRTSStats
    pure Sample
        { wallMillis = fromIntegral (afterWall - beforeWall) / 1.0e6
        , cpuMillis = fromIntegral (afterCPU - beforeCPU) / 1.0e9
        , allocatedBytes =
            fromIntegral
                (afterStats.allocated_bytes - beforeStats.allocated_bytes)
        }
