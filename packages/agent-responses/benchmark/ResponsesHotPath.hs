{-# LANGUAGE BangPatterns #-}

module Main (main) where

import qualified Agent.Json.Decoder as Decoder
import qualified Agent.Json.Decoder.Hermes as Hermes
import qualified Agent.Json.Encoder as Encoder
import Agent.Error (ApiError)
import Agent.Responses.SSE
import qualified Agent.Responses.Hermes as ResponsesHermes
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
            validate workload chunks request
            samples <- forM [1 .. sampleCount] \sampleIndex ->
                measure
                    (runWorkload
                        workload
                        chunks
                        (varyRequest sampleIndex request))
            let median project =
                    sort (map project samples) !! (sampleCount `div` 2)
            printf
                "%s,%d,%d,%d,%.3f,%.3f,%d\n"
                workloadArg eventCount deltaBytes sampleCount
                (median (.wallMillis))
                (median (.cpuMillis))
                (median (.allocatedBytes))
        _ -> die $
            "usage: responses-hot-path WORKLOAD EVENTS DELTA_BYTES SAMPLES\n"
                <> "workloads: direct-sse, aeson-sse, "
                <> "direct-sse-extensions, aeson-sse-extensions, "
                <> "direct-request, aeson-request"

parseWorkload :: String -> IO Workload
parseWorkload = \case
    "direct-sse" -> pure DirectSse
    "aeson-sse" -> pure AesonSse
    "direct-sse-extensions" -> pure DirectSseExtensions
    "aeson-sse-extensions" -> pure AesonSseExtensions
    "direct-request" -> pure DirectRequest
    "aeson-request" -> pure AesonRequest
    value -> die ("unknown workload: " <> value)

positive :: String -> String -> IO Int
positive label raw = case reads raw of
    [(value, "")] | value > 0 -> pure value
    _ -> die ("invalid " <> label <> ": " <> raw)

runWorkload
    :: Workload
    -> [BS.ByteString]
    -> ResponseCreateParams
    -> IO Int
runWorkload workload chunks request =
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
                        ResponsesHermes.textDeltaEventDecoder
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
        current
            + maybe 0 id sequenceNumber
            + maybe 0 Text.length delta
            + maybe 0 Text.length streamItemId
            + maybe 0 id streamOutputIndex
            + maybe 0 id contentIndex
    event ->
        current
            + maybe 0 id (responseStreamEventSequenceNumber event)

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

varyRequest :: Int -> ResponseCreateParams -> ResponseCreateParams
varyRequest sampleIndex request =
    request
        { input = case request.input of
            Just (ResponseInputText value) ->
                Just (ResponseInputText
                    (value <> Text.pack (show sampleIndex)))
            other -> other
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
    -> ResponseCreateParams
    -> IO ()
validate workload chunks request =
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
