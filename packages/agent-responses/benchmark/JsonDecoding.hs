{-# LANGUAGE BangPatterns #-}

module Main (main) where

import qualified Agent.Responses.Codec as Codec
import Agent.Responses.Types
import Control.Exception (evaluate)
import Control.Monad (forM)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List (sort)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (die)
import System.Mem (performGC)
import Text.Printf (printf)

data Workload
    = Utf8RoundTrip
    | DirectBytes
    | CodingUtf8RoundTrip
    | CodingDirectBytes

data Sample = Sample
    { wallMillis :: !Double
    , cpuMillis :: !Double
    , allocatedBytes :: !Integer
    }

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    if enabled then pure () else die "run with +RTS -T"
    getArgs >>= \case
        [workloadArg, eventCountArg, deltaBytesArg, samplesArg] -> do
            workload <- parseWorkload workloadArg
            eventCount <- positive "event count" eventCountArg
            deltaBytes <- positive "delta bytes" deltaBytesArg
            sampleCount <- positive "sample count" samplesArg
            let payloads =
                    [ makePayload workload index deltaBytes
                    | index <- [1 .. eventCount]
                    ]
            _ <- evaluate (sum (map BS.length payloads))
            samples <- forM [1 .. sampleCount] \_ ->
                measure (runWorkload workload payloads)
            let medianWall = median (map (.wallMillis) samples)
                medianCPU = median (map (.cpuMillis) samples)
                medianAllocation = median (map (.allocatedBytes) samples)
            printf
                "%s,%d,%d,%d,%.3f,%.3f,%d\n"
                workloadArg eventCount deltaBytes sampleCount
                medianWall medianCPU medianAllocation
        _ -> die $
            "usage: responses-json-bench WORKLOAD EVENTS DELTA_BYTES SAMPLES\n"
                <> "workloads: utf8-round-trip, direct-bytes, "
                <> "coding-utf8-round-trip, coding-direct-bytes"

parseWorkload :: String -> IO Workload
parseWorkload = \case
    "utf8-round-trip" -> pure Utf8RoundTrip
    "direct-bytes" -> pure DirectBytes
    "coding-utf8-round-trip" -> pure CodingUtf8RoundTrip
    "coding-direct-bytes" -> pure CodingDirectBytes
    other -> die ("unknown workload: " <> other)

positive :: String -> String -> IO Int
positive label raw = case reads raw of
    [(value, "")] | value > 0 -> pure value
    _ -> die ("invalid " <> label <> ": " <> raw)

median :: Ord a => [a] -> a
median values = sort values !! (length values `div` 2)

runWorkload :: Workload -> [BS.ByteString] -> IO Int
runWorkload workload = go checksumSeed
  where
    go !checksum [] = pure checksum
    go !checksum (payload : rest) = do
        event <- evaluate $ decodePayload $ case workload of
            Utf8RoundTrip ->
                TextEncoding.encodeUtf8 (TextEncoding.decodeUtf8 payload)
            DirectBytes -> payload
            CodingUtf8RoundTrip ->
                TextEncoding.encodeUtf8 (TextEncoding.decodeUtf8 payload)
            CodingDirectBytes -> payload
        let checksum' =
                Text.foldl'
                    (\current character ->
                        current * 33 + fromEnum character)
                    (checksum
                        + maybe 0 id
                            (responseStreamEventSequenceNumber event)
                        + eventDeltaLength event)
                    (streamEventTypeText (responseStreamEventType event))
        checksum' `seq` go checksum' rest

decodePayload :: BS.ByteString -> ResponseStreamEvent
decodePayload payload =
    case Codec.decodeResponseStreamEvent payload of
        Left err -> error err
        Right event -> event

eventDeltaLength :: ResponseStreamEvent -> Int
eventDeltaLength = \case
    ResponseFunctionCallArgumentsDeltaEvent { delta } ->
        maybe 0 Text.length delta
    ResponseCustomToolInputDeltaEvent { delta } ->
        maybe 0 Text.length delta
    OtherResponseStreamEvent { eventExtraFields } ->
        case KeyMap.lookup "delta" eventExtraFields of
            Just (Aeson.String delta) -> Text.length delta
            _ -> 0
    _ -> 0

makePayload :: Workload -> Int -> Int -> BS.ByteString
makePayload workload sequenceNumber deltaBytes =
    case workload of
        Utf8RoundTrip -> makeTextDelta sequenceNumber deltaBytes
        DirectBytes -> makeTextDelta sequenceNumber deltaBytes
        CodingUtf8RoundTrip -> makeCodingDelta sequenceNumber deltaBytes
        CodingDirectBytes -> makeCodingDelta sequenceNumber deltaBytes

makeTextDelta :: Int -> Int -> BS.ByteString
makeTextDelta sequenceNumber deltaBytes =
    BS.concat
        [ "{\"type\":\"response.output_text.delta\""
        , ",\"sequence_number\":", BS8.pack (show sequenceNumber)
        , ",\"item_id\":\"msg-", BS8.pack (show sequenceNumber)
        , "\",\"output_index\":0,\"content_index\":0,\"delta\":\""
        , BS.replicate deltaBytes 120
        , "\"}"
        ]

-- A coding turn is mostly reasoning and assistant text, interspersed with
-- function-call arguments and custom tool input such as apply_patch.
makeCodingDelta :: Int -> Int -> BS.ByteString
makeCodingDelta sequenceNumber deltaBytes =
    BS.concat
        [ "{\"type\":\"", eventType, "\""
        , ",\"sequence_number\":", number
        , identityFields
        , ",\"delta\":\"", BS.replicate deltaBytes 120
        , "\"}"
        ]
  where
    number = BS8.pack (show sequenceNumber)
    (eventType, identityFields) =
        case sequenceNumber `mod` 10 of
            value
                | value < 5 ->
                    ( "response.reasoning_summary_text.delta"
                    , ",\"item_id\":\"rs-1\",\"summary_index\":0"
                    )
                | value < 8 ->
                    ( "response.output_text.delta"
                    , ",\"item_id\":\"msg-1\",\"output_index\":0,\
                      \\"content_index\":0"
                    )
                | value < 9 ->
                    ( "response.function_call_arguments.delta"
                    , ",\"item_id\":\"fc-1\",\"output_index\":1"
                    )
                | otherwise ->
                    ( "response.custom_tool_call_input.delta"
                    , ",\"item_id\":\"ctc-1\",\"call_id\":\"call-1\",\
                      \\"output_index\":1"
                    )

checksumSeed :: Int
checksumSeed = 5381

measure :: IO Int -> IO Sample
measure action = do
    performGC
    beforeStats <- getRTSStats
    beforeCPU <- getCPUTime
    beforeWall <- getMonotonicTimeNSec
    result <- action
    _ <- evaluate result
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
