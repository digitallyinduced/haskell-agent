{-# LANGUAGE BangPatterns #-}

module Main (main) where

import qualified Agent.Responses.Codec as Codec
import Agent.Responses.Types
import Control.Exception (evaluate)
import Control.Monad (forM)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List (sortOn)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (die)
import System.Mem (performGC)
import Text.Printf (printf)

data Workload = Utf8RoundTrip | DirectBytes

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
                    [ makePayload index deltaBytes
                    | index <- [1 .. eventCount]
                    ]
            _ <- evaluate (sum (map BS.length payloads))
            samples <- forM [1 .. sampleCount] \_ ->
                measure (runWorkload workload payloads)
            let middle = sortOn (.wallMillis) samples
                    !! (length samples `div` 2)
            printf
                "%s,%d,%d,%d,%.3f,%.3f,%d\n"
                workloadArg eventCount deltaBytes sampleCount
                middle.wallMillis middle.cpuMillis middle.allocatedBytes
        _ -> die $
            "usage: responses-json-bench WORKLOAD EVENTS DELTA_BYTES SAMPLES\n"
                <> "workloads: utf8-round-trip, direct-bytes"

parseWorkload :: String -> IO Workload
parseWorkload = \case
    "utf8-round-trip" -> pure Utf8RoundTrip
    "direct-bytes" -> pure DirectBytes
    other -> die ("unknown workload: " <> other)

positive :: String -> String -> IO Int
positive label raw = case reads raw of
    [(value, "")] | value > 0 -> pure value
    _ -> die ("invalid " <> label <> ": " <> raw)

runWorkload :: Workload -> [BS.ByteString] -> IO Int
runWorkload workload = go checksumSeed
  where
    go !checksum [] = pure checksum
    go !checksum (payload : rest) = do
        event <- evaluate $ decodePayload $ case workload of
            Utf8RoundTrip ->
                TextEncoding.encodeUtf8 (TextEncoding.decodeUtf8 payload)
            DirectBytes -> payload
        let checksum' =
                Text.foldl'
                    (\current character ->
                        current * 33 + fromEnum character)
                    (checksum
                        + maybe 0 id
                            (responseStreamEventSequenceNumber event))
                    (streamEventTypeText (responseStreamEventType event))
        checksum' `seq` go checksum' rest

decodePayload :: BS.ByteString -> ResponseStreamEvent
decodePayload payload =
    case Aeson.eitherDecodeStrict' payload of
        Left err -> error err
        Right value ->
            case Codec.decodeResponseStreamEventValue value of
                Aeson.Error err -> error err
                Aeson.Success event -> event

makePayload :: Int -> Int -> BS.ByteString
makePayload sequenceNumber deltaBytes =
    BS.concat
        [ "{\"type\":\"response.output_text.delta\""
        , ",\"sequence_number\":", BS8.pack (show sequenceNumber)
        , ",\"item_id\":\"msg-", BS8.pack (show sequenceNumber)
        , "\",\"output_index\":0,\"content_index\":0,\"delta\":\""
        , BS.replicate deltaBytes 120
        , "\"}"
        ]

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
