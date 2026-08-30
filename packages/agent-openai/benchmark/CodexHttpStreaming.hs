{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Agent.OpenAI.Client (readCodexSseChunks)
import Agent.OpenAI.Http (decodeCodexHttpBodyBytes)
import Agent.Responses.Types (Response(..))
import Control.Exception (evaluate)
import Control.Monad (forM)
import qualified Data.ByteString as BS
import Data.IORef (atomicModifyIORef', newIORef)
import Data.List (sort)
import qualified Data.Text as Text
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
    , checksum :: !Int
    }

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    if enabled then pure () else die "run with +RTS -T"
    getArgs >>= \case
        [mode, eventArg, payloadArg, sampleArg]
            | mode `elem` ["buffered", "incremental"] -> do
                eventCount <- positive "event count" eventArg
                payloadBytes <- positive "payload bytes" payloadArg
                sampleCount <- positive "sample count" sampleArg
                let chunks = streamChunks eventCount payloadBytes
                    !inputBytes = sum (map BS.length chunks)
                    action = case mode of
                        "buffered" -> runBuffered chunks
                        _ -> runIncremental chunks
                samples <- forM [1 .. sampleCount] \_ -> measure action
                report mode eventCount payloadBytes inputBytes samples
        _ -> die
            "usage: codex-http-streaming-bench \
            \(buffered|incremental) EVENTS PAYLOAD_BYTES SAMPLES"

positive :: String -> String -> IO Int
positive label raw = case reads raw of
    [(value, "")] | value > 0 -> pure value
    _ -> die ("invalid " <> label <> ": " <> raw)

runBuffered :: [BS.ByteString] -> IO Int
runBuffered chunks =
    case decodeCodexHttpBodyBytes (BS.concat chunks) of
        Left err -> error (show err)
        Right response -> evaluate (responseChecksum response)

runIncremental :: [BS.ByteString] -> IO Int
runIncremental chunks = do
    source <- newIORef chunks
    let readChunk = atomicModifyIORef' source \case
            [] -> ([], Nothing)
            chunk : rest -> (rest, Just chunk)
    readCodexSseChunks maxBound Nothing readChunk [] >>= \case
        Left err -> error (show err)
        Right response -> evaluate (responseChecksum response)

streamChunks :: Int -> Int -> [BS.ByteString]
streamChunks eventCount payloadBytes =
    replicate eventCount deltaEvent <> [completedEvent]
  where
    deltaEvent = BS.concat
        [ "event: response.output_text.delta\ndata: {\"type\":\
          \\"response.output_text.delta\",\"item_id\":\"msg-1\",\
          \\"output_index\":0,\"content_index\":0,\"delta\":\""
        , BS.replicate payloadBytes 120
        , "\"}\n\n"
        ]
    completedEvent =
        "event: response.completed\ndata: {\"type\":\"response.completed\",\
        \\"response\":{\"id\":\"resp-bench\",\"created_at\":0,\
        \\"model\":\"gpt-bench\",\"status\":\"completed\",\"output\":[]}}\n\n"

responseChecksum :: Response -> Int
responseChecksum response =
    Text.length response.responseId
        + Text.length response.model
        + length response.output

measure :: IO Int -> IO Sample
measure action = do
    performGC
    beforeStats <- getRTSStats
    beforeCPU <- getCPUTime
    beforeWall <- getMonotonicTimeNSec
    result <- action >>= evaluate
    afterWall <- getMonotonicTimeNSec
    afterCPU <- getCPUTime
    performGC
    afterStats <- getRTSStats
    pure Sample
        { wallMillis = fromIntegral (afterWall - beforeWall) / 1.0e6
        , cpuMillis = fromIntegral (afterCPU - beforeCPU) / 1.0e9
        , allocatedBytes = fromIntegral
            (afterStats.allocated_bytes - beforeStats.allocated_bytes)
        , checksum = result
        }

report :: String -> Int -> Int -> Int -> [Sample] -> IO ()
report mode eventCount payloadBytes inputBytes samples =
    printf "%s,%d,%d,%d,%d,%.3f,%.3f,%d,%d\n"
        mode eventCount payloadBytes inputBytes (length samples)
        (median (map (.wallMillis) samples))
        (median (map (.cpuMillis) samples))
        (median (map (.allocatedBytes) samples))
        (median (map (.checksum) samples))

median :: Ord value => [value] -> value
median values = sort values !! (length values `div` 2)
