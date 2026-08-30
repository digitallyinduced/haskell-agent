{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Agent.OpenAI.Client (readCodexSseChunks)
import Agent.OpenAI.Http (decodeCodexHttpBodyBytes)
import Agent.Responses.SSE
    ( feedSseDecoder
    , newSseDecoder
    )
import Agent.Responses.Types
    ( FunctionCall(..)
    , Response(..)
    , ResponseItem(..)
    , ResponseStreamEvent(..)
    )
import Control.Exception (evaluate)
import Control.Monad (forM)
import qualified Data.ByteString as BS
import Data.IORef
    ( atomicModifyIORef'
    , newIORef
    , writeIORef
    )
import Data.List (sort)
import qualified Data.Text as Text
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (die)
import System.Mem (performGC)
import qualified System.Timeout
import Text.Printf (printf)

data Sample = Sample
    { wallMillis :: !Double
    , cpuMillis :: !Double
    , allocatedBytes :: !Integer
    , liveBytes :: !Integer
    , checksum :: !Int
    }

data WorkResult = WorkResult
    { response :: !Response
    , retainedWireBody :: !(Maybe BS.ByteString)
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
                let representative =
                        streamChunks eventCount payloadBytes 0
                    !inputBytes = sum (map BS.length representative)
                    run = case mode of
                        "buffered" -> runBuffered
                        _ -> runIncremental
                samples <- forM [1 .. sampleCount] \sampleIndex -> do
                    let chunks =
                            streamChunks eventCount payloadBytes sampleIndex
                    -- Each sample gets a distinct SSE comment so the pure
                    -- buffered decoder cannot be floated out and shared.
                    !_ <- evaluate (sum (map BS.length chunks))
                    measure (run chunks)
                report mode eventCount payloadBytes inputBytes samples
        _ -> die
            "usage: codex-http-streaming-bench \
            \(buffered|incremental) EVENTS PAYLOAD_BYTES SAMPLES"

positive :: String -> String -> IO Int
positive label raw = case reads raw of
    [(value, "")] | value > 0 -> pure value
    _ -> die ("invalid " <> label <> ": " <> raw)

runBuffered :: [BS.ByteString] -> IO WorkResult
runBuffered chunks = do
    source <- newIORef chunks
    let readChunk = atomicModifyIORef' source \case
            [] -> ([], Nothing)
            chunk : rest -> (rest, Just chunk)
        readBody decoder reversed = do
            System.Timeout.timeout idleTimeoutMicros readChunk >>= \case
                Nothing -> error "buffered benchmark source timed out"
                Just Nothing -> pure (BS.concat (reverse reversed))
                Just (Just chunk) ->
                    case feedSseDecoder decoder chunk of
                        Left err -> error (show err)
                        Right (nextDecoder, events)
                            | any isTerminalEvent events ->
                                pure (BS.concat (reverse (chunk : reversed)))
                            | otherwise ->
                                readBody nextDecoder (chunk : reversed)
    body <- readBody newSseDecoder []
    !_ <- evaluate (BS.length body)
    case decodeCodexHttpBodyBytes body of
        Left err -> error (show err)
        Right response -> pure WorkResult
            { response
            -- The historical transport held the complete wire body while
            -- parsing the terminal response. Retain it through the measured
            -- GC so the benchmark observes that overlap rather than only a
            -- short-lived allocation high-water mark.
            , retainedWireBody = Just body
            }

runIncremental :: [BS.ByteString] -> IO WorkResult
runIncremental chunks = do
    source <- newIORef chunks
    let readChunk = atomicModifyIORef' source \case
            [] -> ([], Nothing)
            chunk : rest -> (rest, Just chunk)
    readCodexSseChunks idleTimeoutMicros Nothing readChunk [] >>= \case
        Left err -> error (show err)
        Right response -> pure WorkResult
            { response
            , retainedWireBody = Nothing
            }

idleTimeoutMicros :: Int
idleTimeoutMicros = 300 * 1_000_000

isTerminalEvent :: ResponseStreamEvent -> Bool
isTerminalEvent = \case
    ResponseCompletedEvent{} -> True
    ResponseDoneEvent{} -> True
    ResponseIncompleteEvent{} -> True
    ResponseFailedEvent{} -> True
    ResponseErrorEvent{} -> True
    ResponseNestedErrorEvent{} -> True
    _ -> False

streamChunks :: Int -> Int -> Int -> [BS.ByteString]
streamChunks eventCount payloadBytes sampleIndex =
    chunkWire 32768 $
        BS.concat
            ( [sampleComment, addedEvent]
                <> replicate eventCount deltaEvent
                <> [doneEvent, completedEvent]
            )
  where
    sampleComment =
        ": benchmark sample " <> BS.pack (map (fromIntegral . fromEnum)
            (show sampleIndex)) <> "\n"
    addedEvent =
        "event: response.output_item.added\ndata: {\"type\":\
        \\"response.output_item.added\",\"output_index\":0,\"item\":{\
        \\"type\":\"function_call\",\"id\":\"fc-1\",\"call_id\":\"call-1\",\
        \\"name\":\"bench\",\"arguments\":\"\"}}\n\n"
    deltaEvent = BS.concat
        [ "event: response.function_call_arguments.delta\ndata: {\"type\":\
          \\"response.function_call_arguments.delta\",\"item_id\":\"fc-1\",\
          \\"output_index\":0,\"delta\":\""
        , BS.replicate payloadBytes 120
        , "\"}\n\n"
        ]
    doneEvent =
        "event: response.output_item.done\ndata: {\"type\":\
        \\"response.output_item.done\",\"output_index\":0,\"item\":{\
        \\"type\":\"function_call\",\"id\":\"fc-1\",\"call_id\":\"call-1\",\
        \\"name\":\"bench\",\"arguments\":\"\"}}\n\n"
    completedEvent =
        "event: response.completed\ndata: {\"type\":\"response.completed\",\
        \\"response\":{\"id\":\"resp-bench\",\"created_at\":0,\
        \\"model\":\"gpt-bench\",\"status\":\"completed\",\"output\":[]}}\n\n"

chunkWire :: Int -> BS.ByteString -> [BS.ByteString]
chunkWire size bytes
    | BS.null bytes = []
    | otherwise =
        let (chunk, rest) = BS.splitAt size bytes
        in BS.copy chunk : chunkWire size rest

responseChecksum :: Response -> Int
responseChecksum response =
    Text.length response.responseId
        + Text.length response.model
        + length response.output
        + sum
            [ Text.length call.arguments
            | FunctionCallItem call <- response.output
            ]

measure :: IO WorkResult -> IO Sample
measure action = do
    performGC
    beforeStats <- getRTSStats
    beforeCPU <- getCPUTime
    beforeWall <- getMonotonicTimeNSec
    workResult <- action
    _ <- evaluate (maybe 0 BS.length workResult.retainedWireBody)
    result <- evaluate (responseChecksum workResult.response)
    afterWall <- getMonotonicTimeNSec
    afterCPU <- getCPUTime
    retained <- newIORef (Just workResult)
    performGC
    afterStats <- getRTSStats
    writeIORef retained Nothing
    pure Sample
        { wallMillis = fromIntegral (afterWall - beforeWall) / 1.0e6
        , cpuMillis = fromIntegral (afterCPU - beforeCPU) / 1.0e9
        , allocatedBytes = fromIntegral
            (afterStats.allocated_bytes - beforeStats.allocated_bytes)
        , liveBytes =
            fromIntegral afterStats.gc.gcdetails_live_bytes
        , checksum = result
        }

report :: String -> Int -> Int -> Int -> [Sample] -> IO ()
report mode eventCount payloadBytes inputBytes samples =
    printf "%s,%d,%d,%d,%d,%.3f,%.3f,%d,%d,%d\n"
        mode eventCount payloadBytes inputBytes (length samples)
        (median (map (.wallMillis) samples))
        (median (map (.cpuMillis) samples))
        (median (map (.allocatedBytes) samples))
        (median (map (.liveBytes) samples))
        (median (map (.checksum) samples))

median :: Ord value => [value] -> value
median values = sort values !! (length values `div` 2)
