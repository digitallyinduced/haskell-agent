module Main (main) where

import Claude.Agent.SDK.Internal.Transport.OutputBuffer
    ( OutputLine(..)
    , appendOutputChunk
    , emptyOutputBuffer
    , takeOutputLine
    )
import Control.Exception (evaluate)
import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString8
import Data.List (foldl', sort)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
    ( RTSStats(..)
    , getRTSStats
    , getRTSStatsEnabled
    )
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (die)
import System.Mem (performGC)
import Text.Printf (printf)
import Prelude hiding (foldl')

data Implementation
    = Legacy
    | Chunked

data Workload
    = LongRecords
    | SharedStream

data Sample = Sample
    { wallMillis :: !Double
    , cpuMillis :: !Double
    , allocatedBytes :: !Integer
    }

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    if enabled
        then pure ()
        else die "RTS statistics are disabled; run with +RTS -T"
    getArgs >>= \case
        [implementationArg, sizeArg, chunkSizeArg, recordsArg, samplesArg] -> do
            (implementation, workload) <-
                parseImplementation implementationArg
            size <- parsePositive "record size" sizeArg
            chunkSize <- parsePositive "chunk size" chunkSizeArg
            recordCount <- parsePositive "record count" recordsArg
            sampleCount <- parsePositive "sample count" samplesArg
            let payloads =
                    [ makePayload size seed
                    | seed <- [1 .. recordCount]
                    ]
            _ <-
                evaluate $
                    sum (map ByteString.length payloads)
            samples <-
                mapM
                    (\sampleIndex ->
                        measure
                            (run
                                implementation
                                workload
                                chunkSize
                                sampleIndex
                                payloads))
                    [1 .. sampleCount]
            let result = median samples
            printf
                "%s,%d,%d,%d,%.3f,%.3f,%d\n"
                implementationArg
                size
                chunkSize
                recordCount
                result.wallMillis
                result.cpuMillis
                result.allocatedBytes
        _ ->
            die $
                "usage: output-buffer-bench IMPLEMENTATION SIZE CHUNK_SIZE "
                    <> "RECORDS SAMPLES\n"
                    <> "implementations: legacy, chunked, "
                    <> "legacy-shared, chunked-shared"

parseImplementation :: String -> IO (Implementation, Workload)
parseImplementation = \case
    "legacy" -> pure (Legacy, LongRecords)
    "chunked" -> pure (Chunked, LongRecords)
    "legacy-shared" -> pure (Legacy, SharedStream)
    "chunked-shared" -> pure (Chunked, SharedStream)
    other -> die ("unknown implementation: " <> other)

parsePositive :: String -> String -> IO Int
parsePositive label raw =
    case reads raw of
        [(value, "")]
            | value > 0 -> pure value
        _ -> die ("invalid " <> label <> ": " <> raw)

run
    :: Implementation
    -> Workload
    -> Int
    -> Int
    -> [ByteString]
    -> IO Int
run implementation workload chunkSize sampleIndex payloads =
    evaluate $
        case workload of
            LongRecords ->
                sampleIndex
                    + sum
                        [ consume
                            implementation
                            (varyChunkSize
                                chunkSize
                                (sampleSeed + recordIndex))
                            payload
                        | (recordIndex, payload) <-
                            zip [1 ..] payloads
                        ]
            SharedStream ->
                let expected =
                        map ByteString.init payloads
                    stream = ByteString.concat payloads
                    actual =
                        consumeShared
                            implementation
                            (varyChunkSize chunkSize sampleSeed)
                            stream
                 in if actual == expected
                        then
                            sampleIndex + sum (map checksum actual)
                        else
                            error
                                "shared stream records changed order or content"
  where
    sampleSeed = sampleIndex * 1_000_003

consume :: Implementation -> Int -> ByteString -> Int
consume implementation chunkSize payload =
    checksum $
        case implementation of
            Legacy ->
                legacyLine (chunksOf chunkSize payload)
            Chunked ->
                chunkedLine (chunksOf chunkSize payload)

-- Simulates the former readBoundedLine loop: scan the entire accumulated
-- strict ByteString, append the next read, then scan from the beginning again.
legacyLine :: [ByteString] -> ByteString
legacyLine = go ByteString.empty
  where
    go buffered chunks =
        case ByteString8.elemIndex '\n' buffered of
            Just newlineIndex ->
                ByteString.take newlineIndex buffered
            Nothing ->
                case chunks of
                    [] -> buffered
                    chunk : remaining ->
                        go (buffered <> chunk) remaining

chunkedLine :: [ByteString] -> ByteString
chunkedLine chunks =
    case takeOutputLine maxBound $
        foldl' appendOutputChunk emptyOutputBuffer chunks of
        Just (OutputLine line _) ->
            line
        Just (OutputLineTooLarge _) ->
            error "maxBound-sized output line"
        Nothing ->
            error "benchmark payload did not end in a newline"

consumeShared
    :: Implementation
    -> Int
    -> ByteString
    -> [ByteString]
consumeShared implementation chunkSize stream =
    case implementation of
        Legacy ->
            legacyLines (chunksOf chunkSize stream)
        Chunked ->
            chunkedLines (chunksOf chunkSize stream)

legacyLines :: [ByteString] -> [ByteString]
legacyLines = go ByteString.empty []
  where
    go buffered reversed = \case
        chunks
            | Just newlineIndex <-
                ByteString8.elemIndex '\n' buffered ->
                let (line, withNewline) =
                        ByteString.splitAt newlineIndex buffered
                 in go
                        (ByteString.drop 1 withNewline)
                        (line : reversed)
                        chunks
        [] ->
            if ByteString.null buffered
                then reverse reversed
                else reverse (buffered : reversed)
        chunk : remaining ->
            go (buffered <> chunk) reversed remaining

chunkedLines :: [ByteString] -> [ByteString]
chunkedLines = go emptyOutputBuffer []
  where
    go buffered reversed chunks =
        case takeOutputLine maxBound buffered of
            Just (OutputLine line remaining) ->
                go remaining (line : reversed) chunks
            Just (OutputLineTooLarge _) ->
                error "maxBound-sized output line"
            Nothing ->
                case chunks of
                    [] -> reverse reversed
                    chunk : remaining ->
                        go
                            (appendOutputChunk buffered chunk)
                            reversed
                            remaining

chunksOf :: Int -> ByteString -> [ByteString]
chunksOf chunkSize bytes
    | ByteString.null bytes = []
    | otherwise =
        let (chunk, remaining) =
                ByteString.splitAt chunkSize bytes
         in chunk : chunksOf chunkSize remaining

varyChunkSize :: Int -> Int -> Int
varyChunkSize base seed =
    max 1 (base - seed `mod` max 1 (base `div` 8))

checksum :: ByteString -> Int
checksum bytes
    | ByteString.null bytes = 0
    | otherwise =
        ByteString.length bytes
            + fromIntegral (ByteString.head bytes)
            + fromIntegral (ByteString.last bytes)

{-# NOINLINE makePayload #-}
makePayload :: Int -> Int -> ByteString
makePayload size seed =
    ByteString.cons
        (fromIntegral (97 + seed `mod` 23))
        (ByteString.replicate (size - 1) 120)
        <> "\n"

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
    performGC
    afterStats <- getRTSStats
    pure Sample
        { wallMillis =
            fromIntegral (afterWall - beforeWall) / 1_000_000
        , cpuMillis =
            fromIntegral (afterCPU - beforeCPU) / 1_000_000_000
        , allocatedBytes =
            fromIntegral
                ( afterStats.allocated_bytes
                    - beforeStats.allocated_bytes
                )
        }

median :: [Sample] -> Sample
median samples =
    Sample
        { wallMillis = middle (sort (map (.wallMillis) samples))
        , cpuMillis = middle (sort (map (.cpuMillis) samples))
        , allocatedBytes = middle (sort (map (.allocatedBytes) samples))
        }
  where
    middle values = values !! (length values `div` 2)
