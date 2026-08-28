-- | Compare the chunked stdout record reader against the strict-append
-- reader it replaced. Both run the same IO loop over the same in-memory
-- stream, delivered in pipe-sized reads, with the production record cap.
--
-- Usage: output-buffer-bench IMPLEMENTATION RECORD_BYTES READ_BYTES RECORDS SAMPLES +RTS -T
module Main (main) where

import Claude.Agent.SDK.Internal.Transport.OutputBuffer
    ( OutputReadResult(..)
    , emptyOutputBuffer
    , readOutputLine
    )
import Control.Exception (evaluate)
import Control.Exception.Safe (IOException, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString8
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
import Data.List (sort)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
    ( RTSStats(..)
    , getRTSStats
    , getRTSStatsEnabled
    )
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (die)
import System.IO.Error (isEOFError)
import System.Mem (performGC)
import Text.Printf (printf)

data Implementation
    = Legacy
    | Chunked

data Sample = Sample
    { wallMillis :: !Double
    , cpuMillis :: !Double
    , allocatedBytes :: !Integer
    }

-- | The production default for 'maxBufferSizeBytes'.
recordLimit :: Int
recordLimit = 1_073_741_824

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    if enabled
        then pure ()
        else die "RTS statistics are disabled; run with +RTS -T"
    getArgs >>= \case
        [implementationArg, sizeArg, readArg, recordsArg, samplesArg] -> do
            implementation <- parseImplementation implementationArg
            recordBytes <- parsePositive "record size" sizeArg
            readBytes <- parsePositive "read size" readArg
            recordCount <- parsePositive "record count" recordsArg
            sampleCount <- parsePositive "sample count" samplesArg
            if recordBytes < 2
                then die "record size must be at least 2"
                else pure ()
            let stream = makeStream recordBytes recordCount
            _ <- evaluate (ByteString.length stream)
            samples <-
                mapM
                    (\_ ->
                        measure
                            (runSample
                                implementation
                                readBytes
                                recordCount
                                stream))
                    [1 .. sampleCount]
            let result = median samples
            printf
                "%s,%d,%d,%d,%.3f,%.3f,%d\n"
                implementationArg
                recordBytes
                readBytes
                recordCount
                result.wallMillis
                result.cpuMillis
                result.allocatedBytes
        _ ->
            die $
                "usage: output-buffer-bench IMPLEMENTATION RECORD_BYTES "
                    <> "READ_BYTES RECORDS SAMPLES\n"
                    <> "implementations: legacy, chunked"

parseImplementation :: String -> IO Implementation
parseImplementation = \case
    "legacy" -> pure Legacy
    "chunked" -> pure Chunked
    other -> die ("unknown implementation: " <> other)

parsePositive :: String -> String -> IO Int
parsePositive label raw =
    case reads raw of
        [(value, "")]
            | value > 0 -> pure value
        _ -> die ("invalid " <> label <> ": " <> raw)

-- | @recordCount@ newline-terminated records of @recordBytes@ bytes each,
-- with a varying first byte so records are distinguishable.
makeStream :: Int -> Int -> ByteString
makeStream recordBytes recordCount =
    ByteString.concat
        [ ByteString.cons
            (fromIntegral (97 + seed `mod` 23))
            (ByteString.replicate (recordBytes - 2) 120)
            <> "\n"
        | seed <- [1 .. recordCount]
        ]

-- | Read every record from @stream@, delivered in reads of at most
-- @readBytes@, and return a checksum that forces each record.
runSample :: Implementation -> Int -> Int -> ByteString -> IO Int
runSample implementation readBytes expectedRecords stream = do
    source <- newIORef stream
    let readChunk size =
            atomicModifyIORef' source \remaining ->
                let (chunk, rest) =
                        ByteString.splitAt (min size readBytes) remaining
                 in (rest, chunk)
    readLine <-
        case implementation of
            Legacy -> do
                bufferRef <- newIORef ByteString.empty
                pure (legacyReadLine bufferRef recordLimit readChunk)
            Chunked -> do
                bufferRef <- newIORef emptyOutputBuffer
                pure (readOutputLine bufferRef recordLimit readChunk)
    let loop !count !acc =
            readLine >>= \case
                OutputReadLine line ->
                    loop (count + 1) (acc + checksum line)
                OutputReadEnd
                    | count == expectedRecords ->
                        pure acc
                    | otherwise ->
                        die $
                            "read "
                                <> show count
                                <> " records, expected "
                                <> show expectedRecords
                OutputReadTooLarge ->
                    die "unexpected oversized record"
                OutputReadFailure exception ->
                    die (show exception)
    loop (0 :: Int) 0

checksum :: ByteString -> Int
checksum bytes =
    ByteString.length bytes
        + fromIntegral (ByteString.head bytes)
        + fromIntegral (ByteString.last bytes)

-- | The strict-append reader this benchmark's baseline replaces: every read
-- appends to the accumulated record and rescans it from the start, so a
-- record spanning many reads costs quadratic copying.
legacyReadLine
    :: IORef ByteString
    -> Int
    -> (Int -> IO ByteString)
    -> IO OutputReadResult
legacyReadLine bufferRef maximumBytes readChunk =
    go
  where
    go = do
        buffered <- readIORef bufferRef
        case ByteString8.elemIndex '\n' buffered of
            Just newlineIndex -> do
                let (line, withNewline) =
                        ByteString.splitAt newlineIndex buffered
                writeIORef bufferRef (ByteString.drop 1 withNewline)
                pure
                    if ByteString.length line > maximumBytes
                        then OutputReadTooLarge
                        else OutputReadLine line
            Nothing
                | ByteString.length buffered > maximumBytes ->
                    pure OutputReadTooLarge
                | otherwise -> do
                    let readSize =
                            min
                                8_192
                                (maximumBytes - ByteString.length buffered + 1)
                    result <-
                        try (readChunk readSize)
                            :: IO (Either IOException ByteString)
                    case result of
                        Right chunk
                            | ByteString.null chunk ->
                                finish buffered
                            | otherwise -> do
                                writeIORef bufferRef (buffered <> chunk)
                                go
                        Left exception
                            | isEOFError exception ->
                                finish buffered
                            | otherwise ->
                                pure (OutputReadFailure exception)

    finish buffered = do
        writeIORef bufferRef ByteString.empty
        pure
            if ByteString.null buffered
                then OutputReadEnd
                else OutputReadLine buffered

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
    pure
        Sample
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
