module Main (main) where

import Agent.TextBuffer
    ( appendTextBuffer
    , emptyTextBuffer
    , textBufferToText
    )
-- 'evaluate' is required to keep pure work inside the measured IO interval;
-- safe-exceptions does not re-export it.
import Control.Exception (evaluate)
import Control.Monad (forM)
import Data.Char (chr, ord)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
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

data Workload
    = OldAccumulate
    | NewAccumulate
    | NoDuplicateBuffer
    | FullscreenBaseline
    deriving (Eq, Show)

data Sample = Sample
    { elapsedMillis :: !Double
    , allocatedBytes :: !Integer
    }

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    if not enabled
        then die "RTS statistics are disabled; run with +RTS -T"
        else pure ()
    getArgs >>= \case
        [workloadArg, chunkCountArg, chunkSizeArg, sampleCountArg] -> do
            workload <- parseWorkload workloadArg
            chunkCount <- parsePositive "chunk count" chunkCountArg
            chunkSize <- parsePositive "chunk size" chunkSizeArg
            sampleCount <- parsePositive "sample count" sampleCountArg
            samples <- forM [1 .. sampleCount] \sampleIndex -> do
                let character = chr (97 + sampleIndex `mod` 26)
                    chunk = Text.replicate chunkSize (Text.singleton character)
                    chunks = replicate chunkCount chunk
                _ <- evaluate (length chunks + checksumText chunk)
                measure (runWorkload workload chunks)
            let medianSample = median samples
            printf
                "%s,%d,%d,%.3f,%d\n"
                workloadArg
                chunkCount
                chunkSize
                medianSample.elapsedMillis
                medianSample.allocatedBytes
        _ ->
            die $
                "usage: streaming-text-bench WORKLOAD CHUNKS CHUNK_SIZE SAMPLES\n"
                    <> "workloads: old-accumulate, new-accumulate, "
                    <> "no-duplicate-buffer, fullscreen-baseline"

parseWorkload :: String -> IO Workload
parseWorkload = \case
    "old-accumulate" -> pure OldAccumulate
    "new-accumulate" -> pure NewAccumulate
    "no-duplicate-buffer" -> pure NoDuplicateBuffer
    "fullscreen-baseline" -> pure FullscreenBaseline
    other -> die ("unknown workload: " <> other)

parsePositive :: String -> String -> IO Int
parsePositive label raw =
    case reads raw of
        [(value, "")]
            | value > 0 -> pure value
        _ -> die ("invalid " <> label <> ": " <> raw)

runWorkload :: Workload -> [Text] -> IO Int
runWorkload workload chunks =
    evaluate $ case workload of
        OldAccumulate ->
            checksumText (foldl' (<>) "" chunks)
        NewAccumulate ->
            checksumText $
                textBufferToText $
                    foldl'
                        (flip appendTextBuffer)
                        emptyTextBuffer
                        chunks
        NoDuplicateBuffer ->
            foldl'
                (Text.foldl' checksumStep)
                checksumSeed
                chunks
        FullscreenBaseline ->
            oldFullscreen chunks

oldFullscreen :: [Text] -> Int
oldFullscreen =
    snd . foldl' step ("", 0)
  where
    step (buffer, checksum) chunk =
        let buffer' = buffer <> chunk
            checksum' = checksumText buffer'
        in checksum' `seq` (buffer', checksum + checksum')

checksumText :: Text -> Int
checksumText =
    Text.foldl' checksumStep checksumSeed

checksumSeed :: Int
checksumSeed = 5381

checksumStep :: Int -> Char -> Int
checksumStep checksum character =
    checksum * 33 + ord character

measure :: IO Int -> IO Sample
measure action = do
    performGC
    beforeStats <- getRTSStats
    beforeTime <- getCPUTime
    result <- action
    _ <- evaluate result
    afterTime <- getCPUTime
    performGC
    afterStats <- getRTSStats
    pure Sample
        { elapsedMillis =
            fromIntegral (afterTime - beforeTime) / 1.0e9
        , allocatedBytes =
            fromIntegral
                (afterStats.allocated_bytes - beforeStats.allocated_bytes)
        }

median :: [Sample] -> Sample
median samples =
    Sample
        { elapsedMillis = middle (sort (map (.elapsedMillis) samples))
        , allocatedBytes = middle (sort (map (.allocatedBytes) samples))
        }
  where
    middle values = values !! (length values `div` 2)
