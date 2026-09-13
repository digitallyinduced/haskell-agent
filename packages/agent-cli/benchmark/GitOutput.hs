module Main (main) where

import Agent.TUI.TextWidth (displayTerminalText)
import Control.Exception (evaluate)
import Control.Monad (replicateM, unless)
import qualified Data.ByteString as ByteString
import Data.Char (ord)
import Data.IORef (newIORef, readIORef)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import Data.Text.Encoding.Error (lenientDecode)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (RTSStats(..), getRTSStats, getRTSStatsEnabled)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (die)
import System.Mem (performMajorGC)
import Text.Printf (printf)
import Text.Read (readMaybe)

-- Separate producer and consumer boundaries match GitCommandOutput crossing
-- runSafeGit's IO boundary. Prevent fusion from deleting the old representation.
data OriginalOutput = OriginalOutput !String
data TextOutput = TextOutput !Text

{-# NOINLINE originalCapture #-}
originalCapture :: ByteString.ByteString -> OriginalOutput
originalCapture = OriginalOutput . Text.unpack . Encoding.decodeUtf8With lenientDecode

{-# NOINLINE textCapture #-}
textCapture :: ByteString.ByteString -> TextOutput
textCapture = TextOutput . Encoding.decodeUtf8With lenientDecode

{-# NOINLINE originalConsume #-}
originalConsume :: OriginalOutput -> Text
originalConsume (OriginalOutput value) = Text.pack value

{-# NOINLINE textConsume #-}
textConsume :: TextOutput -> Text
textConsume (TextOutput value) = value

data Observation = Observation !Double !Double !Integer !Int

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    unless enabled (die "enable RTS statistics with +RTS -T")
    arguments <- getArgs
    case arguments of
        [operation, sizeArgument, sampleArgument] -> do
            count <- positive sizeArgument
            samples <- positive sampleArgument
            -- Mix ordinary diff text, Unicode, terminal controls and malformed
            -- UTF-8. Input construction is outside all measured intervals.
            let line = Encoding.encodeUtf8 "+value = \"café 日本 👩\x200d💻\"\t-- change\n"
                input = ByteString.concat (replicate count line)
                    <> ByteString.pack [255, 10]
            _ <- evaluate (ByteString.length input)
            reference <- newIORef input
            transform <- case operation of
                "original-output" -> pure (originalConsume . originalCapture)
                "text-output" -> pure (textConsume . textCapture)
                "original-render" -> pure (displayTerminalText . originalConsume . originalCapture)
                "text-render" -> pure (displayTerminalText . textConsume . textCapture)
                _ -> die "unknown operation"
            observations <- replicateM samples $
                measure (transform <$> readIORef reference)
            -- Check complete values, not only additive checksums. Keep this
            -- compatibility validation outside all measured intervals.
            let original = originalConsume (originalCapture input)
                replacement = textConsume (textCapture input)
            unless (original == replacement
                && displayTerminalText original == displayTerminalText replacement)
                (die "original and Text pipelines produced different output")
            let elapsed = [value | Observation value _ _ _ <- observations]
                cpu = [value | Observation _ value _ _ <- observations]
                allocated = [value | Observation _ _ value _ <- observations]
                checksums = [value | Observation _ _ _ value <- observations]
            checksum <- case checksums of
                first : rest
                    | all (== first) rest -> pure first
                    | otherwise -> die "unstable checksum"
                [] -> die "no benchmark observations"
            printf "%s,%d,%d,%d,%.3f,%.3f,%d,%d\n"
                operation count (ByteString.length input) samples
                (median elapsed) (median cpu) (median allocated) checksum
        _ -> die "usage: git-output-benchmark OPERATION FIXTURE_UNITS SAMPLES +RTS -T"

positive :: String -> IO Int
positive raw = case readMaybe raw of
    Just value | value > 0 -> pure value
    _ -> die "expected a positive integer"

median :: Ord a => [a] -> a
median values = sort values !! (length values `div` 2)

{-# NOINLINE measure #-}
measure :: IO Text -> IO Observation
measure action = do
    performMajorGC
    before <- getRTSStats
    cpuBefore <- getCPUTime
    timeBefore <- getMonotonicTimeNSec
    output <- action
    checksum <- evaluate (Text.foldl' (\total char -> total + ord char) 0 output)
    timeAfter <- getMonotonicTimeNSec
    cpuAfter <- getCPUTime
    -- Flush allocation counters; this collection is outside the timed interval.
    performMajorGC
    after <- getRTSStats
    pure (Observation
        (fromIntegral (timeAfter - timeBefore) / 1e6)
        (fromIntegral (cpuAfter - cpuBefore) / 1e9)
        (toInteger after.allocated_bytes - toInteger before.allocated_bytes)
        checksum)
