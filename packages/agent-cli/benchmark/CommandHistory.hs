module Main (main) where

import Agent.CLI.Input.History (appendReplHistoryAt, readReplHistoryAt)
import Agent.PrivateFileLock (withPrivateFileLock)
import Control.Exception (evaluate)
import Control.Monad (replicateM, unless)
import Data.IORef (newIORef, readIORef)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (GCDetails(..), RTSStats(..), getRTSStats, getRTSStatsEnabled)
import qualified System.Console.Haskeline.History as Haskeline
import System.CPUTime (getCPUTime)
import System.Directory (copyFile)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Mem (performMajorGC)
import System.OsPath (unsafeEncodeUtf)
import System.Posix.Files (setFileMode)
import Text.Printf (printf)
import Text.Read (readMaybe)

-- Keep the original Haskeline representation alive as the old editor did.
data Retained = Original Haskeline.History [Text] | Compact [Text]

data Observation = Observation !Double !Double !Integer !Integer

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    unless enabled (die "enable RTS statistics with +RTS -T")
    arguments <- getArgs
    case arguments of
        [operation, countArgument, widthArgument, samplesArgument] -> do
            count <- positive countArgument
            width <- positive widthArgument
            samples <- positive samplesArgument
            withSystemTempDirectory "command-history-benchmark" \directory -> do
                let fixture = directory </> "fixture"
                    working = directory </> "history"
                    entries =
                        [ Text.pack (show index) <> Text.replicate width "x"
                        | index <- [1 .. count]
                        ]
                Haskeline.writeHistory fixture $
                    foldr (Haskeline.addHistory . Text.unpack) Haskeline.emptyHistory entries
                observations <- replicateM samples do
                    copyFile fixture working
                    measure (runOperation operation working)
                let elapsed = [value | Observation value _ _ _ <- observations]
                    cpu = [value | Observation _ value _ _ <- observations]
                    allocated = [value | Observation _ _ value _ <- observations]
                    live = [value | Observation _ _ _ value <- observations]
                printf "%s,%d,%d,%d,%.3f,%.3f,%d,%d\n"
                    operation count width samples (median elapsed) (median cpu)
                    (median allocated) (median live)
        _ -> die "usage: command-history-bench OPERATION ENTRIES WIDTH SAMPLES (+RTS -T); operations: original-read, compact-read, original-append, compact-append"

positive :: String -> IO Int
positive raw = case readMaybe raw of
    Just value | value > 0 -> pure value
    _ -> die "expected a positive integer"

median :: Ord a => [a] -> a
median values = sort values !! (length values `div` 2)

runOperation :: String -> FilePath -> IO Retained
runOperation operation path = case operation of
    "original-read" -> do
        history <- Haskeline.readHistory path
        pure (Original history (map Text.pack (Haskeline.historyLines history)))
    "compact-read" -> Compact <$> readReplHistoryAt path
    "original-append" -> do
        withPrivateFileLock (unsafeEncodeUtf (path <> ".lock")) do
            history <- Haskeline.readHistory path
            Haskeline.writeHistory path (Haskeline.addHistory "benchmark submission" history)
            setFileMode path 0o600
        pure (Compact [])
    "compact-append" -> do
        appendReplHistoryAt path "benchmark submission"
        pure (Compact [])
    _ -> die "unknown benchmark operation"

checksum :: Retained -> Int
checksum retained = case retained of
    Original history entries ->
        sum (map length (Haskeline.historyLines history)) + sum (map Text.length entries)
    Compact entries -> sum (map Text.length entries)

{-# NOINLINE measure #-}
measure :: IO Retained -> IO Observation
measure action = do
    performMajorGC
    before <- getRTSStats
    started <- getMonotonicTimeNSec
    cpuStarted <- getCPUTime
    result <- action
    retained <- newIORef result
    _ <- evaluate (checksum result)
    cpuFinished <- getCPUTime
    finished <- getMonotonicTimeNSec
    performMajorGC
    after <- getRTSStats
    -- The reference is read after collection, preventing dead-result elimination.
    _ <- readIORef retained >>= evaluate . checksum
    pure $ Observation
        (fromIntegral (finished - started) / 1e6)
        (fromIntegral (cpuFinished - cpuStarted) / 1e9)
        (toInteger after.allocated_bytes - toInteger before.allocated_bytes)
        (toInteger after.gc.gcdetails_live_bytes - toInteger before.gc.gcdetails_live_bytes)
