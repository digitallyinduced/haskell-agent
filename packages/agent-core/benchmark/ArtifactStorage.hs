{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Agent.ToolDispatch
import Agent.Tools.OutputArtifact
import Agent.Tools.Types
import Control.Exception (evaluate)
import Control.Exception.Safe (bracket)
import Control.Monad (forM, forM_, unless)
import qualified Data.ByteString as BS
import Data.List (find, sort)
import qualified Data.Text as Text
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
import System.CPUTime (getCPUTime)
import System.Directory
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (hClose, openBinaryTempFile)
import System.Mem (performGC)
import System.OsPath (unsafeEncodeUtf)
import Text.Printf (printf)

-- Measures the entire storage-and-query operation, including copying resident
-- bytes, rather than moving write costs outside the measured region.
main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    unless enabled (die "Run with +RTS -T")
    arguments <- getArgs
    let samples = case arguments of
            [value] -> read value
            _ -> 7
    unless (samples > 0) (die "Sample count must be positive")
    putStrLn "mode,kib,operation,median_elapsed_ms,median_cpu_ms,median_allocated_bytes"
    forM_ [64, 256, 1024, 2048 :: Int] \size -> do
        let payload = BS.replicate (size * 1024 - 6) 97 <> "needle"
        _ <- evaluate (BS.foldl' (\acc byte -> acc + fromIntegral byte) (0 :: Int) payload)
        forM_ ["read_tool_output", "search_tool_output"] \operation -> do
            validation <- forM [False, True] \resident ->
                withEnvironment resident \env -> do
                    artifact <- writeOutputArtifactDetailed env payload
                        >>= either (die . Text.unpack) pure
                    query env operation artifact.artifactHandle
            unless (head validation == last validation)
                (die "Memory and disk query results differ")
            forM_ [False, True] \resident -> do
                observations <- forM [1 .. samples] \_ ->
                    withEnvironment resident \env -> do
                        performGC
                        before <- getRTSStats
                        cpuStart <- getCPUTime
                        elapsedStart <- getMonotonicTimeNSec
                        artifact <- writeOutputArtifactDetailed env payload
                            >>= either (die . Text.unpack) pure
                        result <- query env operation artifact.artifactHandle
                        checksum <- evaluate $
                            Text.foldl' (\acc character -> acc + fromEnum character) (0 :: Int) result
                        unless (checksum > 0) (die "Empty query result")
                        elapsedEnd <- getMonotonicTimeNSec
                        cpuEnd <- getCPUTime
                        performGC
                        after <- getRTSStats
                        pure
                            ( fromIntegral (elapsedEnd - elapsedStart) / 1e6 :: Double
                            , fromIntegral (cpuEnd - cpuStart) / 1e9 :: Double
                            , allocated_bytes after - allocated_bytes before
                            )
                printf "%s,%d,%s,%.4f,%.4f,%d\n"
                    (if resident then "memory-eligible" else "disk-baseline" :: String)
                    size (Text.unpack operation)
                    (median [a | (a, _, _) <- observations])
                    (median [b | (_, b, _) <- observations])
                    (median [c | (_, _, c) <- observations])

median :: Ord a => [a] -> a
median values = sort values !! (length values `div` 2)

withEnvironment :: Bool -> (ToolEnv -> IO a) -> IO a
withEnvironment resident action = bracket allocate removeDirectoryRecursive \root -> do
    env <- defaultToolEnv (unsafeEncodeUtf root)
    setToolSessionTmp env (Just (unsafeEncodeUtf root))
    action (if resident then env else env { toolOutputMemoryCap = 0 })
  where
    allocate = do
        temporary <- getTemporaryDirectory
        (path, handle) <- openBinaryTempFile temporary "artifact-storage-benchmark-"
        hClose handle
        removeFile path
        createDirectory path
        pure path

query :: ToolEnv -> Text.Text -> Text.Text -> IO Text.Text
query env operation handle = do
    let arguments = "{\"handle\":\"" <> handle <> "\","
            <> if operation == "read_tool_output"
                then "\"max_chars\":4096}"
                else "\"pattern\":\"needle\",\"head_limit\":5}"
        call = functionToolCall "benchmark" operation arguments
        tool = find ((== operation) . (.appToolName)) (artifactTools env Nothing)
        config = ToolDispatchConfig
            { toolDispatchUnknownTool = ("unknown tool: " <>)
            , toolDispatchFormatResult = either id id
            , toolDispatchFormatException = \_ exception -> Text.pack (show exception)
            , toolDispatchOnException = \_ _ -> pure ()
            , toolDispatchOnOutput = \_ _ -> pure ()
            , toolDispatchFinalizeOutput = \_ output -> pure output
            }
    result <- dispatchToolHandler config ((.appToolHandler) <$> tool) call
    unless ("\"next_cursor\"" `Text.isInfixOf` result.output)
        (die ("Query failed: " <> Text.unpack result.output))
    pure result.output
