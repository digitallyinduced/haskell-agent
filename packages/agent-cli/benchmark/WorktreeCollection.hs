{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- Compare the replaced snapshot path with clean-object recovery, including
-- their production verification counts. Fixture construction is not measured.
-- Both paths preserve the same clean linked checkout and leave it present.
-- Session/lease discovery, incorporation proof, size estimation and deletion
-- are excluded: this benchmark measures recovery preparation, not total GC.
module Main (main) where

import qualified Agent.CLI.Worktree.Clean as Clean
import qualified Agent.CLI.Worktree.Snapshot as Snapshot
import Control.Exception (evaluate)
import Control.Exception.Safe (bracket)
import Control.Monad (forM, forM_, unless)
import qualified Data.ByteString.Char8 as BS
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (RTSStats(..), getRTSStats, getRTSStatsEnabled)
import System.CPUTime (getCPUTime)
import qualified System.Directory as Directory
import System.Environment (getArgs, getEnvironment, getEnv)
import System.Exit (ExitCode(..), die)
import System.FilePath ((</>))
import System.IO (BufferMode(..), hSetBuffering, stdout)
import System.Mem (performGC)
import System.OsPath (OsPath, unsafeEncodeUtf)
import System.Posix.Temp (mkdtemp)
import System.Process (CreateProcess(..), proc, readCreateProcessWithExitCode)
import Text.Printf (printf)
import Text.Read (readMaybe)

data Sample = Sample
    { elapsedMillis :: !Double
    , cpuMillis :: !Double
    , allocatedBytes :: !Integer
    , resultChecksum :: !Int
    }

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    enabled <- getRTSStatsEnabled
    unless enabled $ die "RTS statistics required: +RTS -T"
    arguments <- getArgs
    (counts, samples, selection) <- case arguments of
        [] -> pure ([100, 1000, 3000], 3, "both")
        [files, repetitions] -> do
            count <- positive files
            sampleCount <- positive repetitions
            pure ([count], sampleCount, "both")
        [implementation, files, repetitions] | implementation `elem` ["old-snapshot", "new-clean"] -> do
            count <- positive files
            sampleCount <- positive repetitions
            pure ([count], sampleCount, implementation)
        _ -> die "usage: worktree-collection-bench [[old-snapshot|new-clean] FILES SAMPLES] +RTS -T"
    putStrLn "implementation,files,bytes_per_file,samples,median_elapsed_ms,median_parent_cpu_ms,median_parent_allocated_bytes,checksum"
    forM_ counts $ \count -> withFixture count $ \checkout -> do
      if selection /= "both" then do
        measurements <- forM [1 .. samples] $ \_ ->
            measure (if selection == "old-snapshot" then oldRecovery checkout else newRecovery checkout)
        printMedian selection count measurements
      else do
        -- Alternate order to avoid attributing a systematic warm-cache or
        -- thermal-order advantage to either implementation.
        pairs <- forM [1 .. samples] $ \sampleNumber ->
            if odd sampleNumber then do
                baseline <- measure (oldRecovery checkout)
                replacement <- measure (newRecovery checkout)
                pure (baseline, replacement)
            else do
                replacement <- measure (newRecovery checkout)
                baseline <- measure (oldRecovery checkout)
                pure (baseline, replacement)
        printMedian "old-snapshot" count (map fst pairs)
        printMedian "new-clean" count (map snd pairs)
  where
    positive value = case readMaybe value of
        Just number | number > 0 -> pure number
        _ -> die "FILES and SAMPLES must be positive integers"

oldRecovery :: OsPath -> IO Snapshot.WorktreeSnapshot
oldRecovery path = do
    require =<< Snapshot.checkSnapshotSupported path
    snapshot <- require =<< Snapshot.createSnapshot path
    require =<< Snapshot.verifySnapshotUnchanged path snapshot
    pure snapshot

newRecovery :: OsPath -> IO Snapshot.WorktreeSnapshot
newRecovery path = do
    clean <- require =<< Clean.inspectCleanCheckout path
    require =<< Clean.verifyCleanCheckout path clean
    snapshot <- require =<< Clean.preserveCleanCheckout path clean
    require =<< Clean.verifyCleanCheckout path clean
    require =<< Clean.verifyCleanCheckout path clean
    pure snapshot

require :: Either Text a -> IO a
require = either (fail . Text.unpack) pure

measure :: IO Snapshot.WorktreeSnapshot -> IO Sample
measure action = do
    performGC
    beforeStatistics <- getRTSStats
    beforeCpu <- getCPUTime
    beforeElapsed <- getMonotonicTimeNSec
    snapshot <- action
    checksum <- evaluate $ sum $ map Text.length
        [ snapshot.snapshotHead, snapshot.snapshotIndexTree
        , snapshot.snapshotWorkTree, snapshot.snapshotRef, snapshot.snapshotBranch ]
    afterElapsed <- getMonotonicTimeNSec
    afterCpu <- getCPUTime
    -- Account for the last partial nursery; this GC is outside elapsed/CPU time.
    performGC
    afterStatistics <- getRTSStats
    pure Sample
        { elapsedMillis = fromIntegral (afterElapsed - beforeElapsed) / 1e6
        , cpuMillis = fromIntegral (afterCpu - beforeCpu) / 1e9
        , allocatedBytes = toInteger afterStatistics.allocated_bytes
            - toInteger beforeStatistics.allocated_bytes
        , resultChecksum = checksum
        }

printMedian :: String -> Int -> [Sample] -> IO ()
printMedian implementation files samples =
    printf "%s,%d,1024,%d,%.3f,%.3f,%d,%d\n"
        implementation files (length samples)
        (median (map (.elapsedMillis) samples))
        (median (map (.cpuMillis) samples))
        (median (map (.allocatedBytes) samples))
        (sum (map (.resultChecksum) samples))
  where
    median values = sort values !! (length values `div` 2)

withFixture :: Int -> (OsPath -> IO a) -> IO a
withFixture count action = do
    temporaryRoot <- getEnv "TMPDIR"
    bracket (mkdtemp (temporaryRoot </> "worktree-collection-benchmark-"))
        Directory.removePathForcibly $ \root -> do
            let repository = root </> "repository"
                checkout = root </> "checkout"
            Directory.createDirectory repository
            git repository ["init", "-q", "-b", "main"]
            git repository ["config", "user.name", "Collection Benchmark"]
            git repository ["config", "user.email", "collection-benchmark@example.invalid"]
            git repository ["config", "core.autocrlf", "false"]
            git repository ["config", "core.attributesFile", "/dev/null"]
            git repository ["config", "core.excludesFile", "/dev/null"]
            git repository ["config", "commit.gpgsign", "false"]
            git repository ["config", "gc.auto", "0"]
            forM_ [1 .. count] $ \number ->
                BS.writeFile (repository </> ("source-" <> show number <> ".txt"))
                    (BS.pack (take 1024 (cycle ("file " <> show number <> "\n"))))
            git repository ["add", "."]
            git repository ["commit", "-q", "-m", "Baseline source files"]
            git repository ["worktree", "add", "-q", "-b", "merged-feature", checkout]
            BS.writeFile (checkout </> "source-1.txt") (BS.replicate 1024 'm')
            git checkout ["commit", "-q", "-a", "-m", "Feature change"]
            git repository ["merge", "--no-ff", "-q", "-m", "Merge feature", "merged-feature"]
            action (unsafeEncodeUtf checkout)

git :: FilePath -> [String] -> IO ()
git directory arguments = do
    environment <- getEnvironment
    let overrides =
            [ ("GIT_CONFIG_NOSYSTEM", "1"), ("GIT_CONFIG_GLOBAL", "/dev/null")
            , ("GIT_TERMINAL_PROMPT", "0")
            ]
    (code, _, diagnostic) <- readCreateProcessWithExitCode
        (proc "git" ("-C" : directory : arguments))
            { env = Just (overrides <> filter (\(name, _) ->
                name `notElem` map fst overrides) environment) } ""
    unless (code == ExitSuccess) $ fail diagnostic
