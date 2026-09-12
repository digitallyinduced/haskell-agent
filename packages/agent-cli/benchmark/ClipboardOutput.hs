{-# LANGUAGE OverloadedStrings #-}

-- Standalone optimized benchmark: see ClipboardOutput.md for build/run commands.
module Main (main) where

import Agent.CLI.Clipboard.Process (readClipboardProcessText)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (forM, forM_, unless)
import qualified Data.ByteString as BS
import Data.List (sort)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import GHC.Stats (RTSStats(..), getRTSStats, getRTSStatsEnabled)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs, getEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO.Temp (withTempDirectory)
import System.Mem (performGC)
import System.Process (readProcessWithExitCode)
import Text.Printf (printf)

type Output = (ExitCode, Text.Text, Text.Text)

data Sample = Sample !Double !Double !Word64

main :: IO ()
main = do
    -- Make the baseline's locale-sensitive decoder deterministic.
    setLocaleEncoding utf8
    enabled <- getRTSStatsEnabled
    unless enabled (fail "Run with +RTS -T")
    args <- getArgs
    case args of
        ["suite", count] -> do
            let samples = read count
            unless (samples > 0) (fail "sample count must be positive")
            tmp <- getEnv "TMPDIR"
            withTempDirectory tmp "clipboard-output" $ \dir -> do
                putStrLn "kind,stdout_bytes,stderr_bytes,pass,method,median_elapsed_ms,median_cpu_ms,median_allocated_bytes"
                forM_ ["ascii", "unicode"] $ \kind ->
                    forM_ [1024, 65536, 1048576, 8388608] $ \size ->
                        benchmark dir samples kind size "initial"
                forM_ ["ascii", "unicode"] $ \kind ->
                    benchmark dir samples kind 1048576 "repeat"
        ["fixture", kind, size, out, err] -> do
            _ <- writeFixture kind (read size) out err
            pure ()
        ["peak", method, out, err] -> do
            -- No fixture generation, expected payload, baseline warmup, or
            -- other method in this process: its high-water marks are isolated.
            result <- capture method out err >>= evaluate . force
            let (code, stdoutText, stderrText) = result
            unless (code == ExitSuccess) (fail "fixture subprocess failed")
            performGC
            stats <- getRTSStats
            print (Text.length stdoutText, Text.length stderrText)
            printf "max_live_bytes=%d max_mem_in_use_bytes=%d\n"
                (max_live_bytes stats) (max_mem_in_use_bytes stats)
        _ -> fail "Usage: suite SAMPLES | fixture ascii|unicode BYTES OUT ERR | peak old|new OUT ERR"

-- Always decode and force BOTH streams for both methods; not a stdout-only
-- baseline that quietly discards the diagnostic stream.
capture :: String -> FilePath -> FilePath -> IO Output
capture method out err =
    let command = "/bin/sh"
        args = ["-ec", "/bin/cat \"$1\"; /bin/cat \"$2\" >&2", "fixture", out, err]
    in case method of
        "old" -> do
            (code, stdoutString, stderrString) <-
                readProcessWithExitCode command args ""
            pure (code, Text.pack stdoutString, Text.pack stderrString)
        "new" -> readClipboardProcessText command args
        _ -> fail "method must be old or new"

writeFixture :: String -> Int -> FilePath -> FilePath -> IO Output
writeFixture kind bytes out err = do
    unless (bytes >= 0) (fail "fixture size must be nonnegative")
    unit <- case kind of
        "ascii" -> pure "clipboard line\r\n"
        "unicode" -> pure "aé中🙂\r\n"
        _ -> fail "fixture kind must be ascii or unicode"
    -- Repeat whole UTF-8 units, then pad with ASCII: exactly the requested byte
    -- size without ever slicing a multibyte code point.
    let unitBytes = BS.length (Text.encodeUtf8 unit)
        (copies, padding) = bytes `divMod` unitBytes
        stdoutText = Text.replicate copies unit <> Text.replicate padding "x"
        stderrText = "fixture diagnostic: café 中 🙂\r\n"
    expected <- evaluate (force (ExitSuccess, stdoutText, stderrText))
    BS.writeFile out (Text.encodeUtf8 stdoutText)
    BS.writeFile err (Text.encodeUtf8 stderrText)
    pure expected

benchmark :: FilePath -> Int -> String -> Int -> String -> IO ()
benchmark dir count kind size pass = do
    let out = dir </> "stdout"
        err = dir </> "stderr"
    expected@(_, _, stderrText) <- writeFixture kind size out err
    -- Warm both methods and compare every character, including CR/LF, before
    -- timing. Every measured result is also compared outside its interval.
    forM_ ["old", "new"] $ \method ->
        capture method out err >>= check expected
    pairs <- forM [1 .. count] $ \index -> do
        let methods = if odd index then ["old", "new"] else ["new", "old"]
        forM methods $ \method -> do
            sample <- measure expected (capture method out err)
            pure (method, sample)
    forM_ ["old", "new"] $ \method -> do
        let samples = [sample | pair <- pairs, (name, sample) <- pair, name == method]
        printf "%s,%d,%d,%s,%s,%.3f,%.3f,%d\n"
            kind size (BS.length (Text.encodeUtf8 stderrText)) pass method
            (median [elapsed | Sample elapsed _ _ <- samples])
            (median [cpu | Sample _ cpu _ <- samples])
            (median [allocated | Sample _ _ allocated <- samples])

check :: Output -> Output -> IO ()
check expected result =
    unless (result == expected) (fail "captured output differs from fixture")

measure :: Output -> IO Output -> IO Sample
measure expected action = do
    performGC
    before <- getRTSStats
    cpuBefore <- getCPUTime
    wallBefore <- getMonotonicTimeNSec
    result <- action >>= evaluate . force
    wallAfter <- getMonotonicTimeNSec
    cpuAfter <- getCPUTime
    -- Flush per-capability allocation counters. This explicit post-sample GC
    -- is outside timing; automatic collections during capture remain included.
    performGC
    after <- getRTSStats
    check expected result
    pure (Sample
        (fromIntegral (wallAfter - wallBefore) / 1000000)
        (fromIntegral (cpuAfter - cpuBefore) / 1000000000)
        (allocated_bytes after - allocated_bytes before))

median :: Ord a => [a] -> a
median values = sort values !! (length values `div` 2)
