{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- Compare the historical whole-process capture with the bounded line reader
-- used by grep. Run with +RTS -T and retain the CSV output as a baseline.
module Main (main) where

import Control.Concurrent.Async (withAsync, wait)
import Control.Exception.Safe (finally)
import Control.Monad (forM_)
import qualified Data.ByteString as BS
import GHC.Stats (RTSStats(..), getRTSStats, getRTSStatsEnabled)
import GHC.Clock (getMonotonicTimeNSec)
import System.CPUTime (getCPUTime)
import System.Directory (findExecutable, getTemporaryDirectory, removeDirectoryRecursive)
import System.Exit (die)
import System.FilePath ((</>))
import System.IO (BufferMode(NoBuffering), hClose, hGetLine, hSetBuffering, hSetEncoding, utf8)
import System.Mem (performGC)
import System.Posix.Temp (mkdtemp)
import System.Process
    ( CreateProcess(cwd, std_err, std_in, std_out)
    , StdStream(CreatePipe)
    , createProcess
    , proc
    , readCreateProcessWithExitCode
    , terminateProcess
    , waitForProcess
    )
import Text.Printf (printf)

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    if enabled then pure () else die "run with +RTS -T"
    rg <- maybe (die "rg is not installed") pure =<< findExecutable "rg"
    tmp <- getTemporaryDirectory
    bracketTemp (tmp </> "grep-streaming-") \dir -> do
        let path = dir </> "large.txt"
        BS.writeFile path (BS.replicate (32 * 1024 * 1024) 120 <> "\nneedle\n")
        forM_ ["buffered", "streaming"] \mode -> do
            performGC
            before <- getRTSStats
            t0 <- getMonotonicTimeNSec
            c0 <- getCPUTime
            !n <- if mode == "buffered"
                then buffered rg path
                else streaming rg path
            n `seq` pure ()
            performGC
            after <- getRTSStats
            t1 <- getMonotonicTimeNSec
            c1 <- getCPUTime
            printf "%s,%d,%.3f,%.3f,%d,%d\n" mode n
                (fromIntegral (t1 - t0) / 1.0e6)
                (fromIntegral (c1 - c0) / 1.0e9)
                (after.allocated_bytes - before.allocated_bytes)
                after.gc.gcdetails_live_bytes

bracketTemp :: FilePath -> (FilePath -> IO a) -> IO a
bracketTemp template action = do
    tmp <- mkdtemp template
    action tmp `finally` removeDirectoryRecursive tmp

command :: FilePath -> FilePath -> CreateProcess
command rg path =
    (proc rg ["--no-config", "--color=never", "--regexp", "needle", "--", path])
        { cwd = Nothing }

buffered :: FilePath -> FilePath -> IO Int
buffered rg path = do
    (_, out, _) <- readCreateProcessWithExitCode (command rg path) ""
    pure (length out)

streaming :: FilePath -> FilePath -> IO Int
streaming rg path = do
    (mIn, mOut, mErr, ph) <- createProcess (command rg path)
        { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
    maybe (pure ()) hClose mIn
    out <- maybe (die "stdout pipe unavailable") pure mOut
    err <- maybe (die "stderr pipe unavailable") pure mErr
    hSetEncoding out utf8
    hSetBuffering out NoBuffering
    -- Only the first line is needed for this representative bounded workload.
    withAsync (drain err) \stderr -> do
        result <- (length <$> hGetLine out) `finally` terminateProcess ph
        _ <- wait stderr
        _ <- waitForProcess ph
        hClose out `finally` hClose err
        pure result
  where
    drain handle = do
        bytes <- BS.hGetSome handle 8192
        if BS.null bytes then pure () else drain handle
