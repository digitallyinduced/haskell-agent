{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Memory benchmark for grep's whole-process capture versus the bounded
-- streaming reader. Run with @+RTS -T@. Five samples are collected for each
-- representative stdout size and the reported row is the median.
module Main (main) where

import Control.Concurrent.Async (withAsync, wait)
import Control.Exception.Safe (evaluate, finally)
import Control.Monad (forM_, replicateM)
import qualified Data.ByteString as BS
import Data.List (sort)
import Data.IORef (newIORef, writeIORef)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (RTSStats(..), getRTSStats, getRTSStatsEnabled)
import System.CPUTime (getCPUTime)
import System.Directory (findExecutable, getTemporaryDirectory, removeDirectoryRecursive)
import System.Exit (die)
import System.FilePath ((</>))
import System.IO (BufferMode(NoBuffering), Handle, hClose, hGetLine, hSetBuffering)
import System.Mem (performGC)
import System.Posix.Temp (mkdtemp)
import System.Process
    ( CreateProcess(std_err, std_in, std_out)
    , StdStream(CreatePipe)
    , createProcess
    , proc
    , readCreateProcessWithExitCode
    , terminateProcess
    , waitForProcess
    )
import Text.Printf (printf)

sampleCount, outputLineLimit :: Int
sampleCount = 5
outputLineLimit = 128

data Measurement = Measurement
    { elapsedMillis :: !Double
    , cpuSeconds :: !Double
    , allocatedBytes :: !Integer
    , liveBytes :: !Integer
    }

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    if enabled then pure () else die "run with +RTS -T"
    rg <- maybe (die "rg is not installed") pure =<< findExecutable "rg"
    tmp <- getTemporaryDirectory
    bracketTemp (tmp </> "grep-streaming-") \dir -> do
        forM_ [1, 8, 32 :: Int] \sizeMb -> do
            let path = dir </> show sizeMb <> "mb.txt"
            writeFixture path sizeMb
            forM_ ["buffered-old", "streaming-bounded"] \mode -> do
                measurements <- replicateM sampleCount do
                    performGC
                    before <- getRTSStats
                    t0 <- getMonotonicTimeNSec
                    c0 <- getCPUTime
                    (!output, !retainedLive) <- if mode == "buffered-old"
                        then buffered rg path
                        else streaming rg path
                    -- Both implementations return the same first 128 lines;
                    -- forcing this length keeps the benchmark honest.
                    output `seq` pure ()
                    performGC
                    after <- getRTSStats
                    t1 <- getMonotonicTimeNSec
                    c1 <- getCPUTime
                    pure Measurement
                        { elapsedMillis =
                            fromIntegral (t1 - t0) / 1.0e6
                        , cpuSeconds =
                            fromIntegral (c1 - c0) / 1.0e12
                        , allocatedBytes =
                            fromIntegral
                                ( after.allocated_bytes
                                    - before.allocated_bytes
                                )
                        , liveBytes = retainedLive
                        }
                let median field = medianOf (map field measurements)
                printf "%d,%s,%d,%.3f,%.3f,%d,%d\n"
                    sizeMb mode outputLineLimit
                    (median elapsedMillis)
                    (median cpuSeconds)
                    (median allocatedBytes)
                    (median liveBytes)

writeFixture :: FilePath -> Int -> IO ()
writeFixture path sizeMb = do
    let line = "needle " <> BS.replicate 72 120 <> "\n"
        lineCount = max 1 ((sizeMb * 1024 * 1024) `div` BS.length line)
    BS.writeFile path (BS.concat (replicate lineCount line))

bracketTemp :: FilePath -> (FilePath -> IO a) -> IO a
bracketTemp template action = do
    tmp <- mkdtemp template
    action tmp `finally` removeDirectoryRecursive tmp

command :: FilePath -> FilePath -> CreateProcess
command rg path =
    proc rg ["--no-config", "--color=never", "--regexp", "needle", "--", path]

buffered :: FilePath -> FilePath -> IO (Int, Integer)
buffered rg path = do
    (_, output, _) <- readCreateProcessWithExitCode (command rg path) ""
    -- Force and retain the whole historical capture across a major GC so the
    -- reported live figure includes the representation this path replaces.
    !_ <- evaluate (length output)
    retained <- newIORef output
    let !selectedLength =
            length (concat (take outputLineLimit (lines output)))
    performGC
    live <- (.gc.gcdetails_live_bytes) <$> getRTSStats
    writeIORef retained ""
    pure (selectedLength, fromIntegral live)

streaming :: FilePath -> FilePath -> IO (Int, Integer)
streaming rg path = do
    (mIn, mOut, mErr, ph) <- createProcess (command rg path)
        { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
    maybe (pure ()) hClose mIn
    out <- maybe (die "stdout pipe unavailable") pure mOut
    err <- maybe (die "stderr pipe unavailable") pure mErr
    hSetBuffering out NoBuffering
    withAsync (drain err) \stderr -> do
        result <- readLines out outputLineLimit `finally` terminateProcess ph
        _ <- wait stderr
        _ <- waitForProcess ph
        hClose out `finally` hClose err
        performGC
        live <- (.gc.gcdetails_live_bytes) <$> getRTSStats
        pure (result, fromIntegral live)
  where
    readLines :: Handle -> Int -> IO Int
    readLines handle count = go count 0
      where
        go remaining total
            | remaining <= 0 = pure total
            | otherwise = do
                line <- hGetLine handle
                go (remaining - 1) (total + length line)
    drain handle = do
        bytes <- BS.hGetSome handle 8192
        if BS.null bytes then pure () else drain handle

medianOf :: Ord a => [a] -> a
medianOf values =
    let ordered = sort values
    in ordered !! (length ordered `div` 2)
