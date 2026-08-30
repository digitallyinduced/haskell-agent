{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Agent.Tools.FileSystem.ReadFile
    ( ReadFileArgs(..)
    , formatReadFile
    , streamReadFile
    )
import Agent.Tools.IO (readTextFile)
import Control.Exception (bracket, evaluate)
import Control.Monad (forM_)
import qualified Data.ByteString as BS
import GHC.Stats (RTSStats(..), GCDetails(..), getRTSStats, getRTSStatsEnabled)
import GHC.Clock (getMonotonicTimeNSec)
import System.CPUTime (getCPUTime)
import System.Directory (getTemporaryDirectory, removeDirectoryRecursive)
import System.Exit (die)
import System.FilePath ((</>))
import System.Mem (performGC)
import System.OsPath (unsafeEncodeUtf)
import System.Posix.Temp (mkdtemp)
import Text.Printf (printf)

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    if enabled then pure () else die "run with +RTS -T"
    root <- getTemporaryDirectory
    bracket (mkdtemp (root </> "read-file-bench-")) removeDirectoryRecursive \dir -> do
        let path = dir </> "large.txt"
            osPath = unsafeEncodeUtf path
            content = BS.replicate (32 * 1024 * 1024) 120 <> "\n"
            args = ReadFileArgs "large.txt" (Just 1) (Just 1) Nothing Nothing
        BS.writeFile path content
        forM_ ["buffered", "streaming"] \mode -> do
            performGC
            before <- getRTSStats
            t0 <- getMonotonicTimeNSec
            c0 <- getCPUTime
            result <- if mode == "buffered"
                then readTextFile osPath >>= \case
                    Left err -> die (show err)
                    Right text -> evaluate (formatReadFile text args)
                else evaluate =<< streamReadFile osPath args
            !_ <- evaluate (length (show result))
            performGC
            after <- getRTSStats
            t1 <- getMonotonicTimeNSec
            c1 <- getCPUTime
            printf "%s,%.3f,%.3f,%d,%d\n" mode
                (fromIntegral (t1 - t0) / 1.0e6)
                (fromIntegral (c1 - c0) / 1.0e9)
                (after.allocated_bytes - before.allocated_bytes)
                after.gc.gcdetails_live_bytes

