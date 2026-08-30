{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Agent.Tools.FileSystem.ReadFile
    ( ReadFileArgs(..)
    , formatReadFile
    , streamReadFile
    )
import Agent.Tools.IO (readTextFile)
import Control.Exception (bracket, evaluate)
import Control.Monad (forM, forM_)
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import Data.List (sort)
import Data.IORef (newIORef, writeIORef)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
    ( GCDetails(..)
    , RTSStats(..)
    , getRTSStats
    , getRTSStatsEnabled
    )
import System.CPUTime (getCPUTime)
import System.Directory (getTemporaryDirectory, removeDirectoryRecursive)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>))
import System.Mem (performGC)
import System.OsPath (OsPath, unsafeEncodeUtf)
import System.Posix.Temp (mkdtemp)
import Text.Printf (printf)

data Sample = Sample
    { elapsedMillis :: !Double
    , cpuMillis :: !Double
    , allocatedBytes :: !Integer
    , liveBytes :: !Integer
    }

data Workload = Buffered | Streaming

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    if enabled then pure () else die "run with +RTS -T"
    args <- getArgs
    let sizes = case args of
            [] -> [1024 * 1024, 8 * 1024 * 1024, 32 * 1024 * 1024]
            values -> map read values
        samples = 3
    root <- getTemporaryDirectory
    bracket (mkdtemp (root </> "read-file-bench-")) removeDirectoryRecursive \dir ->
        forM_ sizes \size -> do
            let path = dir </> "large.txt"
                osPath = unsafeEncodeUtf path
                content = makeContent size
                readArgs =
                    ReadFileArgs "large.txt" (Just 1) (Just 100) Nothing Nothing
            BS.writeFile path content
            forM_ [Buffered, Streaming] \workload -> do
                measurements <- forM [1 .. samples] \_ ->
                    measure workload osPath readArgs
                let sample = median measurements
                printf "%s,%d,%.3f,%.3f,%d,%d\n"
                    (workloadName workload)
                    size
                    sample.elapsedMillis
                    sample.cpuMillis
                    sample.allocatedBytes
                    sample.liveBytes

workloadName :: Workload -> String
workloadName Buffered = "buffered"
workloadName Streaming = "streaming"

makeContent :: Int -> BS.ByteString
makeContent size =
    BS.take size
        (BS.concat
            (replicate
                lineCount
                "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ\n"))
  where
    lineCount = max 1 (size `div` 65 + 1)

measure :: Workload -> OsPath -> ReadFileArgs -> IO Sample
measure workload path args = do
    performGC
    before <- getRTSStats
    t0 <- getMonotonicTimeNSec
    c0 <- getCPUTime
    (result, retainedLive) <- case workload of
        Buffered -> readTextFile path >>= \case
            Left err -> die (show err)
            Right text -> do
                !_ <- evaluate (Text.length text)
                retained <- newIORef text
                result <- evaluate (formatReadFile text args)
                !_ <- evaluate (either Text.length Text.length result)
                performGC
                live <- (.gc.gcdetails_live_bytes) <$> getRTSStats
                writeIORef retained ""
                pure (result, fromIntegral live)
        Streaming -> do
            result <- evaluate =<< streamReadFile path args
            !_ <- evaluate (either Text.length Text.length result)
            performGC
            live <- (.gc.gcdetails_live_bytes) <$> getRTSStats
            pure (result, fromIntegral live)
    -- Force the returned text so the measurement includes materialisation.
    !_ <- evaluate (length (show result))
    performGC
    after <- getRTSStats
    t1 <- getMonotonicTimeNSec
    c1 <- getCPUTime
    pure Sample
        { elapsedMillis = fromIntegral (t1 - t0) / 1.0e6
        , cpuMillis = fromIntegral (c1 - c0) / 1.0e9
        , allocatedBytes =
            fromIntegral (after.allocated_bytes - before.allocated_bytes)
        , liveBytes = retainedLive
        }

median :: [Sample] -> Sample
median values =
    Sample
        { elapsedMillis = middle (map elapsedMillis values)
        , cpuMillis = middle (map cpuMillis values)
        , allocatedBytes = middle (map allocatedBytes values)
        , liveBytes = middle (map liveBytes values)
        }
  where
    middle xs = sort xs !! (length xs `div` 2)
