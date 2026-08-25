module Main (main) where

import Agent.Concurrent (mapConcurrentlyBounded)
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (SomeException, bracket)
import qualified Control.Exception.Safe as Exception
import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.List (sort)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
    ( RTSStats(..)
    , getRTSStats
    , getRTSStatsEnabled
    )
import System.CPUTime (getCPUTime)
import System.Directory
    ( createDirectoryIfMissing
    , getTemporaryDirectory
    , removePathForcibly
    )
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.Mem (performMajorGC)

data Sample = Sample
    { sampleElapsedMillis :: !Double
    , sampleCpuMillis :: !Double
    , sampleAllocatedBytes :: !Word
    }

main :: IO ()
main = do
    statsEnabled <- getRTSStatsEnabled
    when (not statsEnabled) $
        fail "run with +RTS -T"
    (count, bytesPerFile, samples) <- parseArgs <$> getArgs
    withInputFiles count bytesPerFile \paths -> do
        putStrLn
            ("count=" <> show count
                <> " bytes-per-file=" <> show bytesPerFile
                <> " samples=" <> show samples)
        benchmark "startup-serial" samples
            (serialDelayed count)
        benchmark "startup-bounded-8" samples
            (boundedDelayed count)
        benchmark "attachments-serial" samples
            (serialRead paths)
        benchmark "attachments-bounded-4" samples
            (boundedRead paths)

parseArgs :: [String] -> (Int, Int, Int)
parseArgs = \case
    [count, bytesPerFile, samples] ->
        (max 1 (read count), max 1 (read bytesPerFile), max 1 (read samples))
    _ -> (32, 16 * 1024, 7)

benchmark :: String -> Int -> IO Int -> IO ()
benchmark label count action = do
    samples <- mapM (const (measure action)) [1 .. max 1 count]
    let elapsed = median (map (.sampleElapsedMillis) samples)
        cpu = median (map (.sampleCpuMillis) samples)
        allocated = median (map (.sampleAllocatedBytes) samples)
    putStrLn
        (label
            <> " elapsed-ms=" <> show elapsed
            <> " cpu-ms=" <> show cpu
            <> " allocated-bytes=" <> show allocated)

measure :: IO Int -> IO Sample
measure action = do
    performMajorGC
    beforeStats <- getRTSStats
    beforeCpu <- getCPUTime
    beforeElapsed <- getMonotonicTimeNSec
    checksum <- action
    checksum `seq` pure ()
    afterElapsed <- getMonotonicTimeNSec
    afterCpu <- getCPUTime
    afterStats <- getRTSStats
    pure Sample
        { sampleElapsedMillis =
            fromIntegral (afterElapsed - beforeElapsed) / 1_000_000
        , sampleCpuMillis =
            fromIntegral (afterCpu - beforeCpu) / 1_000_000_000
        , sampleAllocatedBytes =
            fromIntegral
                (allocated_bytes afterStats - allocated_bytes beforeStats)
        }

serialDelayed :: Int -> IO Int
serialDelayed count = do
    values <- mapM (const delayedWork) [1 .. count]
    pure (sum values)

boundedDelayed :: Int -> IO Int
boundedDelayed count = do
    values <- mapConcurrentlyBounded 8 (const delayedWork) [1 .. count]
    pure (sum values)

delayedWork :: IO Int
delayedWork = threadDelay 20_000 >> pure 1

serialRead :: [FilePath] -> IO Int
serialRead paths =
    sumLengths <$> mapM BS.readFile paths

boundedRead :: [FilePath] -> IO Int
boundedRead paths =
    sumLengths <$> mapConcurrentlyBounded 4 BS.readFile paths

sumLengths :: [BS.ByteString] -> Int
sumLengths = foldr ((+) . BS.length) 0

withInputFiles :: Int -> Int -> ([FilePath] -> IO a) -> IO a
withInputFiles count bytesPerFile action = do
    root <- getTemporaryDirectory
    let directory = root </> "agent-cli-concurrency-bench"
        contents = BS.replicate bytesPerFile 97
        paths =
            [ directory </> ("attachment-" <> show index <> ".bin")
            | index <- [1 .. count]
            ]
    bracket
        (do
            removePathIfPresent directory
            createDirectoryIfMissing True directory
            mapM_ (`BS.writeFile` contents) paths
            pure paths)
        (const (removePathIfPresent directory))
        action

removePathIfPresent :: FilePath -> IO ()
removePathIfPresent path =
    removePathForcibly path `catchAny` const (pure ())

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Exception.catchAny

median :: Ord a => [a] -> a
median values =
    sort values !! (length values `div` 2)
