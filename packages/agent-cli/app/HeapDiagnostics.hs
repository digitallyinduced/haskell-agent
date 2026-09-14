-- | Opt-in numeric RTS observations for controlled memory investigations.
--
-- Normal sampling does not collect garbage. The @last_gc_*@ columns describe
-- the latest completed collection, which can be a minor collection and can be
-- stale while idle. They are not necessarily current major-GC residency.
-- Setting @AGENT_HEAP_DIAGNOSTICS_FORCE_GC=1@ requests a major collection before
-- each observation; this deliberately perturbs runtime behaviour and must not
-- be used as an uninstrumented performance baseline.
module HeapDiagnostics (withHeapDiagnostics) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (link, withAsync)
import Control.Exception.Safe (finally)
import Control.Monad (forever, unless, when)
import Data.List (intercalate)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (GCDetails(..), RTSStats(..), getRTSStats, getRTSStatsEnabled)
import System.Environment (lookupEnv)
import System.Exit (die)
import System.IO
    ( BufferMode(LineBuffering)
    , Handle
    , IOMode(AppendMode)
    , hFileSize
    , hPutStrLn
    , hSetBuffering
    , withFile
    )
import System.Mem (performMajorGC)
import System.Posix.Process (getProcessID)

withHeapDiagnostics :: IO a -> IO a
withHeapDiagnostics action =
    lookupEnv "AGENT_HEAP_DIAGNOSTICS" >>= \case
        Nothing -> action
        Just path -> do
            when (null path) $
                die "AGENT_HEAP_DIAGNOSTICS must name a CSV output file."
            enabled <- getRTSStatsEnabled
            unless enabled $
                die "AGENT_HEAP_DIAGNOSTICS requires RTS statistics: add +RTS -T -RTS."
            forcedSetting <- lookupEnv "AGENT_HEAP_DIAGNOSTICS_FORCE_GC"
            forceCollection <- case forcedSetting of
                Nothing -> pure False
                Just "0" -> pure False
                Just "1" -> pure True
                _ -> die "AGENT_HEAP_DIAGNOSTICS_FORCE_GC must be 0 or 1."
            processId <- getProcessID
            started <- getMonotonicTimeNSec
            withFile path AppendMode \handle -> do
                hSetBuffering handle LineBuffering
                size <- hFileSize handle
                when (size == 0) $ hPutStrLn handle header
                let sample = do
                        when forceCollection performMajorGC
                        timestamp <- getPOSIXTime
                        observed <- getMonotonicTimeNSec
                        statistics <- getRTSStats
                        writeSample handle
                            [ show (realToFrac timestamp :: Double)
                            , show (observed - started)
                            , show processId
                            , if forceCollection then "1" else "0"
                            ]
                            statistics
                sample
                (withAsync (forever (threadDelay 1000000 >> sample)) \worker -> do
                    link worker
                    action)
                    `finally` sample

header :: String
header = intercalate ","
    [ "unix_seconds"
    , "observation_elapsed_ns"
    , "pid"
    , "forced_major_gc"
    , "gcs"
    , "major_gcs"
    , "allocated_bytes"
    , "max_live_bytes"
    , "max_mem_in_use_bytes"
    , "last_gc_generation"
    , "last_gc_live_bytes"
    , "last_gc_mem_in_use_bytes"
    , "last_gc_large_objects_bytes"
    , "last_gc_slop_bytes"
    , "gc_cpu_ns"
    , "gc_elapsed_ns"
    , "mutator_cpu_ns"
    , "mutator_elapsed_ns"
    ]

writeSample :: Handle -> [String] -> RTSStats -> IO ()
writeSample handle prefix statistics =
    hPutStrLn handle $ intercalate "," $
        prefix <>
            [ show statistics.gcs
            , show statistics.major_gcs
            , show statistics.allocated_bytes
            , show statistics.max_live_bytes
            , show statistics.max_mem_in_use_bytes
            , show statistics.gc.gcdetails_gen
            , show statistics.gc.gcdetails_live_bytes
            , show statistics.gc.gcdetails_mem_in_use_bytes
            , show statistics.gc.gcdetails_large_objects_bytes
            , show statistics.gc.gcdetails_slop_bytes
            , show statistics.gc_cpu_ns
            , show statistics.gc_elapsed_ns
            , show statistics.mutator_cpu_ns
            , show statistics.mutator_elapsed_ns
            ]
