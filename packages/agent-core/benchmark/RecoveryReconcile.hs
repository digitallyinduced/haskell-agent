{-# LANGUAGE RankNTypes #-}
-- | One reconciliation pass over a tree whose children finished and settled,
-- as a durable host keeps them for later replies. Each store transaction
-- sleeps DELAY_US, standing in for the host's database round trips.
--
-- @old@ runs the pass this change replaced ('RecoveryReconcileBaseline'),
-- @new@ runs 'reconcileStoredSubagents'. Prints workload, children,
-- delay, store transactions per pass, and median wall time, CPU time and
-- allocated bytes per pass.
module Main (main) where

import Agent.Loop (LoopError (LoopNoResponseId))
import Agent.Subagents
import Agent.Subagents.Runtime (RuntimeHandle (..))
import Agent.Subagents.Runtime.Recovery
import Agent.Subagents.TaskPath (parseTaskPath)
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (bracket)
import Control.Monad (forM, forM_, unless, when)
import Data.IORef
import Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
    ( RTSStats(..)
    , getRTSStats
    , getRTSStatsEnabled
    )
import RecoveryReconcileBaseline (baselineReconcileStoredSubagents)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (die)
import System.Mem (performGC)
import System.OsPath (unsafeEncodeUtf)
import Text.Printf (printf)

data Workload
    = OldReconcile
    | NewReconcile
    deriving (Eq, Show)

data Sample = Sample
    { wallMillis :: !Double
    , cpuMillis :: !Double
    , allocatedBytes :: !Integer
    , transactions :: !Int
    }

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    unless enabled (die "RTS statistics are disabled; run with +RTS -T")
    getArgs >>= \case
        [workloadArg, childCountArg, delayArg, sampleCountArg] -> do
            workload <- parseWorkload workloadArg
            childCount <- parsePositive "child count" childCountArg
            delayMicros <- parseNonNegative "delay microseconds" delayArg
            sampleCount <- parsePositive "sample count" sampleCountArg
            samples <- withSettledTree workload childCount delayMicros \pass opened ->
                forM [1 .. sampleCount] \_ -> measure pass opened
            let result = median samples
            printf
                "%s,%d,%d,%d,%.3f,%.3f,%d\n"
                workloadArg
                childCount
                delayMicros
                result.transactions
                result.wallMillis
                result.cpuMillis
                result.allocatedBytes
        _ ->
            die $
                "usage: recovery-reconcile-bench WORKLOAD CHILDREN DELAY_US SAMPLES\n"
                    <> "workloads: old, new"

parseWorkload :: String -> IO Workload
parseWorkload = \case
    "old" -> pure OldReconcile
    "new" -> pure NewReconcile
    other -> die ("unknown workload: " <> other)

parsePositive :: String -> String -> IO Int
parsePositive label raw =
    case reads raw of
        [(value, "")]
            | value > 0 -> pure value
        _ -> die ("invalid " <> label <> ": " <> raw)

parseNonNegative :: String -> String -> IO Int
parseNonNegative label raw =
    case reads raw of
        [(value, "")]
            | value >= 0 -> pure value
        _ -> die ("invalid " <> label <> ": " <> raw)

-- | The registry and the store hold the same finished children; the store
-- keeps the settled-mark contract. Setup happens outside the measured pass.
withSettledTree :: Workload -> Int -> Int -> (IO () -> IORef Int -> IO a) -> IO a
withSettledTree workload childCount delayMicros action = do
    children <- forM [1 .. childCount] \index -> do
        path <- either (die . Text.unpack) pure
            (parseTaskPath ("/root/child_" <> Text.pack (show index)))
        pure SubagentSnapshot
            { agentId = SubagentId ("child-" <> Text.pack (show index))
            , parentId = Nothing
            , taskPath = path
            , depth = 1
            , status = finished
            , initialMessage = "Recover this task"
            , wakeRequested = False
            , cancellationRequested = False
            , settled = True
            }
    rows <- newIORef (Map.fromList [(child.agentId, child) | child <- children])
    opened <- newIORef (0 :: Int)
    let store = RecoveryStore
            { loadSnapshots = Map.elems <$> readIORef rows
            , loadSnapshot = \agent -> Map.lookup agent <$> readIORef rows
            , transaction = \run -> do
                modifyIORef' opened (+ 1)
                when (delayMicros > 0) (threadDelay delayMicros)
                run store
            , lockSnapshot = \_ -> pure ()
            , persistStatus = \agent status ->
                modifyIORef' rows (Map.adjust (\row -> row { status, settled = False }) agent)
            , persistRunning = \_ _ -> pure ()
            , persistSuspension = \_ _ _ -> pure (Right ())
            , persistResume = \_ _ -> pure (Right ())
            , settle = \agent _ ->
                modifyIORef' rows (Map.adjust (\row -> row { settled = True }) agent)
            }
    bracket
        (newSubagentRegistry (SubagentConfig 1 (Just 4) Nothing) cwd
            (\_ _ _ _ -> pure (Left LoopNoResponseId)) (\_ _ -> pure ()))
        closeSubagentRegistry
        \registry -> do
            root <- newRootSubagents NoticesToLoop registry
            forM_ children \child ->
                restoreSubagentAtWithCwdStatus registry cwd child.agentId Nothing
                    child.taskPath child.depth Nothing Nothing child.status
                    >>= either (die . Text.unpack) (const (pure ()))
            let handle = RuntimeHandle { registry, root, withOperation = id, withExecution = id }
                config = RecoveryConfig
                    { store
                    , withOwnership = fmap Just
                    , withOperation = id
                    , withExecution = id
                    , executionAllowed = pure True
                    , runner = \_ _ _ _ _ -> pure (Left LoopNoResponseId)
                    , notify = \_ _ -> pure ()
                    , cwd
                    , wakeMessage = "Read pending input"
                    }
                pass = case workload of
                    OldReconcile -> baselineReconcileStoredSubagents config handle
                    NewReconcile -> reconcileStoredSubagents config handle
            action pass opened
  where
    finished = Completed (Just "Recovered result")
    cwd = unsafeEncodeUtf "."

measure :: IO () -> IORef Int -> IO Sample
measure pass opened = do
    writeIORef opened 0
    performGC
    beforeStats <- getRTSStats
    beforeCpu <- getCPUTime
    beforeWall <- getMonotonicTimeNSec
    pass
    afterWall <- getMonotonicTimeNSec
    afterCpu <- getCPUTime
    performGC
    afterStats <- getRTSStats
    count <- readIORef opened
    pure Sample
        { wallMillis =
            fromIntegral (afterWall - beforeWall) / 1.0e6
        , cpuMillis =
            fromIntegral (afterCpu - beforeCpu) / 1.0e9
        , allocatedBytes =
            fromIntegral
                (afterStats.allocated_bytes - beforeStats.allocated_bytes)
        , transactions = count
        }

median :: [Sample] -> Sample
median samples =
    Sample
        { wallMillis = middle (sort (map (.wallMillis) samples))
        , cpuMillis = middle (sort (map (.cpuMillis) samples))
        , allocatedBytes =
            middle (sort (map (.allocatedBytes) samples))
        , transactions = middle (sort (map (.transactions) samples))
        }
  where
    middle values = values !! (length values `div` 2)
