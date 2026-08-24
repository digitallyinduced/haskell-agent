module Main (main) where

import Agent.Cancel (newCancelFlag)
import Agent.Loop
    ( Backend(..)
    , BackendResult(..)
    , BackendStateStore(..)
    , LoopConfig(..)
    , LoopResult(..)
    , defaultLoopDispatch
    , defaultLoopMaxTurns
    , emptyTurnOutput
    , runLoop
    )
import Agent.ToolDispatch
    ( functionToolCall
    , noArgsTool
    )
import Agent.Tools.Scheduling
    ( ToolAccess(..)
    , ToolResource(..)
    , ToolResourceClaim(..)
    )
import Agent.Tools.Types
    ( AppTool
    , ApprovalRule(..)
    , ToolExecutionPolicy(..)
    , jsonAppToolWithExecution
    , mkToolRegistry
    , withToolResourceClaims
    )
import Control.Concurrent (threadDelay)
import Control.Exception (evaluate)
import Control.Monad (forM)
import Data.IORef
    ( atomicModifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
    ( RTSStats(..)
    , getRTSStats
    , getRTSStatsEnabled
    )
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (die)
import System.Mem (performGC)
import Text.Printf (printf)

data Workload
    = OldSequential
    | NewDisjoint
    | NewConflicting
    | ExistingParallel
    deriving (Eq, Show)

data Sample = Sample
    { wallMillis :: !Double
    , cpuMillis :: !Double
    , allocatedBytes :: !Integer
    }

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    if enabled
        then pure ()
        else die "RTS statistics are disabled; run with +RTS -T"
    getArgs >>= \case
        [workloadArg, callCountArg, delayArg, sampleCountArg] -> do
            workload <- parseWorkload workloadArg
            callCount <- parsePositive "call count" callCountArg
            delayMicros <- parseNonNegative "delay microseconds" delayArg
            sampleCount <- parsePositive "sample count" sampleCountArg
            samples <- forM [1 .. sampleCount] \_ ->
                measure (runWorkload workload callCount delayMicros)
            let result = median samples
            printf
                "%s,%d,%d,%.3f,%.3f,%d\n"
                workloadArg
                callCount
                delayMicros
                result.wallMillis
                result.cpuMillis
                result.allocatedBytes
        _ ->
            die $
                "usage: tool-scheduling-bench WORKLOAD CALLS DELAY_US SAMPLES\n"
                    <> "workloads: old-sequential, new-disjoint, "
                    <> "new-conflicting, existing-parallel"

parseWorkload :: String -> IO Workload
parseWorkload = \case
    "old-sequential" -> pure OldSequential
    "new-disjoint" -> pure NewDisjoint
    "new-conflicting" -> pure NewConflicting
    "existing-parallel" -> pure ExistingParallel
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

runWorkload :: Workload -> Int -> Int -> IO Int
runWorkload workload callCount delayMicros = do
    turnIndex <- newIORef (0 :: Int)
    state <- newIORef []
    cancel <- newCancelFlag
    let names =
            [ "tool-" <> Text.pack (show index)
            | index <- [1 .. callCount]
            ]
        calls =
            [ functionToolCall name name "{}"
            | name <- names
            ]
        backend = Backend \currentState _previous _inputs _onEvent -> do
            currentTurn <- atomicModifyIORef' turnIndex \index ->
                (index + 1, index)
            pure $ Right BackendResult
                { backendOutput =
                    if currentTurn == 0
                        then emptyTurnOutput "tools" calls Nothing
                        else emptyTurnOutput "done" [] (Just "done")
                , backendState = currentState
                }
        tools =
            zipWith
                (benchmarkTool workload delayMicros)
                [1 ..]
                names
        registry =
            either (error . Text.unpack) id (mkToolRegistry tools)
        config = LoopConfig
            { loopBackend = backend
            , loopBackendState = BackendStateStore
                { readBackendState = readIORef state
                , commitBackendState = writeIORef state
                }
            , loopTools = registry
            , loopDispatch = defaultLoopDispatch
            , loopMaxTurns = defaultLoopMaxTurns
            , loopOnEvent = \_ -> pure ()
            , loopApprove = \_ -> pure (Right True)
            , loopCancel = cancel
            }
    result <- runLoop config Nothing "benchmark"
    pure $ case result of
        Right LoopResult { turnsUsed } -> turnsUsed + callCount
        Left err -> error (show err)

benchmarkTool :: Workload -> Int -> Int -> Text -> AppTool
benchmarkTool workload delayMicros index name =
    addClaims $
        jsonAppToolWithExecution
            name
            ""
            []
            AlwaysReadOnly
            execution
            (noArgsTool name do
                threadDelay delayMicros
                pure (Right "ok"))
  where
    execution = case workload of
        ExistingParallel -> ParallelSafe
        _ -> TurnSequential
    addClaims = case workload of
        NewDisjoint ->
            withToolResourceClaims
                (const (pure (Right [writeClaim resourceName])))
        NewConflicting ->
            withToolResourceClaims
                (const (pure (Right [writeClaim "shared"])))
        _ -> id
    resourceName = "resource-" <> Text.pack (show index)
    writeClaim resource =
        ToolResourceClaim ToolWrite (ToolNamedResource resource)

measure :: IO Int -> IO Sample
measure action = do
    performGC
    beforeStats <- getRTSStats
    beforeCpu <- getCPUTime
    beforeWall <- getMonotonicTimeNSec
    result <- action
    _ <- evaluate result
    afterWall <- getMonotonicTimeNSec
    afterCpu <- getCPUTime
    performGC
    afterStats <- getRTSStats
    pure Sample
        { wallMillis =
            fromIntegral (afterWall - beforeWall) / 1.0e6
        , cpuMillis =
            fromIntegral (afterCpu - beforeCpu) / 1.0e9
        , allocatedBytes =
            fromIntegral
                (afterStats.allocated_bytes - beforeStats.allocated_bytes)
        }

median :: [Sample] -> Sample
median samples =
    Sample
        { wallMillis = middle (sort (map (.wallMillis) samples))
        , cpuMillis = middle (sort (map (.cpuMillis) samples))
        , allocatedBytes =
            middle (sort (map (.allocatedBytes) samples))
        }
  where
    middle values = values !! (length values `div` 2)
