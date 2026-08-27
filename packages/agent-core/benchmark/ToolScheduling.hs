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
    , ToolSchedulingPlan(..)
    , toolSchedulingWaves
    , toolSchedulingWavesLegacy
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
import System.OsPath (unsafeEncodeUtf)
import Text.Printf (printf)

data Workload
    = OldSequential
    | NewDisjoint
    | NewDisjointPaths
    | NewConflicting
    | ExistingParallel
    | LegacyDisjoint
    | IndexedDisjoint
    | LegacyConflicting
    | IndexedConflicting
    | LegacyPath
    | IndexedPath
    | LegacyMixed
    | IndexedMixed
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
            let preparedSamples =
                    [ schedulingBenchmark workload callCount sampleSeed
                    | sampleSeed <- [1 .. sampleCount]
                    ]
            validateSchedulingBenchmark preparedSamples
            samples <- forM
                (zip [1 :: Int ..] preparedSamples)
                \(sampleSeed, prepared) ->
                measure
                    (runWorkload
                        workload
                        callCount
                        delayMicros
                        sampleSeed
                        prepared)
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
                    <> "new-disjoint-paths, new-conflicting, existing-parallel"
                    <> ", legacy-disjoint, indexed-disjoint, "
                    <> "legacy-conflicting, indexed-conflicting, "
                    <> "legacy-path, indexed-path, legacy-mixed, indexed-mixed"

parseWorkload :: String -> IO Workload
parseWorkload = \case
    "old-sequential" -> pure OldSequential
    "new-disjoint" -> pure NewDisjoint
    "new-disjoint-paths" -> pure NewDisjointPaths
    "new-conflicting" -> pure NewConflicting
    "existing-parallel" -> pure ExistingParallel
    "legacy-disjoint" -> pure LegacyDisjoint
    "indexed-disjoint" -> pure IndexedDisjoint
    "legacy-conflicting" -> pure LegacyConflicting
    "indexed-conflicting" -> pure IndexedConflicting
    "legacy-path" -> pure LegacyPath
    "indexed-path" -> pure IndexedPath
    "legacy-mixed" -> pure LegacyMixed
    "indexed-mixed" -> pure IndexedMixed
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

runWorkload
    :: Workload
    -> Int
    -> Int
    -> Int
    -> Maybe (Scheduler, SchedulingInput)
    -> IO Int
runWorkload workload callCount delayMicros sampleSeed prepared
    | Just (implementation, input) <- prepared =
        pure
            (waveChecksum (implementation input)
                + sampleSeed)
    | otherwise = do
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
            , loopReadSteering = pure []
            , loopCommitSteering = \_ -> pure ()
            , loopCancel = cancel
            }
    result <- runLoop config Nothing "benchmark"
    pure $ case result of
        Right LoopResult { turnsUsed } -> turnsUsed + callCount
        Left err -> error (show err)

type Scheduler =
    [(Int, ToolSchedulingPlan)] -> [[Int]]

type SchedulingInput = [(Int, ToolSchedulingPlan)]

schedulingBenchmark
    :: Workload
    -> Int
    -> Int
    -> Maybe (Scheduler, SchedulingInput)
schedulingBenchmark workload count sampleSeed =
    case workload of
        LegacyDisjoint -> benchmark toolSchedulingWavesLegacy disjointPlans
        IndexedDisjoint -> benchmark toolSchedulingWaves disjointPlans
        LegacyConflicting -> benchmark toolSchedulingWavesLegacy conflictingPlans
        IndexedConflicting -> benchmark toolSchedulingWaves conflictingPlans
        LegacyPath -> benchmark toolSchedulingWavesLegacy pathPlans
        IndexedPath -> benchmark toolSchedulingWaves pathPlans
        LegacyMixed -> benchmark toolSchedulingWavesLegacy mixedPlans
        IndexedMixed -> benchmark toolSchedulingWaves mixedPlans
        _ -> Nothing
  where
    benchmark implementation plans =
        Just
            ( implementation
            , zip [0 :: Int ..] (take count plans)
            )
    suffix = "-sample-" <> Text.pack (show sampleSeed)
    disjointPlans =
        [ ToolResourceClaims
            [namedClaim ToolWrite ("resource-" <> number <> suffix)]
        | index <- [1 :: Int ..]
        , let number = Text.pack (show index)
        ]
    conflictingPlans =
        repeat
            (ToolResourceClaims
                [namedClaim ToolWrite ("shared" <> suffix)])
    pathPlans =
        [ ToolResourceClaims
            [ ToolResourceClaim ToolWrite
                (ToolPath (unsafeEncodeUtf
                    ("/workspace-" <> show sampleSeed
                        <> "/file-" <> show index)))
            ]
        | index <- [1 :: Int ..]
        ]
    mixedPlans =
        [ mixedPlan index
        | index <- [1 :: Int ..]
        ]
    mixedPlan index =
        case index `mod` 11 of
            0 -> ToolExclusive
            1 -> ToolUnconstrained
            2 -> ToolResourceClaims
                [ToolResourceClaim ToolRead ToolAllPaths]
            3 -> ToolResourceClaims
                [ToolResourceClaim ToolWrite
                    (ToolPathTree
                        (unsafeEncodeUtf
                            ("/workspace-" <> show sampleSeed <> "/src")))]
            4 -> ToolResourceClaims
                [ToolResourceClaim ToolRead
                    (ToolPath (unsafeEncodeUtf
                        ("/workspace-" <> show sampleSeed
                            <> "/src/File-" <> show (index `mod` 17))))]
            _ -> ToolResourceClaims
                [namedClaim
                    (if even index then ToolRead else ToolWrite)
                    ("resource-" <> Text.pack (show (index `mod` 23))
                        <> suffix)]

validateSchedulingBenchmark
    :: [Maybe (Scheduler, SchedulingInput)]
    -> IO ()
validateSchedulingBenchmark =
    mapM_ validate
  where
    validate Nothing = pure ()
    validate (Just (_, input)) =
        let legacy = toolSchedulingWavesLegacy input
            indexed = toolSchedulingWaves input
        in if legacy == indexed
            then evaluate (waveChecksum legacy) >> pure ()
            else die "legacy and indexed scheduling differ"

namedClaim :: ToolAccess -> Text -> ToolResourceClaim
namedClaim access =
    ToolResourceClaim access . ToolNamedResource

waveChecksum :: [[Int]] -> Int
waveChecksum =
    foldl'
        (\checksum (wave, values) ->
            foldl'
                (\acc (position, value) ->
                    acc * 16777619
                        + wave * 65537
                        + position * 257
                        + value)
                (checksum * 31 + wave)
                (zip [1 :: Int ..] values))
        2166136261
        . zip [1 :: Int ..]

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
        NewDisjointPaths ->
            withToolResourceClaims
                (const (pure (Right [pathWriteClaim index])))
        NewConflicting ->
            withToolResourceClaims
                (const (pure (Right [writeClaim "shared"])))
        _ -> id
    resourceName = "resource-" <> Text.pack (show index)
    writeClaim resource =
        ToolResourceClaim ToolWrite (ToolNamedResource resource)
    pathWriteClaim n =
        ToolResourceClaim ToolWrite
            (ToolPath (unsafeEncodeUtf ("file-" <> show n)))

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
