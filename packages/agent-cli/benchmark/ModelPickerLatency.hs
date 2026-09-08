-- | Time to first usable rows and, separately, completed refresh publication.
-- The baseline preserves the removed model-refresh + bounded usage barrier.
-- Both workloads use the production picker to construct and publish rows.
module Main (main) where

import Agent.CLI.AgentViewport (AgentTarget(..))
import Agent.CLI.GatewayClient
    ( GatewayModel(..)
    , GatewayModelAccess
    , GatewayModelProtocol(..)
    , GatewayModelProvider(..)
    , cachedGatewayModels
    , fetchGatewayUsage
    , newGatewayModelAccessWithUsage
    , refreshGatewayModels
    )
import Agent.CLI.GatewayModels
    ( modelOptionsForGatewayModels, selectGatewayModelOption, withGatewayModelsForStartup )
import Agent.CLI.Models (ModelOption(..))
import Agent.CLI.Interrupt (CtrlCDecision(..))
import Agent.CLI.ModelConfig
    ( ModelCatalog
    , decodeModelConfig
    , organizationGatewayConnectionId
    , packagedModelCatalogPath
    )
import Agent.CLI.Session.Choices (modelChoiceWithEffort)
import Agent.CLI.TUI.App (newFullscreenInputBuffer, newFullscreenRuntime)
import Agent.CLI.TUI.Types
import Agent.CLI.Usage (formatModelUsageSummary)
import Agent.Concurrent (mapConcurrentlyBounded)
import Agent.Dialect (DialectId(..))
import Agent.OpenAI.Usage (UsageSnapshot(..), UsageLimit(..), UsageWindow(..))
import Agent.Provider (Provider(..))
import Agent.ReasoningEffort (ReasoningEffort(..))
import Agent.TUI.Model (initialUiState)
import Agent.TUI.Motion (MotionMode(..))
import Control.Concurrent (threadDelay, yield)
import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM
    ( atomically, putTMVar, readTVar, retry )
import Control.Exception (evaluate)
import Control.Monad (forM, unless, void, when)
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (toList)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (sort)
import Data.Text qualified as Text
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (die)
import System.Mem (performGC)
import System.Timeout (timeout)
import Text.Printf (printf)
import Text.Read (readMaybe)

data Sample = Sample
    { elapsedMilliseconds :: !Double
    , cpuMilliseconds :: !Double
    }

data PickerSample = PickerSample
    { firstList :: !Sample
    , completedRefresh :: !Sample
    }

main :: IO ()
main = do
    getArgs >>= \case
        [workload, modelsArgument, delayArgument, samplesArgument] -> do
            unless (workload `elem`
                ["blocking-baseline", "cached-first",
                 "startup-baseline", "startup-cached", "startup-cold"]) usage
            modelCount <- positive modelsArgument
            delayMilliseconds <- positive delayArgument
            sampleCount <- positive samplesArgument
            catalogPath <- packagedModelCatalogPath
            catalog <- LBS.readFile catalogPath >>=
                either (die . Text.unpack) pure . decodeModelConfig "packaged catalog"
            results <- forM [1 .. sampleCount] \_ -> do
                -- Models and the successful in-memory cache are constructed
                -- outside the interval, as they are before opening /model.
                let models =
                        [ GatewayModel
                            ("benchmark-model-" <> Text.pack (show index))
                            GatewayResponsesProtocol GatewayOpenAIProvider
                        | index <- [1 .. modelCount]
                        ]
                void (evaluate (sum (map (Text.length . (.gatewayModelId)) models)))
                if "startup-" `Text.isPrefixOf` Text.pack workload
                    then startupSample catalog workload models delayMilliseconds
                    else pickerSample catalog workload models modelCount delayMilliseconds
            printf "%s models=%d delay-ms=%d samples=%d first-list-ms=%.3f first-list-cpu-ms=%.3f complete-ms=%.3f complete-cpu-ms=%.3f\n"
                workload modelCount delayMilliseconds sampleCount
                (median (map (.firstList.elapsedMilliseconds) results))
                (median (map (.firstList.cpuMilliseconds) results))
                (median (map (.completedRefresh.elapsedMilliseconds) results))
                (median (map (.completedRefresh.cpuMilliseconds) results))
        _ -> usage

-- The production startup selector and scope, excluding unrelated startup work
-- and PostgreSQL hydration. A transport signal avoids polling during network
-- latency; only the short cache-publication handoff uses cooperative yielding.
startupSample :: ModelCatalog -> String -> [GatewayModel] -> Int -> IO PickerSample
startupSample catalog workload models delayMilliseconds = do
    unless (length models >= 2) (die "Startup workloads require at least two models")
    delayed <- newIORef False
    transported <- newEmptyMVar
    let refreshed = reverse models
        select available = selectGatewayModelOption
            (modelOptionsForGatewayModels catalog available)
            (Just "benchmark-model-1") Nothing []
    access <- newGatewayModelAccessWithUsage
        (readIORef delayed >>= \case
            False -> pure (Right models)
            True -> do
                threadDelay (delayMilliseconds * 1000)
                putMVar transported ()
                pure (Right refreshed))
        (const (pure (Left "Usage is outside startup catalog selection")))
    unless (workload == "startup-cold") $
        void (refreshGatewayModels access >>= either (die . Text.unpack) pure)
    writeIORef delayed True
    performGC
    started <- startMeasurement
    let continue result = do
            selected <- either (die . Text.unpack) pure result
            void (evaluate (length (show selected.modelTarget)))
            firstList <- finishMeasurement started
            takeMVar transported
            let awaitPublication = cachedGatewayModels access >>= \case
                    Just latest | latest == refreshed ->
                        void (evaluate (sum (map (Text.length . (.gatewayModelId)) latest)))
                    _ -> yield >> awaitPublication
            awaitPublication
            completedRefresh <- finishMeasurement started
            pure PickerSample { firstList, completedRefresh }
    timeout 10000000
        (if workload == "startup-baseline"
            then refreshGatewayModels access >>= continue . (>>= select)
            else withGatewayModelsForStartup access select continue)
        >>= maybe (die "Startup catalog did not finish within ten seconds") pure

pickerSample :: ModelCatalog -> String -> [GatewayModel] -> Int -> Int -> IO PickerSample
pickerSample catalog workload models modelCount delayMilliseconds = do
    delayEnabled <- newIORef False
    transportEnabled <- newIORef True
    let transportDelay = do
            enabled <- readIORef transportEnabled
            unless enabled (atomically retry)
            readIORef delayEnabled >>= \delayed ->
                when delayed (threadDelay (delayMilliseconds * 1000))
    access <- newGatewayModelAccessWithUsage
        (transportDelay >> pure (Right models))
        (\_ -> transportDelay >> pure (Right usageSnapshot))
    refreshGatewayModels access >>= either (die . Text.unpack) (const (pure ()))
    writeIORef delayEnabled True
    runtime <- newBenchmarkRuntime
    performGC
    started <- startMeasurement
    timeout 10000000 (do
        when (workload == "blocking-baseline") $
            blockingBaseline access >> writeIORef transportEnabled False
        observeFirstList catalog access runtime modelCount started
            (workload == "cached-first"))
        >>= maybe (die "Picker did not finish within ten seconds") pure

-- Equivalent to the removed barrier: model refresh followed by at most four
-- usage requests concurrently, with a two-second timeout for the whole group.
-- Every request incurs the configured transport latency.
blockingBaseline :: GatewayModelAccess -> IO ()
blockingBaseline access = do
    models <- refreshGatewayModels access >>= either (die . Text.unpack) pure
    void $ timeout 2000000 $
        mapConcurrentlyBounded 4
            (\model -> fetchGatewayUsage access model.gatewayModelId >>= \case
                Left _ -> pure ()
                Right snapshot -> void (evaluate
                    (maybe 0 Text.length (formatModelUsageSummary snapshot))))
            models

observeFirstList
    :: ModelCatalog -> GatewayModelAccess -> FullscreenRuntime -> Int
    -> (Word64, Integer) -> Bool -> IO PickerSample
observeFirstList catalog access runtime expectedCount started awaitRefresh =
    withAsync
        (modelChoiceWithEffort catalog (Just access) (Just runtime) False
            organizationGatewayConnectionId OpenAIProvider "benchmark-model-1"
            CodexDialect EffortHigh)
        \worker -> do
            (rows, reply) <- atomically do
                let AppEventMailbox mailbox = runtime.runtimeMailbox
                events <- (.mailboxPendingEvents) <$> readTVar mailbox
                case
                    [ (rows, reply)
                    | PendingEvent (AppAskDynamicAdjustableFilterChoice _ _ _ rows reply) <- toList events
                    , not (null rows)
                    ]
                    of
                        first : _ -> pure first
                        [] -> retry
            unless (length rows == expectedCount) $
                die "Picker did not publish the requested catalog size"
            void (evaluate (rowChecksum rows))
            firstList <- finishMeasurement started
            completedRefresh <- if not awaitRefresh
                then pure firstList
                else do
                    updates <- atomically do
                        let AppEventMailbox mailbox = runtime.runtimeMailbox
                        events <- (.mailboxPendingEvents) <$> readTVar mailbox
                        let updates =
                                [ rows
                                | PendingEvent (AppUpdateDynamicAdjustableFilterChoice target _ rows) <- toList events
                                , target == reply
                                ]
                        if length updates >= 2
                            && any (all (\(_, _, detail, _, _) ->
                                "\nUsage: " `Text.isInfixOf` detail)) updates
                            then pure updates
                            else retry
                    void (evaluate (sum (map rowChecksum updates)))
                    finishMeasurement started
            atomically (putTMVar reply Nothing)
            void (wait worker)
            pure PickerSample{firstList, completedRefresh}

rowChecksum :: [(Text.Text, Text.Text, Text.Text, [Text.Text], Int)] -> Int
rowChecksum rows = sum
    [ Text.length key + Text.length label + Text.length detail
        + sum (map Text.length adjustments) + initial
    | (key, label, detail, adjustments, initial) <- rows
    ]

usageSnapshot :: UsageSnapshot
usageSnapshot = UsageSnapshot
    "benchmark"
    (Just (UsageLimit True False
        (Just (UsageWindow 25 18000 9000 2000000000)) Nothing))
    []

newBenchmarkRuntime :: IO FullscreenRuntime
newBenchmarkRuntime = do
    input <- newFullscreenInputBuffer
    newFullscreenRuntime input
        (pure ()) (const (pure ())) (pure WarnExit)
        (const (pure True)) (const (pure ())) (const (pure ()))
        (pure (AgentRoot, [])) (const (pure ())) (pure ())
        (const (pure ())) MotionFull False initialUiState

startMeasurement :: IO (Word64, Integer)
startMeasurement = do
    beforeElapsed <- getMonotonicTimeNSec
    beforeCpu <- getCPUTime
    pure (beforeElapsed, beforeCpu)

finishMeasurement :: (Word64, Integer) -> IO Sample
finishMeasurement (beforeElapsed, beforeCpu) = do
    afterCpu <- getCPUTime
    afterElapsed <- getMonotonicTimeNSec
    pure Sample
        { elapsedMilliseconds = fromIntegral (afterElapsed - beforeElapsed) / 1e6
        , cpuMilliseconds = fromIntegral (afterCpu - beforeCpu) / 1e9
        }

median :: [Double] -> Double
median samples = sort samples !! (length samples `div` 2)

positive :: String -> IO Int
positive input = case readMaybe input of
    Just value | value > 0 -> pure value
    _ -> usage

usage :: IO a
usage = die "model-picker-latency-bench (blocking-baseline|cached-first|startup-baseline|startup-cached|startup-cold) MODEL_COUNT DELAY_MS SAMPLES"
