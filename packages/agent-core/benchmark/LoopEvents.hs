module Main (main) where

import Agent.Cancel (newCancelFlag)
import qualified Agent.Json.Decode as Json
import Agent.Loop
    ( Backend(..)
    , BackendResult(..)
    , BackendStateStore(..)
    , LoopConfig(..)
    , LoopEvent(..)
    , LoopResult
    , defaultLoopDispatch
    , defaultLoopMaxTurns
    , emptyTurnOutput
    , runLoop
    )
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallResult(..)
    , functionToolCall
    , typedStreamingTool
    )
import Agent.Tools.Types
    ( ApprovalRule(..)
    , ToolExecutionPolicy(..)
    , ToolRegistry
    , jsonAppToolWithExecution
    , mkToolRegistry
    )
import Control.Concurrent (threadDelay)
import Control.Exception (evaluate)
import Control.Monad (forM, replicateM_)
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
import Data.List (sort)
import qualified Data.Text as Text
import Data.Word (Word64)
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
    = StreamingEvents
    | ParallelToolEvents
    | QueuedEvents

data EventArgs = EventArgs
    { count :: !Int
    }

eventArgsDecoder :: Json.Decoder EventArgs
eventArgsDecoder = Json.object $
    EventArgs <$> Json.atKey "count" Json.int

data WorkloadResult = WorkloadResult
    { resultChecksum :: !Int
    , resultProducerFinished :: !Word64
    }

data Sample = Sample
    { totalWallMillis :: !Double
    , producerWallMillis :: !Double
    , cpuMillis :: !Double
    , allocatedBytes :: !Integer
    }

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    if not enabled
        then die "RTS statistics are disabled; run with +RTS -T"
        else pure ()
    getArgs >>= \case
        [workloadArg, eventCountArg, sinkDelayArg, sampleCountArg] -> do
            workload <- parseWorkload workloadArg
            eventCount <- parsePositive "event count" eventCountArg
            sinkDelayMicros <- parseNonNegative
                "sink delay microseconds" sinkDelayArg
            sampleCount <- parseOddPositive "sample count" sampleCountArg
            samples <- forM [1 .. sampleCount] \_ -> do
                performGC
                measure workload eventCount sinkDelayMicros
            let sample = median samples
            printf
                "%s,%d,%d,%.3f,%.3f,%.3f,%d\n"
                workloadArg
                eventCount
                sinkDelayMicros
                sample.totalWallMillis
                sample.producerWallMillis
                sample.cpuMillis
                sample.allocatedBytes
        _ ->
            die $
                "usage: loop-events-bench WORKLOAD EVENTS SINK_DELAY_US SAMPLES\n"
                    <> "workloads: streaming, parallel-tools, queued-events"

parseWorkload :: String -> IO Workload
parseWorkload = \case
    "streaming" -> pure StreamingEvents
    "parallel-tools" -> pure ParallelToolEvents
    "queued-events" -> pure QueuedEvents
    other -> die ("unknown workload: " <> other)

parsePositive :: String -> String -> IO Int
parsePositive label raw =
    case reads raw of
        [(value, "")]
            | value > 0 -> pure value
        _ -> die ("invalid " <> label <> ": " <> raw)

parseOddPositive :: String -> String -> IO Int
parseOddPositive label raw = do
    value <- parsePositive label raw
    if odd value
        then pure value
        else die (label <> " must be odd: " <> raw)

parseNonNegative :: String -> String -> IO Int
parseNonNegative label raw =
    case reads raw of
        [(value, "")]
            | value >= 0 -> pure value
        _ -> die ("invalid " <> label <> ": " <> raw)

measure :: Workload -> Int -> Int -> IO Sample
measure workload eventCount sinkDelayMicros = do
    beforeStats <- getRTSStats
    wallBefore <- getMonotonicTimeNSec
    cpuBefore <- getCPUTime
    result <- runWorkload workload eventCount sinkDelayMicros
    _ <- evaluate
        (result.resultChecksum + fromIntegral result.resultProducerFinished)
    cpuAfter <- getCPUTime
    wallAfter <- getMonotonicTimeNSec
    performGC
    afterStats <- getRTSStats
    pure Sample
        { totalWallMillis =
            nanosToMillis (wallAfter - wallBefore)
        , producerWallMillis =
            nanosToMillis
                (fromIntegral result.resultProducerFinished - wallBefore)
        , cpuMillis =
            fromIntegral (cpuAfter - cpuBefore) / 1.0e9
        , allocatedBytes =
            fromIntegral
                (afterStats.allocated_bytes - beforeStats.allocated_bytes)
        }

runWorkload :: Workload -> Int -> Int -> IO WorkloadResult
runWorkload workload eventCount sinkDelayMicros = do
    checksumRef <- newIORef 0
    producerFinishedRef <- newIORef Nothing
    stateRef <- newIORef []
    cancel <- newCancelFlag
    backend <- case workload of
        StreamingEvents ->
            pure $ streamingBackend eventCount producerFinishedRef
        ParallelToolEvents ->
            parallelToolBackend eventCount producerFinishedRef
        QueuedEvents ->
            pure $ queuedEventsBackend eventCount producerFinishedRef
    let sink event = do
            if sinkDelayMicros > 0
                then threadDelay sinkDelayMicros
                else pure ()
            atomicModifyIORef' checksumRef \checksum ->
                (checksum + eventWeight event, ())
        config = LoopConfig
            { loopBackend = backend
            , loopBackendState = BackendStateStore
                { readBackendState = readIORef stateRef
                , commitBackendState = writeIORef stateRef
                }
            , loopTools = case workload of
                StreamingEvents -> emptyRegistry
                ParallelToolEvents -> streamingRegistry
                QueuedEvents -> emptyRegistry
            , loopDispatch = defaultLoopDispatch
            , loopMaxTurns = defaultLoopMaxTurns
            , loopOnEvent = sink
            , loopApprove = \_ -> pure (Right True)
            , loopReadSteering = pure []
            , loopCommitSteering = \_ -> pure ()
            , loopCancel = cancel
            }
    result <- runLoop config Nothing "benchmark"
    forceSuccess result
    checksum <- readIORef checksumRef
    producerFinished <- readIORef producerFinishedRef >>= \case
        Just timestamp -> pure timestamp
        Nothing -> die "benchmark producer did not record completion"
    pure WorkloadResult
        { resultChecksum = checksum
        , resultProducerFinished = producerFinished
        }

streamingBackend
    :: Int
    -> IORef (Maybe Word64)
    -> Backend
streamingBackend eventCount producerFinishedRef =
    Backend \_state _previous _inputs onEvent -> do
        replicateM_ eventCount (onEvent (TextDelta "x"))
        getMonotonicTimeNSec >>= writeIORef producerFinishedRef . Just
        pure . Right $ BackendResult
            { backendOutput =
                emptyTurnOutput "stream-response" [] (Just "done")
            , backendState = []
            }

queuedEventsBackend
    :: Int
    -> IORef (Maybe Word64)
    -> Backend
queuedEventsBackend eventCount producerFinishedRef =
    Backend \_state _previous _inputs onEvent -> do
        replicateM_ eventCount (onEvent (WarningRaised "x"))
        getMonotonicTimeNSec >>= writeIORef producerFinishedRef . Just
        pure . Right $ BackendResult
            { backendOutput =
                emptyTurnOutput "queued-response" [] (Just "done")
            , backendState = []
            }

parallelToolBackend
    :: Int
    -> IORef (Maybe Word64)
    -> IO Backend
parallelToolBackend eventCount producerFinishedRef = do
    submissionRef <- newIORef (0 :: Int)
    let counts = distributeEvents parallelToolCount eventCount
        calls =
            [ functionToolCall
                ("call-" <> Text.pack (show index))
                "stream-events"
                ("{\"count\":" <> Text.pack (show count) <> "}")
            | (index, count) <- zip [1 :: Int ..] counts
            ]
    pure $ Backend \_state _previous _inputs _onEvent -> do
        submission <- atomicModifyIORef' submissionRef \current ->
            let next = current + 1
            in (next, next)
        case submission of
            1 ->
                pure . Right $ BackendResult
                    { backendOutput =
                        emptyTurnOutput "tool-response" calls Nothing
                    , backendState = []
                    }
            2 -> do
                getMonotonicTimeNSec >>= writeIORef producerFinishedRef . Just
                pure . Right $ BackendResult
                    { backendOutput =
                        emptyTurnOutput "final-response" [] (Just "done")
                    , backendState = []
                    }
            _ -> die "parallel tool benchmark submitted too many turns"

parallelToolCount :: Int
parallelToolCount = 4

distributeEvents :: Int -> Int -> [Int]
distributeEvents buckets total =
    [ quotient + if index <= remainder then 1 else 0
    | index <- [1 .. buckets]
    ]
  where
    (quotient, remainder) = total `quotRem` buckets

streamingRegistry :: ToolRegistry
streamingRegistry =
    either (error . Text.unpack) id $
        mkToolRegistry
            [ jsonAppToolWithExecution
                "stream-events"
                ""
                []
                AlwaysReadOnly
                ParallelSafe
                (typedStreamingTool "stream-events" eventArgsDecoder \emit EventArgs{count} -> do
                    mapM_
                        (\size -> emit (Text.replicate size "x"))
                        [1 .. count]
                    pure (Right "done"))
            ]

emptyRegistry :: ToolRegistry
emptyRegistry =
    either (error . Text.unpack) id (mkToolRegistry [])

forceSuccess :: Either error LoopResult -> IO ()
forceSuccess = \case
    Right result -> result `seq` pure ()
    Left _ -> die "agent loop benchmark failed"

eventWeight :: LoopEvent -> Int
eventWeight = \case
    TextDelta text -> Text.length text
    ReasoningDelta text -> Text.length text
    ActivityUpdated text -> Text.length text
    WarningRaised text -> Text.length text
    ResponseRestarted text -> Text.length text
    TurnStarted -> 1
    TurnFinished _ -> 1
    ToolStarted call -> Text.length call.callId
    ToolUpdated call -> Text.length call.callId
    ToolArgumentsUpdated call -> Text.length call.callId
    ToolRetracted callId -> Text.length callId
    ResponseAttemptDiscarded -> 1
    NativeAgentStarted identifier parent label model ->
        sum
            [ Text.length identifier
            , maybe 0 Text.length parent
            , Text.length label
            , maybe 0 Text.length model
            ]
    NativeAgentOutput identifier output ->
        Text.length identifier + Text.length output
    NativeAgentFinished identifier _ -> Text.length identifier
    ToolOutputUpdated callId output ->
        Text.length callId + Text.length output
    ToolFinished result ->
        Text.length result.callId + Text.length result.output

nanosToMillis :: Integral value => value -> Double
nanosToMillis value =
    fromIntegral value / 1000000

median :: [Sample] -> Sample
median samples =
    Sample
        { totalWallMillis =
            middle (sort (map (.totalWallMillis) samples))
        , producerWallMillis =
            middle (sort (map (.producerWallMillis) samples))
        , cpuMillis =
            middle (sort (map (.cpuMillis) samples))
        , allocatedBytes =
            middle (sort (map (.allocatedBytes) samples))
        }

middle :: [a] -> a
middle values = values !! (length values `div` 2)
