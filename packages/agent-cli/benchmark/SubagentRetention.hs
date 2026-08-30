module Main (main) where

import Agent.CLI.AgentViewport (AgentEntry(..), AgentTarget(..))
import Agent.CLI.Compaction (OccupancySnapshot(..), estimatedOccupancy)
import Agent.CLI.NativeAgents
import Agent.CLI.Subagents.Runtime (SubagentResidency(..), SubagentSession(..))
import Agent.Dialect (DialectId(..))
import Agent.Loop
    ( BackendSnapshot(..)
    , LoopEvent(..)
    , NativeAgentStatus(..)
    , emptyBackendSnapshot
    , initialBackendSnapshot
    )
import Agent.Provider (Provider(..))
import Agent.Responses.Types
import Control.Concurrent.MVar (modifyMVar_, newMVar)
import Control.Exception (evaluate)
import Control.Monad (forM, forM_)
import Data.IORef
import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
    ( GCDetails(..)
    , RTSStats(..)
    , getRTSStats
    , getRTSStatsEnabled
    )
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (die)
import System.Mem (performGC)
import Text.Printf (printf)

data Workload
    = Retained
    | Evicted
    | LegacyNative
    | BoundedNative
    deriving (Eq, Show)

data Sample = Sample
    { elapsedMillis :: !Double
    , cpuMillis :: !Double
    , allocatedBytes :: !Integer
    , liveBytes :: !Integer
    }

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    if enabled
        then pure ()
        else die "RTS statistics are disabled; run with +RTS -T"
    getArgs >>= \case
        [] -> runMatrix
        [workloadArg, agentCountArg, itemCountArg, payloadBytesArg, sampleCountArg] -> do
            workload <- parseWorkload workloadArg
            agentCount <- parsePositive "agent count" agentCountArg
            itemCount <- parsePositive "items per agent" itemCountArg
            payloadBytes <- parsePositive "payload bytes" payloadBytesArg
            sampleCount <- parsePositive "sample count" sampleCountArg
            samples <- forM [1 .. sampleCount] \sampleIndex ->
                measure workload agentCount itemCount payloadBytes sampleIndex
            let medianSample = median samples
            printf
                "%s,%d,%d,%d,%.3f,%.3f,%d,%d\n"
                workloadArg
                agentCount
                itemCount
                payloadBytes
                medianSample.elapsedMillis
                medianSample.cpuMillis
                medianSample.allocatedBytes
                medianSample.liveBytes
        _ ->
            die $
                "usage: subagent-retention-bench WORKLOAD AGENTS ITEMS "
                    <> "PAYLOAD_BYTES SAMPLES\n"
                    <> "workloads: retained, evicted, legacy-native, bounded-native\n"
                    <> "output: workload,agents,items,payload_bytes,"
                    <> "elapsed_ms,cpu_ms,allocated_bytes,live_bytes"

runMatrix :: IO ()
runMatrix = do
    putStrLn
        "workload,agents,items,payload_bytes,elapsed_ms,cpu_ms,allocated_bytes,live_bytes"
    forM_ [8, 32, 64] \agentCount ->
        forM_ [Retained, Evicted] \workload -> do
            -- Warm allocation and GC paths before collecting the five samples.
            _ <- measure workload agentCount itemCount payloadBytes 0
            samples <- forM [1 .. sampleCount] \sampleIndex ->
                measure workload agentCount itemCount payloadBytes sampleIndex
            printSample workload agentCount itemCount payloadBytes (median samples)
    forM_ [LegacyNative, BoundedNative] \workload -> do
        _ <- measure workload 32 itemCount payloadBytes 0
        samples <- forM [1 .. sampleCount] \sampleIndex ->
            measure workload 32 itemCount payloadBytes sampleIndex
        printSample workload 32 itemCount payloadBytes (median samples)
  where
    itemCount = 64
    payloadBytes = 16 * 1024
    sampleCount = 5

parseWorkload :: String -> IO Workload
parseWorkload = \case
    "retained" -> pure Retained
    "evicted" -> pure Evicted
    "legacy-native" -> pure LegacyNative
    "bounded-native" -> pure BoundedNative
    other -> die ("unknown workload: " <> other)

parsePositive :: String -> String -> IO Int
parsePositive label raw =
    case reads raw of
        [(value, "")]
            | value > 0 -> pure value
        _ -> die ("invalid " <> label <> ": " <> raw)

measure :: Workload -> Int -> Int -> Int -> Int -> IO Sample
measure LegacyNative agentCount itemCount payloadBytes sampleIndex =
    measureNative False agentCount itemCount payloadBytes sampleIndex
measure BoundedNative agentCount itemCount payloadBytes sampleIndex =
    measureNative True agentCount itemCount payloadBytes sampleIndex
measure workload agentCount itemCount payloadBytes sampleIndex = do
    performGC
    beforeStats <- getRTSStats
    beforeCpu <- getCPUTime
    beforeElapsed <- getMonotonicTimeNSec
    sessions <-
        buildSessions agentCount itemCount payloadBytes sampleIndex
    residentChecksum <- checksumSessions sessions
    _ <- evaluate residentChecksum
    if workload == Evicted
        then evictSessions sessions
        else pure ()
    performGC
    afterStats <- getRTSStats
    -- This future traversal keeps the stable session shells live through the
    -- measured GC; retained mode also keeps every transcript payload live.
    finalChecksum <- checksumSessions sessions
    _ <- evaluate finalChecksum
    afterElapsed <- getMonotonicTimeNSec
    afterCpu <- getCPUTime
    pure Sample
        { elapsedMillis =
            fromIntegral (afterElapsed - beforeElapsed) / 1.0e6
        , cpuMillis =
            fromIntegral (afterCpu - beforeCpu) / 1.0e9
        , allocatedBytes =
            fromIntegral
                (afterStats.allocated_bytes - beforeStats.allocated_bytes)
        , liveBytes =
            fromIntegral afterStats.gc.gcdetails_live_bytes
        }

data LegacyNativeView = LegacyNativeView
    { legacyChunks :: ![Text]
    , legacyRendered :: !Text
    }

-- The legacy workload models the removed representation: every output append
-- copied both a transcript list and the fully rendered conversation. The
-- bounded workload runs the production NativeAgentStore path.
measureNative :: Bool -> Int -> Int -> Int -> Int -> IO Sample
measureNative bounded agentCount itemCount payloadBytes sampleIndex = do
    performGC
    beforeStats <- getRTSStats
    beforeCpu <- getCPUTime
    beforeElapsed <- getMonotonicTimeNSec
    (checksum, residentChecksum) <-
        if bounded
            then do
                let store =
                        buildBoundedNative
                            agentCount itemCount payloadBytes sampleIndex
                first <- evaluate (checksumNativeStore store)
                pure (first, evaluate (checksumNativeStore store))
            else do
                let views =
                        buildLegacyNative
                            agentCount itemCount payloadBytes sampleIndex
                first <- evaluate (checksumLegacyNative views)
                pure (first, evaluate (checksumLegacyNative views))
    _ <- evaluate checksum
    performGC
    finalChecksum <- residentChecksum
    _ <- evaluate finalChecksum
    afterStats <- getRTSStats
    afterElapsed <- getMonotonicTimeNSec
    afterCpu <- getCPUTime
    pure Sample
        { elapsedMillis =
            fromIntegral (afterElapsed - beforeElapsed) / 1.0e6
        , cpuMillis =
            fromIntegral (afterCpu - beforeCpu) / 1.0e9
        , allocatedBytes =
            fromIntegral
                (afterStats.allocated_bytes - beforeStats.allocated_bytes)
        , liveBytes =
            fromIntegral afterStats.gc.gcdetails_live_bytes
        }

buildLegacyNative :: Int -> Int -> Int -> Int -> Map Int LegacyNativeView
buildLegacyNative agentCount itemCount payloadBytes sampleIndex =
    Map.fromList
        [ (agentIndex, foldl' append empty chunks)
        | agentIndex <- [1 .. agentCount]
        , let chunks =
                [ itemText sampleIndex agentIndex itemIndex payloadBytes
                | itemIndex <- [1 .. itemCount]
                ]
        ]
  where
    empty = LegacyNativeView [] ""
    append view chunk =
        LegacyNativeView
            { legacyChunks = view.legacyChunks <> [chunk]
            , legacyRendered = view.legacyRendered <> chunk
            }

buildBoundedNative :: Int -> Int -> Int -> Int -> NativeAgentStore
buildBoundedNative agentCount itemCount payloadBytes sampleIndex =
    foldl' addAgent emptyNativeAgentStore [1 .. agentCount]
  where
    addAgent store agentIndex =
        let identifier = Text.pack ("native-" <> show agentIndex)
            started =
                applyNativeAgentEvent
                    (NativeAgentStarted identifier Nothing identifier Nothing)
                    store
            withOutput =
                foldl'
                    (\current itemIndex ->
                        applyNativeAgentEvent
                            (NativeAgentOutput identifier
                                (itemText sampleIndex agentIndex itemIndex payloadBytes))
                            current)
                    started
                    [1 .. itemCount]
        in applyNativeAgentEvent
            (NativeAgentFinished identifier NativeAgentCompleted)
            withOutput

checksumLegacyNative :: Map Int LegacyNativeView -> Int
checksumLegacyNative =
    Map.foldlWithKey'
        (\checksum key view ->
            Text.foldl'
                (\value character -> value * 33 + fromEnum character)
                (checksum * 33 + key + length view.legacyChunks)
                view.legacyRendered)
        5381

checksumNativeStore :: NativeAgentStore -> Int
checksumNativeStore store =
    foldl'
        (\checksum entry ->
            foldl'
                (\value line -> Text.length line + value * 33)
                (checksum * 33 + Text.length entry.agentPath)
                entry.agentTranscript)
        (nativeAgentStoreBytes store)
        (nativeAgentEntries AgentRoot store)

buildSessions :: Int -> Int -> Int -> Int -> IO (Map Int SubagentSession)
buildSessions agentCount itemCount payloadBytes sampleIndex =
    Map.fromList <$> forM [1 .. agentCount] \agentIndex -> do
        let items =
                [ messageItem
                    (itemText sampleIndex agentIndex itemIndex payloadBytes)
                | itemIndex <- [1 .. itemCount]
                ]
        transcript <- newIORef (initialBackendSnapshot items)
        contextTokens <- newIORef (Just (estimatedOccupancy itemCount payloadBytes))
        residency <- newMVar SessionResident
        pure
            ( agentIndex
            , SubagentSession
                { subSessionTranscript = transcript
                , subSessionContextTokens = contextTokens
                , subSessionProvider = OpenAIProvider
                , subSessionConnection = "openai"
                , subSessionEffectiveModel = "gpt-5.6-luna"
                , subSessionDialect = CodexDialect
                , subSessionResidency = residency
                }
            )

evictSessions :: Map Int SubagentSession -> IO ()
evictSessions sessions =
    forM_ (Map.elems sessions) \session ->
        modifyMVar_ session.subSessionResidency \_ -> do
            writeIORef session.subSessionTranscript emptyBackendSnapshot
            writeIORef session.subSessionContextTokens Nothing
            pure SessionEvicted

checksumSessions :: Map Int SubagentSession -> IO Int
checksumSessions sessions =
    foldMStrict 5381 (Map.toList sessions) \checksum (agentIndex, session) -> do
        snapshot <- readIORef session.subSessionTranscript
        context <- readIORef session.subSessionContextTokens
        pure $
            foldl'
                checksumItem
                ( checksum * 33 + agentIndex
                    + maybe 0
                        (\snapshot ->
                            snapshot.occupancyTokens + snapshot.occupancyLength)
                        context
                )
                snapshot.backendItems

checksumItem :: Int -> ResponseItem -> Int
checksumItem checksum = \case
    MessageItem message ->
        Text.foldl'
            (\value character -> value * 33 + fromEnum character)
            checksum
            (messageText message.content)
    _ -> checksum * 33 + 1

messageText :: MessageContent -> Text
messageText = \case
    MessageContentText text -> text
    MessageContentParts parts ->
        Text.concat
            [ text
            | InputTextPart { text } <- parts
            ]

messageItem :: Text -> ResponseItem
messageItem text = MessageItem ResponseMessage
    { messageId = Nothing
    , content = MessageContentText text
    , role = RoleAssistant
    , status = Nothing
    , phase = Nothing
    , passthrough = Nothing
    }

itemText :: Int -> Int -> Int -> Int -> Text
itemText sampleIndex agentIndex itemIndex payloadBytes =
    Text.pack
        (show sampleIndex <> ":" <> show agentIndex <> ":" <> show itemIndex <> ":")
        <> Text.replicate payloadBytes "x"

foldMStrict :: b -> [a] -> (b -> a -> IO b) -> IO b
foldMStrict initial values step = go initial values
  where
    go accumulator = \case
        [] -> pure accumulator
        value : rest -> do
            next <- step accumulator value
            next `seq` go next rest

median :: [Sample] -> Sample
median samples =
    Sample
        { elapsedMillis = middle (sort (map (.elapsedMillis) samples))
        , cpuMillis = middle (sort (map (.cpuMillis) samples))
        , allocatedBytes = middle (sort (map (.allocatedBytes) samples))
        , liveBytes = middle (sort (map (.liveBytes) samples))
        }
  where
    middle values = values !! (length values `div` 2)

printSample :: Workload -> Int -> Int -> Int -> Sample -> IO ()
printSample workload agentCount itemCount payloadBytes sample =
    printf
        "%s,%d,%d,%d,%.3f,%.3f,%d,%d\n"
        (case workload of
            Retained -> "retained"
            Evicted -> "evicted"
            LegacyNative -> "legacy-native"
            BoundedNative -> "bounded-native" :: String)
        agentCount
        itemCount
        payloadBytes
        sample.elapsedMillis
        sample.cpuMillis
        sample.allocatedBytes
        sample.liveBytes
