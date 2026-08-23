module Main (main) where

import Agent.CLI.Subagents.Runtime (SubagentSession(..))
import Agent.Dialect (DialectId(..))
import Agent.Provider (Provider(..))
import Agent.Responses.Types
import Control.Concurrent.MVar (modifyMVar_, newMVar)
import Control.Exception (evaluate)
import Control.Monad (forM, forM_)
import qualified Data.Aeson.KeyMap as KeyMap
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
                    <> "workloads: retained, evicted\n"
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
  where
    itemCount = 64
    payloadBytes = 16 * 1024
    sampleCount = 5

parseWorkload :: String -> IO Workload
parseWorkload = \case
    "retained" -> pure Retained
    "evicted" -> pure Evicted
    other -> die ("unknown workload: " <> other)

parsePositive :: String -> String -> IO Int
parsePositive label raw =
    case reads raw of
        [(value, "")]
            | value > 0 -> pure value
        _ -> die ("invalid " <> label <> ": " <> raw)

measure :: Workload -> Int -> Int -> Int -> Int -> IO Sample
measure workload agentCount itemCount payloadBytes sampleIndex = do
    performGC
    beforeStats <- getRTSStats
    beforeCpu <- getCPUTime
    beforeElapsed <- getMonotonicTimeNSec
    sessions <-
        buildSessions agentCount itemCount payloadBytes sampleIndex
    residentChecksum <- checksumSessions sessions
    _ <- evaluate residentChecksum
    case workload of
        Retained -> pure ()
        Evicted -> evictSessions sessions
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

buildSessions :: Int -> Int -> Int -> Int -> IO (Map Int SubagentSession)
buildSessions agentCount itemCount payloadBytes sampleIndex =
    Map.fromList <$> forM [1 .. agentCount] \agentIndex -> do
        let items =
                [ messageItem
                    (itemText sampleIndex agentIndex itemIndex payloadBytes)
                | itemIndex <- [1 .. itemCount]
                ]
        transcript <- newIORef items
        contextTokens <- newIORef (Just (itemCount, payloadBytes))
        pinned <- newIORef False
        hydrated <- newMVar True
        pure
            ( agentIndex
            , SubagentSession
                { subSessionTranscript = transcript
                , subSessionContextTokens = contextTokens
                , subSessionProvider = OpenAIProvider
                , subSessionEffectiveModel = "gpt-5.6-luna"
                , subSessionDialect = CodexDialect
                , subSessionPinned = pinned
                , subSessionHydrated = hydrated
                }
            )

evictSessions :: Map Int SubagentSession -> IO ()
evictSessions sessions =
    forM_ (Map.elems sessions) \session ->
        modifyMVar_ session.subSessionHydrated \_ -> do
            writeIORef session.subSessionTranscript []
            writeIORef session.subSessionContextTokens Nothing
            pure False

checksumSessions :: Map Int SubagentSession -> IO Int
checksumSessions sessions =
    foldMStrict 5381 (Map.toList sessions) \checksum (agentIndex, session) -> do
        items <- readIORef session.subSessionTranscript
        context <- readIORef session.subSessionContextTokens
        pure $
            foldl'
                checksumItem
                (checksum * 33 + agentIndex + maybe 0 (uncurry (+)) context)
                items

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
    , extraFields = KeyMap.empty
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
        (case workload of Retained -> "retained"; Evicted -> "evicted" :: String)
        agentCount
        itemCount
        payloadBytes
        sample.elapsedMillis
        sample.cpuMillis
        sample.allocatedBytes
        sample.liveBytes
