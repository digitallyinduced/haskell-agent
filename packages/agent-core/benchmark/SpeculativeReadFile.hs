module Main (main) where

import Agent.Loop (defaultLoopDispatch)
import Agent.Responses.Types
import Agent.ToolDispatch
    ( ToolCallResult(..)
    , functionToolCall
    )
import Agent.Tools.FileSystem.ReadFile
    ( readFileTool
    , readFileToolWithSpeculation
    )
import Agent.Tools.FileSystem.ReadFileSpeculation
import Agent.Tools.Types
    ( AppTool
    , ToolEnv
    , ToolRegistry
    , defaultToolEnv
    , dispatchRegisteredToolCall
    , mkToolRegistry
    )
import Control.Concurrent (threadDelay)
import Control.Exception (evaluate)
import Control.Exception.Safe (bracket)
import Control.Monad (forM, unless, when)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Char (ord)
import Data.List (sort)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
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
    , removeDirectoryRecursive
    )
import System.Environment (getArgs)
import System.Exit (ExitCode(..), die)
import System.FilePath ((</>))
import System.Mem (performGC)
import System.OsPath (unsafeEncodeUtf)
import System.Posix.Temp (mkdtemp)
import System.Process (readProcessWithExitCode)
import Text.Printf (printf)

data Workload
    = Baseline
    | SpeculativeComplete
    | SpeculativePrefix
    deriving (Eq, Show)

data Sample = Sample
    { wallMillis :: !Double
    , cpuMillis :: !Double
    , allocatedBytes :: !Integer
    , checksum :: !Int
    }

benchmarkTarget :: Text
benchmarkTarget = "bench/speculative-input.txt"

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    unless enabled $
        die "RTS statistics are disabled; run with +RTS -T"
    getArgs >>= \case
        [workloadArg, fileMiBArg, tailMillisArg, sampleCountArg] -> do
            workload <- parseWorkload workloadArg
            fileMiB <- parsePositive "file MiB" fileMiBArg
            tailMillis <- parseNonNegative "tail milliseconds" tailMillisArg
            sampleCount <- parsePositive "sample count" sampleCountArg
            withTempDir \dir -> do
                let path = dir </> Text.unpack benchmarkTarget
                createDirectoryIfMissing True (dir </> "bench")
                writeSizedTextFile path (fileMiB * 1024 * 1024)
                initializeGitRepository dir
                env <- defaultToolEnv (unsafeEncodeUtf dir)
                baselineRegistry <-
                    requireRegistry [readFileTool env]
                case workload of
                    Baseline ->
                        runBenchmark
                            workloadArg
                            workload
                            fileMiB
                            tailMillis
                            sampleCount
                            env
                            baselineRegistry
                            Nothing
                    _ ->
                        bracket
                            (newReadFileSpeculation env)
                            closeReadFileSpeculation
                            \cache -> do
                                warmWorkspaceIndex cache
                                registry <- requireRegistry
                                    [readFileToolWithSpeculation env (Just cache)]
                                runBenchmark
                                    workloadArg
                                    workload
                                    fileMiB
                                    tailMillis
                                    sampleCount
                                    env
                                    registry
                                    (Just cache)
        _ ->
            die $
                "usage: speculative-read-file-bench "
                    <> "MODE FILE_MIB TAIL_MS SAMPLES\n"
                    <> "modes: baseline, speculative-complete, "
                    <> "speculative-prefix"

parseWorkload :: String -> IO Workload
parseWorkload = \case
    "baseline" -> pure Baseline
    "speculative-complete" -> pure SpeculativeComplete
    "speculative-prefix" -> pure SpeculativePrefix
    other -> die ("unknown mode: " <> other)

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

runBenchmark
    :: String
    -> Workload
    -> Int
    -> Int
    -> Int
    -> ToolEnv
    -> ToolRegistry
    -> Maybe ReadFileSpeculation
    -> IO ()
runBenchmark workloadArg workload fileMiB tailMillis sampleCount env registry
        speculation = do
    samples <- forM [1 .. sampleCount] \sampleIndex -> do
        mapM_ resetReadFileSpeculation speculation
        let callId = "benchmark-" <> Text.pack (show sampleIndex)
        measure $
            runWorkload
                workload
                tailMillis
                env
                registry
                speculation
                callId
    let distinctChecksums = uniqueSorted (map (.checksum) samples)
    unless (length distinctChecksums == 1) $
        die ("benchmark outputs differed: " <> show distinctChecksums)
    let result = median samples
    metrics <- traverse readReadFileSpeculationMetrics speculation
    let (started, hits, misses) = case metrics of
            Nothing -> (0, 0, 0)
            Just values ->
                ( values.speculativeReadsStarted
                , values.speculativeReadHits
                , values.speculativeReadMisses
                )
    printf
        "%s,%d,%d,%d,%.3f,%.3f,%d,%d,%d,%d,%d\n"
        workloadArg
        fileMiB
        tailMillis
        sampleCount
        result.wallMillis
        result.cpuMillis
        result.allocatedBytes
        result.checksum
        started
        hits
        misses

runWorkload
    :: Workload
    -> Int
    -> ToolEnv
    -> ToolRegistry
    -> Maybe ReadFileSpeculation
    -> Text
    -> IO Int
runWorkload workload tailMillis _env registry speculation callId = do
    let arguments = readArguments benchmarkTarget
        finalItem = finalReadCall (Just callId) callId arguments
        call = functionToolCall callId "read_file" arguments
        delay = when (tailMillis > 0) (threadDelay (tailMillis * 1000))
    case workload of
        Baseline -> delay
        SpeculativeComplete ->
            withCache speculation \cache -> do
                observeReadFileStreamEvent cache $
                    outputItemAdded (Just callId) (Just 0) callId ""
                observeReadFileStreamEvent cache $
                    argumentsDelta
                        (Just callId)
                        (Just 0)
                        arguments
                delay
                finishStream cache (Just callId) finalItem arguments
        SpeculativePrefix ->
            withCache speculation \cache -> do
                observeReadFileStreamEvent cache $
                    outputItemAdded (Just callId) (Just 0) callId ""
                observeReadFileStreamEvent cache $
                    argumentsDelta
                        (Just callId)
                        (Just 0)
                        "{\"target_file\":\"bench/spec"
                delay
                finishStream cache (Just callId) finalItem arguments
    result <- dispatchRegisteredToolCall defaultLoopDispatch registry call
    unless ("1→" `Text.isPrefixOf` result.output) $
        die ("unexpected read_file result: " <> Text.unpack result.output)
    evaluate (checksumText result.output)
  where
    withCache Nothing _ = die "speculative mode requires a cache"
    withCache (Just cache) action = action cache

finishStream
    :: ReadFileSpeculation
    -> Maybe Text
    -> ResponseItem
    -> Text
    -> IO ()
finishStream cache itemId finalItem arguments = do
    observeReadFileStreamEvent cache $
        argumentsDone
            itemId
            (Just 0)
            arguments
    observeReadFileStreamEvent cache $
        ResponseOutputItemDoneEvent
            { item = finalItem
            , outputIndex = Just 0
            , sequenceNumber = Nothing
            , eventExtraFields = KeyMap.empty
            }
    retainFinalReadFileCalls cache [finalItem]

warmWorkspaceIndex :: ReadFileSpeculation -> IO ()
warmWorkspaceIndex cache = do
    observeReadFileStreamEvent cache $
        outputItemAdded
            (Just "benchmark-index-warmup")
            (Just 0)
            "benchmark-index-warmup"
            ""
    waitForReadFileSpeculation cache
    resetReadFileSpeculation cache

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
    -- Flush per-capability allocation counters after the timed interval.
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
        , checksum = result
        }

median :: [Sample] -> Sample
median [] = error "median requires at least one sample"
median samples@(first : _) =
    Sample
        { wallMillis = middle (sort (map (.wallMillis) samples))
        , cpuMillis = middle (sort (map (.cpuMillis) samples))
        , allocatedBytes =
            middle (sort (map (.allocatedBytes) samples))
        , checksum = first.checksum
        }
  where
    middle values = values !! (length values `div` 2)

checksumText :: Text -> Int
checksumText =
    Text.foldl'
        (\checksum character -> checksum * 33 + ord character)
        5381

uniqueSorted :: Ord a => [a] -> [a]
uniqueSorted = Set.toAscList . Set.fromList

requireRegistry :: [AppTool] -> IO ToolRegistry
requireRegistry =
    either (die . Text.unpack) pure . mkToolRegistry

readArguments :: Text -> Text
readArguments target =
    Text.decodeUtf8 $
        LazyByteString.toStrict $
            Aeson.encode $
                Aeson.object ["target_file" Aeson..= target]

finalReadCall :: Maybe Text -> Text -> Text -> ResponseItem
finalReadCall itemId callId arguments =
    FunctionCallItem FunctionCall
        { itemId
        , callId
        , name = "read_file"
        , arguments
        , status = Nothing
        , extraFields = KeyMap.empty
        }

outputItemAdded
    :: Maybe Text
    -> Maybe Int
    -> Text
    -> Text
    -> ResponseStreamEvent
outputItemAdded itemId outputIndex callId arguments =
    ResponseOutputItemAddedEvent
        { item = finalReadCall itemId callId arguments
        , outputIndex
        , sequenceNumber = Nothing
        , eventExtraFields = KeyMap.empty
        }

argumentsDelta
    :: Maybe Text
    -> Maybe Int
    -> Text
    -> ResponseStreamEvent
argumentsDelta itemId outputIndex delta =
    OtherResponseStreamEvent
        { otherEventType = EventFunctionCallArgumentsDelta
        , sequenceNumber = Nothing
        , eventExtraFields =
            KeyMap.fromList
                [ ("item_id", maybe Aeson.Null Aeson.String itemId)
                , ("output_index", maybe
                    Aeson.Null
                    (Aeson.Number . fromIntegral)
                    outputIndex)
                , ("delta", Aeson.String delta)
                ]
        }

argumentsDone
    :: Maybe Text
    -> Maybe Int
    -> Text
    -> ResponseStreamEvent
argumentsDone itemId outputIndex arguments =
    OtherResponseStreamEvent
        { otherEventType = EventFunctionCallArgumentsDone
        , sequenceNumber = Nothing
        , eventExtraFields =
            KeyMap.fromList
                [ ("item_id", maybe Aeson.Null Aeson.String itemId)
                , ("output_index", maybe
                    Aeson.Null
                    (Aeson.Number . fromIntegral)
                    outputIndex)
                , ("arguments", Aeson.String arguments)
                ]
        }

writeSizedTextFile :: FilePath -> Int -> IO ()
writeSizedTextFile path byteCount =
    LazyByteString.writeFile path $
        LazyByteString.fromChunks $
            replicate fullChunks chunk
                <> [ByteString.take remaining chunk | remaining > 0]
  where
    line = ByteString.replicate 79 97 <> ByteString.singleton 10
    chunk = ByteString.concat (replicate 128 line)
    chunkBytes = ByteString.length chunk
    (fullChunks, remaining) = byteCount `divMod` chunkBytes

initializeGitRepository :: FilePath -> IO ()
initializeGitRepository dir = do
    (exitCode, _, stderrText) <-
        readProcessWithExitCode "git" ["-C", dir, "init", "-q"] ""
    case exitCode of
        ExitSuccess -> pure ()
        ExitFailure code ->
            die $
                "git init failed with exit "
                    <> show code
                    <> ": "
                    <> stderrText

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir action = do
    tmp <- getTemporaryDirectory
    bracket
        (mkdtemp (tmp </> "agent-read-benchmark-XXXXXX"))
        removeDirectoryRecursive
        action
