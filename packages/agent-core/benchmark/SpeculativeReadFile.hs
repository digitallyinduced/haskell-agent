module Main (main) where

import Agent.Loop (defaultLoopDispatch)
import Agent.ToolDispatch
    ( ToolArgumentStreamEvent(..)
    , ToolCall(..)
    , ToolCallResult(..)
    , ToolCallStreamRef(..)
    , functionToolCall
    )
import Agent.Tools.FileSystem.ReadFile
    ( readFileTool
    , readFileToolWithSpeculation
    )
import Agent.Tools.FileSystem.ReadFileSpeculation
import Agent.Tools.Speculation
    ( ToolSpeculationRuntime
    , closeToolSpeculationRuntime
    , newToolSpeculationRuntime
    , observeToolArgumentEvent
    , resetToolSpeculationRuntime
    , retainToolSpeculation
    , takeToolSpeculation
    )
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
                        do
                            cache <- newReadFileSpeculation env
                            let tool =
                                    readFileToolWithSpeculation env (Just cache)
                            bracket
                                (newToolSpeculationRuntime [tool])
                                closeToolSpeculationRuntime
                                \runtime -> do
                                    warmWorkspaceIndex cache
                                    registry <- requireRegistry [tool]
                                    runBenchmark
                                        workloadArg
                                        workload
                                        fileMiB
                                        tailMillis
                                        sampleCount
                                        env
                                        registry
                                        (Just (cache, runtime))
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
    -> Maybe (ReadFileSpeculation, ToolSpeculationRuntime)
    -> IO ()
runBenchmark workloadArg workload fileMiB tailMillis sampleCount env registry
        speculation = do
    samples <- forM [1 .. sampleCount] \sampleIndex -> do
        mapM_ (resetToolSpeculationRuntime . snd) speculation
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
    metrics <-
        traverse
            (readReadFileSpeculationMetrics . fst)
            speculation
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
    -> Maybe (ReadFileSpeculation, ToolSpeculationRuntime)
    -> Text
    -> IO Int
runWorkload workload tailMillis _env registry speculation callId = do
    let arguments = readArguments benchmarkTarget
        call = functionToolCall callId "read_file" arguments
        delay = when (tailMillis > 0) (threadDelay (tailMillis * 1000))
    case workload of
        Baseline -> delay
        SpeculativeComplete ->
            withRuntime speculation \runtime -> do
                observeToolArgumentEvent runtime $
                    outputItemAdded (Just callId) (Just 0) callId ""
                observeToolArgumentEvent runtime $
                    argumentsDelta
                        (Just callId)
                        (Just 0)
                        arguments
                delay
                finishStream runtime (Just callId) call
        SpeculativePrefix ->
            withRuntime speculation \runtime -> do
                observeToolArgumentEvent runtime $
                    outputItemAdded (Just callId) (Just 0) callId ""
                observeToolArgumentEvent runtime $
                    argumentsDelta
                        (Just callId)
                        (Just 0)
                        "{\"target_file\":\"bench/spec"
                delay
                finishStream runtime (Just callId) call
    output <-
        case speculation of
            Just (_, runtime) ->
                takeToolSpeculation runtime call >>= \case
                    Just result -> pure (formatToolResult result)
                    Nothing -> dispatchNormally call
            Nothing -> dispatchNormally call
    unless ("1→" `Text.isPrefixOf` output) $
        die ("unexpected read_file result: " <> Text.unpack output)
    evaluate (checksumText output)
  where
    withRuntime Nothing _ = die "speculative mode requires a runtime"
    withRuntime (Just (_, runtime)) action = action runtime

    dispatchNormally call = do
        result <-
            dispatchRegisteredToolCall defaultLoopDispatch registry call
        pure result.output

formatToolResult :: Either Text Text -> Text
formatToolResult = \case
    Left err -> "Error: " <> err
    Right output -> output

finishStream
    :: ToolSpeculationRuntime
    -> Maybe Text
    -> ToolCall
    -> IO ()
finishStream runtime itemId call = do
    observeToolArgumentEvent runtime $
        argumentsDone
            itemId
            (Just 0)
            call.arguments
    observeToolArgumentEvent runtime $
        ToolCallStreamCompleted
            { argumentStreamRefs = streamRefs itemId (Just 0)
            , argumentStreamCall = call
            }
    retainToolSpeculation runtime [call]

warmWorkspaceIndex :: ReadFileSpeculation -> IO ()
warmWorkspaceIndex = waitForReadFileSpeculation

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

outputItemAdded
    :: Maybe Text
    -> Maybe Int
    -> Text
    -> Text
    -> ToolArgumentStreamEvent
outputItemAdded itemId outputIndex callId arguments =
    ToolArgumentsStarted
        { argumentStreamRefs = streamRefs itemId outputIndex
        , argumentStreamCallId = callId
        , argumentStreamName = Just "read_file"
        , argumentStreamArguments = arguments
        }

argumentsDelta
    :: Maybe Text
    -> Maybe Int
    -> Text
    -> ToolArgumentStreamEvent
argumentsDelta itemId outputIndex delta =
    ToolArgumentsDelta
        { argumentStreamRefs = streamRefs itemId outputIndex
        , argumentStreamDelta = delta
        }

argumentsDone
    :: Maybe Text
    -> Maybe Int
    -> Text
    -> ToolArgumentStreamEvent
argumentsDone itemId outputIndex arguments =
    ToolArgumentsDone
        { argumentStreamRefs = streamRefs itemId outputIndex
        , argumentStreamName = Nothing
        , argumentStreamArguments = arguments
        }

streamRefs :: Maybe Text -> Maybe Int -> [ToolCallStreamRef]
streamRefs itemId outputIndex =
    maybe [] (pure . ToolCallStreamItem) itemId
        <> maybe [] (pure . ToolCallStreamOutput) outputIndex

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
