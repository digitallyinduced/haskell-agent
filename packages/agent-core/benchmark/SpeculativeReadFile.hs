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
    , ToolRegistry
    , defaultToolEnv
    , dispatchRegisteredToolCall
    , mkToolRegistry
    , withDefaultArgumentInterpreter
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
            fileMiB <-
                parsePositiveUpTo
                    "file MiB"
                    (maxBound `div` (1024 * 1024))
                    fileMiBArg
            tailMillis <-
                parseNonNegativeUpTo
                    "tail milliseconds"
                    (maxBound `div` 1000)
                    tailMillisArg
            sampleCount <- parsePositive "sample count" sampleCountArg
            withTempDir \dir -> do
                let path = dir </> Text.unpack benchmarkTarget
                createDirectoryIfMissing True (dir </> "bench")
                writeSizedTextFile path (fileMiB * 1024 * 1024)
                initializeGitRepository dir
                env <- defaultToolEnv (unsafeEncodeUtf dir)
                case workload of
                    Baseline ->
                        runWithTool
                            workloadArg
                            workload
                            fileMiB
                            tailMillis
                            sampleCount
                            (withDefaultArgumentInterpreter (readFileTool env))
                            Nothing
                    _ ->
                        bracket
                            (newReadFileSpeculation env)
                            closeReadFileSpeculation
                            \cache -> do
                                let tool =
                                        readFileToolWithSpeculation env (Just cache)
                                runWithTool
                                    workloadArg
                                    workload
                                    fileMiB
                                    tailMillis
                                    sampleCount
                                    tool
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
parsePositive label =
    parseBounded label 1 maxBound

parsePositiveUpTo :: String -> Int -> String -> IO Int
parsePositiveUpTo label upper =
    parseBounded label 1 upper

parseNonNegativeUpTo :: String -> Int -> String -> IO Int
parseNonNegativeUpTo label upper =
    parseBounded label 0 upper

parseBounded :: String -> Int -> Int -> String -> IO Int
parseBounded label lower upper raw =
    case reads raw :: [(Integer, String)] of
        [(value, "")]
            | value >= fromIntegral lower
            , value <= fromIntegral upper ->
                pure (fromInteger value)
        _ -> die ("invalid " <> label <> ": " <> raw)

runWithTool
    :: String
    -> Workload
    -> Int
    -> Int
    -> Int
    -> AppTool
    -> Maybe ReadFileSpeculation
    -> IO ()
runWithTool workloadArg workload fileMiB tailMillis sampleCount tool cache = do
    registry <- requireRegistry [tool]
    bracket
        (newToolSpeculationRuntime [tool])
        closeToolSpeculationRuntime
        \runtime -> do
            mapM_ warmWorkspaceIndex cache
            runBenchmark
                workloadArg
                workload
                fileMiB
                tailMillis
                sampleCount
                registry
                cache
                runtime

runBenchmark
    :: String
    -> Workload
    -> Int
    -> Int
    -> Int
    -> ToolRegistry
    -> Maybe ReadFileSpeculation
    -> ToolSpeculationRuntime
    -> IO ()
runBenchmark workloadArg workload fileMiB tailMillis sampleCount registry cache
        runtime = do
    samples <- forM [1 .. sampleCount] \sampleIndex -> do
        resetToolSpeculationRuntime runtime
        let callId = "benchmark-" <> Text.pack (show sampleIndex)
        measure $
            runWorkload
                workload
                tailMillis
                registry
                runtime
                callId
    let distinctChecksums = uniqueSorted (map (.checksum) samples)
    unless (length distinctChecksums == 1) $
        die ("benchmark outputs differed: " <> show distinctChecksums)
    let result = median samples
    metrics <-
        traverse
            readReadFileSpeculationMetrics
            cache
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
    -> ToolRegistry
    -> ToolSpeculationRuntime
    -> Text
    -> IO Int
runWorkload workload tailMillis registry runtime callId = do
    let arguments = readArguments benchmarkTarget
        call = functionToolCall callId "read_file" arguments
        delay = when (tailMillis > 0) (threadDelay (tailMillis * 1000))
        streamedArguments = case workload of
            Baseline -> arguments
            SpeculativeComplete -> arguments
            SpeculativePrefix -> "{\"target_file\":\"bench/spec"
    observeToolArgumentEvent runtime $
        outputItemAdded (Just callId) (Just 0) callId ""
    observeToolArgumentEvent runtime $
        argumentsDelta (Just callId) (Just 0) streamedArguments
    delay
    finishStream runtime (Just callId) call
    output <-
        takeToolSpeculation runtime call >>= \case
            Just result -> pure (formatToolResult result)
            Nothing -> dispatchNormally call
    unless ("→" `Text.isInfixOf` output) $
        die ("unexpected read_file result: " <> Text.unpack output)
    evaluate (checksumText output)
  where
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
                -- A negative offset makes read_file scan the complete file
                -- while returning only the final line.  Without this field
                -- the default 1000-line window makes the nominal FILE_MIB
                -- workload irrelevant and measures only interpreter overhead.
                Aeson.object
                    [ "target_file" Aeson..= target
                    , "offset" Aeson..= (-1 :: Int)
                    ]

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
