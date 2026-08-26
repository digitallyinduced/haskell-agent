-- | Replay a mixed coding-turn tool stream with and without speculation.
--
-- This keeps the LLM off the clock: the same argument deltas, delays, and
-- consume order run against a fixture repo. Baseline waits out the stream
-- tail then dispatches. Speculative feeds prefixes so reads overlap the tail.
module Main (main) where

import Agent.Codex.Dialect.Tools (applyPatchTool)
import Agent.GrokBuild.Dialect.SearchReplace (searchReplaceToolWithPrefetch)
import Agent.Loop (defaultLoopDispatch)
import Agent.ToolDispatch
    ( ToolArgumentStreamEvent(..)
    , ToolCall(..)
    , ToolCallResult(..)
    , ToolCallStreamRef(..)
    , customToolCall
    , functionToolCall
    )
import Agent.Tools.FileSystem.FilePrefetch
    ( FilePrefetch
    , closeFilePrefetch
    , newFilePrefetch
    , waitForFilePrefetch
    )
import Agent.Tools.FileSystem.ListDir
    ( ListDirSpeculation
    , closeListDirSpeculation
    , listDirToolWithSpeculation
    , newListDirSpeculation
    , waitForListDirSpeculation
    )
import Agent.Tools.FileSystem.ReadFile
    ( readFileTool
    , readFileToolWithSpeculation
    )
import Agent.Tools.FileSystem.ReadFileSpeculation
    ( ReadFileSpeculation
    , closeReadFileSpeculation
    , newReadFileSpeculation
    , waitForReadFileSpeculation
    )
import Agent.Tools.PlanMode (newPlanModeEnv)
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
import Data.Char (ord)
import Data.List (sort)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (RTSStats(..), getRTSStats, getRTSStatsEnabled)
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

data Mode = Baseline | Speculative
    deriving (Eq, Show)

data Sample = Sample
    { wallMillis :: !Double
    , cpuMillis :: !Double
    , allocatedBytes :: !Integer
    , checksum :: !Int
    }

data Fixture = Fixture
    { fixtureDir :: !FilePath
    , replacePath :: !FilePath
    , patchPath :: !FilePath
    , replaceBody :: !Text
    , patchBody :: !Text
    }

data ToolCaches = ToolCaches
    { cacheReads :: !ReadFileSpeculation
    , cacheLists :: !ListDirSpeculation
    , cacheFiles :: !FilePrefetch
    }

readTargets :: [Text]
readTargets =
    [ "src/alpha-file.hs"
    , "src/bravo-file.hs"
    , "src/charlie-file.hs"
    , "src/delta-file.hs"
    ]

listTarget :: Text
listTarget = "src/unique-listing-a"

replaceRel :: Text
replaceRel = "src/replace-target.hs"

patchRel :: Text
patchRel = "src/patch-target.hs"

replaceOld :: Text
replaceOld = "UNIQUE_OLD_LINE_FOR_REPLACE"

replaceNew :: Text
replaceNew = "UNIQUE_NEW_LINE_FOR_REPLACE"

patchOld :: Text
patchOld = "UNIQUE_OLD_PATCH_LINE"

patchNew :: Text
patchNew = "UNIQUE_NEW_PATCH_LINE"

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    unless enabled $
        die "RTS statistics are disabled; run with +RTS -T"
    getArgs >>= \case
        [modeArg, fileKiBArg, tailMillisArg, sampleCountArg] -> do
            mode <- parseMode modeArg
            fileKiB <- parsePositive "file KiB" fileKiBArg
            tailMillis <- parseNonNegative "tail milliseconds" tailMillisArg
            sampleCount <- parsePositive "sample count" sampleCountArg
            withTempDir \dir -> do
                fixture <- setupFixture dir fileKiB
                env <- defaultToolEnv (unsafeEncodeUtf dir)
                plan <- newPlanModeEnv (unsafeEncodeUtf dir) Nothing
                case mode of
                    Baseline -> do
                        registry <-
                            requireRegistry
                                [ readFileTool env
                                , listDirToolWithSpeculation env Nothing
                                , searchReplaceToolWithPrefetch env plan Nothing
                                , applyPatchTool env
                                ]
                        runBenchmark
                            modeArg mode fileKiB tailMillis sampleCount
                            fixture env registry Nothing
                    Speculative -> do
                        caches <- newCaches env
                        planTool <-
                            pure (searchReplaceToolWithPrefetch env plan (Just caches.cacheFiles))
                        let tools =
                                [ readFileToolWithSpeculation env (Just caches.cacheReads)
                                , listDirToolWithSpeculation env (Just caches.cacheLists)
                                , planTool
                                , applyPatchTool env
                                ]
                        registry <- requireRegistry tools
                        bracket
                            (newToolSpeculationRuntime tools)
                            closeToolSpeculationRuntime
                            \runtime -> do
                                warmCaches caches runtime
                                restoreFixture fixture
                                _ <-
                                    runTurn
                                        Speculative
                                        tailMillis
                                        env
                                        registry
                                        (Just (caches, runtime))
                                        0
                                resetToolSpeculationRuntime runtime
                                runBenchmark
                                    modeArg mode fileKiB tailMillis sampleCount
                                    fixture env registry (Just (caches, runtime))
                        closeCaches caches
        _ ->
            die $
                "usage: speculative-tools-bench "
                    <> "MODE FILE_KIB TAIL_MS SAMPLES\n"
                    <> "modes: baseline, speculative\n"
                    <> "Replays list_dir + 4 read_file + search_replace + "
                    <> "apply_patch with a streamed unique-path tail."

parseMode :: String -> IO Mode
parseMode = \case
    "baseline" -> pure Baseline
    "speculative" -> pure Speculative
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

newCaches :: ToolEnv -> IO ToolCaches
newCaches env = do
    readsCache <- newReadFileSpeculation env
    listsCache <- newListDirSpeculation env
    filesCache <- newFilePrefetch env
    pure ToolCaches
        { cacheReads = readsCache
        , cacheLists = listsCache
        , cacheFiles = filesCache
        }

closeCaches :: ToolCaches -> IO ()
closeCaches caches = do
    closeReadFileSpeculation caches.cacheReads
    closeListDirSpeculation caches.cacheLists
    closeFilePrefetch caches.cacheFiles

warmCaches :: ToolCaches -> ToolSpeculationRuntime -> IO ()
warmCaches caches runtime = do
    observePrefix runtime "warmup-list" "list_dir"
        "{\"target_directory\":\"src/unique-li"
    observePrefix runtime "warmup-read" "read_file"
        "{\"target_file\":\"src/alp"
    observePrefix runtime "warmup-replace" "search_replace"
        "{\"file_path\":\"src/replace-target.hs\""
    waitForListDirSpeculation caches.cacheLists
    waitForReadFileSpeculation caches.cacheReads
    waitForFilePrefetch caches.cacheFiles
    resetToolSpeculationRuntime runtime

runBenchmark
    :: String
    -> Mode
    -> Int
    -> Int
    -> Int
    -> Fixture
    -> ToolEnv
    -> ToolRegistry
    -> Maybe (ToolCaches, ToolSpeculationRuntime)
    -> IO ()
runBenchmark modeArg mode fileKiB tailMillis sampleCount fixture env registry
        speculation = do
    samples <- forM [1 .. sampleCount] \sampleIndex -> do
        restoreFixture fixture
        mapM_ (resetToolSpeculationRuntime . snd) speculation
        measure $
            runTurn
                mode
                tailMillis
                env
                registry
                speculation
                sampleIndex
    let distinctChecksums = uniqueSorted (map (.checksum) samples)
    unless (length distinctChecksums == 1) $
        die ("benchmark outputs differed: " <> show distinctChecksums)
    let walls = sort (map (.wallMillis) samples)
        cpus = sort (map (.cpuMillis) samples)
        allocs = sort (map (.allocatedBytes) samples)
        first = case samples of
            sample : _ -> sample
            [] -> error "median requires at least one sample"
    printf
        "%s,%d,%d,%d,%.3f,%.3f,%.3f,%d,%d\n"
        modeArg
        fileKiB
        tailMillis
        sampleCount
        (percentile 0.50 walls)
        (percentile 0.95 walls)
        (percentile 0.50 cpus)
        (percentileInt 0.50 allocs)
        first.checksum

runTurn
    :: Mode
    -> Int
    -> ToolEnv
    -> ToolRegistry
    -> Maybe (ToolCaches, ToolSpeculationRuntime)
    -> Int
    -> IO Int
runTurn mode tailMillis _env registry speculation sampleIndex = do
    let delay = when (tailMillis > 0) (threadDelay (tailMillis * 1000))
        suffix = Text.pack (show sampleIndex)
    outputs <- case mode of
        Baseline -> do
            delay
            traverse (dispatchNormally registry) (allCalls suffix)
        Speculative ->
            withRuntime speculation \runtime -> do
                streamListDir runtime suffix
                streamReads runtime suffix
                streamReplace runtime suffix
                streamPatch runtime suffix
                delay
                finishAll runtime suffix
                traverse (takeOrDispatch registry runtime) (allCalls suffix)
    let combined = Text.intercalate "\n---\n" outputs
    case outputs of
        listOut : read1 : _read2 : _read3 : _read4 : replaceOut : patchOut : _ -> do
            unless (Text.isInfixOf "unique-listing-a" listOut) $
                die ("unexpected list_dir result: " <> Text.unpack listOut)
            unless ("1→" `Text.isPrefixOf` read1) $
                die ("unexpected read_file result: " <> Text.unpack read1)
            unless (Text.isInfixOf "updated successfully" replaceOut) $
                die ("unexpected search_replace result: " <> Text.unpack replaceOut)
            unless (Text.isInfixOf "M src/patch-target.hs" patchOut) $
                die ("unexpected apply_patch result: " <> Text.unpack patchOut)
        _ -> die "mixed turn produced too few tool results"
    evaluate (checksumText combined)
  where
    withRuntime Nothing _ = die "speculative mode requires a runtime"
    withRuntime (Just (_, runtime)) action = action runtime

allCalls :: Text -> [ToolCall]
allCalls suffix =
    listCall suffix
        : map (readCall suffix) readTargets
        <> [replaceCall suffix, patchCall suffix]

listCall :: Text -> ToolCall
listCall suffix =
    functionToolCall ("list-" <> suffix) "list_dir" (listArguments listTarget)

readCall :: Text -> Text -> ToolCall
readCall suffix target =
    functionToolCall
        ("read-" <> suffix <> "-" <> target)
        "read_file"
        (readArguments target)

replaceCall :: Text -> ToolCall
replaceCall suffix =
    functionToolCall
        ("replace-" <> suffix)
        "search_replace"
        (replaceArguments replaceRel)

patchCall :: Text -> ToolCall
patchCall suffix =
    customToolCall ("patch-" <> suffix) "apply_patch" patchText

streamListDir :: ToolSpeculationRuntime -> Text -> IO ()
streamListDir runtime suffix =
    observePrefix runtime ("list-" <> suffix) "list_dir"
        "{\"target_directory\":\"src/unique-li"

streamReads :: ToolSpeculationRuntime -> Text -> IO ()
streamReads runtime suffix =
    mapM_
        (\target ->
            observePrefix
                runtime
                ("read-" <> suffix <> "-" <> target)
                "read_file"
                ("{\"target_file\":\"" <> Text.take 12 target))
        readTargets

streamReplace :: ToolSpeculationRuntime -> Text -> IO ()
streamReplace runtime suffix =
    observePrefix runtime ("replace-" <> suffix) "search_replace"
        ("{\"file_path\":\"" <> replaceRel <> "\"")

streamPatch :: ToolSpeculationRuntime -> Text -> IO ()
streamPatch runtime suffix =
    observePrefix runtime ("patch-" <> suffix) "apply_patch"
        ("*** Begin Patch\n*** Update File: " <> patchRel <> "\n")

finishAll :: ToolSpeculationRuntime -> Text -> IO ()
finishAll runtime suffix = do
    mapM_ (finishOne runtime) (allCalls suffix)
    retainToolSpeculation runtime (allCalls suffix)

finishOne :: ToolSpeculationRuntime -> ToolCall -> IO ()
finishOne runtime call@ToolCall{callId, name, arguments} = do
    observeToolArgumentEvent runtime $
        ToolArgumentsDone
            { argumentStreamRefs = [ToolCallStreamItem callId]
            , argumentStreamName = Just name
            , argumentStreamArguments = arguments
            }
    observeToolArgumentEvent runtime $
        ToolCallStreamCompleted
            { argumentStreamRefs = [ToolCallStreamItem call.callId]
            , argumentStreamCall = call
            }

observePrefix
    :: ToolSpeculationRuntime
    -> Text
    -> Text
    -> Text
    -> IO ()
observePrefix runtime callId name prefix = do
    observeToolArgumentEvent runtime $
        ToolArgumentsStarted
            { argumentStreamRefs = [ToolCallStreamItem callId]
            , argumentStreamCallId = callId
            , argumentStreamName = Just name
            , argumentStreamArguments = prefix
            }

takeOrDispatch
    :: ToolRegistry
    -> ToolSpeculationRuntime
    -> ToolCall
    -> IO Text
takeOrDispatch registry runtime call =
    takeToolSpeculation runtime call >>= \case
        Just result -> pure (formatToolResult result)
        Nothing -> dispatchNormally registry call

dispatchNormally :: ToolRegistry -> ToolCall -> IO Text
dispatchNormally registry call = do
    result <-
        dispatchRegisteredToolCall defaultLoopDispatch registry call
    pure result.output

formatToolResult :: Either Text Text -> Text
formatToolResult = \case
    Left err -> "Error: " <> err
    Right output -> output

listArguments :: Text -> Text
listArguments target =
    "{\"target_directory\":\"" <> target <> "\"}"

readArguments :: Text -> Text
readArguments target =
    "{\"target_file\":\"" <> target <> "\"}"

replaceArguments :: Text -> Text
replaceArguments target =
    "{\"file_path\":\""
        <> target
        <> "\",\"old_string\":\""
        <> replaceOld
        <> "\",\"new_string\":\""
        <> replaceNew
        <> "\"}"

patchText :: Text
patchText =
    "*** Begin Patch\n*** Update File: "
        <> patchRel
        <> "\n@@\n-"
        <> patchOld
        <> "\n+"
        <> patchNew
        <> "\n*** End Patch"

setupFixture :: FilePath -> Int -> IO Fixture
setupFixture dir fileKiB = do
    createDirectoryIfMissing True (dir </> "src" </> "unique-listing-a")
    createDirectoryIfMissing True (dir </> "src" </> "other-listing-b")
    Text.writeFile (dir </> "src" </> "unique-listing-a" </> "keep.txt") "keep"
    Text.writeFile (dir </> "src" </> "other-listing-b" </> "skip.txt") "skip"
    let payloadBytes = fileKiB * 1024
        replaceFile = dir </> Text.unpack replaceRel
        patchFile = dir </> Text.unpack patchRel
    replaceBody <- writePayload replaceFile replaceOld payloadBytes
    patchBody <- writePayload patchFile patchOld payloadBytes
    mapM_
        (\target ->
            writePayload (dir </> Text.unpack target) "module Main where" payloadBytes)
        readTargets
    initializeGitRepository dir
    pure Fixture
        { fixtureDir = dir
        , replacePath = replaceFile
        , patchPath = patchFile
        , replaceBody
        , patchBody
        }

restoreFixture :: Fixture -> IO ()
restoreFixture fixture = do
    Text.writeFile fixture.replacePath fixture.replaceBody
    Text.writeFile fixture.patchPath fixture.patchBody

writePayload :: FilePath -> Text -> Int -> IO Text
writePayload path header byteCount = do
    let line = Text.replicate 79 "a" <> "\n"
        headerLine = header <> "\n"
        remaining = max 0 (byteCount - Text.length headerLine)
        lineBytes = Text.length line
        (fullLines, extra) = remaining `divMod` lineBytes
        body =
            headerLine
                <> Text.replicate fullLines line
                <> Text.take extra line
    Text.writeFile path body
    pure body

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
        , checksum = result
        }

percentile :: Double -> [Double] -> Double
percentile _ [] = error "percentile requires samples"
percentile p values =
    let n = length values
        rank = min (n - 1) (max 0 (ceiling (p * fromIntegral n) - 1))
    in values !! rank

percentileInt :: Double -> [Integer] -> Integer
percentileInt _ [] = error "percentile requires samples"
percentileInt p values =
    let n = length values
        rank = min (n - 1) (max 0 (ceiling (p * fromIntegral n) - 1))
    in values !! rank

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
        (mkdtemp (tmp </> "agent-tools-benchmark-XXXXXX"))
        removeDirectoryRecursive
        action
