-- | Replay a mixed coding-turn tool stream with and without speculation.
--
-- This keeps the LLM off the clock: the same argument deltas, delays, and
-- consume order run against a fixture repo. Baseline uses the default streamed
-- interpreters, while speculative uses the prefetching interpreters so file
-- system work can overlap the stream tail.
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
    )
import Agent.Tools.FileSystem.ListDir
    ( ListDirSpeculation
    , closeListDirSpeculation
    , listDirToolWithSpeculation
    , newListDirSpeculation
    )
import Agent.Tools.FileSystem.ReadFile
    ( readFileToolWithSpeculation )
import Agent.Tools.FileSystem.ReadFileSpeculation
    ( ReadFileSpeculation
    , closeReadFileSpeculation
    , newReadFileSpeculation
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
    , withDefaultArgumentInterpreter
    )
import Control.Concurrent (threadDelay)
import Control.Exception (evaluate)
import Control.Exception.Safe (bracket, bracketOnError, finally)
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
    , readLineCount :: !Int
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
            fileKiB <-
                parsePositiveUpTo
                    "file KiB"
                    (maxBound `div` 1024)
                    fileKiBArg
            tailMillis <-
                parseNonNegativeUpTo
                    "tail milliseconds"
                    (maxBound `div` 1000)
                    tailMillisArg
            sampleCount <- parsePositive "sample count" sampleCountArg
            withTempDir \dir -> do
                fixture <- setupFixture dir fileKiB
                env <- defaultToolEnv (unsafeEncodeUtf dir)
                plan <- newPlanModeEnv (unsafeEncodeUtf dir) Nothing
                case mode of
                    Baseline ->
                        runWithTools
                            modeArg
                            fileKiB
                            tailMillis
                            sampleCount
                            fixture
                            (map
                                withDefaultArgumentInterpreter
                                [ readFileToolWithSpeculation env Nothing
                                , listDirToolWithSpeculation env Nothing
                                , searchReplaceToolWithPrefetch env plan Nothing
                                , applyPatchTool env
                                ])
                    Speculative ->
                        bracket (newCaches env) closeCaches \caches ->
                            runWithTools
                                modeArg
                                fileKiB
                                tailMillis
                                sampleCount
                                fixture
                                [ readFileToolWithSpeculation
                                    env
                                    (Just caches.cacheReads)
                                , listDirToolWithSpeculation
                                    env
                                    (Just caches.cacheLists)
                                , searchReplaceToolWithPrefetch
                                    env
                                    plan
                                    (Just caches.cacheFiles)
                                , applyPatchTool env
                                ]
        _ ->
            die $
                "usage: speculative-tools-bench "
                    <> "MODE FILE_KIB TAIL_MS SAMPLES\n"
                    <> "modes: baseline, speculative\n"
                    <> "Replays list_dir + 4 full-file read_file + "
                    <> "search_replace + apply_patch. FILE_KIB sizes the "
                    <> "edit payloads; reads are capped at 64 KiB so they "
                    <> "stay under read_file token limits.\n"
                    <> "columns: mode,file_kib,tail_ms,samples,"
                    <> "wall_p50,wall_p95,io_p50,io_p95,cpu_p50,alloc,checksum\n"
                    <> "io_* is wall minus the injected tail."

parseMode :: String -> IO Mode
parseMode = \case
    "baseline" -> pure Baseline
    "speculative" -> pure Speculative
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

newCaches :: ToolEnv -> IO ToolCaches
newCaches env =
    bracketOnError
        (newReadFileSpeculation env)
        closeReadFileSpeculation
        \readsCache ->
            bracketOnError
                (newListDirSpeculation env)
                closeListDirSpeculation
                \listsCache -> do
                    filesCache <- newFilePrefetch env
                    pure ToolCaches
                        { cacheReads = readsCache
                        , cacheLists = listsCache
                        , cacheFiles = filesCache
                        }

closeCaches :: ToolCaches -> IO ()
closeCaches caches =
    closeReadFileSpeculation caches.cacheReads
        `finally` (closeListDirSpeculation caches.cacheLists
            `finally` closeFilePrefetch caches.cacheFiles)

runWithTools
    :: String
    -> Int
    -> Int
    -> Int
    -> Fixture
    -> [AppTool]
    -> IO ()
runWithTools modeArg fileKiB tailMillis sampleCount fixture tools = do
    registry <- requireRegistry tools
    bracket
        (newToolSpeculationRuntime tools)
        closeToolSpeculationRuntime
        \runtime -> do
            -- Give both modes the same untimed warm-up turn.
            restoreFixture fixture
            _ <- runTurn tailMillis fixture registry runtime 0
            resetToolSpeculationRuntime runtime
            runBenchmark
                modeArg
                fileKiB
                tailMillis
                sampleCount
                fixture
                registry
                runtime

runBenchmark
    :: String
    -> Int
    -> Int
    -> Int
    -> Fixture
    -> ToolRegistry
    -> ToolSpeculationRuntime
    -> IO ()
runBenchmark modeArg fileKiB tailMillis sampleCount fixture registry runtime = do
    samples <- forM [1 .. sampleCount] \sampleIndex -> do
        resetToolSpeculationRuntime runtime
        restoreFixture fixture
        measure $
            runTurn
                tailMillis
                fixture
                registry
                runtime
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
    let tailMs = fromIntegral tailMillis
        ioOf wall = max 0 (wall - tailMs)
        ios = sort (map (ioOf . (.wallMillis)) samples)
    printf
        "%s,%d,%d,%d,%.3f,%.3f,%.3f,%.3f,%.3f,%d,%d\n"
        modeArg
        fileKiB
        tailMillis
        sampleCount
        (percentile 0.50 walls)
        (percentile 0.95 walls)
        (percentile 0.50 ios)
        (percentile 0.95 ios)
        (percentile 0.50 cpus)
        (percentileInt 0.50 allocs)
        first.checksum

runTurn
    :: Int
    -> Fixture
    -> ToolRegistry
    -> ToolSpeculationRuntime
    -> Int
    -> IO Int
runTurn tailMillis fixture registry runtime sampleIndex = do
    let delay = when (tailMillis > 0) (threadDelay (tailMillis * 1000))
        suffix = Text.pack (show sampleIndex)
        calls = allCalls fixture.readLineCount suffix
    streamListDir runtime suffix
    streamReads fixture.readLineCount runtime suffix
    streamReplace runtime suffix
    streamPatch runtime suffix
    delay
    finishAll fixture.readLineCount runtime suffix
    outputs <- traverse (takeOrDispatch registry runtime) calls
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
allCalls :: Int -> Text -> [ToolCall]
allCalls readLimit suffix =
    listCall suffix
        : map (readCall readLimit suffix) readTargets
        <> [replaceCall suffix, patchCall suffix]

listCall :: Text -> ToolCall
listCall suffix =
    functionToolCall ("list-" <> suffix) "list_dir" (listArguments listTarget)

readCall :: Int -> Text -> Text -> ToolCall
readCall readLimit suffix target =
    functionToolCall
        ("read-" <> suffix <> "-" <> target)
        "read_file"
        (readArguments target readLimit)

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

streamReads :: Int -> ToolSpeculationRuntime -> Text -> IO ()
streamReads readLimit runtime suffix =
    mapM_
        (\target ->
            observePrefix
                runtime
                ("read-" <> suffix <> "-" <> target)
                "read_file"
                (readArguments target readLimit))
        readTargets

streamReplace :: ToolSpeculationRuntime -> Text -> IO ()
streamReplace runtime suffix =
    observePrefix runtime ("replace-" <> suffix) "search_replace"
        ("{\"file_path\":\"" <> replaceRel <> "\"")

streamPatch :: ToolSpeculationRuntime -> Text -> IO ()
streamPatch runtime suffix =
    observePrefix runtime ("patch-" <> suffix) "apply_patch"
        ("*** Begin Patch\n*** Update File: " <> patchRel <> "\n")

finishAll :: Int -> ToolSpeculationRuntime -> Text -> IO ()
finishAll readLimit runtime suffix = do
    mapM_ (finishOne runtime) (allCalls readLimit suffix)
    retainToolSpeculation runtime (allCalls readLimit suffix)

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

readArguments :: Text -> Int -> Text
readArguments target readLimit =
    "{\"target_file\":\""
        <> target
        <> "\",\"limit\":"
        <> Text.pack (show readLimit)
        <> "}"

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
        readBytes = min payloadBytes (64 * 1024)
        replaceFile = dir </> Text.unpack replaceRel
        patchFile = dir </> Text.unpack patchRel
    replaceBody <- writePayload replaceFile replaceOld payloadBytes
    patchBody <- writePayload patchFile patchOld payloadBytes
    readBodies <-
        mapM
            (\target ->
                writePayload
                    (dir </> Text.unpack target)
                    "module Main where"
                    readBytes)
            readTargets
    initializeGitRepository dir
    pure Fixture
        { fixtureDir = dir
        , replacePath = replaceFile
        , patchPath = patchFile
        , replaceBody
        , patchBody
        , readLineCount = case readBodies of
            body : _ -> payloadLineCount body
            [] -> 0
        }

restoreFixture :: Fixture -> IO ()
restoreFixture fixture = do
    Text.writeFile fixture.replacePath fixture.replaceBody
    Text.writeFile fixture.patchPath fixture.patchBody

payloadLineCount :: Text -> Int
payloadLineCount body =
    let fields = Text.splitOn "\n" body
    in if Text.isSuffixOf "\n" body
        then max 0 (length fields - 1)
        else length fields

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
