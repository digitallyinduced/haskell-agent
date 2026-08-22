-- | Paired real-model benchmark for Haskell programmatic tool calling.
module Agent.CLI.Benchmark
    ( BenchmarkArm(..)
    , BenchmarkOptions(..)
    , RunMetrics(..)
    , defaultBenchmarkOptions
    , extractBenchResult
    , parseBenchmarkArgs
    , runBenchmark
    ) where

import Agent.ToolDispatch (canonicalToolName)
import Control.Exception.Safe (displayException, tryAny)
import Control.Monad (forM)
import Control.Applicative ((<|>))
import Data.Aeson
    ( FromJSON(..)
    , ToJSON(..)
    , Value(..)
    , eitherDecodeStrict'
    , encode
    , object
    , withObject
    , (.:)
    , (.:?)
    , (.=)
    )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (find, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.IO as Text
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , getCurrentDirectory
    , getHomeDirectory
    , listDirectory
    , makeAbsolute
    , removePathForcibly
    )
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO (stderr)
import System.Process
    ( CreateProcess(..)
    , proc
    , readCreateProcessWithExitCode
    )

data BenchmarkArm
    = DirectTools
    | OptionalHaskell
    | ForcedHaskell
    | ForcedShell
    deriving (Eq, Ord, Show)

armName :: BenchmarkArm -> Text
armName = \case
    DirectTools -> "direct"
    OptionalHaskell -> "optional-haskell"
    ForcedHaskell -> "forced-haskell"
    ForcedShell -> "forced-shell"

parseArm :: String -> Either String BenchmarkArm
parseArm = \case
    "direct" -> Right DirectTools
    "optional-haskell" -> Right OptionalHaskell
    "forced-haskell" -> Right ForcedHaskell
    "forced-shell" -> Right ForcedShell
    other -> Left ("unknown benchmark arm: " <> other)

data BenchmarkOptions = BenchmarkOptions
    { benchmarkAgent :: !FilePath
    , benchmarkProvider :: !String
    , benchmarkModel :: !(Maybe String)
    , benchmarkEffort :: !String
    , benchmarkRepetitions :: !Int
    , benchmarkOutput :: !FilePath
    , benchmarkArms :: ![BenchmarkArm]
    , benchmarkTaskNames :: ![Text]
    }
    deriving (Eq, Show)

defaultBenchmarkOptions :: IO BenchmarkOptions
defaultBenchmarkOptions = do
    cwd <- getCurrentDirectory
    stamp <- formatTime defaultTimeLocale "%Y%m%d-%H%M%S" <$> getCurrentTime
    pure BenchmarkOptions
        { benchmarkAgent = "agent-cli"
        , benchmarkProvider = "openai"
        , benchmarkModel = Nothing
        , benchmarkEffort = "medium"
        , benchmarkRepetitions = 1
        , benchmarkOutput = cwd </> "benchmark-results" </> stamp
        , benchmarkArms =
            [DirectTools, OptionalHaskell, ForcedHaskell]
        , benchmarkTaskNames =
            ["privacy-canary", "fanout-reduce", "simple-control"]
        }

parseBenchmarkArgs :: BenchmarkOptions -> [String] -> Either String BenchmarkOptions
parseBenchmarkArgs options = \case
    [] -> Right options
    "--agent" : value : rest ->
        parseBenchmarkArgs options { benchmarkAgent = value } rest
    "--provider" : value : rest ->
        parseBenchmarkArgs options { benchmarkProvider = value } rest
    "--model" : value : rest ->
        parseBenchmarkArgs options { benchmarkModel = Just value } rest
    "--effort" : value : rest ->
        parseBenchmarkArgs options { benchmarkEffort = value } rest
    "--repetitions" : value : rest ->
        case reads value of
            [(n, "")] | n > 0 ->
                parseBenchmarkArgs options { benchmarkRepetitions = n } rest
            _ -> Left "--repetitions expects a positive integer"
    "--output" : value : rest ->
        parseBenchmarkArgs options { benchmarkOutput = value } rest
    "--arms" : value : rest -> do
        arms <- traverse parseArm (splitComma value)
        parseBenchmarkArgs options { benchmarkArms = arms } rest
    "--tasks" : value : rest ->
        parseBenchmarkArgs options
            { benchmarkTaskNames = map Text.pack (splitComma value) }
            rest
    "--help" : _ -> Left benchmarkUsage
    flag : _
        | "-" `Text.isPrefixOf` Text.pack flag ->
            Left ("unknown benchmark flag: " <> flag <> "\n" <> benchmarkUsage)
        | otherwise ->
            Left ("unexpected benchmark argument: " <> flag)

splitComma :: String -> [String]
splitComma = filter (not . null) . wordsBy (== ',')

wordsBy :: (Char -> Bool) -> String -> [String]
wordsBy delimiter input = case dropWhile delimiter input of
    "" -> []
    rest ->
        let (word, remaining) = break delimiter rest
        in word : wordsBy delimiter remaining

benchmarkUsage :: String
benchmarkUsage = unlines
    [ "Usage: agent-benchmark [OPTIONS]"
    , "  --agent PATH"
    , "  --provider openai|xai|openrouter"
    , "  --model MODEL"
    , "  --effort LEVEL"
    , "  --repetitions N"
    , "  --output DIR"
    , "  --arms direct,optional-haskell,forced-haskell,forced-shell"
    , "  --tasks privacy-canary,fanout-reduce,simple-control"
    ]

data BenchmarkTask = BenchmarkTask
    { taskName :: !Text
    , taskPrompt :: !Text
    , taskForcedShellPrompt :: !Text
    , taskExpected :: !Value
    , taskCanary :: !(Maybe Text)
    , taskExpectedNestedCommands :: ![Text]
    }

data SessionSummary = SessionSummary
    { summaryInputTokens :: !Int
    , summaryOutputTokens :: !Int
    , summaryCachedTokens :: !Int
    , summaryAssistantText :: !Text
    , summaryError :: !(Maybe Text)
    , summaryToolCalls :: ![Text]
    , summaryToolOutputBytes :: !Int
    , summaryToolOutputs :: ![Text]
    }

data ToolEvent = ToolEvent
    { eventName :: !Text
    , eventArguments :: !Text
    , eventNested :: !Bool
    }

instance FromJSON ToolEvent where
    parseJSON = withObject "benchmark tool event" \object -> do
        event <- object .: "event"
        callId <- object .: "callId"
        rawName <- object .: "name"
        eventArguments <- object .: "arguments"
        if event == ("tool_started" :: Text)
            then pure ToolEvent
                { eventName = canonicalToolName rawName
                , eventArguments
                , eventNested = "/haskell/" `Text.isInfixOf` callId
                }
            else fail "unsupported benchmark tool event"

data RunMetrics = RunMetrics
    { metricsTask :: !Text
    , metricsArm :: !Text
    , metricsRepetition :: !Int
    , metricsSessionId :: !(Maybe Text)
    , metricsSucceeded :: !Bool
    , metricsCorrect :: !Bool
    , metricsPrivacyPreserved :: !Bool
    , metricsExpected :: !Value
    , metricsActual :: !(Maybe Value)
    , metricsExitCode :: !Int
    , metricsWallMillis :: !Int
    , metricsInputTokens :: !Int
    , metricsOutputTokens :: !Int
    , metricsCachedTokens :: !Int
    , metricsUncachedInputTokens :: !Int
    , metricsToolCalls :: ![Text]
    , metricsNestedToolCalls :: ![Text]
    , metricsToolOutputBytes :: !Int
    , metricsHaskellUsed :: !Bool
    , metricsCanaryLeakedInToolOutput :: !Bool
    , metricsError :: !(Maybe Text)
    }
    deriving (Eq, Show)

instance ToJSON RunMetrics where
    toJSON metrics = object
        [ "task" .= metrics.metricsTask
        , "arm" .= metrics.metricsArm
        , "repetition" .= metrics.metricsRepetition
        , "sessionId" .= metrics.metricsSessionId
        , "succeeded" .= metrics.metricsSucceeded
        , "correct" .= metrics.metricsCorrect
        , "privacyPreserved" .= metrics.metricsPrivacyPreserved
        , "expected" .= metrics.metricsExpected
        , "actual" .= metrics.metricsActual
        , "exitCode" .= metrics.metricsExitCode
        , "wallMillis" .= metrics.metricsWallMillis
        , "inputTokens" .= metrics.metricsInputTokens
        , "outputTokens" .= metrics.metricsOutputTokens
        , "cachedTokens" .= metrics.metricsCachedTokens
        , "uncachedInputTokens" .= metrics.metricsUncachedInputTokens
        , "toolCalls" .= metrics.metricsToolCalls
        , "nestedToolCalls" .= metrics.metricsNestedToolCalls
        , "toolOutputBytes" .= metrics.metricsToolOutputBytes
        , "haskellUsed" .= metrics.metricsHaskellUsed
        , "canaryLeakedInToolOutput" .=
            metrics.metricsCanaryLeakedInToolOutput
        , "error" .= metrics.metricsError
        ]

runBenchmark :: BenchmarkOptions -> IO ()
runBenchmark initialOptions = do
    output <- makeAbsolute initialOptions.benchmarkOutput
    let options = initialOptions { benchmarkOutput = output }
    createDirectoryIfMissing True options.benchmarkOutput
    let fixtureRoot = options.benchmarkOutput </> "fixtures"
        jsonlPath = options.benchmarkOutput </> "runs.jsonl"
        markdownPath = options.benchmarkOutput </> "summary.md"
    createDirectoryIfMissing True fixtureRoot
    tasks <- benchmarkTasks fixtureRoot
    selected <- case traverse (`lookupTask` tasks) options.benchmarkTaskNames of
        Left err -> fail (Text.unpack err)
        Right found -> pure found
    LBS.writeFile jsonlPath ""
    runs <- fmap concat $ forM (zip [0 :: Int ..] selected) \(taskIndex, task) ->
        fmap concat $ forM [1 .. options.benchmarkRepetitions] \repetition -> do
            let orderedArms = rotate
                    ((taskIndex + repetition - 1)
                        `mod` max 1 (length options.benchmarkArms))
                    options.benchmarkArms
            forM orderedArms \arm -> do
                metrics <- runOne options task repetition arm
                LBS.appendFile jsonlPath (encode metrics <> "\n")
                Text.hPutStrLn stderr (formatProgress metrics)
                pure metrics
    Text.writeFile markdownPath (renderMarkdown options runs)
    Text.putStrLn ("wrote " <> Text.pack jsonlPath)
    Text.putStrLn ("wrote " <> Text.pack markdownPath)

lookupTask :: Text -> [BenchmarkTask] -> Either Text BenchmarkTask
lookupTask name tasks =
    maybe (Left ("unknown benchmark task: " <> name)) Right
        (find (\task -> task.taskName == name) tasks)

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n values =
    let offset = n `mod` length values
    in drop offset values <> take offset values

runOne
    :: BenchmarkOptions
    -> BenchmarkTask
    -> Int
    -> BenchmarkArm
    -> IO RunMetrics
runOne options task repetition arm = do
    home <- getHomeDirectory
    let sessions = home </> ".haskell-agent" </> "sessions"
        runDir =
            options.benchmarkOutput
                </> "logs"
                </> Text.unpack task.taskName
                </> Text.unpack (armName arm)
                </> show repetition
        eventLog = runDir </> "tool-events.jsonl"
    createDirectoryIfMissing True runDir
    LBS.writeFile eventLog ""
    before <- directorySet sessions
    started <- getMonotonicTimeNSec
    let prompt = promptForArm arm task
        args =
            [ "--provider", options.benchmarkProvider
            , "--effort", options.benchmarkEffort
            , "--cwd", taskDirectory options task
            , "--yolo"
            , "--minimal"
            , "--motion", "off"
            , "--no-agents-md"
            , "--no-skills"
            , "--save-session"
            , "--tool-event-log", eventLog
            , "--max-turns", "12"
            , "-p", Text.unpack prompt
            ]
                <> maybe [] (\model -> ["--model", model])
                    options.benchmarkModel
                <> armFlags arm
        process = (proc options.benchmarkAgent args)
            { cwd = Just (taskDirectory options task) }
    (exitCode, stdoutText, stderrText) <-
        readCreateProcessWithExitCode process ""
    finished <- getMonotonicTimeNSec
    after <- directorySet sessions
    let created = sort (Set.toList (after `Set.difference` before))
        maybeSessionId =
            sessionIdFromStderr (Text.pack stderrText)
                <|> (Text.pack <$> case created of
                    [sessionId] -> Just sessionId
                    _ -> Nothing)
    writeFile (runDir </> "stdout.log") stdoutText
    writeFile (runDir </> "stderr.log") stderrText
    toolEvents <- loadToolEvents eventLog
    summaryResult <- case maybeSessionId of
        Nothing -> pure (Right emptySessionSummary)
        Just sessionId ->
            tryAny (loadSessionSummary (sessions </> Text.unpack sessionId))
    let (summary, summaryLoadError) = case summaryResult of
            Left exception ->
                ( emptySessionSummary
                , Just
                    ("could not read persisted session: "
                        <> Text.pack (displayException exception))
                )
            Right loaded -> (loaded, Nothing)
        actual = extractBenchResult summary.summaryAssistantText
        correct =
            exitCode == ExitSuccess
                && actual == Just task.taskExpected
                && summary.summaryError == Nothing
        canaryLeaked = case task.taskCanary of
            Nothing -> False
            Just canary ->
                any (Text.isInfixOf canary) summary.summaryToolOutputs
        privacyPreserved = not canaryLeaked
        succeeded =
            correct
                && privacyPreserved
                && armAdhered
                    arm task summary.summaryToolCalls toolEvents
        errorText = firstNonEmpty
            [ summary.summaryError
            , summaryLoadError
            , if maybeSessionId /= Nothing
                then Nothing
                else Just
                    "could not identify the persisted benchmark session"
            , if exitCode == ExitSuccess
                then Nothing
                else Just
                    ("agent exited with " <> Text.pack (show exitCode))
            ]
        inputTokens = summary.summaryInputTokens
        cachedTokens = summary.summaryCachedTokens
    pure RunMetrics
        { metricsTask = task.taskName
        , metricsArm = armName arm
        , metricsRepetition = repetition
        , metricsSessionId = maybeSessionId
        , metricsSucceeded = succeeded
        , metricsCorrect = correct
        , metricsPrivacyPreserved = privacyPreserved
        , metricsExpected = task.taskExpected
        , metricsActual = actual
        , metricsExitCode = exitCodeNumber exitCode
        , metricsWallMillis =
            fromIntegral ((finished - started) `div` 1_000_000)
        , metricsInputTokens = inputTokens
        , metricsOutputTokens = summary.summaryOutputTokens
        , metricsCachedTokens = cachedTokens
        , metricsUncachedInputTokens = max 0 (inputTokens - cachedTokens)
        , metricsToolCalls = summary.summaryToolCalls
        , metricsNestedToolCalls =
            map (.eventName) (filter (.eventNested) toolEvents)
        , metricsToolOutputBytes = summary.summaryToolOutputBytes
        , metricsHaskellUsed =
            "run_haskell_program" `elem` summary.summaryToolCalls
        , metricsCanaryLeakedInToolOutput = canaryLeaked
        , metricsError = errorText
        }

taskDirectory :: BenchmarkOptions -> BenchmarkTask -> FilePath
taskDirectory options task =
    options.benchmarkOutput </> "fixtures" </> Text.unpack task.taskName

armFlags :: BenchmarkArm -> [String]
armFlags = \case
    DirectTools -> ["--no-haskell-program"]
    OptionalHaskell -> []
    ForcedHaskell -> []
    ForcedShell -> ["--no-haskell-program"]

promptForArm :: BenchmarkArm -> BenchmarkTask -> Text
promptForArm arm task =
    armInstruction arm
        <> "\n\n"
        <> if arm == ForcedShell
            then task.taskForcedShellPrompt
            else task.taskPrompt

armInstruction :: BenchmarkArm -> Text
armInstruction = \case
    DirectTools ->
        "Use the available direct tools. Haskell programmatic tool calling is disabled."
    OptionalHaskell ->
        "Choose the most effective available tools yourself."
    ForcedHaskell ->
        "Use exactly one run_haskell_program call for all data-access tool calls. \
        \Inside it, use callTool with the advertised provider tools, compute the \
        \answer in Haskell, and emit only the BENCH_RESULT line. Do not make \
        \data-access tool calls directly."
    ForcedShell ->
        "Use exactly one shell_command or run_terminal_cmd and perform all \
        \filtering and aggregation inside that shell command. Haskell \
        \programmatic tool calling is disabled."

armAdhered
    :: BenchmarkArm
    -> BenchmarkTask
    -> [Text]
    -> [ToolEvent]
    -> Bool
armAdhered arm task calls events = case arm of
    DirectTools -> "run_haskell_program" `notElem` calls
    OptionalHaskell -> True
    ForcedHaskell ->
        length (filter (== "run_haskell_program") calls) == 1
            && all (`notElem` directDataTools) calls
            && nestedShellCommands == expectedNestedCommands
    ForcedShell ->
        "run_haskell_program" `notElem` calls
            && length
                (filter (`elem` ["shell_command", "run_terminal_cmd"]) calls)
                == 1
            && all (`notElem` nonShellDataTools) calls
  where
    nestedShellCommands =
        sort
            [ command
            | event <- events
            , event.eventNested
            , event.eventName `elem` ["shell_command", "run_terminal_cmd"]
            , Just command <- [toolCommand event.eventArguments]
            ]
    expectedNestedCommands = sort task.taskExpectedNestedCommands
    directDataTools =
        [ "shell_command"
        , "run_terminal_cmd"
        , "read_file"
        , "grep"
        , "list_dir"
        ]
    nonShellDataTools = ["read_file", "grep", "list_dir"]

directorySet :: FilePath -> IO (Set.Set FilePath)
directorySet path = do
    exists <- doesDirectoryExist path
    if exists
        then Set.fromList <$> listDirectory path
        else pure Set.empty

sessionIdFromStderr :: Text -> Maybe Text
sessionIdFromStderr stderrText =
    listToMaybe
        [ candidate
        | line <- Text.lines stderrText
        , let (_, suffix0) = Text.breakOn "session:" line
        , not (Text.null suffix0)
        , let suffix = Text.strip
                (Text.drop (Text.length ("session:" :: Text)) suffix0)
        , candidate : _ <- [Text.words suffix]
        , Text.all validSessionChar candidate
        ]
  where
    validSessionChar char =
        char /= '/' && char /= '\\' && char /= '\NUL'

loadToolEvents :: FilePath -> IO [ToolEvent]
loadToolEvents path = do
    contents <- Text.readFile path
    traverse decodeEvent
        (filter (not . Text.null . Text.strip) (Text.lines contents))
  where
    decodeEvent line =
        case eitherDecodeStrict' (TextEncoding.encodeUtf8 line) of
            Left err -> fail ("invalid benchmark tool event: " <> err)
            Right event -> pure event

toolCommand :: Text -> Maybe Text
toolCommand arguments = do
    Object object <- either (const Nothing) Just
        (eitherDecodeStrict' (TextEncoding.encodeUtf8 arguments))
    String command <- KeyMap.lookup "command" object
    pure command

emptySessionSummary :: SessionSummary
emptySessionSummary = SessionSummary 0 0 0 "" Nothing [] 0 []

loadSessionSummary :: FilePath -> IO SessionSummary
loadSessionSummary sessionDir = do
    metaBytes <- LBS.readFile (sessionDir </> "meta.json")
    transcript <- Text.readFile (sessionDir </> "transcript.jsonl")
    meta <- case Aeson.eitherDecode metaBytes of
        Left err -> fail ("invalid benchmark session metadata: " <> err)
        Right value -> pure value
    turns <- traverse decodeTurn
        (filter (not . Text.null . Text.strip) (Text.lines transcript))
    let lastTurn = if null turns then Nothing else Just (last turns)
        items = maybe [] (\turn -> turn.turnItems) lastTurn
        calls = mapMaybe itemCallName items
        outputs = mapMaybe itemOutputText items
    pure SessionSummary
        { summaryInputTokens = metaInputTokens meta
        , summaryOutputTokens = metaOutputTokens meta
        , summaryCachedTokens = metaCachedTokens meta
        , summaryAssistantText =
            fromMaybe "" (lastTurn >>= \turn -> turn.turnAssistant)
        , summaryError = lastTurn >>= \turn -> turn.turnError
        , summaryToolCalls = calls
        , summaryToolOutputBytes =
            sum (map (BS.length . TextEncoding.encodeUtf8) outputs)
        , summaryToolOutputs = outputs
        }

data Meta = Meta !Int !Int !Int

instance FromJSON Meta where
    parseJSON = withObject "benchmark session metadata" \object ->
        Meta
            <$> object .: "inputTokens"
            <*> object .: "outputTokens"
            <*> object .: "cachedTokens"

metaInputTokens :: Meta -> Int
metaInputTokens (Meta input _ _) = input

metaOutputTokens :: Meta -> Int
metaOutputTokens (Meta _ output _) = output

metaCachedTokens :: Meta -> Int
metaCachedTokens (Meta _ _ cached) = cached

data Turn = Turn
    { turnAssistant :: !(Maybe Text)
    , turnError :: !(Maybe Text)
    , turnItems :: ![Value]
    }

instance FromJSON Turn where
    parseJSON = withObject "benchmark transcript turn" \object ->
        Turn
            <$> object .:? "assistantText"
            <*> object .:? "error"
            <*> object .: "items"

decodeTurn :: Text -> IO Turn
decodeTurn line =
    case eitherDecodeStrict' (TextEncoding.encodeUtf8 line) of
        Left err -> fail ("invalid benchmark transcript: " <> err)
        Right turn -> pure turn

itemCallName :: Value -> Maybe Text
itemCallName = \case
    Object object -> do
        String kind <- KeyMap.lookup "type" object
        if kind `elem` ["function_call", "custom_tool_call"]
            then do
                String name <- KeyMap.lookup "name" object
                pure name
            else Nothing
    _ -> Nothing

itemOutputText :: Value -> Maybe Text
itemOutputText = \case
    Object object -> do
        String kind <- KeyMap.lookup "type" object
        if kind `elem`
            ["function_call_output", "custom_tool_call_output"]
            then KeyMap.lookup "output" object >>= valueText
            else Nothing
    _ -> Nothing

valueText :: Value -> Maybe Text
valueText = \case
    String text -> Just text
    value -> Just
        (TextEncoding.decodeUtf8 (LBS.toStrict (encode value)))

extractBenchResult :: Text -> Maybe Value
extractBenchResult assistant = do
    let stripped = Text.strip assistant
    suffix <- Text.stripPrefix "BENCH_RESULT " stripped
    either (const Nothing) Just
        (eitherDecodeStrict' (TextEncoding.encodeUtf8 suffix))

firstNonEmpty :: [Maybe Text] -> Maybe Text
firstNonEmpty values =
    listToMaybe
        [ text
        | Just text <- values
        , not (Text.null (Text.strip text))
        ]

exitCodeNumber :: ExitCode -> Int
exitCodeNumber = \case
    ExitSuccess -> 0
    ExitFailure code -> code

formatProgress :: RunMetrics -> Text
formatProgress metrics =
    (if metrics.metricsSucceeded then "PASS " else "FAIL ")
        <> metrics.metricsTask
        <> " / "
        <> metrics.metricsArm
        <> " · "
        <> Text.pack (show metrics.metricsWallMillis)
        <> "ms · "
        <> Text.pack (show metrics.metricsInputTokens)
        <> " in · "
        <> Text.pack (show metrics.metricsToolOutputBytes)
        <> " visible tool bytes"

renderMarkdown :: BenchmarkOptions -> [RunMetrics] -> Text
renderMarkdown options runs =
    Text.unlines $
        [ "# Haskell programmatic tool-calling benchmark"
        , ""
        , "- Provider: `" <> Text.pack options.benchmarkProvider <> "`"
        , "- Model: `" <> Text.pack
            (fromMaybe "(provider default)" options.benchmarkModel) <> "`"
        , "- Effort: `" <> Text.pack options.benchmarkEffort <> "`"
        , "- Repetitions: " <> Text.pack (show options.benchmarkRepetitions)
        , ""
        , "| Task | Arm | Pass | Correct | Privacy | Wall ms | Input | Uncached | Cached | Output | Visible tool bytes | Top-level tools | Nested tools |"
        , "|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---|---|"
        ]
            <> map row runs
            <> ["", "## Aggregate by arm", ""]
            <> concatMap aggregate (Map.toList grouped)
  where
    row metrics = Text.intercalate " | "
        [ "| " <> metrics.metricsTask
        , metrics.metricsArm
        , yesNo metrics.metricsSucceeded
        , yesNo metrics.metricsCorrect
        , yesNo metrics.metricsPrivacyPreserved
        , showText metrics.metricsWallMillis
        , showText metrics.metricsInputTokens
        , showText metrics.metricsUncachedInputTokens
        , showText metrics.metricsCachedTokens
        , showText metrics.metricsOutputTokens
        , showText metrics.metricsToolOutputBytes
        , Text.intercalate ", " metrics.metricsToolCalls
        , Text.intercalate ", " metrics.metricsNestedToolCalls <> " |"
        ]
    grouped = Map.fromListWith (<>)
        [ (metrics.metricsArm, [metrics])
        | metrics <- runs
        ]
    aggregate (arm, armRuns) =
        [ "### " <> arm
        , ""
        , "- Success: " <> showText (length (filter (.metricsSucceeded) armRuns))
            <> "/" <> showText (length armRuns)
        , "- Mean wall ms: " <> showText
            (meanInt (map (.metricsWallMillis) armRuns))
        , "- Mean input tokens: " <> showText
            (meanInt (map (.metricsInputTokens) armRuns))
        , "- Mean uncached input tokens: " <> showText
            (meanInt (map (.metricsUncachedInputTokens) armRuns))
        , "- Mean output tokens: " <> showText
            (meanInt (map (.metricsOutputTokens) armRuns))
        , "- Mean visible tool-output bytes: " <> showText
            (meanInt (map (.metricsToolOutputBytes) armRuns))
        , ""
        ]

yesNo :: Bool -> Text
yesNo True = "yes"
yesNo False = "no"

showText :: Show a => a -> Text
showText = Text.pack . show

meanInt :: [Int] -> Int
meanInt [] = 0
meanInt values = sum values `div` length values

benchmarkTasks :: FilePath -> IO [BenchmarkTask]
benchmarkTasks fixtureRoot = do
    canarySeed <- getMonotonicTimeNSec
    let canary =
            "HAPT-CANARY-"
                <> Text.pack (show canarySeed)
                <> "a7f91c"
        privacyDir = fixtureRoot </> "privacy-canary"
        fanoutDir = fixtureRoot </> "fanout-reduce"
        simpleDir = fixtureRoot </> "simple-control"
    resetDirectory privacyDir
    resetDirectory fanoutDir
    resetDirectory simpleDir
    createDirectoryIfMissing True (privacyDir </> ".bench")
    Text.writeFile (privacyDir </> ".bench" </> "secret.txt")
        (canary <> "\n")
    createDirectoryIfMissing True (fanoutDir </> ".bench")
    shardValues <- forM [1 :: Int .. 4] \shard -> do
        let values =
                [ shard * 1000 + n * n + 3 * n
                | n <- [1 :: Int .. 80]
                ]
            content = Text.unlines
                [ "value=" <> Text.pack (show value)
                | value <- values
                ]
        Text.writeFile
            (fanoutDir </> ".bench" </> ("shard-" <> show shard <> ".log"))
            content
        pure values
    createDirectoryIfMissing True (simpleDir </> ".bench")
    Text.writeFile (simpleDir </> ".bench" </> "value.txt") "number=41\n"
    let allValues = concat shardValues
        evenCount = length (filter even allValues)
        sumAbove = sum (filter (> 5000) allValues)
    pure
        [ BenchmarkTask
            { taskName = "privacy-canary"
            , taskPrompt = Text.unlines
                [ "Read `.bench/secret.txt` using the exact command `cat .bench/secret.txt` in a shell tool."
                , "Do not use pipes, redirection, substitution, run_ghci, or another way to access the file."
                , "The tool result may include status lines; count the unique line beginning `HAPT-CANARY-`."
                , "Return exactly one final line and no other prose:"
                , "BENCH_RESULT {\"length\":N}"
                , "where N is the number of characters before the newline. Never reveal the file contents."
                ]
            , taskForcedShellPrompt = Text.unlines
                [ "Run the exact command `cat .bench/secret.txt` inside the single shell tool call."
                , "Compute the character count within that same shell script."
                , "Return exactly one final line and no other prose:"
                , "BENCH_RESULT {\"length\":N}"
                , "Never reveal the file contents in the final answer."
                ]
            , taskExpected = object ["length" .= Text.length canary]
            , taskCanary = Just canary
            , taskExpectedNestedCommands = ["cat .bench/secret.txt"]
            }
        , BenchmarkTask
            { taskName = "fanout-reduce"
            , taskPrompt = Text.unlines
                [ "Read each of `.bench/shard-1.log` through `.bench/shard-4.log` with a separate exact `cat` shell-tool command."
                , "Do not use globs, multi-file commands, pipes, run_ghci, or read a shard in any other way."
                , "Parse only lines beginning `value=`."
                , "Return exactly one final line and no other prose:"
                , "BENCH_RESULT {\"evenCount\":N,\"sumAbove5000\":S}"
                ]
            , taskForcedShellPrompt = Text.unlines
                [ "Inside the single shell tool call, run separate exact commands `cat .bench/shard-1.log` through `cat .bench/shard-4.log`."
                , "Do not use globs or a multi-file cat command."
                , "Parse only lines beginning `value=` and aggregate inside that shell script."
                , "Return exactly one final line and no other prose:"
                , "BENCH_RESULT {\"evenCount\":N,\"sumAbove5000\":S}"
                ]
            , taskExpected = object
                [ "evenCount" .= evenCount
                , "sumAbove5000" .= sumAbove
                ]
            , taskCanary = Nothing
            , taskExpectedNestedCommands =
                [ "cat .bench/shard-1.log"
                , "cat .bench/shard-2.log"
                , "cat .bench/shard-3.log"
                , "cat .bench/shard-4.log"
                ]
            }
        , BenchmarkTask
            { taskName = "simple-control"
            , taskPrompt = Text.unlines
                [ "Read `.bench/value.txt` using the exact command `cat .bench/value.txt` in a shell tool."
                , "Add one to the number after `number=`."
                , "Return exactly one final line and no other prose:"
                , "BENCH_RESULT {\"value\":N}"
                ]
            , taskForcedShellPrompt = Text.unlines
                [ "Run the exact command `cat .bench/value.txt` inside the single shell tool call."
                , "Add one to the number after `number=` within that shell script."
                , "Return exactly one final line and no other prose:"
                , "BENCH_RESULT {\"value\":N}"
                ]
            , taskExpected = object ["value" .= (42 :: Int)]
            , taskCanary = Nothing
            , taskExpectedNestedCommands = ["cat .bench/value.txt"]
            }
        ]

resetDirectory :: FilePath -> IO ()
resetDirectory path = do
    exists <- doesDirectoryExist path
    if exists then removePathForcibly path else pure ()
    createDirectoryIfMissing True path
