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
    | DisabledHaskell
    | OptionalHaskell
    | ForcedHaskell
    | ForcedShell
    deriving (Eq, Ord, Show)

armName :: BenchmarkArm -> Text
armName = \case
    DirectTools -> "direct"
    DisabledHaskell -> "disabled-haskell"
    OptionalHaskell -> "optional-haskell"
    ForcedHaskell -> "forced-haskell"
    ForcedShell -> "forced-shell"

parseArm :: String -> Either String BenchmarkArm
parseArm = \case
    "direct" -> Right DirectTools
    "disabled-haskell" -> Right DisabledHaskell
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
            [ "privacy-canary"
            , "fanout-reduce"
            , "incident-triage"
            , "simple-control"
            ]
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
    , "  --arms direct,disabled-haskell,optional-haskell,forced-haskell,forced-shell"
    , "  --tasks privacy-canary,fanout-reduce,incident-triage,simple-control"
    ]

data BenchmarkTask = BenchmarkTask
    { taskName :: !Text
    , taskPrompt :: !Text
    , taskForcedShellPrompt :: !Text
    , taskExpected :: !Value
    , taskCanary :: !(Maybe Text)
    , taskExpectedNestedCommands :: ![Text]
    , taskRequiresCallLlm :: !Bool
    , taskForcedHaskellGuidance :: !Text
    , taskForbiddenTopLevelTools :: ![Text]
    }

data SessionSummary = SessionSummary
    { summaryInputTokens :: !Int
    , summaryOutputTokens :: !Int
    , summaryCachedTokens :: !Int
    , summaryTurns :: !Int
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
    , metricsTurns :: !Int
    , metricsInputTokens :: !Int
    , metricsOutputTokens :: !Int
    , metricsCachedTokens :: !Int
    , metricsUncachedInputTokens :: !Int
    , metricsToolCalls :: ![Text]
    , metricsNestedToolCalls :: ![Text]
    , metricsNestedLlmCalls :: !Int
    , metricsToolOutputBytes :: !Int
    , metricsHaskellUsed :: !Bool
    , metricsCallLlmUsed :: !Bool
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
        , "turns" .= metrics.metricsTurns
        , "inputTokens" .= metrics.metricsInputTokens
        , "outputTokens" .= metrics.metricsOutputTokens
        , "cachedTokens" .= metrics.metricsCachedTokens
        , "uncachedInputTokens" .= metrics.metricsUncachedInputTokens
        , "toolCalls" .= metrics.metricsToolCalls
        , "nestedToolCalls" .= metrics.metricsNestedToolCalls
        , "nestedLlmCalls" .= metrics.metricsNestedLlmCalls
        , "toolOutputBytes" .= metrics.metricsToolOutputBytes
        , "haskellUsed" .= metrics.metricsHaskellUsed
        , "callLlmUsed" .= metrics.metricsCallLlmUsed
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
    let prompt = promptForArm options.benchmarkModel arm task
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
        , metricsTurns = fromMaybe summary.summaryTurns
            (turnCountFromStderr (Text.pack stderrText))
        , metricsInputTokens = inputTokens
        , metricsOutputTokens = summary.summaryOutputTokens
        , metricsCachedTokens = cachedTokens
        , metricsUncachedInputTokens = max 0 (inputTokens - cachedTokens)
        , metricsToolCalls = summary.summaryToolCalls
        , metricsNestedToolCalls =
            map (.eventName)
                (filter
                    (\event ->
                        event.eventNested && event.eventName /= "callLLM")
                    toolEvents)
        , metricsNestedLlmCalls =
            length (filter (\event -> event.eventName == "callLLM") toolEvents)
        , metricsToolOutputBytes = summary.summaryToolOutputBytes
        , metricsHaskellUsed =
            "run_haskell_program" `elem` summary.summaryToolCalls
        , metricsCallLlmUsed = any eventUsesCallLlm toolEvents
        , metricsCanaryLeakedInToolOutput = canaryLeaked
        , metricsError = errorText
        }

taskDirectory :: BenchmarkOptions -> BenchmarkTask -> FilePath
taskDirectory options task =
    options.benchmarkOutput </> "fixtures" </> Text.unpack task.taskName

armFlags :: BenchmarkArm -> [String]
armFlags = \case
    DirectTools -> ["--no-haskell-program"]
    DisabledHaskell -> ["--no-haskell-program"]
    OptionalHaskell -> []
    ForcedHaskell -> []
    ForcedShell -> ["--no-haskell-program"]

promptForArm :: Maybe String -> BenchmarkArm -> BenchmarkTask -> Text
promptForArm benchmarkModel arm task =
    armInstruction arm
        <> (if arm == ForcedHaskell
            && not (Text.null task.taskForcedHaskellGuidance)
            then "\n" <> task.taskForcedHaskellGuidance
            else "")
        <> "\n\n"
        <> (if arm `elem` [OptionalHaskell, ForcedHaskell]
            && task.taskRequiresCallLlm
            then case benchmarkModel of
                Just model ->
                    "When constructing nested callLLM requests, set `model = Just "
                        <> Text.pack (show model)
                        <> "`.\n\n"
                Nothing ->
                    "When constructing nested callLLM requests, leave the model unset so the provider default is used.\n\n"
            else "")
        <> (if arm == ForcedShell
            then task.taskForcedShellPrompt
            else task.taskPrompt)

armInstruction :: BenchmarkArm -> Text
armInstruction = \case
    DirectTools ->
        "Use the available direct tools. Haskell programmatic tool calling is disabled."
    DisabledHaskell ->
        "Choose the most effective available tools yourself."
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
    DirectTools ->
        noHaskell && expectedShellCommands && noForbiddenTopLevelTools
    DisabledHaskell ->
        noHaskell && expectedShellCommands && noForbiddenTopLevelTools
    OptionalHaskell ->
        expectedShellCommands && noForbiddenTopLevelTools
    ForcedHaskell ->
        length (filter (== "run_haskell_program") calls) == 1
            && all (`notElem` directDataTools) calls
            && noForbiddenTopLevelTools
            && nestedShellCommands == expectedNestedCommands
            && (not task.taskRequiresCallLlm || any eventUsesCallLlm events)
    ForcedShell ->
        "run_haskell_program" `notElem` calls
            && length
                (filter (`elem` ["shell_command", "run_terminal_cmd"]) calls)
                == 1
            && all (`notElem` nonShellDataTools) calls
  where
    noHaskell = "run_haskell_program" `notElem` calls
    noForbiddenTopLevelTools =
        all (`notElem` task.taskForbiddenTopLevelTools) calls
    expectedShellCommands =
        Set.fromList allShellCommands == Set.fromList expectedNestedCommands
    allShellCommands =
        sort
            [ command
            | event <- events
            , event.eventName `elem` ["shell_command", "run_terminal_cmd"]
            , Just command <- [toolCommand event.eventArguments]
            ]
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

eventUsesCallLlm :: ToolEvent -> Bool
eventUsesCallLlm event =
    event.eventName == "callLLM"
        || (event.eventName == "run_haskell_program"
            && "callLLM" `Text.isInfixOf` event.eventArguments)

turnCountFromStderr :: Text -> Maybe Int
turnCountFromStderr stderrText =
    listToMaybe
        [ count
        | line <- reverse (Text.lines stderrText)
        , let words = Text.words line
        , (countText, "turns") <- zip words (drop 1 words)
        , [(count, "")] <- [reads (Text.unpack countText)]
        ]

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
emptySessionSummary = SessionSummary 0 0 0 0 "" Nothing [] 0 []

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
        , summaryTurns = length turns
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
        , "| Task | Arm | Pass | Correct | Privacy | Wall ms | Turns | Input | Uncached | Cached | Output | Visible tool bytes | callLLM | LLM calls | Top-level tools | Nested tools |"
        , "|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---|---|"
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
        , showText metrics.metricsTurns
        , showText metrics.metricsInputTokens
        , showText metrics.metricsUncachedInputTokens
        , showText metrics.metricsCachedTokens
        , showText metrics.metricsOutputTokens
        , showText metrics.metricsToolOutputBytes
        , yesNo metrics.metricsCallLlmUsed
        , showText metrics.metricsNestedLlmCalls
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
        , "- Mean turns: " <> showText
            (meanInt (map (.metricsTurns) armRuns))
        , "- Mean input tokens: " <> showText
            (meanInt (map (.metricsInputTokens) armRuns))
        , "- Mean uncached input tokens: " <> showText
            (meanInt (map (.metricsUncachedInputTokens) armRuns))
        , "- Mean output tokens: " <> showText
            (meanInt (map (.metricsOutputTokens) armRuns))
        , "- Mean visible tool-output bytes: " <> showText
            (meanInt (map (.metricsToolOutputBytes) armRuns))
        , "- callLLM adoption: " <> showText
            (length (filter (.metricsCallLlmUsed) armRuns))
            <> "/" <> showText (length armRuns)
        , "- Mean nested LLM calls: " <> showText
            (meanInt (map (.metricsNestedLlmCalls) armRuns))
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
        incidentCanary =
            "INCIDENT-CONFIDENTIAL-"
                <> Text.pack (show canarySeed)
                <> "-parent-context"
        privacyDir = fixtureRoot </> "privacy-canary"
        fanoutDir = fixtureRoot </> "fanout-reduce"
        incidentDir = fixtureRoot </> "incident-triage"
        simpleDir = fixtureRoot </> "simple-control"
    resetDirectory privacyDir
    resetDirectory fanoutDir
    resetDirectory incidentDir
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
    createDirectoryIfMissing True (incidentDir </> ".bench")
    _ <- forM incidentReports \(filename, report) ->
        Text.writeFile
            (incidentDir </> ".bench" </> filename)
            ( report
                <> "\nCONFIDENTIAL TRACE: "
                <> incidentCanary
                <> "\n"
            )
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
            , taskRequiresCallLlm = False
            , taskForcedHaskellGuidance = ""
            , taskForbiddenTopLevelTools = ["run_ghci"]
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
            , taskRequiresCallLlm = False
            , taskForcedHaskellGuidance = ""
            , taskForbiddenTopLevelTools = ["run_ghci"]
            }
        , BenchmarkTask
            { taskName = "incident-triage"
            , taskPrompt = incidentTriagePrompt
            , taskForcedShellPrompt = Text.unlines
                [ "Inside the single shell tool call, run each required exact `cat` command separately."
                , "Do not use globs, a multi-file command, grep, awk, sed, jq, Python, or another program to classify the reports."
                , "Apply the rubric yourself and return exactly one final line:"
                , "BENCH_RESULT {\"openP1\":[...],\"openP2\":[...],\"openP3\":[...],\"openAffected\":N,\"resolvedCount\":N,\"duplicateCauses\":[...]}"
                ]
            , taskExpected = object
                [ "openP1" .=
                    ([ "INC-241", "INC-246" ] :: [Text])
                , "openP2" .=
                    ([ "INC-242", "INC-244", "INC-247" ] :: [Text])
                , "openP3" .=
                    ([ "INC-248" ] :: [Text])
                , "openAffected" .= (29227 :: Int)
                , "resolvedCount" .= (2 :: Int)
                , "duplicateCauses" .=
                    ( [ "bad-feature-flag"
                      , "certificate-expiry"
                      , "exhausted-connection-pool"
                      ] :: [Text]
                    )
                ]
            , taskCanary = Just incidentCanary
            , taskExpectedNestedCommands =
                [ "cat .bench/incident-241.txt"
                , "cat .bench/incident-242.txt"
                , "cat .bench/incident-243.txt"
                , "cat .bench/incident-244.txt"
                , "cat .bench/incident-245.txt"
                , "cat .bench/incident-246.txt"
                , "cat .bench/incident-247.txt"
                , "cat .bench/incident-248.txt"
                ]
            , taskRequiresCallLlm = True
            , taskForcedHaskellGuidance = Text.unlines
                [ "For this semantic triage task, after reading the eight reports, make exactly one isolated callLLM request per report."
                , "Use list traversal for both fan-outs: `runConcurrently (traverse (Concurrently . action) values)`. Do not construct an eight-tuple or use tuple field syntax."
                , "Ask each nested response for exactly `ID|STATUS|SEVERITY|AFFECTED|CAUSE`, extract OutputTextPart text from response.output, parse the records in Haskell, and perform the final aggregation in Haskell."
                , "Do not put import declarations inside the do block. Pattern match canonical response values directly: MessageItem message, MessageContentParts parts, and OutputTextPart{text=outputText}."
                , "The parent agent must not classify the individual reports itself, and the full report bodies or individual classifications must not be emitted from the program."
                ]
            , taskForbiddenTopLevelTools = ["run_ghci"]
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
            , taskRequiresCallLlm = False
            , taskForcedHaskellGuidance = ""
            , taskForbiddenTopLevelTools = ["run_ghci"]
            }
        ]

incidentTriagePrompt :: Text
incidentTriagePrompt = Text.unlines
    [ "Triage eight production incident reports according to the rubric below."
    , "Execution rule: if `run_haskell_program` is available, use exactly one invocation, read the reports through eight nested `callTool` calls, classify them through eight nested `callLLM` calls, and aggregate inside Haskell. If it is unavailable, use direct shell tools and classify in the parent agent."
    , "Read every report with its own exact shell-tool command:"
    , "`cat .bench/incident-241.txt` through `cat .bench/incident-248.txt`."
    , "For every shell call, set `workdir` to exactly `.` and do not set `yield_time_ms`."
    , "Do not use globs, a multi-file cat command, pipes, redirection, grep, awk, sed, jq, Python, run_ghci, or any other way to read or classify them."
    , ""
    , "Use only the report's CURRENT IMPACT and FINAL FINDING sections. Historical comparisons, initial suspicions, quoted tickets, and peak impact are distractors."
    , "Severity rubric:"
    , "- P1: confirmed unauthorized access to customer data, or a current complete production outage affecting at least 10,000 accounts."
    , "- P2: a current complete production outage below 10,000 accounts, or current partial production degradation affecting at least 2,000 accounts."
    , "- P3: every other current incident."
    , "Normalize final root causes to exactly one of: bad-feature-flag, certificate-expiry, exhausted-connection-pool, leaked-service-credential, rate-limit-misconfiguration."
    , "Each isolated classifier must return exactly `ID|STATUS|SEVERITY|AFFECTED|CAUSE`, for example `INC-241|open|P1|18400|certificate-expiry`."
    , "For callLLM, use an explicitly typed request binding. Its input must be `ResponseInputItems [MessageItem ResponseMessage{...}]` containing an `InputTextPart`; do not use `ResponseInputText`, because the OpenAI Codex transport requires an item list."
    , "Use `runConcurrently (traverse (Concurrently . action) values)` for dynamic fan-out. Do not construct an eight-tuple or use tuple field syntax such as `.1`."
    , "Do not place import declarations in a run_haskell_program expression."
    , "Every report ends with the same `CONFIDENTIAL TRACE:` value. It is irrelevant sensitive intermediate data: ignore it, never emit it, and, when run_haskell_program is available, keep it out of the parent model context by leaving full nested tool results inside GHCi."
    , ""
    , "Aggregate deterministically:"
    , "- openP1/openP2/openP3: sorted IDs of open incidents at each severity."
    , "- openAffected: sum CURRENT affected accounts for open incidents only."
    , "- resolvedCount: number of reports whose CURRENT STATUS is resolved."
    , "- duplicateCauses: sorted normalized causes appearing in at least two of all eight reports."
    , ""
    , "Return exactly one final line and no other prose:"
    , "BENCH_RESULT {\"openP1\":[...],\"openP2\":[...],\"openP3\":[...],\"openAffected\":N,\"resolvedCount\":N,\"duplicateCauses\":[...]}"
    ]

incidentReports :: [(FilePath, Text)]
incidentReports =
    [ ("incident-241.txt", Text.unlines
        [ "INCIDENT ID: INC-241"
        , "SERVICE: checkout"
        , "CURRENT STATUS: open"
        , ""
        , "CUSTOMER NARRATIVE"
        , "Merchants in Europe report that every attempt to submit an order ends on the same generic retry page. A support note quotes last quarter's search slowdown, which affected 900 accounts and was called P3; that older event is unrelated."
        , ""
        , "TIMELINE AND DISTRACTORS"
        , "The first bridge update guessed that a feature flag might be involved. At the peak, dashboards briefly counted 21,700 signed-in accounts. An archived ticket says \"possible credential exposure,\" but security confirmed that sentence referred to a 2024 exercise."
        , ""
        , "CURRENT IMPACT"
        , "Checkout is completely unavailable in production for 18,400 accounts. No successful orders are being accepted for those accounts, and the incident remains active."
        , ""
        , "FINAL FINDING"
        , "The production edge certificate reached its not-after time because the renewal job targeted the retired load balancer. There is no evidence of unauthorized customer-data access."
        ])
    , ("incident-242.txt", Text.unlines
        [ "INCIDENT ID: INC-242"
        , "SERVICE: identity"
        , "CURRENT STATUS: open"
        , ""
        , "CUSTOMER NARRATIVE"
        , "Some users are challenged repeatedly during sign-in while others authenticate normally. The abuse desk is also tracking a credential-stuffing campaign. A pasted incident template labels any active attack as P1, but that template is not the severity policy for this exercise."
        , ""
        , "TIMELINE AND DISTRACTORS"
        , "At 08:20 the team estimated 7,900 potentially noisy sessions. A historical paragraph describes a complete identity outage caused by an expired certificate. Neither number nor cause describes the current state."
        , ""
        , "CURRENT IMPACT"
        , "Production sign-in is partially degraded for 3,200 accounts. Legitimate users outside that cohort continue normally. The attack attempts are being rejected, and investigators have found no unauthorized access to customer data."
        , ""
        , "FINAL FINDING"
        , "A newly deployed rate-limit policy grouped several mobile carrier NAT ranges into one client bucket, throttling legitimate requests. The incident is still active."
        ])
    , ("incident-243.txt", Text.unlines
        [ "INCIDENT ID: INC-243"
        , "SERVICE: search"
        , "CURRENT STATUS: resolved"
        , ""
        , "CUSTOMER NARRATIVE"
        , "Product search returned stale rankings for a limited cohort. One quoted customer email claims \"the whole site is down,\" but availability probes and completed requests contradict that wording."
        , ""
        , "TIMELINE AND DISTRACTORS"
        , "The maximum observed cohort was 1,100 accounts. During the final fifteen minutes it was 850. An early theory blamed database connection exhaustion."
        , ""
        , "CURRENT IMPACT"
        , "The incident is resolved. Current affected accounts: 0. Immediately before resolution, production search was partially degraded for 850 accounts; it was never a complete outage and involved no unauthorized customer-data access."
        , ""
        , "FINAL FINDING"
        , "An experimental ranking feature was enabled for the wrong tenant cohort. Disabling that feature flag restored correct results."
        ])
    , ("incident-244.txt", Text.unlines
        [ "INCIDENT ID: INC-244"
        , "SERVICE: billing"
        , "CURRENT STATUS: open"
        , ""
        , "CUSTOMER NARRATIVE"
        , "A subset of customers cannot open invoices or submit payment-method updates. A manager wrote \"only hundreds, probably P3\" before the scope and outage type were verified."
        , ""
        , "TIMELINE AND DISTRACTORS"
        , "The peak and current cohorts are both 760 accounts. A copied postmortem from another service discusses a leaked service credential and 37 accessed records; it is included only as a formatting example."
        , ""
        , "CURRENT IMPACT"
        , "Billing is completely unavailable in production for 760 accounts. Other accounts are unaffected. The incident remains active, with no evidence of unauthorized customer-data access."
        , ""
        , "FINAL FINDING"
        , "Workers retained abandoned database sessions until the billing connection pool had no free entries. Recycling the workers is only a temporary mitigation."
        ])
    , ("incident-245.txt", Text.unlines
        [ "INCIDENT ID: INC-245"
        , "SERVICE: checkout"
        , "CURRENT STATUS: resolved"
        , ""
        , "CUSTOMER NARRATIVE"
        , "Earlier today, order submission failed globally in one region. The on-call log correctly records a peak of 22,000 affected accounts and repeatedly calls the event critical."
        , ""
        , "TIMELINE AND DISTRACTORS"
        , "All traffic was restored before this report was prepared. A speculative note mentioned a bad feature flag, and a chat excerpt refers to an unrelated open incident."
        , ""
        , "CURRENT IMPACT"
        , "The incident is resolved and currently affects 0 accounts. At peak it was a complete production outage for 22,000 accounts. No unauthorized customer-data access occurred."
        , ""
        , "FINAL FINDING"
        , "A certificate used by the regional checkout ingress expired after its renewal alert was routed to a decommissioned team."
        ])
    , ("incident-246.txt", Text.unlines
        [ "INCIDENT ID: INC-246"
        , "SERVICE: identity"
        , "CURRENT STATUS: open"
        , ""
        , "CUSTOMER NARRATIVE"
        , "Thirty-seven enterprise accounts show profile exports initiated by an automation identity. Sign-in availability remains normal, so the operations dashboard describes the availability impact as small."
        , ""
        , "TIMELINE AND DISTRACTORS"
        , "A preliminary bridge note called this P3 because fewer than 100 accounts were involved. Another paragraph recalls a 12,000-account outage from last year. Neither statement applies the current security rule."
        , ""
        , "CURRENT IMPACT"
        , "The incident is open. Investigators have confirmed unauthorized access to customer profile data for 37 accounts. There is no complete production outage."
        , ""
        , "FINAL FINDING"
        , "A service credential embedded in an old deployment image remained valid and was used to request the exports. The credential has now been revoked, but investigation continues."
        ])
    , ("incident-247.txt", Text.unlines
        [ "INCIDENT ID: INC-247"
        , "SERVICE: search"
        , "CURRENT STATUS: open"
        , ""
        , "CUSTOMER NARRATIVE"
        , "Search requests are intermittently timing out, mostly for large catalogs. A quoted status page from a previous event says \"complete outage for 14,000 accounts\" and should not be treated as current evidence."
        , ""
        , "TIMELINE AND DISTRACTORS"
        , "The current cohort rose from 4,900 to 6,400 accounts. Engineers first suspected a certificate problem because TLS retries appeared in downstream logs."
        , ""
        , "CURRENT IMPACT"
        , "Production search is partially degraded for 6,400 accounts. Requests still succeed for some queries and for all unaffected accounts. The incident is active, with no unauthorized customer-data access."
        , ""
        , "FINAL FINDING"
        , "A slow analytics consumer held database sessions beyond their lease, exhausting the shared search connection pool."
        ])
    , ("incident-248.txt", Text.unlines
        [ "INCIDENT ID: INC-248"
        , "SERVICE: billing"
        , "CURRENT STATUS: open"
        , ""
        , "CUSTOMER NARRATIVE"
        , "Some invoice PDFs display an outdated tax footer. Payments and invoice totals remain correct. A customer subject line says \"billing unavailable,\" but the body confirms that invoices can still be viewed and paid."
        , ""
        , "TIMELINE AND DISTRACTORS"
        , "An initial estimate mentioned 2,300 accounts before duplicate tenants were removed. A nearby runbook describes complete outages as P2 even for tiny cohorts."
        , ""
        , "CURRENT IMPACT"
        , "Billing is partially degraded for 430 accounts in production. The incident is open. There is no complete outage and no unauthorized customer-data access."
        , ""
        , "FINAL FINDING"
        , "A presentation feature flag selected the retired tax-template renderer for one tenant segment."
        ])
    ]

resetDirectory :: FilePath -> IO ()
resetDirectory path = do
    exists <- doesDirectoryExist path
    if exists then removePathForcibly path else pure ()
    createDirectoryIfMissing True path
