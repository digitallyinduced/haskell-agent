module Main (main) where

import Agent.CLI.Session
    ( SessionMeta(..)
    , SessionTurn(..)
    , loadSession
    )
import Agent.Responses.Types
    ( CustomToolCall(..)
    , FunctionCall(..)
    , ResponseItem(..)
    )
import Agent.Store.Postgres
    ( managedPostgresConfigFromEnv
    , trustedPool
    , withStore
    )
import Agent.Store.Types (renderStoreError)
import Control.Exception.Safe (tryIO)
import Control.Monad (forM, forM_, unless, when)
import Data.Aeson (ToJSON(..), encode, object, (.=))
import qualified Data.ByteString.Lazy as LBS
import Data.List (intercalate, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Directory
    ( copyFile
    , createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , getHomeDirectory
    , listDirectory
    , removePathForcibly
    )
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.IO (IOMode(..), hPutStrLn, stderr, withFile)
import System.OsPath (unsafeEncodeUtf)
import System.Process
    ( CreateProcess(..)
    , StdStream(..)
    , getPid
    , proc
    , readProcess
    , readProcessWithExitCode
    , waitForProcess
    , withCreateProcess
    )
import System.Posix.Signals
    ( Signal
    , sigKILL
    , sigTERM
    , signalProcess
    )
import System.Posix.Types (ProcessID)
import System.Timeout (timeout)

data Mode = GhciOnly | BashOnly | GhciAndBash
    deriving (Eq, Ord, Show)

data Config = Config
    { agentBin :: !FilePath
    , resultsDir :: !FilePath
    , trials :: !Int
    , timeoutSeconds :: !Int
    , selectedTask :: !(Maybe Text)
    , selectedMode :: !(Maybe Mode)
    , forwardedArgs :: ![String]
    }

data EvalTask = EvalTask
    { taskName :: !Text
    , taskPrompt :: !Text
    , prepareTask :: !(FilePath -> IO ())
    , gradeTask :: !(FilePath -> IO (Bool, Text))
    }

data RunResult = RunResult
    { resultTask :: !Text
    , resultTrial :: !Int
    , resultMode :: !Mode
    , resultPassed :: !Bool
    , resultGrade :: !Text
    , resultExitCode :: !Int
    , resultTimedOut :: !Bool
    , resultSeconds :: !Double
    , resultInputTokens :: !(Maybe Int)
    , resultOutputTokens :: !(Maybe Int)
    , resultCachedTokens :: !(Maybe Int)
    , resultToolCalls :: !(Maybe [Text])
    , resultSessionId :: !(Maybe Text)
    , resultProvider :: !(Maybe Text)
    , resultModel :: !(Maybe Text)
    , resultEffort :: !(Maybe Text)
    , resultWorkspace :: !FilePath
    , resultStdoutLog :: !FilePath
    , resultStderrLog :: !FilePath
    }

instance ToJSON Mode where
    toJSON GhciOnly = toJSON ("ghci-only" :: Text)
    toJSON BashOnly = toJSON ("bash-only" :: Text)
    toJSON GhciAndBash = toJSON ("ghci-plus-bash" :: Text)

instance ToJSON RunResult where
    toJSON result = object
        [ "task" .= result.resultTask
        , "trial" .= result.resultTrial
        , "mode" .= result.resultMode
        , "passed" .= result.resultPassed
        , "grade" .= result.resultGrade
        , "exitCode" .= result.resultExitCode
        , "timedOut" .= result.resultTimedOut
        , "seconds" .= result.resultSeconds
        , "inputTokens" .= result.resultInputTokens
        , "outputTokens" .= result.resultOutputTokens
        , "cachedTokens" .= result.resultCachedTokens
        , "toolCalls" .= result.resultToolCalls
        , "sessionId" .= result.resultSessionId
        , "provider" .= result.resultProvider
        , "model" .= result.resultModel
        , "effort" .= result.resultEffort
        , "workspace" .= result.resultWorkspace
        , "stdoutLog" .= result.resultStdoutLog
        , "stderrLog" .= result.resultStderrLog
        ]

main :: IO ()
main = do
    args <- getArgs
    config <- case parseConfig args of
        Left err -> hPutStrLn stderr err >> exitFailure
        Right parsed -> pure parsed
    case config.selectedTask of
        Just name
            | name `notElem` map (.taskName) evalTasks -> do
                hPutStrLn stderr $
                    "unknown task: " <> Text.unpack name
                        <> " (expected "
                        <> intercalate ", " (map (Text.unpack . (.taskName)) evalTasks)
                        <> ")"
                exitFailure
        _ -> pure ()
    case validateForwardedArgs config.forwardedArgs of
        Left err -> hPutStrLn stderr err >> exitFailure
        Right () -> pure ()
    prepareResultsDirectory config.resultsDir
    let tasks = evalTasks
        scheduled =
            [ (trial, task, mode)
            | trial <- [1 .. config.trials]
            , task <- tasks
            , maybe True (== task.taskName) config.selectedTask
            , mode <- modeOrder trial task.taskName
            , maybe True (== mode) config.selectedMode
            ]
    results <- forM scheduled \(trial, task, mode) -> do
        putStrLn $
            "Running " <> Text.unpack task.taskName
                <> " trial " <> show trial
                <> " (" <> modeSlug mode <> ")"
        runOne config trial task mode
    LBS.writeFile (config.resultsDir </> "results.json")
        (encode results)
    Text.writeFile (config.resultsDir </> "summary.md")
        (renderSummary config results)
    putStrLn ""
    Text.putStrLn (renderConsoleSummary results)
    unless (all (.resultPassed) results) exitFailure

parseConfig :: [String] -> Either String Config
parseConfig args =
    go Config
        { agentBin = "." </> "agent-cli"
        , resultsDir = "." </> "eval-results" </> "ghci-vs-bash"
        , trials = 1
        , timeoutSeconds = 180
        , selectedTask = Nothing
        , selectedMode = Nothing
        , forwardedArgs = []
        }
        args
  where
    go config = \case
        [] -> Right config
        "--agent-bin" : value : rest ->
            go config { agentBin = value } rest
        "--results-dir" : value : rest ->
            go config { resultsDir = value } rest
        "--trials" : value : rest -> case reads value of
            [(count, "")] | count > 0 ->
                go config { trials = count } rest
            _ -> Left "--trials expects a positive integer"
        "--timeout-seconds" : value : rest -> case reads value of
            [(seconds, "")] | seconds > 0 ->
                go config { timeoutSeconds = seconds } rest
            _ -> Left "--timeout-seconds expects a positive integer"
        "--task" : value : rest ->
            go config { selectedTask = Just (Text.pack value) } rest
        "--mode" : value : rest -> case value of
            "ghci-only" ->
                go config { selectedMode = Just GhciOnly } rest
            "bash-only" ->
                go config { selectedMode = Just BashOnly } rest
            "ghci-plus-bash" ->
                go config { selectedMode = Just GhciAndBash } rest
            _ -> Left "--mode expects ghci-only, bash-only, or ghci-plus-bash"
        "--" : rest ->
            Right config { forwardedArgs = rest }
        "--help" : _ ->
            Left usage
        unknown : _ ->
            Left ("unknown eval argument: " <> unknown <> "\n\n" <> usage)

usage :: String
usage = unlines
    [ "Usage: eval-ghci-vs-bash --agent-bin PATH [OPTIONS] [-- AGENT_ARGS]"
    , ""
    , "Options:"
    , "  --results-dir DIR   Artifact directory"
    , "  --trials N          Repetitions per task and mode (default: 1)"
    , "  --timeout-seconds N  Per-run timeout (default: 180)"
    , "  --task NAME         Run only one named task"
    , "  --mode NAME         ghci-only, bash-only, or ghci-plus-bash"
    , ""
    , "Example:"
    , "  eval-ghci-vs-bash --agent-bin $(cabal list-bin agent-cli:exe:agent-cli) \\"
    , "    -- --provider openai --model gpt-5.6-sol"
    ]

evalTasks :: [EvalTask]
evalTasks =
    [ dataSummaryTask
    , cProgramTask
    , treeAuditTask
    ]

dataSummaryTask :: EvalTask
dataSummaryTask = EvalTask
    { taskName = "data-summary"
    , taskPrompt = Text.unlines
        [ "Complete this task in the workspace. Do not only explain the answer."
        , "Read numbers.csv and create report.txt with exactly these five lines:"
        , "count=<integer>"
        , "sum=<integer>"
        , "minimum=<integer>"
        , "maximum=<integer>"
        , "mean=<decimal with exactly two digits>"
        ]
    , prepareTask = \dir ->
        Text.writeFile (dir </> "numbers.csv")
            "17,-4,23,9,12,31,-8,6,14,5,19,2\n"
    , gradeTask = \dir ->
        exactFile (dir </> "report.txt") $ Text.unlines
            [ "count=12"
            , "sum=126"
            , "minimum=-8"
            , "maximum=31"
            , "mean=10.50"
            ]
    }

cProgramTask :: EvalTask
cProgramTask = EvalTask
    { taskName = "c-program"
    , taskPrompt = Text.unlines
        [ "Complete this task in the workspace. Do not only explain the answer."
        , "Write src/max_window.c so it implements the command-line program described in SPEC.md."
        , "Use standard C11 and no third-party libraries. Verify the behavior if possible."
        ]
    , prepareTask = \dir -> do
        createDirectoryIfMissing True (dir </> "src")
        Text.writeFile (dir </> "SPEC.md") $ Text.unlines
            [ "# max_window"
            , ""
            , "Build a command-line program from `src/max_window.c`."
            , ""
            , "Usage: `max_window WINDOW INTEGER...`"
            , ""
            , "- `WINDOW` must be positive and no larger than the number of integers."
            , "- Every `INTEGER` must fit in a signed 32-bit integer."
            , "- Find the contiguous window of exactly `WINDOW` integers with the greatest sum."
            , "- If several windows have the same greatest sum, choose the earliest one."
            , "- Print exactly `start=<zero-based index> sum=<sum>` followed by a newline."
            , "- Use a signed 64-bit or wider type for sums."
            , "- Invalid integers or an invalid argument count/window must return a non-zero exit status."
            ]
        Text.writeFile (dir </> "src" </> "max_window.c")
            "/* Implement the program described in ../SPEC.md. */\n"
    , gradeTask = gradeCProgram
    }

treeAuditTask :: EvalTask
treeAuditTask = EvalTask
    { taskName = "tree-audit"
    , taskPrompt = Text.unlines
        [ "Complete this task in the workspace. Do not only explain the answer."
        , "Recursively inspect the files under logs/."
        , "Create audit.txt with one line per service, sorted by service name:"
        , "<service> errors=<ERROR line count> warnings=<WARN line count>"
        , "Only .log files count; ignore all other files."
        ]
    , prepareTask = prepareTreeAudit
    , gradeTask = \dir ->
        exactFile (dir </> "audit.txt") $ Text.unlines
            [ "api errors=3 warnings=2"
            , "billing errors=1 warnings=3"
            , "worker errors=2 warnings=1"
            ]
    }

prepareTreeAudit :: FilePath -> IO ()
prepareTreeAudit dir = do
    let files =
            [ ("logs/api/2026-08-21.log", ["INFO start", "WARN slow", "ERROR timeout"])
            , ("logs/api/archive/2026-08-20.log", ["ERROR reset", "WARN retry", "ERROR failed"])
            , ("logs/api/notes.txt", ["ERROR ignored"])
            , ("logs/billing/current.log", ["WARN late", "WARN retry", "ERROR declined", "WARN queued"])
            , ("logs/worker/a.log", ["INFO ready", "ERROR crash"])
            , ("logs/worker/nested/b.log", ["WARN busy", "ERROR lost"])
            ]
    forM_ files \(relative, rows) -> do
        let path = dir </> relative
        createDirectoryIfMissing True (takeDirectory path)
        Text.writeFile path (Text.unlines rows)

gradeCProgram :: FilePath -> IO (Bool, Text)
gradeCProgram dir = do
    let binary = dir </> "grader-max-window"
        processSpec =
            (proc "cc"
                [ "-std=c11"
                , "-Wall"
                , "-Wextra"
                , "-Werror"
                , "-pedantic"
                , "src/max_window.c"
                , "-o"
                , "grader-max-window"
                ])
                { cwd = Just dir }
        stdoutPath = dir </> "grader.stdout.log"
        stderrPath = dir </> "grader.stderr.log"
    exitCode <- runProcessWithTimeout 15 processSpec stdoutPath stderrPath
    if exitCode /= ExitSuccess
        then do
            stderrText <- readFileIfExists stderrPath
            pure
                ( False
                , Text.pack $
                    "C compilation failed: " <> trim stderrText
                )
        else do
            validResults <- forM validCases \(args, expected) -> do
                (code, stdoutText, stderrText) <-
                    readProcessWithExitCode binary args ""
                pure $
                    if code == ExitSuccess && stdoutText == expected
                        then Nothing
                        else Just $
                            "args=" <> show args
                                <> " exit=" <> show code
                                <> " stdout=" <> show stdoutText
                                <> " stderr=" <> show stderrText
            invalidResults <- forM invalidCases \args -> do
                (code, stdoutText, stderrText) <-
                    readProcessWithExitCode binary args ""
                pure $
                    if code /= ExitSuccess
                        then Nothing
                        else Just $
                            "invalid args=" <> show args
                                <> " unexpectedly succeeded"
                                <> " stdout=" <> show stdoutText
                                <> " stderr=" <> show stderrText
            let failures = catMaybes (validResults <> invalidResults)
            pure
                ( null failures
                , if null failures
                    then "compiled cleanly and passed all valid and invalid input cases"
                    else Text.pack (intercalate "; " failures)
                )
  where
    validCases =
        [ (["2", "1", "5", "2", "4"], "start=1 sum=7\n")
        , (["2", "4", "1", "4", "1"], "start=0 sum=5\n")
        , (["3", "-5", "-2", "-7", "-1"], "start=1 sum=-10\n")
        , (["4", "2", "-1", "3", "6"], "start=0 sum=10\n")
        , ( ["2", "2147483647", "2147483647", "-1"]
          , "start=0 sum=4294967294\n"
          )
        ]
    invalidCases =
        [ []
        , ["0", "1"]
        , ["2", "1"]
        , ["1", "not-an-integer"]
        , ["1", "2147483648"]
        , ["1", "-2147483649"]
        ]

runOne :: Config -> Int -> EvalTask -> Mode -> IO RunResult
runOne config trial task mode = do
    let runName =
            Text.unpack task.taskName
                <> "-trial-" <> show trial
                <> "-" <> modeSlug mode
        workspace = config.resultsDir </> "workspaces" </> runName
        stdoutLog = config.resultsDir </> "logs" </> runName <> ".stdout.log"
        stderrLog = config.resultsDir </> "logs" </> runName <> ".stderr.log"
    resetDirectory workspace
    createDirectoryIfMissing True (takeDirectory stdoutLog)
    task.prepareTask workspace
    home <- getHomeDirectory
    let sessionsDir = home </> ".haskell-agent" </> "sessions"
    started <- getCurrentTime
    let processSpec =
            proc config.agentBin
                ( [ "--cwd", workspace
                  , "--prompt", Text.unpack task.taskPrompt
                  , "--save-session"
                  , "--no-agents-md"
                  , "--no-skills"
                  , "--no-computer-use"
                  , "--max-turns", "30"
                  ]
                    <> modeFlags mode
                    <> config.forwardedArgs
                )
    exitCode <- runProcessWithTimeout
        config.timeoutSeconds processSpec stdoutLog stderrLog
    ended <- getCurrentTime
    sessionId <- sessionIdFromLog stderrLog
    sessionData <- case sessionId of
        Nothing -> pure Nothing
        Just identifier -> do
            storeConfig <-
                managedPostgresConfigFromEnv (home </> ".haskell-agent")
            loaded <- withStore storeConfig \store ->
                    loadSession
                        (trustedPool store)
                        (unsafeEncodeUtf sessionsDir)
                        identifier
            let loadedResult = case loaded of
                    Left err -> Left (renderStoreError err)
                    Right result -> result
            case loadedResult of
                Left err -> do
                    hPutStrLn stderr ("could not load eval session: " <> Text.unpack err)
                    pure Nothing
                Right value -> do
                    copySessionArtifacts config.resultsDir runName sessionsDir identifier
                    pure (Just value)
    (passed, rawGrade) <- task.gradeTask workspace
    let timedOut = exitCode == ExitFailure 124
        grade
            | timedOut = "timed out; " <> rawGrade
            | otherwise = rawGrade
    let (inputTokens, outputTokens, cachedTokens, toolCalls) =
            sessionMetrics sessionData
        (providerName, modelName, effortName) = case sessionData of
            Just (meta, _) ->
                ( Just (Text.pack (show meta.metaProvider))
                , Just meta.metaModel
                , Just meta.metaEffort
                )
            Nothing -> (Nothing, Nothing, Nothing)
    pure RunResult
        { resultTask = task.taskName
        , resultTrial = trial
        , resultMode = mode
        , resultPassed = passed && exitCode == ExitSuccess
        , resultGrade = grade
        , resultExitCode = exitCodeNumber exitCode
        , resultTimedOut = timedOut
        , resultSeconds = realToFrac (diffUTCTime ended started)
        , resultInputTokens = inputTokens
        , resultOutputTokens = outputTokens
        , resultCachedTokens = cachedTokens
        , resultToolCalls = toolCalls
        , resultSessionId = sessionId
        , resultProvider = providerName
        , resultModel = modelName
        , resultEffort = effortName
        , resultWorkspace = workspace
        , resultStdoutLog = stdoutLog
        , resultStderrLog = stderrLog
        }

sessionMetrics
    :: Maybe (SessionMeta, [SessionTurn])
    -> (Maybe Int, Maybe Int, Maybe Int, Maybe [Text])
sessionMetrics Nothing = (Nothing, Nothing, Nothing, Nothing)
sessionMetrics (Just (_, [])) = (Nothing, Nothing, Nothing, Nothing)
sessionMetrics (Just (meta, turns)) =
    ( Just meta.metaInputTokens
    , Just meta.metaOutputTokens
    , Just meta.metaCachedTokens
    , Just (concatMap (concatMap itemToolName . (.turnItems)) turns)
    )

itemToolName :: ResponseItem -> [Text]
itemToolName = \case
    FunctionCallItem call -> [call.name]
    CustomToolCallItem call -> [call.name]
    _ -> []

renderSummary :: Config -> [RunResult] -> Text
renderSummary config results =
    Text.unlines
        [ "# GHCi-only vs bash-only vs combined agent eval"
        , ""
        , "This compares the product configurations, not mutually exclusive tools:"
        , "`ghci-only` exposes only `run_ghci`; `bash-only` exposes only the"
        , "provider shell tool; `ghci-plus-bash` exposes both."
        , ""
        , "- Trials per task and mode: " <> Text.pack (show config.trials)
        , "- Per-run timeout: " <> Text.pack (show config.timeoutSeconds) <> " seconds"
        , "- Agent binary: `" <> Text.pack config.agentBin <> "`"
        , "- Forwarded agent arguments: `" <> Text.pack (unwords config.forwardedArgs) <> "`"
        , ""
        , renderConsoleSummary results
        , ""
        , "## Individual runs"
        , ""
        , "| task | trial | mode | pass | timeout | seconds | input | output | cached | tools |"
        , "|---|---:|---|---:|---:|---:|---:|---:|---:|---|"
        ]
        <> Text.unlines (map renderRunRow results)

renderConsoleSummary :: [RunResult] -> Text
renderConsoleSummary results =
    Text.unlines $
        [ "| mode | passed | median successful seconds | median successful input | median successful output | successful tool calls |"
        , "|---|---:|---:|---:|---:|---|"
        ]
            <> map renderModeSummary [GhciOnly, BashOnly, GhciAndBash]
  where
    renderModeSummary mode =
        let selected = filter ((== mode) . (.resultMode)) results
            successful = filter (.resultPassed) selected
            passed = length successful
            tools = Map.toList $ Map.fromListWith (+)
                [ (name, 1 :: Int)
                | result <- successful
                , names <- maybeToList result.resultToolCalls
                , name <- names
                ]
            inputValues =
                map (fromIntegral @Int @Double) $
                    catMaybes (map (.resultInputTokens) successful)
            outputValues =
                map (fromIntegral @Int @Double) $
                    catMaybes (map (.resultOutputTokens) successful)
        in Text.intercalate " | "
            [ "| " <> Text.pack (modeSlug mode)
            , Text.pack (show passed <> "/" <> show (length selected))
            , formatOptionalDoubleMedian (map (.resultSeconds) successful)
            , formatOptionalMedian inputValues
            , formatOptionalMedian outputValues
            , Text.pack (intercalate ", "
                [Text.unpack name <> "=" <> show count | (name, count) <- tools])
                <> " |"
            ]

renderRunRow :: RunResult -> Text
renderRunRow result = Text.intercalate " | "
    [ "| " <> result.resultTask
    , Text.pack (show result.resultTrial)
    , Text.pack (modeSlug result.resultMode)
    , if result.resultPassed then "yes" else "no"
    , if result.resultTimedOut then "yes" else "no"
    , formatDouble result.resultSeconds
    , maybe "n/a" (Text.pack . show) result.resultInputTokens
    , maybe "n/a" (Text.pack . show) result.resultOutputTokens
    , maybe "n/a" (Text.pack . show) result.resultCachedTokens
    , maybe "n/a" (Text.intercalate ", ") result.resultToolCalls <> " |"
    ]

modeOrder :: Int -> Text -> [Mode]
modeOrder trial name =
    take 3 . drop offset . cycle $ [GhciOnly, BashOnly, GhciAndBash]
  where
    offset = (trial + Text.length name) `mod` 3

modeSlug :: Mode -> String
modeSlug GhciOnly = "ghci-only"
modeSlug BashOnly = "bash-only"
modeSlug GhciAndBash = "ghci-plus-bash"

modeFlags :: Mode -> [String]
modeFlags GhciOnly = ["--ghci", "--no-bash"]
modeFlags BashOnly = ["--no-ghci", "--bash"]
modeFlags GhciAndBash = ["--ghci", "--bash"]

sessionIdFromLog :: FilePath -> IO (Maybe Text)
sessionIdFromLog path = do
    exists <- doesFileExist path
    if not exists
        then pure Nothing
        else do
            contents <- Text.readFile path
            pure $ case
                [ Text.strip suffix
                | line <- Text.lines contents
                , let (_, suffixWithMarker) = Text.breakOn "session:" line
                , not (Text.null suffixWithMarker)
                , let suffix = Text.drop (Text.length ("session:" :: Text)) suffixWithMarker
                , not (Text.null (Text.strip suffix))
                ] of
                sessionId : _ -> Just sessionId
                [] -> Nothing

validateForwardedArgs :: [String] -> Either String ()
validateForwardedArgs = go
  where
    go [] = Right ()
    go (flag : _value : rest)
        | flag `elem` ["--provider", "--model", "--effort", "--compact-threshold"] =
            go rest
    go (flag : _)
        | flag `elem` reservedFlags =
            Left ("eval controls " <> flag <> "; do not pass it after --")
        | otherwise =
            Left
                ( "unsupported forwarded agent argument: " <> flag
                    <> " (allowed: --provider, --model, --effort, --compact-threshold)"
                )

    reservedFlags =
        [ "--ghci", "--no-ghci", "--bash", "--no-bash"
        , "--computer-use", "--no-computer-use"
        , "--cwd", "--prompt", "-p", "--prompt-file"
        , "--save-session", "--agents-md", "--no-agents-md", "--skills"
        , "--no-skills", "--max-turns", "--worktree", "--resume"
        ]

prepareResultsDirectory :: FilePath -> IO ()
prepareResultsDirectory path = do
    exists <- doesDirectoryExist path
    when exists do
        entries <- listDirectory path
        unless (null entries) do
            hPutStrLn stderr $
                "results directory is not empty: " <> path
                    <> " (choose a fresh directory)"
            exitFailure
    createDirectoryIfMissing True path

runProcessWithTimeout
    :: Int
    -> CreateProcess
    -> FilePath
    -> FilePath
    -> IO ExitCode
runProcessWithTimeout seconds processSpec stdoutPath stderrPath =
    withFile stdoutPath WriteMode \stdoutHandle ->
        withFile stderrPath WriteMode \stderrHandle ->
            withCreateProcess
                processSpec
                    { std_out = UseHandle stdoutHandle
                    , std_err = UseHandle stderrHandle
                    , create_group = True
                    , new_session = True
                    }
                \_ _ _ processHandle -> do
                    sessionId <- getPid processHandle
                    completed <- timeout
                        (seconds * 1000000)
                        (waitForProcess processHandle)
                    case completed of
                        Just code -> do
                            pure code
                        Nothing -> do
                            processIds <- case sessionId of
                                Nothing -> pure []
                                Just rootPid -> processTreeIds rootPid
                            signalProcesses processIds sigTERM
                            _ <- timeout 3000000 (waitForProcess processHandle)
                            signalProcesses processIds sigKILL
                            _ <- timeout 3000000 (waitForProcess processHandle)
                            pure (ExitFailure 124)

signalProcesses :: [ProcessID] -> Signal -> IO ()
signalProcesses processIds signal =
    forM_ processIds \pid -> do
        _ <- tryIO (signalProcess signal pid)
        pure ()

processTreeIds :: ProcessID -> IO [ProcessID]
processTreeIds rootPid = do
    listed <- tryIO (readProcess "ps" ["-axo", "pid=,ppid="] "")
    pure $ case listed of
        Left _ -> [rootPid]
        Right output ->
            let relationships =
                    [ (pid, parentPid)
                    | line <- lines output
                    , [pidText, parentText] <- [words line]
                    , [(pid, "")] <- [reads pidText]
                    , [(parentPid, "")] <- [reads parentText]
                    ]
                descendants parent =
                    concat
                        [ descendants child <> [child]
                        | (child, childParent) <- relationships
                        , childParent == parent
                        ]
            in descendants rootPid <> [rootPid]

copySessionArtifacts
    :: FilePath
    -> FilePath
    -> FilePath
    -> Text
    -> IO ()
copySessionArtifacts resultsRoot runName sessionsRootPath sessionId = do
    let source = sessionsRootPath </> Text.unpack sessionId
        destination = resultsRoot </> "sessions" </> runName
    createDirectoryIfMissing True destination
    forM_ ["meta.json", "transcript.jsonl"] \name -> do
        let sourcePath = source </> name
        exists <- doesFileExist sourcePath
        when exists (copyFile sourcePath (destination </> name))

readFileIfExists :: FilePath -> IO String
readFileIfExists path = do
    exists <- doesFileExist path
    if exists then readFile path else pure ""

resetDirectory :: FilePath -> IO ()
resetDirectory path = do
    exists <- doesDirectoryExist path
    when exists (removePathForcibly path)
    createDirectoryIfMissing True path

exactFile :: FilePath -> Text -> IO (Bool, Text)
exactFile path expected = do
    exists <- doesFileExist path
    if not exists
        then pure (False, Text.pack ("missing " <> path))
        else do
            actual <- Text.readFile path
            let passed = normalize actual == normalize expected
            pure
                ( passed
                , if passed
                    then "exact output matched"
                    else "expected " <> quoted (normalize expected)
                        <> " but got " <> quoted (normalize actual)
                )

normalize :: Text -> Text
normalize input =
    let unix = Text.replace "\r\n" "\n" input
    in maybe unix id (Text.stripSuffix "\n" unix)

quoted :: Text -> Text
quoted text = "`" <> Text.replace "\n" "\\n" text <> "`"

median :: [Double] -> Double
median [] = 0
median values =
    let ordered = sort values
        count = length ordered
        middle = count `div` 2
    in if odd count
        then ordered !! middle
        else (ordered !! (middle - 1) + ordered !! middle) / 2

formatDouble :: Double -> Text
formatDouble value = Text.pack (showFFloat2 value)

formatOptionalMedian :: [Double] -> Text
formatOptionalMedian [] = "n/a"
formatOptionalMedian values =
    Text.pack (show (round (median values) :: Int))

formatOptionalDoubleMedian :: [Double] -> Text
formatOptionalDoubleMedian [] = "n/a"
formatOptionalDoubleMedian values = formatDouble (median values)

showFFloat2 :: Double -> String
showFFloat2 value =
    let scaled = round (value * 100) :: Integer
        (whole, fraction) = scaled `divMod` 100
    in show whole <> "." <> pad2 fraction

pad2 :: Integer -> String
pad2 value
    | value < 10 = "0" <> show value
    | otherwise = show value

exitCodeNumber :: ExitCode -> Int
exitCodeNumber ExitSuccess = 0
exitCodeNumber (ExitFailure code) = code

trim :: String -> String
trim = Text.unpack . Text.strip . Text.pack
