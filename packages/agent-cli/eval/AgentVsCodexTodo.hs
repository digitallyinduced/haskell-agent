module Main (main) where

import Agent.CLI.Session
    ( SessionMeta(..)
    , SessionTurn(..)
    , loadSession
    , sessionConversationText
    )
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (finally, tryIO)
import Control.Monad (forM, forM_, unless, when)
import Data.Aeson
    ( ToJSON(..)
    , Value(..)
    , decode
    , encode
    , fromJSON
    , object
    , Result(..)
    , (.=)
    )
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.List (sort)
import qualified Data.List as List
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
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
import System.Environment (getArgs, getEnvironment)
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath (takeDirectory, takeExtension, (</>))
import System.IO (IOMode(..), hPutStrLn, stderr, withFile)
import System.OsPath (unsafeEncodeUtf)
import System.Process
    ( CreateProcess(..)
    , ProcessHandle
    , StdStream(..)
    , getPid
    , getProcessExitCode
    , proc
    , readCreateProcessWithExitCode
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

data Runner = AgentCli | AgentCliRlm | Codex
    deriving (Eq, Ord, Show)

data Config = Config
    { agentBin :: !FilePath
    , codexBin :: !FilePath
    , resultsDir :: !FilePath
    , model :: !Text
    , effort :: !Text
    , trials :: !Int
    , timeoutSeconds :: !Int
    , selectedRunner :: !(Maybe Runner)
    }

data RunResult = RunResult
    { resultTrial :: !Int
    , resultRunner :: !Runner
    , resultPassed :: !Bool
    , resultGrade :: !Text
    , resultExitCode :: !Int
    , resultTimedOut :: !Bool
    , resultSeconds :: !Double
    , resultGradeSeconds :: !Double
    , resultInputTokens :: !(Maybe Int)
    , resultOutputTokens :: !(Maybe Int)
    , resultCachedTokens :: !(Maybe Int)
    , resultUncachedInputTokens :: !(Maybe Int)
    , resultSelfVerified :: !Bool
    , resultDelegated :: !Bool
    , resultSessionId :: !(Maybe Text)
    , resultWorkspace :: !FilePath
    , resultStdoutLog :: !FilePath
    , resultStderrLog :: !FilePath
    }

data TokenUsage = TokenUsage
    { inputTokens :: !(Maybe Int)
    , outputTokens :: !(Maybe Int)
    , cachedTokens :: !(Maybe Int)
    , selfVerified :: !Bool
    , delegated :: !Bool
    }

data HttpResponse = HttpResponse
    { responseStatus :: !Int
    , responseContentType :: !Text
    , responseBody :: !LBS.ByteString
    }

instance ToJSON Runner where
    toJSON AgentCli = toJSON ("agent-cli" :: Text)
    toJSON AgentCliRlm = toJSON ("agent-cli-rlm" :: Text)
    toJSON Codex = toJSON ("codex" :: Text)

instance ToJSON RunResult where
    toJSON result = object
        [ "trial" .= result.resultTrial
        , "runner" .= result.resultRunner
        , "passed" .= result.resultPassed
        , "grade" .= result.resultGrade
        , "exitCode" .= result.resultExitCode
        , "timedOut" .= result.resultTimedOut
        , "seconds" .= result.resultSeconds
        , "gradeSeconds" .= result.resultGradeSeconds
        , "inputTokens" .= result.resultInputTokens
        , "outputTokens" .= result.resultOutputTokens
        , "cachedTokens" .= result.resultCachedTokens
        , "uncachedInputTokens" .= result.resultUncachedInputTokens
        , "selfVerified" .= result.resultSelfVerified
        , "delegated" .= result.resultDelegated
        , "sessionId" .= result.resultSessionId
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
    ensureExecutionEnvironment
    prepareResultsDirectory config.resultsDir
    Text.writeFile (config.resultsDir </> "prompt.txt") todoPrompt
    versions <- collectVersions config
    let scheduled =
            [ (trial, runner)
            | trial <- [1 .. config.trials]
            , runner <- runnerOrder trial
            , maybe True (== runner) config.selectedRunner
            ]
    results <- forM scheduled \(trial, runner) -> do
        putStrLn $
            "Running todo-app trial " <> show trial
                <> " (" <> runnerSlug runner <> ")"
        runOne config trial runner
    LBS.writeFile (config.resultsDir </> "results.json") (encode results)
    Text.writeFile (config.resultsDir </> "summary.md")
        (renderSummary config versions results)
    putStrLn ""
    Text.putStrLn (renderConsoleSummary results)
    unless (all (.resultPassed) results) exitFailure

parseConfig :: [String] -> Either String Config
parseConfig = go Config
    { agentBin = "." </> "agent-cli"
    , codexBin = "codex"
    , resultsDir = "." </> "eval-results" </> "agent-vs-codex-todo"
    , model = ""
    , effort = "medium"
    , trials = 1
    , timeoutSeconds = 900
    , selectedRunner = Nothing
    }
  where
    go config = \case
        []
            | Text.null config.model -> Left ("--model is required\n\n" <> usage)
            | otherwise -> Right config
        "--agent-bin" : value : rest ->
            go config { agentBin = value } rest
        "--codex-bin" : value : rest ->
            go config { codexBin = value } rest
        "--results-dir" : value : rest ->
            go config { resultsDir = value } rest
        "--model" : value : rest ->
            go config { model = Text.pack value } rest
        "--effort" : value : rest
            | value `elem` ["none", "low", "medium", "high", "xhigh"] ->
                go config { effort = Text.pack value } rest
            | otherwise ->
                Left "--effort expects none, low, medium, high, or xhigh"
        "--trials" : value : rest -> case reads value of
            [(count, "")] | count > 0 ->
                go config { trials = count } rest
            _ -> Left "--trials expects a positive integer"
        "--timeout-seconds" : value : rest -> case reads value of
            [(seconds, "")] | seconds > 0 ->
                go config { timeoutSeconds = seconds } rest
            _ -> Left "--timeout-seconds expects a positive integer"
        "--runner" : value : rest -> case value of
            "agent-cli" -> go config { selectedRunner = Just AgentCli } rest
            "agent-cli-rlm" ->
                go config { selectedRunner = Just AgentCliRlm } rest
            "codex" -> go config { selectedRunner = Just Codex } rest
            _ -> Left "--runner expects agent-cli, agent-cli-rlm, or codex"
        "--help" : _ -> Left usage
        unknown : _ -> Left ("unknown eval argument: " <> unknown <> "\n\n" <> usage)

usage :: String
usage = unlines
    [ "Usage: eval-agent-vs-codex-todo --model MODEL [OPTIONS]"
    , ""
    , "Options:"
    , "  --agent-bin PATH       agent-cli executable"
    , "  --codex-bin PATH       Codex executable (default: codex)"
    , "  --results-dir DIR      New or empty artifact directory"
    , "  --model MODEL          Exact model passed to both runners (required)"
    , "  --effort LEVEL         Same reasoning effort for both (default: medium)"
    , "  --trials N             Repetitions per runner (default: 1)"
    , "  --timeout-seconds N    Agent timeout per run (default: 900)"
    , "  --runner NAME          Run only agent-cli, agent-cli-rlm, or codex"
    ]

todoPrompt :: Text
todoPrompt = Text.unlines
    [ "Build a complete Haskell todo application in the empty workspace."
    , "Do not only explain the solution: create all files and verify the app."
    , ""
    , "Requirements:"
    , "- Use a Nix flake for all dependencies and a development environment."
    , "- Use GHC 9.10."
    , "- `nix run` must build and start the application."
    , "- The application is an HTTP server."
    , "- Read the listening port from the `PORT` environment variable, defaulting to 3000."
    , "- `GET /tasks` returns a JSON array of tasks."
    , "- A task has exactly these fields: `id` (Int), `name` (Text), and `description` (Text)."
    , "- `POST /tasks` accepts JSON with `name` and `description`, creates a task,"
    , "  assigns an integer id, returns the created task as JSON, and uses HTTP 201."
    , "- `DELETE /tasks/:id` deletes an existing task and uses HTTP 204."
    , "- Store all task state in memory in an `MVar`; do not use a database or disk persistence."
    , "- Use `application/json` for JSON responses."
    , ""
    , "Fair-evaluation constraints:"
    , "- Do not spawn subagents or use web search; do the implementation and verification yourself."
    , "- Verify GHC with `nix develop path:. -c ghc --numeric-version`."
    , "- Build with `nix build path:.`."
    , "- Start the server with `nix run path:.` and test GET, POST, and DELETE with `curl`."
    , "- End your final response with exactly `SELF_VERIFIED: yes` only after those checks succeed."
    ]

rlmTodoPrompt :: Text
rlmTodoPrompt =
    Text.replace
        "- Do not spawn subagents or use web search; do the implementation and verification yourself."
        "- Use the RLM worker helpers for delegated inspection and implementation; do not use web search."
        todoPrompt

runOne :: Config -> Int -> Runner -> IO RunResult
runOne config trial runner = do
    let runName = "todo-trial-" <> show trial <> "-" <> runnerSlug runner
        workspace = config.resultsDir </> "workspaces" </> runName
        stdoutLog = config.resultsDir </> "logs" </> runName <> ".stdout.log"
        stderrLog = config.resultsDir </> "logs" </> runName <> ".stderr.log"
        port = 38000 + trial * 10 + runnerOffset runner
    resetDirectory workspace
    createDirectoryIfMissing True (takeDirectory stdoutLog)
    _ <- readProcessWithExitCode "git" ["-C", workspace, "init", "--quiet"] ""
    home <- getHomeDirectory
    executionEnv <- executionEnvironment
    let sessionsDir = home </> ".haskell-agent" </> "sessions"
        processSpec = (case runner of
            AgentCli ->
                proc config.agentBin
                    [ "--cwd", workspace
                    , "--prompt", Text.unpack todoPrompt
                    , "--save-session"
                    , "--no-agents-md"
                    , "--no-skills"
                    , "--no-subagents"
                    , "--bash"
                    , "--no-ghci"
                    , "--yolo"
                    , "--max-turns", "50"
                    , "--provider", "openai"
                    , "--model", Text.unpack config.model
                    , "--effort", Text.unpack config.effort
                    ]
            AgentCliRlm ->
                proc config.agentBin
                    [ "--cwd", workspace
                    , "--prompt", Text.unpack rlmTodoPrompt
                    , "--save-session"
                    , "--no-agents-md"
                    , "--no-skills"
                    , "--no-subagents"
                    , "--rlm"
                    , "--rlm-model", Text.unpack config.model
                    , "--rlm-effort", Text.unpack config.effort
                    , "--yolo"
                    , "--max-turns", "50"
                    , "--provider", "openai"
                    , "--model", Text.unpack config.model
                    , "--effort", Text.unpack config.effort
                    ]
            Codex ->
                proc config.codexBin
                    [ "exec"
                    , "--cd", workspace
                    , "--model", Text.unpack config.model
                    , "-c", "model_reasoning_effort=" <> show (Text.unpack config.effort)
                    , "--dangerously-bypass-approvals-and-sandbox"
                    , "--ephemeral"
                    , "--ignore-user-config"
                    , "--ignore-rules"
                    , "--json"
                    , "--color", "never"
                    , Text.unpack todoPrompt
                    ]
            ) { env = Just executionEnv }
    started <- getCurrentTime
    exitCode <- runProcessWithTimeout
        config.timeoutSeconds processSpec stdoutLog stderrLog
    ended <- getCurrentTime
    sessionId <- case runner of
        AgentCli -> sessionIdFromLog stderrLog
        AgentCliRlm -> sessionIdFromLog stderrLog
        Codex -> codexSessionId stdoutLog
    usageResult <- case runner of
        AgentCli -> agentUsage sessionsDir config.resultsDir runName sessionId
        AgentCliRlm -> agentUsage sessionsDir config.resultsDir runName sessionId
        Codex -> codexUsage stdoutLog
    gradeStarted <- getCurrentTime
    (graded, gradeMessage) <- gradeTodo workspace port
    gradeEnded <- getCurrentTime
    let timedOut = exitCode == ExitFailure 124
        comparable =
            usageResult.selfVerified
                && (runner == AgentCliRlm || not usageResult.delegated)
        finalGrade = Text.intercalate "; "
            [ if timedOut then "timed out" else "agent exited"
            , "self-verified=" <> yesNo usageResult.selfVerified
            , "delegated=" <> yesNo usageResult.delegated
            , gradeMessage
            ]
    pure RunResult
        { resultTrial = trial
        , resultRunner = runner
        , resultPassed = graded && comparable && exitCode == ExitSuccess
        , resultGrade = finalGrade
        , resultExitCode = exitCodeNumber exitCode
        , resultTimedOut = timedOut
        , resultSeconds = realToFrac (diffUTCTime ended started)
        , resultGradeSeconds = realToFrac (diffUTCTime gradeEnded gradeStarted)
        , resultInputTokens = usageResult.inputTokens
        , resultOutputTokens = usageResult.outputTokens
        , resultCachedTokens = usageResult.cachedTokens
        , resultUncachedInputTokens =
            uncachedInput usageResult.inputTokens usageResult.cachedTokens
        , resultSelfVerified = usageResult.selfVerified
        , resultDelegated = usageResult.delegated
        , resultSessionId = sessionId
        , resultWorkspace = workspace
        , resultStdoutLog = stdoutLog
        , resultStderrLog = stderrLog
        }

agentUsage
    :: FilePath
    -> FilePath
    -> FilePath
    -> Maybe Text
    -> IO TokenUsage
agentUsage _ _ _ Nothing = pure emptyUsage
agentUsage sessionsDir resultsRoot runName (Just identifier) = do
    loaded <- loadSession (unsafeEncodeUtf sessionsDir) identifier
    case loaded of
        Left err -> do
            hPutStrLn stderr ("could not load eval session: " <> Text.unpack err)
            pure emptyUsage
        Right (meta, turns) -> do
            copySessionArtifacts resultsRoot runName sessionsDir identifier
            pure (sessionUsage meta turns)

sessionUsage :: SessionMeta -> [SessionTurn] -> TokenUsage
sessionUsage _ [] = emptyUsage
sessionUsage meta turns = TokenUsage
    { inputTokens = Just meta.metaInputTokens
    , outputTokens = Just meta.metaOutputTokens
    , cachedTokens = Just meta.metaCachedTokens
    , selfVerified = "SELF_VERIFIED: yes" `Text.isInfixOf` sessionConversationText turns
    , delegated = any delegatedTurn turns
    }
  where
    delegatedTurn SessionTurn { turnItems = items } =
        let encoded = Text.decodeUtf8 (LBS.toStrict (encode items))
        in "spawn_agent" `Text.isInfixOf` encoded
            || "rlmQuery" `Text.isInfixOf` encoded
            || "rlmCode" `Text.isInfixOf` encoded

emptyUsage :: TokenUsage
emptyUsage = TokenUsage Nothing Nothing Nothing False False

codexUsage :: FilePath -> IO TokenUsage
codexUsage path = do
    events <- readJsonLines path
    pure $ case
        [ usage
        | Object event <- events
        , lookupText "type" event == Just "turn.completed"
        , Just (Object usage) <- [lookupKey "usage" event]
        ] of
        [] -> emptyUsage
        usages ->
            let finalUsage = last usages
            in TokenUsage
                { inputTokens = lookupInt "input_tokens" finalUsage
                , outputTokens = lookupInt "output_tokens" finalUsage
                , cachedTokens =
                    lookupInt "cached_input_tokens" finalUsage
                        <|> nestedInt
                            ["input_tokens_details", "cached_tokens"]
                            finalUsage
                , selfVerified = any
                    selfVerifiedEvent
                    events
                , delegated = any
                    delegatedEvent
                    events
                }
  where
    selfVerifiedEvent (Object event) =
        lookupText "type" event == Just "item.completed"
            && case lookupKey "item" event of
                Just (Object item) ->
                    lookupText "type" item == Just "agent_message"
                        && maybe False ("SELF_VERIFIED: yes" `Text.isInfixOf`) (lookupText "text" item)
                _ -> False
    selfVerifiedEvent _ = False
    delegatedEvent (Object event) =
        lookupText "type" event == Just "item.completed"
            && case lookupKey "item" event of
                Just (Object item) ->
                    lookupText "type" item == Just "command_execution"
                        && maybe False ("spawn_agent" `Text.isInfixOf`) (lookupText "command" item)
                _ -> False
    delegatedEvent _ = False

codexSessionId :: FilePath -> IO (Maybe Text)
codexSessionId path = do
    events <- readJsonLines path
    pure $ case catMaybes (map eventSessionId events) of
        identifier : _ -> Just identifier
        [] -> Nothing
  where
    eventSessionId (Object event) =
        lookupText "thread_id" event <|> lookupText "session_id" event
    eventSessionId _ = Nothing

readJsonLines :: FilePath -> IO [Value]
readJsonLines path = do
    exists <- doesFileExist path
    if not exists
        then pure []
        else do
            contents <- Text.readFile path
            pure
                [ value
                | line <- Text.lines contents
                , Just value <-
                    [decode (LBS.fromStrict (Text.encodeUtf8 line))]
                ]

lookupKey :: Text -> KeyMap.KeyMap Value -> Maybe Value
lookupKey name = KeyMap.lookup (Key.fromText name)

lookupText :: Text -> KeyMap.KeyMap Value -> Maybe Text
lookupText name fields = case lookupKey name fields of
    Just (String value) -> Just value
    _ -> Nothing

lookupInt :: Text -> KeyMap.KeyMap Value -> Maybe Int
lookupInt name fields = lookupKey name fields >>= valueInt

nestedInt :: [Text] -> KeyMap.KeyMap Value -> Maybe Int
nestedInt [] _ = Nothing
nestedInt [name] fields = lookupInt name fields
nestedInt (name : rest) fields = case lookupKey name fields of
    Just (Object nested) -> nestedInt rest nested
    _ -> Nothing

valueInt :: Value -> Maybe Int
valueInt value = case fromJSON value of
    Success number -> Just number
    Error _ -> Nothing

uncachedInput :: Maybe Int -> Maybe Int -> Maybe Int
uncachedInput (Just input) (Just cached) = Just (max 0 (input - cached))
uncachedInput _ _ = Nothing

ensureExecutionEnvironment :: IO ()
ensureExecutionEnvironment = do
    forM_ [("nix", ["--version"]), ("curl", ["--version"]), ("bash", ["--version"])] $
        \(executable, args) ->
            tryIO (readProcessWithExitCode executable args "") >>= \case
                Right (ExitSuccess, _, _) -> pure ()
                Right (_, _, err) ->
                    hPutStrLn stderr
                        ("execution preflight failed for " <> executable <> ": " <> err)
                        >> exitFailure
                Left err ->
                    hPutStrLn stderr
                        ("execution preflight failed for " <> executable <> ": " <> show err)
                        >> exitFailure

executionEnvironment :: IO [(String, String)]
executionEnvironment = getEnvironment

gradeTodo :: FilePath -> Int -> IO (Bool, Text)
gradeTodo workspace port = do
    flakeExists <- doesFileExist (workspace </> "flake.nix")
    ghcCheck <- if not flakeExists
        then pure (False, "missing flake.nix")
        else do
            ghcResult <- timeout (5 * 60 * 1000000) $
                readCreateProcessWithExitCode
                    (proc "nix" ["develop", "path:.", "-c", "ghc", "--numeric-version"])
                        { cwd = Just workspace }
                    ""
            pure $ case ghcResult of
                Just (ExitSuccess, version, _) ->
                    ("9.10." `Text.isPrefixOf` Text.strip (Text.pack version),
                        "GHC version " <> Text.strip (Text.pack version))
                Just (_, _, err) ->
                    (False, "nix develop failed: " <> oneLine (Text.pack err))
                Nothing -> (False, "nix develop timed out")
    mvarCheck <- workspaceUsesMVar workspace
    httpCheck <- if flakeExists
        then runHttpGrade workspace port
        else pure (False, "cannot run app without flake.nix")
    let checks =
            [ ("ghc-9.10", fst ghcCheck, snd ghcCheck)
            , ("mvar", mvarCheck, if mvarCheck then "MVar found in Haskell source" else "no MVar found in Haskell source")
            , ("http-crud", fst httpCheck, snd httpCheck)
            ]
        passed = all (\(_, ok, _) -> ok) checks
        message = Text.intercalate "; "
            [ name <> "=" <> (if ok then "pass" else "fail") <> " (" <> detail <> ")"
            | (name, ok, detail) <- checks
            ]
    pure (passed, message)

workspaceUsesMVar :: FilePath -> IO Bool
workspaceUsesMVar root = do
    files <- recursiveFiles root
    contents <- forM
        [ path | path <- files, takeExtension path `elem` [".hs", ".lhs"] ]
        Text.readFile
    let source = Text.unlines contents
    pure $
        "MVar" `Text.isInfixOf` source
            && any (`Text.isInfixOf` source)
                ["newMVar", "newEmptyMVar"]
            && any (`Text.isInfixOf` source)
                [ "modifyMVar", "modifyMVar_", "modifyMVarMasked"
                , "readMVar", "withMVar", "swapMVar", "takeMVar", "putMVar"
                ]

recursiveFiles :: FilePath -> IO [FilePath]
recursiveFiles root = do
    entries <- listDirectory root
    fmap concat $ forM entries \entry -> do
        let path = root </> entry
        isDir <- doesDirectoryExist path
        if isDir && entry /= ".git"
            then recursiveFiles path
            else pure [path | not isDir]

runHttpGrade :: FilePath -> Int -> IO (Bool, Text)
runHttpGrade workspace port = do
    environment <- getEnvironment
    let stdoutPath = workspace </> ".eval-server.stdout.log"
        stderrPath = workspace </> ".eval-server.stderr.log"
    withFile stdoutPath WriteMode \stdoutHandle ->
        withFile stderrPath WriteMode \stderrHandle -> do
            let processSpec =
                    (proc "nix" ["run", "path:."])
                        { cwd = Just workspace
                        , env = Just (("PORT", show port) : filter ((/= "PORT") . fst) environment)
                        , std_out = UseHandle stdoutHandle
                        , std_err = UseHandle stderrHandle
                        , std_in = NoStream
                        , create_group = True
                        , new_session = True
                        }
            withCreateProcess processSpec \_ _ _ processHandle ->
                gradeRunningServer processHandle port
                    `finally` stopProcessTree processHandle

gradeRunningServer :: ProcessHandle -> Int -> IO (Bool, Text)
gradeRunningServer processHandle port = do
    ready <- waitForServer processHandle port 600
    if not ready
        then pure (False, "`nix run` did not expose GET /tasks within 5 minutes")
        else do
            initial <- curl port "GET" "/tasks" Nothing
            createdOne <- curl port "POST" "/tasks"
                (Just "{\"name\":\"write eval\",\"description\":\"compare harnesses\"}")
            let firstId = createdOne >>= responseTaskId
            afterFirst <- curl port "GET" "/tasks" Nothing
            createdTwo <- curl port "POST" "/tasks"
                (Just "{\"name\":\"publish report\",\"description\":\"record tokens and time\"}")
            afterSecond <- curl port "GET" "/tasks" Nothing
            deleted <- case firstId of
                Nothing -> pure Nothing
                Just identifier ->
                    curl port "DELETE" ("/tasks/" <> show identifier) Nothing
            final <- curl port "GET" "/tasks" Nothing
            let checks =
                    [ ("initial GET", maybe False isEmptyTaskList initial)
                    , ("first POST", maybe False validFirstTask createdOne)
                    , ("first persisted", maybe False (containsTask "write eval" "compare harnesses") afterFirst)
                    , ("second POST", maybe False validSecondTask createdTwo)
                    , ("two tasks", maybe False ((== 2) . taskCount) afterSecond)
                    , ("unique ids", uniqueCreatedIds createdOne createdTwo)
                    , ("DELETE", maybe False ((== 204) . (.responseStatus)) deleted)
                    , ("deletion persisted", maybe False (onlyTaskNamed "publish report") final)
                    ]
                failed = [name | (name, ok) <- checks, not ok]
            pure
                ( null failed
                , if null failed
                    then "GET/POST/DELETE behavior passed"
                    else "failed checks: " <> Text.intercalate ", " failed
                )

waitForServer :: ProcessHandle -> Int -> Int -> IO Bool
waitForServer _ _ attempts | attempts <= 0 = pure False
waitForServer processHandle port attempts = do
    exited <- getProcessExitCode processHandle
    case exited of
        Just _ -> pure False
        Nothing -> do
            response <- curl port "GET" "/tasks" Nothing
            case response of
                Just value | value.responseStatus == 200 -> pure True
                _ -> threadDelay 500000 >> waitForServer processHandle port (attempts - 1)

curl :: Int -> String -> String -> Maybe String -> IO (Maybe HttpResponse)
curl port method path body = do
    let url = "http://127.0.0.1:" <> show port <> path
        args =
            [ "--silent", "--show-error"
            , "--max-time", "2"
            , "--request", method
            , "--write-out", "\n__STATUS__%{http_code}\n__CONTENT_TYPE__%{content_type}"
            ]
                <> case body of
                    Nothing -> []
                    Just json ->
                        [ "--header", "Content-Type: application/json"
                        , "--data-binary", json
                        ]
                <> [url]
    result <- tryIO (readProcessWithExitCode "curl" args "")
    pure $ case result of
        Right (ExitSuccess, output, _) -> parseCurlOutput output
        _ -> Nothing

parseCurlOutput :: String -> Maybe HttpResponse
parseCurlOutput output = do
    let rows = lines output
        statusPrefix = "__STATUS__"
        contentTypePrefix = "__CONTENT_TYPE__"
    statusText <- case
        [ drop (length statusPrefix) row
        | row <- rows
        , statusPrefix `List.isPrefixOf` row
        ] of
        value : _ -> Just value
        [] -> Nothing
    let contentType = case
            [ drop (length contentTypePrefix) row
            | row <- rows
            , contentTypePrefix `List.isPrefixOf` row
            ] of
            value : _ -> value
            [] -> ""
    status <- case reads statusText of
        [(value, "")] -> Just value
        _ -> Nothing
    let bodyLines =
            takeWhile (not . List.isPrefixOf statusPrefix) rows
    pure HttpResponse
        { responseStatus = status
        , responseContentType = Text.pack contentType
        , responseBody = LBS.fromStrict (Text.encodeUtf8 (Text.pack (unlines bodyLines)))
        }

isEmptyTaskList :: HttpResponse -> Bool
isEmptyTaskList response =
    response.responseStatus == 200
        && isJsonResponse response
        && taskCount response == 0

validFirstTask :: HttpResponse -> Bool
validFirstTask response =
    response.responseStatus == 201
        && isJsonResponse response
        && containsTask "write eval" "compare harnesses" response
        && maybe False (> 0) (responseTaskId response)

validSecondTask :: HttpResponse -> Bool
validSecondTask response =
    response.responseStatus == 201
        && isJsonResponse response
        && containsTask "publish report" "record tokens and time" response
        && maybe False (> 0) (responseTaskId response)

isJsonResponse :: HttpResponse -> Bool
isJsonResponse response =
    "application/json" `Text.isPrefixOf`
        Text.toLower response.responseContentType

uniqueCreatedIds :: Maybe HttpResponse -> Maybe HttpResponse -> Bool
uniqueCreatedIds first second = case (first >>= responseTaskId, second >>= responseTaskId) of
    (Just firstId, Just secondId) -> firstId /= secondId
    _ -> False

responseTaskId :: HttpResponse -> Maybe Int
responseTaskId response = do
    Object fields <- decode response.responseBody
    lookupInt "id" fields

taskCount :: HttpResponse -> Int
taskCount response = case decode response.responseBody of
    Just (Array tasks) -> length tasks
    _ -> -1

containsTask :: Text -> Text -> HttpResponse -> Bool
containsTask expectedName expectedDescription response =
    isJsonResponse response
        && case decode response.responseBody of
            Just (Array tasks) -> any matches tasks
            Just value -> matches value
            Nothing -> False
  where
    matches (Object fields) =
        lookupText "name" fields == Just expectedName
            && lookupText "description" fields == Just expectedDescription
            && maybe False (> 0) (lookupInt "id" fields)
            && KeyMap.size fields == 3
    matches _ = False

onlyTaskNamed :: Text -> HttpResponse -> Bool
onlyTaskNamed expected response =
    isJsonResponse response
        && case decode response.responseBody of
            Just (Array tasks) -> case toList tasks of
                [Object fields] ->
                    lookupText "name" fields == Just expected
                        && lookupText "description" fields == Just "record tokens and time"
                        && maybe False (> 0) (lookupInt "id" fields)
                        && KeyMap.size fields == 3
                _ -> False
            _ -> False

runnerOrder :: Int -> [Runner]
runnerOrder trial
    | odd trial = [AgentCli, AgentCliRlm, Codex]
    | otherwise = [Codex, AgentCliRlm, AgentCli]

runnerOffset :: Runner -> Int
runnerOffset AgentCli = 1
runnerOffset AgentCliRlm = 2
runnerOffset Codex = 3

runnerSlug :: Runner -> String
runnerSlug AgentCli = "agent-cli"
runnerSlug AgentCliRlm = "agent-cli-rlm"
runnerSlug Codex = "codex"

collectVersions :: Config -> IO [(Text, Text)]
collectVersions config = do
    agentVersion <- commandVersion config.agentBin ["--version"]
    codexVersion <- commandVersion config.codexBin ["--version"]
    pure [("agent-cli", agentVersion), ("codex", codexVersion)]

commandVersion :: FilePath -> [String] -> IO Text
commandVersion executable args = do
    result <- tryIO (readProcessWithExitCode executable args "")
    pure $ case result of
        Right (ExitSuccess, output, _) -> Text.strip (Text.pack output)
        Right (_, _, err) -> "unavailable: " <> oneLine (Text.pack err)
        Left err -> "unavailable: " <> Text.pack (show err)

renderSummary :: Config -> [(Text, Text)] -> [RunResult] -> Text
renderSummary config versions results =
    Text.unlines
        [ "# agent-cli vs Codex: Haskell todo application"
        , ""
        , "Both runners received the same task prompt, model, reasoning effort,"
        , "empty Git workspace, auto-approval policy, and per-run timeout."
        , "Wall time covers the coding-agent process only; deterministic grading"
        , "runs afterward and is not included."
        , ""
        , "- Model: `" <> config.model <> "`"
        , "- Reasoning effort: `" <> config.effort <> "`"
        , "- Trials per runner: " <> Text.pack (show config.trials)
        , "- Per-run timeout: " <> Text.pack (show config.timeoutSeconds) <> " seconds"
        ]
        <> Text.unlines
            [ "- " <> name <> " version: `" <> version <> "`"
            | (name, version) <- versions
            ]
        <> Text.unlines
            [ ""
            , renderConsoleSummary results
            , ""
            , "## Individual runs"
            , ""
            , "| trial | runner | pass | self-verified | delegated | timeout | agent seconds | grade seconds | input | uncached | output | cached | grade |"
            , "|---:|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---|"
            ]
        <> Text.unlines (map renderRunRow results)

renderConsoleSummary :: [RunResult] -> Text
renderConsoleSummary results =
    Text.unlines $
        [ "| runner | passed | median successful seconds | median successful input | median successful uncached | median successful output | median successful cached |"
        , "|---|---:|---:|---:|---:|---:|---:|"
        ]
            <> map renderRunnerSummary [AgentCli, AgentCliRlm, Codex]
  where
    renderRunnerSummary runner =
        let selected = filter ((== runner) . (.resultRunner)) results
            successful = filter (.resultPassed) selected
        in Text.intercalate " | "
            [ "| " <> Text.pack (runnerSlug runner)
            , Text.pack (show (length successful) <> "/" <> show (length selected))
            , optionalMedian (map (.resultSeconds) successful)
            , optionalIntMedian (catMaybes (map (.resultInputTokens) successful))
            , optionalIntMedian (catMaybes (map (.resultUncachedInputTokens) successful))
            , optionalIntMedian (catMaybes (map (.resultOutputTokens) successful))
            , optionalIntMedian (catMaybes (map (.resultCachedTokens) successful)) <> " |"
            ]

renderRunRow :: RunResult -> Text
renderRunRow result = Text.intercalate " | "
    [ "| " <> Text.pack (show result.resultTrial)
    , Text.pack (runnerSlug result.resultRunner)
    , yesNo result.resultPassed
    , yesNo result.resultSelfVerified
    , yesNo result.resultDelegated
    , yesNo result.resultTimedOut
    , formatDouble result.resultSeconds
    , formatDouble result.resultGradeSeconds
    , maybe "n/a" (Text.pack . show) result.resultInputTokens
    , maybe "n/a" (Text.pack . show) result.resultUncachedInputTokens
    , maybe "n/a" (Text.pack . show) result.resultOutputTokens
    , maybe "n/a" (Text.pack . show) result.resultCachedTokens
    , Text.replace "|" "\\|" result.resultGrade <> " |"
    ]

yesNo :: Bool -> Text
yesNo True = "yes"
yesNo False = "no"

optionalMedian :: [Double] -> Text
optionalMedian [] = "n/a"
optionalMedian values = formatDouble (median values)

optionalIntMedian :: [Int] -> Text
optionalIntMedian [] = "n/a"
optionalIntMedian values =
    Text.pack (show (round (median (map fromIntegral values)) :: Int))

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
formatDouble value =
    let scaled = round (value * 100) :: Integer
        (whole, fraction) = scaled `divMod` 100
    in Text.pack (show whole <> "." <> if fraction < 10 then "0" <> show fraction else show fraction)

exitCodeNumber :: ExitCode -> Int
exitCodeNumber ExitSuccess = 0
exitCodeNumber (ExitFailure code) = code

oneLine :: Text -> Text
oneLine = Text.unwords . Text.words

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
                identifier : _ -> Just identifier
                [] -> Nothing

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

resetDirectory :: FilePath -> IO ()
resetDirectory path = do
    exists <- doesDirectoryExist path
    when exists (removePathForcibly path)
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
                    { std_in = NoStream
                    , std_out = UseHandle stdoutHandle
                    , std_err = UseHandle stderrHandle
                    , create_group = True
                    , new_session = True
                    }
                \_ _ _ processHandle -> do
                    completed <- timeout
                        (seconds * 1000000)
                        (waitForProcess processHandle)
                    case completed of
                        Just code -> pure code
                        Nothing -> stopProcessTree processHandle >> pure (ExitFailure 124)

stopProcessTree :: ProcessHandle -> IO ()
stopProcessTree processHandle = do
    root <- getPid processHandle
    processIds <- case root of
        Nothing -> pure []
        Just rootPid -> processTreeIds rootPid
    signalProcesses processIds sigTERM
    _ <- timeout 3000000 (waitForProcess processHandle)
    signalProcesses processIds sigKILL
    _ <- timeout 3000000 (waitForProcess processHandle)
    pure ()

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

(<|>) :: Maybe a -> Maybe a -> Maybe a
Just value <|> _ = Just value
Nothing <|> other = other

toList :: Foldable f => f a -> [a]
toList = foldr (:) []
