module Agent.Tools.GrokSpec (spec) where

import Agent.Loop (LoopError(..), defaultLoopDispatch)
import System.OsPath (OsPath, decodeUtf, unsafeEncodeUtf)
import Agent.Provider (Provider(..))
import Agent.Subagents (SubagentId, closeSubagentRegistry, defaultSubagentConfig, newSubagentRegistry)
import Agent.ToolDispatch (ToolCallResult(..), dispatchToolCall, functionToolCall)
import Agent.ToolDSL (PropertySchema(..))
import Agent.Tools (CodingTools(..), appToolHandlers, codingToolsFor, defaultToolEnv)
import Agent.Tools.Types (jsonToolParameters)
import Agent.Tools.Grok (closeGrokSession, grokTools, newGrokSession)
import Agent.Tools.Grok.Task (GrokSubagentSpec)
import Agent.Tools.Ghci (GhciSession, closeGhciSession, newGhciSession)
import Agent.Tools.IO (CommandResult(..))
import Agent.Tools.Grok.Shell
    ( GrokSession
    , grokSessionEnv
    , grokSessionEnvFile
    , hasUnwaitedBackgroundOp
    , killTask
    , readTaskOutput
    , runForegroundStreaming
    , startBackground
    )
import Agent.Subagents.TaskPath (taskPathRoot)
import Agent.Tools.MultiAgents (MultiAgentContext(..))
import Agent.Tools.PlanMode (PlanModeEnv, activatePlanMode, newPlanModeEnv)
import Agent.Tools.Types (AppTool(..), ToolEnv(..))
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Control.Exception (bracket, bracket_, finally)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory
    ( createDirectoryIfMissing
    , createDirectoryLink
    , doesFileExist
    , findExecutable
    , getTemporaryDirectory
    )
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Posix.Files (setFileMode)
import System.Posix.Temp (mkdtemp)
import System.Directory (removeDirectoryRecursive)
import System.Timeout (timeout)
import Test.Hspec

fromFilePath = unsafeEncodeUtf
toFilePath path = either (error . show) id (decodeUtf path)

spec :: Spec
spec = describe "Agent.Tools.Grok" do
    it "advertises grok-build wire names and not Codex names" do
        withTempSession \(session, ghci) -> do
            plan <- newPlanModeEnv (sessionCwd session) Nothing
            typesRef <- newIORef (Map.empty :: Map SubagentId GrokSubagentSpec)
            let names = map (.appToolName) (grokTools session ghci plan Nothing typesRef)
            names `shouldBe`
                [ "read_file"
                , "grep"
                , "list_dir"
                , "search_replace"
                , "run_terminal_cmd"
                , "run_ghci"
                , "get_task_output"
                , "kill_task"
                , "enter_plan_mode"
                , "exit_plan_mode"
                , "ask_user_question"
                ]
            names `shouldNotContain` ["apply_patch"]
            names `shouldNotContain` ["shell_command"]
            let outputSchemas =
                    [ fromMaybe [] (jsonToolParameters tool)
                    | tool <- grokTools session ghci plan Nothing typesRef
                    , tool.appToolName == "get_task_output"
                    ]
            map (map (.propertyName)) outputSchemas
                `shouldBe` [["task_ids", "timeout_ms"]]
            xai <- codingToolsFor XAIProvider
                (grokSessionEnv session) Nothing Nothing
            openrouter <- codingToolsFor OpenRouterProvider
                (grokSessionEnv session) Nothing Nothing
            (do
                map (.appToolName) xai.codingAppTools `shouldBe` names
                map (.appToolName) openrouter.codingAppTools `shouldBe` names)
                `finally` (xai.codingClose >> openrouter.codingClose)


    it "registers task when a multi-agent context is provided" do
        withTempSession \(session, ghci) -> do
            plan <- newPlanModeEnv (sessionCwd session) Nothing
            registry <- newSubagentRegistry defaultSubagentConfig
                (sessionCwd session)
                (\_ _ _ _ -> pure $ Left LoopNoResponseId)
                (\_ _ -> pure ())
            typesRef <- newIORef Map.empty
            let ctx = MultiAgentContext registry Nothing 0 taskPathRoot
                    (pure Nothing) Nothing Nothing Nothing Nothing
                names = map (.appToolName) (grokTools session ghci plan (Just ctx) typesRef)
            names `shouldContain` ["task"]
            closeSubagentRegistry registry

    it "reads a file with grok line-number anchors" do
        withTempSession \(session, ghci) -> do
            let path = toFilePath (sessionCwd session) </> "sample.txt"
            Text.writeFile path (Text.unlines ["alpha", "bravo", "charlie"])
            output <- runTool session ghci "read_file" "{\"target_file\":\"sample.txt\"}"
            output `shouldSatisfy` Text.isPrefixOf "1\8594alpha"
            output `shouldSatisfy` Text.isInfixOf "bravo"

    it "rejects a path that escapes cwd" do
        withTempSession \(session, ghci) -> do
            let cwd = toFilePath (sessionCwd session)
                name = takeFileName cwd <> "-outside.txt"
                outsider = takeDirectory cwd </> name
            Text.writeFile outsider "secret"
            relative <- runTool session ghci "read_file"
                ("{\"target_file\":\"../" <> Text.pack name <> "\"}")
            relative `shouldSatisfy` Text.isInfixOf "escapes"
            absolute <- runTool session ghci "read_file" "{\"target_file\":\"/etc/passwd\"}"
            absolute `shouldSatisfy` Text.isInfixOf "escapes"

    it "lists directory entries with a trailing slash for folders" do
        withTempSession \(session, ghci) -> do
            createDirectoryIfMissing True
                (toFilePath (sessionCwd session) </> "sub")
            Text.writeFile
                (toFilePath (sessionCwd session) </> "a.txt") "x"
            output <- runTool session ghci "list_dir" "{\"target_directory\":\".\"}"
            output `shouldSatisfy` Text.isInfixOf "a.txt"
            output `shouldSatisfy` Text.isInfixOf "sub/"

    it "does not descend through directory symlinks" do
        withTempSession \(session, ghci) -> do
            let cwd = toFilePath (sessionCwd session)
                outside = cwd </> ".outside"
            createDirectoryIfMissing True outside
            Text.writeFile (outside </> "secret.txt") "secret"
            createDirectoryLink outside (cwd </> "outside-link")
            output <- runTool session ghci "list_dir" "{\"target_directory\":\".\"}"
            output `shouldSatisfy` Text.isInfixOf "outside-link"
            output `shouldNotSatisfy` Text.isInfixOf "secret.txt"

    it "creates a file with empty old_string and replaces a unique match" do
        withTempSession \(session, ghci) -> do
            created <- runTool session ghci "search_replace"
                "{\"file_path\":\"new.txt\",\"old_string\":\"\",\"new_string\":\"hello world\\n\"}"
            created `shouldSatisfy` Text.isInfixOf "created successfully"
            doesFileExist
                (toFilePath (sessionCwd session) </> "new.txt")
                `shouldReturn` True

            updated <- runTool session ghci "search_replace"
                "{\"file_path\":\"new.txt\",\"old_string\":\"hello\",\"new_string\":\"goodbye\"}"
            updated `shouldSatisfy` Text.isInfixOf "updated successfully"
            Text.readFile
                (toFilePath (sessionCwd session) </> "new.txt")
                `shouldReturn` "goodbye world\n"

    it "refuses a non-unique search_replace without replace_all" do
        withTempSession \(session, ghci) -> do
            let path = toFilePath (sessionCwd session) </> "dup.txt"
            Text.writeFile path "aaa bbb aaa\n"
            output <- runTool session ghci "search_replace"
                "{\"file_path\":\"dup.txt\",\"old_string\":\"aaa\",\"new_string\":\"ccc\"}"
            output `shouldSatisfy` Text.isInfixOf "multiple times"
            Text.readFile path `shouldReturn` "aaa bbb aaa\n"

    it "grep finds a literal match" do
        withTempSession \(session, ghci) -> do
            Text.writeFile
                (toFilePath (sessionCwd session) </> "hit.txt")
                "needle in haystack\n"
            output <- runTool session ghci "grep" "{\"pattern\":\"needle\"}"
            output `shouldSatisfy` Text.isInfixOf "needle"

    it "grep accepts glob before the path terminator" do
        withTempSession \(session, ghci) -> do
            Text.writeFile
                (toFilePath (sessionCwd session) </> "hit.txt")
                "needle in haystack\n"
            Text.writeFile
                (toFilePath (sessionCwd session) </> "hit.md")
                "needle in markdown\n"
            output <- runTool session ghci "grep"
                "{\"pattern\":\"needle\",\"glob\":\"*.txt\"}"
            output `shouldSatisfy` Text.isInfixOf "hit.txt"
            output `shouldNotSatisfy` Text.isInfixOf "hit.md"
            output `shouldNotSatisfy` Text.isInfixOf "No such file or directory"

    it "rejects rm -rf via run_terminal_cmd even before execution" do
        withTempSession \(session, ghci) -> do
            output <- runTool session ghci "run_terminal_cmd"
                "{\"command\":\"rm -rf /tmp/should-not-run\",\"description\":\"dangerous delete\"}"
            output `shouldSatisfy` Text.isInfixOf "Blocked dangerous shell command"
            output `shouldSatisfy` Text.isInfixOf "rm -rf"

    it "runs a foreground shell command" do
        withTempSession \(session, ghci) -> do
            output <- runTool session ghci "run_terminal_cmd"
                "{\"command\":\"echo hi\",\"description\":\"print hi\"}"
            output `shouldSatisfy` Text.isPrefixOf "exit: 0"
            output `shouldSatisfy` Text.isInfixOf "hi"

    it "times out a long-running shell command" do
        withTempSession \(session, ghci) -> do
            output <- runTool session ghci "run_terminal_cmd"
                "{\"command\":\"sleep 5\",\"timeout\":\"200\",\"description\":\"timeout test\"}"
            output `shouldSatisfy` Text.isPrefixOf "exit: killed (timeout)"

    it "starts read_file from a negative offset" do
        withTempSession \(session, ghci) -> do
            Text.writeFile
                (toFilePath (sessionCwd session) </> "lines.txt")
                (Text.unlines ["a", "b", "c", "d"])
            output <- runTool session ghci "read_file" "{\"target_file\":\"lines.txt\",\"offset\":-2}"
            output `shouldNotSatisfy` Text.isInfixOf "beyond the end"
            output `shouldSatisfy` Text.isInfixOf "d"

    it "wraps grep output in a workspace_result card" do
        withTempSession \(session, ghci) -> do
            Text.writeFile
                (toFilePath (sessionCwd session) </> "hit.txt")
                "needle in haystack\n"
            output <- runTool session ghci "grep" "{\"pattern\":\"needle\"}"
            output `shouldSatisfy` Text.isInfixOf "<workspace_result"
            output `shouldSatisfy` Text.isInfixOf "needle"

    it "hints the nearest line when search_replace misses" do
        withTempSession \(session, ghci) -> do
            Text.writeFile
                (toFilePath (sessionCwd session) </> "near.txt")
                "alpha\nbravo unique\ncharlie\n"
            output <- runTool session ghci "search_replace"
                "{\"file_path\":\"near.txt\",\"old_string\":\"xyz unique\",\"new_string\":\"x\"}"
            output `shouldSatisfy` Text.isInfixOf "Nearest match: line 2"

    it "refuses to edit a gitignored file" do
        withTempSession \(session, ghci) -> do
            Text.writeFile
                (toFilePath (sessionCwd session) </> ".gitignore")
                "secret.txt\n"
            Text.writeFile
                (toFilePath (sessionCwd session) </> "secret.txt")
                "hidden\n"
            initOut <- runTool session ghci "run_terminal_cmd"
                "{\"command\":\"git init\",\"description\":\"init git\"}"
            initOut `shouldSatisfy` Text.isPrefixOf "exit: 0"
            output <- runTool session ghci "search_replace"
                "{\"file_path\":\"secret.txt\",\"old_string\":\"hidden\",\"new_string\":\"shown\"}"
            output `shouldSatisfy` Text.isInfixOf "gitignore"

    it "only edits plan.md while plan mode is active" do
        withTempSession \(session, ghci) -> do
            plan <- newPlanModeEnv (sessionCwd session) Nothing
            activatePlanMode plan
            blocked <- runToolWithPlan session ghci plan "search_replace"
                "{\"file_path\":\"blocked.txt\",\"old_string\":\"\",\"new_string\":\"no\"}"
            blocked `shouldSatisfy` Text.isInfixOf "only editable file is the plan file"
            doesFileExist
                (toFilePath (sessionCwd session) </> "blocked.txt")
                `shouldReturn` False
            allowed <- runToolWithPlan session ghci plan "search_replace"
                "{\"file_path\":\"plan.md\",\"old_string\":\"\",\"new_string\":\"# Plan\\n\"}"
            allowed `shouldSatisfy` Text.isInfixOf "created successfully"
            Text.readFile
                (toFilePath (sessionCwd session) </> "plan.md")
                `shouldReturn` "# Plan\n"

    it "persists cwd and exported env across run_terminal_cmd calls" do
        withTempSession \(session, ghci) -> do
            _ <- runTool session ghci "run_terminal_cmd"
                "{\"command\":\"mkdir nest && cd nest && export GROK_SESSION_VAR=persisted\",\"description\":\"cd and export\"}"
            pwdOut <- runTool session ghci "run_terminal_cmd"
                "{\"command\":\"pwd\",\"description\":\"show cwd\"}"
            pwdOut `shouldSatisfy` Text.isInfixOf "nest"
            envOut <- runTool session ghci "run_terminal_cmd"
                "{\"command\":\"echo $GROK_SESSION_VAR\",\"description\":\"show env\"}"
            envOut `shouldSatisfy` Text.isInfixOf "persisted"

    it "captures background environment at launch" do
        withTempSession \(session, ghci) -> do
            _ <- runTool session ghci "run_terminal_cmd"
                "{\"command\":\"export GROK_SESSION_VAR=before\",\"description\":\"seed env\"}"
            let cwd = toFilePath (sessionCwd session)
                shimDir = cwd </> "shim-bin"
                shimPath = shimDir </> "bash"
                claimPath = cwd </> "bash-shim-claim"
                startedPath = cwd </> "bash-shim-started"
                releasePath = cwd </> "bash-shim-release"
                observedPath = cwd </> "background-env"
            bashPath <- requireExecutable "bash"
            createDirectoryIfMissing True shimDir
            writeFile shimPath $ unlines
                [ "#!/bin/sh"
                , "if mkdir " <> shellQuote claimPath <> " 2>/dev/null; then"
                , "  touch " <> shellQuote startedPath
                , "  while [ ! -e " <> shellQuote releasePath
                    <> " ]; do sleep 0.01; done"
                , "fi"
                , "exec " <> shellQuote bashPath <> " \"$@\""
                ]
            setFileMode shimPath 0o700

            withPathPrefix shimDir do
                started <- startBackground session
                    "printf '%s' \"$GROK_SESSION_VAR\" > background-env"
                started `shouldSatisfy` either (const False) (Text.isInfixOf "task_id:")
                let taskId = either (const "") taskIdFrom started
                waitForFile startedPath
                foreground <- runTool session ghci "run_terminal_cmd"
                    "{\"command\":\"export GROK_SESSION_VAR=after\",\"description\":\"update env\"}"
                    `finally` writeFile releasePath ""
                foreground `shouldSatisfy` Text.isPrefixOf "exit: 0"
                finished <- readTaskOutput session taskId (Just 5000)
                finished `shouldSatisfy` Text.isPrefixOf "exit: 0"
                Text.readFile observedPath `shouldReturn` "before"

    it "rejects an un-waited & in a foreground command" do
        withTempSession \(session, ghci) -> do
            output <- runTool session ghci "run_terminal_cmd"
                "{\"command\":\"sleep 1 &\",\"description\":\"ampersand\"}"
            output `shouldSatisfy` Text.isInfixOf "background '&'"
            waited <- runTool session ghci "run_terminal_cmd"
                "{\"command\":\"sleep 0.1 & wait\",\"description\":\"wait for child\"}"
            waited `shouldSatisfy` Text.isPrefixOf "exit: 0"

    it "runs a background command, waits for output, and kills a task" do
        withTempSession \(session, ghci) -> do
            started <- runTool session ghci "run_terminal_cmd"
                "{\"command\":\"sleep 0.3 && echo bgdone\",\"description\":\"bg echo\",\"background\":true}"
            started `shouldSatisfy` Text.isInfixOf "task_id:"
            let taskId = taskIdFrom started
            snapshot <- runTool session ghci "get_task_output"
                ("{\"task_ids\":[\"" <> taskId <> "\"]}")
            snapshot `shouldSatisfy` \text ->
                Text.isInfixOf "still running" text || Text.isInfixOf "bgdone" text
            finished <- runTool session ghci "get_task_output"
                ("{\"task_ids\":[\"" <> taskId <> "\"],\"timeout_ms\":5000}")
            finished `shouldSatisfy` Text.isInfixOf "exit: 0"
            finished `shouldSatisfy` Text.isInfixOf "bgdone"

            longRunning <- runTool session ghci "run_terminal_cmd"
                "{\"command\":\"sleep 30\",\"description\":\"bg sleep\",\"background\":true}"
            let killId = taskIdFrom longRunning
            killed <- runTool session ghci "kill_task"
                ("{\"task_id\":\"" <> killId <> "\"}")
            killed `shouldSatisfy` Text.isInfixOf killId

    it "does not let a background command overwrite later foreground env" do
        withTempSession \(session, ghci) -> do
            started <- runTool session ghci "run_terminal_cmd"
                "{\"command\":\"sleep 0.4\",\"description\":\"bg sleeper\",\"background\":true}"
            let taskId = taskIdFrom started
            _ <- runTool session ghci "run_terminal_cmd"
                "{\"command\":\"export GROK_SESSION_VAR=fromfg\",\"description\":\"fg export\"}"
            _ <- runTool session ghci "get_task_output"
                ("{\"task_id\":\"" <> taskId <> "\",\"timeout\":2000}")
            envOut <- runTool session ghci "run_terminal_cmd"
                "{\"command\":\"echo $GROK_SESSION_VAR\",\"description\":\"show env\"}"
            envOut `shouldSatisfy` Text.isInfixOf "fromfg"

    it "returns output already emitted by a still-running background task" do
        withTempSession \(session, ghci) -> do
            started <- runTool session ghci "run_terminal_cmd"
                "{\"command\":\"echo liveout; touch flag; sleep 30\",\"description\":\"live snapshot\",\"background\":true}"
            let taskId = taskIdFrom started
            waitForFile (toFilePath (sessionCwd session) </> "flag")
            snap <- runTool session ghci "get_task_output"
                ("{\"task_id\":\"" <> taskId <> "\"}")
            snap `shouldSatisfy` Text.isInfixOf "liveout"
            _ <- runTool session ghci "kill_task"
                ("{\"task_id\":\"" <> taskId <> "\"}")
            pure ()

    it "drains a large stderr pipe without hanging the foreground command" do
        withTempSession \(session, ghci) -> do
            output <- runTool session ghci "run_terminal_cmd"
                "{\"command\":\"yes x | head -c 262144 >&2; echo drained\",\"timeout\":10000,\"description\":\"fill stderr\"}"
            output `shouldSatisfy` Text.isInfixOf "drained"
            output `shouldSatisfy` Text.isPrefixOf "exit: 0"

    it "deletes the env dump when the session closes" do
        withTempSession \(session, _ghci) -> do
            let envFile = grokSessionEnvFile session
            doesFileExist (toFilePath envFile) `shouldReturn` True
            closeGrokSession session
            doesFileExist (toFilePath envFile) `shouldReturn` False

    it "stops background commands when the session closes" do
        withTempSession \(session, ghci) -> do
            let escaped = toFilePath (sessionCwd session) </> "escaped"
            started <- runTool session ghci "run_terminal_cmd"
                "{\"command\":\"sleep 1; touch escaped\",\"description\":\"cleanup test\",\"background\":true}"
            started `shouldSatisfy` Text.isInfixOf "task_id:"
            closeGrokSession session
            threadDelay 1500000
            doesFileExist escaped `shouldReturn` False

    it "keeps closing after the initiating caller is cancelled" do
        withTempSession \(session, _ghci) -> do
            let envFile = grokSessionEnvFile session
                cwd = toFilePath (sessionCwd session)
                startedPath = cwd </> "foreground-started"
                releasePath = cwd </> "foreground-release"
                command =
                    "touch foreground-started; "
                        <> "while [ ! -e foreground-release ]; "
                        <> "do sleep 0.01; done"
            Async.withAsync
                (runForegroundStreaming session command 10000 (\_ _ -> pure ()))
                \foreground -> do
                    waitForFile startedPath
                    flip finally (Text.writeFile releasePath "") do
                        Async.withAsync (closeGrokSession session) \closing -> do
                            waitForClosed session
                            Async.cancel closing
                            Async.withAsync
                                (closeGrokSession session)
                                \joined -> do
                                    premature <-
                                        timeout 200000 (Async.wait joined)
                                    Text.writeFile releasePath ""
                                    result <- Async.wait foreground
                                    result.commandExitCode `shouldBe` Just 0
                                    Async.wait joined
                                    premature `shouldBe` Nothing
            doesFileExist (toFilePath envFile) `shouldReturn` False

    it "rejects a background start queued before close" do
        withTempSession \(session, _ghci) -> do
            let cwd = toFilePath (sessionCwd session)
                foregroundStarted = cwd </> "queue-foreground-started"
                foregroundRelease = cwd </> "queue-foreground-release"
                backgroundMarker = cwd </> "queued-background-ran"
                foregroundCommand =
                    "touch queue-foreground-started; "
                        <> "while [ ! -e queue-foreground-release ]; "
                        <> "do sleep 0.01; done"
            startCalled <- newEmptyMVar
            Async.withAsync
                (runForegroundStreaming
                    session foregroundCommand 10000 (\_ _ -> pure ()))
                \foreground -> do
                    waitForFile foregroundStarted
                    flip finally (Text.writeFile foregroundRelease "") do
                        Async.withAsync
                            (putMVar startCalled ()
                                >> startBackground
                                    session
                                    "touch queued-background-ran")
                            \starting -> do
                                takeMVar startCalled
                                Async.withAsync
                                    (closeGrokSession session)
                                    \closing -> do
                                        waitForClosed session
                                        Text.writeFile foregroundRelease ""
                                        foregroundResult <- Async.wait foreground
                                        foregroundResult.commandExitCode
                                            `shouldBe` Just 0
                                        backgroundResult <- Async.wait starting
                                        backgroundResult
                                            `shouldBe`
                                                Left "Grok session is closed."
                                        Async.wait closing
            threadDelay 100000
            doesFileExist backgroundMarker `shouldReturn` False

    it "keeps a task snapshot when kill is cancelled" do
        withTempSession \(session, ghci) -> do
            _ <- runTool session ghci "run_terminal_cmd"
                "{\"command\":\"export GROK_SESSION_VAR=before\",\"description\":\"seed env\"}"
            let cwd = toFilePath (sessionCwd session)
                shimDir = cwd </> "kill-shim-bin"
                shimPath = shimDir </> "bash"
                startedPath = cwd </> "kill-shim-started"
                signalPath = cwd </> "kill-shim-signal"
                releasePath = cwd </> "kill-shim-release"
                observedPath = cwd </> "kill-snapshot-env"
            bashPath <- requireExecutable "bash"
            createDirectoryIfMissing True shimDir
            writeFile shimPath $ unlines
                [ "#!/bin/sh"
                , "on_signal() { touch " <> shellQuote signalPath <> "; }"
                , "trap on_signal INT TERM"
                , "touch " <> shellQuote startedPath
                , "while [ ! -e " <> shellQuote releasePath
                    <> " ]; do sleep 0.01; done"
                , "exec " <> shellQuote bashPath <> " \"$@\""
                ]
            setFileMode shimPath 0o700

            withPathPrefix shimDir $
                flip finally (writeFile releasePath "") do
                    started <- startBackground session
                        "printf '%s' \"$GROK_SESSION_VAR\" > kill-snapshot-env"
                    let taskId = either (error . Text.unpack) taskIdFrom started
                    waitForFile startedPath
                    killing <- Async.async (killTask session taskId)
                    waitForFile signalPath
                    Async.cancel killing
                    writeFile releasePath ""
                    waitForFile observedPath
                    Text.readFile observedPath `shouldReturn` "before"
                    _ <- readTaskOutput session taskId (Just 5000)
                    pure ()

    it "rejects shell operations after close" do
        withTempSession \(session, _ghci) -> do
            let marker = toFilePath (sessionCwd session) </> "after-close"
            closeGrokSession session
            startBackground session "touch after-close"
                `shouldReturn` Left "Grok session is closed."
            foreground <- runForegroundStreaming
                session "touch after-close" 1000 (\_ _ -> pure ())
            foreground.commandExitCode `shouldBe` Just 1
            foreground.commandStderr `shouldBe` "Grok session is closed."
            readTaskOutput session "t1" Nothing
                `shouldReturn` "exit: 1\nGrok session is closed."
            killTask session "t1"
                `shouldReturn` "Grok session is closed."
            doesFileExist marker `shouldReturn` False

    describe "hasUnwaitedBackgroundOp" do
        it "detects a trailing bare ampersand" do
            hasUnwaitedBackgroundOp "sleep 1 &" `shouldBe` True
            hasUnwaitedBackgroundOp "sleep 1&" `shouldBe` True
        it "allows &&, 2>&1, quoted ampersands, and trailing wait" do
            hasUnwaitedBackgroundOp "true && echo x" `shouldBe` False
            hasUnwaitedBackgroundOp "cmd 2>&1" `shouldBe` False
            hasUnwaitedBackgroundOp "echo 'foo & bar'" `shouldBe` False
            hasUnwaitedBackgroundOp "sleep 1 & wait" `shouldBe` False
            hasUnwaitedBackgroundOp "echo hi" `shouldBe` False

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

runTool :: GrokSession -> GhciSession -> Text -> Text -> IO Text
runTool session ghci name arguments = do
    plan <- newPlanModeEnv (sessionCwd session) Nothing
    runToolWithPlan session ghci plan name arguments

runToolWithPlan :: GrokSession -> GhciSession -> PlanModeEnv -> Text -> Text -> IO Text
runToolWithPlan session ghci plan name arguments = do
    typesRef <- newIORef (Map.empty :: Map SubagentId GrokSubagentSpec)
    result <- dispatchToolCall defaultLoopDispatch
        (appToolHandlers (grokTools session ghci plan Nothing typesRef))
        (functionToolCall "call-1" name arguments)
    pure result.output

waitForFile :: FilePath -> IO ()
waitForFile path = go (20 :: Int)
  where
    go n
        | n <= 0 = expectationFailure ("timed out waiting for " <> path)
        | otherwise = doesFileExist path >>= \case
            True -> pure ()
            False -> threadDelay 50000 >> go (n - 1)

waitForClosed :: GrokSession -> IO ()
waitForClosed session = go (100 :: Int)
  where
    expected = "exit: 1\nGrok session is closed."
    go n
        | n <= 0 =
            expectationFailure "timed out waiting for Grok session to close"
        | otherwise = do
            output <- readTaskOutput session "__close_probe__" Nothing
            if output == expected
                then pure ()
                else threadDelay 10000 >> go (n - 1)

requireExecutable :: String -> IO FilePath
requireExecutable name =
    findExecutable name >>= \case
        Just path -> pure path
        Nothing -> do
            expectationFailure ("could not find executable: " <> name)
            pure name

taskIdFrom :: Text -> Text
taskIdFrom output =
    case [tid | line <- Text.lines output, Just tid <- [Text.stripPrefix "task_id: " line]] of
        (tid : _) -> tid
        [] -> error ("missing task_id in:\n" <> Text.unpack output)

withPathPrefix :: FilePath -> IO a -> IO a
withPathPrefix prefix action = do
    previous <- lookupEnv "PATH"
    bracket_
        (setEnv "PATH" (prefix <> maybe "" (":" <>) previous))
        (maybe (unsetEnv "PATH") (setEnv "PATH") previous)
        action

shellQuote :: FilePath -> String
shellQuote path = "'" <> concatMap escape path <> "'"
  where
    escape '\'' = "'\\''"
    escape c = [c]

sessionCwd :: GrokSession -> OsPath
sessionCwd session = (grokSessionEnv session).toolCwd

withTempSession :: ((GrokSession, GhciSession) -> IO a) -> IO a
withTempSession action = do
    tmp <- getTemporaryDirectory
    bracket
        (mkdtemp (tmp </> "agent-grok-XXXXXX"))
        (\dir -> removeDirectoryRecursive dir)
        \dir -> do
            env <- defaultToolEnv (fromFilePath dir)
            session <- newGrokSession env
            ghci <- newGhciSession env
            action (session, ghci) `finally` (closeGrokSession session >> closeGhciSession ghci)
