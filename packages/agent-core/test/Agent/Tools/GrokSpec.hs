module Agent.Tools.GrokSpec (spec) where

import Agent.Dialect (genericResponsesDialect, grokBuildDialect)
import Agent.Loop (LoopError(..), defaultLoopDispatch)
import System.OsPath (decodeUtf, unsafeEncodeUtf)
import Agent.Subagents (SubagentId, closeSubagentRegistry, defaultSubagentConfig, newSubagentRegistry)
import Agent.ToolDispatch (ToolCallResult(..), dispatchToolCall, functionToolCall)
import Agent.ToolDSL (PropertySchema(..))
import Agent.Tools (CodingTools(..), appToolHandlers, codingToolsFor, defaultToolEnv)
import Agent.Tools.Types (jsonToolParameters)
import Agent.Tools.Grok (closeGrokSession, grokTools, newGrokSession)
import Agent.Tools.Grok.Task (GrokSubagentSpec)
import Agent.Tools.Ghci (GhciSession, closeGhciSession, newGhciSession)
import Agent.Tools.Grok.Shell (GrokSession(..), PersistentShell(..), hasUnwaitedBackgroundOp)
import Agent.Subagents.TaskPath (taskPathRoot)
import Agent.Tools.MultiAgents (MultiAgentContext(..))
import Agent.Tools.PlanMode (PlanModeEnv, activatePlanMode, newPlanModeEnv)
import Agent.Tools.Types (AppTool(..), ToolEnv(..))
import Control.Concurrent (threadDelay)
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Control.Concurrent.MVar (readMVar)
import Control.Exception (bracket, finally)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory
    ( createDirectoryIfMissing
    , createDirectoryLink
    , doesFileExist
    , getTemporaryDirectory
    )
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.Posix.Temp (mkdtemp)
import System.Directory (removeDirectoryRecursive)
import Test.Hspec

fromFilePath = unsafeEncodeUtf
toFilePath path = either (error . show) id (decodeUtf path)

spec :: Spec
spec = describe "Agent.Tools.Grok" do
    it "advertises grok-build wire names and not Codex names" do
        withTempSession \(session, ghci) -> do
            plan <- newPlanModeEnv session.grokEnv.toolCwd Nothing
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
            grok <- codingToolsFor grokBuildDialect session.grokEnv Nothing Nothing
            generic <-
                codingToolsFor
                    genericResponsesDialect session.grokEnv Nothing Nothing
            (do
                map (.appToolName) grok.codingAppTools `shouldBe` names
                map (.appToolName) generic.codingAppTools `shouldBe` names)
                `finally` (grok.codingClose >> generic.codingClose)


    it "registers task when a multi-agent context is provided" do
        withTempSession \(session, ghci) -> do
            plan <- newPlanModeEnv session.grokEnv.toolCwd Nothing
            registry <- newSubagentRegistry defaultSubagentConfig session.grokEnv.toolCwd
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
            let path = toFilePath session.grokEnv.toolCwd </> "sample.txt"
            Text.writeFile path (Text.unlines ["alpha", "bravo", "charlie"])
            output <- runTool session ghci "read_file" "{\"target_file\":\"sample.txt\"}"
            output `shouldSatisfy` Text.isPrefixOf "1\8594alpha"
            output `shouldSatisfy` Text.isInfixOf "bravo"

    it "rejects a path that escapes cwd" do
        withTempSession \(session, ghci) -> do
            let cwd = toFilePath session.grokEnv.toolCwd
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
            createDirectoryIfMissing True (toFilePath session.grokEnv.toolCwd </> "sub")
            Text.writeFile (toFilePath session.grokEnv.toolCwd </> "a.txt") "x"
            output <- runTool session ghci "list_dir" "{\"target_directory\":\".\"}"
            output `shouldSatisfy` Text.isInfixOf "a.txt"
            output `shouldSatisfy` Text.isInfixOf "sub/"

    it "does not descend through directory symlinks" do
        withTempSession \(session, ghci) -> do
            let cwd = toFilePath session.grokEnv.toolCwd
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
            doesFileExist (toFilePath session.grokEnv.toolCwd </> "new.txt") `shouldReturn` True

            updated <- runTool session ghci "search_replace"
                "{\"file_path\":\"new.txt\",\"old_string\":\"hello\",\"new_string\":\"goodbye\"}"
            updated `shouldSatisfy` Text.isInfixOf "updated successfully"
            Text.readFile (toFilePath session.grokEnv.toolCwd </> "new.txt") `shouldReturn` "goodbye world\n"

    it "refuses a non-unique search_replace without replace_all" do
        withTempSession \(session, ghci) -> do
            let path = toFilePath session.grokEnv.toolCwd </> "dup.txt"
            Text.writeFile path "aaa bbb aaa\n"
            output <- runTool session ghci "search_replace"
                "{\"file_path\":\"dup.txt\",\"old_string\":\"aaa\",\"new_string\":\"ccc\"}"
            output `shouldSatisfy` Text.isInfixOf "multiple times"
            Text.readFile path `shouldReturn` "aaa bbb aaa\n"

    it "grep finds a literal match" do
        withTempSession \(session, ghci) -> do
            Text.writeFile (toFilePath session.grokEnv.toolCwd </> "hit.txt") "needle in haystack\n"
            output <- runTool session ghci "grep" "{\"pattern\":\"needle\"}"
            output `shouldSatisfy` Text.isInfixOf "needle"

    it "grep accepts glob before the path terminator" do
        withTempSession \(session, ghci) -> do
            Text.writeFile (toFilePath session.grokEnv.toolCwd </> "hit.txt") "needle in haystack\n"
            Text.writeFile (toFilePath session.grokEnv.toolCwd </> "hit.md") "needle in markdown\n"
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
            Text.writeFile (toFilePath session.grokEnv.toolCwd </> "lines.txt") (Text.unlines ["a", "b", "c", "d"])
            output <- runTool session ghci "read_file" "{\"target_file\":\"lines.txt\",\"offset\":-2}"
            output `shouldNotSatisfy` Text.isInfixOf "beyond the end"
            output `shouldSatisfy` Text.isInfixOf "d"

    it "wraps grep output in a workspace_result card" do
        withTempSession \(session, ghci) -> do
            Text.writeFile (toFilePath session.grokEnv.toolCwd </> "hit.txt") "needle in haystack\n"
            output <- runTool session ghci "grep" "{\"pattern\":\"needle\"}"
            output `shouldSatisfy` Text.isInfixOf "<workspace_result"
            output `shouldSatisfy` Text.isInfixOf "needle"

    it "hints the nearest line when search_replace misses" do
        withTempSession \(session, ghci) -> do
            Text.writeFile (toFilePath session.grokEnv.toolCwd </> "near.txt") "alpha\nbravo unique\ncharlie\n"
            output <- runTool session ghci "search_replace"
                "{\"file_path\":\"near.txt\",\"old_string\":\"xyz unique\",\"new_string\":\"x\"}"
            output `shouldSatisfy` Text.isInfixOf "Nearest match: line 2"

    it "refuses to edit a gitignored file" do
        withTempSession \(session, ghci) -> do
            Text.writeFile (toFilePath session.grokEnv.toolCwd </> ".gitignore") "secret.txt\n"
            Text.writeFile (toFilePath session.grokEnv.toolCwd </> "secret.txt") "hidden\n"
            initOut <- runTool session ghci "run_terminal_cmd"
                "{\"command\":\"git init\",\"description\":\"init git\"}"
            initOut `shouldSatisfy` Text.isPrefixOf "exit: 0"
            output <- runTool session ghci "search_replace"
                "{\"file_path\":\"secret.txt\",\"old_string\":\"hidden\",\"new_string\":\"shown\"}"
            output `shouldSatisfy` Text.isInfixOf "gitignore"

    it "only edits plan.md while plan mode is active" do
        withTempSession \(session, ghci) -> do
            plan <- newPlanModeEnv session.grokEnv.toolCwd Nothing
            activatePlanMode plan
            blocked <- runToolWithPlan session ghci plan "search_replace"
                "{\"file_path\":\"blocked.txt\",\"old_string\":\"\",\"new_string\":\"no\"}"
            blocked `shouldSatisfy` Text.isInfixOf "only editable file is the plan file"
            doesFileExist (toFilePath session.grokEnv.toolCwd </> "blocked.txt")
                `shouldReturn` False
            allowed <- runToolWithPlan session ghci plan "search_replace"
                "{\"file_path\":\"plan.md\",\"old_string\":\"\",\"new_string\":\"# Plan\\n\"}"
            allowed `shouldSatisfy` Text.isInfixOf "created successfully"
            Text.readFile (toFilePath session.grokEnv.toolCwd </> "plan.md")
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
            waitForFile (toFilePath session.grokEnv.toolCwd </> "flag")
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
            shell <- readMVar session.grokShell
            doesFileExist (toFilePath shell.shellEnvFile) `shouldReturn` True
            closeGrokSession session
            doesFileExist (toFilePath shell.shellEnvFile) `shouldReturn` False

    it "stops background commands when the session closes" do
        withTempSession \(session, ghci) -> do
            let escaped = toFilePath session.grokEnv.toolCwd </> "escaped"
            started <- runTool session ghci "run_terminal_cmd"
                "{\"command\":\"sleep 1; touch escaped\",\"description\":\"cleanup test\",\"background\":true}"
            started `shouldSatisfy` Text.isInfixOf "task_id:"
            closeGrokSession session
            threadDelay 1500000
            doesFileExist escaped `shouldReturn` False

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
    plan <- newPlanModeEnv session.grokEnv.toolCwd Nothing
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

taskIdFrom :: Text -> Text
taskIdFrom output =
    case [tid | line <- Text.lines output, Just tid <- [Text.stripPrefix "task_id: " line]] of
        (tid : _) -> tid
        [] -> error ("missing task_id in:\n" <> Text.unpack output)

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
