module Agent.Tools.CodexSpec (spec) where

import Agent.Loop (LoopError(..), defaultLoopDispatch)
import System.OsPath (decodeUtf, unsafeEncodeUtf)
import Agent.Subagents (closeSubagentRegistry, defaultSubagentConfig, newSubagentRegistry)
import Agent.Subagents.TaskPath (taskPathRoot)
import Agent.Tools.MultiAgents (MultiAgentContext(..))
import Agent.Provider (Provider(..))
import Agent.ToolDispatch
    ( ToolCallResult(..)
    , customToolCall
    , dispatchToolCall
    , functionToolCall
    )
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.Tools (CodingTools(..), appToolHandlers, codingToolsFor, defaultToolEnv)
import Agent.Tools.Types (jsonToolParameters, toolAllowsWithoutPrompt)
import Agent.Tools.ApplyPatch (applyPatch, parsePatch)
import Agent.Tools.Codex (codexTools)
import Agent.Tools.Codex.Shell
    ( closeCodexShellSession
    , newCodexShellSession
    )
import Agent.Tools.Ghci (closeGhciSession, newGhciSession)
import Agent.Tools.PlanMode (isPlanModeActive, newPlanModeEnv)
import Agent.Tools.Types (AppTool(..), ToolEnv(..))
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (bracket, finally)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory
    ( doesFileExist
    , getTemporaryDirectory
    , removeDirectoryRecursive
    )
import System.FilePath (takeFileName, (</>))
import System.Posix.Temp (mkdtemp)
import Test.Hspec

fromFilePath = unsafeEncodeUtf
toFilePath path = either (error . show) id (decodeUtf path)

spec :: Spec
spec = describe "Agent.Tools.Codex" do
    it "advertises Codex wire names and not Grok names" do
        withTempEnv \env -> do
            -- create a throwaway ghci for schema listing; codingToolsFor owns lifecycle
            coding <- codingToolsFor OpenAIProvider env Nothing Nothing
            let names = map (.appToolName) coding.codingAppTools
            names `shouldBe`
                [ "shell_command"
                , "write_stdin"
                , "apply_patch"
                , "update_plan"
                , "run_ghci"
                , "enter_plan_mode"
                , "ask_user_question"
                ]
            names `shouldNotContain` ["read_file"]
            names `shouldNotContain` ["run_terminal_cmd"]
            names `shouldNotContain` ["search_replace"]
            coding.codingClose

    it "registers multi-agent tools when a registry is provided" do
        withTempEnv \env -> do
            registry <- newSubagentRegistry defaultSubagentConfig env.toolCwd
                (\_ _ _ _ -> pure $ Left LoopNoResponseId)
                (\_ _ -> pure ())
            let ctx = MultiAgentContext
                    { multiRegistry = registry
                    , multiSelfId = Nothing
                    , multiDepth = 0
                    , multiTaskPath = taskPathRoot
                    , multiRootTurnId = pure Nothing
                    , multiResumeFromDisk = Nothing
                    , multiCreateWorktree = Nothing
                    , multiPrepareSpawn = Nothing
                    , multiSendToRoot = Nothing
                    }
            coding <- codingToolsFor OpenAIProvider env Nothing (Just ctx)
            let names = map (.appToolName) coding.codingAppTools
            names `shouldContain` ["spawn_agent", "wait_agent", "send_message", "followup_task", "list_agents", "interrupt_agent"]
            let parameters name =
                    [ property
                    | tool <- coding.codingAppTools
                    , tool.appToolName == name
                    , property <- fromMaybe [] (jsonToolParameters tool)
                    ]
            map (.propertyName) (parameters "spawn_agent") `shouldBe`
                [ "task_name"
                , "message"
                , "model"
                , "reasoning_effort"
                , "fork_turns"
                ]
            map (.propertyType) (parameters "wait_agent") `shouldBe`
                [PropertyNumber]
            case map (.propertyType) (parameters "spawn_agent") of
                _ : PropertyRaw (Aeson.Object messageSchema) : _ ->
                    KeyMap.lookup "encrypted" messageSchema
                        `shouldBe` Just (Aeson.Bool True)
                other -> expectationFailure
                    ("expected encrypted spawn message schema, got " <> show other)
            coding.codingClose
            closeSubagentRegistry registry

    it "lets the OpenAI agent enter plan mode proactively" do
        withTempEnv \env -> do
            ghci <- newGhciSession env
            shell <- newCodexShellSession env
            plan <- newPlanModeEnv env.toolCwd Nothing
            tools <- codexTools env shell ghci plan Nothing
            (do
                result <- dispatchToolCall defaultLoopDispatch (appToolHandlers tools)
                    (functionToolCall "call-enter-plan" "enter_plan_mode"
                        "{\"explanation\":\"The user requested a design plan.\"}")
                result.output `shouldSatisfy` Text.isInfixOf "entered plan mode"
                isPlanModeActive plan `shouldReturn` True)
                `finally` (closeGhciSession ghci >> closeCodexShellSession shell)

    it "adds, updates, and deletes files via apply_patch" do
        withTempEnv \env -> do
            added <- runPatch env $ Text.unlines
                [ "*** Begin Patch"
                , "*** Add File: hello.txt"
                , "+hello"
                , "+world"
                , "*** End Patch"
                ]
            added `shouldSatisfy` Text.isInfixOf "Success."
            added `shouldSatisfy` Text.isInfixOf "A hello.txt"
            Text.readFile (toFilePath env.toolCwd </> "hello.txt") `shouldReturn` "hello\nworld\n"

            updated <- runPatch env $ Text.unlines
                [ "*** Begin Patch"
                , "*** Update File: hello.txt"
                , "@@"
                , " hello"
                , "-world"
                , "+there"
                , "*** End Patch"
                ]
            updated `shouldSatisfy` Text.isInfixOf "M hello.txt"
            Text.readFile (toFilePath env.toolCwd </> "hello.txt") `shouldReturn` "hello\nthere\n"

            deleted <- runPatch env $ Text.unlines
                [ "*** Begin Patch"
                , "*** Delete File: hello.txt"
                , "*** End Patch"
                ]
            deleted `shouldSatisfy` Text.isInfixOf "D hello.txt"
            doesFileExist (toFilePath env.toolCwd </> "hello.txt") `shouldReturn` False

    it "rejects apply_patch paths that escape cwd" do
        withTempEnv \env -> do
            let name = takeFileName (toFilePath env.toolCwd) <> "-outside.txt"
            output <- runPatch env $ Text.unlines
                [ "*** Begin Patch"
                , "*** Add File: ../" <> Text.pack name
                , "+secret"
                , "*** End Patch"
                ]
            output `shouldSatisfy` Text.isInfixOf "escapes"

    it "rejects a patch whose context does not match the file" do
        withTempEnv \env -> do
            Text.writeFile (toFilePath env.toolCwd </> "a.txt") "foo\n"
            output <- runPatch env $ Text.unlines
                [ "*** Begin Patch"
                , "*** Update File: a.txt"
                , "@@"
                , "-missing"
                , "+present"
                , "*** End Patch"
                ]
            output `shouldSatisfy` Text.isInfixOf "Failed to find expected lines"

    it "preserves unchanged blank lines in update hunks" do
        withTempEnv \env -> do
            Text.writeFile (toFilePath env.toolCwd </> "blank.txt")
                "before\n\nold\nafter\n"
            output <- runPatch env $ Text.unlines
                [ "*** Begin Patch"
                , "*** Update File: blank.txt"
                , "@@"
                , " before"
                , " "
                , "-old"
                , "+new"
                , " after"
                , "*** End Patch"
                ]
            output `shouldSatisfy` Text.isInfixOf "M blank.txt"
            Text.readFile (toFilePath env.toolCwd </> "blank.txt")
                `shouldReturn` "before\n\nnew\nafter\n"

    it "rejects malformed update lines without recursing forever" do
        let parsed = parsePatch $ Text.unlines
                [ "*** Begin Patch"
                , "*** Update File: malformed.txt"
                , "@@"
                , " context"
                , ""
                , "-old"
                , "+new"
                , "*** End Patch"
                ]
        parsed `shouldSatisfy` \case
            Left err -> "Invalid update line" `Text.isInfixOf` err
            Right _ -> False
    it "preserves trailing whitespace in added lines" do
        withTempEnv \env -> do
            _ <- runPatch env $ Text.unlines
                [ "*** Begin Patch"
                , "*** Add File: spaces.txt"
                , "+hello  "
                , "*** End Patch"
                ]
            Text.readFile (toFilePath env.toolCwd </> "spaces.txt")
                `shouldReturn` "hello  \n"

    it "stops applying hunks after the first failure" do
        withTempEnv \env -> do
            let beginPatch = "*** Begin " <> "Patch"
                endPatch = "*** End " <> "Patch"
                patch = Text.unlines
                    [ beginPatch
                    , "*** Add File: before.txt"
                    , "+written"
                    , "*** Update File: missing.txt"
                    , "@@"
                    , "-old"
                    , "+new"
                    , "*** Add File: after.txt"
                    , "+not written"
                    , endPatch
                    ]
            result <- applyPatch env patch
            result `shouldSatisfy` \case
                Left err -> "Failed to read file" `Text.isInfixOf` err
                Right _ -> False
            Text.readFile (toFilePath env.toolCwd </> "before.txt")
                `shouldReturn` "written\n"
            doesFileExist (toFilePath env.toolCwd </> "after.txt")
                `shouldReturn` False

    it "parses add/update/delete hunks" do
        let parsed = parsePatch $ Text.unlines
                [ "*** Begin Patch"
                , "*** Add File: path/add.py"
                , "+abc"
                , "+def"
                , "*** Delete File: path/delete.py"
                , "*** Update File: path/update.py"
                , "*** Move to: path/update2.py"
                , "@@ def f():"
                , "-    pass"
                , "+    return 123"
                , "*** End Patch"
                ]
        parsed `shouldSatisfy` either (const False) ((== 3) . length)

    it "rejects rm -rf via shell_command even before execution" do
        withTempEnv \env -> do
            output <- runFn env "shell_command"
                "{\"command\":\"rm -rf /tmp/should-not-run\",\"workdir\":\".\"}"
            output `shouldSatisfy` Text.isInfixOf "Blocked dangerous shell command"
            output `shouldSatisfy` Text.isInfixOf "rm -rf"

    it "runs shell_command and times out" do
        withTempEnv \env -> do
            output <- runFn env "shell_command"
                "{\"command\":\"echo hi\",\"workdir\":\".\"}"
            output `shouldSatisfy` Text.isInfixOf "Exit code: 0"
            output `shouldSatisfy` Text.isInfixOf "hi"
            timed <- runFn env "shell_command"
                "{\"command\":\"sleep 5\",\"timeout_ms\":\"200\"}"
            timed `shouldSatisfy` Text.isInfixOf "timed out"

    it "rejects timeout_ms together with yield_time_ms" do
        withTempEnv \env -> do
            output <- runFn env "shell_command"
                "{\"command\":\"sleep 1\",\"timeout_ms\":100,\"yield_time_ms\":10}"
            output `shouldSatisfy`
                Text.isInfixOf "timeout_ms and yield_time_ms are mutually exclusive"

    it "only treats write_stdin polling as read-only" do
        withTempEnv \env ->
            withCodexTools env \tools -> do
                let tool =
                        fromMaybe (error "missing write_stdin") $
                            findTool "write_stdin" tools
                toolAllowsWithoutPrompt tool
                    (functionToolCall "poll" "write_stdin" "{\"session_id\":1}")
                    `shouldReturn` True
                toolAllowsWithoutPrompt tool
                    (functionToolCall "write" "write_stdin"
                        "{\"session_id\":1,\"chars\":\"hello\\n\"}")
                    `shouldReturn` False

    it "yields a long-running shell_command and polls it to completion" do
        withTempEnv \env ->
            withCodexTools env \tools -> do
                started <- runFnWith tools "shell_command"
                    "{\"command\":\"printf first; sleep 0.2; printf second\",\"workdir\":\".\",\"yield_time_ms\":20}"
                started `shouldSatisfy` Text.isInfixOf "Process still running"
                started `shouldSatisfy` Text.isInfixOf "first"
                let sessionId = sessionIdFrom started
                finished <- runFnWith tools "write_stdin" $
                    "{\"session_id\":" <> Text.pack (show sessionId)
                        <> ",\"yield_time_ms\":1000}"
                finished `shouldSatisfy` Text.isInfixOf "Exit code: 0"
                finished `shouldSatisfy` Text.isInfixOf "second"
                finished `shouldNotSatisfy` Text.isInfixOf "firstsecond"

    it "writes stdin to a managed shell_command" do
        withTempEnv \env ->
            withCodexTools env \tools -> do
                started <- runFnWith tools "shell_command"
                    "{\"command\":\"IFS= read -r line; printf 'got:%s' \\\"$line\\\"\",\"workdir\":\".\",\"yield_time_ms\":20}"
                let sessionId = sessionIdFrom started
                finished <- runFnWith tools "write_stdin" $
                    "{\"session_id\":" <> Text.pack (show sessionId)
                        <> ",\"chars\":\"hello\\n\",\"yield_time_ms\":1000}"
                finished `shouldSatisfy` Text.isInfixOf "Exit code: 0"
                finished `shouldSatisfy` Text.isInfixOf "got:hello"

    it "returns completed output when stdin is written after process exit" do
        withTempEnv \env ->
            withCodexTools env \tools -> do
                started <- runFnWith tools "shell_command"
                    "{\"command\":\"sleep 0.05; printf done\",\"workdir\":\".\",\"yield_time_ms\":10}"
                let sessionId = sessionIdFrom started
                threadDelay 150000
                finished <- runFnWith tools "write_stdin" $
                    "{\"session_id\":" <> Text.pack (show sessionId)
                        <> ",\"chars\":\"late\",\"yield_time_ms\":1000}"
                finished `shouldSatisfy` Text.isInfixOf "Exit code: 0"
                finished `shouldSatisfy` Text.isInfixOf "done"

    it "keeps completed sessions available until they are polled" do
        withTempEnv \env ->
            withCodexTools env \tools -> do
                first <- runFnWith tools "shell_command"
                    "{\"command\":\"sleep 0.05; printf first\",\"workdir\":\".\",\"yield_time_ms\":10}"
                let firstId = sessionIdFrom first
                threadDelay 150000
                _ <- runFnWith tools "shell_command"
                    "{\"command\":\"sleep 1\",\"workdir\":\".\",\"yield_time_ms\":10}"
                finished <- runFnWith tools "write_stdin" $
                    "{\"session_id\":" <> Text.pack (show firstId)
                        <> ",\"yield_time_ms\":10}"
                finished `shouldSatisfy` Text.isInfixOf "Exit code: 0"
                finished `shouldSatisfy` Text.isInfixOf "first"

    it "returns new output even after the capture cap has rolled forward" do
        withTempEnv \base -> do
            let env = base { toolStdoutCap = 64 }
            withCodexTools env \tools -> do
                started <- runFnWith tools "shell_command"
                    "{\"command\":\"yes x | head -c 4096; sleep 0.1; printf LATE; sleep 0.5\",\"workdir\":\".\",\"yield_time_ms\":50}"
                let sessionId = sessionIdFrom started
                threadDelay 200000
                polled <- runFnWith tools "write_stdin" $
                    "{\"session_id\":" <> Text.pack (show sessionId)
                        <> ",\"yield_time_ms\":1}"
                polled `shouldSatisfy` Text.isInfixOf "LATE"

    it "rejects a command before spawning when the session is full" do
        withTempEnv \env ->
            withCodexTools env \tools -> do
                mapM_
                    (\_ -> do
                        started <- runFnWith tools "shell_command"
                            "{\"command\":\"sleep 5\",\"workdir\":\".\",\"yield_time_ms\":1}"
                        started `shouldSatisfy` Text.isInfixOf "session_id:")
                    [1 :: Int .. 64]
                let marker = toFilePath env.toolCwd </> "over-cap"
                rejected <- runFnWith tools "shell_command" $
                    "{\"command\":\"printf bad > "
                        <> Text.pack marker
                        <> "\",\"workdir\":\".\",\"yield_time_ms\":1}"
                rejected `shouldSatisfy` Text.isInfixOf "session is full"
                threadDelay 100000
                doesFileExist marker `shouldReturn` False

    it "rejects unknown managed shell sessions" do
        withTempEnv \env ->
            withCodexTools env \tools -> do
                output <- runFnWith tools "write_stdin"
                    "{\"session_id\":999,\"yield_time_ms\":1}"
                output `shouldSatisfy` Text.isInfixOf "Unknown session_id: 999"

    it "stops managed shell commands when coding tools close" do
        withTempEnv \env -> do
            let marker = toFilePath env.toolCwd </> "escaped"
            coding <- codingToolsFor OpenAIProvider env Nothing Nothing
            started <- runFnWith coding.codingAppTools "shell_command" $
                "{\"command\":\"sleep 0.3; printf done > "
                    <> Text.pack marker
                    <> "\",\"workdir\":\".\",\"yield_time_ms\":20}"
            started `shouldSatisfy` Text.isInfixOf "session_id:"
            coding.codingClose
            threadDelay 500000
            doesFileExist marker `shouldReturn` False

    it "stores an update_plan and rejects two in_progress steps" do
        withTempEnv \env -> do
            ok <- runFn env "update_plan"
                "{\"plan\":[{\"step\":\"one\",\"status\":\"in_progress\"},{\"step\":\"two\",\"status\":\"pending\"}]}"
            ok `shouldSatisfy` Text.isInfixOf "[in_progress] one"
            ok `shouldSatisfy` Text.isInfixOf "[pending] two"
            bad <- runFn env "update_plan"
                "{\"plan\":[{\"step\":\"a\",\"status\":\"in_progress\"},{\"step\":\"b\",\"status\":\"in_progress\"}]}"
            bad `shouldSatisfy` Text.isInfixOf "At most one step"

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

runPatch :: ToolEnv -> Text -> IO Text
runPatch env patch = withCodexTools env \tools -> do
    result <- dispatchToolCall defaultLoopDispatch (appToolHandlers tools)
        (customToolCall "call-1" "apply_patch" patch)
    pure result.output

runFn :: ToolEnv -> Text -> Text -> IO Text
runFn env name arguments = withCodexTools env \tools -> do
    runFnWith tools name arguments

runFnWith :: [AppTool] -> Text -> Text -> IO Text
runFnWith tools name arguments = do
    result <- dispatchToolCall defaultLoopDispatch (appToolHandlers tools)
        (functionToolCall "call-1" name arguments)
    pure result.output

withCodexTools :: ToolEnv -> ([AppTool] -> IO a) -> IO a
withCodexTools env action = do
    shell <- newCodexShellSession env
    ghci <- newGhciSession env
    plan <- newPlanModeEnv env.toolCwd Nothing
    tools <- codexTools env shell ghci plan Nothing
    action tools
        `finally` (closeGhciSession ghci >> closeCodexShellSession shell)

sessionIdFrom :: Text -> Int
sessionIdFrom output =
    case
        [ Text.strip rest
        | line <- Text.lines output
        , Just rest <- [Text.stripPrefix "session_id:" line]
        ] of
        value : _ -> case reads (Text.unpack value) of
            [(sessionId, "")] -> sessionId
            _ -> error ("invalid session_id: " <> Text.unpack value)
        [] -> error ("missing session_id in: " <> Text.unpack output)

findTool :: Text -> [AppTool] -> Maybe AppTool
findTool name = go
  where
    go :: [AppTool] -> Maybe AppTool
    go [] = Nothing
    go (tool : tools)
        | tool.appToolName == name = Just tool
        | otherwise = go tools

withTempEnv :: (ToolEnv -> IO a) -> IO a
withTempEnv action = do
    tmp <- getTemporaryDirectory
    bracket
        (mkdtemp (tmp </> "agent-codex-XXXXXX"))
        removeDirectoryRecursive
        (\dir -> defaultToolEnv (fromFilePath dir) >>= action)
