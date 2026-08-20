module Agent.Tools.CodexSpec (spec) where

import Agent.Loop (LoopError(..), defaultLoopDispatch)
import Agent.Subagents (closeSubagentRegistry, defaultSubagentConfig, newSubagentRegistry)
import Agent.Tools.MultiAgents (MultiAgentContext(..))
import Agent.Provider (Provider(..))
import Agent.ToolDispatch
    ( ToolCallResult(..)
    , customToolCall
    , dispatchToolCall
    , functionToolCall
    )
import Agent.Tools (CodingTools(..), appToolHandlers, codingToolsFor, defaultToolEnv)
import Agent.Tools.ApplyPatch (parsePatch)
import Agent.Tools.Codex (codexTools)
import Agent.Tools.Ghci (closeGhciSession, newGhciSession)
import Agent.Tools.PlanMode (newPlanModeEnv)
import Agent.Tools.Types (AppTool(..), ToolEnv(..))
import Control.Exception.Safe (bracket, finally)
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

spec :: Spec
spec = describe "Agent.Tools.Codex" do
    it "advertises Codex wire names and not Grok names" do
        withTempEnv \env -> do
            -- create a throwaway ghci for schema listing; codingToolsFor owns lifecycle
            coding <- codingToolsFor OpenAIProvider env Nothing Nothing
            let names = map (.appToolName) coding.codingAppTools
            names `shouldBe` ["shell_command", "apply_patch", "update_plan", "run_ghci"]
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
                    }
            coding <- codingToolsFor OpenAIProvider env Nothing (Just ctx)
            let names = map (.appToolName) coding.codingAppTools
            names `shouldContain` ["spawn_agent", "wait_agent", "send_input", "close_agent", "resume_agent"]
            coding.codingClose
            closeSubagentRegistry registry

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
            Text.readFile (env.toolCwd </> "hello.txt") `shouldReturn` "hello\nworld\n"

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
            Text.readFile (env.toolCwd </> "hello.txt") `shouldReturn` "hello\nthere\n"

            deleted <- runPatch env $ Text.unlines
                [ "*** Begin Patch"
                , "*** Delete File: hello.txt"
                , "*** End Patch"
                ]
            deleted `shouldSatisfy` Text.isInfixOf "D hello.txt"
            doesFileExist (env.toolCwd </> "hello.txt") `shouldReturn` False

    it "rejects apply_patch paths that escape cwd" do
        withTempEnv \env -> do
            let name = takeFileName env.toolCwd <> "-outside.txt"
            output <- runPatch env $ Text.unlines
                [ "*** Begin Patch"
                , "*** Add File: ../" <> Text.pack name
                , "+secret"
                , "*** End Patch"
                ]
            output `shouldSatisfy` Text.isInfixOf "escapes"

    it "rejects a patch whose context does not match the file" do
        withTempEnv \env -> do
            Text.writeFile (env.toolCwd </> "a.txt") "foo\n"
            output <- runPatch env $ Text.unlines
                [ "*** Begin Patch"
                , "*** Update File: a.txt"
                , "@@"
                , "-missing"
                , "+present"
                , "*** End Patch"
                ]
            output `shouldSatisfy` Text.isInfixOf "Failed to find expected lines"

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
                "{\"command\":\"sleep 5\",\"timeout_ms\":200}"
            timed `shouldSatisfy` Text.isInfixOf "timed out"

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
    result <- dispatchToolCall defaultLoopDispatch (appToolHandlers tools)
        (functionToolCall "call-1" name arguments)
    pure result.output

withCodexTools :: ToolEnv -> ([AppTool] -> IO a) -> IO a
withCodexTools env action = do
    ghci <- newGhciSession env
    plan <- newPlanModeEnv env.toolCwd Nothing
    tools <- codexTools env ghci plan Nothing
    action tools `finally` closeGhciSession ghci

withTempEnv :: (ToolEnv -> IO a) -> IO a
withTempEnv action = do
    tmp <- getTemporaryDirectory
    bracket
        (mkdtemp (tmp </> "agent-codex-XXXXXX"))
        removeDirectoryRecursive
        (\dir -> defaultToolEnv dir >>= action)
