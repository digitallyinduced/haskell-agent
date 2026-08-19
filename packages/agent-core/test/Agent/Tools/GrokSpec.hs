module Agent.Tools.GrokSpec (spec) where

import Agent.Loop (defaultLoopDispatch)
import Agent.Provider (Provider(..))
import Agent.ToolDispatch (ToolCallResult(..), dispatchToolCall, functionToolCall)
import Agent.Tools (appToolHandlers, codingToolsFor, defaultToolEnv)
import Agent.Tools.Grok (grokTools)
import Agent.Tools.Types (AppTool(..), ToolEnv(..))
import Control.Exception (bracket)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory
    ( createDirectoryIfMissing
    , doesFileExist
    , getTemporaryDirectory
    )
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.Posix.Temp (mkdtemp)
import System.Directory (removeDirectoryRecursive)
import Test.Hspec

spec :: Spec
spec = describe "Agent.Tools.Grok" do
    it "advertises grok-build wire names and not Codex names" do
        withTempEnv \env -> do
            let names = map (.appToolName) (grokTools env)
            names `shouldBe`
                [ "read_file"
                , "grep"
                , "list_dir"
                , "search_replace"
                , "run_terminal_cmd"
                ]
            names `shouldNotContain` ["apply_patch"]
            names `shouldNotContain` ["shell_command"]
            xai <- codingToolsFor XAIProvider env
            map (.appToolName) xai `shouldBe` names

    it "reads a file with grok line-number anchors" do
        withTempEnv \env -> do
            let path = env.toolCwd </> "sample.txt"
            Text.writeFile path (Text.unlines ["alpha", "bravo", "charlie"])
            output <- runTool env "read_file" "{\"target_file\":\"sample.txt\"}"
            output `shouldSatisfy` Text.isPrefixOf "1\8594alpha"
            output `shouldSatisfy` Text.isInfixOf "bravo"

    it "rejects a path that escapes cwd" do
        withTempEnv \env -> do
            let name = takeFileName env.toolCwd <> "-outside.txt"
                outsider = takeDirectory env.toolCwd </> name
            Text.writeFile outsider "secret"
            relative <- runTool env "read_file"
                ("{\"target_file\":\"../" <> Text.pack name <> "\"}")
            relative `shouldSatisfy` Text.isInfixOf "escapes"
            absolute <- runTool env "read_file" "{\"target_file\":\"/etc/passwd\"}"
            absolute `shouldSatisfy` Text.isInfixOf "escapes"

    it "lists directory entries with a trailing slash for folders" do
        withTempEnv \env -> do
            createDirectoryIfMissing True (env.toolCwd </> "sub")
            Text.writeFile (env.toolCwd </> "a.txt") "x"
            output <- runTool env "list_dir" "{\"target_directory\":\".\"}"
            output `shouldSatisfy` Text.isInfixOf "a.txt"
            output `shouldSatisfy` Text.isInfixOf "sub/"

    it "creates a file with empty old_string and replaces a unique match" do
        withTempEnv \env -> do
            created <- runTool env "search_replace"
                "{\"file_path\":\"new.txt\",\"old_string\":\"\",\"new_string\":\"hello world\\n\"}"
            created `shouldSatisfy` Text.isInfixOf "created successfully"
            doesFileExist (env.toolCwd </> "new.txt") `shouldReturn` True

            updated <- runTool env "search_replace"
                "{\"file_path\":\"new.txt\",\"old_string\":\"hello\",\"new_string\":\"goodbye\"}"
            updated `shouldSatisfy` Text.isInfixOf "updated successfully"
            Text.readFile (env.toolCwd </> "new.txt") `shouldReturn` "goodbye world\n"

    it "refuses a non-unique search_replace without replace_all" do
        withTempEnv \env -> do
            Text.writeFile (env.toolCwd </> "dup.txt") "aaa bbb aaa\n"
            output <- runTool env "search_replace"
                "{\"file_path\":\"dup.txt\",\"old_string\":\"aaa\",\"new_string\":\"ccc\"}"
            output `shouldSatisfy` Text.isInfixOf "multiple times"

    it "grep finds a literal match" do
        withTempEnv \env -> do
            Text.writeFile (env.toolCwd </> "hit.txt") "needle in haystack\n"
            output <- runTool env "grep" "{\"pattern\":\"needle\"}"
            output `shouldSatisfy` Text.isInfixOf "needle"

    it "runs a foreground shell command" do
        withTempEnv \env -> do
            output <- runTool env "run_terminal_cmd"
                "{\"command\":\"echo hi\",\"description\":\"print hi\"}"
            output `shouldSatisfy` Text.isInfixOf "Exit code: 0"
            output `shouldSatisfy` Text.isInfixOf "hi"

    it "times out a long-running shell command" do
        withTempEnv \env -> do
            output <- runTool env "run_terminal_cmd"
                "{\"command\":\"sleep 5\",\"timeout\":200,\"description\":\"timeout test\"}"
            output `shouldSatisfy` Text.isInfixOf "timed out"

    it "rejects background run_terminal_cmd in v1" do
        withTempEnv \env -> do
            output <- runTool env "run_terminal_cmd"
                "{\"command\":\"echo hi\",\"description\":\"bg\",\"background\":true}"
            output `shouldSatisfy` Text.isInfixOf "Background execution is not available"

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

runTool :: ToolEnv -> Text -> Text -> IO Text
runTool env name arguments = do
    result <- dispatchToolCall defaultLoopDispatch
        (appToolHandlers (grokTools env))
        (functionToolCall "call-1" name arguments)
    pure result.output

withTempEnv :: (ToolEnv -> IO a) -> IO a
withTempEnv action = do
    tmp <- getTemporaryDirectory
    bracket
        (mkdtemp (tmp </> "agent-grok-XXXXXX"))
        (\dir -> removeDirectoryRecursive dir)
        (\dir -> action (defaultToolEnv dir))
