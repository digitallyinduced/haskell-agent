module Agent.CLI.DialectsSpec (spec) where

import Agent.CLI.Dialects
    ( CodingTools(..)
    , classifyCodingTool
    , codingToolsFor
    , filterBashTools
    , filterGhciTools
    , formatAgentsMdForDialect
    , globalAgentsHomeDir
    )
import Agent.Dialect
    ( claudeCodeDialect
    , codexDialect
    , genericResponsesDialect
    , grokBuildDialect
    )
import Agent.ProjectInstructions (InstructionFile(..), LoadedAgentsMd(..))
import Agent.ToolDispatch (noArgsTool)
import Agent.Tools.Secret (SecretPromptHooks(..))
import Agent.Tools.ShowImage (ImageDisplayHooks(..))
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRule(AlwaysReadOnly)
    , ToolPlacement(..)
    , ToolEnv
    , defaultToolEnv
    , jsonAppTool
    )
import Control.Exception.Safe (bracket, finally)
import Control.Monad (forM_)
import qualified Data.Text as Text
import System.Directory (getTemporaryDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.OsPath (unsafeEncodeUtf)
import System.Posix.Temp (mkdtemp)
import Test.Hspec

spec :: Spec
spec = describe "Agent.CLI.Dialects" do
    it "dispatches project-instruction formatting by dialect" do
        let cwd = unsafeEncodeUtf "/repo"
            loaded = LoadedAgentsMd
                { loadedGlobal = Nothing
                , loadedProject =
                    [InstructionFile (unsafeEncodeUtf "/repo/AGENTS.md") "rules"]
                , loadedWarnings = []
                }
        formatAgentsMdForDialect codexDialect cwd loaded
            `shouldSatisfy`
                maybe False (Text.isPrefixOf "# AGENTS.md instructions")
        formatAgentsMdForDialect grokBuildDialect cwd loaded
            `shouldSatisfy`
                maybe False (Text.isInfixOf "<system-reminder>")
        formatAgentsMdForDialect genericResponsesDialect cwd loaded
            `shouldSatisfy`
                maybe False (Text.isInfixOf "<system-reminder>")
        formatAgentsMdForDialect claudeCodeDialect cwd loaded
            `shouldSatisfy`
                maybe False (Text.isPrefixOf "# AGENTS.md instructions")

    it "uses each dialect's compatibility instruction home" do
        let home = unsafeEncodeUtf "/home/u"
        globalAgentsHomeDir codexDialect home
            `shouldBe` unsafeEncodeUtf "/home/u/.codex"
        globalAgentsHomeDir grokBuildDialect home
            `shouldBe` unsafeEncodeUtf "/home/u/.grok"
        globalAgentsHomeDir genericResponsesDialect home
            `shouldBe` unsafeEncodeUtf "/home/u/.haskell-agent"
        globalAgentsHomeDir claudeCodeDialect home
            `shouldBe` unsafeEncodeUtf "/home/u/.claude"

    it "allocates only the AskUserQuestion MCP fallback for Claude Code" do
        env <- defaultToolEnv (unsafeEncodeUtf "/tmp")
        coding <- codingToolsFor claudeCodeDialect env Nothing Nothing Nothing Nothing
        map (.appToolName) coding.codingAppTools
            `shouldBe` ["ask_user_question"]
        coding.codingClose

    it "registers ask_secret only when root prompt hooks are supplied" do
        withTempToolEnv \env ->
            forM_ [codexDialect, grokBuildDialect] \dialect -> do
                withoutSecret <-
                    codingToolsFor dialect env Nothing Nothing Nothing Nothing
                map (.appToolName) withoutSecret.codingAppTools
                    `shouldNotContain` ["ask_secret"]
                withoutSecret.codingClose

                let hooks = SecretPromptHooks
                        (const (pure (Right Nothing)))
                withSecret <-
                    codingToolsFor dialect env Nothing (Just hooks) Nothing Nothing
                (map (.appToolName) withSecret.codingAppTools
                    `shouldContain` ["ask_secret"])
                    `finally` withSecret.codingClose

    it "registers show_image only when image display hooks are supplied" do
        withTempToolEnv \env ->
            forM_ [codexDialect, grokBuildDialect] \dialect -> do
                withoutImages <-
                    codingToolsFor dialect env Nothing Nothing Nothing Nothing
                map (.appToolName) withoutImages.codingAppTools
                    `shouldNotContain` ["show_image"]
                withoutImages.codingClose

                let hooks = ImageDisplayHooks (const (pure (Right ())))
                withImages <-
                    codingToolsFor dialect env Nothing Nothing (Just hooks) Nothing
                (map (.appToolName) withImages.codingAppTools
                    `shouldContain` ["show_image"])
                    `finally` withImages.codingClose

    it "registers bounded artifact readers on coding tool surfaces" do
        withTempToolEnv \env ->
            forM_ [codexDialect, grokBuildDialect] \dialect -> do
                coding <- codingToolsFor dialect env Nothing Nothing Nothing Nothing
                let names = map (.appToolName) coding.codingAppTools
                names `shouldContain`
                    ["read_tool_output", "search_tool_output"]
                names `shouldNotContain` ["analyze_tool_output"]
                coding.codingClose

    it "keeps model-controlled coding tools on the sandbox side" do
        let placement name =
                (classifyCodingTool (fakeTool name)).appToolPlacement
        placement "update_plan" `shouldBe` HostTool
        placement "read_tool_output" `shouldBe` SandboxTool
        placement "search_tool_output" `shouldBe` SandboxTool
        placement "analyze_tool_output" `shouldBe` SandboxTool
        placement "shell_command" `shouldBe` SandboxTool
        placement "read_file" `shouldBe` SandboxTool
        placement "apply_patch" `shouldBe` SandboxTool
        placement "new_unreviewed_tool" `shouldBe` SandboxTool

    it "filters shell and ghci tools independently" do
        let tools = map fakeTool
                [ "run_ghci"
                , "read_file"
                , "shell_command"
                , "write_stdin"
                , "run_terminal_cmd"
                ]
            names = map (.appToolName)
        names (filterBashTools False tools)
            `shouldBe` ["run_ghci", "read_file"]
        names (filterGhciTools False tools)
            `shouldBe`
                [ "read_file"
                , "shell_command"
                , "write_stdin"
                , "run_terminal_cmd"
                ]

withTempToolEnv :: (ToolEnv -> IO a) -> IO a
withTempToolEnv action = do
    root <- getTemporaryDirectory
    bracket
        (mkdtemp (root </> "agent-cli-dialects-"))
        removeDirectoryRecursive
        (\directory ->
            defaultToolEnv (unsafeEncodeUtf directory) >>= action)

fakeTool :: Text.Text -> AppTool
fakeTool name =
    jsonAppTool name "" [] AlwaysReadOnly
        (noArgsTool name (pure (Right "")))
