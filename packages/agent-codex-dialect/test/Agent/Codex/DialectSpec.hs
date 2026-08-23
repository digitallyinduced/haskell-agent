module Agent.Codex.DialectSpec (spec) where

import Agent.Codex.Dialect.ProjectInstructions (formatCodexAgentsMd)
import Agent.Codex.Dialect.Prompt (codexSystemPrompt)
import Agent.Codex.Dialect.Runtime
    ( CodexCodingTools(..)
    , newCodexCodingTools
    )
import Agent.ProjectInstructions (InstructionFile(..), LoadedAgentsMd(..))
import Agent.Tools.Types (AppTool(..), defaultToolEnv)
import Control.Exception.Safe (bracket)
import qualified Data.Text as Text
import Data.Time.Calendar (fromGregorian)
import System.Directory (getTemporaryDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.OsPath (unsafeEncodeUtf)
import System.Posix.Temp (mkdtemp)
import Test.Hspec

spec :: Spec
spec = describe "Codex dialect" do
    it "renders the Codex tool contract" do
        let prompt =
                codexSystemPrompt
                    (unsafeEncodeUtf "/repo")
                    (fromGregorian 2026 8 23)
        prompt `shouldSatisfy` Text.isInfixOf "shell_command"
        prompt `shouldSatisfy` Text.isInfixOf "apply_patch"
        prompt `shouldSatisfy` Text.isInfixOf "<proposed_plan>"

    it "constructs only the Codex tool surface" do
        withTempDir \dir -> do
            env <- defaultToolEnv (unsafeEncodeUtf dir)
            coding <- newCodexCodingTools env Nothing Nothing
            let names = map (.appToolName) coding.codexAppTools
            map (`elem` names)
                [ "shell_command"
                , "apply_patch"
                , "update_plan"
                , "write_plan"
                ]
                `shouldBe` replicate 4 True
            names `shouldNotContain` ["run_terminal_cmd", "search_replace"]
            coding.codexClose

    it "formats project instructions as a contextual user fragment" do
        let loaded = LoadedAgentsMd
                { loadedGlobal = Just
                    (InstructionFile
                        (unsafeEncodeUtf "/home/.codex/AGENTS.md")
                        "global")
                , loadedProject =
                    [ InstructionFile
                        (unsafeEncodeUtf "/repo/AGENTS.md")
                        "project"
                    ]
                }
        formatCodexAgentsMd (unsafeEncodeUtf "/repo") loaded
            `shouldBe` Just
                "# AGENTS.md instructions for /repo\n\n<INSTRUCTIONS>\n\
                \global\n\n--- project-doc ---\n\nproject\n</INSTRUCTIONS>"

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir action = do
    tmp <- getTemporaryDirectory
    bracket
        (mkdtemp (tmp </> "agent-codex-dialect-XXXXXX"))
        removeDirectoryRecursive
        action
