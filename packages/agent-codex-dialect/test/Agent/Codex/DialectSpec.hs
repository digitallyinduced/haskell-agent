module Agent.Codex.DialectSpec (spec) where

import Agent.Codex.Dialect.ProjectInstructions (formatCodexAgentsMd)
import Agent.Codex.Dialect.Prompt (codexSystemPrompt)
import Agent.Codex.Dialect.Runtime
    ( CodexCodingTools(..)
    , newCodexCodingTools
    )
import Agent.Codex.Dialect.Tools (shellCommandIsReadOnly)
import Agent.ProjectInstructions (InstructionFile(..), LoadedAgentsMd(..))
import Agent.ToolDispatch (customToolCall, functionToolCall)
import Agent.Tools.Types
    ( AppTool(..)
    , defaultToolEnv
    , mkToolRegistry
    , schedulingPlansConflict
    , toolSchedulingPlanFor
    )
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

    it "classifies only strict observational shell commands as read-only" do
        map shellCommandIsReadOnly
            [ "rg --files"
            , "sed -n '1,80p' src/Main.hs"
            , "git diff -- src/Main.hs"
            , "gh pr view 123 --json statusCheckRollup"
            ]
            `shouldBe` replicate 4 True
        map shellCommandIsReadOnly
            [ "git status --short"
            , "git fetch origin"
            , "cat src/Main.hs > copy"
            , "printf x | tee output"
            , "nix develop -c cabal test"
            , "bash -c 'rg foo'"
            , "find . -delete"
            , "sed -n '/pattern/w output' src/Main.hs"
            , "git diff --output=copy"
            ]
            `shouldBe` replicate 9 False

    it "derives disjoint apply_patch resources from patch paths" do
        withTempDir \dir -> do
            env <- defaultToolEnv (unsafeEncodeUtf dir)
            coding <- newCodexCodingTools env Nothing Nothing
            let registry =
                    either (error . Text.unpack) id $
                        mkToolRegistry coding.codexAppTools
                patch ident path =
                    customToolCall ident "apply_patch" $
                        "*** Begin Patch\n*** Add File: "
                            <> path
                            <> "\n+content\n*** End Patch"
            first <- toolSchedulingPlanFor registry (patch "p1" "a.txt")
            second <- toolSchedulingPlanFor registry (patch "p2" "b.txt")
            same <- toolSchedulingPlanFor registry (patch "p3" "a.txt")
            schedulingPlansConflict first second `shouldBe` False
            schedulingPlansConflict first same `shouldBe` True
            coding.codexClose

    it "serializes write_stdin only per shell session" do
        withTempDir \dir -> do
            env <- defaultToolEnv (unsafeEncodeUtf dir)
            coding <- newCodexCodingTools env Nothing Nothing
            let registry =
                    either (error . Text.unpack) id $
                        mkToolRegistry coding.codexAppTools
                stdin ident sessionId =
                    functionToolCall ident "write_stdin" $
                        "{\"session_id\":" <> sessionId <> "}"
            first <- toolSchedulingPlanFor registry (stdin "s1" "1")
            second <- toolSchedulingPlanFor registry (stdin "s2" "2")
            same <- toolSchedulingPlanFor registry (stdin "s3" "1")
            schedulingPlansConflict first second `shouldBe` False
            schedulingPlansConflict first same `shouldBe` True
            coding.codexClose

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir action = do
    tmp <- getTemporaryDirectory
    bracket
        (mkdtemp (tmp </> "agent-codex-dialect-XXXXXX"))
        removeDirectoryRecursive
        action
