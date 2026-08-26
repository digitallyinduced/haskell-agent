module Agent.Codex.DialectSpec (spec) where

import Agent.Codex.Dialect.ApplyPatch
    ( applyPatch
    , streamingPatchReadTarget
    )
import Agent.Tools.FileSystem.FilePrefetch (PathProgress(..))
import Agent.Codex.Dialect.ProjectInstructions (formatCodexAgentsMd)
import Agent.Codex.Dialect.Prompt (codexSystemPrompt)
import Agent.Codex.Dialect.Runtime
    ( CodexCodingTools(..)
    , newCodexCodingTools
    )
import Agent.Codex.Dialect.Tools (shellCommandIsReadOnly)
import Agent.ProjectInstructions (InstructionFile(..), LoadedAgentsMd(..))
import Agent.ToolDispatch
    ( ToolArgumentStreamEvent(..)
    , ToolCallStreamRef(..)
    , customToolCall
    , functionToolCall
    )
import Agent.Tools.Speculation
    ( closeToolSpeculationRuntime
    , newToolSpeculationRuntime
    , observeToolArgumentEvent
    , retainToolSpeculation
    , takeToolSpeculation
    , waitForToolSpeculation
    )
import Agent.Tools.Scheduling (schedulingPlansConflict)
import Agent.Tools.Types
    ( AppTool(..)
    , defaultToolEnv
    , mkToolRegistry
    , toolSchedulingPlanFor
    )
import Control.Exception.Safe (bracket)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Calendar (fromGregorian)
import System.Directory
    ( doesFileExist
    , getTemporaryDirectory
    , removeDirectoryRecursive
    )
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

    it "does not partially apply a patch when a later hunk is stale" do
        withTempDir \dir -> do
            let firstPath = dir </> "first.txt"
                secondPath = dir </> "second.txt"
            Text.writeFile firstPath "first old\n"
            Text.writeFile secondPath "second current\n"
            env <- defaultToolEnv (unsafeEncodeUtf dir)
            result <- applyPatch env $
                "*** Begin Patch\n\
                \*** Update File: first.txt\n\
                \@@\n\
                \-first old\n\
                \+first new\n\
                \*** Update File: second.txt\n\
                \@@\n\
                \-second stale\n\
                \+second new\n\
                \*** End Patch"
            result `shouldSatisfy` \case
                Left message ->
                    and
                        [ "second.txt" `Text.isInfixOf` message
                        , "Failed to find expected lines" `Text.isInfixOf` message
                        , "no files were changed" `Text.isInfixOf` message
                        ]
                Right _ -> False
            Text.readFile firstPath `shouldReturn` "first old\n"
            Text.readFile secondPath `shouldReturn` "second current\n"

    it "does not commit staged add, move, or delete actions after validation fails" do
        withTempDir \dir -> do
            let addedPath = dir </> "added.txt"
                moveSourcePath = dir </> "move-source.txt"
                moveDestPath = dir </> "move-dest.txt"
                deletePath = dir </> "delete.txt"
                stalePath = dir </> "stale.txt"
            Text.writeFile moveSourcePath "move old\n"
            Text.writeFile deletePath "delete me\n"
            Text.writeFile stalePath "current\n"
            env <- defaultToolEnv (unsafeEncodeUtf dir)
            result <- applyPatch env $
                "*** Begin Patch\n\
                \*** Add File: added.txt\n\
                \+added\n\
                \*** Update File: move-source.txt\n\
                \*** Move to: move-dest.txt\n\
                \@@\n\
                \-move old\n\
                \+move new\n\
                \*** Delete File: delete.txt\n\
                \*** Update File: stale.txt\n\
                \@@\n\
                \-stale\n\
                \+updated\n\
                \*** End Patch"
            result `shouldSatisfy` \case
                Left _ -> True
                Right _ -> False
            doesFileExist addedPath `shouldReturn` False
            Text.readFile moveSourcePath `shouldReturn` "move old\n"
            doesFileExist moveDestPath `shouldReturn` False
            Text.readFile deletePath `shouldReturn` "delete me\n"
            Text.readFile stalePath `shouldReturn` "current\n"

    it "validates later hunks against earlier staged changes" do
        withTempDir \dir -> do
            let path = dir </> "staged.txt"
            Text.writeFile path "before\n"
            env <- defaultToolEnv (unsafeEncodeUtf dir)
            result <- applyPatch env $
                "*** Begin Patch\n\
                \*** Update File: staged.txt\n\
                \@@\n\
                \-before\n\
                \+middle\n\
                \*** Update File: staged.txt\n\
                \@@\n\
                \-middle\n\
                \+after\n\
                \*** End Patch"
            result `shouldSatisfy` \case
                Right _ -> True
                Left _ -> False
            Text.readFile path `shouldReturn` "after\n"

    it "extracts a streamed update path from a partial apply_patch body" do
        streamingPatchReadTarget
            "*** Begin Patch\n*** Update File: src/alpha.hs\n@@\n"
            `shouldBe` Just (PathComplete "src/alpha.hs")
        streamingPatchReadTarget
            "*** Begin Patch\n*** Update File: src/uni"
            `shouldBe` Just (PathPrefix "src/uni")

    it "prefetches the updated file while the rest of the patch streams" do
        withTempDir \dir -> do
            Text.writeFile (dir </> "alpha.txt") "old\n"
            env <- defaultToolEnv (unsafeEncodeUtf dir)
            coding <- newCodexCodingTools env Nothing Nothing
            let tools = coding.codexAppTools
                callId = "call-patch"
                prefix =
                    "*** Begin Patch\n*** Update File: alpha.txt\n"
                patch =
                    prefix
                        <> "@@\n-old\n+new\n*** End Patch"
            bracket
                (newToolSpeculationRuntime tools)
                closeToolSpeculationRuntime
                \runtime -> do
                    observeToolArgumentEvent runtime $
                        ToolArgumentsStarted
                            { argumentStreamRefs =
                                [ToolCallStreamItem "item-patch"]
                            , argumentStreamCallId = callId
                            , argumentStreamName = Just "apply_patch"
                            , argumentStreamArguments = prefix
                            }
                    waitForToolSpeculation runtime
                    retainToolSpeculation runtime
                        [customToolCall callId "apply_patch" patch]
                    takeToolSpeculation runtime
                        (customToolCall callId "apply_patch" patch)
                        `shouldReturn` Just
                            (Right "Success. Updated the following files:\nM alpha.txt\n")
                    Text.readFile (dir </> "alpha.txt") `shouldReturn` "new\n"
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
