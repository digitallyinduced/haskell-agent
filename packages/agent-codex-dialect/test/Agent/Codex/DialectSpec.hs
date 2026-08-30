module Agent.Codex.DialectSpec (spec) where

import Agent.Codex.Dialect.ApplyPatch
    ( applyPatch
    , streamingPatchReadTarget
    )
import Agent.Codex.Dialect.ProjectInstructions (formatCodexAgentsMd)
import Agent.Codex.Dialect.Prompt
    ( codexSystemPrompt
    , codexSystemPromptForTools
    )
import Agent.Codex.Dialect.Runtime
    ( CodexCodingTools(..)
    , newCodexCodingTools
    )
import Agent.Codex.Dialect.Shell
    ( CodexShellResult(..)
    , closeCodexShellSession
    , continueCodexShellCommand
    , newCodexShellSession
    , resetCodexShellSession
    , startCodexShellCommand
    )
import Agent.Codex.Dialect.Tools (shellCommandIsReadOnly)
import Agent.ProjectInstructions (InstructionFile(..), LoadedAgentsMd(..))
import Agent.ToolDispatch
    ( ToolArgumentStreamEvent(..)
    , ToolCallStreamRef(..)
    , ToolCallResult(..)
    , ToolDispatchConfig(..)
    , customToolCall
    , dispatchToolCall
    , functionToolCall
    )
import Agent.Tools.FileSystem.FilePrefetch (PathProgress(..))
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
    , ToolEnv(..)
    , appToolHandlers
    , defaultToolEnv
    , mkToolRegistry
    , setToolSessionTmp
    , toolSchedulingPlanFor
    )
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (bracket)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Calendar (fromGregorian)
import System.Directory
    ( canonicalizePath
    , createDirectory
    , doesFileExist
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
        Text.toLower prompt
            `shouldSatisfy` Text.isInfixOf "omit workdir"
        Text.toLower prompt
            `shouldNotSatisfy` Text.isInfixOf "always set workdir"

        let dynamicPrompt =
                codexSystemPromptForTools
                    ["shell_command"]
                    (unsafeEncodeUtf "/repo")
                    (fromGregorian 2026 8 23)
        Text.toLower dynamicPrompt
            `shouldSatisfy` Text.isInfixOf "omit workdir"
        Text.toLower dynamicPrompt
            `shouldNotSatisfy` Text.isInfixOf "always set workdir"

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

    it "defaults shell_command to the turn cwd when workdir is omitted" do
        withTempDir \dir -> do
            Text.writeFile (dir </> "cwd-marker") "present"
            env <- defaultToolEnv (unsafeEncodeUtf dir)
            bracket
                (newCodexCodingTools env Nothing Nothing)
                (.codexClose)
                \coding -> do
                    result <- dispatchToolCall
                        testDispatchConfig
                        (appToolHandlers coding.codexAppTools)
                        (functionToolCall
                            "shell-default-cwd"
                            "shell_command"
                            "{\"command\":\"test -f cwd-marker && printf cwd-defaulted\"}")
                    result.output
                        `shouldSatisfy` Text.isInfixOf "cwd-defaulted"

    it "rejects literal system temp paths in shell commands" do
        withTempDir \dir -> do
            env <- defaultToolEnv (unsafeEncodeUtf dir)
            bracket
                (newCodexCodingTools env Nothing Nothing)
                (.codexClose)
                \coding -> do
                    result <- dispatchToolCall
                        testDispatchConfig
                        (appToolHandlers coding.codexAppTools)
                        (functionToolCall
                            "shell-system-tmp"
                            "shell_command"
                            "{\"command\":\"touch /tmp/agent-output\"}")
                    result.output `shouldSatisfy`
                        Text.isInfixOf "Blocked hardcoded system temp path"
                    result.output `shouldSatisfy` Text.isInfixOf "$TMPDIR"

    it "maps a /tmp shell workdir to the private session directory" do
        withTempDir \dir -> do
            let scratch = dir </> "session-scratch"
            createDirectory scratch
            env <- defaultToolEnv (unsafeEncodeUtf dir)
            setToolSessionTmp env (Just (unsafeEncodeUtf scratch))
            canonicalScratch <- canonicalizePath scratch
            bracket
                (newCodexCodingTools env Nothing Nothing)
                (.codexClose)
                \coding -> do
                    result <- dispatchToolCall
                        testDispatchConfig
                        (appToolHandlers coding.codexAppTools)
                        (functionToolCall
                            "shell-system-tmp-workdir"
                            "shell_command"
                            "{\"command\":\"pwd\",\"workdir\":\"/tmp\"}")
                    result.output `shouldSatisfy`
                        Text.isInfixOf (Text.pack canonicalScratch)

    it "stops retained shell commands when resetting a session" do
        withTempDir \dir -> do
            let scratch = dir </> "session-scratch"
                output = scratch </> "retained-output"
            createDirectory scratch
            env <- defaultToolEnv (unsafeEncodeUtf dir)
            setToolSessionTmp env (Just (unsafeEncodeUtf scratch))
            bracket
                (newCodexShellSession env)
                closeCodexShellSession
                \session -> do
                    started <- startCodexShellCommand
                        session
                        env.toolCwd
                        "while :; do printf x >> \"$TMPDIR/retained-output\"; sleep 0.02; done"
                        100
                        (\_ _ -> pure ())
                    commandId <- case started of
                        Right CodexShellRunning
                                { codexShellSessionId = runningId } ->
                            pure runningId
                        _ -> expectationFailure
                            "expected a retained command"
                            >> pure 0
                    waitForFile output `shouldReturn` True
                    resetCodexShellSession session
                    before <- Text.readFile output
                    threadDelay 100000
                    Text.readFile output `shouldReturn` before
                    continued <-
                        continueCodexShellCommand session commandId "" 1
                    case continued of
                        Left message ->
                            message `shouldBe` "Unknown session_id: "
                                <> Text.pack (show commandId)
                        Right _ ->
                            expectationFailure "stale command remained available"

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
                , loadedWarnings = []
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
            , "uniq -c file.txt"
            , "cd src && rg --files"
            , "cd /tmp/repo && git diff -- src/Main.hs"
            , "cd 'packages/agent-core' && ls"
            , "git status --short"
            , "git status --short && git diff --check && git log --oneline"
            , "git -C src log --oneline"
            , "git diff --stat | head -20"
            , "git branch"
            , "cd src && git status --short && git diff"
            , "cd src; ls"
            , "gh issue list"
            ]
            `shouldBe` replicate 16 True
        map shellCommandIsReadOnly
            [ "git fetch origin"
            , "cat src/Main.hs > copy"
            , "printf x | tee output"
            , "nix develop -c cabal test"
            , "bash -c 'rg foo'"
            , "find . -delete"
            , "sed -n '/pattern/w output' src/Main.hs"
            , "git diff --output=copy"
            , "uniq input.txt output.txt"
            , "cd src && nix develop -c cabal test"
            , "cd -- src && ls"
            , "cd $HOME && ls"
            , "git branch -d leftover"
            , "git status --short && git fetch origin"
            , "git diff | tee output"
            , "git rebase origin/master"
            ]
            `shouldBe` replicate 16 False

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

waitForFile :: FilePath -> IO Bool
waitForFile path = go (100 :: Int)
  where
    go 0 = doesFileExist path
    go remaining =
        doesFileExist path >>= \case
            True -> pure True
            False -> threadDelay 10000 >> go (remaining - 1)

testDispatchConfig :: ToolDispatchConfig
testDispatchConfig = ToolDispatchConfig
    { toolDispatchUnknownTool = \name -> "unknown:" <> name
    , toolDispatchFormatResult = either ("ERR " <>) id
    , toolDispatchFormatException = \name _ -> "EX " <> name
    , toolDispatchOnException = \_ _ -> pure ()
    , toolDispatchOnOutput = \_ _ -> pure ()
    , toolDispatchFinalizeOutput = \_call output -> pure output
    }
