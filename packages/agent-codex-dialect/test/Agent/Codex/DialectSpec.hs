module Agent.Codex.DialectSpec (spec) where

import Agent.Codex.Dialect.ApplyPatch (applyPatch)
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
import Agent.Tools.Background (setBackgroundTaskHooks)
import Agent.Tools.IO (CommandResult(..))
import Agent.ToolDispatch
    ( ToolCallResult(..)
    , ToolDispatchConfig(..)
    , customToolCall
    , dispatchToolCall
    , functionToolCall
    )
import Agent.Tools.Scheduling (schedulingPlansConflict)
import Agent.Tools.Types
    ( AppTool(..)
    , BackgroundTaskHooks(..)
    , BackgroundTaskNotice(..)
    , ToolEnv(..)
    , appToolHandlers
    , defaultToolEnv
    , mkToolRegistry
    , setToolSessionTmp
    , toolSchedulingPlanFor
    )
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar_
    , newMVar
    , readMVar
    )
import Control.Exception.Safe (bracket, tryIO)
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
import System.Exit (ExitCode(..))
import System.FilePath (makeRelative, (</>))
import System.Info (os)
import System.IO.Error (isPermissionError)
import System.OsPath (unsafeEncodeUtf)
import System.Posix.Temp (mkdtemp)
import System.Process (readProcessWithExitCode)
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
                , "view_image"
                , "update_plan"
                , "write_plan"
                ]
                `shouldBe` replicate 5 True
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

    it "retains commands after the default ten-second yield" do
        withTempDir \dir -> do
            env <- defaultToolEnv (unsafeEncodeUtf dir)
            bracket
                (newCodexCodingTools env Nothing Nothing)
                (.codexClose)
                \coding -> do
                    let handlers = appToolHandlers coding.codexAppTools
                    started <- dispatchToolCall
                        testDispatchConfig
                        handlers
                        (functionToolCall
                            "shell-default-yield"
                            "shell_command"
                            "{\"command\":\"printf started; trap 'exit 0' INT; while :; do sleep 1; done\"}")
                    started.output `shouldSatisfy`
                        Text.isPrefixOf "Process still running."
                    started.output `shouldSatisfy` Text.isInfixOf "started"
                    started.output `shouldNotSatisfy`
                        Text.isInfixOf "timed out"
                    sessionId <- expectShellSessionId started.output

                    interrupted <- dispatchToolCall
                        testDispatchConfig
                        handlers
                        (functionToolCall
                            "shell-default-yield-interrupt"
                            "write_stdin"
                            ("{\"session_id\":" <> sessionId
                                <> ",\"chars\":\"\\u0003\","
                                <> "\"yield_time_ms\":5000}"))
                    interrupted.output `shouldSatisfy`
                        Text.isPrefixOf "Exit code:"

                    stale <- dispatchToolCall
                        testDispatchConfig
                        handlers
                        (functionToolCall
                            "shell-default-yield-stale"
                            "write_stdin"
                            ("{\"session_id\":" <> sessionId <> "}"))
                    stale.output `shouldSatisfy`
                        Text.isInfixOf "Unknown session_id"

    it "keeps timeout_ms as an explicit hard timeout" do
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
                            "shell-explicit-timeout"
                            "shell_command"
                            "{\"command\":\"sleep 1\",\"timeout_ms\":10}")
                    result.output `shouldSatisfy`
                        Text.isInfixOf "timed out after 10ms"
                    result.output `shouldNotSatisfy`
                        Text.isInfixOf "session_id:"

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

    it "rejects traversal above the private temp variables" do
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
                            "shell-session-tmp-traversal"
                            "shell_command"
                            "{\"command\":\"cat \\\"$TMPDIR/../other-session/secret\\\"\"}")
                    result.output `shouldSatisfy`
                        Text.isInfixOf "Blocked path traversal"

    it "rejects system temp paths relative to the shell cwd" do
        withTempDir \dir -> do
            env <- defaultToolEnv (unsafeEncodeUtf dir)
            let relativeTmp =
                    Text.pack (makeRelative dir "/tmp/agent-output")
            bracket
                (newCodexCodingTools env Nothing Nothing)
                (.codexClose)
                \coding -> do
                    result <- dispatchToolCall
                        testDispatchConfig
                        (appToolHandlers coding.codexAppTools)
                        (functionToolCall
                            "shell-relative-system-tmp"
                            "shell_command"
                            ("{\"command\":\"cat "
                                <> relativeTmp
                                <> "\"}"))
                    result.output `shouldSatisfy`
                        Text.isInfixOf "Blocked hardcoded system temp path"

    it "maps a /tmp shell workdir to the private session directory" do
        requireProcessSandbox
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
        requireProcessSandbox
        withTempDir \dir -> do
            let scratch = dir </> "session-scratch"
                output = scratch </> "retained-output"
            createDirectory scratch
            env <- defaultToolEnv (unsafeEncodeUtf dir)
            setToolSessionTmp env (Just (unsafeEncodeUtf scratch))
            notices <- installBackgroundNoticeStore env
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
                    readMVar notices `shouldReturn` Nothing

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

    it "waits instead of hot-polling an empty write_stdin call" do
        withTempDir \dir -> do
            env <- defaultToolEnv (unsafeEncodeUtf dir)
            bracket
                (newCodexCodingTools env Nothing Nothing)
                (.codexClose)
                \coding -> do
                    let handlers = appToolHandlers coding.codexAppTools
                    started <- dispatchToolCall
                        testDispatchConfig
                        handlers
                        (functionToolCall
                            "shell-wait"
                            "shell_command"
                            "{\"command\":\"sleep 0.2; printf done\",\"yield_time_ms\":1}")
                    started.output `shouldSatisfy`
                        Text.isPrefixOf "Process still running."
                    sessionId <- expectShellSessionId started.output
                    continued <- dispatchToolCall
                        testDispatchConfig
                        handlers
                        (functionToolCall
                            "shell-poll"
                            "write_stdin"
                            ("{\"session_id\":" <> sessionId
                                <> ",\"yield_time_ms\":1}"))
                    continued.output `shouldSatisfy`
                        Text.isPrefixOf "Exit code: 0\n"
                    continued.output `shouldSatisfy` Text.isInfixOf "done"

    it "reports an unobserved retained command without polling" do
        requireProcessSandbox
        withTempDir \dir -> do
            env <- defaultToolEnv (unsafeEncodeUtf dir)
            notices <- installBackgroundNoticeStore env
            bracket
                (newCodexShellSession env)
                closeCodexShellSession
                \session -> do
                    started <- startCodexShellCommand
                        session
                        env.toolCwd
                        "sleep 0.05; printf completion-output"
                        1
                        (\_ _ -> pure ())
                    commandId <- case started of
                        Right CodexShellRunning
                                { codexShellSessionId = runningId } ->
                            pure runningId
                        _ -> expectationFailure
                            "expected a retained command"
                            >> pure 0
                    notice <- waitForBackgroundNotice
                        notices
                        ("codex-shell:" <> Text.pack (show commandId))
                    notice.noticeBody `shouldSatisfy`
                        Text.isInfixOf "completion-output"
                    notice.noticeBody `shouldSatisfy`
                        Text.isInfixOf "do not call write_stdin"

    it "does not publish a notice when the initial wait returns the result" do
        requireProcessSandbox
        withTempDir \dir -> do
            env <- defaultToolEnv (unsafeEncodeUtf dir)
            notices <- installBackgroundNoticeStore env
            bracket
                (newCodexShellSession env)
                closeCodexShellSession
                \session -> do
                    result <- startCodexShellCommand
                        session
                        env.toolCwd
                        "printf immediate-output"
                        1000
                        (\_ _ -> pure ())
                    case result of
                        Right (CodexShellFinished final) ->
                            final.commandStdout `shouldBe` "immediate-output"
                        _ -> expectationFailure
                            "expected the command to finish in the initial wait"
                    readMVar notices `shouldReturn` Nothing

    it "omits output already returned by the initial yield from its notice" do
        requireProcessSandbox
        withTempDir \dir -> do
            env <- defaultToolEnv (unsafeEncodeUtf dir)
            notices <- installBackgroundNoticeStore env
            bracket
                (newCodexShellSession env)
                closeCodexShellSession
                \session -> do
                    started <- startCodexShellCommand
                        session
                        env.toolCwd
                        "printf early-output; sleep 0.1; printf late-output"
                        50
                        (\_ _ -> pure ())
                    commandId <- case started of
                        Right CodexShellRunning
                                { codexShellSessionId = runningId
                                , codexShellStdout = initialOutput
                                } -> do
                            initialOutput `shouldSatisfy`
                                Text.isInfixOf "early-output"
                            pure runningId
                        _ -> expectationFailure
                            "expected a retained command"
                            >> pure 0
                    notice <- waitForBackgroundNotice
                        notices
                        ("codex-shell:" <> Text.pack (show commandId))
                    let (_, resultSection) =
                            Text.breakOn "\nResult:\n" notice.noticeBody
                    resultSection `shouldSatisfy`
                        Text.isInfixOf "late-output"
                    resultSection `shouldNotSatisfy`
                        Text.isInfixOf "early-output"

    it "retracts a retained-command notice when explicitly collected" do
        requireProcessSandbox
        withTempDir \dir -> do
            env <- defaultToolEnv (unsafeEncodeUtf dir)
            notices <- installBackgroundNoticeStore env
            bracket
                (newCodexShellSession env)
                closeCodexShellSession
                \session -> do
                    started <- startCodexShellCommand
                        session
                        env.toolCwd
                        "sleep 0.05; printf explicit-output"
                        1
                        (\_ _ -> pure ())
                    commandId <- case started of
                        Right CodexShellRunning
                                { codexShellSessionId = runningId } ->
                            pure runningId
                        _ -> expectationFailure
                            "expected a retained command"
                            >> pure 0
                    _ <- waitForBackgroundNotice
                        notices
                        ("codex-shell:" <> Text.pack (show commandId))
                    collected <-
                        continueCodexShellCommand session commandId "" 1000
                    case collected of
                        Right (CodexShellFinished result) ->
                            "explicit-output"
                                `Text.isInfixOf` result.commandStdout
                                `shouldBe` True
                        _ -> expectationFailure
                            "expected the retained command's final result"
                    readMVar notices `shouldReturn` Nothing

    it "does not count completed sessions against the live command limit" do
        requireProcessSandbox
        withTempDir \dir -> do
            env <- defaultToolEnv (unsafeEncodeUtf dir)
            bracket
                (newCodexShellSession env)
                closeCodexShellSession
                \session ->
                    mapM_
                        (\_ -> do
                            started <- startCodexShellCommand
                                session
                                env.toolCwd
                                "sleep 0.01"
                                1
                                (\_ _ -> pure ())
                            case started of
                                Right CodexShellRunning{} -> pure ()
                                _ -> expectationFailure
                                    "completed archive exhausted the live command limit"
                            threadDelay 30000)
                        [1 .. 66 :: Int]

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

requireProcessSandbox :: IO ()
requireProcessSandbox
    | os /= "darwin" = pure ()
    | otherwise = do
        doesFileExist "/usr/bin/sandbox-exec" >>= \case
            False -> pendingWith
                "sandbox-exec is unavailable in this test environment"
            True -> do
                probe <- tryIO $
                    readProcessWithExitCode
                        "/usr/bin/sandbox-exec"
                        ["-p", "(version 1) (allow default)", "/usr/bin/true"]
                        ""
                case probe of
                    Left err
                        | isPermissionError err -> pendingWith
                            "sandbox-exec is unavailable in this test environment"
                        | otherwise -> ioError err
                    Right (ExitSuccess, _, _) -> pure ()
                    Right (ExitFailure 71, _, stderr)
                        | "sandbox_apply: Operation not permitted"
                            `Text.isInfixOf` Text.pack stderr ->
                            pendingWith
                                "sandbox-exec is unavailable in this test environment"
                    Right result ->
                        expectationFailure $
                            "sandbox-exec capability probe failed unexpectedly: "
                                <> show result

waitForFile :: FilePath -> IO Bool
waitForFile path = go (100 :: Int)
  where
    go 0 = doesFileExist path
    go remaining =
        doesFileExist path >>= \case
            True -> pure True
            False -> threadDelay 10000 >> go (remaining - 1)

expectShellSessionId :: Text.Text -> IO Text.Text
expectShellSessionId output =
    case
        [ ident
        | line <- Text.lines output
        , Just ident <- [Text.stripPrefix "session_id: " line]
        ]
    of
        [ident] -> pure ident
        _ ->
            expectationFailure "expected one retained shell session id"
                >> pure "0"

installBackgroundNoticeStore
    :: ToolEnv
    -> IO (MVar (Maybe BackgroundTaskNotice))
installBackgroundNoticeStore env = do
    notices <- newMVar Nothing
    setBackgroundTaskHooks env BackgroundTaskHooks
        { backgroundTaskCompleted = \notice ->
            modifyMVar_ notices (\_ -> pure (Just notice))
                >> pure True
        , backgroundTaskDismissed = \key ->
            modifyMVar_ notices \current ->
                pure $ case current of
                    Just notice | notice.noticeKey == key -> Nothing
                    _ -> current
        }
    pure notices

waitForBackgroundNotice
    :: MVar (Maybe BackgroundTaskNotice)
    -> Text.Text
    -> IO BackgroundTaskNotice
waitForBackgroundNotice notices key = go (200 :: Int)
  where
    go 0 = expectationFailure
        ("timed out waiting for background notice " <> Text.unpack key)
        >> fail "unreachable"
    go remaining =
        readMVar notices >>= \case
            Just notice | notice.noticeKey == key -> pure notice
            Nothing -> threadDelay 10000 >> go (remaining - 1)
            Just _ -> threadDelay 10000 >> go (remaining - 1)

testDispatchConfig :: ToolDispatchConfig
testDispatchConfig = ToolDispatchConfig
    { toolDispatchUnknownTool = \name -> "unknown:" <> name
    , toolDispatchFormatResult = either ("ERR " <>) id
    , toolDispatchFormatException = \name _ -> "EX " <> name
    , toolDispatchOnException = \_ _ -> pure ()
    , toolDispatchOnOutput = \_ _ -> pure ()
    , toolDispatchFinalizeOutput = \_call output -> pure output
    }
