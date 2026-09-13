module Agent.CLI.ApprovalSpec (spec) where

import Agent.CLI.Approval
    ( ApprovalAction(..)
    , ApprovalFacts(..)
    , ApprovalNotice(..)
    , ApprovalPlan(..)
    , approveFilesystemRootAccess
    , approveToolDecisionWithReporter
    , approveToolDecisionWithReporterAndPersistence
    , approveToolDecisionWithReporterAndPersistenceClassified
    , childApprove
    , planApproval
    , resolveApprovalPrompt
    , resolveApprovalPromptWith
    )
import Agent.CLI.Options (ApprovalPolicy(..))
import Agent.ComputerUse (computerToolName, computerUseTool)
import Agent.CLI.Permission (PermissionChoice(..))
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallKind(..)
    , functionToolCall
    , noArgsTool
    )
import Agent.Tools.Types
    ( AppTool(..)
    , ToolApproval(..)
    , ApprovalRequirement(..)
    , ApprovalRule(..)
    , ToolRegistry
    , jsonAppTool
    , mkToolRegistry
    )
import Agent.Tools.ShellPermission (shellPermissionApproval)
import Agent.Tools.PlanMode
    ( activatePlanMode
    , newPlanModeEnv
    )
import Data.IORef
    ( modifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import System.OsPath (unsafeEncodeUtf)
import Test.Hspec

spec :: Spec
spec = do
    describe "planApproval" do
        it "hard-denies catastrophic shell calls before requesting classification" do
            let call = functionToolCall
                    "call-shell"
                    "shell_command"
                    "{\"command\":\"rm -rf /\"}"
                facts = (approvalFacts call)
                    { policy = ApproveAll
                    , allowedForSession = Just True
                    }
            planApproval facts
                `shouldSatisfy` \case
                    CompleteApproval
                        (ToolApprovalDenied message)
                        [ReportApprovalNotice (ApprovalWarning notice)] ->
                            "Blocked dangerous shell command"
                                `Text.isInfixOf` message
                                && message `Text.isInfixOf` notice
                    _ -> False

        it "hard-denies session-destructive computer shortcuts before prompting" do
            let call = ToolCall
                    "call-computer"
                    computerToolName
                    "{\"actions\":[{\"type\":\"keypress\",\"keys\":[\"ctrl\",\"alt\",\"delete\"]}]}"
                    ComputerFunctionCallKind
                    False
                facts = (approvalFacts call)
                    { policy = ApproveAll
                    , allowedForSession = Just True
                    }
            planApproval facts
                `shouldSatisfy` \case
                    CompleteApproval
                        (ToolApprovalDenied message)
                        [ReportApprovalNotice (ApprovalWarning notice)] ->
                            "Computer key combination is blocked"
                                `Text.isInfixOf` message
                                && message `Text.isInfixOf` notice
                    _ -> False

        it "rejects hardcoded system temp paths before prompting" do
            let call = functionToolCall
                    "call-shell"
                    "shell_command"
                    "{\"command\":\"render input.svg /tmp/output.png\"}"
                facts = (approvalFacts call)
                    { policy = ApproveAll
                    , allowedForSession = Just True
                    }
            planApproval facts
                `shouldSatisfy` \case
                    CompleteApproval
                        (ToolApprovalDenied message)
                        [ReportApprovalNotice (ApprovalWarning notice)] ->
                            "Blocked hardcoded system temp path"
                                `Text.isInfixOf` message
                                && "$TMPDIR" `Text.isInfixOf` message
                                && message `Text.isInfixOf` notice
                    _ -> False

        it "requests facts in security-precedence order" do
            let initial = approvalFacts mutatingCall
                classified = initial { readOnly = Just False }
            planApproval initial `shouldBe` NeedReadOnlyClassification
            planApproval classified `shouldBe` NeedSessionAllowance
            planApproval (classified { allowedForSession = Just False })
                `shouldBe` NeedPermissionPrompt

        it "cannot bypass plan mode with remembered approval or yolo" do
            let facts = (approvalFacts mutatingCall)
                    { policy = ApproveAll
                    , planActive = True
                    , readOnly = Just False
                    , allowedForSession = Just True
                    }
            planApproval facts `shouldSatisfy` \case
                CompleteApproval
                    (ToolApprovalDenied message)
                    [ReportApprovalNotice (ApprovalWarning notice)] ->
                        "file edits are not allowed in plan mode"
                            `Text.isInfixOf` message
                            && notice == message
                _ -> False

        it "auto-approves only the dedicated plan-file exception" do
            let planWrite = functionToolCall
                    "call-write-plan"
                    "write_plan"
                    "{\"content\":\"# Plan\"}"
                facts = (approvalFacts planWrite)
                    { policy = PromptMutating
                    , planActive = True
                    , readOnly = Just False
                    }
            planApproval facts
                `shouldBe` CompleteApproval ToolApprovalGranted []

        it "allows the path-locked plan edit but rejects another target" do
            let searchReplace target = functionToolCall
                    "call-search-replace"
                    "search_replace"
                    ("{\"file_path\":\"" <> target <> "\"}")
                facts call = (approvalFacts call)
                    { policy = ApproveAll
                    , planActive = True
                    , readOnly = Just False
                    , allowedForSession = Just True
                    }
            planApproval (facts (searchReplace "plan.md"))
                `shouldBe` CompleteApproval ToolApprovalGranted []
            planApproval (facts (searchReplace "src/Main.hs"))
                `shouldSatisfy` \case
                    CompleteApproval (ToolApprovalDenied _) [_] -> True
                    _ -> False

        it "blocks collaboration writes in plan mode even if marked read-only" do
            let call = functionToolCall
                    "call-spawn"
                    "collaboration.spawn_agent"
                    "{}"
                facts = (approvalFacts call)
                    { policy = ApproveAll
                    , planActive = True
                    , readOnly = Just True
                    , allowedForSession = Just True
                    }
            planApproval facts `shouldSatisfy` \case
                CompleteApproval (ToolApprovalDenied _) [_] -> True
                _ -> False

        it "applies the session policy after remembered-tool approval" do
            let classified = (approvalFacts mutatingCall)
                    { readOnly = Just False
                    }
            planApproval (classified { allowedForSession = Just True })
                `shouldBe` CompleteApproval ToolApprovalGranted []
            planApproval (classified
                { policy = ApproveAll
                , allowedForSession = Just False
                })
                `shouldBe` CompleteApproval ToolApprovalGranted []
            planApproval (classified
                { policy = DenyMutating
                , allowedForSession = Just False
                })
                `shouldBe` CompleteApproval ToolApprovalRejected []

        it "auto-approves read-only calls under restrictive policies" do
            let facts policy = (approvalFacts readOnlyCall)
                    { policy
                    , readOnly = Just True
                    , allowedForSession = Just False
                    }
            planApproval (facts DenyMutating)
                `shouldBe` CompleteApproval ToolApprovalGranted []
            planApproval (facts PromptMutating)
                `shouldBe` CompleteApproval ToolApprovalGranted []

        it "requires fresh confirmation despite yolo or remembered approval" do
            let facts policy = (approvalFacts mutatingCall)
                    { policy
                    , readOnly = Just False
                    , allowedForSession = Just True
                    , requiresExplicitApproval = True
                    }
            planApproval (facts ApproveAll)
                `shouldBe` NeedPermissionPrompt
            planApproval (facts PromptMutating)
                `shouldBe` NeedPermissionPrompt
            planApproval (facts DenyMutating)
                `shouldBe` CompleteApproval ToolApprovalRejected []

    describe "resolveApprovalPrompt" do
        it "maps cancellation and explicit denial to an in-band denial" do
            resolveApprovalPrompt mutatingCall Nothing
                `shouldBe` CompleteApproval ToolApprovalRejected []
            resolveApprovalPrompt mutatingCall (Just PermissionDeny)
                `shouldBe` CompleteApproval ToolApprovalRejected []

        it "allows once without changing approval state" do
            resolveApprovalPrompt mutatingCall (Just PermissionAllowOnce)
                `shouldBe` CompleteApproval ToolApprovalGranted []

        it "plans project persistence after enabling auto-approval" do
            resolveApprovalPrompt mutatingCall (Just PermissionAllowAll)
                `shouldBe` CompleteApproval
                    ToolApprovalGranted
                    [ SetApprovalPolicy ApproveAll
                    , PersistProjectAutoApprove
                    , ReportApprovalNotice
                        (ApprovalSuccess
                            "✓ auto-approve on (saved for project)")
                    ]

        it "canonicalizes remembered aliases but reports the requested name" do
            let call = functionToolCall
                    "call-shell"
                    "run_terminal_command"
                    "{\"command\":\"git status\"}"
            resolveApprovalPrompt call (Just PermissionAllowTool)
                `shouldBe` CompleteApproval
                    ToolApprovalGranted
                    [ RememberToolForSession "run_terminal_cmd"
                    , ReportApprovalNotice
                        (ApprovalSuccess
                            "✓ always allow run_terminal_command this session")
                    ]

        it "distinguishes unavailable fresh approval from an explicit denial" do
            resolveApprovalPromptWith True mutatingCall Nothing
                `shouldBe` CompleteApproval
                    (ToolApprovalDenied "Fresh approval was not obtained: the approval prompt was unavailable or closed without a decision. The tool was not run.")
                    []
            resolveApprovalPromptWith True mutatingCall (Just PermissionDeny)
                `shouldBe` CompleteApproval ToolApprovalRejected []
            resolveApprovalPromptWith True mutatingCall (Just PermissionAllowOnce)
                `shouldBe` CompleteApproval ToolApprovalGranted []

        it "rejects broader approval for an explicit-confirmation call" do
            resolveApprovalPromptWith True mutatingCall
                (Just PermissionAllowAll)
                `shouldBe` CompleteApproval
                    (ToolApprovalDenied "This tool requires fresh approval for this invocation; a remembered or blanket approval cannot authorize it. The tool was not run.")
                    []
            resolveApprovalPromptWith True mutatingCall
                (Just PermissionAllowTool)
                `shouldBe` CompleteApproval
                    (ToolApprovalDenied "This tool requires fresh approval for this invocation; a remembered or blanket approval cannot authorize it. The tool was not run.")
                    []
        it "preserves project-wide approval semantics for a computer workflow" do
            let call = ToolCall
                    { callId = "computer-1"
                    , name = computerToolName
                    , arguments = "{}"
                    , callKind = ComputerCallKind
                    , argumentsEncrypted = False
                    }
            resolveApprovalPrompt call (Just PermissionAllowAll)
                `shouldBe` CompleteApproval
                    ToolApprovalGranted
                    [ SetApprovalPolicy ApproveAll
                    , PersistProjectAutoApprove
                    , RememberToolForSession computerToolName
                    , ReportApprovalNotice
                        (ApprovalSuccess
                            "✓ auto-approve on (saved for project)")
                    , ReportApprovalNotice
                        (ApprovalSuccess
                            "✓ computer use approved until disabled")
                    ]

        it "keeps Allow once distinct from a computer workflow grant" do
            let call = ToolCall
                    { callId = "computer-1"
                    , name = computerToolName
                    , arguments = "{}"
                    , callKind = ComputerCallKind
                    , argumentsEncrypted = False
                    }
            resolveApprovalPrompt call (Just PermissionAllowOnce)
                `shouldBe` CompleteApproval ToolApprovalGranted []
            resolveApprovalPrompt call (Just PermissionAllowTool)
                `shouldBe` CompleteApproval
                    ToolApprovalGranted
                    [ RememberToolForSession computerToolName
                    , ReportApprovalNotice
                        (ApprovalSuccess
                            "✓ computer use approved until disabled")
                    ]

    describe "approveFilesystemRootAccess" do
        it "bypasses the prompt whenever the live policy is yolo" do
            policy <- newIORef PromptMutating
            requests <- newIORef (0 :: Int)
            let request = modifyIORef' requests (+ 1) >> pure False

            approveFilesystemRootAccess policy request
                `shouldReturn` False
            readIORef requests `shouldReturn` 1

            writeIORef policy ApproveAll
            approveFilesystemRootAccess policy request
                `shouldReturn` True
            readIORef requests `shouldReturn` 1

    describe "approveToolDecisionWith" do
        it "auto-approves only the marked call without changing session policy" do
            policy <- newIORef PromptMutating
            allowed <- newIORef Set.empty
            plan <- newPlanModeEnv (unsafeEncodeUtf "/tmp/approval-test") Nothing
            permissionRequests <- newIORef (0 :: Int)
            persistenceCalls <- newIORef (0 :: Int)

            approveToolDecisionWithReporterAndPersistence
                (\_ -> modifyIORef' permissionRequests (+ 1)
                    >> pure (Just PermissionAllowAll))
                (\_ -> pure ())
                (modifyIORef' persistenceCalls (+ 1))
                policy allowed
                (registry [autoApproveMutatingTool])
                plan mutatingCall
                `shouldReturn` ToolApprovalGranted

            readIORef permissionRequests `shouldReturn` 0
            readIORef persistenceCalls `shouldReturn` 0
            readIORef policy `shouldReturn` PromptMutating
            readIORef allowed `shouldReturn` Set.empty

        it "still prompts for an unmarked host tool" do
            policy <- newIORef PromptMutating
            allowed <- newIORef Set.empty
            plan <- newPlanModeEnv (unsafeEncodeUtf "/tmp/approval-test") Nothing
            permissionRequests <- newIORef (0 :: Int)

            approveToolDecisionWithReporter
                (\_ -> modifyIORef' permissionRequests (+ 1)
                    >> pure (Just PermissionDeny))
                (\_ -> pure ())
                policy allowed
                (registry [mutatingTool])
                plan mutatingCall
                `shouldReturn` ToolApprovalRejected

            readIORef permissionRequests `shouldReturn` 1
            readIORef policy `shouldReturn` PromptMutating

        it "keeps plan mode ahead of scoped auto-approval" do
            policy <- newIORef PromptMutating
            allowed <- newIORef Set.empty
            plan <- newPlanModeEnv (unsafeEncodeUtf "/tmp/approval-test") Nothing
            activatePlanMode plan
            permissionRequests <- newIORef (0 :: Int)

            result <- approveToolDecisionWithReporter
                (\_ -> modifyIORef' permissionRequests (+ 1)
                    >> pure (Just PermissionAllowOnce))
                (\_ -> pure ())
                policy allowed
                (registry [autoApproveMutatingTool])
                plan mutatingCall

            result `shouldSatisfy` \case
                ToolApprovalDenied message -> "only editable file" `Text.isInfixOf` message
                _ -> False
            readIORef permissionRequests `shouldReturn` 0

        it "keeps dangerous shell denials ahead of scoped auto-approval" do
            policy <- newIORef PromptMutating
            allowed <- newIORef Set.empty
            plan <- newPlanModeEnv (unsafeEncodeUtf "/tmp/approval-test") Nothing
            permissionRequests <- newIORef (0 :: Int)
            let call = functionToolCall
                    "call-shell"
                    "shell_command"
                    "{\"command\":\"rm -rf /\"}"
                shellTool = tool
                    "shell_command"
                    (AutoApprove AlwaysPrompt)

            result <- approveToolDecisionWithReporter
                (\_ -> modifyIORef' permissionRequests (+ 1)
                    >> pure (Just PermissionAllowOnce))
                (\_ -> pure ())
                policy allowed (registry [shellTool]) plan call

            result `shouldSatisfy` \case
                ToolApprovalDenied message -> "Blocked dangerous shell command" `Text.isInfixOf` message
                _ -> False
            readIORef permissionRequests `shouldReturn` 0

        it "does not bypass computer-use consent when marked for auto-approval" do
            policy <- newIORef PromptMutating
            allowed <- newIORef Set.empty
            plan <- newPlanModeEnv (unsafeEncodeUtf "/tmp/approval-test") Nothing
            permissionRequests <- newIORef (0 :: Int)
            let call = ToolCall
                    { callId = "computer-1"
                    , name = computerToolName
                    , arguments = "{}"
                    , callKind = ComputerCallKind
                    , argumentsEncrypted = False
                    }
                autoApproveComputer = computerUseTool
                    { appToolApproval =
                        AutoApprove computerUseTool.appToolApproval
                    }

            approveToolDecisionWithReporter
                (\_ -> modifyIORef' permissionRequests (+ 1)
                    >> pure (Just PermissionAllowOnce))
                (\_ -> pure ())
                policy allowed
                (registry [autoApproveComputer])
                plan call
                `shouldReturn` ToolApprovalGranted

            readIORef permissionRequests `shouldReturn` 1

        it "does not classify, prompt, or persist a catastrophic shell call" do
            policy <- newIORef PromptMutating
            allowed <- newIORef Set.empty
            plan <- newPlanModeEnv (unsafeEncodeUtf "/tmp/approval-test") Nothing
            classifications <- newIORef (0 :: Int)
            permissionRequests <- newIORef (0 :: Int)
            persistenceCalls <- newIORef (0 :: Int)
            notices <- newIORef []
            let call = functionToolCall
                    "call-shell"
                    "shell_command"
                    "{\"command\":\"rm -rf /\"}"
                shellTool = tool "shell_command" $
                    ClassifyReadOnly \_ -> do
                        modifyIORef' classifications (+ 1)
                        pure True

            result <- approveToolDecisionWithReporterAndPersistence
                (\_ -> modifyIORef' permissionRequests (+ 1)
                    >> pure (Just PermissionAllowOnce))
                (\notice -> modifyIORef' notices (<> [notice]))
                (modifyIORef' persistenceCalls (+ 1))
                policy allowed (registry [shellTool]) plan call

            result `shouldSatisfy` \case
                ToolApprovalDenied message -> "Blocked dangerous shell command" `Text.isInfixOf` message
                _ -> False
            readIORef classifications `shouldReturn` 0
            readIORef permissionRequests `shouldReturn` 0
            readIORef persistenceCalls `shouldReturn` 0
            readIORef policy `shouldReturn` PromptMutating
            readIORef allowed `shouldReturn` Set.empty
            recordedNotices <- readIORef notices
            recordedNotices `shouldSatisfy` \case
                [ApprovalWarning message] ->
                    "Blocked dangerous shell command"
                        `Text.isInfixOf` message
                _ -> False

        it "auto-approves sandbox escalation only under full access, including child calls" do
            policy <- newIORef ApproveAll
            allowed <- newIORef (Set.singleton "shell_command")
            plan <- newPlanModeEnv (unsafeEncodeUtf "/approval-test") Nothing
            permissionRequests <- newIORef (0 :: Int)
            let shellTool = tool "shell_command" (ClassifyApproval shellPermissionApproval)
                tools = registry [shellTool]
                call = functionToolCall "escalation" "shell_command"
                    "{\"command\":\"pwd\",\"sandbox_permissions\":\"require_escalated\",\"justification\":\"Inspect directory\"}"
                approve = approveToolDecisionWithReporterAndPersistenceClassified
                    (const (pure (Just True)))
                    (\_ -> modifyIORef' permissionRequests (+ 1) >> pure (Just PermissionAllowOnce))
                    (\_ -> pure ()) (pure ()) policy allowed tools plan call
            approve `shouldReturn` ToolApprovalGranted
            readIORef permissionRequests `shouldReturn` 0
            childApprove ApproveAll tools call `shouldReturn` ToolApprovalGranted
            childApprove PromptMutating tools call `shouldReturn`
                ToolApprovalDenied "Sandbox escalation requires parent approval or --yolo."
            writeIORef policy PromptMutating
            approve `shouldReturn` ToolApprovalGranted
            approve `shouldReturn` ToolApprovalGranted
            readIORef permissionRequests `shouldReturn` 2
            writeIORef policy DenyMutating
            approve `shouldReturn` ToolApprovalRejected
            readIORef permissionRequests `shouldReturn` 2
            writeIORef policy ApproveAll
            activatePlanMode plan
            result <- approve
            result `shouldSatisfy` \case
                ToolApprovalDenied message -> "only editable file" `Text.isInfixOf` message
                _ -> False

        it "does not treat a tool-specific allowance as full access for escalation" do
            policy <- newIORef PromptMutating
            allowed <- newIORef (Set.singleton "shell_command")
            plan <- newPlanModeEnv (unsafeEncodeUtf "/approval-test") Nothing
            permissionRequests <- newIORef (0 :: Int)
            let shellTool = tool "shell_command"
                    (AutoApprove (ClassifyApproval shellPermissionApproval))
                tools = registry [shellTool]
                call = functionToolCall "escalation" "shell_command"
                    "{\"command\":\"pwd\",\"sandbox_permissions\":\"require_escalated\",\"justification\":\"Inspect directory\"}"
            approveToolDecisionWithReporter
                (\_ -> modifyIORef' permissionRequests (+ 1) >> pure (Just PermissionDeny))
                (\_ -> pure ()) policy allowed tools plan call
                `shouldReturn` ToolApprovalRejected
            readIORef permissionRequests `shouldReturn` 1
            childApprove PromptMutating tools call `shouldReturn`
                ToolApprovalDenied "Sandbox escalation requires parent approval or --yolo."

        it "prompts for every explicit-confirmation call under ApproveAll" do
            policy <- newIORef ApproveAll
            allowed <- newIORef (Set.singleton "sensitive")
            plan <- newPlanModeEnv
                (unsafeEncodeUtf "/tmp/approval-test") Nothing
            permissionRequests <- newIORef (0 :: Int)
            let request _ = do
                    modifyIORef' permissionRequests (+ 1)
                    pure (Just PermissionAllowOnce)
                sensitiveTool = tool "sensitive" AlwaysConfirm
                sensitiveCall =
                    functionToolCall "call-sensitive" "sensitive" "{}"
                approve = approveToolDecisionWithReporter
                    request (\_ -> pure ()) policy allowed
                    (registry [sensitiveTool]) plan sensitiveCall
            approve `shouldReturn` ToolApprovalGranted
            approve `shouldReturn` ToolApprovalGranted
            readIORef permissionRequests `shouldReturn` 2
            readIORef policy `shouldReturn` ApproveAll
            readIORef allowed `shouldReturn` Set.singleton "sensitive"

        it "cannot bypass a call-sensitive fresh confirmation with yolo or remembered approval" do
            policy <- newIORef ApproveAll
            allowed <- newIORef (Set.singleton "multiplexer")
            plan <- newPlanModeEnv
                (unsafeEncodeUtf "/tmp/approval-test") Nothing
            permissionRequests <- newIORef (0 :: Int)
            let request _ = do
                    modifyIORef' permissionRequests (+ 1)
                    pure (Just PermissionAllowOnce)
                approve call = approveToolDecisionWithReporter
                    request (\_ -> pure ()) policy allowed
                    (registry [callSensitiveTool]) plan call

            approve callSensitiveReadCall `shouldReturn` ToolApprovalGranted
            approve callSensitiveWriteCall `shouldReturn` ToolApprovalGranted
            approve callSensitiveFreshCall `shouldReturn` ToolApprovalGranted
            approve callSensitiveFreshCall `shouldReturn` ToolApprovalGranted

            writeIORef policy PromptMutating
            approve callSensitiveFreshCall `shouldReturn` ToolApprovalGranted

            readIORef permissionRequests `shouldReturn` 3
            readIORef policy `shouldReturn` PromptMutating
            readIORef allowed `shouldReturn` Set.singleton "multiplexer"

        it "never downgrades wrapped sensitive rules with host auto-approval or native read-only metadata" do
            policy <- newIORef PromptMutating
            allowed <- newIORef (Set.fromList ["sensitive", "multiplexer"])
            plan <- newPlanModeEnv
                (unsafeEncodeUtf "/tmp/approval-test") Nothing
            permissionRequests <- newIORef (0 :: Int)
            let sensitiveTool = tool "sensitive" (AutoApprove AlwaysConfirm)
                wrappedMultiplexer = callSensitiveTool
                    { appToolApproval = AutoApprove callSensitiveTool.appToolApproval }
                sensitiveCall =
                    functionToolCall "call-sensitive" "sensitive" "{}"
                tools = registry [sensitiveTool, wrappedMultiplexer]
                approve = approveToolDecisionWithReporterAndPersistenceClassified
                    (const (pure (Just True)))
                    (\_ -> modifyIORef' permissionRequests (+ 1)
                        >> pure (Just PermissionAllowOnce))
                    (\_ -> pure ())
                    (pure ())
                    policy allowed tools plan
            approve sensitiveCall `shouldReturn` ToolApprovalGranted
            approve callSensitiveFreshCall `shouldReturn` ToolApprovalGranted
            readIORef permissionRequests `shouldReturn` 2
            readIORef policy `shouldReturn` PromptMutating
            childApprove ApproveAll tools sensitiveCall
                `shouldReturn` ToolApprovalDenied
                    "This sensitive tool requires an explicit parent approval for every call."
            childApprove ApproveAll tools callSensitiveFreshCall
                `shouldReturn` ToolApprovalDenied
                    "This sensitive tool requires an explicit parent approval for every call."

        it "reports plan-mode denials without requiring terminal output" do
            policy <- newIORef ApproveAll
            allowed <- newIORef Set.empty
            plan <- newPlanModeEnv (unsafeEncodeUtf "/tmp/approval-test") Nothing
            activatePlanMode plan
            notices <- newIORef []
            permissionRequests <- newIORef (0 :: Int)
            let call = functionToolCall "call-patch" "apply_patch" "{}"

            result <- approveToolDecisionWithReporter
                (\_ -> modifyIORef' permissionRequests (+ 1)
                    >> pure (Just PermissionAllowOnce))
                (\notice -> modifyIORef' notices (<> [notice]))
                policy
                allowed
                (registry [])
                plan
                call

            result `shouldSatisfy` \case
                ToolApprovalDenied message ->
                    "file edits are not allowed in plan mode"
                        `Text.isInfixOf` message
                _ -> False
            readIORef permissionRequests `shouldReturn` 0
            readIORef notices `shouldReturn`
                [ApprovalWarning
                    "Rejected: file edits are not allowed in plan mode - \
                    \the only editable file is the plan file \
                    \(/tmp/approval-test/plan.md)."]

        it "reports dangerous shell denials through the callback" do
            policy <- newIORef ApproveAll
            allowed <- newIORef Set.empty
            plan <- newPlanModeEnv (unsafeEncodeUtf "/tmp/approval-test") Nothing
            notices <- newIORef []
            let call =
                    functionToolCall
                        "call-shell"
                        "shell_command"
                        "{\"command\":\"rm -rf /\"}"

            result <- approveToolDecisionWithReporter
                (\_ -> pure (Just PermissionAllowOnce))
                (\notice -> modifyIORef' notices (<> [notice]))
                policy
                allowed
                (registry [])
                plan
                call

            result `shouldSatisfy` \case
                ToolApprovalDenied{} -> True
                _ -> False
            recorded <- readIORef notices
            recorded `shouldSatisfy` \case
                [ApprovalWarning message] ->
                    "blocked dangerous shell command"
                        `Text.isInfixOf` Text.toLower message
                _ -> False

        it "hard-denies dangerous commands through the public Grok shell alias" do
            policy <- newIORef ApproveAll
            allowed <- newIORef Set.empty
            plan <- newPlanModeEnv (unsafeEncodeUtf "/tmp/approval-test") Nothing
            let call = functionToolCall
                    "call-grok-shell"
                    "run_terminal_command"
                    "{\"command\":\"rm -rf /\"}"
            result <- approveToolDecisionWithReporter
                (\_ -> pure (Just PermissionAllowOnce))
                (\_ -> pure ())
                policy allowed (registry []) plan call
            result `shouldSatisfy` \case
                ToolApprovalDenied message -> "Blocked dangerous shell command" `Text.isInfixOf` message
                _ -> False

        it "rejects shell inspection in plan mode in favor of dedicated read tools" do
            policy <- newIORef ApproveAll
            allowed <- newIORef Set.empty
            plan <- newPlanModeEnv (unsafeEncodeUtf "/tmp/approval-test") Nothing
            activatePlanMode plan
            let call = functionToolCall "call-shell" "shell_command"
                    "{\"command\":\"rg -n plan packages | head -20\"}"
            result <- approveToolDecisionWithReporter
                (\_ -> pure (Just PermissionAllowOnce))
                (\_ -> pure ())
                policy allowed (registry []) plan call
            result `shouldSatisfy` \case
                ToolApprovalDenied message -> "only editable file" `Text.isInfixOf` message
                _ -> False

        it "rejects the public Grok shell alias in plan mode" do
            policy <- newIORef ApproveAll
            allowed <- newIORef Set.empty
            plan <- newPlanModeEnv (unsafeEncodeUtf "/tmp/approval-test") Nothing
            activatePlanMode plan
            let call = functionToolCall
                    "call-grok-shell"
                    "run_terminal_command"
                    "{\"command\":\"git status\"}"
            result <- approveToolDecisionWithReporter
                (\_ -> pure (Just PermissionAllowOnce))
                (\_ -> pure ())
                policy allowed (registry []) plan call
            result `shouldSatisfy` \case
                ToolApprovalDenied message -> "only editable file" `Text.isInfixOf` message
                _ -> False

        it "rejects shell writes in plan mode even under yolo" do
            policy <- newIORef ApproveAll
            allowed <- newIORef Set.empty
            plan <- newPlanModeEnv (unsafeEncodeUtf "/tmp/approval-test") Nothing
            activatePlanMode plan
            let call = functionToolCall "call-shell" "shell_command"
                    "{\"command\":\"printf x > plan.md\"}"
            result <- approveToolDecisionWithReporter
                (\_ -> pure (Just PermissionAllowOnce))
                (\_ -> pure ())
                policy allowed (registry []) plan call
            result `shouldSatisfy` \case
                ToolApprovalDenied message -> "only editable file" `Text.isInfixOf` message
                _ -> False

        it "auto-approves the path-locked write_plan tool" do
            policy <- newIORef PromptMutating
            allowed <- newIORef Set.empty
            plan <- newPlanModeEnv (unsafeEncodeUtf "/tmp/approval-test") Nothing
            activatePlanMode plan
            permissionRequests <- newIORef (0 :: Int)
            let call = functionToolCall "call-write-plan" "write_plan"
                    "{\"content\":\"# Plan\"}"
            approveToolDecisionWithReporter
                (\_ -> modifyIORef' permissionRequests (+ 1)
                    >> pure (Just PermissionAllowOnce))
                (\_ -> pure ())
                policy allowed (registry [writePlanSafeTool]) plan call
                `shouldReturn` ToolApprovalGranted
            readIORef permissionRequests `shouldReturn` 0

        it "rejects every other mutating tool in plan mode even under yolo" do
            policy <- newIORef ApproveAll
            allowed <- newIORef Set.empty
            plan <- newPlanModeEnv (unsafeEncodeUtf "/tmp/approval-test") Nothing
            activatePlanMode plan
            result <- approveToolDecisionWithReporter
                (\_ -> pure (Just PermissionAllowOnce))
                (\_ -> pure ())
                policy allowed (registry [mutatingTool]) plan mutatingCall
            result `shouldSatisfy` \case
                ToolApprovalDenied message -> "only editable file" `Text.isInfixOf` message
                _ -> False

        it "reports remembered tool approval through the callback" do
            policy <- newIORef PromptMutating
            allowed <- newIORef Set.empty
            plan <- newPlanModeEnv (unsafeEncodeUtf "/tmp/approval-test") Nothing
            notices <- newIORef []
            permissionRequests <- newIORef (0 :: Int)
            let request _ = do
                    modifyIORef' permissionRequests (+ 1)
                    pure (Just PermissionAllowTool)
                report notice = do
                    readIORef allowed
                        `shouldReturn` Set.singleton "write"
                    modifyIORef' notices (<> [notice])

            approveToolDecisionWithReporter
                request report policy allowed
                (registry [mutatingTool]) plan mutatingCall
                `shouldReturn` ToolApprovalGranted
            approveToolDecisionWithReporter
                request report policy allowed
                (registry [mutatingTool]) plan mutatingCall
                `shouldReturn` ToolApprovalGranted

            readIORef permissionRequests `shouldReturn` 1
            readIORef notices `shouldReturn`
                [ApprovalSuccess "✓ always allow write this session"]
            readIORef allowed `shouldReturn` Set.singleton "write"

        it "enables and persists project-wide auto-approval from a prompt" do
            policy <- newIORef PromptMutating
            allowed <- newIORef Set.empty
            plan <- newPlanModeEnv (unsafeEncodeUtf "/tmp/approval-test") Nothing
            notices <- newIORef []
            persisted <- newIORef (0 :: Int)
            events <- newIORef ([] :: [Text])
            let persist = do
                    readIORef policy `shouldReturn` ApproveAll
                    modifyIORef' persisted (+ 1)
                    modifyIORef' events (<> ["persist"])
                report notice = do
                    readIORef events `shouldReturn` ["persist"]
                    modifyIORef' events (<> ["report"])
                    modifyIORef' notices (<> [notice])

            approveToolDecisionWithReporterAndPersistence
                (\_ -> pure (Just PermissionAllowAll))
                report
                persist
                policy allowed (registry [mutatingTool]) plan mutatingCall
                `shouldReturn` ToolApprovalGranted

            readIORef policy `shouldReturn` ApproveAll
            readIORef persisted `shouldReturn` 1
            readIORef notices `shouldReturn`
                [ApprovalSuccess "✓ auto-approve on (saved for project)"]
            readIORef events `shouldReturn` ["persist", "report"]

        it "does not mutate or report when a prompt is denied" do
            policy <- newIORef PromptMutating
            allowed <- newIORef Set.empty
            plan <- newPlanModeEnv (unsafeEncodeUtf "/tmp/approval-test") Nothing
            notices <- newIORef []
            persisted <- newIORef (0 :: Int)

            approveToolDecisionWithReporterAndPersistence
                (\_ -> pure (Just PermissionDeny))
                (\notice -> modifyIORef' notices (<> [notice]))
                (modifyIORef' persisted (+ 1))
                policy allowed (registry [mutatingTool]) plan mutatingCall
                `shouldReturn` ToolApprovalRejected

            readIORef policy `shouldReturn` PromptMutating
            readIORef allowed `shouldReturn` Set.empty
            readIORef notices `shouldReturn` []
            readIORef persisted `shouldReturn` 0

        it "shares remembered approval across public and internal Grok aliases" do
            policy <- newIORef PromptMutating
            allowed <- newIORef Set.empty
            plan <- newPlanModeEnv (unsafeEncodeUtf "/tmp/approval-test") Nothing
            permissionRequests <- newIORef (0 :: Int)
            let request _ = do
                    modifyIORef' permissionRequests (+ 1)
                    pure (Just PermissionAllowTool)
                publicCall = functionToolCall
                    "call-public"
                    "run_terminal_command"
                    "{\"command\":\"git status\"}"
                internalCall = functionToolCall
                    "call-internal"
                    "run_terminal_cmd"
                    "{\"command\":\"git status\"}"
                tools = registry [tool "run_terminal_cmd" AlwaysPrompt]
            approveToolDecisionWithReporter
                request (\_ -> pure ()) policy allowed tools plan publicCall
                `shouldReturn` ToolApprovalGranted
            approveToolDecisionWithReporter
                request (\_ -> pure ()) policy allowed tools plan internalCall
                `shouldReturn` ToolApprovalGranted
            readIORef permissionRequests `shouldReturn` 1
            readIORef allowed `shouldReturn` Set.singleton "run_terminal_cmd"

        it "prompts once for a computer-use workflow even under ApproveAll" do
            policy <- newIORef ApproveAll
            allowed <- newIORef Set.empty
            plan <- newPlanModeEnv
                (unsafeEncodeUtf "/tmp/approval-test") Nothing
            permissionRequests <- newIORef (0 :: Int)
            notices <- newIORef []
            let request _ = do
                    modifyIORef' permissionRequests (+ 1)
                    pure (Just PermissionAllowTool)
                computerCall kind = ToolCall
                    { callId = "computer-1"
                    , name = computerToolName
                    , arguments = "{}"
                    , callKind = kind
                    , argumentsEncrypted = False
                    }
                approve kind = approveToolDecisionWithReporter
                    request
                    (\notice -> modifyIORef' notices (<> [notice]))
                    policy allowed
                    (registry [computerUseTool]) plan (computerCall kind)
            mapM_ (\kind -> do
                approve kind `shouldReturn` ToolApprovalGranted
                approve kind `shouldReturn` ToolApprovalGranted)
                [ComputerCallKind, ComputerFunctionCallKind]
            readIORef permissionRequests `shouldReturn` 1
            readIORef policy `shouldReturn` ApproveAll
            readIORef allowed `shouldReturn` Set.singleton computerToolName
            readIORef notices `shouldReturn`
                [ApprovalSuccess
                    "✓ computer use approved until disabled"]

        it "never lets a child bypass explicit confirmation under ApproveAll" do
            let sensitiveTool = tool "sensitive" AlwaysConfirm
                sensitiveCall =
                    functionToolCall "call-sensitive" "sensitive" "{}"
            childApprove ApproveAll
                (registry [sensitiveTool]) sensitiveCall
                `shouldReturn`
                    ToolApprovalDenied
                        "This sensitive tool requires an explicit parent approval for every call."

        it "rejects only fresh calls from a call-sensitive child tool" do
            childApprove ApproveAll
                (registry [callSensitiveTool]) callSensitiveFreshCall
                `shouldReturn`
                    ToolApprovalDenied
                        "This sensitive tool requires an explicit parent approval for every call."
            childApprove ApproveAll
                (registry [callSensitiveTool]) callSensitiveWriteCall
                `shouldReturn` ToolApprovalGranted
            childApprove DenyMutating
                (registry [callSensitiveTool]) callSensitiveReadCall
                `shouldReturn` ToolApprovalGranted

        it "prompts for each computer call after an Allow once choice" do
            policy <- newIORef ApproveAll
            allowed <- newIORef Set.empty
            plan <- newPlanModeEnv
                (unsafeEncodeUtf "/tmp/approval-test") Nothing
            permissionRequests <- newIORef (0 :: Int)
            notices <- newIORef []
            let request _ = do
                    modifyIORef' permissionRequests (+ 1)
                    pure (Just PermissionAllowOnce)
                call = ToolCall
                    { callId = "computer-1"
                    , name = computerToolName
                    , arguments = "{}"
                    , callKind = ComputerCallKind
                    , argumentsEncrypted = False
                    }
                approve = approveToolDecisionWithReporter
                    request
                    (\notice -> modifyIORef' notices (<> [notice]))
                    policy allowed
                    (registry [computerUseTool]) plan call
            approve `shouldReturn` ToolApprovalGranted
            approve `shouldReturn` ToolApprovalGranted
            readIORef permissionRequests `shouldReturn` 2
            readIORef allowed `shouldReturn` Set.empty
            readIORef notices `shouldReturn` []

        it "prompts again when a computer call introduces safety checks" do
            policy <- newIORef ApproveAll
            allowed <- newIORef (Set.singleton computerToolName)
            plan <- newPlanModeEnv
                (unsafeEncodeUtf "/tmp/approval-test") Nothing
            permissionRequests <- newIORef (0 :: Int)
            let call arguments = ToolCall
                    { callId = "computer-1"
                    , name = computerToolName
                    , arguments
                    , callKind = ComputerCallKind
                    , argumentsEncrypted = False
                    }
                approve computerCall = approveToolDecisionWithReporter
                    (\_ -> modifyIORef' permissionRequests (+ 1)
                        >> pure (Just PermissionAllowOnce))
                    (\_ -> pure ())
                    policy allowed
                    (registry [computerUseTool])
                    plan computerCall
            approve (call "{\"actions\":[{\"type\":\"screenshot\"}]}")
                `shouldReturn` ToolApprovalGranted
            readIORef permissionRequests `shouldReturn` 0
            approve
                (call
                    "{\"actions\":[{\"type\":\"screenshot\"}],\
                    \\"pending_safety_checks\":[{\"id\":\"check-1\"}]}")
                `shouldReturn` ToolApprovalGranted
            readIORef permissionRequests `shouldReturn` 1

        it "prompts again after the computer-use workflow grant is cleared" do
            policy <- newIORef PromptMutating
            allowed <- newIORef Set.empty
            plan <- newPlanModeEnv
                (unsafeEncodeUtf "/tmp/approval-test") Nothing
            permissionRequests <- newIORef (0 :: Int)
            let request _ = do
                    modifyIORef' permissionRequests (+ 1)
                    pure (Just PermissionAllowTool)
                computerCall kind = ToolCall
                    { callId = "computer-1"
                    , name = computerToolName
                    , arguments = "{}"
                    , callKind = kind
                    , argumentsEncrypted = False
                    }
                approve kind = approveToolDecisionWithReporter
                    request (\_ -> pure ()) policy allowed
                    (registry [computerUseTool]) plan (computerCall kind)
            approve ComputerCallKind `shouldReturn` ToolApprovalGranted
            approve ComputerCallKind `shouldReturn` ToolApprovalGranted
            readIORef permissionRequests `shouldReturn` 1
            modifyIORef' allowed (Set.delete computerToolName)
            approve ComputerFunctionCallKind `shouldReturn` ToolApprovalGranted
            approve ComputerFunctionCallKind `shouldReturn` ToolApprovalGranted
            readIORef permissionRequests `shouldReturn` 2
            readIORef allowed `shouldReturn` Set.singleton computerToolName

        it "rejects spoofed function/custom computer calls under ApproveAll" do
            policy <- newIORef ApproveAll
            allowed <- newIORef (Set.singleton computerToolName)
            plan <- newPlanModeEnv
                (unsafeEncodeUtf "/tmp/approval-test") Nothing
            permissionRequests <- newIORef (0 :: Int)
            let spoof kind = ToolCall
                    { callId = "spoof-1"
                    , name = computerToolName
                    , arguments = "{}"
                    , callKind = kind
                    , argumentsEncrypted = False
                    }
                approve call = approveToolDecisionWithReporter
                    (\_ -> modifyIORef' permissionRequests (+ 1)
                        >> pure (Just PermissionAllowOnce))
                    (\_ -> pure ())
                    policy allowed
                    (registry [computerUseTool])
                    plan call
            functionResult <- approve (spoof FunctionCallKind)
            customResult <- approve (spoof CustomCallKind)
            functionResult `shouldSatisfy` \case
                ToolApprovalDenied message -> "mismatched provider-native" `Text.isInfixOf` message
                _ -> False
            customResult `shouldSatisfy` \case
                ToolApprovalDenied message -> "mismatched provider-native" `Text.isInfixOf` message
                _ -> False
            readIORef permissionRequests `shouldReturn` 0

    describe "childApprove" do
        it "allows every known tool under ApproveAll" do
            childApprove ApproveAll (registry [mutatingTool]) mutatingCall
                `shouldReturn` ToolApprovalGranted

        it "never lets a child bypass computer approval" do
            let computerCall kind = ToolCall
                    { callId = "computer-1"
                    , name = "computer"
                    , arguments = "{}"
                    , callKind = kind
                    , argumentsEncrypted = False
                    }
            mapM_ (\kind ->
                childApprove ApproveAll
                    (registry [mutatingTool])
                    (computerCall kind)
                    `shouldReturn`
                        ToolApprovalDenied
                            "Computer use must be approved in the interactive parent session.")
                [ComputerCallKind, ComputerFunctionCallKind]

        it "allows only read-only tools under DenyMutating" do
            childApprove DenyMutating (registry [readOnlyTool]) readOnlyCall
                `shouldReturn` ToolApprovalGranted
            childApprove DenyMutating (registry [mutatingTool]) mutatingCall
                `shouldReturn` ToolApprovalRejected

        it "recognizes namespaced collaboration tools as read-only" do
            childApprove DenyMutating (registry [namespacedReadOnlyTool]) namespacedReadOnlyCall
                `shouldReturn` ToolApprovalGranted

        it "returns an in-band denial when a child would need to prompt" do
            result <- childApprove PromptMutating (registry [mutatingTool]) mutatingCall
            result `shouldSatisfy` \case
                ToolApprovalDenied message -> "cannot prompt for approval" `Text.isInfixOf` message
                _ -> False

        it "honors scoped auto-approval without weakening read-only mode" do
            let tools = registry [autoApproveMutatingTool]
            childApprove PromptMutating tools mutatingCall
                `shouldReturn` ToolApprovalGranted
            childApprove DenyMutating tools mutatingCall
                `shouldReturn` ToolApprovalRejected

        it "honors per-call read-only classifiers" do
            childApprove DenyMutating (registry [dynamicTool]) dynamicReadCall
                `shouldReturn` ToolApprovalGranted
            childApprove DenyMutating (registry [dynamicTool]) dynamicWriteCall
                `shouldReturn` ToolApprovalRejected

readOnlyCall :: ToolCall
readOnlyCall = functionToolCall "call-read" "read" "{}"

mutatingCall :: ToolCall
mutatingCall = functionToolCall "call-write" "write" "{}"

dynamicReadCall :: ToolCall
dynamicReadCall = functionToolCall "call-dynamic-read" "dynamic" "read"

dynamicWriteCall :: ToolCall
dynamicWriteCall = functionToolCall "call-dynamic-write" "dynamic" "write"

callSensitiveReadCall :: ToolCall
callSensitiveReadCall =
    functionToolCall "call-multiplexer-read" "multiplexer" "read"

callSensitiveWriteCall :: ToolCall
callSensitiveWriteCall =
    functionToolCall "call-multiplexer-write" "multiplexer" "write"

callSensitiveFreshCall :: ToolCall
callSensitiveFreshCall =
    functionToolCall "call-multiplexer-fresh" "multiplexer" "fresh"

namespacedReadOnlyCall :: ToolCall
namespacedReadOnlyCall =
    functionToolCall "call-list-agents" "collaboration.list_agents" "{}"

readOnlyTool :: AppTool
readOnlyTool = tool "read" AlwaysReadOnly

mutatingTool :: AppTool
mutatingTool = tool "write" AlwaysPrompt

autoApproveMutatingTool :: AppTool
autoApproveMutatingTool = tool "write" (AutoApprove AlwaysPrompt)

dynamicTool :: AppTool
dynamicTool = tool "dynamic" (ClassifyReadOnly (\call -> pure (call == dynamicReadCall)))

callSensitiveTool :: AppTool
callSensitiveTool = tool "multiplexer" $
    ClassifyApproval \call -> pure $ case call.arguments of
        "read" -> ApprovalNotRequired
        "fresh" -> FreshApprovalRequired
        _ -> ApprovalPromptRequired

namespacedReadOnlyTool :: AppTool
namespacedReadOnlyTool = tool "list_agents" AlwaysReadOnly

writePlanSafeTool :: AppTool
writePlanSafeTool = tool "write_plan" AlwaysReadOnly

tool :: Text -> ApprovalRule -> AppTool
tool name approval =
    jsonAppTool
        name "" [] approval
        (noArgsTool name (pure (Right "ok")))

registry :: [AppTool] -> ToolRegistry
registry = either (error . Text.unpack) id . mkToolRegistry

approvalFacts :: ToolCall -> ApprovalFacts
approvalFacts call = ApprovalFacts
    { policy = PromptMutating
    , planActive = False
    , planPath = unsafeEncodeUtf "/tmp/approval-test/plan.md"
    , readOnly = Nothing
    , allowedForSession = Nothing
    , requiresExplicitApproval = False
    , call
    }
