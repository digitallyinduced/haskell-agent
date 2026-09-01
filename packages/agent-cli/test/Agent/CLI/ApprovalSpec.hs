module Agent.CLI.ApprovalSpec (spec) where

import Agent.CLI.Approval
    ( ApprovalAction(..)
    , ApprovalFacts(..)
    , ApprovalNotice(..)
    , ApprovalPlan(..)
    , approveFilesystemRootAccess
    , approveToolDecisionWithReporter
    , approveToolDecisionWithReporterAndPersistence
    , childApprove
    , planApproval
    , resolveApprovalPrompt
    )
import Agent.CLI.Options (ApprovalPolicy(..))
import Agent.CLI.ComputerUse (computerUseTool)
import Agent.CLI.Permission (PermissionChoice(..))
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallKind(..)
    , functionToolCall
    , noArgsTool
    )
import Agent.Tools.Types
    ( AppTool
    , ApprovalRule(..)
    , ToolRegistry
    , jsonAppTool
    , mkToolRegistry
    )
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
                        (Left message)
                        [ReportApprovalNotice (ApprovalWarning notice)] ->
                            "Blocked dangerous shell command"
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
                        (Left message)
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
                    (Left message)
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
                `shouldBe` CompleteApproval (Right True) []

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
                `shouldBe` CompleteApproval (Right True) []
            planApproval (facts (searchReplace "src/Main.hs"))
                `shouldSatisfy` \case
                    CompleteApproval (Left _) [_] -> True
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
                CompleteApproval (Left _) [_] -> True
                _ -> False

        it "applies the session policy after remembered-tool approval" do
            let classified = (approvalFacts mutatingCall)
                    { readOnly = Just False
                    }
            planApproval (classified { allowedForSession = Just True })
                `shouldBe` CompleteApproval (Right True) []
            planApproval (classified
                { policy = ApproveAll
                , allowedForSession = Just False
                })
                `shouldBe` CompleteApproval (Right True) []
            planApproval (classified
                { policy = DenyMutating
                , allowedForSession = Just False
                })
                `shouldBe` CompleteApproval (Right False) []

        it "auto-approves read-only calls under restrictive policies" do
            let facts policy = (approvalFacts readOnlyCall)
                    { policy
                    , readOnly = Just True
                    , allowedForSession = Just False
                    }
            planApproval (facts DenyMutating)
                `shouldBe` CompleteApproval (Right True) []
            planApproval (facts PromptMutating)
                `shouldBe` CompleteApproval (Right True) []

    describe "resolveApprovalPrompt" do
        it "maps cancellation and explicit denial to an in-band denial" do
            resolveApprovalPrompt mutatingCall Nothing
                `shouldBe` CompleteApproval (Right False) []
            resolveApprovalPrompt mutatingCall (Just PermissionDeny)
                `shouldBe` CompleteApproval (Right False) []

        it "allows once without changing approval state" do
            resolveApprovalPrompt mutatingCall (Just PermissionAllowOnce)
                `shouldBe` CompleteApproval (Right True) []

        it "plans project persistence after enabling auto-approval" do
            resolveApprovalPrompt mutatingCall (Just PermissionAllowAll)
                `shouldBe` CompleteApproval
                    (Right True)
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
                    (Right True)
                    [ RememberToolForSession "run_terminal_cmd"
                    , ReportApprovalNotice
                        (ApprovalSuccess
                            "✓ always allow run_terminal_command this session")
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

            result `shouldSatisfy` either
                (Text.isInfixOf "Blocked dangerous shell command")
                (const False)
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
                Left message ->
                    "file edits are not allowed in plan mode"
                        `Text.isInfixOf` message
                Right _ -> False
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

            result `shouldSatisfy` either (const True) (const False)
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
            result `shouldSatisfy` either
                (Text.isInfixOf "Blocked dangerous shell command")
                (const False)

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
            result `shouldSatisfy` either
                (Text.isInfixOf "only editable file")
                (const False)

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
            result `shouldSatisfy` either
                (Text.isInfixOf "only editable file")
                (const False)

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
            result `shouldSatisfy` either
                (Text.isInfixOf "only editable file")
                (const False)

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
                `shouldReturn` Right True
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
            result `shouldSatisfy` either
                (Text.isInfixOf "only editable file")
                (const False)

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
                `shouldReturn` Right True
            approveToolDecisionWithReporter
                request report policy allowed
                (registry [mutatingTool]) plan mutatingCall
                `shouldReturn` Right True

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
                `shouldReturn` Right True

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
                `shouldReturn` Right False

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
                `shouldReturn` Right True
            approveToolDecisionWithReporter
                request (\_ -> pure ()) policy allowed tools plan internalCall
                `shouldReturn` Right True
            readIORef permissionRequests `shouldReturn` 1
            readIORef allowed `shouldReturn` Set.singleton "run_terminal_cmd"

        it "prompts for every computer call even under ApproveAll" do
            policy <- newIORef ApproveAll
            allowed <- newIORef Set.empty
            plan <- newPlanModeEnv
                (unsafeEncodeUtf "/tmp/approval-test") Nothing
            permissionRequests <- newIORef (0 :: Int)
            let request _ = do
                    modifyIORef' permissionRequests (+ 1)
                    pure (Just PermissionAllowTool)
                computerCall kind = ToolCall
                    { callId = "computer-1"
                    , name = "computer"
                    , arguments = "{}"
                    , callKind = kind
                    , argumentsEncrypted = False
                    }
                approve kind = approveToolDecisionWithReporter
                    request (\_ -> pure ()) policy allowed
                    (registry [mutatingTool]) plan (computerCall kind)
            mapM_ (\kind -> do
                approve kind `shouldReturn` Right True
                approve kind `shouldReturn` Right True)
                [ComputerCallKind, ComputerFunctionCallKind]
            readIORef permissionRequests `shouldReturn` 4
            readIORef policy `shouldReturn` ApproveAll
            readIORef allowed `shouldReturn` Set.empty

        it "does not cache allow-tool for computer calls" do
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
                    , name = "computer"
                    , arguments = "{}"
                    , callKind = kind
                    , argumentsEncrypted = False
                    }
                approve kind = approveToolDecisionWithReporter
                    request (\_ -> pure ()) policy allowed
                    (registry [mutatingTool]) plan (computerCall kind)
            mapM_ (\kind -> do
                approve kind `shouldReturn` Right True
                approve kind `shouldReturn` Right True)
                [ComputerCallKind, ComputerFunctionCallKind]
            readIORef permissionRequests `shouldReturn` 4
            readIORef allowed `shouldReturn` Set.empty

        it "rejects spoofed function/custom computer calls under ApproveAll" do
            policy <- newIORef ApproveAll
            allowed <- newIORef (Set.singleton "computer")
            plan <- newPlanModeEnv
                (unsafeEncodeUtf "/tmp/approval-test") Nothing
            permissionRequests <- newIORef (0 :: Int)
            let spoof kind = ToolCall
                    { callId = "spoof-1"
                    , name = "computer"
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
            functionResult `shouldSatisfy` either
                (Text.isInfixOf "mismatched provider-native")
                (const False)
            customResult `shouldSatisfy` either
                (Text.isInfixOf "mismatched provider-native")
                (const False)
            readIORef permissionRequests `shouldReturn` 0

    describe "childApprove" do
        it "allows every known tool under ApproveAll" do
            childApprove ApproveAll (registry [mutatingTool]) mutatingCall
                `shouldReturn` Right True

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
                        Left
                            "Computer use requires an explicit parent approval for every call.")
                [ComputerCallKind, ComputerFunctionCallKind]

        it "allows only read-only tools under DenyMutating" do
            childApprove DenyMutating (registry [readOnlyTool]) readOnlyCall
                `shouldReturn` Right True
            childApprove DenyMutating (registry [mutatingTool]) mutatingCall
                `shouldReturn` Right False

        it "recognizes namespaced collaboration tools as read-only" do
            childApprove DenyMutating (registry [namespacedReadOnlyTool]) namespacedReadOnlyCall
                `shouldReturn` Right True

        it "returns an in-band denial when a child would need to prompt" do
            result <- childApprove PromptMutating (registry [mutatingTool]) mutatingCall
            result `shouldSatisfy` \case
                Left message -> "cannot prompt for approval" `Text.isInfixOf` message
                Right _ -> False

        it "honors per-call read-only classifiers" do
            childApprove DenyMutating (registry [dynamicTool]) dynamicReadCall
                `shouldReturn` Right True
            childApprove DenyMutating (registry [dynamicTool]) dynamicWriteCall
                `shouldReturn` Right False

readOnlyCall :: ToolCall
readOnlyCall = functionToolCall "call-read" "read" "{}"

mutatingCall :: ToolCall
mutatingCall = functionToolCall "call-write" "write" "{}"

dynamicReadCall :: ToolCall
dynamicReadCall = functionToolCall "call-dynamic-read" "dynamic" "read"

dynamicWriteCall :: ToolCall
dynamicWriteCall = functionToolCall "call-dynamic-write" "dynamic" "write"

namespacedReadOnlyCall :: ToolCall
namespacedReadOnlyCall =
    functionToolCall "call-list-agents" "collaboration.list_agents" "{}"

readOnlyTool :: AppTool
readOnlyTool = tool "read" AlwaysReadOnly

mutatingTool :: AppTool
mutatingTool = tool "write" AlwaysPrompt

dynamicTool :: AppTool
dynamicTool = tool "dynamic" (ClassifyReadOnly (\call -> pure (call == dynamicReadCall)))

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
    , call
    }
