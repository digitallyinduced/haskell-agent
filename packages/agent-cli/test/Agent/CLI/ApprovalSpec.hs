module Agent.CLI.ApprovalSpec (spec) where

import Agent.CLI.Approval
    ( ApprovalNotice(..)
    , approveToolDecisionWithReporter
    , childApprove
    )
import Agent.CLI.Options (ApprovalPolicy(..))
import Agent.CLI.Permission (PermissionChoice(..))
import Agent.ToolDispatch
    ( ToolCall
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
    )
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import System.OsPath (unsafeEncodeUtf)
import Test.Hspec

spec :: Spec
spec = do
    describe "approveToolDecisionWith" do
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

        it "reports remembered tool approval through the callback" do
            policy <- newIORef PromptMutating
            allowed <- newIORef Set.empty
            plan <- newPlanModeEnv (unsafeEncodeUtf "/tmp/approval-test") Nothing
            notices <- newIORef []
            permissionRequests <- newIORef (0 :: Int)
            let request _ = do
                    modifyIORef' permissionRequests (+ 1)
                    pure (Just PermissionAllowTool)
                report notice = modifyIORef' notices (<> [notice])

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

    describe "childApprove" do
        it "allows every known tool under ApproveAll" do
            childApprove ApproveAll (registry [mutatingTool]) mutatingCall
                `shouldReturn` Right True

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

tool :: Text -> ApprovalRule -> AppTool
tool name approval =
    jsonAppTool
        name "" [] approval
        (noArgsTool name (pure (Right "ok")))

registry :: [AppTool] -> ToolRegistry
registry = either (error . Text.unpack) id . mkToolRegistry
