module Agent.CLI.PermissionSpec (spec) where

import Agent.CLI.Permission
import Agent.CLI.Picker (PickerKey(..))
import Agent.CLI.Options (ApprovalPolicy(..))
import Agent.ToolDispatch (ToolCall(..), ToolCallKind(..))
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = do
    describe "applyPermissionKey" do
        let state = initialPermissionState "search_replace src/A.hs"

        it "confirms the selected row" do
            applyPermissionKey PickerKeyConfirm state
                `shouldBe` Left PermissionAllowOnce

        it "moves down to always-this-tool" do
            case applyPermissionKey PickerKeyDown state of
                Right global ->
                    case applyPermissionKey PickerKeyDown global of
                        Right tool ->
                            applyPermissionKey PickerKeyConfirm tool
                                `shouldBe` Left PermissionAllowTool
                        Left choice ->
                            expectationFailure
                                ("expected second navigation, got " <> show choice)
                Left choice ->
                    expectationFailure ("expected navigation, got " <> show choice)

        it "moves down once to project-wide auto-approval" do
            case applyPermissionKey PickerKeyDown state of
                Right down ->
                    applyPermissionKey PickerKeyConfirm down
                        `shouldBe` Left PermissionAllowAll
                Left choice ->
                    expectationFailure ("expected navigation, got " <> show choice)

        it "maps y / A / a / n shortcuts" do
            applyPermissionKey (PickerKeyChar 'y') state
                `shouldBe` Left PermissionAllowOnce
            applyPermissionKey (PickerKeyChar 'A') state
                `shouldBe` Left PermissionAllowAll
            applyPermissionKey (PickerKeyChar 'a') state
                `shouldBe` Left PermissionAllowTool
            applyPermissionKey (PickerKeyChar 'n') state
                `shouldBe` Left PermissionDeny

        it "cancels with esc" do
            applyPermissionKey PickerKeyCancel state
                `shouldBe` Left PermissionDeny

    describe "renderPermissionFrame" do
        it "names the tool and the four choices" do
            let frame =
                    renderPermissionFrame False
                        (initialPermissionState "Allow search_replace src/A.hs?")
            frame `shouldSatisfy`
                Text.isInfixOf "Allow search_replace src/A.hs?"
            frame `shouldSatisfy` Text.isInfixOf "Allow once"
            frame `shouldSatisfy`
                Text.isInfixOf "Always approve all tools for this project"
            frame `shouldSatisfy` Text.isInfixOf "Always allow this tool this session"
            frame `shouldSatisfy` Text.isInfixOf "Deny"

    describe "approvalToolCallPrompt" do
        it "summarizes privileged computer calls without dumping JSON" do
            let call = ToolCall
                    { callId = "computer-1"
                    , name = "computer"
                    , arguments =
                        "{\"actions\":[{\"type\":\"screenshot\"}]}"
                    , callKind = ComputerCallKind
                    , argumentsEncrypted = False
                    }
                prompt = approvalToolCallPromptRelative "/repo" call
            prompt `shouldSatisfy`
                Text.isPrefixOf
                    "Allow this computer-use workflow until disabled?"
            prompt `shouldNotSatisfy` Text.isInfixOf "\"actions\""

    describe "approval policy picker" do
        it "selects the current policy" do
            (initialApprovalPolicyState ApproveAll).approvalPolicyIndex
                `shouldBe` 2
            (initialApprovalPolicyState DenyMutating).approvalPolicyIndex
                `shouldBe` 1

        it "maps shortcuts and confirmation" do
            let state = initialApprovalPolicyState PromptMutating
            applyApprovalPolicyKey (PickerKeyChar 'f') state
                `shouldBe` Left (ApprovalPolicySelected ApproveAll)
            applyApprovalPolicyKey PickerKeyDown state
                `shouldBe` Right (ApprovalPolicyState PromptMutating 1)
            applyApprovalPolicyKey PickerKeyConfirm
                (ApprovalPolicyState PromptMutating 1)
                `shouldBe` Left (ApprovalPolicySelected DenyMutating)


        it "cancels without changing the current policy" do
            applyApprovalPolicyKey PickerKeyCancel
                (initialApprovalPolicyState DenyMutating)
                `shouldBe` Left ApprovalPolicyCancelled
