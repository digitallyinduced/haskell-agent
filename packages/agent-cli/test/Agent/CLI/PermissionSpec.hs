module Agent.CLI.PermissionSpec (spec) where

import Agent.CLI.Permission
import Agent.CLI.Picker (PickerKey(..))
import Agent.CLI.Options (ApprovalPolicy(..))
import Agent.ToolDispatch (ToolCall(..), ToolCallKind(..))
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
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
        let escalatedCall command = ToolCall
                { callId = "shell-escalation-1"
                , name = "shell_command"
                , arguments = Text.decodeUtf8 $ ByteString.toStrict $ encode $ object
                    [ "command" .= command
                    , "sandbox_permissions" .= ("require_escalated" :: Text.Text)
                    , "justification" .= ("Swift manifest evaluation needs its own sandbox." :: Text.Text)
                    ]
                , callKind = FunctionCallKind
                , argumentsEncrypted = False
                }

        it "warns about isolation loss and shows the complete command and justification" do
            let command = "printf " <> Text.replicate 500 "x"
                prompt = approvalToolCallPromptRelative "/repo" (escalatedCall command)
            prompt `shouldSatisfy` Text.isInfixOf "outside session filesystem isolation?"
            prompt `shouldSatisfy` Text.isInfixOf "WARNING:"
            prompt `shouldSatisfy` Text.isInfixOf command
            prompt `shouldSatisfy` Text.isInfixOf "Working directory: \"/repo\""
            prompt `shouldSatisfy` Text.isInfixOf "Swift manifest evaluation needs its own sandbox."
            prompt `shouldSatisfy` Text.isInfixOf "does not enforce isolation"

        it "shows the explicitly requested working directory without shortening it" do
            let call = (escalatedCall ("pwd" :: Text.Text))
                    { arguments =
                        "{\"command\":\"pwd\",\"workdir\":\"/repo/application\",\"sandbox_permissions\":\"require_escalated\",\"justification\":\"Build application\"}"
                    }
            approvalToolCallPromptRelative "/repo" call `shouldSatisfy`
                Text.isInfixOf "Working directory: \"/repo/application\""

        it "ignores unsupported Grok workdir arguments in the warning" do
            let call = (escalatedCall ("pwd" :: Text.Text))
                    { name = "run_terminal_cmd"
                    , arguments =
                        "{\"command\":\"pwd\",\"workdir\":\"/fake\",\"sandbox_permissions\":\"require_escalated\",\"justification\":\"Build application\"}"
                    }
            approvalToolCallPromptRelative "/repo" call `shouldSatisfy`
                Text.isInfixOf "Working directory: \"/repo\""

        it "escapes control characters instead of allowing the command to rewrite the warning" do
            let prompt = approvalToolCallPromptRelative "/repo"
                    (escalatedCall ("printf '\ESC[2J'\ntrue" :: Text.Text))
            prompt `shouldNotSatisfy` Text.isInfixOf "\ESC"
            prompt `shouldSatisfy` Text.isInfixOf "\\ESC"
            prompt `shouldSatisfy` Text.isInfixOf "\\ntrue"

        it "shows the turn working directory as the base for a relative workdir" do
            let call = (escalatedCall ("pwd" :: Text.Text))
                    { arguments =
                        "{\"command\":\"pwd\",\"workdir\":\"application\",\"sandbox_permissions\":\"require_escalated\",\"justification\":\"Build application\"}"
                    }
            approvalToolCallPromptRelative "/repo" call `shouldSatisfy`
                Text.isInfixOf "Working directory: \"/repo/application\""

        it "does not display escalation warnings for ordinary shell calls" do
            let call = (escalatedCall ("pwd" :: Text.Text))
                    { arguments = "{\"command\":\"pwd\"}" }
            approvalToolCallPromptRelative "/repo" call `shouldNotSatisfy`
                Text.isInfixOf "outside session filesystem isolation"

        it "warns before sending additional input to an escalated process" do
            let call = (escalatedCall ("pwd" :: Text.Text))
                    { name = "write_stdin"
                    , arguments = "{\"session_id\":12,\"chars\":\"printf additional\\\\n\"}"
                    }
                prompt = approvalToolCallPromptOnceRelative "/repo" call
            prompt `shouldSatisfy` Text.isInfixOf "outside session filesystem isolation?"
            prompt `shouldSatisfy` Text.isInfixOf "may execute additional commands"
            prompt `shouldSatisfy` Text.isInfixOf (Text.pack (show call.arguments))
            prompt `shouldSatisfy` Text.isInfixOf "this input only"

        it "recognizes the Grok terminal tool" do
            let call = (escalatedCall ("pwd" :: Text.Text))
                    { name = "run_terminal_cmd" }
            approvalToolCallPromptRelative "/repo" call `shouldSatisfy`
                Text.isInfixOf "outside session filesystem isolation"

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
                    "Allow this computer-use request?"
            prompt `shouldNotSatisfy` Text.isInfixOf "\"actions\""

    describe "fresh permission card" do
        it "offers only allow once and deny" do
            let frame = renderPermissionOnceFrame False "Run outside isolation?" 0
            frame `shouldSatisfy` Text.isInfixOf "Allow once"
            frame `shouldSatisfy` Text.isInfixOf "Deny"
            frame `shouldNotSatisfy` Text.isInfixOf "Always"

        it "does not accept persistent approval shortcuts" do
            applyPermissionOnceKey (PickerKeyChar 'A') 0 `shouldBe` Right 0
            applyPermissionOnceKey (PickerKeyChar 'a') 0 `shouldBe` Right 0
            applyPermissionOnceKey PickerKeyCancel 0 `shouldBe` Left PermissionDeny
            applyPermissionOnceKey (PickerKeyChar 'y') 0 `shouldBe` Left PermissionAllowOnce
            applyPermissionOnceKey PickerKeyDown 0 `shouldBe` Right 1
            applyPermissionOnceKey PickerKeyConfirm 1 `shouldBe` Left PermissionDeny

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
