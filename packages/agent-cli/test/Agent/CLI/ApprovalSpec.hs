module Agent.CLI.ApprovalSpec (spec) where

import Agent.CLI.Approval (childApprove)
import Agent.CLI.Options (ApprovalPolicy(..))
import Agent.ToolDispatch
    ( ToolCall
    , functionToolCall
    , noArgsTool
    )
import Agent.Tools.Types
    ( AppTool(..)
    , AppToolKind(..)
    )
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "childApprove" do
    it "allows every known tool under ApproveAll" do
        childApprove ApproveAll [mutatingTool] mutatingCall
            `shouldReturn` Right True

    it "allows only read-only tools under DenyMutating" do
        childApprove DenyMutating [readOnlyTool] readOnlyCall
            `shouldReturn` Right True
        childApprove DenyMutating [mutatingTool] mutatingCall
            `shouldReturn` Right False

    it "returns an in-band denial when a child would need to prompt" do
        result <- childApprove PromptMutating [mutatingTool] mutatingCall
        result `shouldSatisfy` \case
            Left message -> "cannot prompt for approval" `Text.isInfixOf` message
            Right _ -> False

    it "honors per-call read-only classifiers" do
        childApprove DenyMutating [dynamicTool] dynamicReadCall
            `shouldReturn` Right True
        childApprove DenyMutating [dynamicTool] dynamicWriteCall
            `shouldReturn` Right False

readOnlyCall :: ToolCall
readOnlyCall = functionToolCall "call-read" "read" "{}"

mutatingCall :: ToolCall
mutatingCall = functionToolCall "call-write" "write" "{}"

dynamicReadCall :: ToolCall
dynamicReadCall = functionToolCall "call-dynamic-read" "dynamic" "read"

dynamicWriteCall :: ToolCall
dynamicWriteCall = functionToolCall "call-dynamic-write" "dynamic" "write"

readOnlyTool :: AppTool
readOnlyTool = tool "read" True Nothing

mutatingTool :: AppTool
mutatingTool = tool "write" False Nothing

dynamicTool :: AppTool
dynamicTool = tool "dynamic" False (Just (\call -> pure (call == dynamicReadCall)))

tool :: Text -> Bool -> Maybe (ToolCall -> IO Bool) -> AppTool
tool name readOnly classify = AppTool
    { appToolName = name
    , appToolDescription = ""
    , appToolParameters = []
    , appToolHandler = noArgsTool name (pure (Right "ok"))
    , appToolKind = JsonFunction
    , appToolReadOnly = readOnly
    , appToolIsReadOnlyCall = classify
    }
