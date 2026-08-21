module Agent.CLI.ApprovalSpec (spec) where

import Agent.CLI.Approval (childApprove)
import Agent.CLI.Options (ApprovalPolicy(..))
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
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "childApprove" do
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
    jsonAppTool name "" [] approval (noArgsTool name (pure (Right "ok")))

registry :: [AppTool] -> ToolRegistry
registry = either (error . Text.unpack) id . mkToolRegistry
