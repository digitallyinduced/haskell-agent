module Agent.CLI.ReplStatusSpec (spec) where

import Agent.CLI (formatReplStatusLine)
import Agent.CLI.Options (ApprovalPolicy(..))
import Test.Hspec

spec :: Spec
spec = describe "formatReplStatusLine" do
    it "shows model, effort, and approval mode" do
        formatReplStatusLine False "grok-4.6" "high" PromptMutating
            `shouldBe` "  grok-4.6 · high · ask"
        formatReplStatusLine False "gpt-5.1-codex" "medium" ApproveAll
            `shouldBe` "  gpt-5.1-codex · medium · yolo"
        formatReplStatusLine False "gpt-5.1" "low" DenyMutating
            `shouldBe` "  gpt-5.1 · low · deny"
