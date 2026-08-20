module Agent.CLI.ReplStatusSpec (spec) where

import Agent.CLI (formatReplStatusLine, formatTokenUsage)
import Agent.CLI.Options (ApprovalPolicy(..))
import Agent.Loop (TokenUsage(..), emptyTokenUsage)
import Test.Hspec

spec :: Spec
spec = do
    describe "formatReplStatusLine" do
        it "shows model, effort, and approval mode" do
            formatReplStatusLine False Nothing "grok-4.6" "high" PromptMutating emptyTokenUsage
                `shouldBe` "  grok-4.6 · high · ask"
            formatReplStatusLine False Nothing "gpt-5.1-codex" "medium" ApproveAll emptyTokenUsage
                `shouldBe` "  gpt-5.1-codex · medium · yolo"
            formatReplStatusLine False Nothing "gpt-5.1" "low" DenyMutating emptyTokenUsage
                `shouldBe` "  gpt-5.1 · low · deny"

        it "appends session usage when no width is known" do
            formatReplStatusLine False Nothing "grok-4.6" "high" PromptMutating
                TokenUsage { inputTokens = 1200, outputTokens = 340, cachedTokens = 0 }
                `shouldBe` "  grok-4.6 · high · ask  1.2k in · 340 out"

        it "right-aligns session usage when the TTY is wide enough" do
            formatReplStatusLine False (Just 48) "grok-4.6" "high" PromptMutating
                TokenUsage { inputTokens = 1200, outputTokens = 340, cachedTokens = 0 }
                `shouldBe` "  grok-4.6 · high · ask        1.2k in · 340 out"

        it "keeps a two-space gap when the line would overflow" do
            formatReplStatusLine False (Just 20) "grok-4.6" "high" PromptMutating
                TokenUsage { inputTokens = 1200, outputTokens = 340, cachedTokens = 0 }
                `shouldBe` "  grok-4.6 · high · ask  1.2k in · 340 out"

    describe "formatTokenUsage" do
        it "omits empty totals" do
            formatTokenUsage emptyTokenUsage `shouldBe` ""

        it "formats compact in/out counts" do
            formatTokenUsage TokenUsage
                { inputTokens = 42
                , outputTokens = 7
                , cachedTokens = 0
                } `shouldBe` "42 in · 7 out"

        it "includes cached tokens when present" do
            formatTokenUsage TokenUsage
                { inputTokens = 1500
                , outputTokens = 80
                , cachedTokens = 1200
                } `shouldBe` "1.5k in · 80 out · 1.2k cached"

        it "uses k/M suffixes" do
            formatTokenUsage TokenUsage
                { inputTokens = 12500
                , outputTokens = 1500000
                , cachedTokens = 0
                } `shouldBe` "13k in · 1.5M out"
