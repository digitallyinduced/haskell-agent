module Agent.CLI.ReplStatusSpec (spec) where

import Agent.CLI (applyReplMode, cycleReplInteraction, formatReplStatusLine, formatTokenUsage)
import Agent.CLI.Options (ApprovalPolicy(..))
import Agent.CLI.Project (ProjectSettings(..), loadProjectSettings)
import Agent.CLI.ReplMode
    ( ReplMode(..)
    , cycleReplMode
    , replModeFromState
    )
import Agent.Loop (TokenUsage(..), emptyTokenUsage)
import Agent.Tools.PlanMode (PlanModeEnv(..), PlanModeState(..), newPlanModeEnv)
import Control.Exception (bracket)
import Data.IORef (newIORef, readIORef)
import System.Directory (getTemporaryDirectory, removeDirectoryRecursive)
import System.Posix.Temp (mkdtemp)
import Test.Hspec

spec :: Spec
spec = do
    describe "formatReplStatusLine" do
        it "shows model, effort, and interaction mode" do
            formatReplStatusLine False Nothing "grok-4.6" "high" ReplModeNormal emptyTokenUsage
                `shouldBe` "  grok-4.6 · high · ask"
            formatReplStatusLine False Nothing "gpt-5.1-codex" "medium" ReplModeAlwaysApprove emptyTokenUsage
                `shouldBe` "  gpt-5.1-codex · medium · yolo"
            formatReplStatusLine False Nothing "gpt-5.1" "low" ReplModePlan emptyTokenUsage
                `shouldBe` "  gpt-5.1 · low · plan"

        it "appends session usage when no width is known" do
            formatReplStatusLine False Nothing "grok-4.6" "high" ReplModeNormal
                TokenUsage { inputTokens = 1200, outputTokens = 340, cachedTokens = 0 }
                `shouldBe` "  grok-4.6 · high · ask  1.2k in · 340 out"

        it "right-aligns session usage when the TTY is wide enough" do
            formatReplStatusLine False (Just 48) "grok-4.6" "high" ReplModeNormal
                TokenUsage { inputTokens = 1200, outputTokens = 340, cachedTokens = 0 }
                `shouldBe` "  grok-4.6 · high · ask        1.2k in · 340 out"

        it "keeps a two-space gap when the line would overflow" do
            formatReplStatusLine False (Just 20) "grok-4.6" "high" ReplModeNormal
                TokenUsage { inputTokens = 1200, outputTokens = 340, cachedTokens = 0 }
                `shouldBe` "  grok-4.6 · high · ask  1.2k in · 340 out"

    describe "cycleReplMode" do
        it "walks ask → plan → always-approve → ask" do
            cycleReplMode ReplModeNormal `shouldBe` ReplModePlan
            cycleReplMode ReplModePlan `shouldBe` ReplModeAlwaysApprove
            cycleReplMode ReplModeAlwaysApprove `shouldBe` ReplModeNormal

        it "treats pending/active plan as plan even under yolo" do
            replModeFromState PlanPending ApproveAll `shouldBe` ReplModePlan
            replModeFromState PlanActive PromptMutating `shouldBe` ReplModePlan
            replModeFromState PlanInactive ApproveAll `shouldBe` ReplModeAlwaysApprove
            replModeFromState PlanInactive PromptMutating `shouldBe` ReplModeNormal
            replModeFromState PlanInactive DenyMutating `shouldBe` ReplModeNormal

        it "cycles from current plan/approval state" do
            cycleReplInteraction PlanInactive PromptMutating
                `shouldBe` ReplModePlan
            cycleReplInteraction PlanPending PromptMutating
                `shouldBe` ReplModeAlwaysApprove
            cycleReplInteraction PlanInactive ApproveAll
                `shouldBe` ReplModeNormal

        it "applies plan, yolo, then ask" $
            withTempDir "agent-mode-" \root -> do
                plan <- newPlanModeEnv root Nothing
                policyRef <- newIORef PromptMutating
                applyReplMode plan policyRef root ReplModePlan
                readIORef plan.planStateRef `shouldReturn` PlanPending
                readIORef policyRef `shouldReturn` PromptMutating

                applyReplMode plan policyRef root ReplModeAlwaysApprove
                readIORef plan.planStateRef `shouldReturn` PlanInactive
                readIORef policyRef `shouldReturn` ApproveAll
                settings <- loadProjectSettings root
                settings.settingsAutoApprove `shouldBe` True

                applyReplMode plan policyRef root ReplModeNormal
                readIORef plan.planStateRef `shouldReturn` PlanInactive
                readIORef policyRef `shouldReturn` PromptMutating
                settings' <- loadProjectSettings root
                settings'.settingsAutoApprove `shouldBe` False

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

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir prefix action = do
    tmp <- getTemporaryDirectory
    bracket (mkdtemp (tmp <> "/" <> prefix)) removeDirectoryRecursive action
