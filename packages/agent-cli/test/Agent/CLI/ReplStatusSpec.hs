module Agent.CLI.ReplStatusSpec (spec) where

import Agent.CLI (applyReplMode, cycleReplInteraction, formatReplStatusLine)
import Agent.CLI.Options (ApprovalPolicy(..))
import Agent.CLI.Project (ProjectSettings(..), loadProjectSettings)
import Agent.CLI.ReplMode
    ( ReplMode(..)
    , cycleReplMode
    , replModeFromState
    )
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
            formatReplStatusLine False "grok-4.6" "high" ReplModeNormal
                `shouldBe` "  grok-4.6 · high · ask"
            formatReplStatusLine False "gpt-5.1-codex" "medium" ReplModeAlwaysApprove
                `shouldBe` "  gpt-5.1-codex · medium · yolo"
            formatReplStatusLine False "gpt-5.1" "low" ReplModePlan
                `shouldBe` "  gpt-5.1 · low · plan"

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

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir prefix action = do
    tmp <- getTemporaryDirectory
    bracket (mkdtemp (tmp <> "/" <> prefix)) removeDirectoryRecursive action
