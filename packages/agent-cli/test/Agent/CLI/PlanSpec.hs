module Agent.CLI.PlanSpec (spec) where

import Agent.CLI.Plan
import Agent.Tools.PlanMode (PlanDecision(..))
import Test.Hspec

spec :: Spec
spec = do
    describe "extractProposedPlan" do
        it "pulls the inner markdown from a proposed_plan block" do
            extractProposedPlan
                "intro\n<proposed_plan>\n# Title\n\nbody\n</proposed_plan>\nout"
                `shouldBe` Just "# Title\n\nbody"

        it "returns Nothing without a closed block" do
            extractProposedPlan "no plan here" `shouldBe` Nothing
            extractProposedPlan "<proposed_plan>\nunclosed" `shouldBe` Nothing

    describe "stripProposedPlan" do
        it "removes the tagged block and keeps surrounding text" do
            stripProposedPlan
                "before\n<proposed_plan>\nplan\n</proposed_plan>\nafter"
                `shouldBe` "before\n\nafter"

    describe "parsePlanDecisionAnswer" do
        it "maps approve / changes / cancel aliases" do
            parsePlanDecisionAnswer "a" `shouldBe` Just PlanApprove
            parsePlanDecisionAnswer "approve" `shouldBe` Just PlanApprove
            parsePlanDecisionAnswer "s" `shouldBe` Just (PlanRequestChanges "")
            parsePlanDecisionAnswer "changes" `shouldBe` Just (PlanRequestChanges "")
            parsePlanDecisionAnswer "q" `shouldBe` Just PlanCancel
            parsePlanDecisionAnswer "cancel" `shouldBe` Just PlanCancel
            parsePlanDecisionAnswer "maybe" `shouldBe` Nothing
