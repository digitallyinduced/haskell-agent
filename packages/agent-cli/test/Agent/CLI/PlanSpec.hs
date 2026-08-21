module Agent.CLI.PlanSpec (spec) where

import Agent.CLI.Plan
import Agent.CLI.Picker (PickerKey(..))
import Agent.Tools.PlanMode (PlanDecision(..))
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = do
    describe "plan entry picker" do
        let state = initialPlanEnterState "Need an architectural pass."

        it "accepts a/y and declines n/escape" do
            applyPlanEnterKey (PickerKeyChar 'a') state
                `shouldBe` Left PlanEnter
            applyPlanEnterKey (PickerKeyChar 'Y') state
                `shouldBe` Left PlanEnter
            applyPlanEnterKey (PickerKeyChar 'n') state
                `shouldBe` Left PlanStayNormal
            applyPlanEnterKey PickerKeyCancel state
                `shouldBe` Left PlanStayNormal

        it "supports navigation, selection, and an interactive frame" do
            case applyPlanEnterKey PickerKeyDown state of
                Left _ -> expectationFailure "down unexpectedly selected"
                Right down -> do
                    applyPlanEnterKey PickerKeyConfirm down
                        `shouldBe` Left PlanStayNormal
                    renderPlanEnterFrame False down
                        `shouldSatisfy` Text.isInfixOf "› Stay in normal mode"

    describe "plan exit picker" do
        it "supports shortcuts and selecting request changes" do
            applyPlanExitKey (PickerKeyChar 'a') initialPlanExitState
                `shouldBe` Left PlanApprove
            applyPlanExitKey (PickerKeyChar 's') initialPlanExitState
                `shouldBe` Left (PlanRequestChanges "")
            applyPlanExitKey PickerKeyCancel initialPlanExitState
                `shouldBe` Left PlanCancel
            case applyPlanExitKey PickerKeyDown initialPlanExitState of
                Left _ -> expectationFailure "down unexpectedly selected"
                Right changes ->
                    applyPlanExitKey PickerKeyConfirm changes
                        `shouldBe` Left (PlanRequestChanges "")

        it "renders all decisions and keyboard hints" do
            let frame = renderPlanExitFrame False initialPlanExitState
            frame `shouldSatisfy` Text.isInfixOf "› Approve and implement"
            frame `shouldSatisfy` Text.isInfixOf "Request changes"
            frame `shouldSatisfy` Text.isInfixOf "q/esc cancel"

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

    describe "renderPlanMarkdown" do
        it "leaves plan Markdown unchanged when color is off" do
            let plan = "# Plan\n\n- edit `Plan.hs`"
            renderPlanMarkdown False plan `shouldBe` plan

        it "styles plan Markdown when color is on" do
            let out = renderPlanMarkdown True "# Plan"
            out `shouldSatisfy` Text.isInfixOf "Plan"
            out `shouldSatisfy` (not . Text.isInfixOf "# Plan")

    describe "parsePlanDecisionAnswer" do
        it "maps approve / changes / cancel aliases" do
            parsePlanDecisionAnswer "a" `shouldBe` Just PlanApprove
            parsePlanDecisionAnswer "approve" `shouldBe` Just PlanApprove
            parsePlanDecisionAnswer "s" `shouldBe` Just (PlanRequestChanges "")
            parsePlanDecisionAnswer "changes" `shouldBe` Just (PlanRequestChanges "")
            parsePlanDecisionAnswer "q" `shouldBe` Just PlanCancel
            parsePlanDecisionAnswer "cancel" `shouldBe` Just PlanCancel
            parsePlanDecisionAnswer "maybe" `shouldBe` Nothing

    describe "planDecisionFollowUp" do
        it "continues immediately when the plan is approved" do
            planDecisionFollowUp PlanApprove
                `shouldSatisfy` maybe False (Text.isInfixOf "Begin implementing")

        it "keeps revising when changes are requested" do
            planDecisionFollowUp (PlanRequestChanges "cover retries")
                `shouldSatisfy` maybe False (Text.isInfixOf "cover retries")

        it "does not continue after cancellation" do
            planDecisionFollowUp PlanCancel `shouldBe` Nothing
