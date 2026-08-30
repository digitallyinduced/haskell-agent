module Agent.CLI.PlanSpec (spec) where

import Agent.CLI.Plan
import Agent.CLI.Picker (PickerKey(..))
import Agent.Tools.PlanMode (PlanDecision(..))
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Text as Text
import System.OsPath (unsafeEncodeUtf)
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
            applyPlanEnterKey (PickerKeyChar 'q') state
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
            applyPlanExitKey (PickerKeyChar 'Y') initialPlanExitState
                `shouldBe` Left PlanApprove
            map
                (`applyPlanExitKey` initialPlanExitState)
                (map PickerKeyChar "scr")
                `shouldBe` replicate 3 (Left (PlanRequestChanges ""))
            applyPlanExitKey (PickerKeyChar 'n') initialPlanExitState
                `shouldBe` Left PlanCancel
            applyPlanExitKey (PickerKeyChar 'q') initialPlanExitState
                `shouldBe` Left PlanCancel
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
            frame `shouldSatisfy` Text.isInfixOf "Revise plan"
            frame `shouldSatisfy` Text.isInfixOf "Abandon plan"
            frame `shouldSatisfy` Text.isInfixOf "q/esc abandon"

    describe "typed terminal plan review" do
        it "requires approve-anyway acknowledgement when warnings exist" do
            let state =
                    initialTerminalPlanReviewState
                        ["Verification section is missing."]
                frame = renderTerminalPlanReviewFrame False state
            frame `shouldSatisfy`
                Text.isInfixOf "Approve anyway and implement"
            frame `shouldSatisfy`
                Text.isInfixOf "Verification section is missing"
            applyTerminalPlanReviewKey (PickerKeyChar 'a') state
                `shouldBe` Left TerminalPlanApproveAnyway
            applyTerminalPlanReviewKey
                (PickerKeyChar 'a')
                (initialTerminalPlanReviewState [])
                `shouldBe` Left TerminalPlanApprove

        it "distinguishes revise, abandon, and close/defer" do
            let state = initialTerminalPlanReviewState []
                emptyFeedback = PlanReviewFeedback "" []
            applyTerminalPlanReviewKey (PickerKeyChar 'r') state
                `shouldBe` Left (TerminalPlanRevise emptyFeedback)
            applyTerminalPlanReviewKey (PickerKeyChar 'b') state
                `shouldBe` Left TerminalPlanAbandon
            applyTerminalPlanReviewKey PickerKeyCancel state
                `shouldBe` Left TerminalPlanDefer
            applyTerminalPlanReviewKey (PickerKeyChar 'q') state
                `shouldBe` Left TerminalPlanDefer

        it "navigates all four decisions" do
            let state = initialTerminalPlanReviewState []
            case applyTerminalPlanReviewKey PickerKeyUp state of
                Left _ -> expectationFailure "up unexpectedly selected"
                Right closeState ->
                    applyTerminalPlanReviewKey PickerKeyConfirm closeState
                        `shouldBe` Left TerminalPlanDefer

    describe "parsePlanReviewFeedback" do
        it "separates line and range comments from overall feedback" do
            parsePlanReviewFeedback
                "Please simplify this.\nL12: Explain the retry.\n\
                \L20-L23: Add exact tests."
                `shouldBe` PlanReviewFeedback
                    { planFeedbackOverall = "Please simplify this."
                    , planFeedbackLineComments =
                        [ PlanLineComment 12 12 "Explain the retry."
                        , PlanLineComment 20 23 "Add exact tests."
                        ]
                    }

        it "keeps malformed or descending references as overall feedback" do
            parsePlanReviewFeedback
                "L0: invalid\nL9-L3: backwards\nL4:"
                `shouldBe` PlanReviewFeedback
                    { planFeedbackOverall =
                        "L0: invalid\nL9-L3: backwards\nL4:"
                    , planFeedbackLineComments = []
                    }

    describe "runPlanReviewAdapter" do
        it "writes and verifies the exact snapshot before and after review" do
            events <- newIORef []
            let record event = modifyIORef' events (<> [event])
                adapter = PlanReviewAdapter
                    { planReviewWriteSnapshot = \content -> do
                        record ("write:" <> content)
                        pure (Right ())
                    , planReviewReadSnapshot = do
                        record "read"
                        pure (Right "# Plan")
                    , planReviewPresentSnapshot = \content warnings -> do
                        record
                            ("present:"
                                <> content
                                <> ":"
                                <> Text.intercalate "," warnings)
                        pure TerminalPlanApproveAnyway
                    }
            runPlanReviewAdapter
                adapter
                ["Missing verification section"]
                "# Plan"
                `shouldReturn` Right TerminalPlanApproveAnyway
            readIORef events `shouldReturn`
                [ "write:# Plan"
                , "read"
                , "present:# Plan:Missing verification section"
                , "read"
                ]

        it "does not present unreadable or stale snapshots" do
            presented <- newIORef False
            let presenter _ _ = do
                    modifyIORef' presented (const True)
                    pure TerminalPlanApprove
                unreadable = PlanReviewAdapter
                    { planReviewWriteSnapshot = \_ -> pure (Right ())
                    , planReviewReadSnapshot =
                        pure (Left "permission denied")
                    , planReviewPresentSnapshot = presenter
                    }
                stale = PlanReviewAdapter
                    { planReviewWriteSnapshot = \_ -> pure (Right ())
                    , planReviewReadSnapshot =
                        pure (Right "different plan")
                    , planReviewPresentSnapshot = presenter
                    }
            runPlanReviewAdapter unreadable [] "# Plan"
                `shouldReturn`
                    Left (PlanReviewReadFailed "permission denied")
            runPlanReviewAdapter stale [] "# Plan"
                `shouldReturn` Left PlanReviewStale
            readIORef presented `shouldReturn` False

        it "rejects a snapshot that changes while the UI is open" do
            reads <- newIORef ["# Plan", "# Changed"]
            let readNext =
                    readIORef reads >>= \case
                        content : rest -> do
                            modifyIORef' reads (const rest)
                            pure (Right content)
                        [] -> pure (Left "unexpected read")
                adapter = PlanReviewAdapter
                    { planReviewWriteSnapshot = \_ -> pure (Right ())
                    , planReviewReadSnapshot = readNext
                    , planReviewPresentSnapshot = \_ _ ->
                        pure TerminalPlanApprove
                    }
            runPlanReviewAdapter adapter [] "# Plan"
                `shouldReturn` Left PlanReviewStale

        it "requires approve-anyway when advisory warnings exist" do
            let adapter decision = PlanReviewAdapter
                    { planReviewWriteSnapshot = \_ -> pure (Right ())
                    , planReviewReadSnapshot = pure (Right "# Plan")
                    , planReviewPresentSnapshot = \_ _ -> pure decision
                    }
                warnings = ["Missing verification section"]
            runPlanReviewAdapter
                (adapter TerminalPlanApprove)
                warnings
                "# Plan"
                `shouldReturn`
                    Left
                        (PlanReviewWarningsNotAcknowledged warnings)
            runPlanReviewAdapter
                (adapter TerminalPlanApproveAnyway)
                warnings
                "# Plan"
                `shouldReturn` Right TerminalPlanApproveAnyway

    describe "resolveTerminalPlanReview" do
        it "keeps approve, revise, abandon, and defer lifecycle effects distinct" do
            resolveTerminalPlanReview TerminalPlanApprove
                `shouldSatisfy` \resolution ->
                    resolution.planReviewShouldDeactivate
                        && maybe False
                            (Text.isInfixOf "Begin implementing")
                            resolution.planReviewContinuation
            resolveTerminalPlanReview TerminalPlanApproveAnyway
                `shouldBe`
                    resolveTerminalPlanReview TerminalPlanApprove
            let feedback = PlanReviewFeedback
                    { planFeedbackOverall = "Simplify this."
                    , planFeedbackLineComments =
                        [PlanLineComment 12 15 "Explain the retry."]
                    }
                revised =
                    resolveTerminalPlanReview
                        (TerminalPlanRevise feedback)
            revised.planReviewShouldDeactivate `shouldBe` False
            revised.planReviewContinuation
                `shouldSatisfy`
                    maybe False
                        (Text.isInfixOf
                            "L12-L15: Explain the retry.")
            resolveTerminalPlanReview TerminalPlanAbandon
                `shouldBe` PlanReviewResolution True Nothing
            resolveTerminalPlanReview TerminalPlanDefer
                `shouldBe` PlanReviewResolution False Nothing

    describe "parseProposedPlan" do
        it "pulls the inner markdown from a proposed_plan block" do
            parseProposedPlan
                "intro\n<proposed_plan>\n# Title\n\nbody\n</proposed_plan>\nout"
                `shouldBe` Right "# Title\n\nbody"

        it "reports missing and unclosed blocks explicitly" do
            parseProposedPlan "no plan here"
                `shouldBe` Left ProposedPlanNotFound
            parseProposedPlan "<proposed_plan>\nunclosed"
                `shouldBe` Left ProposedPlanUnclosed

        it "ignores proposal-looking tags inside backtick fences" do
            parseProposedPlan
                "```xml\n<proposed_plan>fake</proposed_plan>\n```\n\
                \<proposed_plan>real</proposed_plan>"
                `shouldBe` Right "real"

        it "ignores proposal-looking tags inside tilde fences" do
            parseProposedPlan
                "~~~\n<proposed_plan>fake</proposed_plan>\n~~~~\n\
                \<proposed_plan>real</proposed_plan>"
                `shouldBe` Right "real"

        it "ignores fenced examples nested in quotes and lists" do
            parseProposedPlan
                "> ```xml\n> <proposed_plan>quoted</proposed_plan>\n> ```\n\
                \- example\n\
                \  ~~~xml\n\
                \  <proposed_plan>listed</proposed_plan>\n\
                \  ~~~\n\
                \<proposed_plan>real</proposed_plan>"
                `shouldBe` Right "real"

        it "rejects nested and multiple blocks" do
            parseProposedPlan
                "<proposed_plan>outer <proposed_plan>inner</proposed_plan></proposed_plan>"
                `shouldBe` Left ProposedPlanNested
            parseProposedPlan
                "<proposed_plan>one</proposed_plan>\n\
                \<proposed_plan>two</proposed_plan>"
                `shouldBe` Left ProposedPlanMultiple

        it "rejects an unmatched closing tag" do
            parseProposedPlan "</proposed_plan>"
                `shouldBe` Left ProposedPlanUnexpectedClose

        it "keeps a compatibility Maybe wrapper" do
            extractProposedPlan "<proposed_plan>ok</proposed_plan>"
                `shouldBe` Just "ok"
            extractProposedPlan "<proposed_plan>broken"
                `shouldBe` Nothing

    describe "stripProposedPlan" do
        it "removes the tagged block and keeps surrounding text" do
            stripProposedPlan
                "before\n<proposed_plan>\nplan\n</proposed_plan>\nafter"
                `shouldBe` "before\n\nafter"

        it "does not strip proposal examples inside fenced code" do
            stripProposedPlan
                "```xml\n<proposed_plan>example</proposed_plan>\n```\n\
                \before\n<proposed_plan>real</proposed_plan>\nafter"
                `shouldBe`
                    "```xml\n<proposed_plan>example</proposed_plan>\n```\n\
                    \before\n\nafter"

        it "leaves malformed output intact" do
            let malformed =
                    "<proposed_plan>one</proposed_plan>\n\
                    \<proposed_plan>two</proposed_plan>"
            stripProposedPlan malformed `shouldBe` malformed

    describe "renderPlanMarkdown" do
        it "leaves plan Markdown unchanged when color is off" do
            let plan = "# Plan\n\n- edit `Plan.hs`"
            renderPlanMarkdown False plan `shouldBe` plan

        it "styles plan Markdown when color is on" do
            let out = renderPlanMarkdown True "# Plan"
            out `shouldSatisfy` Text.isInfixOf "Plan"
            out `shouldSatisfy` (not . Text.isInfixOf "# Plan")

    describe "formatPlanPreview" do
        let path = unsafeEncodeUtf "/tmp/session/plan.md"

        it "renders an explicit empty state and path" do
            let preview = formatPlanPreview path " \n"
            preview `shouldSatisfy`
                Text.isInfixOf "No plan has been written yet"
            preview `shouldSatisfy`
                Text.isInfixOf "/tmp/session/plan.md"

        it "preserves plan markdown and identifies its file" do
            formatPlanPreview path "# Plan\n\n- verify"
                `shouldBe`
                    "# Plan\n\n- verify\n\n---\n\
                    \Plan file: `/tmp/session/plan.md`"

    describe "parsePlanDecisionAnswer" do
        it "maps approve / changes / cancel aliases" do
            map parsePlanDecisionAnswer ["a", "approve", "y", "yes"]
                `shouldBe` replicate 4 (Just PlanApprove)
            map parsePlanDecisionAnswer ["s", "c", "changes", "r"]
                `shouldBe` replicate 4 (Just (PlanRequestChanges ""))
            map parsePlanDecisionAnswer ["q", "cancel", "n", "no"]
                `shouldBe` replicate 4 (Just PlanCancel)
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
