module Agent.TUI.PlanReviewSpec (spec) where

import Agent.TUI.PlanReview
import qualified Agent.TUI.Theme as Theme
import Brick (Widget, renderWidget)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.List (isInfixOf)
import qualified Graphics.Vty as V
import Test.Hspec

spec :: Spec
spec = describe "plan review state" do
    it "represents an empty plan explicitly" do
        let state = initialPlanReview (request "")
        state.reviewSelectedLine `shouldBe` 0
        planSourceLines state.reviewRequest.requestMarkdown `shouldBe` []
        selectedPlanLineRange state `shouldBe` Nothing

    it "preserves feedback and comment drafts across display modes" do
        let
            initial = initialPlanReview (request "one\ntwo")
            withDrafts =
                continuing $
                    stepPlanReview
                        (ReviewSetCommentDraft "Explain this")
                        (continuing $
                            stepPlanReview
                                (ReviewSetFeedbackDraft "Overall")
                                initial)
            toggled =
                continuing $
                    stepPlanReview ReviewToggleMode withDrafts
        toggled.reviewMode `shouldBe` PlanReviewSource
        toggled.reviewFeedbackDraft `shouldBe` "Overall"
        toggled.reviewCommentDraft `shouldBe` "Explain this"

    it "normalizes selected ranges and adds correlated line comments" do
        let
            initial = initialPlanReview (request "one\ntwo\nthree")
            selected =
                applyCommands
                    [ ReviewSetMode PlanReviewSource
                    , ReviewMoveLine 2
                    , ReviewToggleRangeAnchor
                    , ReviewMoveLine (-1)
                    , ReviewSetCommentDraft "Clarify these lines"
                    , ReviewAddComment
                    ]
                    initial
        selected.reviewComments
            `shouldBe`
                [ PlanReviewComment
                    { commentRange = PlanLineRange 2 3
                    , commentBody = "Clarify these lines"
                    }
                ]
        selected.reviewCommentDraft `shouldBe` ""
        numberedPlanSource selected
            `shouldBe`
                [ " 1 │ one"
                , "›2 │ two"
                , "•3 │ three"
                ]

    it "requires an explicit approve-anyway action label for warnings" do
        let
            warning =
                PlanReviewWarning
                    { warningCode = "missing-verification"
                    , warningMessage = "Verification is missing."
                    }
            state =
                initialPlanReview
                    ((request "# Plan")
                        { requestWarnings = [warning]
                        })
        planReviewActionLabel state PlanReviewApprove
            `shouldBe` "Approve anyway"
        stepPlanReview (ReviewChooseAction PlanReviewApprove) state
            `shouldBe`
                PlanReviewComplete
                    (PlanApproved PlanApproval
                        { approvalRequestId = PlanReviewId "review-1"
                        , approvalDigest = "0123456789abcdef"
                        , approvalAcceptedWarnings = True
                        })

    it "returns feedback and comments on revision" do
        let
            state =
                (initialPlanReview (request "one"))
                    { reviewFeedbackDraft = "  Rework this  "
                    , reviewComments =
                        [ PlanReviewComment
                            { commentRange = PlanLineRange 1 1
                            , commentBody = "Be specific"
                            }
                        ]
                    }
        stepPlanReview (ReviewChooseAction PlanReviewRevise) state
            `shouldBe`
                PlanReviewComplete
                    (PlanRevisionRequested PlanRevision
                        { revisionRequestId = PlanReviewId "review-1"
                        , revisionDigest = "0123456789abcdef"
                        , revisionFeedback = "Rework this"
                        , revisionComments = state.reviewComments
                        })

    it "dismisses only the externally resolved request" do
        let state = initialPlanReview (request "one")
        stepPlanReview
            (ReviewDismissExternal (PlanReviewId "different"))
            state
            `shouldBe` PlanReviewContinue state
        stepPlanReview
            (ReviewDismissExternal (PlanReviewId "review-1"))
            state
            `shouldBe`
                PlanReviewComplete
                    (PlanReviewExternallyResolved (PlanReviewId "review-1"))

    it "renders warning, empty, preview, and source surfaces" do
        let
            warning =
                PlanReviewWarning
                    { warningCode = "empty"
                    , warningMessage = "The plan is empty."
                    }
            emptyState =
                initialPlanReview
                    ((request "")
                        { requestWarnings = [warning]
                        })
            sourceState =
                continuing $
                    stepPlanReview
                        (ReviewSetMode PlanReviewSource)
                        (initialPlanReview (request "# Plan\n\nDo work"))
            rendered state =
                show $
                    renderWidget
                        (Just Theme.terminalDefault)
                        [ planReviewWidget
                            (Text.pack . show)
                            state
                            :: Widget Text
                        ]
                        (80, 24)
        rendered emptyState `shouldSatisfy`
            isInfixOf "No plan has been written yet."
        rendered sourceState `shouldSatisfy`
            isInfixOf "# Plan"

    it "maps review navigation and explicit actions without consuming text editing" do
        let
            state = initialPlanReview (request "one\ntwo")
            editing =
                state { reviewFocus = PlanReviewFeedback }
        planReviewCommandForEvent state (V.EvKey V.KRight [])
            `shouldBe` Just (ReviewSetMode PlanReviewSource)
        planReviewCommandForEvent state (V.EvKey (V.KChar 'a') [])
            `shouldBe` Just (ReviewChooseAction PlanReviewApprove)
        planReviewCommandForEvent editing (V.EvKey (V.KChar 'a') [])
            `shouldBe` Nothing
        planReviewCommandForEvent editing (V.EvKey V.KEsc [])
            `shouldBe` Just (ReviewSetFocus PlanReviewDocument)
        planReviewCommandForControl (ReviewLineControl 2)
            `shouldBe` ReviewSelectLine 2

request :: Text -> PlanReviewRequest
request markdown =
    PlanReviewRequest
        { requestId = PlanReviewId "review-1"
        , requestTitle = "Implementation plan"
        , requestMarkdown = markdown
        , requestDigest = "0123456789abcdef"
        , requestWarnings = []
        }

continuing :: PlanReviewTransition -> PlanReviewState
continuing = \case
    PlanReviewContinue state -> state
    PlanReviewComplete outcome ->
        error ("unexpected completed review: " <> show outcome)

applyCommands
    :: [PlanReviewCommand]
    -> PlanReviewState
    -> PlanReviewState
applyCommands commands initial =
    foldl
        (\state command -> continuing (stepPlanReview command state))
        initial
        commands
