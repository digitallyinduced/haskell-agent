-- | Adapt plan-mode, secret, and image-display hooks to the currently active
-- fullscreen UI.
module Agent.CLI.PromptHooks
    ( fullscreenAwareImageHooks
    , fullscreenAwarePlanHooks
    , fullscreenAwareSecretHooks
    ) where

import Agent.CLI.Secret (sanitizeSecretPromptText)
import Agent.CLI.TUI.App
    ( FullscreenRuntime
    , requestFullscreenChoiceWithBody
    , requestFullscreenPlanReview
    , requestFullscreenQuestionnaire
    , requestFullscreenSecret
    , showFullscreenToolImage
    )
import Agent.TUI.PlanReview
    ( PlanApproval(..)
    , PlanLineRange(..)
    , PlanRevision(..)
    , PlanReviewComment(..)
    , PlanReviewId(..)
    , PlanReviewOutcome(..)
    , PlanReviewRequest(..)
    , PlanReviewWarning(..)
    )
import Agent.TUI.Questionnaire
    ( QuestionnaireAnswer(..)
    , QuestionnaireId(..)
    , QuestionnaireOption(..)
    , QuestionnaireOutcome(..)
    , QuestionnaireQuestion(..)
    , QuestionnaireRequest(..)
    , QuestionnaireSubmission(..)
    )
import Agent.Tools.PlanMode
    ( PlanModeHooks(..) )
import qualified Agent.Tools.PlanMode as Plan
import Agent.Tools.PlanMode.Document (PlanValidationWarning(..))
import Agent.Tools.Secret
    ( SecretPrompt(..)
    , SecretPromptHooks(..)
    )
import Agent.Tools.ShowImage
    ( ImageDisplayHooks(..)
    , ImageDisplayRequest(..)
    )
import Control.Applicative ((<|>))
import Data.IORef (IORef, readIORef)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Crypto.Hash (Digest, SHA256, hash)
import System.OsPath (unsafeEncodeUtf)

fullscreenCompatibilityHooks
    :: IORef (Maybe FullscreenRuntime)
    -> PlanModeHooks
    -> PlanModeHooks
fullscreenCompatibilityHooks runtimeRef hooks = PlanModeHooks
    { planConfirmEnter = confirmEnter
    , planDecideExit = compatibilityReview
    , planAskQuestion = askQuestion
    }
  where
    confirmEnter reason =
        withCurrentFullscreen runtimeRef
            (hooks.planConfirmEnter reason)
            \runtime ->
                requestFullscreenChoiceWithBody
                    runtime
                    "Enter plan mode?"
                    reason
                    0
                    [ ("Enter plan mode", "Explore and design before implementing")
                    , ("Stay in normal mode", "Continue without entering plan mode")
                    ]
                    >>= pure . (== Just 0)

    compatibilityReview planBody =
        case hooks of
            PlanModeHooks{planDecideExit} ->
                planDecideExit planBody
            PlanModeLifecycleHooks{planReviewPlan} ->
                planReviewPlan (syntheticCoreReview planBody) >>= \case
                    Plan.PlanReviewApprove -> pure Plan.PlanApprove
                    Plan.PlanReviewApproveAnyway -> pure Plan.PlanApprove
                    Plan.PlanReviewRequestChanges notes ->
                        pure (Plan.PlanRequestChanges notes)
                    Plan.PlanReviewAbandon -> pure Plan.PlanCancel
                    Plan.PlanReviewDefer -> pure Plan.PlanCancel

    askQuestion question options =
        withCurrentFullscreen runtimeRef
            (hooks.planAskQuestion question options)
            \runtime ->
                questionnaireResult
                    <$> requestFullscreenQuestionnaire
                        runtime
                        (planningQuestionnaire question options)

-- | Upgrade the compatibility wrapper above to the typed review lifecycle.
-- Provider runtimes add their quiesce/resume barriers afterwards.
fullscreenAwarePlanHooks
    :: IORef (Maybe FullscreenRuntime)
    -> PlanModeHooks
    -> PlanModeHooks
fullscreenAwarePlanHooks runtimeRef hooks =
    PlanModeLifecycleHooks
        { planConfirmEnter = wrapped.planConfirmEnter
        , planAskQuestion = wrapped.planAskQuestion
        , planReviewPlan = \request ->
            withCurrentFullscreen runtimeRef
                (reviewFallback request)
                \runtime ->
                    planReviewDecision
                        <$> requestFullscreenPlanReview
                            runtime
                            (planReviewRequest request)
        , planQuiesceBeforeActivation = existingQuiesce
        , planResumeAfterExit = existingResume
        }
  where
    wrapped = fullscreenCompatibilityHooks runtimeRef hooks
    reviewFallback request =
        case hooks of
            PlanModeHooks{planDecideExit} ->
                Plan.legacyPlanReviewHook planDecideExit request
            PlanModeLifecycleHooks{planReviewPlan} ->
                planReviewPlan request
    existingQuiesce =
        case hooks of
            PlanModeHooks{} -> pure (Right ())
            PlanModeLifecycleHooks{planQuiesceBeforeActivation} ->
                planQuiesceBeforeActivation
    existingResume =
        case hooks of
            PlanModeHooks{} -> pure ()
            PlanModeLifecycleHooks{planResumeAfterExit} ->
                planResumeAfterExit

fullscreenAwareSecretHooks
    :: IORef (Maybe FullscreenRuntime)
    -> SecretPromptHooks
    -> SecretPromptHooks
fullscreenAwareSecretHooks runtimeRef hooks =
    SecretPromptHooks \request ->
        withCurrentFullscreen runtimeRef
            (hooks.promptSecret request)
            \runtime ->
                Right <$> requestFullscreenSecret
                    runtime
                    "Secret requested by agent"
                    (secretRequestBody request)

-- | Attach agent-displayed images to the fullscreen transcript while it is
-- active; otherwise fall back to the plain-terminal presentation.
fullscreenAwareImageHooks
    :: IORef (Maybe FullscreenRuntime)
    -> ImageDisplayHooks
    -> ImageDisplayHooks
fullscreenAwareImageHooks runtimeRef hooks =
    ImageDisplayHooks \request ->
        withCurrentFullscreen runtimeRef
            (hooks.showImage request)
            \runtime ->
                showFullscreenToolImage
                    runtime
                    request.displayCallId
                    request.displayImage

secretRequestBody :: SecretPrompt -> Text
secretRequestBody request =
    Text.intercalate "\n\n" $
        filter (not . Text.null)
            [ maybe ""
                (\purpose ->
                    "Purpose: "
                        <> sanitizeSecretPromptText (Text.strip purpose))
                request.secretPromptPurpose
            , sanitizeSecretPromptText
                (Text.strip request.secretPromptMessage)
            , "Input is hidden and is not added to conversation history."
            ]

withCurrentFullscreen
    :: IORef (Maybe FullscreenRuntime)
    -> IO a
    -> (FullscreenRuntime -> IO a)
    -> IO a
withCurrentFullscreen runtimeRef fallback fullscreenAction = do
    runtime <- readIORef runtimeRef
    case runtime of
        Nothing -> fallback
        Just active -> fullscreenAction active

nonBlank :: Maybe Text -> Maybe Text
nonBlank =
    (>>= \text ->
        let stripped = Text.strip text
        in if Text.null stripped then Nothing else Just stripped)

planReviewRequest :: Plan.PlanReviewRequest -> PlanReviewRequest
planReviewRequest request =
    PlanReviewRequest
        { requestId = PlanReviewId request.planReviewRequestKey
        , requestTitle =
            maybe
                "Ready to implement this plan?"
                (\summary ->
                    if Text.null (Text.strip summary)
                        then "Ready to implement this plan?"
                        else Text.strip summary)
                request.planReviewSummary
        , requestMarkdown = request.planReviewMarkdown
        , requestDigest =
            request.planReviewSnapshotDigest.unPlanDigest
        , requestWarnings =
            [ PlanReviewWarning
                { warningCode =
                    Text.pack (show warning.planWarningCode)
                , warningMessage = warning.planWarningMessage
                }
            | warning <- request.planReviewWarnings
            ]
        }

planReviewDecision :: PlanReviewOutcome -> Plan.PlanReviewDecision
planReviewDecision = \case
    PlanApproved approval
        | approval.approvalAcceptedWarnings ->
            Plan.PlanReviewApproveAnyway
        | otherwise ->
            Plan.PlanReviewApprove
    PlanRevisionRequested revision ->
        Plan.PlanReviewRequestChanges (revisionNotes revision)
    PlanAbandoned _ _ -> Plan.PlanReviewAbandon
    PlanDeferred _ -> Plan.PlanReviewDefer
    PlanReviewExternallyResolved _ -> Plan.PlanReviewDefer

syntheticCoreReview :: Text -> Plan.PlanReviewRequest
syntheticCoreReview markdown =
    Plan.PlanReviewRequest
        { Plan.planReviewRequestKey = "legacy-plan-review:" <> digest
        , Plan.planReviewPath = unsafeEncodeUtf "plan.md"
        , Plan.planReviewSnapshotDigest = Plan.PlanDigest digest
        , Plan.planReviewMarkdown = markdown
        , Plan.planReviewWarnings = []
        , Plan.planReviewVerification = []
        , Plan.planReviewSummary = Nothing
        }
  where
    digest = sha256Text markdown

revisionNotes :: PlanRevision -> Text
revisionNotes revision =
    Text.intercalate "\n" $
        filter (not . Text.null)
            (Text.strip revision.revisionFeedback
                : map renderComment revision.revisionComments)
  where
    renderComment comment =
        renderRange comment.commentRange <> ": " <> comment.commentBody
    renderRange range
        | range.rangeStart == range.rangeEnd =
            "Line " <> Text.pack (show range.rangeStart)
        | otherwise =
            "Lines "
                <> Text.pack (show range.rangeStart)
                <> "-"
                <> Text.pack (show range.rangeEnd)

planningQuestionnaire :: Text -> [Text] -> QuestionnaireRequest
planningQuestionnaire question choices =
    QuestionnaireRequest
        { requestId =
            QuestionnaireId
                ("planning-question:" <> sha256Text payload)
        , requestQuestions =
            [ QuestionnaireQuestion
                { questionText = question
                , questionOptions =
                    [ QuestionnaireOption
                        { optionLabel = choice
                        , optionDescription = ""
                        , optionPreview = Nothing
                        }
                    | choice <- choices
                    ]
                , questionMultiSelect = False
                }
            ]
        }
  where
    payload = Text.intercalate "\NUL" (question : choices)

questionnaireResult :: QuestionnaireOutcome -> Maybe Text
questionnaireResult = \case
    QuestionnaireSubmitted submission ->
        listToMaybe submission.submissionAnswers >>= answerText
    QuestionnaireClarificationRequested _ clarification ->
        nonBlank (Just clarification)
    QuestionnaireFinished _ -> Nothing
    QuestionnaireCancelled _ -> Nothing
    QuestionnaireTimeout _ -> Nothing
    QuestionnaireExternallyResolved _ -> Nothing
  where
    answerText answer =
        nonBlank $
            answer.answerOther
                <|> listToMaybe answer.answerLabels

sha256Text :: Text -> Text
sha256Text value =
    Text.pack $
        show (hash (TextEncoding.encodeUtf8 value) :: Digest SHA256)
