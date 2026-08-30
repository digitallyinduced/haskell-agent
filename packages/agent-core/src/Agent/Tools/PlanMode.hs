-- | Shared plan-mode state: explore/write only @plan.md@, then present for
-- approve / request-changes / cancel.
--
-- Wire names for Grok follow grok-build (@enter_plan_mode@ / @exit_plan_mode@).
-- Codex presents plans via a @\<proposed_plan\>@ block in assistant text;
-- @update_plan@ stays a separate progress checklist and is blocked while
-- plan mode is active.
module Agent.Tools.PlanMode
    ( PlanModeState(..)
    , PlanDecision(..)
    , PlanReviewRequest(..)
    , PlanReviewDecision(..)
    , PlanReviewOutcome(..)
    , AskUserQuestionOption(..)
    , AskUserQuestion(..)
    , PlanQuestionnaireRequest(..)
    , PlanQuestionnaireAnswer(..)
    , PlanQuestionnaireDecision(..)
    , validatePlanQuestionnaireDecision
    , PlanEnterRequest(..)
    , PlanReminderKind(..)
    , PlanReminderToolNames(..)
    , PlanReminder(..)
    , PlanCompletion(..)
    , PlanDigest(..)
    , PlanSnapshot(..)
    , PlanFileError(..)
    , PlanReadResult(..)
    , PlanModeEnv(..)
    , PlanModeHooks(..)
    , withPlanModeLifecycle
    , newPlanModeEnv
    , planFileName
    , planFilePath
    , readPlanModeState
    , writePlanModeState
    , readPlanSessionDir
    , attachPlanSessionDir
    , readPlanTracker
    , readPlanAgentActivationRevision
    , updatePlanTracker
    , restrictPlanTracker
    , isPlanModeActive
    , activatePlanMode
    , deactivatePlanMode
    , readPlanMarkdown
    , readPlanSnapshot
    , writePlanMarkdown
    , writePlanSnapshot
    , ensurePlanMarkdown
    , planModeReminder
    , nextPlanModeReminder
    , submitPlanForReview
    , planReviewRequestKey
    , legacyPlanReviewHook
    , legacyPlanQuestionnaireHook
    , planApprovedContinuation
    , planModeBlockedEditMessage
    , isPlanFileEditTarget
    , enterPlanModeTool
    , enterCodexPlanModeTool
    , writePlanTool
    , exitPlanModeTool
    , askUserQuestionTool
    ) where

import Agent.Json.Decode (Decoder)
import Agent.OsPath (toText)
import Agent.ToolArgs (objectArgs, optBool, optList, optText, reqText)
import Agent.ToolDSL
    ( PropertySchema(..)
    , PropertyType(..)
    )
import Agent.ToolDispatch
    ( ToolCall(..)
    , typedTool
    , typedToolWithCall
    )
import Agent.Tools.Types
    ( AppTool
    , PlanModeCapability(..)
    , ToolBatchPhase(..)
    , ToolExecutionPolicy(..)
    , jsonTool
    , withPlanModeCapability
    , withToolBatchPhase
    )
import Agent.Tools.PlanMode.File
    ( PlanDigest(..)
    , PlanFileError(..)
    , PlanReadResult(..)
    , PlanSnapshot(..)
    , ensurePlanFile
    , readPlanFile
    , renderPlanFileError
    , writePlanFile
    )
import Agent.Tools.PlanMode.Document
    ( PlanDocument(..)
    , PlanValidationWarning(..)
    , parsePlanDocument
    )
import Agent.Tools.PlanMode.Persistence
    ( compareAndWritePlanTrackerState
    , readPlanTrackerState
    , validatePlanTracker
    )
import Agent.Tools.PlanMode.Tracker
    ( ApprovedPlanContinuation(..)
    , PlanTracker(..)
    , PlanTrackerPhase(..)
    , activatePlanTracker
    , beginPlanExit
    , consumePlanExitNotice
    , deactivatePlanTracker
    , initialPlanTracker
    , notePlanReminder
    , normalizePlanTrackerAfterRestart
    , PlanApprovalResolution(..)
    , PlanGeneration(..)
    , PendingPlanApproval(..)
    , queuePlanExitNotice
    , requestPlanActivation
    , resolvePlanApproval
    )
import Control.Applicative ((<|>))
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar_
    , newMVar
    , putMVar
    , readMVar
    , tryTakeMVar
    )
import Control.Exception.Safe
    ( displayException
    , finally
    , mask
    , throwString
    , tryAny
    , tryIO
    )
import Control.Monad (when)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import System.Directory.OsPath (canonicalizePath)
import System.OsPath (OsPath, equalFilePath, unsafeEncodeUtf, (</>))

data PlanModeState
    = PlanInactive
    | PlanPending
    -- ^ User toggled plan mode; becomes Active on the next prompt.
    | PlanActive
    | PlanExitPending
    -- ^ The exact on-disk plan snapshot is awaiting a correlated decision.
    deriving (Eq, Show)

-- | Immutable identity supplied to the enter-plan-mode confirmation hook.
data PlanEnterRequest = PlanEnterRequest
    { planEnterRequestKey :: !Text
    , planEnterReason :: !Text
    }
    deriving (Eq, Show)

data PlanQuestionnaireAnswer = PlanQuestionnaireAnswer
    { planAnswerQuestionIndex :: !Int
    , planAnswerQuestion :: !Text
    , planAnswerLabels :: ![Text]
    , planAnswerOther :: !(Maybe Text)
    } deriving (Eq, Show)

data PlanQuestionnaireRequest = PlanQuestionnaireRequest
    { planQuestionnaireRequestKey :: !Text
    , planQuestionnaireQuestions :: ![AskUserQuestion]
    } deriving (Eq, Show)

data PlanQuestionnaireDecision
    = PlanQuestionnaireSubmitted ![PlanQuestionnaireAnswer]
    | PlanQuestionnaireClarification !Text
    | PlanQuestionnaireFinished
    | PlanQuestionnaireCancelled
    | PlanQuestionnaireDeferred
    deriving (Eq, Show)

validatePlanQuestionnaireDecision
    :: PlanQuestionnaireRequest
    -> PlanQuestionnaireDecision
    -> Either Text PlanQuestionnaireDecision
validatePlanQuestionnaireDecision request = \case
    PlanQuestionnaireSubmitted answers -> do
        let questions = request.planQuestionnaireQuestions
        if length answers /= length questions
            then
                Left
                    "submitted questionnaire must answer every question exactly once"
            else
                PlanQuestionnaireSubmitted
                    <$> traverse
                        validateAnswer
                        (zip3 [0 ..] questions answers)
    PlanQuestionnaireClarification clarification
        | Text.null (Text.strip clarification) ->
            Left "questionnaire clarification must not be blank"
        | otherwise ->
            Right
                (PlanQuestionnaireClarification
                    (Text.strip clarification))
    other -> Right other
  where
    validateAnswer
        :: (Int, AskUserQuestion, PlanQuestionnaireAnswer)
        -> Either Text PlanQuestionnaireAnswer
    validateAnswer (expectedIndex, question, answer)
        | answer.planAnswerQuestionIndex /= expectedIndex =
            Left "questionnaire answers must be ordered by question index"
        | answer.planAnswerQuestion /= question.question =
            Left "questionnaire answer text does not match its request"
        | any (`notElem` offered) answer.planAnswerLabels =
            Left "questionnaire answer contains an unknown option label"
        | hasDuplicates answer.planAnswerLabels =
            Left "questionnaire answer contains duplicate option labels"
        | not multi
        , length answer.planAnswerLabels + otherCount /= 1 =
            Left
                "single-select questionnaire answer must contain exactly one option or Other"
        | multi
        , null answer.planAnswerLabels && normalizedOther == Nothing =
            Left
                "multi-select questionnaire answer must select an option or Other"
        | otherwise =
            Right answer
                { planAnswerOther = normalizedOther
                }
      where
        offered = map (.label) question.options
        multi = question.multiSelect == Just True
        normalizedOther =
            answer.planAnswerOther >>= nonBlankText
        otherCount = maybe 0 (const 1) normalizedOther

    hasDuplicates values =
        length values
            /= Map.size (Map.fromList [(value, ()) | value <- values])

data PlanCompletion
    = CompleteWithExitTool
    | CompleteWithProposedPlan
    deriving (Eq, Show)

data PlanDecision
    = PlanApprove
    | PlanRequestChanges Text
    | PlanCancel
    deriving (Eq, Show)

-- | Immutable data presented to the host for one correlated plan review.
-- The digest covers the exact UTF-8 bytes read from 'planReviewPath'.
data PlanReviewRequest = PlanReviewRequest
    { planReviewRequestKey :: !Text
    , planReviewPath :: !OsPath
    , planReviewSnapshotDigest :: !PlanDigest
    , planReviewMarkdown :: !Text
    , planReviewWarnings :: ![PlanValidationWarning]
    , planReviewVerification :: ![Text]
    , planReviewSummary :: !(Maybe Text)
    } deriving (Eq, Show)

data PlanReviewDecision
    = PlanReviewApprove
    | PlanReviewApproveAnyway
    | PlanReviewRequestChanges !Text
    | PlanReviewAbandon
    | PlanReviewDefer
    deriving (Eq, Show)

-- | Result of the shared review protocol. Provider integrations should use
-- this rather than inferring approval from assistant text.
data PlanReviewOutcome
    = PlanReviewAccepted !ApprovedPlanContinuation
    | PlanReviewRevisionRequired !Text
    | PlanReviewApprovalOverrideRequired !PlanReviewRequest
    | PlanReviewAbandoned
    | PlanReviewDeferred !PlanReviewRequest
    deriving (Eq, Show)

data PlanReminderKind
    = PlanReminderFull
    | PlanReminderSparse
    | PlanReminderReentry
    | PlanReminderPostExit
    deriving (Eq, Show)

-- | Actual client-facing tool names for reminder text. Codex callers may use
-- @\<proposed_plan\>@ as the completion name.
data PlanReminderToolNames = PlanReminderToolNames
    { planReminderWriteToolName :: !Text
    , planReminderQuestionToolName :: !Text
    , planReminderCompletionToolName :: !Text
    } deriving (Eq, Show)

data PlanReminder = PlanReminder
    { planReminderKind :: !PlanReminderKind
    , planReminderText :: !Text
    } deriving (Eq, Show)

-- | Session-scoped plan mode. 'planSessionDir' is the persisted session
-- directory when known; otherwise plans live under the tool cwd.
data PlanModeEnv = PlanModeEnv
    { planStateRef :: !(IORef PlanModeState)
    , planSessionDir :: !(IORef (Maybe OsPath))
    , planFallbackDir :: !OsPath
    , planHooks :: !PlanModeHooks
    , planTrackerRuntime :: !(MVar PlanTrackerRuntime)
    , planTransitionLock :: !(MVar ())
    , planAgentActivationRevision :: !(IORef (Maybe Word64))
    }

data PlanTrackerRuntime = PlanTrackerRuntime
    { runtimeTracker :: !PlanTracker
    , runtimeAttachedDir :: !(Maybe OsPath)
    , runtimeQuiesced :: !Bool
    }

-- | Hosts can keep using the compatibility constructor, or opt into the
-- correlated lifecycle constructor. Keeping distinct constructors means old
-- record construction remains total under @-Werror=missing-fields@.
data PlanModeHooks
    = PlanModeHooks
        { planConfirmEnter :: !(Text -> IO Bool)
        -- ^ Ask the user before agent-initiated enter_plan_mode.
        , planDecideExit :: !(Text -> IO PlanDecision)
        -- ^ Compatibility review hook.
        , planAskQuestion :: !(Text -> [Text] -> IO (Maybe Text))
        -- ^ Optional multiple-choice style question during planning.
        }
    | PlanModeLifecycleHooks
        { planConfirmEnter :: !(Text -> IO Bool)
        , planConfirmEnterRequest :: !(PlanEnterRequest -> IO Bool)
        , planAskQuestion :: !(Text -> [Text] -> IO (Maybe Text))
        , planAskQuestionnaire
            :: !(PlanQuestionnaireRequest -> IO PlanQuestionnaireDecision)
        , planReviewPlan :: !(PlanReviewRequest -> IO PlanReviewDecision)
        -- ^ Correlated typed review used by all new provider integrations.
        , planQuiesceBeforeActivation :: !(IO (Either Text ()))
        -- ^ Stop or suspend writers before plan-mode restrictions activate.
        , planResumeAfterExit :: !(IO ())
        -- ^ Resume quiesced work only after durable policy relaxation.
        }

-- | Add provider or host lifecycle barriers without discarding a typed review
-- hook. Multiple layers compose: quiescers run outside-in and resumptions run
-- inside-out, with both resume actions attempted.
withPlanModeLifecycle
    :: IO (Either Text ())
    -> IO ()
    -> PlanModeHooks
    -> PlanModeHooks
withPlanModeLifecycle quiesce resume = \case
    PlanModeHooks
        { planConfirmEnter
        , planDecideExit
        , planAskQuestion
        } ->
            PlanModeLifecycleHooks
                { planConfirmEnter
                , planConfirmEnterRequest =
                    planConfirmEnter . (.planEnterReason)
                , planAskQuestion
                , planAskQuestionnaire =
                    legacyPlanQuestionnaireHook planAskQuestion
                , planReviewPlan =
                    legacyPlanReviewHook planDecideExit
                , planQuiesceBeforeActivation = quiesce
                , planResumeAfterExit = resume
                }
    PlanModeLifecycleHooks
        { planConfirmEnter
        , planConfirmEnterRequest
        , planAskQuestion
        , planAskQuestionnaire
        , planReviewPlan
        , planQuiesceBeforeActivation
        , planResumeAfterExit
        } ->
            PlanModeLifecycleHooks
                { planConfirmEnter
                , planConfirmEnterRequest
                , planAskQuestion
                , planAskQuestionnaire
                , planReviewPlan
                , planQuiesceBeforeActivation = do
                    planQuiesceBeforeActivation >>= \case
                        Left err -> pure (Left err)
                        Right () -> quiesce
                , planResumeAfterExit =
                    resume `finallyAny` planResumeAfterExit
                }
  where
    finallyAny first second = do
        firstResult <- tryAny first
        secondResult <- tryAny second
        case (firstResult, secondResult) of
            (Left err, _) -> throwString (displayException err)
            (_, Left err) -> throwString (displayException err)
            _ -> pure ()

planFileName :: OsPath
planFileName = unsafeEncodeUtf "plan.md"

defaultHooks :: PlanModeHooks
defaultHooks = PlanModeHooks
    { planConfirmEnter = \_ -> pure True
    , planDecideExit = \_ -> pure PlanApprove
    , planAskQuestion = \_ _ -> pure Nothing
    }

newPlanModeEnv :: OsPath -> Maybe PlanModeHooks -> IO PlanModeEnv
newPlanModeEnv fallbackDir hooks = do
    stateRef <- newIORef PlanInactive
    sessionRef <- newIORef Nothing
    trackerRuntime <- newMVar PlanTrackerRuntime
        { runtimeTracker = initialPlanTracker
        , runtimeAttachedDir = Nothing
        , runtimeQuiesced = False
        }
    transitionLock <- newMVar ()
    agentActivationRevision <- newIORef Nothing
    pure PlanModeEnv
        { planStateRef = stateRef
        , planSessionDir = sessionRef
        , planFallbackDir = fallbackDir
        , planHooks = fromMaybe defaultHooks hooks
        , planTrackerRuntime = trackerRuntime
        , planTransitionLock = transitionLock
        , planAgentActivationRevision = agentActivationRevision
        }

planFilePath :: PlanModeEnv -> IO OsPath
planFilePath env = do
    sessionDir <- readPlanSessionDir env
    canonicalDirectory <- canonicalizePath $ case sessionDir of
        Just dir -> dir
        Nothing -> env.planFallbackDir
    pure (canonicalDirectory </> planFileName)

readPlanModeState :: PlanModeEnv -> IO PlanModeState
readPlanModeState = readIORef . (.planStateRef)

-- | Compatibility setter while callers migrate from direct 'IORef' access to
-- the durable tracker. New code should keep state transitions in one owner.
writePlanModeState :: PlanModeEnv -> PlanModeState -> IO ()
writePlanModeState env target = do
    let transition tracker = case target of
            PlanExitPending
                | tracker.trackerPhase == TrackerExitPending ->
                    Right tracker
                | otherwise ->
                    Left
                        "PlanExitPending can only mirror an existing pending review"
            _ -> Right (legacyStateTransition target tracker)
        apply
            | target `elem` [PlanActive, PlanExitPending] =
                restrictPlanTracker
            | otherwise = updatePlanTracker
    apply env transition >>= \case
        Left err -> throwString (Text.unpack err)
        Right _ -> pure ()

readPlanSessionDir :: PlanModeEnv -> IO (Maybe OsPath)
readPlanSessionDir = readIORef . (.planSessionDir)

setPlanSessionDir :: PlanModeEnv -> Maybe OsPath -> IO ()
setPlanSessionDir env = writeIORef env.planSessionDir

-- | Attach durable plan state to a session directory exactly once. A missing
-- sidecar is a legacy inactive session unless an in-memory pre-attach mode
-- transition already occurred, in which case that transition is persisted.
-- Corrupt or unreadable state is returned as a startup error.
attachPlanSessionDir :: PlanModeEnv -> OsPath -> IO (Either Text ())
attachPlanSessionDir env rawDirectory = do
    tryIO (canonicalizePath rawDirectory) >>= \case
        Left err ->
            pure
                (Left
                    ("could not resolve plan-mode session directory: "
                        <> Text.pack (displayException err)))
        Right directory ->
            withPlanTransition env do
                runtime <- readMVar env.planTrackerRuntime
                (next, result) <-
                    attachRuntime runtime directory
                replacePlanTrackerRuntime env next
                pure result
  where
    attachRuntime runtime directory =
                case runtime.runtimeAttachedDir of
                    Just attached
                        | equalFilePath attached directory -> do
                            setPlanSessionDir env (Just directory)
                            pure (runtime, Right ())
                        | not (trackerCanReattach runtime.runtimeTracker) ->
                            pure
                                ( runtime
                                , Left
                                    ("cannot reattach plan mode from "
                                        <> toText attached
                                        <> " while its tracker is restricted "
                                        <> "or has an undelivered continuation")
                                )
                        | otherwise ->
                            attachDirectory env runtime directory False
                    Nothing ->
                        attachDirectory env runtime directory True

attachDirectory
    :: PlanModeEnv
    -> PlanTrackerRuntime
    -> OsPath
    -> Bool
    -> IO (PlanTrackerRuntime, Either Text ())
attachDirectory env runtime directory mergeLegacy =
    readPlanTrackerState directory >>= \case
        Left err -> pure (runtime, Left err)
        Right persisted -> do
            legacyState <- readIORef env.planStateRef
            let selected = case persisted of
                    Just tracker ->
                        normalizePlanTrackerAfterRestart tracker
                    Nothing
                        | mergeLegacy ->
                            mergeLegacyState
                                legacyState
                                runtime.runtimeTracker
                        | otherwise -> initialPlanTracker
                shouldWrite =
                    maybe
                        (selected /= initialPlanTracker)
                        (/= selected)
                        persisted
                restricted = trackerRestricts selected
            ensureRuntimeQuiesced env runtime selected >>= \case
                Left err -> do
                    when restricted (mirrorTrackerState env selected)
                    pure
                        ( if restricted
                            then runtime { runtimeTracker = selected }
                            else runtime
                        , Left err
                        )
                Right prepared -> do
                    when restricted (mirrorTrackerState env selected)
                    persistedResult <-
                        if shouldWrite
                            then
                                compareAndWritePlanTrackerState
                                    directory
                                    (fromMaybe
                                        initialPlanTracker
                                        persisted)
                                    selected
                            else pure (Right ())
                    case persistedResult of
                        Left err ->
                            pure
                                ( if restricted
                                    then prepared
                                        { runtimeTracker = selected }
                                    else runtime
                                , Left err
                                )
                        Right () -> do
                            mirrorTrackerState env selected
                            setPlanSessionDir env (Just directory)
                            let attached = prepared
                                    { runtimeTracker = selected
                                    , runtimeAttachedDir = Just directory
                                    }
                            finishRuntimeTransition env attached (Right ())

readPlanTracker :: PlanModeEnv -> IO PlanTracker
readPlanTracker env =
    (.runtimeTracker) <$> readMVar env.planTrackerRuntime

-- | Revision of the most recent activation confirmed through the
-- @enter_plan_mode@ tool in this process. This volatile owner marker lets a
-- cancelled turn roll back only its own activation rather than a concurrent
-- user transition.
readPlanAgentActivationRevision :: PlanModeEnv -> IO (Maybe Word64)
readPlanAgentActivationRevision =
    readIORef . (.planAgentActivationRevision)

-- | Persist a candidate before exposing it. Persistence failure leaves the
-- previous tracker and restrictions intact, which is appropriate for reminder
-- updates, approval resolution, and any transition that might relax policy.
updatePlanTracker
    :: PlanModeEnv
    -> (PlanTracker -> Either Text PlanTracker)
    -> IO (Either Text PlanTracker)
updatePlanTracker = changePlanTracker PersistBeforeExpose

-- | Expose a more restrictive candidate before persistence. If persistence
-- fails, the new restriction remains active in memory and the caller receives
-- the error.
restrictPlanTracker
    :: PlanModeEnv
    -> (PlanTracker -> Either Text PlanTracker)
    -> IO (Either Text PlanTracker)
restrictPlanTracker = changePlanTracker ExposeBeforePersist

data TrackerCommitOrder
    = PersistBeforeExpose
    | ExposeBeforePersist

changePlanTracker
    :: TrackerCommitOrder
    -> PlanModeEnv
    -> (PlanTracker -> Either Text PlanTracker)
    -> IO (Either Text PlanTracker)
changePlanTracker order env transition =
    withPlanTransition env do
        runtime <- readMVar env.planTrackerRuntime
        (next, result) <-
            case transition runtime.runtimeTracker of
                Left err -> pure (runtime, Left err)
                Right candidate
                    | Left err <- validatePlanTracker candidate ->
                        pure (runtime, Left err)
                    | otherwise ->
                        ensureRuntimeQuiesced env runtime candidate >>= \case
                            Left err -> pure (runtime, Left err)
                            Right prepared ->
                                commitPlanTracker
                                    order
                                    env
                                    runtime
                                    prepared
                                    candidate
        replacePlanTrackerRuntime env next
        pure result

-- Lifecycle callbacks are application code and may inspect plan state. Keep
-- the runtime MVar available while they run, but fail concurrent or re-entrant
-- mutations closed instead of waiting forever on a non-reentrant lock.
withPlanTransition
    :: PlanModeEnv
    -> IO (Either Text a)
    -> IO (Either Text a)
withPlanTransition env action =
    mask \restore ->
        tryTakeMVar env.planTransitionLock >>= \case
            Nothing ->
                pure (Left "another plan-mode transition is already in progress")
            Just () ->
                restore action `finally` putMVar env.planTransitionLock ()

replacePlanTrackerRuntime :: PlanModeEnv -> PlanTrackerRuntime -> IO ()
replacePlanTrackerRuntime env runtime =
    modifyMVar_ env.planTrackerRuntime (const (pure runtime))

commitPlanTracker
    :: TrackerCommitOrder
    -> PlanModeEnv
    -> PlanTrackerRuntime
    -> PlanTrackerRuntime
    -> PlanTracker
    -> IO (PlanTrackerRuntime, Either Text PlanTracker)
commitPlanTracker order env previous prepared candidate =
    case
        ( effectiveCommitOrder order previous.runtimeTracker candidate
        , prepared.runtimeAttachedDir
        )
    of
        (_, Nothing) -> exposeAndFinish
        (PersistBeforeExpose, Just directory) ->
            compareAndWritePlanTrackerState
                directory
                previous.runtimeTracker
                candidate >>= \case
                Left err -> do
                    restored <- rollbackQuiescence env previous prepared
                    pure (restored, Left err)
                Right () -> exposeAndFinish
        (ExposeBeforePersist, Just directory) -> do
            mirrorTrackerState env candidate
            let exposed = prepared { runtimeTracker = candidate }
            replacePlanTrackerRuntime env exposed
            compareAndWritePlanTrackerState
                directory
                previous.runtimeTracker
                candidate >>= \case
                Left err -> pure (exposed, Left err)
                Right () ->
                    finishRuntimeTransition env exposed
                        (Right candidate)
  where
    exposeAndFinish = do
        mirrorTrackerState env candidate
        finishRuntimeTransition
            env
            (prepared { runtimeTracker = candidate })
            (Right candidate)

effectiveCommitOrder
    :: TrackerCommitOrder
    -> PlanTracker
    -> PlanTracker
    -> TrackerCommitOrder
effectiveCommitOrder requested previous candidate
    | not (trackerRestricts previous) && trackerRestricts candidate =
        requested
    | otherwise = PersistBeforeExpose

ensureRuntimeQuiesced
    :: PlanModeEnv
    -> PlanTrackerRuntime
    -> PlanTracker
    -> IO (Either Text PlanTrackerRuntime)
ensureRuntimeQuiesced env runtime candidate
    | not (trackerRestricts candidate) || runtime.runtimeQuiesced =
        pure (Right runtime)
    | otherwise =
        planQuiesceAction env.planHooks >>= \case
            Left err -> do
                rollback <- tryAny (planResumeAction env.planHooks)
                pure $ Left $ case rollback of
                    Right () -> err
                    Left rollbackErr ->
                        err
                            <> "; rollback also failed: "
                            <> Text.pack
                                (displayException rollbackErr)
            Right () ->
                pure (Right runtime { runtimeQuiesced = True })

rollbackQuiescence
    :: PlanModeEnv
    -> PlanTrackerRuntime
    -> PlanTrackerRuntime
    -> IO PlanTrackerRuntime
rollbackQuiescence env previous prepared
    | not previous.runtimeQuiesced && prepared.runtimeQuiesced = do
        _ <- tryAny (planResumeAction env.planHooks)
        pure previous
    | otherwise = pure previous

finishRuntimeTransition
    :: PlanModeEnv
    -> PlanTrackerRuntime
    -> Either Text a
    -> IO (PlanTrackerRuntime, Either Text a)
finishRuntimeTransition env runtime result
    | runtime.runtimeQuiesced
    , not (trackerRestricts runtime.runtimeTracker) = do
        replacePlanTrackerRuntime env runtime
        resumed <- tryAny (planResumeAction env.planHooks)
        let finished = runtime { runtimeQuiesced = False }
        replacePlanTrackerRuntime env finished
        pure
            ( finished
            , case resumed of
                Left err ->
                    Left
                        ("plan mode relaxed, but its resume callback failed: "
                            <> Text.pack (displayException err))
                Right () -> result
            )
    | otherwise = do
        replacePlanTrackerRuntime env runtime
        pure (runtime, result)

planQuiesceAction :: PlanModeHooks -> IO (Either Text ())
planQuiesceAction = \case
    PlanModeHooks{} -> pure (Right ())
    PlanModeLifecycleHooks{planQuiesceBeforeActivation} ->
        tryAny planQuiesceBeforeActivation >>= \case
            Left err ->
                pure
                    (Left
                        ("could not quiesce before entering plan mode: "
                            <> Text.pack (displayException err)))
            Right result -> pure result

planResumeAction :: PlanModeHooks -> IO ()
planResumeAction = \case
    PlanModeHooks{} -> pure ()
    PlanModeLifecycleHooks{planResumeAfterExit} ->
        planResumeAfterExit

trackerCanReattach :: PlanTracker -> Bool
trackerCanReattach tracker =
    not (trackerRestricts tracker)
        && tracker.trackerPendingApproval == Nothing
        && tracker.trackerApprovedContinuation == Nothing

isPlanModeActive :: PlanModeEnv -> IO Bool
isPlanModeActive env = do
    state <- readPlanModeState env
    pure (state `elem` [PlanActive, PlanExitPending])

activatePlanMode :: PlanModeEnv -> IO ()
activatePlanMode env = writePlanModeState env PlanActive

deactivatePlanMode :: PlanModeEnv -> IO ()
deactivatePlanMode env =
    updatePlanTracker env transition >>= \case
        Left err -> throwString (Text.unpack err)
        Right _ -> pure ()
  where
    transition tracker =
        Right
            (deactivatePlanTracker
                (trackerRestricts tracker)
                tracker)

legacyStateTransition :: PlanModeState -> PlanTracker -> PlanTracker
legacyStateTransition = \case
    PlanInactive -> deactivatePlanTracker False
    PlanPending ->
        requestPlanActivation . deactivatePlanTracker False
    PlanActive -> activatePlanTracker
    -- There is no safe way to synthesize an exit-pending state without its
    -- generation, digest, and request key.
    PlanExitPending -> id

mergeLegacyState :: PlanModeState -> PlanTracker -> PlanTracker
mergeLegacyState legacy tracker
    | tracker /= initialPlanTracker = tracker
    | otherwise = case legacy of
        PlanInactive -> tracker
        _ -> legacyStateTransition legacy tracker

trackerRestricts :: PlanTracker -> Bool
trackerRestricts tracker =
    tracker.trackerPhase `elem` [TrackerActive, TrackerExitPending]

mirrorTrackerState :: PlanModeEnv -> PlanTracker -> IO ()
mirrorTrackerState env tracker =
    writeIORef env.planStateRef $ case tracker.trackerPhase of
        TrackerInactive -> PlanInactive
        TrackerPending -> PlanPending
        TrackerActive -> PlanActive
        TrackerExitPending -> PlanExitPending

readPlanMarkdown :: PlanModeEnv -> IO Text
readPlanMarkdown env =
    readPlanSnapshot env >>= \case
        PlanAbsent -> pure ""
        PlanPresent snapshot -> pure snapshot.planSnapshotMarkdown
        PlanUnreadable err ->
            throwString (Text.unpack (renderPlanFileError err))

readPlanSnapshot :: PlanModeEnv -> IO PlanReadResult
readPlanSnapshot env =
    planFilePath env >>= readPlanFile

writePlanMarkdown :: PlanModeEnv -> Text -> IO (Either Text ())
writePlanMarkdown env content =
    fmap (either (Left . renderPlanFileError) (const (Right ())))
        (writePlanSnapshot env content)

writePlanSnapshot
    :: PlanModeEnv
    -> Text
    -> IO (Either PlanFileError PlanSnapshot)
writePlanSnapshot env content = do
    path <- planFilePath env
    writePlanFile path content

ensurePlanMarkdown
    :: PlanModeEnv
    -> IO (Either PlanFileError PlanSnapshot)
ensurePlanMarkdown env =
    planFilePath env >>= ensurePlanFile

-- | Submit the authoritative plan snapshot through the correlated review
-- protocol. This is the single entry point for both tool-based and
-- assistant-text-based completion flows.
submitPlanForReview
    :: PlanModeEnv
    -> Maybe Text
    -> IO (Either Text PlanReviewOutcome)
submitPlanForReview env summary = do
    path <- planFilePath env
    readPlanFile path >>= \case
        PlanAbsent ->
            failOrRevisePending
                env
                "The plan file is missing. Return to planning and write it again."
        PlanUnreadable err ->
            failOrRevisePending env (renderPlanFileError err)
        PlanPresent snapshot ->
            preparePlanReview env snapshot >>= \case
                Left err -> pure (Left err)
                Right pending
                    | pending.pendingPlanDigest
                        /= snapshot.planSnapshotDigest ->
                            reviseStalePlan
                                env
                                pending
                                "The plan changed after this review request was created."
                    | otherwise -> do
                        let document =
                                parsePlanDocument
                                    snapshot.planSnapshotMarkdown
                            request = PlanReviewRequest
                                { planReviewRequestKey =
                                    pending.pendingPlanRequestKey
                                , planReviewPath = path
                                , planReviewSnapshotDigest =
                                    snapshot.planSnapshotDigest
                                , planReviewMarkdown =
                                    snapshot.planSnapshotMarkdown
                                , planReviewWarnings =
                                    document.planDocumentWarnings
                                , planReviewVerification =
                                    document.planDocumentVerification
                                , planReviewSummary = summary
                                }
                        invokePlanReviewHook env.planHooks request >>= \case
                            Left err -> pure (Left err)
                            Right decision ->
                                finishPlanReview
                                    env
                                    pending
                                    request
                                    decision

preparePlanReview
    :: PlanModeEnv
    -> PlanSnapshot
    -> IO (Either Text PendingPlanApproval)
preparePlanReview env snapshot =
    updatePlanTracker env transition >>= \case
        Left err -> pure (Left err)
        Right tracker ->
            pure $ case tracker.trackerPendingApproval of
                Just pending -> Right pending
                Nothing ->
                    Left "plan review did not produce a pending approval"
  where
    transition tracker = case tracker.trackerPhase of
        TrackerActive ->
            let generation =
                    PlanGeneration
                        (tracker.trackerGeneration.unPlanGeneration + 1)
                requestKey =
                    planReviewRequestKey
                        generation
                        snapshot.planSnapshotDigest
            in firstTrackerError
                (beginPlanExit
                    requestKey
                    snapshot.planSnapshotDigest
                    tracker)
        TrackerExitPending -> Right tracker
        TrackerInactive ->
            Left "plan mode is not active"
        TrackerPending ->
            Left "plan mode activation is still pending"

finishPlanReview
    :: PlanModeEnv
    -> PendingPlanApproval
    -> PlanReviewRequest
    -> PlanReviewDecision
    -> IO (Either Text PlanReviewOutcome)
finishPlanReview env pending request decision =
    readPlanFile request.planReviewPath >>= \case
        PlanAbsent ->
            reviseStalePlan
                env
                pending
                "The plan file disappeared while it was being reviewed."
        PlanUnreadable err ->
            reviseStalePlan env pending (renderPlanFileError err)
        PlanPresent current
            | current.planSnapshotDigest
                /= request.planReviewSnapshotDigest ->
                    reviseStalePlan
                        env
                        pending
                        "The plan changed while it was being reviewed."
            | otherwise ->
                applyPlanReviewDecision env pending request decision

applyPlanReviewDecision
    :: PlanModeEnv
    -> PendingPlanApproval
    -> PlanReviewRequest
    -> PlanReviewDecision
    -> IO (Either Text PlanReviewOutcome)
applyPlanReviewDecision env pending request = \case
    PlanReviewDefer ->
        pure (Right (PlanReviewDeferred request))
    PlanReviewApprove
        | not (null request.planReviewWarnings) ->
            pure
                (Right
                    (PlanReviewApprovalOverrideRequired request))
    PlanReviewApprove ->
        acceptPlanReview env pending request
    PlanReviewApproveAnyway ->
        acceptPlanReview env pending request
    PlanReviewRequestChanges notes ->
        resolveReview env pending RevisePlan >>= \case
            Left err -> pure (Left err)
            Right _ ->
                pure
                    (Right
                        (PlanReviewRevisionRequired
                            (nonBlankPlanNotes notes)))
    PlanReviewAbandon ->
        resolveReview env pending AbandonPlan >>= \case
            Left err -> pure (Left err)
            Right _ -> pure (Right PlanReviewAbandoned)

acceptPlanReview
    :: PlanModeEnv
    -> PendingPlanApproval
    -> PlanReviewRequest
    -> IO (Either Text PlanReviewOutcome)
acceptPlanReview env pending request = do
    let continuation = ApprovedPlanContinuation
            { approvedPlanDigest = request.planReviewSnapshotDigest
            , approvedPlanVerification = request.planReviewVerification
            , approvedPlanContinuation =
                approvedContinuationWithVerification
                    request.planReviewVerification
            }
    resolveReview env pending (ApprovePlan continuation) >>= \case
        Left err -> pure (Left err)
        Right _ -> pure (Right (PlanReviewAccepted continuation))

resolveReview
    :: PlanModeEnv
    -> PendingPlanApproval
    -> PlanApprovalResolution
    -> IO (Either Text PlanTracker)
resolveReview env pending resolution =
    updatePlanTracker env \tracker -> do
        resolved <-
            firstTrackerError
                (resolvePlanApproval
                    pending.pendingPlanGeneration
                    pending.pendingPlanDigest
                    resolution
                    tracker)
        pure $ case resolution of
            RevisePlan -> resolved
            _ -> queuePlanExitNotice resolved

reviseStalePlan
    :: PlanModeEnv
    -> PendingPlanApproval
    -> Text
    -> IO (Either Text PlanReviewOutcome)
reviseStalePlan env pending message =
    resolveReview env pending RevisePlan >>= \case
        Left err -> pure (Left err)
        Right _ -> pure (Right (PlanReviewRevisionRequired message))

failOrRevisePending
    :: PlanModeEnv
    -> Text
    -> IO (Either Text PlanReviewOutcome)
failOrRevisePending env message = do
    tracker <- readPlanTracker env
    case tracker.trackerPendingApproval of
        Just pending -> reviseStalePlan env pending message
        Nothing -> pure (Left message)

invokePlanReviewHook
    :: PlanModeHooks
    -> PlanReviewRequest
    -> IO (Either Text PlanReviewDecision)
invokePlanReviewHook hooks request =
    tryAny action >>= \case
        Left err ->
            pure
                (Left
                    ("plan review failed while awaiting a decision: "
                        <> Text.pack (displayException err)))
        Right decision -> pure (Right decision)
  where
    action = case hooks of
        PlanModeHooks{planDecideExit} ->
            legacyPlanReviewHook planDecideExit request
        PlanModeLifecycleHooks{planReviewPlan} ->
            planReviewPlan request

legacyPlanReviewHook
    :: (Text -> IO PlanDecision)
    -> PlanReviewRequest
    -> IO PlanReviewDecision
legacyPlanReviewHook decide request =
    decide (legacyPlanReviewBody request) >>= \case
        PlanApprove -> pure PlanReviewApprove
        PlanRequestChanges notes ->
            pure (PlanReviewRequestChanges notes)
        PlanCancel -> pure PlanReviewAbandon

legacyPlanReviewBody :: PlanReviewRequest -> Text
legacyPlanReviewBody request =
    summary <> request.planReviewMarkdown <> warnings
  where
    summary = case request.planReviewSummary of
        Just value
            | not (Text.null (Text.strip value)) ->
                value <> "\n\n"
        _ -> ""
    warnings = case request.planReviewWarnings of
        [] -> ""
        values ->
            "\n\nAdvisory plan warnings:\n"
                <> Text.unlines
                    [ "- " <> value.planWarningMessage
                    | value <- values
                    ]

planReviewRequestKey :: PlanGeneration -> PlanDigest -> Text
planReviewRequestKey generation digest =
    "plan-review-v1:"
        <> Text.pack (show generation.unPlanGeneration)
        <> ":"
        <> digest.unPlanDigest

firstTrackerError
    :: Show error
    => Either error value
    -> Either Text value
firstTrackerError =
    either (Left . Text.pack . show) Right

nonBlankPlanNotes :: Text -> Text
nonBlankPlanNotes notes
    | Text.null (Text.strip notes) = "(no review notes)"
    | otherwise = Text.strip notes

planModeReminder :: PlanCompletion -> OsPath -> Text
planModeReminder completion path =
    Text.unlines
        [ "Plan mode is active. Do not make any edits or writes to the system except for the plan file."
        , ""
        , "## Plan File"
        , "Write your plan to `" <> toText path <> "`."
        , "Use `write_plan` when it is available; otherwise use the provider's dedicated plan-file edit tool."
        , "That is the only file you may create or modify."
        , "Inspect the repository with dedicated read-only tools such as `read_file`, `grep`, and `list_dir`; shell tools are unavailable in Plan Mode."
        , ""
        , completionInstruction completion
        ]

-- | Persistently alternate full and sparse reminders, use a dedicated first
-- reminder on re-entry, and consume a one-shot post-exit notice only after its
-- state update is durable.
nextPlanModeReminder
    :: PlanModeEnv
    -> PlanReminderToolNames
    -> IO (Either Text (Maybe PlanReminder))
nextPlanModeReminder env tools = do
    tracker <- readPlanTracker env
    if tracker.trackerExitNoticePending
        then consumePostExitReminder env tracker
        else case tracker.trackerPhase of
            TrackerActive -> activeReminder env tools tracker
            _ -> pure (Right Nothing)

consumePostExitReminder
    :: PlanModeEnv
    -> PlanTracker
    -> IO (Either Text (Maybe PlanReminder))
consumePostExitReminder env expected =
    updatePlanTracker env transition >>= \case
        Left err -> pure (Left err)
        Right _ ->
            pure
                (Right
                    (Just PlanReminder
                        { planReminderKind = PlanReminderPostExit
                        , planReminderText =
                            "You have exited plan mode. You can now make edits, run tools, and take actions."
                        }))
  where
    transition current
        | current.trackerRevision /= expected.trackerRevision =
            Left "plan mode changed while preparing its post-exit reminder"
        | otherwise = Right (consumePlanExitNotice current)

activeReminder
    :: PlanModeEnv
    -> PlanReminderToolNames
    -> PlanTracker
    -> IO (Either Text (Maybe PlanReminder))
activeReminder env tools expected = do
    path <- planFilePath env
    readPlanFile path >>= \case
        PlanUnreadable err -> pure (Left (renderPlanFileError err))
        snapshotResult -> do
            let hasContent = case snapshotResult of
                    PlanPresent snapshot ->
                        not
                            (Text.null
                                (Text.strip snapshot.planSnapshotMarkdown))
                    PlanAbsent -> False
                kind
                    | expected.trackerReentered
                        && expected.trackerReminderCount == 0 =
                            PlanReminderReentry
                    | even expected.trackerReminderCount =
                        PlanReminderFull
                    | otherwise = PlanReminderSparse
                reminder = PlanReminder
                    { planReminderKind = kind
                    , planReminderText =
                        renderTrackedPlanReminder
                            kind
                            tools
                            path
                            hasContent
                    }
            updatePlanTracker env (advance expected) >>= \case
                Left err -> pure (Left err)
                Right _ -> pure (Right (Just reminder))
  where
    advance
        :: PlanTracker
        -> PlanTracker
        -> Either Text PlanTracker
    advance expectedTracker current
        | current.trackerRevision /= expectedTracker.trackerRevision =
            Left "plan mode changed while preparing its reminder"
        | not (trackerRestricts current) =
            Left "plan mode ended while preparing its reminder"
        | otherwise = Right (notePlanReminder current)

renderTrackedPlanReminder
    :: PlanReminderKind
    -> PlanReminderToolNames
    -> OsPath
    -> Bool
    -> Text
renderTrackedPlanReminder kind tools path hasContent =
    case kind of
        PlanReminderSparse ->
            "Plan mode is still active. Do not make any edits or writes to the system except for the plan file."
        PlanReminderPostExit ->
            "You have exited plan mode. You can now make edits, run tools, and take actions."
        PlanReminderReentry ->
            Text.unlines
                [ "## Returning to Plan Mode"
                , ""
                , "You are entering plan mode again. The existing plan file is at `"
                    <> toText path
                    <> "`."
                , completionToolsInstruction tools
                ]
        PlanReminderFull ->
            Text.unlines
                [ "Plan mode is active. Do not make any edits or writes to the system except for the plan file."
                , ""
                , "## Plan File"
                , if hasContent
                    then
                        "A plan exists at `"
                            <> toText path
                            <> "`. Read and update it with `"
                            <> tools.planReminderWriteToolName
                            <> "`."
                    else
                        "No plan is written yet. Write it to `"
                            <> toText path
                            <> "` with `"
                            <> tools.planReminderWriteToolName
                            <> "`."
                , "That is the only file you may create or modify."
                , completionToolsInstruction tools
                ]

completionToolsInstruction :: PlanReminderToolNames -> Text
completionToolsInstruction tools =
    "End the planning turn only with `"
        <> tools.planReminderQuestionToolName
        <> "` to clarify requirements or `"
        <> tools.planReminderCompletionToolName
        <> "` to present the plan."

completionInstruction :: PlanCompletion -> Text
completionInstruction = \case
    CompleteWithExitTool ->
        "When the plan is ready, call `exit_plan_mode` so the user can approve, request changes, or cancel."
    CompleteWithProposedPlan ->
        "When the plan is ready, end your turn with a complete `<proposed_plan>` … `</proposed_plan>` block so the user can approve, request changes, or cancel."

planApprovedContinuation :: Text
planApprovedContinuation =
    "The user approved the plan. Plan mode is now off. "
        <> "Begin implementing the approved plan immediately. "
        <> "Do not wait for another user message."

approvedContinuationWithVerification :: [Text] -> Text
approvedContinuationWithVerification [] = planApprovedContinuation
approvedContinuationWithVerification verification =
    planApprovedContinuation
        <> "\n\nRun these verification steps before reporting completion:\n"
        <> Text.unlines ["- " <> step | step <- verification]

planModeBlockedEditMessage :: OsPath -> Text
planModeBlockedEditMessage path =
    "Rejected: file edits are not allowed in plan mode - the only editable file is the plan file ("
        <> toText path
        <> ")."

-- | Compare paths only after the caller has resolved both to canonical paths.
-- A raw @plan.md@ basename is deliberately not authorization.
isPlanFileEditTarget :: OsPath -> OsPath -> Bool
isPlanFileEditTarget = equalFilePath

--------------------------------------------------------------------------------
-- Grok-build tools
--------------------------------------------------------------------------------

data EnterPlanArgs = EnterPlanArgs
    { explanation :: Maybe Text
    }

enterPlanArgsDecoder :: Decoder EnterPlanArgs
enterPlanArgsDecoder = objectArgs \object -> EnterPlanArgs
        <$> optText object "explanation"

enterPlanModeTool :: PlanModeEnv -> AppTool
enterPlanModeTool env =
    enterPlanModeToolWith CompleteWithExitTool env

enterCodexPlanModeTool :: PlanModeEnv -> AppTool
enterCodexPlanModeTool env =
    enterPlanModeToolWith CompleteWithProposedPlan env

enterPlanModeToolWith :: PlanCompletion -> PlanModeEnv -> AppTool
enterPlanModeToolWith completion env =
    withPlanModeCapability PlanModeInteraction $
    withToolBatchPhase ToolBatchModeBarrier $
        jsonTool "enter_plan_mode"
            (enterPlanDescription completion)
            [ PropertySchema "explanation" PropertyString False $ Just
                "Optional reason this task needs a planning phase before implementation."
            ]
            -- The tool performs its own explicit user confirmation through
            -- planConfirmEnter, so it must not also trigger generic tool approval.
            True
            TurnSequential
            (typedToolWithCall "enter_plan_mode" enterPlanArgsDecoder
                (runEnterPlanMode completion env))

enterPlanDescription :: PlanCompletion -> Text
enterPlanDescription completion =
    "Enter plan mode when a task has genuine architectural ambiguity.\n\
    \Requires user approval. While active, only the session plan.md file may be edited;\n\
    \explore the codebase, write the plan, then "
        <> case completion of
            CompleteWithExitTool -> "call exit_plan_mode for approval."
            CompleteWithProposedPlan ->
                "present it in a complete <proposed_plan> block for approval."

runEnterPlanMode
    :: PlanCompletion
    -> PlanModeEnv
    -> ToolCall
    -> EnterPlanArgs
    -> IO (Either Text Text)
runEnterPlanMode completion env call args = do
    tracker <- readPlanTracker env
    case tracker.trackerApprovedContinuation of
        Just _ ->
            pure
                (Left
                    "An approved plan is still waiting to run. Continue that plan before entering plan mode again.")
        Nothing -> do
            active <- isPlanModeActive env
            if active
                then pure $ Right "Plan mode is already active."
                else do
                    let reason = fromMaybe "Enter plan mode to design an approach before coding." args.explanation
                    ok <- invokeEnterHook env.planHooks PlanEnterRequest
                        { planEnterRequestKey = call.callId
                        , planEnterReason = reason
                        }
                    if not ok
                        then pure $ Left "User declined plan mode. Stay in normal mode and continue."
                        else do
                            path <- planFilePath env
                            ensurePlanFile path >>= \case
                                Left err ->
                                    pure (Left (renderPlanFileError err))
                                Right _ -> do
                                    activatePlanMode env
                                    activated <- readPlanTracker env
                                    writeIORef
                                        env.planAgentActivationRevision
                                        (Just activated.trackerRevision)
                                    pure $ Right $
                                        "You have entered plan mode. Explore the codebase and write an implementation plan to "
                                            <> toText path
                                            <> ". "
                                            <> completionInstruction completion

invokeEnterHook :: PlanModeHooks -> PlanEnterRequest -> IO Bool
invokeEnterHook hooks request =
    case hooks of
        PlanModeHooks{planConfirmEnter} ->
            planConfirmEnter request.planEnterReason
        PlanModeLifecycleHooks{planConfirmEnterRequest} ->
            planConfirmEnterRequest request

data WritePlanArgs = WritePlanArgs
    { content :: Text
    }

writePlanArgsDecoder :: Decoder WritePlanArgs
writePlanArgsDecoder = objectArgs \object ->
        WritePlanArgs <$> reqText object "content"

writePlanTool :: PlanModeEnv -> AppTool
writePlanTool env =
    withPlanModeCapability
        (PlanModePlanFileWrite (\_ -> Right <$> planFilePath env)) $
    jsonTool "write_plan" writePlanDescription
    [ PropertySchema "content" PropertyString True $ Just
        "Complete Markdown content to store in the session plan.md file."
    ]
    True
    TurnSequential
    (typedTool "write_plan" writePlanArgsDecoder (runWritePlan env))

writePlanDescription :: Text
writePlanDescription =
    "Write the current implementation plan to the session plan.md file.\n\
    \This tool is available only while Plan Mode is active and cannot write any other path."

runWritePlan :: PlanModeEnv -> WritePlanArgs -> IO (Either Text Text)
runWritePlan env args = do
    readPlanModeState env >>= \case
        PlanActive ->
            writePlanMarkdown env args.content >>= \case
                Left err -> pure (Left err)
                Right () -> do
                    path <- planFilePath env
                    pure $ Right $
                        "Wrote the plan to " <> toText path
                            <> ". Continue planning or present it for approval when ready."
        PlanExitPending ->
            pure
                (Left
                    "The submitted plan snapshot is awaiting review and is frozen. Resolve or revise that review before writing it again.")
        _ -> pure (Left "Plan mode is not active.")

data ExitPlanArgs = ExitPlanArgs
    { summary :: Maybe Text
    }

exitPlanArgsDecoder :: Decoder ExitPlanArgs
exitPlanArgsDecoder = objectArgs \object -> ExitPlanArgs
        <$> optText object "summary"

exitPlanModeTool :: PlanModeEnv -> AppTool
exitPlanModeTool env =
    withPlanModeCapability PlanModeInteraction $
    withToolBatchPhase ToolBatchTerminal $
        jsonTool "exit_plan_mode" exitPlanDescription
            [ PropertySchema "summary" PropertyString False $ Just
                "Optional short summary shown with the plan approval prompt."
            ]
            False
            TurnSequential
            (typedTool "exit_plan_mode" exitPlanArgsDecoder
                (runExitPlanMode env))

exitPlanDescription :: Text
exitPlanDescription =
    "Present the plan file for user approval and leave plan mode if approved.\n\
    \The plan is read from plan.md on disk (not passed as an argument).\n\
    \The user may approve (start implementing), request changes (stay in plan mode),\n\
    \or cancel (abandon the plan and turn plan mode off)."

runExitPlanMode :: PlanModeEnv -> ExitPlanArgs -> IO (Either Text Text)
runExitPlanMode env args = do
    active <- isPlanModeActive env
    if not active
        then pure $ Left "Plan mode is not active."
        else submitPlanForReview env args.summary >>= \case
            Left err -> pure (Left err)
            Right (PlanReviewAccepted continuation) ->
                pure (Right continuation.approvedPlanContinuation)
            Right (PlanReviewRevisionRequired notes) ->
                pure $ Right $
                    "The plan needs revision. Stay in plan mode and revise plan.md.\n"
                        <> "Feedback:\n"
                        <> notes
            Right (PlanReviewApprovalOverrideRequired request) ->
                pure $ Left $
                    "The plan has advisory warnings and was not approved. "
                        <> "The user must explicitly choose approve-anyway.\n"
                        <> Text.unlines
                            [ "- " <> warning.planWarningMessage
                            | warning <- request.planReviewWarnings
                            ]
            Right PlanReviewAbandoned ->
                pure $ Right
                    "The user abandoned the plan. Plan mode is off. Do not call exit_plan_mode again unless asked to re-enter plan mode."
            Right (PlanReviewDeferred request) ->
                pure $ Right $
                    "Plan review was deferred. Plan mode remains write-restricted; "
                        <> "retry the same review request later ("
                        <> request.planReviewRequestKey
                        <> ")."

data AskUserQuestionOption = AskUserQuestionOption
    { label :: Text
    , description :: Text
    , preview :: Maybe Text
    }
    deriving (Eq, Show)

askUserQuestionOptionDecoder :: Decoder AskUserQuestionOption
askUserQuestionOptionDecoder = objectArgs \object ->
        AskUserQuestionOption
            <$> reqText object "label"
            <*> reqText object "description"
            <*> optText object "preview"

data AskUserQuestion = AskUserQuestion
    { question :: Text
    , options :: [AskUserQuestionOption]
    , multiSelect :: Maybe Bool
    }
    deriving (Eq, Show)

askUserQuestionDecoder :: Decoder AskUserQuestion
askUserQuestionDecoder = objectArgs \object -> do
        question <- reqText object "question"
        options <- optList askUserQuestionOptionDecoder object "options" "Expected array for key: options"
            >>= maybe (fail "Missing parameter: options") pure
        multiSelectSnake <- optBool object "multi_select"
        multiSelectCamel <- optBool object "multiSelect"
        let multiSelect = multiSelectSnake <|> multiSelectCamel
        pure AskUserQuestion { question, options, multiSelect }

newtype AskUserQuestionArgs = AskUserQuestionArgs
    { questions :: [AskUserQuestion]
    }

askUserQuestionArgsDecoder :: Decoder AskUserQuestionArgs
askUserQuestionArgsDecoder = objectArgs \object -> do
        modern <- optList askUserQuestionDecoder object "questions" "Expected array for key: questions"
        case modern of
            Just questions -> pure AskUserQuestionArgs { questions }
            Nothing -> do
                -- Compatibility with the original single-question shape.
                question <- reqText object "question"
                rawOptions <- optText object "options"
                let options =
                        [ AskUserQuestionOption
                            { label = choice
                            , description = ""
                            , preview = Nothing
                            }
                        | choice <- parseOptions (fromMaybe "" rawOptions)
                        ]
                pure AskUserQuestionArgs
                    { questions =
                        [ AskUserQuestion
                            { question
                            , options
                            , multiSelect = Nothing
                            }
                        ]
                    }

askUserQuestionTool :: PlanModeEnv -> AppTool
askUserQuestionTool env =
    withPlanModeCapability PlanModeInteraction $
    jsonTool "ask_user_question" askUserDescription
    [ PropertySchema "questions"
        (PropertyArray (PropertyObject
            [ PropertySchema "question" PropertyString True $ Just
                "The question to ask, phrased as a full question."
            , PropertySchema "options"
                (PropertyArray (PropertyObject
                    [ PropertySchema "label" PropertyString True $ Just
                        "Option text shown to the user. A few words at most."
                    , PropertySchema "description" PropertyString True $ Just
                        "What picking this option means or implies."
                    , PropertySchema "preview" PropertyString False $ Just
                        "Optional content shown while the option is focused, such as a mockup or code snippet. Single-select questions only."
                    ]))
                True
                (Just "The choices for this question.")
            , PropertySchema "multi_select" PropertyBoolean False $ Just
                "Let the user pick more than one option (default false)."
            ]))
        True
        (Just "The questions to ask, each with its own options.")
    ]
    True
    TurnSequential
    (typedToolWithCall
        "ask_user_question"
        askUserQuestionArgsDecoder
        (runAskUserQuestion env))

askUserDescription :: Text
askUserDescription =
    "Ask the user one or more multiple-choice questions. "
        <> "This tool works both inside and outside plan mode."

runAskUserQuestion
    :: PlanModeEnv
    -> ToolCall
    -> AskUserQuestionArgs
    -> IO (Either Text Text)
runAskUserQuestion env call args
    | null args.questions =
        pure (Right "No questions provided. Continue with the task.")
    | otherwise = do
        let request = PlanQuestionnaireRequest
                { planQuestionnaireRequestKey = call.callId
                , planQuestionnaireQuestions = args.questions
                }
        decision <- invokeQuestionnaireHook env.planHooks request
        case validatePlanQuestionnaireDecision request decision of
            Left err -> pure (Left err)
            Right outcome -> case outcome of
                PlanQuestionnaireSubmitted answers ->
                    pure (Right (formatAnswers answers))
                PlanQuestionnaireClarification clarification ->
                    pure $ Right $
                        "The user requested clarification before answering: "
                            <> Text.strip clarification
                PlanQuestionnaireFinished ->
                    pure
                        (Right
                            "The user finished the questionnaire without providing further answers. Continue using the information already available.")
                PlanQuestionnaireCancelled ->
                    pure
                        (Right
                            "The user declined to answer the questionnaire. Continue without those answers.")
                PlanQuestionnaireDeferred ->
                    pure
                        (Left
                            "The questionnaire remains unanswered. Wait for the user or retry the same request later.")

invokeQuestionnaireHook
    :: PlanModeHooks
    -> PlanQuestionnaireRequest
    -> IO PlanQuestionnaireDecision
invokeQuestionnaireHook hooks request =
    case hooks of
        PlanModeHooks{planAskQuestion} ->
            legacyPlanQuestionnaireHook planAskQuestion request
        PlanModeLifecycleHooks{planAskQuestionnaire} ->
            planAskQuestionnaire request

legacyPlanQuestionnaireHook
    :: (Text -> [Text] -> IO (Maybe Text))
    -> PlanQuestionnaireRequest
    -> IO PlanQuestionnaireDecision
legacyPlanQuestionnaireHook askHook request =
    collectAnswers 0 questions >>= \case
        Left _ -> pure PlanQuestionnaireDeferred
        Right answers -> pure (PlanQuestionnaireSubmitted answers)
  where
    questions = request.planQuestionnaireQuestions

    collectAnswers
        :: Int
        -> [AskUserQuestion]
        -> IO (Either Text [PlanQuestionnaireAnswer])
    collectAnswers _ [] = pure (Right [])
    collectAnswers index (question : rest) =
        ask index question >>= \case
            Left err -> pure (Left err)
            Right answer ->
                collectAnswers (index + 1) rest >>= \case
                    Left err -> pure (Left err)
                    Right answers -> pure (Right (answer : answers))

    ask
        :: Int
        -> AskUserQuestion
        -> IO (Either Text PlanQuestionnaireAnswer)
    ask index question
        | question.multiSelect == Just True =
            askMultiple question >>= \case
                Left err -> pure (Left err)
                Right (labels, other) ->
                    pure
                        (Right PlanQuestionnaireAnswer
                            { planAnswerQuestionIndex = index
                            , planAnswerQuestion = question.question
                            , planAnswerLabels = labels
                            , planAnswerOther = other
                            })
        | otherwise = do
            let choices = map formatOption question.options
                labelsByChoice =
                    Map.fromList (zip choices (map (.label) question.options))
            answer <- askHook question.question choices
            pure $ case answer of
                Nothing -> Left "No answer from user."
                Just text | Text.null (Text.strip text) ->
                    Left "No answer from user."
                Just text -> Right PlanQuestionnaireAnswer
                    { planAnswerQuestionIndex = index
                    , planAnswerQuestion = question.question
                    , planAnswerLabels =
                        maybe [] pure (Map.lookup text labelsByChoice)
                    , planAnswerOther =
                        case Map.lookup text labelsByChoice of
                            Just _ -> Nothing
                            Nothing -> Just (Text.strip text)
                    }

    askMultiple
        :: AskUserQuestion
        -> IO (Either Text ([Text], Maybe Text))
    askMultiple question =
        choose [] question.options
      where
        doneChoice = "Done selecting"
        choose selected remaining = do
            let displayed = map formatOption remaining
                labelsByDisplayed =
                    Map.fromList (zip displayed (map (.label) remaining))
                choices = displayed <> [doneChoice]
                prompt
                    | null selected = question.question
                    | otherwise =
                        question.question
                            <> "\nSelected: "
                            <> Text.intercalate ", " (reverse selected)
            answer <- askHook prompt choices
            case answer of
                Nothing -> noAnswer selected
                Just raw
                    | Text.null (Text.strip raw) -> noAnswer selected
                    | raw == doneChoice ->
                        if null selected
                            then pure (Left "No answer from user.")
                            else pure (Right (reverse selected, Nothing))
                    | Just label <-
                        Map.lookup raw labelsByDisplayed ->
                            choose
                                (label : selected)
                                [ option
                                | option <- remaining
                                , option.label /= label
                                ]
                    | otherwise ->
                        -- Non-TUI hooks may return a comma-separated answer
                        -- directly rather than one displayed choice at a time.
                        let
                            parts = parseOptions raw
                            known =
                                [ part
                                | part <- parts
                                , part `elem` map (.label) question.options
                                ]
                            unknown =
                                [ part
                                | part <- parts
                                , part `notElem` known
                                ]
                        in pure
                            (Right
                                ( known
                                , nonBlankText (Text.intercalate ", " unknown)
                                ))

        noAnswer selected
            | null selected = pure (Left "No answer from user.")
            | otherwise = pure (Right (reverse selected, Nothing))

formatOption :: AskUserQuestionOption -> Text
formatOption option =
    Text.intercalate " — " $
        [option.label]
            <> [option.description | nonBlank option.description]
            <> [ "Preview: " <> Text.replace "\n" "\\n" preview
               | Just preview <- [option.preview]
               , nonBlank preview
               ]
  where
    nonBlank = not . Text.null . Text.strip

formatAnswers :: [PlanQuestionnaireAnswer] -> Text
formatAnswers answers =
    "User has answered your questions: "
        <> Text.intercalate ", "
            [ "\""
                <> answer.planAnswerQuestion
                <> "\"=\""
                <> renderAnswer answer
                <> "\""
            | answer <- answers
            ]
        <> ". You can now continue with the user's answers in mind."
  where
    renderAnswer :: PlanQuestionnaireAnswer -> Text
    renderAnswer answer =
        Text.intercalate ", " $
            answer.planAnswerLabels
                <> maybe [] pure answer.planAnswerOther

nonBlankText :: Text -> Maybe Text
nonBlankText value
    | Text.null (Text.strip value) = Nothing
    | otherwise = Just (Text.strip value)

parseOptions :: Text -> [Text]
parseOptions raw =
    filter (not . Text.null)
        [ Text.strip part
        | part <- Text.split (\c -> c == '\n' || c == ',') raw
        ]
