-- | Execute one model turn and commit its observable session state.
module Agent.CLI.Turn
    ( applyPendingSessionTitles
    , consumeInteractionDeliveries
    , grokFirstTurnPrefix
    , grokFrameLastUserInput
    , grokUserQuery
    , reviewAfterDurableAppend
    , restorePlanStateAfterIncomplete
    , retryCheckpointedTurn
    , runOneTurn
    ) where

import Agent.Cancel (resetCancel)
import Agent.CLI.CancelWatch (withEscCancel)
import Agent.CLI.Interrupt (withTurnCancel)
import Agent.CLI.Plan
    ( ProposedPlanParseError(..)
    , parseProposedPlan
    , renderProposedPlanParseError
    )
import Agent.CLI.ProviderFallback (isProviderUnavailable)
import Agent.CLI.PendingInteraction
    ( DurableInteractionDelivery(..)
    , recoverUndeliveredInteractions
    , renderPendingInteractionError
    , renderRestoredInteractions
    , resolvedPlanEnterDecision
    )
import Agent.CLI.ProviderTransition
    ( PendingTurn(..)
    , TurnResult(..)
    )
import Agent.CLI.TUI.App
    ( commitFullscreenHistoryTurn
    , emitUiEvent
    )
import Agent.CLI.TUI.SessionHistory (sessionHistoryTurn)
import Agent.CLI.TUI.Types (HistoryCommit(..))
import Agent.TUI.Model
    ( BlockState(..)
    , UiEvent(..)
    , successNotice
    )
import Agent.CLI.Render
    ( RenderConfig(..)
    , clearThinking
    , formatElapsed
    , formatLoopErrorAt
    , formatLoopErrorColored
    , formatLoopErrorColoredAt
    , formatLoopErrorPersistedAt
    , formatTurnStatus
    , putTextLn
    , renderAssistantText
    , renderPrintedText
    , resetRenderPrintedText
    , stateLastTokensPerSecond
    , stateStartedAt
    )
import Agent.CLI.Session
    ( SessionHandle(..)
    , SessionMeta(..)
    , SessionTurn(..)
    , SessionTurnPage(..)
    , TranscriptEffect(..)
    , Persistence(..)
    , PersistenceState(..)
    , appendTurnWithMetaUpdateIndexedAndDeliver
    , ensureSession
    , loadRecentSessionTurns
    , sessionConversationText
    , sessionsRoot
    , sessionTitleFromPrompt
    , setGeneratedSessionTitle
    )
import Agent.CLI.SessionEnv (SessionEnv(..))
import Agent.CLI.Session.History
    ( currentLiveTranscriptGeneration
    , durableTranscriptCheckpoint
    , evictLiveTranscript
    , readLivePreviousResponseId
    , withLiveTranscript
    , writeLivePreviousResponseId
    , writeLiveTranscript
    )
import Agent.CLI.SessionTitle
    ( SessionTitleResult(..)
    , requestSessionTitle
    , shouldRequestSessionTitle
    , takeSessionTitleResults
    , titleRefreshIndex
    )
import Agent.CLI.Status (formatUsageWithRate)
import Agent.CLI.Style
    ( cliWindowTitle
    , glyphSession
    , glyphWarn
    , roleMuted
    , roleWarn
    )
import Agent.CLI.Terminal
    ( TerminalCapabilities(..)
    , emitTerminalSequence
    , notifyTerminal
    , osc133CommandFinished
    , osc133CommandStart
    , resolveColor
    )
import Agent.CLI.Timestamp (stampTurnInputs, stripBracketedTimestamps)
import Agent.CLI.TurnState
    ( ConversationOutcome(..)
    , ConversationPatch(..)
    , FieldUpdate(..)
    , PreparedTurn(..)
    , StartupUpdate(..)
    , TurnAbort(..)
    , finishConversation
    , interruptedTurnItems
    , rebasePreparedTurn
    , restoreStartupContext
    , turnInputsWithContext
    , turnNewItems
    , turnReplacesTranscript
    )
import Agent.Dialect (DialectId(..), dialectId)
import Agent.Loop
    ( LoopConfig(..)
    , LoopExecution(..)
    , LoopError(..)
    , LoopProgress(..)
    , LoopResult(..)
    , TurnCompletion(..)
    , TurnInput(..)
    , TurnOutput(..)
    , addTokenUsage
    , runLoopInputsDetailed
    )
import Agent.Provider (Provider(..))
import Agent.Responses.Types (ResponseItem)
import Agent.Store.Postgres.Interaction
    ( InteractionDeliveryIntent(..) )
import Agent.Tools.PlanMode
    ( PlanModeEnv(..)
    , PlanModeState(..)
    , PlanReminder(..)
    , PlanReminderToolNames(..)
    , PlanReviewOutcome(..)
    , activatePlanMode
    , ensurePlanMarkdown
    , isPlanModeActive
    , readPlanAgentActivationRevision
    , readPlanModeState
    , readPlanTracker
    , nextPlanModeReminder
    , submitPlanForReview
    , updatePlanTracker
    , writePlanMarkdown
    )
import Agent.Tools.PlanMode.File (renderPlanFileError)
import Agent.Tools.PlanMode.Tracker
    ( ApprovedPlanContinuation(..)
    , PlanTracker(..)
    , PlanTrackerPhase(..)
    , markPlanContinuationDelivered
    , restorePlanTrackerIfRevision
    )
import Agent.OsPath (toText, unsafeToFilePath)
import Control.Monad (forM_, unless, when)
import Control.Exception.Safe
    ( bracket_, finally, mask, onException, throwString, tryAny )
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , readIORef
    , writeIORef
    )
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getZonedTime, localDay, zonedTimeToLocalTime)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.IO (Handle)
import System.Info (os)
import qualified System.OsPath
import System.Process
    ( CreateProcess(..)
    , proc
    , readCreateProcessWithExitCode
    )
import System.Timeout (timeout)
import System.Mem (performMajorGC)

runOneTurn :: SessionEnv -> Text -> [TurnInput] -> IO TurnResult
runOneTurn = runOneTurnWithContext True Nothing

-- | Retry after a failed turn whose exact prepared inputs are already
-- checkpointed in the live transcript. Per-turn plan/startup context must not
-- be generated again. New durable-interaction responses may still be appended
-- to that checkpoint, and the pending turn carries the exact approved-plan
-- continuation identity already represented by it.
retryCheckpointedTurn :: SessionEnv -> PendingTurn -> IO TurnResult
retryCheckpointedTurn env pending =
    runOneTurnWithContext
        False
        pending.pendingPlanContinuation
        env
        ""
        pending.pendingInputs

runOneTurnWithContext
    :: Bool
    -> Maybe ApprovedPlanContinuation
    -> SessionEnv
    -> Text
    -> [TurnInput]
    -> IO TurnResult
runOneTurnWithContext
        includeTurnContext checkpointedContinuation env promptText inputs =
    readPlanModeState env.sessionPlanMode >>= \case
        -- A deferred correlated review is a hard turn barrier. Its immutable
        -- snapshot must be resolved (or revised) before any unrelated model
        -- work can run against the session.
        PlanExitPending -> do
            let message =
                    "plan review is awaiting a decision; use /plan to resume it before starting another turn"
            case env.sessionFullscreen of
                Just runtime ->
                    emitUiEvent runtime (UiSystemMessage message)
                Nothing -> do
                    color <- resolveColor env.sessionRender.renderStderr
                    putTextLn
                        env.sessionRender.renderStderr
                        (roleWarn color (glyphWarn <> message))
            pure TurnCancelled
        _ -> do
            recoveredInputs <- recoverTurnInteractionInputs env
            -- A newly submitted turn supersedes any older retry candidate. If
            -- this attempt fails, finishTurn installs its PendingTurn.
            writeIORef env.sessionLastFailedTurn Nothing
            -- Automatic compaction is scoped to one enclosing user turn. A
            -- committed boundary from an earlier attempt is already
            -- represented by the live and durable transcripts.
            writeIORef env.sessionAutomaticCompaction Nothing
            bracket_
                env.sessionBeginWindowTitleBusy
                env.sessionEndWindowTitleBusy
                (withLiveTranscript env.sessionConversation \beforeItems ->
                    runOneTurnBusy
                        includeTurnContext
                        checkpointedContinuation
                        env
                        beforeItems
                        promptText
                        (recoveredInputs <> inputs))
                `finally` writeIORef env.sessionAutomaticCompaction Nothing

recoverTurnInteractionInputs :: SessionEnv -> IO [TurnInput]
recoverTurnInteractionInputs env =
    case env.sessionPersist of
        PersistenceDisabled -> pure []
        PersistenceEnabled slotRef ->
            readIORef slotRef >>= \case
                PersistencePending{} -> pure []
                PersistenceActive handle ->
                    recoverUndeliveredInteractions
                        handle.sessionPool
                        handle.sessionMeta.metaId
                        env.sessionInteractionDeliveries >>= \case
                            Left err ->
                                throwString
                                    (Text.unpack
                                        (renderPendingInteractionError err))
                            Right [] -> pure []
                            Right interactions -> do
                                forM_ interactions \interaction ->
                                    case
                                        resolvedPlanEnterDecision interaction
                                    of
                                        Right (Just True) -> do
                                            ensurePlanMarkdown
                                                env.sessionPlanMode >>= \case
                                                    Left planError ->
                                                        throwString
                                                            (Text.unpack
                                                                (renderPlanFileError
                                                                    planError))
                                                    Right _ -> do
                                                        active <-
                                                            isPlanModeActive
                                                                env.sessionPlanMode
                                                        unless active
                                                            (activatePlanMode
                                                                env.sessionPlanMode)
                                        _ -> pure ()
                                pure
                                    [ UserMessage
                                        (renderRestoredInteractions
                                            interactions)
                                    ]

runOneTurnBusy
    :: Bool
    -> Maybe ApprovedPlanContinuation
    -> SessionEnv
    -> [ResponseItem]
    -> Text
    -> [TurnInput]
    -> IO TurnResult
runOneTurnBusy includeTurnContext checkpointedContinuation env@SessionEnv
    { sessionLoop = config
    , sessionRender = render
    , sessionConversation = conversationRef
    , sessionPersist = persist
    , sessionPlanMode = planMode
    , sessionStartupContext = startupContext
    , sessionBackground = background
    , sessionEscPaused = escPaused
    , sessionInterrupt = interrupt
    , sessionInteractionDeliveries = interactionDeliveriesRef
    , sessionUsage = usageRef
    , sessionLastAssistant = lastAssistantRef
    , sessionTerminal = terminal
    , sessionFullscreen = fullscreen
    , sessionSetWindowTitle = setWindowTitle
    , sessionBeginSubagentTurn = beginSubagentTurn
    , sessionFinishSubagentTurn = finishSubagentTurn
    , sessionAbortSubagentTurn = abortSubagentTurn
    , sessionOnPersisted = onPersisted
    } beforeItems promptText inputs = do
  let stdoutHandle = render.renderStdout
      stderrHandle = render.renderStderr
  -- Clear the prior turn before publishing this flag to Ctrl-C / Esc.
  -- Resetting inside runLoopInputs could erase the one-shot Esc signal.
  resetCancel config.loopCancel
  writeIORef env.sessionRestartEffort Nothing
  withTurnCancel interrupt config.loopCancel $
    (if isJust fullscreen || background
        then id
        else withEscCancel config.loopCancel escPaused) do
    applyPendingSessionTitles env
    -- Create the session directory before tools run so first-turn subagents
    -- and plan-mode state can persist under it as they change.
    case persist of
        PersistenceEnabled slotRef -> do
            created <- isPendingPersistence <$> readIORef slotRef
            handle <- ensureSession slotRef
            onPersisted handle
            when
                ( handle.sessionMeta.metaTitle == "untitled"
                    && not (Text.null (Text.strip promptText))
                )
                (setWindowTitle
                    (cliWindowTitle handle.sessionMeta.metaCwd
                        (Just (sessionTitleFromPrompt promptText))))
            when created do
                case fullscreen of
                    Just runtime ->
                        emitUiEvent runtime
                            (UiSystemMessage
                                ("session: " <> handle.sessionMeta.metaId))
                    Nothing -> do
                        color <- resolveColor stderrHandle
                        putTextLn stderrHandle
                            (roleMuted color
                                (glyphSession <> "session: "
                                    <> handle.sessionMeta.metaId))
        PersistenceDisabled -> pure ()
    initialPlanTracker <- readPlanTracker planMode
    initialPlanState <- readPlanModeState planMode
    when (initialPlanState == PlanPending) (activatePlanMode planMode)
    prev <- readLivePreviousResponseId conversationRef
    pendingStartup <-
        if includeTurnContext
            then atomicModifyIORef' startupContext \pendingCtx ->
                (Nothing, pendingCtx)
            else pure Nothing
    planReminder <-
        if includeTurnContext
            then
                nextPlanModeReminder
                    planMode
                    PlanReminderToolNames
                        { planReminderWriteToolName = "write_plan"
                        , planReminderQuestionToolName =
                            "ask_user_question"
                        , planReminderCompletionToolName =
                            case env.sessionProvider of
                                OpenAIProvider -> "<proposed_plan>"
                                _ -> "exit_plan_mode"
                        }
                    >>= either (throwString . Text.unpack)
                        (pure . fmap (.planReminderText))
            else pure Nothing
    trackerAtTurnStart <- readPlanTracker planMode
    let pendingContinuation
            | includeTurnContext =
                trackerAtTurnStart.trackerApprovedContinuation
            | otherwise = checkpointedContinuation
        continuationInputs =
            case pendingContinuation of
                Just continuation
                    | includeTurnContext
                    , not
                        (hasExactUserMessage
                            continuation.approvedPlanContinuation
                            inputs) ->
                                [ UserMessage
                                    continuation.approvedPlanContinuation
                                ]
                _ -> []
    let
        turnInputs0 =
            turnInputsWithContext
                planReminder
                pendingStartup
                (continuationInputs <> inputs)
        continuationInjected =
            includeTurnContext && not (null continuationInputs)
        continuationCarried =
            isJust checkpointedContinuation
                || continuationInjected
                || maybe
                    False
                    (\continuation ->
                        hasExactUserMessage
                            continuation.approvedPlanContinuation
                            inputs)
                    pendingContinuation
    stampedInputs <- stampTurnInputs turnInputs0
    turnInputs <-
        if dialectId env.sessionDialect == GrokBuildDialect
            then do
                let framed = grokFrameLastUserInput stampedInputs
                    firstTurn = null beforeItems && prev == Nothing
                if firstTurn
                    then do
                        prefix <- loadGrokFirstTurnPrefix env.sessionCwd
                        pure (UserMessage prefix : framed)
                    else pure framed
            else pure stampedInputs
    let prepared = PreparedTurn
            { preparedBeforeItems = beforeItems
            , preparedConsumedStartup = pendingStartup
            , preparedTurnInputs = turnInputs
            }
        commitConversationPatch patch = do
            case patch.patchPreviousResponseId of
                KeepField -> pure ()
                SetField value -> writeLivePreviousResponseId conversationRef value
            case patch.patchTranscript of
                KeepField -> pure ()
                SetField value -> writeLiveTranscript conversationRef value
            case patch.patchStartupContext of
                KeepStartup -> pure ()
                RestoreStartup consumed ->
                    atomicModifyIORef' startupContext \current ->
                        (restoreStartupContext consumed current, ())
            atomicModifyIORef' usageRef \current ->
                (addTokenUsage current patch.patchUsageDelta, ())
            case patch.patchLastAssistant of
                KeepField -> pure ()
                SetField value -> writeIORef lastAssistantRef value
    startedAt <- stateStartedAt <$> readIORef render.renderState
    wallStarted <- getCurrentTime
    when (isNothing fullscreen && terminal.terminalSemanticPrompts) $
        emitTerminalSequence terminal stdoutHandle osc133CommandStart
    rootTurnId <- beginSubagentTurn
    execution <- runLoopInputsDetailed config prev turnInputs
        `onException`
            ( readIORef env.sessionAutomaticCompaction >>= \boundary ->
              commitConversationPatch
                (finishConversation
                    (rebasePreparedTurn boundary prepared)
                    ConversationInterrupted)
                >> restorePlanStateAfterIncomplete
                    planMode
                    initialPlanTracker
                    trackerAtTurnStart.trackerRevision
                >> abortSubagentTurn rootTurnId
            )
    automaticCompaction <- readIORef env.sessionAutomaticCompaction
    let committedPrepared =
            rebasePreparedTurn automaticCompaction prepared
    when (isJust automaticCompaction && continuationCarried) $
        forM_ pendingContinuation
            (markPlanContinuationDeliveredIfCurrent planMode)
    let result = execution.executionResult
    clearThinking render
    finishedAt <- getCurrentTime
    restartEffort <-
        atomicModifyIORef' env.sessionRestartEffort \requested ->
            (Nothing, requested)
    let elapsedDetail extra = case startedAt of
            Nothing -> extra
            Just t0 -> extra <> " · " <> formatElapsed (realToFrac (diffUTCTime finishedAt t0))
        persistIncomplete
            :: [ResponseItem]
            -> Text
            -> Maybe TurnOutput
            -> Maybe Text
            -> IO ()
        persistIncomplete retainedItems errorText maybeTurn displayAssistant = case persist of
            PersistenceDisabled -> pure ()
            PersistenceEnabled slotRef -> do
                now <- getCurrentTime
                handle <- ensureSession slotRef
                let turn = SessionTurn
                        { turnAt = now
                        , turnUserText = promptText
                        , turnAssistantText =
                            case displayAssistant of
                                Just text -> Just text
                                Nothing -> maybeTurn >>= (.assistantText)
                        , turnError = Just errorText
                        , turnResponseId = (.responseId) <$> maybeTurn
                        , turnEffect = TranscriptAppend
                        , turnItems = retainedItems
                        , turnUsage = (.tokenUsage) <$> maybeTurn
                        }
                (handle', turnIndex) <-
                    consumeInteractionDeliveries
                        interactionDeliveriesRef
                        handle.sessionMeta.metaId
                        \intents ->
                            appendTurnWithMetaUpdateIndexedAndDeliver
                                handle
                                turn
                                (\meta ->
                                    meta { metaLastResponseId = Nothing })
                                intents
                writeIORef slotRef (PersistenceActive handle')
                forM_ fullscreen \runtime ->
                    commitFullscreenHistoryTurn
                        runtime
                        (sessionHistoryTurn turnIndex turn)
                        HistoryCommitAppend
                evictDurableConversation env handle'
                when continuationCarried $
                    forM_ pendingContinuation
                        (markPlanContinuationDeliveredIfCurrent
                            planMode)
    case (restartEffort, result) of
        (Just level, _) -> do
            abortSubagentTurn rootTurnId
            commitConversationPatch
                (finishConversation committedPrepared ConversationRestarted)
            planState <- readPlanModeState planMode
            case fullscreen of
                Just runtime ->
                    emitUiEvent runtime UiTurnRestarted
                Nothing -> pure ()
            pure $ TurnRestartRequested level PendingTurn
                { pendingPromptText = promptText
                , pendingInputs = maybe inputs (const []) automaticCompaction
                , pendingCheckpointed = isJust automaticCompaction
                , pendingPlanContinuation =
                    if continuationCarried
                        then pendingContinuation
                        else Nothing
                , pendingExitAfter = False
                , pendingPlanState = planState
                }
        (Nothing, Left cancelled@(LoopCancelled _)) -> do
            restorePlanStateAfterIncomplete
                planMode
                initialPlanTracker
                trackerAtTurnStart.trackerRevision
            finishTerminal (isNothing fullscreen)
                stdoutHandle terminal wallStarted finishedAt 130 Nothing
            abortSubagentTurn rootTurnId
            -- turnInputs already contains any startup context consumed above.
            -- Checkpoint it instead of restoring it separately, which would
            -- duplicate the instructions on the next full-history request.
            -- Completed model steps stay with it; only the sample that never
            -- committed is dropped.
            let retained =
                    interruptedTurnItems
                        committedPrepared execution TurnAbortedByUser
            commitConversationPatch
                (finishConversation committedPrepared
                    (ConversationCancelled retained))
            model <- readIORef render.renderModelRef
            case fullscreen of
                Just runtime -> do
                    emitUiEvent runtime
                        (UiTurnEnded BlockCancelled)
                    emitUiEvent runtime
                        (UiSystemMessage
                            ("cancelled · " <> elapsedDetail model))
                Nothing -> do
                    color <- resolveColor stderrHandle
                    putTextLn stderrHandle
                        (formatLoopErrorColored color cancelled)
                    putTextLn stderrHandle
                        (formatTurnStatus color "cancelled" (elapsedDetail model))
            -- Text of the sample that never committed is display-only: it
            -- reaches the durable turn but not the model transcript.
            persistIncomplete retained "cancelled" Nothing
                (uncommittedAssistantText execution)
            pure TurnCancelled
        (Nothing, Left err) -> do
            abortSubagentTurn rootTurnId
            case err of
                LoopTransport apiError
                    | execution.executionProgress == NoResponseCommitted
                    , isProviderUnavailable apiError -> do
                        commitConversationPatch
                            (finishConversation
                                committedPrepared
                                ConversationProviderUnavailable)
                        case fullscreen of
                            Nothing -> pure ()
                            Just runtime ->
                                emitUiEvent runtime
                                    (UiTurnEnded BlockFailed)
                        finishTerminal (isNothing fullscreen)
                            stdoutHandle terminal wallStarted finishedAt 1
                            (Just "Agent provider unavailable")
                        planState <- readPlanModeState planMode
                        pure $ TurnProviderUnavailable apiError PendingTurn
                            { pendingPromptText = promptText
                            , pendingInputs = maybe inputs (const []) automaticCompaction
                            , pendingCheckpointed = isJust automaticCompaction
                            , pendingPlanContinuation =
                                if continuationCarried
                                    then pendingContinuation
                                    else Nothing
                            , pendingExitAfter = False
                            , pendingPlanState = planState
                            }
                _ -> do
                    restorePlanStateAfterIncomplete
                        planMode
                        initialPlanTracker
                        trackerAtTurnStart.trackerRevision
                    finishTerminal (isNothing fullscreen)
                        stdoutHandle terminal wallStarted finishedAt 1
                        (Just "Agent turn failed")
                    let retained =
                            interruptedTurnItems
                                committedPrepared
                                execution
                                (TurnAbortedByFailure
                                    (loopErrorAbortReason err))
                    commitConversationPatch
                        (finishConversation committedPrepared
                            (ConversationFailed retained))
                    model <- readIORef render.renderModelRef
                    case fullscreen of
                        Just runtime ->
                            do
                                emitUiEvent runtime
                                    (UiTurnEnded BlockFailed)
                                emitUiEvent runtime
                                    (UiErrorMessage
                                        (formatLoopErrorAt finishedAt err
                                            <> "\n"
                                            <> elapsedDetail model))
                        Nothing -> do
                            color <- resolveColor stderrHandle
                            putTextLn stderrHandle
                                (formatLoopErrorColoredAt color finishedAt err)
                            putTextLn stderrHandle
                                (formatTurnStatus color "error" (elapsedDetail model))
                    let maybeIncompleteTurn = case err of
                            LoopIncomplete turn -> Just turn
                            _ -> Nothing
                    forM_ maybeIncompleteTurn \turn ->
                        atomicModifyIORef' usageRef \current ->
                            (addTokenUsage current turn.tokenUsage, ())
                    -- Keep the live and resumed model transcript aligned:
                    -- the same retained items become the durable turn, so
                    -- the fullscreen history shows what the live turn showed.
                    -- Response id, usage, and the incomplete reason remain
                    -- available in turn metadata.
                    persistIncomplete retained
                        (formatLoopErrorPersistedAt finishedAt err)
                        maybeIncompleteTurn
                        (uncommittedAssistantText execution)
                    planState <- readPlanModeState planMode
                    pure $ TurnFailed PendingTurn
                        { pendingPromptText = promptText
                        -- ConversationFailed checkpoints the exact stamped
                        -- inputs (including attachments) in the live
                        -- transcript. Do not retain a second potentially
                        -- large copy here.
                        , pendingInputs = []
                        , pendingCheckpointed = True
                        , pendingPlanContinuation =
                            if continuationCarried
                                then pendingContinuation
                                else Nothing
                        , pendingExitAfter = False
                        , pendingPlanState = planState
                        }
        (Nothing, Right loopResult) -> do
            finishTerminal (isNothing fullscreen)
                stdoutHandle terminal wallStarted finishedAt 0
                (Just "Agent finished")
            finishSubagentTurn rootTurnId
            let assistantText =
                    fmap stripBracketedTimestamps loopResult.finalText
            commitConversationPatch
                (finishConversation committedPrepared
                    (ConversationCompleted
                        loopResult.finalResponseId
                        loopResult.tokenUsage
                        assistantText))
            do
                model <- readIORef render.renderModelRef
                tokenRate <-
                    stateLastTokensPerSecond <$> readIORef render.renderState
                let turns = Text.pack (show loopResult.turnsUsed)
                    unit = if loopResult.turnsUsed == 1 then " turn" else " turns"
                    usageDetail =
                        formatUsageWithRate loopResult.tokenUsage tokenRate
                    extra =
                        if Text.null usageDetail
                            then model <> " · " <> turns <> unit
                            else model <> " · " <> turns <> unit <> " · " <> usageDetail
                    detail = elapsedDetail extra
                case fullscreen of
                    Just runtime ->
                        emitUiEvent runtime
                            (UiSetNotice
                                (Just
                                    (successNotice
                                        ("Finished · " <> detail))))
                    Nothing -> do
                        color <- resolveColor stderrHandle
                        putTextLn stderrHandle
                            (formatTurnStatus color "ok" detail)
            printedText <- renderPrintedText render
            case (fullscreen, printedText, assistantText) of
                (Just _, _, _) -> pure ()
                (Nothing, False, Just text) | not (Text.null (Text.strip text)) -> do
                    useColor <- resolveColor stdoutHandle
                    putTextLn stdoutHandle (renderAssistantText useColor text)
                _ -> pure ()
            let newItems =
                    turnNewItems
                        committedPrepared.preparedBeforeItems
                        execution.executionState
                effect =
                    if turnReplacesTranscript
                        committedPrepared.preparedBeforeItems
                        execution.executionState
                        then TranscriptReplace
                        else TranscriptAppend
            pendingDeliveries <- readIORef interactionDeliveriesRef
            let deliveredReview =
                    any
                        (\delivery ->
                            delivery.durableDeliveryInteractionKind
                                == "plan_mode.review")
                        pendingDeliveries
            continuationDeliveredByTurn <-
                if continuationCarried
                    then pure pendingContinuation
                    else
                        if deliveredReview
                            then
                                (.trackerApprovedContinuation)
                                    <$> readPlanTracker planMode
                            else pure Nothing
            case persist of
                PersistenceDisabled ->
                    forM_ continuationDeliveredByTurn
                        (markPlanContinuationDeliveredIfCurrent
                            planMode)
                PersistenceEnabled{} -> pure ()
            followUp <-
                reviewAfterDurableAppend
                    (case persist of
                        PersistenceDisabled -> pure False
                        PersistenceEnabled slotRef -> do
                            now <- getCurrentTime
                            handle <- ensureSession slotRef
                            let turn = SessionTurn
                                    { turnAt = now
                                    , turnUserText = promptText
                                    , turnAssistantText = assistantText
                                    , turnError = Nothing
                                    , turnResponseId =
                                        Just loopResult.finalResponseId
                                    , turnEffect = effect
                                    , turnItems = newItems
                                    , turnUsage = Just loopResult.tokenUsage
                                    }
                            titleTurns <-
                                (+ 1) <$> readIORef env.sessionTitleTurnCount
                            (countedHandle, turnIndex) <-
                                consumeInteractionDeliveries
                                    interactionDeliveriesRef
                                    handle.sessionMeta.metaId
                                    \intents ->
                                        appendTurnWithMetaUpdateIndexedAndDeliver
                                            handle
                                            turn
                                            (\meta ->
                                                meta
                                                    { metaTitleUserTurns =
                                                        titleTurns
                                                    })
                                            intents
                            writeIORef env.sessionTitleTurnCount titleTurns
                            let countedMeta = countedHandle.sessionMeta
                            writeIORef slotRef
                                (PersistenceActive countedHandle)
                            forM_ fullscreen \runtime ->
                                commitFullscreenHistoryTurn
                                    runtime
                                    (sessionHistoryTurn turnIndex turn)
                                    (case effect of
                                        TranscriptAppend ->
                                            HistoryCommitAppend
                                        -- Compaction replaces model context,
                                        -- not the on-screen transcript.
                                        -- Keep earlier turns scrollable and
                                        -- archive the compact summary.
                                        TranscriptReplace ->
                                            HistoryCommitAppend
                                        TranscriptReset ->
                                            HistoryCommitReset)
                            evictDurableConversation env countedHandle
                            when
                                ( not countedMeta.metaTitleIsManual
                                    && shouldRequestSessionTitle
                                        titleTurns
                                        countedMeta.metaTitleRefreshIndex
                                )
                                (requestConversationTitle
                                    env
                                    countedHandle
                                    titleTurns)
                            when
                                ( countedMeta.metaTitle
                                    /= handle.sessionMeta.metaTitle
                                ) do
                                    setWindowTitle
                                        (cliWindowTitle
                                            countedMeta.metaCwd
                                            (Just countedMeta.metaTitle))
                            applyPendingSessionTitles env
                            forM_ continuationDeliveredByTurn
                                (markPlanContinuationDeliveredIfCurrent
                                    planMode)
                            pure True)
                    (handleProposedPlan planMode loopResult.finalText)
            case followUp of
                Nothing -> pure TurnSucceeded
                Just notes -> do
                    resetRenderPrintedText render
                    current <- readPlanTracker planMode
                    let followInputs =
                            case current.trackerApprovedContinuation of
                                Just continuation
                                    | continuation.approvedPlanContinuation
                                        == notes ->
                                            []
                                _ -> [UserMessage notes]
                    runOneTurn env notes followInputs

hasExactUserMessage :: Text -> [TurnInput] -> Bool
hasExactUserMessage expected =
    any \case
        UserMessage value -> value == expected
        UserMultimodal{userText} -> userText == expected
        UserMultimodalFiles{userText} -> userText == expected
        _ -> False

markPlanContinuationDeliveredIfCurrent
    :: PlanModeEnv
    -> ApprovedPlanContinuation
    -> IO ()
markPlanContinuationDeliveredIfCurrent planMode delivered =
    updatePlanTracker planMode transition >>= \case
        Left err -> throwString (Text.unpack err)
        Right _ -> pure ()
  where
    transition tracker
        | tracker.trackerApprovedContinuation == Just delivered =
            Right (markPlanContinuationDelivered tracker)
        | otherwise = Right tracker

consumeInteractionDeliveries
    :: IORef [DurableInteractionDelivery]
    -> Text
    -> ([InteractionDeliveryIntent] -> IO value)
    -> IO value
consumeInteractionDeliveries ref sessionKey action =
    mask \restore -> do
        pending <- readIORef ref
        let selected =
                [ delivery.durableDeliveryIntent
                | delivery <- pending
                , delivery.durableDeliverySessionKey == sessionKey
                ]
        result <- restore (action selected)
        let consumedIds =
                map (.interactionDeliveryIntentInteractionId) selected
        atomicModifyIORef' ref \current ->
            ( filter
                (\delivery ->
                    delivery.durableDeliverySessionKey /= sessionKey
                        || delivery.durableDeliveryIntent.interactionDeliveryIntentInteractionId
                            `notElem` consumedIds)
                current
            , ()
            )
        pure result

-- | Do not open a review surface or start its continuation until the
-- assistant turn that proposed the plan is durable. A disabled or failed
-- append therefore leaves Plan Mode active for a later recovery path.
reviewAfterDurableAppend
    :: IO Bool
    -> IO (Maybe value)
    -> IO (Maybe value)
reviewAfterDurableAppend appendAssistantTurn review = do
    durable <- appendAssistantTurn
    if durable
        then review
        else pure Nothing

-- | Release the parsed root transcript after the exact turn is durable.
--
-- The checkpoint deliberately closes over only durable session identity and
-- the store pool. A stale callback cannot evict a newer in-memory generation.
evictDurableConversation :: SessionEnv -> SessionHandle -> IO ()
evictDurableConversation env handle = do
    generation <-
        currentLiveTranscriptGeneration env.sessionConversation
    let sessionId = handle.sessionMeta.metaId
        checkpoint =
            durableTranscriptCheckpoint
                env.sessionDatabasePool
                (sessionsRoot env.sessionHome)
                sessionId
    evicted <-
        evictLiveTranscript
            env.sessionConversation generation checkpoint
    when evicted performMajorGC

-- | Wrap the last actual user payload in the Grok Build request envelope.
-- Synthetic startup, skill, plan, and reminder messages precede it and remain
-- separately framed.
grokFrameLastUserInput :: [TurnInput] -> [TurnInput]
grokFrameLastUserInput = reverse . go . reverse
  where
    go [] = []
    go (UserMessage text : rest) =
        UserMessage (grokUserQuery text) : rest
    go (input@UserMultimodal{userText} : rest) =
        input { userText = grokUserQuery userText } : rest
    go (input@UserMultimodalFiles{userText} : rest) =
        input { userText = grokUserQuery userText } : rest
    go (input : rest) = input : go rest

grokUserQuery :: Text -> Text
grokUserQuery text =
    "<user_query>\n" <> text <> "\n</user_query>"

grokFirstTurnPrefix
    :: Text
    -> Text
    -> System.OsPath.OsPath
    -> Day
    -> Maybe Text
    -> Text
grokFirstTurnPrefix osName shell cwd today gitStatus =
    Text.intercalate "\n"
        [ "<user_info>"
        , "OS Version: " <> osName
        , "Shell: " <> shellBaseName shell
        , "Workspace Path: " <> toText cwd
        , "Today's date: "
            <> Text.pack
                (formatTime defaultTimeLocale "%A %b %-d, %Y" today)
        , "Note: Prefer using relative paths over absolute paths as tool call args when possible."
        , "</user_info>"
        ]
        <> maybe "" renderGitStatus gitStatus
  where
    shellBaseName path =
        case filter (not . Text.null)
                (Text.split (\char -> char == '/' || char == '\\') path) of
            [] -> path
            parts -> last parts
    renderGitStatus status =
        "\n\n<git_status>\n\
        \This is the git status at the start of the conversation. Note that this status is a snapshot in time, and will not update during the conversation.\n"
            <> status
            <> "\n</git_status>"

loadGrokFirstTurnPrefix :: System.OsPath.OsPath -> IO Text
loadGrokFirstTurnPrefix cwd = do
    shell <- maybe "/bin/sh" Text.pack <$> lookupEnv "SHELL"
    osVersion <- loadOperatingSystem
    today <- localDay . zonedTimeToLocalTime <$> getZonedTime
    status <- loadGitStatus cwd
    pure (grokFirstTurnPrefix osVersion shell cwd today status)

loadOperatingSystem :: IO Text
loadOperatingSystem = do
    result <- tryAny $
        timeout 1000000 $
            readCreateProcessWithExitCode (proc "uname" ["-sr"]) ""
    pure $ case result of
        Right (Just (ExitSuccess, output, _))
            | kernel : release <- Text.words (Text.strip (Text.pack output))
            , not (null release) ->
                Text.toLower kernel <> " " <> Text.unwords release
        _ -> Text.pack os

loadGitStatus :: System.OsPath.OsPath -> IO (Maybe Text)
loadGitStatus cwd = do
    result <- tryAny $
        timeout 5000000 $
            readCreateProcessWithExitCode
                (proc "git"
                    [ "status"
                    , "--short"
                    , "--branch"
                    , "--untracked-files=normal"
                    ])
                    { cwd = Just (unsafeToFilePath cwd) }
                ""
    pure $ case result of
        Right (Just (ExitSuccess, output, _)) ->
            normalizeGitStatus
                (collapseStatusSpaces (Text.pack output))
        _ -> Nothing

collapseStatusSpaces :: Text -> Text
collapseStatusSpaces = Text.pack . go False . Text.unpack
  where
    go _ [] = []
    go previousSpace (char : rest)
        | char == ' ' && previousSpace =
            go True rest
        | otherwise =
            char : go (char == ' ') rest

normalizeGitStatus :: Text -> Maybe Text
normalizeGitStatus raw
    | Text.null status = Nothing
    | Text.length status <= maxCharacters = Just status
    | otherwise =
        Just
            (snapToLastNewline (Text.take maxCharacters status)
                <> "\n\n... (git status truncated)")
  where
    status = Text.strip raw
    maxCharacters = 10000
    snapToLastNewline prefix =
        case Text.breakOnEnd "\n" prefix of
            ("", _) -> prefix
            (throughNewline, _) -> Text.dropEnd 1 throughNewline

isPendingPersistence :: PersistenceState -> Bool
isPendingPersistence = \case
    PersistencePending _ _ _ -> True
    PersistenceActive _ -> False

uncommittedAssistantText :: LoopExecution -> Maybe Text
uncommittedAssistantText execution =
    fmap stripBracketedTimestamps execution.executionUncommittedAssistantText

-- | Short reason recorded on the synthetic outputs of tool calls a failed
-- turn never executed.
loopErrorAbortReason :: LoopError -> Text
loopErrorAbortReason = \case
    LoopIncomplete turn -> case turn.completion of
        TurnIncomplete reason _ ->
            "the response was cut off (" <> reason <> ")"
        TurnCompleted -> "the response was cut off"
    LoopMaxTurns _ -> "the turn reached its maximum number of model steps"
    LoopTransport _ -> "the provider request failed"
    LoopTransportAfterOutput _ -> "the provider connection was interrupted"
    LoopNoResponseId -> "the provider returned no response id"
    LoopUnexpected _ -> "the agent hit an unexpected error"
    LoopCancelled _ -> "the turn was cancelled"

requestConversationTitle :: SessionEnv -> SessionHandle -> Int -> IO ()
requestConversationTitle env handle milestone =
    loadRecentSessionTurns
        env.sessionDatabasePool
        (System.OsPath.takeDirectory handle.sessionDir)
        handle.sessionMeta.metaId
        24
        >>= \case
            Left _ -> pure ()
            Right page ->
                requestSessionTitle env.sessionTitleManager
                    handle.sessionMeta.metaId
                    milestone
                    (sessionConversationText (map snd page.pageTurns))

applyPendingSessionTitles :: SessionEnv -> IO ()
applyPendingSessionTitles env =
    takeSessionTitleResults env.sessionTitleManager >>= mapM_ applyOne
  where
    applyOne SessionTitleResult{..} =
        case env.sessionPersist of
            PersistenceDisabled -> pure ()
            PersistenceEnabled slotRef ->
                readIORef slotRef >>= \case
                    PersistencePending _ _ _ -> pure ()
                    PersistenceActive handle
                        | handle.sessionMeta.metaId /= resultSessionId -> pure ()
                        | handle.sessionMeta.metaTitleIsManual -> pure ()
                        | otherwise -> do
                            updated <- setGeneratedSessionTitle
                                (titleRefreshIndex resultMilestone)
                                resultTitle
                                handle
                            writeIORef slotRef (PersistenceActive updated)
                            env.sessionSetWindowTitle
                                (cliWindowTitle updated.sessionMeta.metaCwd
                                    (Just updated.sessionMeta.metaTitle))

-- | Roll a cancelled or failed turn back to the interaction mode it started
-- in. In particular, an agent-initiated enter_plan_mode must not trap the next
-- user prompt in Plan Mode after the turn is interrupted.
restorePlanStateAfterIncomplete
    :: PlanModeEnv
    -> PlanTracker
    -> Word64
    -> IO ()
restorePlanStateAfterIncomplete planMode snapshot preparedRevision = do
    current <- readPlanTracker planMode
    agentActivationRevision <- readPlanAgentActivationRevision planMode
    let expectedRevision
            | snapshot.trackerPhase == TrackerPending =
                Just preparedRevision
            | snapshot.trackerPhase == TrackerInactive =
                agentActivationRevision
            | otherwise = Nothing
    when
        ( turnOwnedActivation snapshot current
            && expectedRevision == Just current.trackerRevision
        ) do
        _ <-
            updatePlanTracker planMode $
                either
                    (Left . Text.pack . show)
                    Right
                    . restorePlanTrackerIfRevision
                        current.trackerRevision
                        snapshot
        pure ()
  where
    -- Only undo the activation this turn could have introduced. Never roll
    -- back an exit-pending request or a user resolution/continuation.
    turnOwnedActivation before after =
        before.trackerPhase `elem` [TrackerInactive, TrackerPending]
            && after.trackerPhase == TrackerActive
            && after.trackerPendingApproval == Nothing
            && after.trackerApprovedContinuation == Nothing

finishTerminal
    :: Bool
    -> Handle
    -> TerminalCapabilities
    -> UTCTime
    -> UTCTime
    -> Int
    -> Maybe Text
    -> IO ()
finishTerminal
        semanticPrompts output terminal started finished exitCode notification = do
    when (semanticPrompts && terminal.terminalSemanticPrompts) $
        emitTerminalSequence terminal output
            (osc133CommandFinished (Just exitCode))
    let seconds = realToFrac (diffUTCTime finished started) :: Double
    case notification of
        Just message
            | exitCode /= 0 || seconds >= 10 ->
                notifyTerminal terminal output message
        _ -> pure ()

handleProposedPlan
    :: PlanModeEnv
    -> Maybe Text
    -> IO (Maybe Text)
handleProposedPlan planMode = \case
    Nothing -> pure Nothing
    Just text -> do
        state <- readPlanModeState planMode
        case (state, parseProposedPlan text) of
            (PlanActive, Right planBody) -> do
                writePlanMarkdown planMode planBody >>= \case
                    Left writeError ->
                        pure $ Just $
                            "The proposed plan could not be reviewed because "
                                <> writeError
                                <> ". Stay in plan mode and present a fresh, readable plan."
                    Right () ->
                        submitPlanForReview planMode Nothing >>= \case
                            Left reviewError ->
                                pure $ Just $
                                    "The proposed plan could not be reviewed because "
                                        <> reviewError
                                        <> ". Stay in plan mode and present a fresh, readable plan."
                            Right (PlanReviewAccepted continuation) ->
                                pure
                                    (Just
                                        continuation.approvedPlanContinuation)
                            Right (PlanReviewRevisionRequired notes) ->
                                pure $ Just $
                                    "The user requested changes to the plan. Stay in plan mode and revise plan.md.\nFeedback:\n"
                                        <> notes
                            Right PlanReviewApprovalOverrideRequired{} ->
                                pure $ Just
                                    "The plan has advisory warnings. It remains pending until the user explicitly chooses Approve anyway."
                            Right PlanReviewAbandoned ->
                                pure Nothing
                            Right PlanReviewDeferred{} ->
                                pure Nothing
            (PlanActive, Left ProposedPlanNotFound) -> pure Nothing
            (PlanActive, Left parseError) ->
                pure $ Just $
                    "The proposed plan could not be reviewed because "
                        <> renderProposedPlanParseError parseError
                        <> ". Stay in plan mode and end with exactly one complete, non-nested <proposed_plan>…</proposed_plan> block outside fenced code."
            _ -> pure Nothing
