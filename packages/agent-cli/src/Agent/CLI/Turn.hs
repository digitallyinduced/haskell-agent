-- | Execute one model turn and commit its observable session state.
module Agent.CLI.Turn
    ( applyPendingSessionTitles
    , runOneTurn
    ) where

import Agent.Cancel (resetCancel)
import Agent.CLI.CancelWatch (withEscCancel)
import Agent.CLI.Interrupt (withTurnCancel)
import Agent.CLI.Plan (extractProposedPlan, planDecisionFollowUp)
import Agent.CLI.ProviderFallback (isProviderUnavailable)
import Agent.CLI.ProviderTransition
    ( PendingTurn(..)
    , TurnResult(..)
    )
import Agent.CLI.TUI.App
    ( emitUiEvent
    )
import Agent.TUI.Model (BlockState(..), UiEvent(..))
import Agent.CLI.Render
    ( RenderConfig(..)
    , clearThinking
    , formatElapsed
    , formatLoopErrorColored
    , formatTurnStatus
    , putTextLn
    , renderAssistantText
    )
import Agent.CLI.Session
    ( SessionHandle(..)
    , SessionMeta(..)
    , SessionTurn(..)
    , Persistence(..)
    , PersistenceState(..)
    , appendTurn
    , ensureSession
    , loadSession
    , sessionConversationText
    , setGeneratedSessionTitle
    , writeSessionMeta
    )
import Agent.CLI.SessionEnv (SessionEnv(..))
import Agent.CLI.SessionTitle
    ( SessionTitleResult(..)
    , requestSessionTitle
    , takeSessionTitleResults
    , titleRefreshIndex
    )
import Agent.CLI.Status (formatTokenUsage)
import Agent.CLI.Style
    ( cliWindowTitle
    , glyphSession
    , roleMuted
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
import Agent.Loop
    ( LoopConfig(..)
    , LoopError(..)
    , LoopResult(..)
    , TurnInput(..)
    , addTokenUsage
    , runLoopInputs
    )
import Agent.Tools.PlanMode
    ( PlanDecision(..)
    , PlanModeEnv(..)
    , PlanModeHooks(..)
    , PlanModeState(..)
    , activatePlanMode
    , deactivatePlanMode
    , isPlanModeActive
    , planFilePath
    , planModeReminder
    , writePlanMarkdown
    )
import Control.Monad (when)
import Control.Exception.Safe (onException)
import Data.IORef
    ( atomicModifyIORef'
    , modifyIORef'
    , readIORef
    , writeIORef
    )
import Data.List (isPrefixOf)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import System.IO (stderr, stdout)
import qualified System.OsPath

runOneTurn :: SessionEnv -> Text -> [TurnInput] -> IO TurnResult
runOneTurn env@SessionEnv
    { sessionLoop = config
    , sessionRender = render
    , sessionPrevious = previous
    , sessionPrinted = printed
    , sessionTranscript = transcriptRef
    , sessionPersist = persist
    , sessionPlanMode = planMode
    , sessionStartupContext = startupContext
    , sessionEscPaused = escPaused
    , sessionInterrupt = interrupt
    , sessionStoreRoot = storeRoot
    , sessionUsage = usageRef
    , sessionLastAssistant = lastAssistantRef
    , sessionTerminal = terminal
    , sessionFullscreen = fullscreen
    , sessionSetWindowTitle = setWindowTitle
    , sessionBeginSubagentTurn = beginSubagentTurn
    , sessionFinishSubagentTurn = finishSubagentTurn
    , sessionAbortSubagentTurn = abortSubagentTurn
    , sessionOnPersisted = onPersisted
    } promptText inputs = do
  -- Clear the prior turn before publishing this flag to Ctrl-C / Esc.
  -- Resetting inside runLoopInputs could erase the one-shot Esc signal.
  resetCancel config.loopCancel
  withTurnCancel interrupt config.loopCancel $
    (if isJust fullscreen
        then id
        else withEscCancel config.loopCancel escPaused) do
    applyPendingSessionTitles env
    pending <- readIORef planMode.planStateRef
    when (pending == PlanPending) (activatePlanMode planMode)
    -- Create the session directory before tools run so first-turn subagents
    -- can persist under agents/<id>/ as they complete.
    case persist of
        PersistenceEnabled slotRef -> do
            created <- isPendingPersistence <$> readIORef slotRef
            handle <- ensureSession slotRef
            onPersisted handle
            writeIORef planMode.planSessionDir (Just handle.sessionDir)
            writeIORef storeRoot (Just handle.sessionDir)
            when created do
                case fullscreen of
                    Just runtime ->
                        emitUiEvent runtime
                            (UiSystemMessage
                                ("session: " <> handle.sessionMeta.metaId))
                    Nothing -> do
                        color <- resolveColor stderr
                        putTextLn stderr
                            (roleMuted color
                                (glyphSession <> "session: "
                                    <> handle.sessionMeta.metaId))
            titleTurns <- readIORef env.sessionTitleTurnCount
            when
                ( titleTurns == 0
                    && not handle.sessionMeta.metaTitleIsManual
                    && handle.sessionMeta.metaTitleRefreshIndex == 0
                )
                (requestSessionTitle env.sessionTitleManager
                    handle.sessionMeta.metaId 1 promptText)
        PersistenceDisabled -> pure ()
    prev <- readIORef previous
    beforeItems <- readIORef transcriptRef
    pendingStartup <- atomicModifyIORef' startupContext \pendingCtx -> (Nothing, pendingCtx)
    planActive <- isPlanModeActive planMode
    planPath <- planFilePath planMode
    let planReminder =
            if planActive
                then Just (planModeReminder planPath)
                else Nothing
        baseInputs = case pendingStartup of
            Just context -> UserMessage context : inputs
            Nothing -> inputs
        restoreStartupContext = case pendingStartup of
            Nothing -> pure ()
            Just context ->
                modifyIORef' startupContext \current ->
                    Just $ case current of
                        Nothing -> context
                        Just newer -> context <> "\n\n" <> newer
        turnInputs0 = case planReminder of
            Just reminder -> UserMessage reminder : baseInputs
            Nothing -> baseInputs
    turnInputs <- stampTurnInputs turnInputs0
    startedAt <- readIORef render.renderStartedAt
    wallStarted <- getCurrentTime
    when (isNothing fullscreen && terminal.terminalSemanticPrompts) $
        emitTerminalSequence terminal stdout osc133CommandStart
    rootTurnId <- beginSubagentTurn
    result <- runLoopInputs config prev turnInputs
        `onException`
            (restoreStartupContext >> abortSubagentTurn rootTurnId)
    clearThinking render
    finishedAt <- getCurrentTime
    let elapsedDetail extra = case startedAt of
            Nothing -> extra
            Just t0 -> extra <> " · " <> formatElapsed (realToFrac (diffUTCTime finishedAt t0))
        persistIncomplete errorText = case persist of
            PersistenceDisabled -> pure ()
            PersistenceEnabled slotRef -> do
                now <- getCurrentTime
                handle <- ensureSession slotRef
                writeIORef planMode.planSessionDir (Just handle.sessionDir)
                writeIORef storeRoot (Just handle.sessionDir)
                let turn = SessionTurn
                        { turnAt = now
                        , turnUserText = promptText
                        , turnAssistantText = Nothing
                        , turnError = Just errorText
                        , turnResponseId = Nothing
                        , turnItems = []
                        , turnUsage = Nothing
                        }
                handle' <- appendTurn handle turn
                writeIORef slotRef (PersistenceActive handle')
    case result of
        Left cancelled@(LoopCancelled _) -> do
            restoreStartupContext
            finishTerminal (isNothing fullscreen)
                terminal wallStarted finishedAt 130 "Agent cancelled"
            abortSubagentTurn rootTurnId
            writeIORef transcriptRef beforeItems
            model <- readIORef render.renderModelRef
            case fullscreen of
                Just runtime -> do
                    emitUiEvent runtime
                        (UiTurnEnded BlockCancelled)
                    emitUiEvent runtime
                        (UiSystemMessage
                            ("cancelled · " <> elapsedDetail model))
                Nothing -> do
                    color <- resolveColor stderr
                    putTextLn stderr (formatLoopErrorColored color cancelled)
                    putTextLn stderr
                        (formatTurnStatus color "cancelled" (elapsedDetail model))
            persistIncomplete "cancelled"
            pure TurnSucceeded
        Left err -> do
            restoreStartupContext
            abortSubagentTurn rootTurnId
            afterItems <- readIORef transcriptRef
            case err of
                LoopTransport apiError
                    | length afterItems == length beforeItems
                    , isProviderUnavailable apiError -> do
                        case fullscreen of
                            Nothing -> pure ()
                            Just runtime ->
                                emitUiEvent runtime
                                    (UiTurnEnded BlockFailed)
                        finishTerminal (isNothing fullscreen)
                            terminal wallStarted finishedAt 1
                            "Agent provider unavailable"
                        planState <- readIORef planMode.planStateRef
                        pure $ TurnProviderUnavailable apiError PendingTurn
                            { pendingPromptText = promptText
                            , pendingInputs = inputs
                            , pendingExitAfter = False
                            , pendingPlanState = planState
                            }
                _ -> do
                    finishTerminal (isNothing fullscreen)
                        terminal wallStarted finishedAt 1 "Agent turn failed"
                    writeIORef transcriptRef beforeItems
                    model <- readIORef render.renderModelRef
                    case fullscreen of
                        Just runtime ->
                            do
                                emitUiEvent runtime
                                    (UiTurnEnded BlockFailed)
                                emitUiEvent runtime
                                    (UiErrorMessage
                                        (Text.pack (show err)
                                            <> "\n"
                                            <> elapsedDetail model))
                        Nothing -> do
                            color <- resolveColor stderr
                            putTextLn stderr (formatLoopErrorColored color err)
                            putTextLn stderr
                                (formatTurnStatus color "error" (elapsedDetail model))
                    persistIncomplete (Text.pack (show err))
                    pure TurnFailed
        Right loopResult -> do
            finishTerminal (isNothing fullscreen)
                terminal wallStarted finishedAt 0 "Agent finished"
            finishSubagentTurn rootTurnId
            writeIORef previous (Just loopResult.finalResponseId)
            modifyIORef' usageRef (`addTokenUsage` loopResult.tokenUsage)
            do
                model <- readIORef render.renderModelRef
                let turns = Text.pack (show loopResult.turnsUsed)
                    unit = if loopResult.turnsUsed == 1 then " turn" else " turns"
                    usageDetail = formatTokenUsage loopResult.tokenUsage
                    extra =
                        if Text.null usageDetail
                            then model <> " · " <> turns <> unit
                            else model <> " · " <> turns <> unit <> " · " <> usageDetail
                    detail = elapsedDetail extra
                case fullscreen of
                    Just runtime ->
                        emitUiEvent runtime (UiSetNotice (Just detail))
                    Nothing -> do
                        color <- resolveColor stderr
                        putTextLn stderr
                            (formatTurnStatus color "ok" detail)
            followUp <- handleProposedPlan planMode loopResult.finalText
            printedText <- readIORef printed
            let assistantText =
                    fmap stripBracketedTimestamps loopResult.finalText
            writeIORef lastAssistantRef assistantText
            case (fullscreen, printedText, assistantText) of
                (Just _, _, _) -> pure ()
                (Nothing, False, Just text) | not (Text.null (Text.strip text)) -> do
                    useColor <- resolveColor stdout
                    putTextLn stdout (renderAssistantText useColor text)
                _ -> pure ()
            afterItems <- readIORef transcriptRef
            let newItems
                    | beforeItems `isPrefixOf` afterItems =
                        drop (length beforeItems) afterItems
                    | otherwise = afterItems
            case persist of
                PersistenceDisabled -> pure ()
                PersistenceEnabled slotRef -> do
                    now <- getCurrentTime
                    handle <- ensureSession slotRef
                    writeIORef planMode.planSessionDir (Just handle.sessionDir)
                    writeIORef storeRoot (Just handle.sessionDir)
                    let turn = SessionTurn
                            { turnAt = now
                            , turnUserText = promptText
                            , turnAssistantText = assistantText
                            , turnError = Nothing
                            , turnResponseId = Just loopResult.finalResponseId
                            , turnItems = newItems
                            , turnUsage = Just loopResult.tokenUsage
                            }
                    handle' <- appendTurn handle turn
                    titleTurns <- atomicModifyIORef' env.sessionTitleTurnCount \n ->
                        let next = n + 1 in (next, next)
                    let countedMeta = handle'.sessionMeta
                            { metaTitleUserTurns = titleTurns }
                        countedHandle = handle' { sessionMeta = countedMeta }
                    writeSessionMeta countedHandle.sessionMetaPath countedMeta
                    writeIORef slotRef (PersistenceActive countedHandle)
                    when
                        ( titleTurns `elem` [3, 6]
                            && not countedMeta.metaTitleIsManual
                            && countedMeta.metaTitleRefreshIndex
                                < titleRefreshIndex titleTurns
                        )
                        (requestConversationTitle env countedHandle titleTurns)
                    when (countedMeta.metaTitle /= handle.sessionMeta.metaTitle) do
                        setWindowTitle
                            (cliWindowTitle countedMeta.metaCwd
                                (Just countedMeta.metaTitle))
                    applyPendingSessionTitles env
            case followUp of
                Nothing -> pure TurnSucceeded
                Just notes -> do
                    writeIORef printed False
                    runOneTurn env notes [UserMessage notes]

isPendingPersistence :: PersistenceState -> Bool
isPendingPersistence = \case
    PersistencePending _ -> True
    PersistenceActive _ -> False

requestConversationTitle :: SessionEnv -> SessionHandle -> Int -> IO ()
requestConversationTitle env handle milestone =
    loadSession
        (System.OsPath.takeDirectory handle.sessionDir)
        handle.sessionMeta.metaId
        >>= \case
            Left _ -> pure ()
            Right (_, turns) ->
                requestSessionTitle env.sessionTitleManager
                    handle.sessionMeta.metaId
                    milestone
                    (sessionConversationText turns)

applyPendingSessionTitles :: SessionEnv -> IO ()
applyPendingSessionTitles env =
    takeSessionTitleResults env.sessionTitleManager >>= mapM_ applyOne
  where
    applyOne SessionTitleResult{..} =
        case env.sessionPersist of
            PersistenceDisabled -> pure ()
            PersistenceEnabled slotRef ->
                readIORef slotRef >>= \case
                    PersistencePending _ -> pure ()
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

finishTerminal
    :: Bool
    -> TerminalCapabilities
    -> UTCTime
    -> UTCTime
    -> Int
    -> Text
    -> IO ()
finishTerminal semanticPrompts terminal started finished exitCode message = do
    when (semanticPrompts && terminal.terminalSemanticPrompts) $
        emitTerminalSequence terminal stdout
            (osc133CommandFinished (Just exitCode))
    let seconds = realToFrac (diffUTCTime finished started) :: Double
    when (exitCode /= 0 || seconds >= 10) $
        notifyTerminal terminal stdout message

handleProposedPlan
    :: PlanModeEnv
    -> Maybe Text
    -> IO (Maybe Text)
handleProposedPlan planMode = \case
    Nothing -> pure Nothing
    Just text -> do
        active <- isPlanModeActive planMode
        case (active, extractProposedPlan text) of
            (True, Just planBody) -> do
                _ <- writePlanMarkdown planMode planBody
                let PlanModeHooks{ planDecideExit = decideExit } = planMode.planHooks
                decision <- decideExit planBody
                case decision of
                    PlanApprove -> do
                        deactivatePlanMode planMode
                        pure (planDecisionFollowUp decision)
                    PlanCancel -> do
                        deactivatePlanMode planMode
                        pure Nothing
                    PlanRequestChanges _ ->
                        pure (planDecisionFollowUp decision)
            _ -> pure Nothing
