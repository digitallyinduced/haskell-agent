-- | Execute one model turn and commit its observable session state.
module Agent.CLI.Turn
    ( runOneTurn
    ) where

import Agent.CLI.CancelWatch (withEscCancel)
import Agent.CLI.Interrupt (withTurnCancel)
import Agent.CLI.Plan (extractProposedPlan, planDecisionFollowUp)
import Agent.CLI.ProviderFallback (isProviderUnavailable)
import Agent.CLI.ProviderTransition
    ( PendingTurn(..)
    , TurnResult(..)
    )
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
    , appendTurn
    , ensureSession
    )
import Agent.CLI.SessionEnv (SessionEnv(..))
import Agent.CLI.Status (formatTokenUsage)
import Agent.CLI.Style
    ( cliWindowTitle
    , glyphSession
    , roleMuted
    , setCliWindowTitle
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
import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Exception.Safe (onException)
import Data.IORef
    ( atomicModifyIORef'
    , modifyIORef'
    , readIORef
    , writeIORef
    )
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import System.IO (hIsTerminalDevice, stderr, stdout)

runOneTurn :: SessionEnv -> Text -> [TurnInput] -> IO TurnResult
runOneTurn env@SessionEnv
    { sessionLoop = config
    , sessionRender = render
    , sessionPrevious = previous
    , sessionPrinted = printed
    , sessionTranscript = transcriptRef
    , sessionPersist = persist
    , sessionPlanMode = planMode
    , sessionAgentsContext = agentsContext
    , sessionEscPaused = escPaused
    , sessionInterrupt = interrupt
    , sessionStoreRoot = storeRoot
    , sessionUsage = usageRef
    , sessionLastAssistant = lastAssistantRef
    , sessionTerminal = terminal
    , sessionBeginSubagentTurn = beginSubagentTurn
    , sessionFinishSubagentTurn = finishSubagentTurn
    , sessionAbortSubagentTurn = abortSubagentTurn
    , sessionOnPersisted = onPersisted
    } promptText inputs =
  withTurnCancel interrupt config.loopCancel $
  withEscCancel config.loopCancel escPaused do
    pending <- readIORef planMode.planStateRef
    when (pending == PlanPending) (activatePlanMode planMode)
    -- Create the session directory before tools run so first-turn subagents
    -- can persist under agents/<id>/ as they complete.
    case persist of
        Just slotRef -> do
            created <- isLeftSlot <$> readIORef slotRef
            handle <- ensureSession slotRef
            onPersisted handle
            writeIORef planMode.planSessionDir (Just handle.sessionDir)
            writeIORef storeRoot (Just handle.sessionDir)
            when created do
                color <- resolveColor stderr
                putTextLn stderr
                    (roleMuted color
                        (glyphSession <> "session: " <> handle.sessionMeta.metaId))
        Nothing -> pure ()
    prev <- readIORef previous
    beforeItems <- readIORef transcriptRef
    pendingAgents <- atomicModifyIORef' agentsContext \pendingCtx -> (Nothing, pendingCtx)
    planActive <- isPlanModeActive planMode
    planPath <- planFilePath planMode
    let planReminder =
            if planActive
                then Just (planModeReminder planPath)
                else Nothing
        agentsInput = case pendingAgents of
            Just agents | null beforeItems && isNothing prev ->
                Just (UserMessage agents)
            _ -> Nothing
        baseInputs = maybe inputs (: inputs) agentsInput
        restoreAgentsInput = case agentsInput of
            Just (UserMessage agents) ->
                atomicModifyIORef' agentsContext \current ->
                    (current <|> Just agents, ())
            _ -> pure ()
        turnInputs0 = case planReminder of
            Just reminder -> UserMessage reminder : baseInputs
            Nothing -> baseInputs
    turnInputs <- stampTurnInputs turnInputs0
    startedAt <- readIORef render.renderStartedAt
    wallStarted <- getCurrentTime
    when terminal.terminalSemanticPrompts $
        emitTerminalSequence terminal stdout osc133CommandStart
    rootTurnId <- beginSubagentTurn
    result <- runLoopInputs config prev turnInputs
        `onException` abortSubagentTurn rootTurnId
    clearThinking render
    finishedAt <- getCurrentTime
    let elapsedDetail extra = case startedAt of
            Nothing -> extra
            Just t0 -> extra <> " · " <> formatElapsed (realToFrac (diffUTCTime finishedAt t0))
        persistIncomplete errorText = case persist of
            Nothing -> pure ()
            Just slotRef -> do
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
                writeIORef slotRef (Right handle')
    case result of
        Left cancelled@(LoopCancelled _) -> do
            restoreAgentsInput
            finishTerminal terminal wallStarted finishedAt 130 "Agent cancelled"
            abortSubagentTurn rootTurnId
            writeIORef transcriptRef beforeItems
            color <- resolveColor stderr
            putTextLn stderr (formatLoopErrorColored color cancelled)
            model <- readIORef render.renderModelRef
            putTextLn stderr (formatTurnStatus color "cancelled" (elapsedDetail model))
            persistIncomplete "cancelled"
            pure TurnSucceeded
        Left err -> do
            restoreAgentsInput
            abortSubagentTurn rootTurnId
            afterItems <- readIORef transcriptRef
            case err of
                LoopTransport apiError
                    | length afterItems == length beforeItems
                    , isProviderUnavailable apiError -> do
                        finishTerminal terminal wallStarted finishedAt 1
                            "Agent provider unavailable"
                        planState <- readIORef planMode.planStateRef
                        pure $ TurnProviderUnavailable apiError PendingTurn
                            { pendingPromptText = promptText
                            , pendingInputs = inputs
                            , pendingExitAfter = False
                            , pendingPlanState = planState
                            }
                _ -> do
                    finishTerminal terminal wallStarted finishedAt 1 "Agent turn failed"
                    writeIORef transcriptRef beforeItems
                    color <- resolveColor stderr
                    putTextLn stderr (formatLoopErrorColored color err)
                    model <- readIORef render.renderModelRef
                    putTextLn stderr (formatTurnStatus color "error" (elapsedDetail model))
                    persistIncomplete (Text.pack (show err))
                    pure TurnFailed
        Right loopResult -> do
            finishTerminal terminal wallStarted finishedAt 0 "Agent finished"
            finishSubagentTurn rootTurnId
            writeIORef previous (Just loopResult.finalResponseId)
            modifyIORef' usageRef (`addTokenUsage` loopResult.tokenUsage)
            do
                color <- resolveColor stderr
                model <- readIORef render.renderModelRef
                let turns = Text.pack (show loopResult.turnsUsed)
                    unit = if loopResult.turnsUsed == 1 then " turn" else " turns"
                    usageDetail = formatTokenUsage loopResult.tokenUsage
                    extra =
                        if Text.null usageDetail
                            then model <> " · " <> turns <> unit
                            else model <> " · " <> turns <> unit <> " · " <> usageDetail
                putTextLn stderr
                    (formatTurnStatus color "ok" (elapsedDetail extra))
            followUp <- handleProposedPlan planMode loopResult.finalText
            printedText <- readIORef printed
            let assistantText =
                    fmap stripBracketedTimestamps loopResult.finalText
            writeIORef lastAssistantRef assistantText
            case (printedText, assistantText) of
                (False, Just text) | not (Text.null (Text.strip text)) -> do
                    useColor <- resolveColor stdout
                    putTextLn stdout (renderAssistantText useColor text)
                _ -> pure ()
            afterItems <- readIORef transcriptRef
            let newItems = drop (length beforeItems) afterItems
            case persist of
                Nothing -> pure ()
                Just slotRef -> do
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
                    writeIORef slotRef (Right handle')
                    when (handle'.sessionMeta.metaTitle /= handle.sessionMeta.metaTitle) do
                        tty <- hIsTerminalDevice stdout
                        setCliWindowTitle tty stdout
                            (cliWindowTitle handle'.sessionMeta.metaCwd
                                (Just handle'.sessionMeta.metaTitle))
            case followUp of
                Nothing -> pure TurnSucceeded
                Just notes -> do
                    writeIORef printed False
                    runOneTurn env notes [UserMessage notes]

isLeftSlot :: Either a b -> Bool
isLeftSlot = \case
    Left _ -> True
    Right _ -> False

finishTerminal
    :: TerminalCapabilities
    -> UTCTime
    -> UTCTime
    -> Int
    -> Text
    -> IO ()
finishTerminal terminal started finished exitCode message = do
    when terminal.terminalSemanticPrompts $
        emitTerminalSequence terminal stdout
            (osc133CommandFinished (Just exitCode))
    let seconds = realToFrac (diffUTCTime finished started) :: Double
    when (exitCode /= 0 || seconds >= 10) $
        notifyTerminal terminal stdout message

handleProposedPlan :: PlanModeEnv -> Maybe Text -> IO (Maybe Text)
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
