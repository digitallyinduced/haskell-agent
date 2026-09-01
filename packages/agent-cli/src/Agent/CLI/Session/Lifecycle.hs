-- | Pending-turn execution and turn-completion transitions.
module Agent.CLI.Session.Lifecycle
    ( SessionContinuation(..)
    , finishTurn
    , retryFailedTurn
    , runPendingTurn
    ) where

import Agent.CLI.Notification
    ( AttentionRequest(InputRequested)
    , notifyAttention
    )
import Agent.CLI.Error (formatApiErrorAt)
import Agent.CLI.Project (saveProjectAccount)
import Agent.CLI.Recap (RecapRequest(RecapTurnSummary))
import Agent.CLI.Provider.Switch
    ( reportProviderUnavailable
    , requestAutomaticProviderFallback
    )
import Agent.CLI.ProviderFallback
    ( ProviderRecoveryPreference(..)
    , providerRecoveryPreference
    )
import Agent.CLI.ProviderTransition
    ( PendingTurn(..)
    , TurnResult(..)
    , setPendingExitAfter
    )
import Agent.CLI.Runtime.Types
    ( PendingTurnPresentation(..)
    , RunResult(..)
    , StartupFailure(..)
    )
import Agent.CLI.Session.Interaction
    ( setSessionEffortText
    , syncFullscreenPrompt
    )
import Agent.CLI.Session.Retry (waitAndRetryPendingTurn)
import Agent.CLI.SessionEnv (SessionEnv(..))
import Agent.CLI.SteeringInputs (hasBackgroundCompletionWake)
import Agent.CLI.Render
    ( RenderConfig(..)
    , putTextLn
    , renderPrintedText
    )
import Agent.CLI.TUI.App
    ( emitUiEvent
    , hasQueuedFullscreenInput
    )
import Agent.CLI.Turn (retryCheckpointedTurn, runOneTurn)
import Agent.Tools.PlanMode (PlanModeEnv(..))
import Agent.TUI.Model (UiEvent(..))
import Control.Exception.Safe (throwIO)
import Control.Monad (unless, when)
import Data.IORef
    ( readIORef
    , writeIORef
    )
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (getCurrentTime)
import System.Exit (exitFailure)

data SessionContinuation = SessionContinuation
    { resumeSession :: SessionEnv -> IO RunResult
    , resumeSessionWithDraft :: SessionEnv -> Text -> IO RunResult
    }

runPendingTurn
    :: SessionContinuation
    -> PendingTurnPresentation
    -> SessionEnv
    -> PendingTurn
    -> IO RunResult
runPendingTurn continuation presentation =
    runPendingTurnWithCooldownRetry continuation True presentation

-- | Retry a failed turn. Terminal failures and committed compactions retain
-- their exact input checkpoint; pre-commit restart/provider failures must
-- resubmit the original raw inputs instead.
retryFailedTurn
    :: SessionContinuation
    -> SessionEnv
    -> PendingTurn
    -> IO RunResult
retryFailedTurn continuation env pending = do
    writeIORef env.sessionPlanMode.planStateRef pending.pendingPlanState
    syncFullscreenPrompt env
    case env.sessionFullscreen of
        Nothing -> pure ()
        Just runtime -> emitUiEvent runtime UiTurnRestarted
    writeIORef env.sessionLastFailedTurn Nothing
    -- The failed turn already owns the persisted/displayed user prompt.
    -- Keep the retry turn's user text empty so session history does not show
    -- the same prompt twice.
    result <- runPendingAttempt env pending
    finishTurnWithCooldownRetry
        continuation True env pending.pendingExitAfter result

runPendingTurnWithCooldownRetry
    :: SessionContinuation
    -> Bool
    -> PendingTurnPresentation
    -> SessionEnv
    -> PendingTurn
    -> IO RunResult
runPendingTurnWithCooldownRetry
    continuation allowCooldownRetry presentation env pending = do
    writeIORef env.sessionPlanMode.planStateRef pending.pendingPlanState
    syncFullscreenPrompt env
    case env.sessionFullscreen of
        Nothing -> pure ()
        Just runtime -> case presentation of
            SubmitPendingTurn ->
                emitUiEvent runtime
                    (UiUserSubmitted pending.pendingPromptText)
            RestartPendingTurn ->
                emitUiEvent runtime UiTurnRestarted
            ContinuePendingTurn ->
                pure ()
    writeIORef env.sessionLastFailedTurn Nothing
    result <- runPendingAttempt env pending
    finishTurnWithCooldownRetry
        continuation allowCooldownRetry env pending.pendingExitAfter result

runPendingAttempt :: SessionEnv -> PendingTurn -> IO TurnResult
runPendingAttempt env pending
    | pending.pendingCheckpointed = retryCheckpointedTurn env
    | otherwise =
        runOneTurn env pending.pendingPromptText pending.pendingInputs

finishTurn
    :: SessionContinuation
    -> SessionEnv
    -> Bool
    -> TurnResult
    -> IO RunResult
finishTurn continuation =
    finishTurnWithCooldownRetry continuation True

finishTurnWithCooldownRetry
    :: SessionContinuation
    -> Bool
    -> SessionEnv
    -> Bool
    -> TurnResult
    -> IO RunResult
finishTurnWithCooldownRetry continuation allowCooldownRetry env exitAfter = \case
    TurnSucceeded -> do
        env.sessionQueueRecap RecapTurnSummary
        writeIORef env.sessionUnavailableProviders Set.empty
        selectionId <- readIORef env.sessionAccountSelectionId
        accountId <- readIORef env.sessionAccountId
        when
            (not (Text.null (Text.strip selectionId))
                && not (Text.null (Text.strip accountId))) $
            saveProjectAccount
                env.sessionProjectRoot
                env.sessionProvider
                selectionId
                accountId
        case env.sessionFullscreen of
            Nothing -> putTrailingNewline env.sessionRender
            Just _ -> pure ()
        if exitAfter
            then pure RunQuit
            else continueAfterTurn continuation env
    TurnCancelled -> do
        case env.sessionFullscreen of
            Nothing -> putTrailingNewline env.sessionRender
            Just _ -> pure ()
        if exitAfter
            then pure RunQuit
            else continuation.resumeSession env
    TurnFailed pending -> do
        writeIORef env.sessionLastFailedTurn
            (Just (setPendingExitAfter exitAfter pending))
        if exitAfter
            then
                if env.sessionBackground
                    then
                        throwIO (StartupFailure "agent turn failed")
                    else exitFailure
            else do
                case env.sessionFullscreen of
                    Nothing -> putTrailingNewline env.sessionRender
                    Just _ -> pure ()
                continueAfterTurn continuation env
    TurnRestartRequested level pending -> do
        setSessionEffortText env level
        writeIORef env.sessionPlanMode.planStateRef pending.pendingPlanState
        case env.sessionFullscreen of
            Just runtime ->
                emitUiEvent runtime
                    (UiSystemMessage
                        ("restarting current turn with " <> level <> " effort"))
            Nothing -> pure ()
        result <- runPendingAttempt env pending
        finishTurnWithCooldownRetry
            continuation allowCooldownRetry env exitAfter result
    TurnProviderUnavailable apiError pending ->
        let pending' = setPendingExitAfter exitAfter pending
        in do
            now <- getCurrentTime
            case providerRecoveryPreference
                    allowCooldownRetry now apiError of
                RetryCurrentProviderAfter delay -> do
                    writeIORef env.sessionLastFailedTurn (Just pending')
                    waitAndRetryPendingTurn
                        (continuation.resumeSessionWithDraft env)
                        (runPendingTurnWithCooldownRetry
                            continuation False RestartPendingTurn env)
                        env
                        delay
                        pending'
                TryProviderFallback ->
                    requestAutomaticProviderFallback env apiError pending'
                        >>= \case
                        Just providerTransition ->
                            pure (RunSwitchProvider providerTransition)
                        Nothing -> do
                            writeIORef
                                env.sessionLastFailedTurn
                                (Just pending')
                            if env.sessionBackground
                                then
                                    putTextLn
                                        env.sessionRender.renderStderr
                                        (formatApiErrorAt now apiError)
                                else
                                    reportProviderUnavailable
                                        env.sessionFullscreen apiError
                            if exitAfter
                                then
                                    if env.sessionBackground
                                        then throwIO
                                            (StartupFailure
                                                "agent provider unavailable")
                                        else exitFailure
                                else do
                                    unless env.sessionBackground $
                                        notifyAttention
                                            env.sessionRender.renderStderr
                                            InputRequested
                                    continuation.resumeSessionWithDraft
                                        env
                                        pending.pendingPromptText

continueAfterTurn
    :: SessionContinuation
    -> SessionEnv
    -> IO RunResult
continueAfterTurn continuation env = do
    queued <- case env.sessionFullscreen of
        Nothing -> pure False
        Just runtime -> hasQueuedFullscreenInput runtime
    backgroundCompletion <-
        hasBackgroundCompletionWake env.sessionSteeringInputs
    failedTurn <- readIORef env.sessionLastFailedTurn
    let willWake = case env.sessionFullscreen of
            Just _ -> backgroundCompletion && isNothing failedTurn
            Nothing -> False
    when (not queued && not willWake && not env.sessionBackground) $
        notifyAttention env.sessionRender.renderStderr InputRequested
    continuation.resumeSession env

putTrailingNewline :: RenderConfig -> IO ()
putTrailingNewline render = do
    didPrint <- renderPrintedText render
    when didPrint (putTextLn render.renderStdout "")
