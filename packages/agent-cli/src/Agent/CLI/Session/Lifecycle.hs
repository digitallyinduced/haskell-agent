-- | Pending-turn execution and turn-completion transitions.
module Agent.CLI.Session.Lifecycle
    ( SessionContinuation(..)
    , finishTurn
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
    ( setSessionEffort
    , syncFullscreenPrompt
    )
import Agent.CLI.Session.Retry (waitAndRetryPendingTurn)
import Agent.CLI.SessionEnv (SessionEnv(..))
import Agent.CLI.Render
    ( RenderConfig(..)
    , putTextLn
    , renderPrintedText
    )
import Agent.CLI.TUI.App
    ( emitUiEvent
    , hasQueuedFullscreenInput
    )
import Agent.CLI.Turn (runOneTurn)
import Agent.Tools.PlanMode (PlanModeEnv(..))
import Agent.TUI.Model (UiEvent(..))
import Control.Exception.Safe (throwIO)
import Control.Monad (unless, when)
import Data.IORef
    ( readIORef
    , writeIORef
    )
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
    result <- runOneTurn env pending.pendingPromptText pending.pendingInputs
    finishTurnWithCooldownRetry
        continuation allowCooldownRetry env pending.pendingExitAfter result

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
        writeIORef env.sessionUnavailableProviders []
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
    TurnFailed ->
        if exitAfter
            then
                if env.sessionBackground
                    then throwIO (StartupFailure "agent turn failed")
                    else exitFailure
            else do
                case env.sessionFullscreen of
                    Nothing -> putTrailingNewline env.sessionRender
                    Just _ -> pure ()
                continueAfterTurn continuation env
    TurnRestartRequested level pending -> do
        setSessionEffort env level
        writeIORef env.sessionPlanMode.planStateRef pending.pendingPlanState
        case env.sessionFullscreen of
            Just runtime ->
                emitUiEvent runtime
                    (UiSystemMessage
                        ("restarting current turn with " <> level <> " effort"))
            Nothing -> pure ()
        result <- runOneTurn env pending.pendingPromptText pending.pendingInputs
        finishTurnWithCooldownRetry
            continuation allowCooldownRetry env exitAfter result
    TurnProviderUnavailable apiError pending ->
        let pending' = setPendingExitAfter exitAfter pending
        in do
            now <- getCurrentTime
            case providerRecoveryPreference
                    allowCooldownRetry now apiError of
                RetryCurrentProviderAfter delay ->
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
    when (not queued && not env.sessionBackground) $
        notifyAttention env.sessionRender.renderStderr InputRequested
    continuation.resumeSession env

putTrailingNewline :: RenderConfig -> IO ()
putTrailingNewline render = do
    didPrint <- renderPrintedText render
    when didPrint (putTextLn render.renderStdout "")
