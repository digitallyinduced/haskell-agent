-- | Provider cooldown waiting and automatic retry presentation for an active
-- session.
module Agent.CLI.Session.Retry
    ( AutomaticWait(..)
    , awaitAutomaticRetry
    , waitAndRetryPendingTurn
    , waitAndResumeAfterUsageLimit
    ) where

import Agent.Cancel (resetCancel, waitCancel)
import Agent.CLI.CancelWatch (withEscCancel)
import Agent.CLI.Interrupt (withTurnCancel)
import Agent.CLI.ProviderFallback (automaticRetryCountdownText)
import Agent.CLI.ProviderTransition (PendingTurn(..))
import Agent.CLI.Render
    ( RenderConfig(..)
    , clearThinking
    , putTextLn
    , renderEvent
    )
import Agent.CLI.Runtime.Types (RunResult(..))
import Agent.CLI.UsageLimitRecovery
    ( usageLimitResumeTimeText
    , usageLimitWaitText
    )
import Agent.Runtime.Session
    ( clearPersistenceActivity
    , setPersistenceActivity
    )
import Agent.CLI.SessionEnv (SessionEnv(..))
import Agent.CLI.Style
    ( glyphOk
    , roleMuted
    )
import Agent.CLI.Terminal (resolveColor)
import Agent.CLI.TUI.App (emitUiEvent)
import Agent.Loop (LoopConfig(..), LoopEvent(..))
import Agent.TUI.Model
    ( UiEvent(..)
    , infoNotice
    , progressNotice
    , successNotice
    )
import Control.Exception.Safe (finally)
import Control.Monad (when)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Time.Clock
    ( NominalDiffTime
    , UTCTime
    , addUTCTime
    , diffUTCTime
    , getCurrentTime
    )
import Data.Time.LocalTime (getTimeZone)
import System.Timeout (timeout)

-- | A cancellable wait for a provider deadline, with its live presentation
-- and the activity marker recorded for session observers.
data AutomaticWait = AutomaticWait
    { waitDeadline :: !UTCTime
    , waitActivityKind :: !Text
    , waitActivityMessage :: !Text
    -- | Live status text for the remaining whole seconds.
    , waitCountdownText :: !(Int -> Text)
    , waitCancelledText :: !Text
    }

-- | Block until the deadline passes or the user cancels with Esc or Ctrl-C.
-- Returns 'True' when the wait was cancelled.
awaitAutomaticRetry :: SessionEnv -> AutomaticWait -> IO Bool
awaitAutomaticRetry env wait = do
    let deadline = wait.waitDeadline
        cancel = env.sessionLoop.loopCancel
        renderCountdown message =
            case env.sessionFullscreen of
                Just runtime ->
                    emitUiEvent runtime
                        (UiSetNotice (Just (progressNotice message)))
                Nothing ->
                    renderEvent env.sessionRender (ActivityUpdated message)
        waitForCancel = do
            let poll lastShown = do
                    now <- getCurrentTime
                    let remaining = max 0 (diffUTCTime deadline now)
                        message =
                            wait.waitCountdownText (max 0 (ceiling remaining))
                    when (lastShown /= Just message) (renderCountdown message)
                    if remaining <= 0
                        then
                            -- Give the provider reset boundary a small margin
                            -- so the retry does not race a rounded timestamp.
                            isJust <$> timeout 250000 (waitCancel cancel)
                        else do
                            let waitMicros =
                                    max 1 $
                                        min 1000000
                                            (ceiling
                                                (realToFrac remaining
                                                    * 1_000_000
                                                    :: Double))
                            cancelled <-
                                isJust <$> timeout waitMicros (waitCancel cancel)
                            if cancelled
                                then pure True
                                else poll (Just message)
            poll Nothing
        waitAction = case env.sessionFullscreen of
            Just _ -> waitForCancel
            Nothing
                | env.sessionBackground -> waitForCancel
                | otherwise ->
                    withEscCancel cancel env.sessionStdinControl waitForCancel
    setPersistenceActivity
        env.sessionPersist
        wait.waitActivityKind
        wait.waitActivityMessage
        (Just deadline)
    resetCancel cancel
    case env.sessionFullscreen of
        Just _ -> pure ()
        Nothing -> renderEvent env.sessionRender TurnStarted
    cancelled <-
        (withTurnCancel env.sessionInterrupt cancel waitAction)
            `finally` do
                resetCancel cancel
                clearPersistenceActivity env.sessionPersist
                case env.sessionFullscreen of
                    Just _ -> pure ()
                    Nothing -> clearThinking env.sessionRender
    when cancelled $
        case env.sessionFullscreen of
            Just runtime ->
                emitUiEvent runtime
                    (UiSetNotice (Just (infoNotice wait.waitCancelledText)))
            Nothing -> do
                let output = env.sessionRender.renderStderr
                color <- resolveColor output
                putTextLn output (roleMuted color wait.waitCancelledText)
    pure cancelled

waitAndRetryPendingTurn
    :: (Text -> IO RunResult)
    -> (PendingTurn -> IO RunResult)
    -> SessionEnv
    -> NominalDiffTime
    -> PendingTurn
    -> IO RunResult
waitAndRetryPendingTurn resumeDraft retryPending env delay pending = do
    startedAt <- getCurrentTime
    cancelled <-
        awaitAutomaticRetry env AutomaticWait
            { waitDeadline = addUTCTime (max 0 delay) startedAt
            , waitActivityKind = "provider_cooldown"
            , waitActivityMessage =
                "Provider temporarily unavailable; waiting before \
                \automatically retrying the pending turn."
            , waitCountdownText = automaticRetryCountdownText
            , waitCancelledText = "automatic retry cancelled"
            }
    if cancelled
        then
            if pending.pendingExitAfter
                then pure RunQuit
                else resumeDraft pending.pendingPromptText
        else do
            reportResumption env "retrying turn"
            setPersistenceActivity
                env.sessionPersist
                "provider_retry"
                "Retrying the pending turn after the provider cooldown."
                Nothing
            retryPending pending
                `finally` clearPersistenceActivity env.sessionPersist

-- | Wait for a usage-window reset, then resume the session's work. The
-- cancellation continuation returns control to the user with the failed turn
-- still available for a manual retry.
waitAndResumeAfterUsageLimit
    :: IO RunResult
    -> IO RunResult
    -> SessionEnv
    -> UTCTime
    -> IO RunResult
waitAndResumeAfterUsageLimit onCancelled resume env resumeAt = do
    now <- getCurrentTime
    zone <- getTimeZone resumeAt
    cancelled <-
        awaitAutomaticRetry env AutomaticWait
            { waitDeadline = resumeAt
            , waitActivityKind = "usage_limit_wait"
            , waitActivityMessage =
                "Usage limit reached; waiting for the provider reset before \
                \automatically resuming."
            , waitCountdownText =
                usageLimitWaitText (usageLimitResumeTimeText zone now resumeAt)
            , waitCancelledText = "automatic resume cancelled"
            }
    if cancelled
        then onCancelled
        else do
            reportResumption env "usage limit reset; resuming"
            setPersistenceActivity
                env.sessionPersist
                "usage_limit_resume"
                "Resuming the session after the usage limit reset."
                Nothing
            resume `finally` clearPersistenceActivity env.sessionPersist

reportResumption :: SessionEnv -> Text -> IO ()
reportResumption env message =
    case env.sessionFullscreen of
        Just runtime ->
            emitUiEvent runtime (UiSetNotice (Just (successNotice message)))
        Nothing -> do
            let output = env.sessionRender.renderStderr
            color <- resolveColor output
            putTextLn output (roleMuted color (glyphOk <> message))
