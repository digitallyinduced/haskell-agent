-- | Provider cooldown waiting and retry presentation for an active session.
module Agent.CLI.Session.Retry
    ( waitAndRetryPendingTurn
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
import Agent.CLI.Session
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
    , addUTCTime
    , diffUTCTime
    , getCurrentTime
    )
import System.Timeout (timeout)

waitAndRetryPendingTurn
    :: (Text -> IO RunResult)
    -> (PendingTurn -> IO RunResult)
    -> SessionEnv
    -> NominalDiffTime
    -> PendingTurn
    -> IO RunResult
waitAndRetryPendingTurn resumeDraft retryPending env delay pending = do
    startedAt <- getCurrentTime
    let retryAt = addUTCTime (max 0 delay) startedAt
        cancel = env.sessionLoop.loopCancel
        renderCountdown seconds =
            let message = automaticRetryCountdownText seconds
            in case env.sessionFullscreen of
                Just runtime ->
                    emitUiEvent runtime
                        (UiSetNotice (Just (progressNotice message)))
                Nothing ->
                    renderEvent env.sessionRender (ActivityUpdated message)
        waitForCancel = do
            let poll lastShown = do
                    now <- getCurrentTime
                    let remaining = max 0 (diffUTCTime retryAt now)
                        seconds = max 0 (ceiling remaining)
                    when (lastShown /= Just seconds) (renderCountdown seconds)
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
                                else poll (Just seconds)
            poll Nothing
        waitAction = case env.sessionFullscreen of
            Just _ -> waitForCancel
            Nothing
                | env.sessionBackground -> waitForCancel
                | otherwise ->
                    withEscCancel cancel env.sessionStdinControl waitForCancel
    setPersistenceActivity
        env.sessionPersist
        "provider_cooldown"
        "Provider temporarily unavailable; waiting before automatically retrying the pending turn."
        (Just retryAt)
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
    if cancelled
        then do
            case env.sessionFullscreen of
                Just runtime ->
                    emitUiEvent runtime
                        (UiSetNotice
                            (Just
                                (infoNotice
                                    "automatic retry cancelled")))
                Nothing -> do
                    let output = env.sessionRender.renderStderr
                    color <- resolveColor output
                    putTextLn output
                        (roleMuted color "automatic retry cancelled")
            if pending.pendingExitAfter
                then pure RunQuit
                else resumeDraft pending.pendingPromptText
        else do
            case env.sessionFullscreen of
                Just runtime ->
                    emitUiEvent runtime
                        (UiSetNotice
                            (Just (successNotice "retrying turn")))
                Nothing -> do
                    let output = env.sessionRender.renderStderr
                    color <- resolveColor output
                    putTextLn output
                        (roleMuted color (glyphOk <> "retrying turn"))
            setPersistenceActivity
                env.sessionPersist
                "provider_retry"
                "Retrying the pending turn after the provider cooldown."
                Nothing
            retryPending pending
                `finally` clearPersistenceActivity env.sessionPersist
