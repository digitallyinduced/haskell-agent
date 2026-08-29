-- | Recover one provider submission from transient transport/server failures.
--
-- Recovery wraps one 'Backend' submission rather than an entire agent turn.
-- If a connection drops after tools have run, the loop can therefore retry
-- the exact model continuation without executing those tools again.
module Agent.CLI.Connectivity
    ( reconnectDelayMicros
    , transientRetryDelayMicros
    , withConnectionRecovery
    , withConnectionRecoveryUsing
    ) where

import Agent.Error
    ( ApiError(..)
    , apiErrorRetryAfter
    , isInlineRetryableProviderError
    )
import Agent.Loop (Backend(..), LoopEvent(..))
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as Text

-- | Retry after 1s, 2s, 4s, 8s, then every 15s while offline.
reconnectDelayMicros :: Int -> Int
reconnectDelayMicros attempt =
    case max 1 attempt of
        1 -> 1_000_000
        2 -> 2_000_000
        3 -> 4_000_000
        4 -> 8_000_000
        _ -> 15_000_000

-- | Retry transient provider failures after 3s, 6s, then 12s. A provider
-- supplied Retry-After takes precedence, capped at one minute.
transientRetryDelayMicros :: ApiError -> Int -> Int
transientRetryDelayMicros apiError attempt =
    case apiErrorRetryAfter apiError of
        Just seconds | seconds > 0 ->
            min 60 seconds * 1_000_000
        _ ->
            case max 1 attempt of
                1 -> 3_000_000
                2 -> 6_000_000
                _ -> 12_000_000

-- | Automatically retry connection failures until the submission succeeds or
-- its owner cancels it. Transient provider errors are retried a bounded number
-- of times after their lower-level, replay-safe retry policy gives up.
-- 'runLoopInputs' races every backend submission against the session cancel
-- flag, so the sleep and all retries remain scoped to the current turn.
withConnectionRecovery :: Backend -> Backend
withConnectionRecovery =
    withConnectionRecoveryUsing
        threadDelay

-- | Injectable variant used by tests. If a response already streamed, emit a
-- structural restart boundary before the next attempt. Providers may generate
-- a different continuation when replayed, so renderers keep the failed partial
-- attempt visible rather than trying to splice or silently deduplicate it.
withConnectionRecoveryUsing
    :: (Int -> IO ())
    -> Backend
    -> Backend
withConnectionRecoveryUsing waitMicros (Backend submit) =
    Backend \state previous inputs onEvent ->
        let go reconnectAttempt transientAttempt = do
                streamed <- newIORef False
                result <- submit state previous inputs \event -> do
                    when (isStreamOutput event) (writeIORef streamed True)
                    onEvent event
                didStream <- readIORef streamed
                case result of
                    Left ConnectionError{} -> do
                        let delay = reconnectDelayMicros reconnectAttempt
                        onEvent
                            (ActivityUpdated
                                (connectionWaitingMessage delay))
                        waitMicros delay
                        onEvent
                            (ActivityUpdated
                                "Checking internet connection…")
                        when didStream $
                            onEvent
                                (ResponseRestarted
                                    connectionRestartMessage)
                        go (reconnectAttempt + 1) transientAttempt
                    Left apiError
                        | isInlineRetryableProviderError apiError
                        , transientAttempt <= maxTransientRetries -> do
                            let delay =
                                    transientRetryDelayMicros
                                        apiError transientAttempt
                            onEvent
                                (ActivityUpdated
                                    (transientWaitingMessage
                                        transientAttempt delay))
                            waitMicros delay
                            onEvent
                                (ActivityUpdated
                                    (transientRetryingMessage
                                        transientAttempt))
                            when didStream $
                                onEvent
                                    (ResponseRestarted
                                        transientRestartMessage)
                            go reconnectAttempt (transientAttempt + 1)
                    _ -> pure result
        in go 1 1

maxTransientRetries :: Int
maxTransientRetries = 2

isStreamOutput :: LoopEvent -> Bool
isStreamOutput = \case
    TextDelta _ -> True
    ReasoningDelta _ -> True
    -- A tool call announced from the stream is already a visible running
    -- block; the replayed attempt must close it with the same restart
    -- boundary that text output gets.
    ToolStarted _ -> True
    ToolUpdated _ -> True
    _ -> False

connectionWaitingMessage :: Int -> Text
connectionWaitingMessage delay =
    "Connection lost; waiting for internet. Retrying automatically in "
        <> formatDelay delay
        <> " (Esc or Ctrl-C to cancel)…"

connectionRestartMessage :: Text
connectionRestartMessage =
    "Connection interrupted the response; restarting automatically. "
        <> "The new attempt may repeat partial output shown above."

transientWaitingMessage :: Int -> Int -> Text
transientWaitingMessage attempt delay =
    "Provider returned a temporary error; retrying automatically in "
        <> formatDelay delay
        <> " (attempt "
        <> Text.pack (show attempt)
        <> "/"
        <> Text.pack (show maxTransientRetries)
        <> "; Esc or Ctrl-C to cancel)…"

transientRetryingMessage :: Int -> Text
transientRetryingMessage attempt =
    "Retrying provider request (attempt "
        <> Text.pack (show attempt)
        <> "/"
        <> Text.pack (show maxTransientRetries)
        <> ")…"

transientRestartMessage :: Text
transientRestartMessage =
    "Provider interrupted the response; restarting automatically. "
        <> "The new attempt may repeat partial output shown above."

formatDelay :: Int -> Text
formatDelay micros =
    Text.pack (show ((micros + 999_999) `div` 1_000_000)) <> "s"
