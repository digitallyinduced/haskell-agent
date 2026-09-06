-- | Recover one provider submission from transient transport/server failures.
--
-- Recovery wraps one 'Backend' submission rather than an entire agent turn.
-- If a connection drops after tools have run, the loop can therefore retry
-- the exact model continuation without executing those tools again.
module Agent.Connectivity
    ( RecoveryWatcher(..)
    , reconnectDelayMicros
    , transientRetryDelayMicros
    , withConnectionRecovery
    , withConnectionRecoveryOn
    , withConnectionRecoveryUsing
    , withConnectionRecoveryUsingWatcher
    ) where

import Agent.Connectivity.NetworkPath
    ( NetworkRecovery
    , armNetworkRecovery
    )
import Agent.Error
    ( ApiError(..)
    , apiErrorRetryAfter
    , isInlineRetryableProviderError
    )
import Agent.Loop
    ( Backend(..)
    , BackendCallbacks(..)
    , BackendMiddleware
    , LoopEvent(..)
    , backendWithCallbacks
    )
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM
    ( TVar
    , atomically
    , newTVarIO
    , readTVar
    , readTVarIO
    , retry
    , writeTVar
    )
import Control.Monad (when)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as Text

-- | Arms an interruptible, restartable wait for the next network recovery.
-- One armed wait is shared by an in-flight request and its retry delay so an
-- edge racing the transport error cannot be lost between those two phases.
newtype RecoveryWatcher = RecoveryWatcher
    { armRecoveryWatcher :: IO (IO ())
    }

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
withConnectionRecovery :: BackendMiddleware
withConnectionRecovery =
    withConnectionRecoveryUsingMaybeWatcher threadDelay Nothing

-- | Retry as above, and also restart an in-flight provider submission as soon
-- as macOS reports that an unavailable network path is satisfied again.
withConnectionRecoveryOn :: Maybe NetworkRecovery -> BackendMiddleware
withConnectionRecoveryOn recovery =
    withConnectionRecoveryUsingMaybeWatcher
        threadDelay
        (networkRecoveryWatcher <$> recovery)

-- | Injectable variant used by tests. If a response already streamed, emit a
-- structural restart boundary before the next attempt. Providers may generate
-- a different continuation when replayed, so renderers keep the failed partial
-- attempt visible rather than trying to splice or silently deduplicate it.
withConnectionRecoveryUsing
    :: (Int -> IO ())
    -> BackendMiddleware
withConnectionRecoveryUsing waitMicros =
    withConnectionRecoveryUsingMaybeWatcher waitMicros Nothing

-- | Injectable recovery watcher used to verify the macOS edge-triggered path
-- without changing the host's real network configuration.
withConnectionRecoveryUsingWatcher
    :: (Int -> IO ())
    -> RecoveryWatcher
    -> BackendMiddleware
withConnectionRecoveryUsingWatcher waitMicros watcher =
    withConnectionRecoveryUsingMaybeWatcher waitMicros (Just watcher)

withConnectionRecoveryUsingMaybeWatcher
    :: (Int -> IO ())
    -> Maybe RecoveryWatcher
    -> BackendMiddleware
withConnectionRecoveryUsingMaybeWatcher
    waitMicros watcher backend =
    backendWithCallbacks \state previous inputs callbacks ->
        let go reconnectAttempt transientAttempt = do
                streamed <- newIORef False
                replayGate <- newTVarIO ReplayAllowed
                armedRecovery <- traverse (.armRecoveryWatcher) watcher
                outcome <-
                    raceWithRecovery replayGate armedRecovery $
                        backend.submitTurnWithCallbacks
                            state
                            previous
                            inputs
                            callbacks
                                { onLoopEvent = \event -> do
                                    when (isStreamOutput event)
                                        (writeIORef streamed True)
                                    -- A lower layer already closed the
                                    -- interrupted attempt with its own
                                    -- boundary; only output streamed
                                    -- afterwards needs another one before the
                                    -- next replay.
                                    when (isRestartBoundary event)
                                        (writeIORef streamed False)
                                    callbacks.onLoopEvent event
                                , onAsyncToolCall = \call -> do
                                    shouldAdmit <- atomically do
                                        readTVar replayGate >>= \case
                                            RecoveryCommitted -> pure False
                                            _ -> do
                                                writeTVar replayGate
                                                    AsyncAdmitted
                                                pure True
                                    -- If recovery won first, this provider
                                    -- attempt is already being discarded.
                                    -- Never expose its async call.
                                    when shouldAdmit
                                        (callbacks.onAsyncToolCall call)
                                }
                didStream <- readIORef streamed
                didAdmitAsync <-
                    (== AsyncAdmitted) <$> readTVarIO replayGate
                case outcome of
                    RecoveryObserved -> do
                        callbacks.onLoopEvent
                            (ActivityUpdated connectionRestoredMessage)
                        when didStream $
                            callbacks.onLoopEvent
                                (ResponseRestarted
                                    connectionRestartMessage)
                        go reconnectAttempt transientAttempt
                    OperationFinished result
                        | didAdmitAsync -> pure result
                    OperationFinished (Left ConnectionError{}) -> do
                        let delay = reconnectDelayMicros reconnectAttempt
                        callbacks.onLoopEvent
                            (ActivityUpdated
                                (connectionWaitingMessage delay))
                        recovered <-
                            waitForDelayOrRecovery
                                waitMicros armedRecovery delay
                        callbacks.onLoopEvent
                            (ActivityUpdated
                                (if recovered
                                    then connectionRestoredMessage
                                    else "Checking internet connection…"))
                        when didStream $
                            callbacks.onLoopEvent
                                (ResponseRestarted
                                    connectionRestartMessage)
                        go (reconnectAttempt + 1) transientAttempt
                    OperationFinished (Left apiError)
                        | isInlineRetryableProviderError apiError
                        , transientAttempt <= maxTransientRetries -> do
                            let delay =
                                    transientRetryDelayMicros
                                        apiError transientAttempt
                            callbacks.onLoopEvent
                                (ActivityUpdated
                                    (transientWaitingMessage
                                        transientAttempt delay))
                            waitMicros delay
                            callbacks.onLoopEvent
                                (ActivityUpdated
                                    (transientRetryingMessage
                                        transientAttempt))
                            when didStream $
                                callbacks.onLoopEvent
                                    (ResponseRestarted
                                        transientRestartMessage)
                            go reconnectAttempt (transientAttempt + 1)
                    OperationFinished result -> pure result
        in go 1 1

data RecoveryOutcome result
    = RecoveryObserved
    | OperationFinished !result

data ReplayGate
    = ReplayAllowed
    | AsyncAdmitted
    | RecoveryCommitted
    deriving (Eq)

networkRecoveryWatcher :: NetworkRecovery -> RecoveryWatcher
networkRecoveryWatcher recovery =
    RecoveryWatcher (armNetworkRecovery recovery)

raceWithRecovery
    :: TVar ReplayGate
    -> Maybe (IO ())
    -> IO result
    -> IO (RecoveryOutcome result)
raceWithRecovery replayGate armedRecovery operation =
    case armedRecovery of
        Nothing -> OperationFinished <$> operation
        Just waitForRecovery ->
            race
                operation
                recovery
                >>= \case
                    Left result -> pure (OperationFinished result)
                    Right () -> pure RecoveryObserved
          where
            recovery = do
                waitForRecovery
                atomically do
                    readTVar replayGate >>= \case
                        AsyncAdmitted ->
                            -- Block until the operation branch wins and
                            -- cancels this watcher.
                            retry
                        RecoveryCommitted -> pure ()
                        ReplayAllowed ->
                            writeTVar replayGate RecoveryCommitted

waitForDelayOrRecovery
    :: (Int -> IO ())
    -> Maybe (IO ())
    -> Int
    -> IO Bool
waitForDelayOrRecovery waitMicros armedRecovery delay =
    case armedRecovery of
        Nothing -> waitMicros delay >> pure False
        Just waitForRecovery ->
            race
                waitForRecovery
                (waitMicros delay)
                >>= \case
                    Left () -> pure True
                    Right () -> pure False

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

isRestartBoundary :: LoopEvent -> Bool
isRestartBoundary = \case
    ResponseRestarted _ -> True
    ResponseAttemptDiscarded -> True
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

connectionRestoredMessage :: Text
connectionRestoredMessage =
    "Internet connection restored; reconnecting…"

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
