-- | Keep a provider submission alive while the machine is offline.
--
-- Recovery wraps one 'Backend' submission rather than an entire agent turn.
-- If a connection drops after tools have run, the loop can therefore retry
-- the exact model continuation without executing those tools again.
module Agent.CLI.Connectivity
    ( reconnectDelayMicros
    , withConnectionRecovery
    , withConnectionRecoveryUsing
    ) where

import Agent.Error (ApiError(..))
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

-- | Automatically retry connection failures until the submission succeeds or
-- its owner cancels it. 'runLoopInputs' races every backend submission against
-- the session cancel flag, so the sleep and all retries remain scoped to the
-- current turn.
withConnectionRecovery :: Backend state -> Backend state
withConnectionRecovery =
    withConnectionRecoveryUsing
        (threadDelay . reconnectDelayMicros)

-- | Injectable variant used by tests.
--
-- Retrying stops once response text/reasoning has streamed. Replaying after
-- visible output could duplicate it; those rare mid-stream failures continue
-- through the existing error path instead.
withConnectionRecoveryUsing
    :: (Int -> IO ())
    -> Backend state
    -> Backend state
withConnectionRecoveryUsing waitBeforeRetry (Backend submit) =
    Backend \state previous inputs onEvent ->
        let go attempt = do
                streamed <- newIORef False
                result <- submit state previous inputs \event -> do
                    when (isStreamOutput event) (writeIORef streamed True)
                    onEvent event
                didStream <- readIORef streamed
                case result of
                    Left ConnectionError{}
                        | not didStream -> do
                            onEvent (ActivityUpdated (waitingMessage attempt))
                            waitBeforeRetry attempt
                            onEvent
                                (ActivityUpdated
                                    "Checking internet connection…")
                            go (attempt + 1)
                    _ -> pure result
        in go 1

isStreamOutput :: LoopEvent -> Bool
isStreamOutput = \case
    TextDelta _ -> True
    ReasoningDelta _ -> True
    _ -> False

waitingMessage :: Int -> Text
waitingMessage attempt =
    "Connection lost; waiting for internet. Retrying automatically in "
        <> formatDelay (reconnectDelayMicros attempt)
        <> " (Esc or Ctrl-C to cancel)…"

formatDelay :: Int -> Text
formatDelay micros =
    Text.pack (show ((micros + 999_999) `div` 1_000_000)) <> "s"
