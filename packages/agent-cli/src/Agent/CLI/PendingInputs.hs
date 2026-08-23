-- | Queue parent inputs that must survive failed or interrupted submissions.
module Agent.CLI.PendingInputs
    ( withPendingInputs
    ) where

import Agent.Loop (Backend(..), TurnInput)
import Control.Exception.Safe (onException)
import Data.IORef (IORef, atomicModifyIORef')

withPendingInputs :: IORef [TurnInput] -> Backend state -> Backend state
withPendingInputs pending (Backend submit) =
    Backend \state previous inputs onEvent -> do
        queued <- atomicModifyIORef' pending \xs -> ([], xs)
        let requeue =
                atomicModifyIORef' pending \current ->
                    (queued <> current, ())
            prefixed
                | null queued = inputs
                | otherwise = queued <> inputs
        result <- submit state previous prefixed onEvent `onException` requeue
        case result of
            Left _ -> requeue
            Right _ -> pure ()
        pure result
