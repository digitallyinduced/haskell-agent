-- | Queue parent inputs that must survive failed or interrupted submissions.
module Agent.CLI.PendingInputs
    ( PendingInputs
    , newPendingInputs
    , clearPendingInputs
    , enqueuePendingInput
    , withPendingInputs
    ) where

import Agent.Loop (Backend(..), TurnInput)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception.Safe (mask, onException)
import Data.Foldable (toList)
import Data.IORef (IORef, atomicModifyIORef')
import qualified Data.IORef
import qualified Data.Sequence as Seq

data PendingState = PendingState
    { pendingEpoch :: !Word
    , pendingQueue :: !(Seq.Seq TurnInput)
    }

data PendingBatch = PendingBatch !Word !(Seq.Seq TurnInput)

data PendingInputs = PendingInputs
    (IORef PendingState)
    (MVar ())

newPendingInputs :: IO PendingInputs
newPendingInputs = PendingInputs
    <$> Data.IORef.newIORef (PendingState 0 Seq.empty)
    <*> newMVar ()

clearPendingInputs :: PendingInputs -> IO ()
clearPendingInputs (PendingInputs pending _) =
    atomicModifyIORef' pending \state ->
        (state { pendingEpoch = epochOf state + 1
               , pendingQueue = Seq.empty
               }, ())

enqueuePendingInput :: PendingInputs -> TurnInput -> IO ()
enqueuePendingInput (PendingInputs pending _) input =
    atomicModifyIORef' pending \state ->
        (state { pendingQueue = queueOf state Seq.|> input }, ())

drainPendingInputs :: IORef PendingState -> IO PendingBatch
drainPendingInputs pending =
    atomicModifyIORef' pending \state ->
        (state { pendingQueue = Seq.empty }
        , PendingBatch (epochOf state) (queueOf state))

requeuePendingInputs :: IORef PendingState -> PendingBatch -> IO ()
requeuePendingInputs pending (PendingBatch epoch queued) =
    atomicModifyIORef' pending \state ->
        if epochOf state == epoch
            then (state { pendingQueue = queued <> queueOf state }, ())
            else (state, ())

epochOf :: PendingState -> Word
epochOf (PendingState epoch _) = epoch

queueOf :: PendingState -> Seq.Seq TurnInput
queueOf (PendingState _ queue) = queue

withPendingInputs :: PendingInputs -> Backend -> Backend
withPendingInputs (PendingInputs pending lifecycle) (Backend submit) =
    Backend \state previous inputs onEvent ->
        mask \restore ->
            withMVar lifecycle \_ -> do
                batch <- drainPendingInputs pending
                let queued = case batch of
                        PendingBatch _ values -> values
                    requeue = requeuePendingInputs pending batch
                    prefixed
                        | Seq.null queued = inputs
                        | otherwise = toList queued <> inputs
                result <- restore
                    (submit state previous prefixed onEvent)
                    `onException` requeue
                case result of
                    Left _ -> requeue
                    Right _ -> pure ()
                pure result
