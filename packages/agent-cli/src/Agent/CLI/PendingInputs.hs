-- | Queue parent inputs that must survive failed or interrupted submissions.
module Agent.CLI.PendingInputs
    ( PendingInputs
    , PendingNoticeKind(..)
    , newPendingInputs
    , clearPendingInputs
    , enqueuePendingInput
    , enqueuePendingNotice
    , pendingInputByteLimit
    , pendingInputCountLimit
    , withPendingInputs
    ) where

import Agent.CLI.PendingInputs.Model
    ( PendingNoticeKind(..)
    , PendingState
    , pendingInputByteLimit
    , pendingInputCountLimit
    )
import qualified Agent.CLI.PendingInputs.Model as Model
import Agent.Loop
    ( Backend(..)
    , BackendMiddleware
    , TurnInput(..)
    , backendWithCallbacks
    )
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception.Safe (mask, onException)
import Data.IORef (IORef, atomicModifyIORef')
import qualified Data.IORef
import Data.Text (Text)

data PendingInputs = PendingInputs
    (IORef PendingState)
    (MVar ())

newPendingInputs :: IO PendingInputs
newPendingInputs = PendingInputs
    <$> Data.IORef.newIORef Model.emptyPendingState
    <*> newMVar ()

clearPendingInputs :: PendingInputs -> IO ()
clearPendingInputs (PendingInputs pending _) =
    atomicModifyIORef' pending Model.clearPendingState

enqueuePendingInput :: PendingInputs -> TurnInput -> IO (Either Text ())
enqueuePendingInput (PendingInputs pending _) input =
    atomicModifyIORef' pending (Model.enqueueInput input)

-- | MCP snapshots replace queued snapshots; other notices remain ordered.
enqueuePendingNotice
    :: PendingInputs
    -> PendingNoticeKind
    -> TurnInput
    -> IO (Either Text ())
enqueuePendingNotice (PendingInputs pending _) kind input =
    atomicModifyIORef' pending (Model.enqueueNotice kind input)

withPendingInputs :: PendingInputs -> BackendMiddleware
withPendingInputs (PendingInputs pending lifecycle) backend =
    backendWithCallbacks \state previous inputs callbacks ->
        mask \restore ->
            withMVar lifecycle \_ -> do
                batch <- atomicModifyIORef' pending Model.drain
                let queued = Model.batchInputs batch
                    requeue = atomicModifyIORef' pending (Model.requeue batch)
                    prefixed
                        | null queued = inputs
                        | otherwise =
                            queued <> inputs
                result <- restore
                    (backend.submitTurnWithCallbacks
                        state previous prefixed callbacks)
                    `onException` requeue
                case result of
                    Left _ -> requeue
                    Right _ -> atomicModifyIORef' pending (Model.commit batch)
                pure result
