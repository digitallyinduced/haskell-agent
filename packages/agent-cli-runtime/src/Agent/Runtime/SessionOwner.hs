-- | Frontend-independent ownership of in-process session turns.
--
-- Admission is nonblocking: one worker per session and a bounded number of
-- workers per owner. The action includes persistence and cleanup; its slot is
-- not reusable until that action and the completion notification have ended.
module Agent.Runtime.SessionOwner
    ( SessionOwner
    , AdmissionFailure(..)
    , SessionOutcome(..)
    , SessionStatus(..)
    , newSessionOwner
    , withSessionOwner
    , submitSessionTurn
    , sessionOwnerSnapshot
    , prepareSessionWait
    , cancelSessionTurn
    , closeSessionOwner
    ) where

import Control.Concurrent.Async
    ( Async, AsyncCancelled, asyncWithUnmask, cancel, poll, waitCatch )
import Control.Concurrent.MVar
    ( MVar, modifyMVar, modifyMVar_, newEmptyMVar, newMVar
    , putMVar, readMVar, takeMVar, withMVar
    )
import Control.Exception.Safe
    ( SomeException, bracket, displayException, fromException, mask, tryAny )
-- Cancellation is an outcome here, not an exception to discard. Catch all
-- exceptions only at the owned worker's terminal publication boundary.
import qualified Control.Exception as Exception
import Control.Monad (void)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Numeric.Natural (Natural)

data AdmissionFailure = OwnerClosed | SessionBusy | OwnerAtCapacity
    deriving (Eq, Show)

data SessionOutcome = SessionCompleted | SessionFailed !Text | SessionCancelled
    deriving (Eq, Show)

data SessionStatus = SessionRunning | SessionFinished !SessionOutcome
    deriving (Eq, Show)

data Entry
    = Running !OwnedWorker
    | Finished !SessionOutcome

data OwnedWorker = OwnedWorker
    { workerAsync :: !(Async SessionOutcome)
    , cancelGate :: !(MVar ())
    }

data OwnerState = OwnerState
    { closed :: !Bool
    , entries :: !(Map Text Entry)
    }

data SessionOwner = SessionOwner
    { capacity :: !Natural
    , state :: !(MVar OwnerState)
    }

newSessionOwner :: Natural -> IO SessionOwner
newSessionOwner capacity =
    SessionOwner capacity <$> newMVar (OwnerState False Map.empty)

withSessionOwner :: Natural -> (SessionOwner -> IO a) -> IO a
withSessionOwner capacity = bracket (newSessionOwner capacity) closeSessionOwner

-- | The notification is part of terminal publication. It must be nonblocking
-- and must not call back into this owner. Notification failures cannot replace
-- the action's outcome. Closing suppresses notifications, as in the legacy
-- background-session adapter.
submitSessionTurn
    :: SessionOwner
    -> Text
    -> (SessionOutcome -> IO ())
    -> IO (Either Text ())
    -> IO (Either AdmissionFailure ())
submitSessionTurn owner sessionId notify action = mask \_ ->
    modifyMVar owner.state \previous -> do
        current <- settleState previous
        if current.closed
            then pure (current, Left OwnerClosed)
            else case Map.lookup sessionId current.entries of
                Just (Running _) -> pure (current, Left SessionBusy)
                _
                    | fromIntegral (length [() | Running _ <- Map.elems current.entries])
                        >= owner.capacity ->
                        pure (current, Left OwnerAtCapacity)
                    | otherwise -> do
                        gate <- newEmptyMVar
                        -- This worker outlives submission, but is registered
                        -- before execution and retained until close joins it.
                        cancelGate <- newMVar ()
                        workerAsync <- asyncWithUnmask \unmask -> do
                            takeMVar gate
                            result <- Exception.try (unmask action)
                            let outcome = either exceptionOutcome
                                    (either SessionFailed (const SessionCompleted))
                                    result
                            modifyMVar_ owner.state \latest ->
                                if latest.closed
                                    then pure latest
                                    else do
                                        void (tryAny (notify outcome))
                                        pure latest
                                            { entries = Map.insert sessionId
                                                (Finished outcome) latest.entries
                                            }
                            pure outcome
                        let worker = OwnedWorker { workerAsync, cancelGate }
                        putMVar gate ()
                        pure
                            ( current { entries = Map.insert sessionId
                                (Running worker) current.entries }
                            , Right ()
                            )

exceptionOutcome :: SomeException -> SessionOutcome
exceptionOutcome err
    | isJust (fromException err :: Maybe AsyncCancelled) = SessionCancelled
    | otherwise = SessionFailed (Text.pack (displayException err))

sessionOwnerSnapshot :: SessionOwner -> IO (Bool, Map Text SessionStatus)
sessionOwnerSnapshot owner =
    modifyMVar owner.state \previous -> do
        current <- settleState previous
        pure (current, (current.closed, Map.map status current.entries))
  where
    status (Running _) = SessionRunning
    status (Finished outcome) = SessionFinished outcome

-- | Capture this generation, never accidentally wait for a later retry.
prepareSessionWait
    :: SessionOwner -> Text -> IO (Maybe (SessionStatus, IO SessionOutcome))
prepareSessionWait owner sessionId = do
    current <- readMVar owner.state
    pure $ case Map.lookup sessionId current.entries of
        Nothing -> Nothing
        Just (Finished outcome) -> Just (SessionFinished outcome, pure outcome)
        Just (Running worker) ->
            Just (SessionRunning, either exceptionOutcome id <$> waitCatch worker.workerAsync)

-- A second asynchronous exception can interrupt terminal publication while
-- acquiring the lock. Recover that outcome from the owned Async rather than
-- retaining a permanently busy slot.
settleState :: OwnerState -> IO OwnerState
settleState current
    | current.closed = pure current
    | otherwise = do
        settled <- traverse settle current.entries
        pure current { entries = settled }
  where
    settle entry@(Finished _) = pure entry
    settle entry@(Running worker) =
        poll worker.workerAsync >>= \case
            Nothing -> pure entry
            Just outcome -> pure (Finished (either exceptionOutcome id outcome))

-- | Cancel and join only the generation captured by this call.
cancelSessionTurn :: SessionOwner -> Text -> IO Bool
cancelSessionTurn owner sessionId = do
    current <- readMVar owner.state
    case Map.lookup sessionId current.entries of
        Just (Running worker) -> cancelOwnedWorker worker >> pure True
        _ -> pure False

-- Concurrent cancellation and close must not inject a second cancellation
-- into the first cancellation's interruptible cleanup.
cancelOwnedWorker :: OwnedWorker -> IO ()
cancelOwnedWorker worker =
    withMVar worker.cancelGate \_ -> cancel worker.workerAsync

-- | All concurrent closers retain the same workers until they are joined.
-- If a closer is interrupted, a subsequent close can still finish cleanup.
closeSessionOwner :: SessionOwner -> IO ()
closeSessionOwner owner = mask \restore -> do
    workers <- modifyMVar owner.state \current ->
        pure
            ( current { closed = True }
            , [worker | Running worker <- Map.elems current.entries]
            )
    restore (mapM_ cancelOwnedWorker workers)
    modifyMVar_ owner.state \current ->
        pure current { entries = Map.empty }
