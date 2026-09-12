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
    ( Async, AsyncCancelled(..), asyncThreadId, asyncWithUnmask, poll, wait, waitCatch )
import Control.Concurrent (throwTo)
import Control.Concurrent.MVar
    ( MVar, modifyMVar, modifyMVar_, newEmptyMVar, newMVar
    , putMVar, readMVar, takeMVar
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
    , cancelSignal :: !(MVar (Maybe (Async ())))
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
                        cancelSignal <- newMVar Nothing
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
                            pure outcome
                        let worker = OwnedWorker { workerAsync, cancelSignal }
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
prepareSessionWait owner sessionId =
    modifyMVar owner.state \previous -> do
        current <- settleState previous
        pure (current, capture current)
  where
    capture current = case Map.lookup sessionId current.entries of
        Nothing -> Nothing
        Just (Finished outcome) -> Just (SessionFinished outcome, pure outcome)
        Just (Running worker) ->
            Just (SessionRunning, either exceptionOutcome id <$> waitCatch worker.workerAsync)

-- Retain the worker handle until the Async has actually terminated, not merely
-- until its action and notification have returned. Close therefore joins every
-- live worker, including one leaving its final publication boundary.
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
            Just outcome -> do
                sender <- readMVar worker.cancelSignal
                senderFinished <- case sender of
                    Nothing -> pure True
                    Just signal -> isJust <$> poll signal
                pure $ if senderFinished
                    then Finished (either exceptionOutcome id outcome)
                    else entry

-- | Cancel and join only the generation captured by this call.
cancelSessionTurn :: SessionOwner -> Text -> IO Bool
cancelSessionTurn owner sessionId = do
    current <- readMVar owner.state
    case Map.lookup sessionId current.entries of
        Just (Running worker) -> cancelOwnedWorker worker >> pure True
        _ -> pure False

-- Signal exactly once, independently of the cancelling caller's lifetime.
-- throwTo itself is interruptible before delivery, so a Boolean "sent" flag in
-- the caller is insufficient: interrupted delivery could lose cancellation.
-- The single sender is tracked alongside its target and joined by every
-- cancellation/close caller. No caller interruption cancels this owned sender.
cancelOwnedWorker :: OwnedWorker -> IO ()
cancelOwnedWorker worker = mask \restore -> do
    sender <- requestWorkerCancellation worker
    restore do
        mapM_ wait sender
        void (waitCatch worker.workerAsync)

-- Request delivery without waiting for it: a worker may defer cancellation
-- until another worker starts cleanup. The sender remains owned by the worker.
requestWorkerCancellation :: OwnedWorker -> IO (Maybe (Async ()))
requestWorkerCancellation worker =
    modifyMVar worker.cancelSignal \case
        Just sender -> pure (Just sender, Just sender)
        Nothing -> do
            poll worker.workerAsync >>= \case
                Just _ -> pure (Nothing, Nothing)
                Nothing -> do
                    sender <- asyncWithUnmask \unmask ->
                        unmask (throwTo (asyncThreadId worker.workerAsync) AsyncCancelled)
                    pure (Just sender, Just sender)

-- | All concurrent closers retain the same workers until they are joined.
-- If a closer is interrupted, a subsequent close can still finish cleanup.
closeSessionOwner :: SessionOwner -> IO ()
closeSessionOwner owner = mask \restore -> do
    workers <- modifyMVar owner.state \current ->
        pure
            ( current { closed = True }
            , [worker | Running worker <- Map.elems current.entries]
            )
    -- Start every cancellation before joining any worker. Sequential
    -- cancel-and-join can deadlock when cleanup depends on a sibling stopping.
    senders <- traverse requestWorkerCancellation workers
    restore do
        mapM_ (mapM_ wait) senders
        mapM_ (\worker -> void (waitCatch worker.workerAsync)) workers
    modifyMVar_ owner.state \current ->
        pure current { entries = Map.empty }
