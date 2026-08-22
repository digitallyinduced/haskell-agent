-- | FIFO coordination for session-wide fullscreen modal prompts.
module Agent.CLI.TUI.Modal
    ( ModalCoordinator
    , ModalId
    , ModalSubmission(..)
    , ModalTicket
    , ModalTransition(..)
    , cancelModal
    , closeModalCoordinator
    , completeModal
    , modalTicketId
    , modalTicketRequest
    , newModalCoordinator
    , runModalRequest
    , submitModal
    ) where

import Control.Concurrent.STM
    ( STM
    , TVar
    , atomically
    , newTVarIO
    , readTVar
    , writeTVar
    )
import Control.Exception.Safe (mask, onException)
import Control.Monad (forM_)
import Data.Sequence (Seq, ViewL(..), (|>))
import qualified Data.Sequence as Seq
import Data.Word (Word64)

newtype ModalId = ModalId Word64
    deriving (Eq, Ord, Show)

data ModalTicket request = ModalTicket
    { ticketId :: !ModalId
    , ticketRequest :: request
    , ticketCancel :: !(STM ())
    }

modalTicketId :: ModalTicket request -> ModalId
modalTicketId ModalTicket { ticketId } = ticketId

modalTicketRequest :: ModalTicket request -> request
modalTicketRequest ModalTicket { ticketRequest } = ticketRequest

data ModalSubmission request
    = ModalClosed
    | ModalSubmitted !ModalId !(Maybe (ModalTicket request))

data ModalTransition request
    = ModalUnchanged
    | ModalAdvanced !(Maybe (ModalTicket request))

data ModalCoordinatorState request
    = ModalOpen
        !Word64
        !(Maybe (ModalTicket request))
        !(Seq (ModalTicket request))
    | ModalCoordinatorClosed

newtype ModalCoordinator request =
    ModalCoordinator (TVar (ModalCoordinatorState request))

newModalCoordinator :: IO (ModalCoordinator request)
newModalCoordinator =
    ModalCoordinator <$> newTVarIO (ModalOpen 0 Nothing Seq.empty)

-- | Queue one request. Only the request returned in the final field should be
-- presented immediately; later requests remain queued until the active modal
-- completes or is cancelled.
submitModal
    :: ModalCoordinator request
    -> request
    -> STM ()
    -> STM (ModalSubmission request)
submitModal (ModalCoordinator stateRef) request cancelRequest = do
    state <- readTVar stateRef
    case state of
        ModalCoordinatorClosed ->
            pure ModalClosed
        ModalOpen nextId active queued -> do
            let modalId = ModalId nextId
                ticket = ModalTicket
                    { ticketId = modalId
                    , ticketRequest = request
                    , ticketCancel = cancelRequest
                    }
            case active of
                Nothing -> do
                    writeTVar stateRef $
                        ModalOpen (nextId + 1) (Just ticket) queued
                    pure (ModalSubmitted modalId (Just ticket))
                Just _ -> do
                    writeTVar stateRef $
                        ModalOpen (nextId + 1) active (queued |> ticket)
                    pure (ModalSubmitted modalId Nothing)

-- | Complete the active request if its id and response type still match.
-- The response callback and promotion of the next queued request happen in
-- one STM transaction.
completeModal
    :: ModalCoordinator request
    -> ModalId
    -> (request -> Maybe (STM ()))
    -> STM (ModalTransition request)
completeModal (ModalCoordinator stateRef) modalId respond = do
    state <- readTVar stateRef
    case state of
        ModalOpen nextId (Just active) queued
            | active.ticketId == modalId ->
                case respond active.ticketRequest of
                    Nothing ->
                        pure ModalUnchanged
                    Just response -> do
                        response
                        advanceModal stateRef nextId queued
        _ ->
            pure ModalUnchanged

-- | Cancel an active or queued request. Cancelling the active request promotes
-- the next request; cancelling a queued request leaves the visible modal alone.
cancelModal
    :: ModalCoordinator request
    -> ModalId
    -> STM (ModalTransition request)
cancelModal (ModalCoordinator stateRef) modalId = do
    state <- readTVar stateRef
    case state of
        ModalOpen nextId (Just active) queued
            | active.ticketId == modalId -> do
                active.ticketCancel
                advanceModal stateRef nextId queued
        ModalOpen nextId active queued ->
            case removeQueued modalId queued of
                Nothing ->
                    pure ModalUnchanged
                Just (removed, remaining) -> do
                    removed.ticketCancel
                    writeTVar stateRef (ModalOpen nextId active remaining)
                    pure ModalUnchanged
        ModalCoordinatorClosed ->
            pure ModalUnchanged

-- | Close the coordinator and unblock every active or queued requester.
-- Further submissions are rejected.
closeModalCoordinator :: ModalCoordinator request -> STM ()
closeModalCoordinator (ModalCoordinator stateRef) = do
    state <- readTVar stateRef
    case state of
        ModalCoordinatorClosed ->
            pure ()
        ModalOpen _ active queued -> do
            forM_ active (.ticketCancel)
            forM_ queued (.ticketCancel)
            writeTVar stateRef ModalCoordinatorClosed

-- | Submit a request and wait for its response. Async cancellation removes the
-- request from the coordinator; if it was active, the caller is notified of
-- the next request to present. That notification is STM so promotion and
-- publication retain the same order under concurrent cancellations.
runModalRequest
    :: ModalCoordinator request
    -> request
    -> STM ()
    -> STM result
    -> (ModalTicket request -> IO ())
    -> (ModalId -> Maybe (ModalTicket request) -> STM ())
    -> IO result
runModalRequest
    coordinator
    request
    cancelRequest
    awaitResponse
    present
    presentAfterCancellation =
        mask \restore -> do
            submission <- atomically $
                submitModal coordinator request cancelRequest
            case submission of
                ModalClosed -> do
                    atomically cancelRequest
                    restore (atomically awaitResponse)
                ModalSubmitted modalId ready -> do
                    let waitForResponse = do
                            forM_ ready present
                            restore (atomically awaitResponse)
                    waitForResponse `onException` do
                        atomically do
                            transition <-
                                cancelModal coordinator modalId
                            case transition of
                                ModalUnchanged ->
                                    pure ()
                                ModalAdvanced next ->
                                    presentAfterCancellation modalId next

advanceModal
    :: TVar (ModalCoordinatorState request)
    -> Word64
    -> Seq (ModalTicket request)
    -> STM (ModalTransition request)
advanceModal stateRef nextId queued =
    case Seq.viewl queued of
        EmptyL -> do
            writeTVar stateRef (ModalOpen nextId Nothing Seq.empty)
            pure (ModalAdvanced Nothing)
        next :< remaining -> do
            writeTVar stateRef (ModalOpen nextId (Just next) remaining)
            pure (ModalAdvanced (Just next))

removeQueued
    :: ModalId
    -> Seq (ModalTicket request)
    -> Maybe (ModalTicket request, Seq (ModalTicket request))
removeQueued target = go Seq.empty
  where
    go retained remaining =
        case Seq.viewl remaining of
            EmptyL ->
                Nothing
            ticket :< rest
                | ticket.ticketId == target ->
                    Just (ticket, retained <> rest)
                | otherwise ->
                    go (retained |> ticket) rest
