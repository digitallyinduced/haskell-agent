-- | Bounded, process-local supervision for active HTTP turns.
--
-- Durable conversation history remains in PostgreSQL. This module owns only
-- live execution state, human-input waits, and the bounded SSE replay window.
module Agent.Server.Supervisor
    ( Supervisor
    , SupervisorConfig(..)
    , SubmitError(..)
    , CheckedSubmitError(..)
    , SessionMutationError(..)
    , EventSubscriptionError(..)
    , TurnControl(..)
    , TurnRunner
    , TurnBoundaryGuard
    , HumanRequestPersistenceResolution(..)
    , HumanRequestResolutionError(..)
    , HumanRequestCleanup(..)
    , TurnPersistence(..)
    , inMemoryTurnPersistence
    , newSupervisor
    , newSupervisorWithBoundaryGuard
    , newSupervisorWithBoundaryGuardAndPersistence
    , closeSupervisor
    , submitTurn
    , submitTurnChecked
    , submitReservedTurnChecked
    , trySubmitReservedTurnChecked
    , cancelTurn
    , lookupTurn
    , listTurns
    , sessionHasActiveTurn
    , withSessionCleanup
    , withSessionMutation
    , listHumanRequests
    , resolveHumanRequest
    , lookupTurnAgents
    , publishEvent
    , subscribeEvents
    , deleteCancellationTaskIfOwned
    ) where

import Agent.Loop (ImageAttachment(..))
import Agent.Server.Identifier (newUUIDv7Text)
import Agent.Server.Types
import Control.Concurrent (ThreadId, myThreadId, threadDelay)
import Control.Concurrent.Async
    ( Async
    , async
    , asyncThreadId
    , cancel
    , forConcurrently
    , mapConcurrently_
    , waitCatch
    )
import Control.Concurrent.MVar
    ( MVar
    , newEmptyMVar
    , putMVar
    , readMVar
    )
import Control.Concurrent.STM
    ( STM
    , TBQueue
    , TMVar
    , TVar
    , atomically
    , isFullTBQueue
    , modifyTVar'
    , newEmptyTMVar
    , newTBQueue
    , newTVarIO
    , putTMVar
    , readTBQueue
    , readTVar
    , retry
    , takeTMVar
    , tryPutTMVar
    , writeTBQueue
    , writeTVar
    )
import Control.Exception.Safe
    ( SomeException
    , finally
    , mask
    , onException
    , tryAny
    )
import Control.Monad (forM_, void, when)
import Data.Aeson (Value, encode, object, toJSON, (.=))
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Foldable (toList)
import Data.Int (Int64)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Ord (Down(..))
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime, getCurrentTime)
import System.Timeout (timeout)

data SupervisorConfig = SupervisorConfig
    { supervisorMaxConcurrentTurns :: !Int
    , supervisorMaxConcurrentTurnsPerTenant :: !Int
    , supervisorMaxQueuedTurns :: !Int
    , supervisorMaxQueuedTurnsPerTenant :: !Int
    , supervisorMaxEventSubscribers :: !Int
    , supervisorMaxEventSubscribersPerTenant :: !Int
    , supervisorEventReplayLimit :: !Int
    }
    deriving (Eq, Show)

data EventSubscriptionError
    = EventSubscriberLimitReached
    | EventSubscriberTenantLimitReached
    deriving (Eq, Show)

data CheckedSubmitError validationError
    = SubmitValidationFailed !validationError
    | SubmitValidationRejected !SubmitError
    deriving (Eq, Show)

data SessionMutationError
    = SessionMutationBusy
    | SessionMutationSupervisorClosed
    deriving (Eq, Show)

data SubmitError
    = SubmitQueueFull
    | SubmitTenantQueueFull
    | SubmitSessionBusy
    | SubmitSupervisorClosed
    deriving (Eq, Show)

data TurnControl = TurnControl
    { turnControlEmit :: !(Text -> Value -> IO ())
    , turnControlRequestInput
        :: !(HumanRequestSpec -> IO (Either Text HumanResponse))
    , turnControlRegisterCancel :: !(IO () -> IO ())
    , turnControlSetAgents :: !(IO Value -> IO ())
    }

type TurnRunner =
    TurnControl -> TurnSpec -> IO (Either Text TurnExecutionOutput)

-- | Run an action only while the exact admitted gateway boundary is leased.
--
-- Production uses the credential turn lease here so the runtime invocation
-- and its terminal state/event commit share one uninterrupted boundary.
type TurnBoundaryGuard =
    forall value.
    AccessBoundary ->
    IO value ->
    IO (Either Text value)

data HumanRequestPersistenceResolution
    = HumanRequestResolvedDurably !HumanRequest
    | HumanRequestNotFoundDurably
    | HumanRequestAlreadyResolvedDurably
    | HumanRequestInvalidDecisionDurably
    | HumanRequestLocalOnly
    deriving (Eq, Show)

data HumanRequestResolutionError
    = HumanRequestResolutionNotFound
    | HumanRequestResolutionConflict !Text
    | HumanRequestResolutionStoreUnavailable !Text
    deriving (Eq, Show)

data HumanRequestCleanup
    = HumanRequestAbandoned
    | HumanResponseConsumed
    deriving (Eq, Show)

data TurnPersistence = TurnPersistence
    { turnPersistenceStarted ::
        !(TurnRecord -> UTCTime -> IO (Either Text ()))
    , turnPersistenceTerminal ::
        !( TurnRecord ->
           UTCTime ->
           TurnTerminalOutcome ->
           IO (Either Text TurnRecord)
         )
    , turnPersistenceShouldCancel ::
        !(TurnRecord -> IO (Either Text Bool))
    , turnPersistenceCreateHumanRequest ::
        !(TurnRecord -> HumanRequest -> IO (Either Text ()))
    , turnPersistenceListHumanRequests ::
        !(AccessBoundary -> Maybe TurnId -> IO (Either Text [HumanRequest]))
    , turnPersistenceResolveHumanRequest ::
        !( AccessBoundary ->
           RequestId ->
           HumanResponse ->
           IO (Either Text HumanRequestPersistenceResolution)
         )
    , turnPersistenceLoadHumanResponse ::
        !( TurnRecord ->
           RequestId ->
           IO (Either Text (Maybe HumanResponse))
         )
    , turnPersistenceDeleteHumanRequest ::
        !( TurnRecord ->
           RequestId ->
           HumanRequestCleanup ->
           IO (Either Text ())
         )
    }

inMemoryTurnPersistence :: TurnPersistence
inMemoryTurnPersistence =
    TurnPersistence
        { turnPersistenceStarted = \_ _ -> pure (Right ())
        , turnPersistenceTerminal = \record finishedAt outcome ->
            pure (Right (terminalRecord finishedAt outcome record))
        , turnPersistenceShouldCancel = \_ ->
            pure (Right False)
        , turnPersistenceCreateHumanRequest = \_ _ ->
            pure (Right ())
        , turnPersistenceListHumanRequests = \_ _ ->
            pure (Right [])
        , turnPersistenceResolveHumanRequest = \_ _ _ ->
            pure (Right HumanRequestLocalOnly)
        , turnPersistenceLoadHumanResponse = \_ _ ->
            pure (Right Nothing)
        , turnPersistenceDeleteHumanRequest = \_ _ _ ->
            pure (Right ())
        }

data TurnSlot = TurnSlot
    { turnSlotSpec :: !TurnSpec
    , turnSlotRecord :: !TurnRecord
    , turnSlotCancelling :: !Bool
    , turnSlotCancel :: !(Maybe (IO ()))
    , turnSlotAgents :: !(IO Value)
    }

data PendingInput = PendingInput
    { pendingInputView :: !HumanRequest
    , pendingInputReply :: !(TMVar (Either Text HumanResponse))
    }

data EventBuffer = EventBuffer
    { eventBufferReplay :: !(Seq ServerEvent)
    , eventBufferNextId :: !Integer
    }

data EventSubscriber = EventSubscriber
    { eventSubscriberBoundary :: !AccessBoundary
    , eventSubscriberQueue :: !(TBQueue ServerEvent)
    }

data SupervisorState = SupervisorState
    { stateQueue :: !(Seq TurnId)
    , stateTurns :: !(Map TurnId TurnSlot)
    , stateActiveSessions :: !(Set (AccessBoundary, Text))
    , stateWorkers :: !(Map TurnId (Async ()))
    , stateCancellationTasks :: !(Map TurnId (Async ()))
    , statePendingInputs :: !(Map RequestId PendingInput)
    , stateEvents :: !(Map AccessBoundary EventBuffer)
    , stateNextSubscriberId :: !Integer
    , stateSubscribers :: !(Map Integer EventSubscriber)
    , stateLastScheduledTenant :: !(Maybe TenantId)
    , stateClosed :: !Bool
    }

data Supervisor = Supervisor
    { supervisorConfig :: !SupervisorConfig
    , supervisorRunner :: !TurnRunner
    , supervisorBoundaryGuard :: !TurnBoundaryGuard
    , supervisorPersistence :: !TurnPersistence
    , supervisorState :: !(TVar SupervisorState)
    , supervisorDispatcher :: !(MVar (Async ()))
    , supervisorCancellationMonitor :: !(MVar (Async ()))
    }

newSupervisor :: SupervisorConfig -> TurnRunner -> IO Supervisor
newSupervisor config =
    newSupervisorWithBoundaryGuard config
        (\_ action -> Right <$> action)

newSupervisorWithBoundaryGuard ::
    SupervisorConfig ->
    TurnBoundaryGuard ->
    TurnRunner ->
    IO Supervisor
newSupervisorWithBoundaryGuard config boundaryGuard runner =
    newSupervisorWithBoundaryGuardAndPersistence
        config
        boundaryGuard
        inMemoryTurnPersistence
        runner

newSupervisorWithBoundaryGuardAndPersistence ::
    SupervisorConfig ->
    TurnBoundaryGuard ->
    TurnPersistence ->
    TurnRunner ->
    IO Supervisor
newSupervisorWithBoundaryGuardAndPersistence
    config
    boundaryGuard
    persistence
    runner
        | config.supervisorMaxConcurrentTurns < 1 =
            fail "supervisorMaxConcurrentTurns must be positive"
        | config.supervisorMaxConcurrentTurnsPerTenant < 1 =
            fail "supervisorMaxConcurrentTurnsPerTenant must be positive"
        | config.supervisorMaxConcurrentTurnsPerTenant
            > config.supervisorMaxConcurrentTurns =
            fail
                "supervisorMaxConcurrentTurnsPerTenant exceeds global concurrency"
        | config.supervisorMaxQueuedTurns < 1 =
            fail "supervisorMaxQueuedTurns must be positive"
        | config.supervisorMaxQueuedTurnsPerTenant < 1 =
            fail "supervisorMaxQueuedTurnsPerTenant must be positive"
        | config.supervisorMaxQueuedTurnsPerTenant
            > config.supervisorMaxQueuedTurns =
            fail "supervisorMaxQueuedTurnsPerTenant exceeds global queue capacity"
        | config.supervisorMaxEventSubscribers < 1 =
            fail "supervisorMaxEventSubscribers must be positive"
        | config.supervisorMaxEventSubscribersPerTenant < 1 =
            fail "supervisorMaxEventSubscribersPerTenant must be positive"
        | config.supervisorMaxEventSubscribersPerTenant
            > config.supervisorMaxEventSubscribers =
            fail
                "supervisorMaxEventSubscribersPerTenant exceeds global capacity"
        | config.supervisorEventReplayLimit < 1 =
            fail "supervisorEventReplayLimit must be positive"
        | otherwise = mask \restore -> do
            state <-
                newTVarIO
                    SupervisorState
                        { stateQueue = Seq.empty
                        , stateTurns = Map.empty
                        , stateActiveSessions = Set.empty
                        , stateWorkers = Map.empty
                        , stateCancellationTasks = Map.empty
                        , statePendingInputs = Map.empty
                        , stateEvents = Map.empty
                        , stateNextSubscriberId = 1
                        , stateSubscribers = Map.empty
                        , stateLastScheduledTenant = Nothing
                        , stateClosed = False
                        }
            dispatcherVar <- newEmptyMVar
            cancellationMonitorVar <- newEmptyMVar
            let supervisor =
                    Supervisor
                        { supervisorConfig = config
                        , supervisorRunner = runner
                        , supervisorBoundaryGuard = boundaryGuard
                        , supervisorPersistence = persistence
                        , supervisorState = state
                        , supervisorDispatcher = dispatcherVar
                        , supervisorCancellationMonitor =
                            cancellationMonitorVar
                        }
            dispatcher <- async (restore (dispatcherLoop supervisor))
            cancellationMonitor <-
                async (restore (cancellationMonitorLoop supervisor))
                    `onException` do
                        cancel dispatcher
                        void (waitCatch dispatcher)
            putMVar dispatcherVar dispatcher
            putMVar cancellationMonitorVar cancellationMonitor
            pure supervisor

closeSupervisor :: Supervisor -> IO ()
closeSupervisor supervisor = mask \restore -> do
    now <- getCurrentTime
    (workers, cancellationTasks, cancellations, replies, activeRecords) <-
        atomically do
            state <- readTVar supervisor.supervisorState
            if state.stateClosed
                then pure ([], [], [], [], [])
                else do
                    let activeSlots =
                            [ slot
                            | slot <- Map.elems state.stateTurns
                            , isActiveStatus slot.turnSlotRecord.turnRecordStatus
                            ]
                        cancellationActions =
                            mapMaybe (.turnSlotCancel) activeSlots
                        pendingReplies =
                            map (.pendingInputReply)
                                (Map.elems state.statePendingInputs)
                        cancelledTurns =
                            foldr
                                (cancelSlot now)
                                state.stateTurns
                                activeSlots
                        state' =
                            state
                                { stateClosed = True
                                , stateQueue = Seq.empty
                                , stateTurns = cancelledTurns
                                , stateCancellationTasks = Map.empty
                                , statePendingInputs = Map.empty
                                }
                    writeTVar supervisor.supervisorState state'
                    pure
                        ( Map.elems state.stateWorkers
                        , Map.elems state.stateCancellationTasks
                        , cancellationActions
                        , pendingReplies
                        , map (.turnSlotRecord) activeSlots
                        )
    forM_ replies \reply ->
        atomically (void (tryPutTMVar reply (Left "server is shutting down")))
    let interruptInBand =
            forM_ cancellations \action ->
                void $
                    timeout cancelHookTimeoutMicros
                        (void (tryAny (restore action)))
        stopWorkers =
            mapConcurrently_ cancel workers
        stopBackground = do
            dispatcher <- readMVar supervisor.supervisorDispatcher
            cancel dispatcher
            void (waitCatch dispatcher)
            cancellationMonitor <-
                readMVar supervisor.supervisorCancellationMonitor
            cancel cancellationMonitor
            void (waitCatch cancellationMonitor)
            forM_ cancellationTasks cancel
            forM_ cancellationTasks (void . waitCatch)
    -- A stuck provider interrupt must not skip structured worker teardown.
    (interruptInBand `finally` stopWorkers)
        `finally` stopBackground
    persisted <-
        timeout
            terminalPersistenceShutdownTimeoutMicros
            ( forConcurrently activeRecords \record ->
                persistTerminalEventually
                    supervisor
                    record
                    now
                    TurnWasCancelled
            )
    forM_ persisted $
        mapM_ \canonical ->
            atomically $
                publishTerminalRecord supervisor canonical

submitTurn ::
    Supervisor ->
    TurnSpec ->
    IO (Either SubmitError TurnRecord)
submitTurn supervisor spec = do
    now <- getCurrentTime
    turnId <- TurnId <$> newUUIDv7Text
    enqueueTurnRecord
        supervisor
        False
        spec
        TurnRecord
            { turnRecordId = turnId
            , turnRecordSessionId = spec.turnSpecSessionId
            , turnRecordClientRequestId = spec.turnSpecClientRequestId
            , turnRecordBoundary = spec.turnSpecBoundary
            , turnRecordStatus = TurnQueued
            , turnRecordCreatedAt = now
            , turnRecordStartedAt = Nothing
            , turnRecordFinishedAt = Nothing
            , turnRecordError = Nothing
            }

-- | Validate a session while holding the same per-boundary reservation that
-- is atomically converted into a queued turn. This prevents delete/patch/fork
-- from interleaving between durable validation and turn admission.
submitTurnChecked
    :: Supervisor
    -> TurnSpec
    -> IO (Either validationError ())
    -> IO (Either (CheckedSubmitError validationError) TurnRecord)
submitTurnChecked supervisor spec validate = do
    reserved <-
        withSessionMutation
            supervisor
            spec.turnSpecBoundary
            spec.turnSpecSessionId
            (validate >>= \case
                Left err ->
                    pure (Left (SubmitValidationFailed err))
                Right () ->
                    fmap
                        (either
                            (Left . SubmitValidationRejected)
                            Right)
                        (enqueueTurn supervisor True spec))
    pure case reserved of
        Left SessionMutationBusy ->
            Left (SubmitValidationRejected SubmitSessionBusy)
        Left SessionMutationSupervisorClosed ->
            Left (SubmitValidationRejected SubmitSupervisorClosed)
        Right result -> result

-- | Queue a newly reserved durable turn, waiting for an in-flight local
-- mutation to release the session. The durable reservation makes that
-- mutation unwind before the turn is admitted.
submitReservedTurnChecked ::
    Supervisor ->
    TurnSpec ->
    TurnRecord ->
    IO (Either validationError ()) ->
    IO (Either (CheckedSubmitError validationError) TurnRecord)
submitReservedTurnChecked =
    submitReservedTurnCheckedWith True

-- | Try to admit an existing durable reservation without waiting for another
-- process-local admission of the same session. The caller can return the
-- durable reservation when an identical retry loses that race.
trySubmitReservedTurnChecked ::
    Supervisor ->
    TurnSpec ->
    TurnRecord ->
    IO (Either validationError ()) ->
    IO (Either (CheckedSubmitError validationError) TurnRecord)
trySubmitReservedTurnChecked =
    submitReservedTurnCheckedWith False

submitReservedTurnCheckedWith
    :: Bool
    -> Supervisor
    -> TurnSpec
    -> TurnRecord
    -> IO (Either validationError ())
    -> IO (Either (CheckedSubmitError validationError) TurnRecord)
submitReservedTurnCheckedWith
        waitForReservation supervisor spec record validate = do
    reserved <-
        withSessionReservation
            waitForReservation
            supervisor
            spec.turnSpecBoundary
            spec.turnSpecSessionId
            ( validate >>= \case
                Left err ->
                    pure (Left (SubmitValidationFailed err))
                Right () ->
                    fmap
                        ( either
                            (Left . SubmitValidationRejected)
                            Right
                        )
                        (enqueueTurnRecord supervisor True spec record)
            )
    pure case reserved of
        Left SessionMutationBusy ->
            Left (SubmitValidationRejected SubmitSessionBusy)
        Left SessionMutationSupervisorClosed ->
            Left (SubmitValidationRejected SubmitSupervisorClosed)
        Right result -> result

enqueueTurn ::
    Supervisor ->
    Bool ->
    TurnSpec ->
    IO (Either SubmitError TurnRecord)
enqueueTurn supervisor reservationHeld spec = do
    now <- getCurrentTime
    turnId <- TurnId <$> newUUIDv7Text
    enqueueTurnRecord
        supervisor
        reservationHeld
        spec
        TurnRecord
            { turnRecordId = turnId
            , turnRecordSessionId = spec.turnSpecSessionId
            , turnRecordClientRequestId = spec.turnSpecClientRequestId
            , turnRecordBoundary = spec.turnSpecBoundary
            , turnRecordStatus = TurnQueued
            , turnRecordCreatedAt = now
            , turnRecordStartedAt = Nothing
            , turnRecordFinishedAt = Nothing
            , turnRecordError = Nothing
            }

enqueueTurnRecord ::
    Supervisor ->
    Bool ->
    TurnSpec ->
    TurnRecord ->
    IO (Either SubmitError TurnRecord)
enqueueTurnRecord supervisor reservationHeld spec record
    | record.turnRecordSessionId /= spec.turnSpecSessionId =
        fail "reserved turn session does not match its specification"
    | record.turnRecordClientRequestId /= spec.turnSpecClientRequestId =
        fail "reserved turn client request does not match its specification"
    | record.turnRecordBoundary /= spec.turnSpecBoundary =
        fail "reserved turn boundary does not match its specification"
    | record.turnRecordStatus /= TurnQueued =
        fail "only a queued reserved turn can be admitted"
    | otherwise =
        atomically do
            state <- readTVar supervisor.supervisorState
            let now = record.turnRecordCreatedAt
                key = (spec.turnSpecBoundary, spec.turnSpecSessionId)
                reserved = Set.member key state.stateActiveSessions
            if state.stateClosed
                then pure (Left SubmitSupervisorClosed)
                else
                    if reservationHeld && not reserved
                        then pure (Left SubmitSessionBusy)
                        else
                            if (not reservationHeld && reserved)
                                || sessionBusy
                                    spec.turnSpecBoundary
                                    spec.turnSpecSessionId
                                    state.stateTurns
                                then pure (Left SubmitSessionBusy)
                                else
                                    if Seq.length state.stateQueue
                                        >= supervisor.supervisorConfig.supervisorMaxQueuedTurns
                                        || queuedAttachmentBytes state.stateTurns
                                            + turnAttachmentBytes spec
                                            > maximumQueuedAttachmentBytes
                                        then pure (Left SubmitQueueFull)
                                        else
                                            if queuedForTenant
                                                spec.turnSpecBoundary.accessTenantId
                                                state.stateTurns
                                                >= supervisor.supervisorConfig.supervisorMaxQueuedTurnsPerTenant
                                                || queuedAttachmentBytesForTenant
                                                    spec.turnSpecBoundary.accessTenantId
                                                    state.stateTurns
                                                    + turnAttachmentBytes spec
                                                    > maximumQueuedAttachmentBytesPerTenant
                                                then pure (Left SubmitTenantQueueFull)
                                                else do
                                                    let turnId = record.turnRecordId
                                                        slot =
                                                            TurnSlot
                                                                { turnSlotSpec = spec
                                                                , turnSlotRecord = record
                                                                , turnSlotCancelling = False
                                                                , turnSlotCancel = Nothing
                                                                , turnSlotAgents = pure toJSONEmptyArray
                                                                }
                                                        withTurn =
                                                            state
                                                                { stateQueue = state.stateQueue |> turnId
                                                                , stateTurns =
                                                                    Map.insert
                                                                        turnId
                                                                        slot
                                                                        (pruneTerminalTurns state.stateTurns)
                                                                }
                                                        (event, state') =
                                                            appendEventToState
                                                                supervisor.supervisorConfig
                                                                now
                                                                spec.turnSpecBoundary
                                                                "turn.queued"
                                                                (Just turnId)
                                                                (Just spec.turnSpecSessionId)
                                                                (object [])
                                                                withTurn
                                                    writeTVar supervisor.supervisorState state'
                                                    publishToSubscribers state' event
                                                    pure (Right record)

cancelTurn ::
    Supervisor ->
    AccessBoundary ->
    TurnId ->
    IO (Either Text TurnRecord)
cancelTurn supervisor boundary turnId = mask \restore -> do
    now <- getCurrentTime
    outcome <- atomically do
        state <- readTVar supervisor.supervisorState
        case Map.lookup turnId state.stateTurns of
            Nothing -> pure (Left "turn not found")
            Just slot
                | slot.turnSlotRecord.turnRecordBoundary /= boundary ->
                    pure (Left "turn not found")
                | slot.turnSlotCancelling ->
                    pure (Left "turn cancellation is in progress")
                | not
                    ( isActiveStatus
                        slot.turnSlotRecord.turnRecordStatus
                    ) ->
                    pure
                        ( Right
                            ( slot.turnSlotRecord
                            , Nothing
                            , Nothing
                            , False
                            )
                        )
                | otherwise -> do
                    let slot' =
                            slot { turnSlotCancelling = True }
                        worker =
                            Map.lookup turnId state.stateWorkers
                        (inputs, retainedInputs) =
                            Map.partition
                                ((== turnId)
                                    . (.pendingInputView.humanRequestTurnId))
                                state.statePendingInputs
                        state' =
                            state
                                { stateQueue =
                                    Seq.filter (/= turnId) state.stateQueue
                                , stateTurns =
                                    Map.insert turnId slot' state.stateTurns
                                , statePendingInputs = retainedInputs
                                }
                    forM_ (Map.elems inputs) \input ->
                        void
                            (tryPutTMVar
                                input.pendingInputReply
                                (Left "turn cancelled"))
                    writeTVar supervisor.supervisorState state'
                    pure
                        (Right
                            ( slot.turnSlotRecord
                            , slot.turnSlotCancel
                            , worker
                            , True
                            )
                        )
    case outcome of
        Left err -> pure (Left err)
        Right (record, cancellation, worker, transitioned) -> do
            let stopWorker =
                    forM_ worker \running -> do
                        cancel running
                        void (waitCatch running)
                interruptInBand =
                    forM_ cancellation \action ->
                        void $
                            timeout cancelHookTimeoutMicros
                                (void (tryAny (restore action)))
                completeCancellation = do
                    -- Preserve in-band cancellation when it is responsive, but
                    -- always deliver and join the structured cancellation.
                    interruptInBand `finally` stopWorker
                    if not transitioned
                        then pure (Right record)
                        else do
                            canonical <-
                                persistTerminalEventually
                                    supervisor
                                    record
                                    now
                                    TurnWasCancelled
                            atomically $
                                publishTerminalRecord supervisor canonical
                            pure (Right canonical)
            completeCancellation
                `onException`
                    when transitioned
                        (atomically (releaseCancellationClaim supervisor record))

lookupTurn
    :: Supervisor
    -> AccessBoundary
    -> TurnId
    -> IO (Maybe TurnRecord)
lookupTurn supervisor boundary turnId =
    atomically do
        state <- readTVar supervisor.supervisorState
        pure do
            slot <- Map.lookup turnId state.stateTurns
            if slot.turnSlotRecord.turnRecordBoundary == boundary
                then Just slot.turnSlotRecord
                else Nothing

listTurns
    :: Supervisor
    -> AccessBoundary
    -> Maybe Text
    -> IO [TurnRecord]
listTurns supervisor boundary sessionId =
    atomically do
        state <- readTVar supervisor.supervisorState
        pure $
            sortOn (Down . (.turnRecordCreatedAt))
                [ record
                | slot <- Map.elems state.stateTurns
                , let record = slot.turnSlotRecord
                , record.turnRecordBoundary == boundary
                , maybe True (== record.turnRecordSessionId) sessionId
                ]

sessionHasActiveTurn
    :: Supervisor
    -> AccessBoundary
    -> Text
    -> IO Bool
sessionHasActiveTurn supervisor boundary sessionId =
    atomically do
        state <- readTVar supervisor.supervisorState
        pure $
            Set.member
                (boundary, sessionId)
                state.stateActiveSessions
                || any
                (\slot ->
                    slot.turnSlotRecord.turnRecordBoundary == boundary
                        && slot.turnSlotRecord.turnRecordSessionId == sessionId
                        && isActiveStatus
                            slot.turnSlotRecord.turnRecordStatus)
                (Map.elems state.stateTurns)

-- | Reserve a session for a mutation, atomically excluding turn admission.
--
-- The key includes the exact gateway boundary so equal session identifiers in
-- two credential identities do not interfere with one another.
withSessionMutation
    :: Supervisor
    -> AccessBoundary
    -> Text
    -> IO value
    -> IO (Either SessionMutationError value)
withSessionMutation =
    withSessionReservation False

-- | Wait behind a transient reservation before cleaning up durable state.
--
-- An admitted turn still rejects cleanup so its live execution wins.
withSessionCleanup
    :: Supervisor
    -> AccessBoundary
    -> Text
    -> IO value
    -> IO (Either SessionMutationError value)
withSessionCleanup =
    withSessionReservation True

withSessionReservation
    :: Bool
    -> Supervisor
    -> AccessBoundary
    -> Text
    -> IO value
    -> IO (Either SessionMutationError value)
withSessionReservation waitForReservation supervisor boundary sessionId action =
    mask \restore -> do
        acquired <- atomically do
            state <- readTVar supervisor.supervisorState
            let key = (boundary, sessionId)
            if state.stateClosed
                then pure (Left SessionMutationSupervisorClosed)
                else if sessionBusy boundary sessionId state.stateTurns
                    then pure (Left SessionMutationBusy)
                    else if Set.member key state.stateActiveSessions
                        then
                            if waitForReservation
                                then retry
                                else pure (Left SessionMutationBusy)
                    else do
                        writeTVar supervisor.supervisorState
                            state
                                { stateActiveSessions =
                                    Set.insert key state.stateActiveSessions
                                }
                        pure (Right ())
        case acquired of
            Left err -> pure (Left err)
            Right () ->
                (Right <$> restore action)
                    `finally`
                        atomically
                            (modifyTVar'
                                supervisor.supervisorState
                                (\state ->
                                    state
                                        { stateActiveSessions =
                                            Set.delete
                                                (boundary, sessionId)
                                                state.stateActiveSessions
                                        }))

listHumanRequests
    :: Supervisor
    -> AccessBoundary
    -> Maybe TurnId
    -> IO (Either Text [HumanRequest])
listHumanRequests supervisor boundary turnId = do
    local <- atomically do
        state <- readTVar supervisor.supervisorState
        pure
            [ request
            | pending <- Map.elems state.statePendingInputs
            , let request = pending.pendingInputView
            , request.humanRequestBoundary == boundary
            , maybe
                True
                (== request.humanRequestTurnId)
                turnId
            ]
    durable <-
        supervisor.supervisorPersistence.turnPersistenceListHumanRequests
            boundary
            turnId
    pure do
        persisted <- durable
        let combined = persisted <> local
            sorted =
                sortOn
                    ( \request ->
                        ( Down request.humanRequestCreatedAt
                        , Down request.humanRequestId
                        )
                    )
                    ( Map.elems $
                        Map.fromList
                            [ (request.humanRequestId, request)
                            | request <- combined
                            ]
                    )
        pure $
            case turnId of
                Nothing -> take maximumHumanRequestPageSize sorted
                Just _ -> sorted

resolveHumanRequest
    :: Supervisor
    -> AccessBoundary
    -> RequestId
    -> HumanResponse
    -> IO (Either HumanRequestResolutionError HumanRequest)
resolveHumanRequest supervisor boundary requestId response = do
    durable <-
        supervisor.supervisorPersistence.turnPersistenceResolveHumanRequest
            boundary
            requestId
            response
    case durable of
        Left err ->
            pure (Left (HumanRequestResolutionStoreUnavailable err))
        Right HumanRequestNotFoundDurably ->
            pure (Left HumanRequestResolutionNotFound)
        Right HumanRequestAlreadyResolvedDurably ->
            pure
                ( Left
                    ( HumanRequestResolutionConflict
                        "request has already been resolved"
                    )
                )
        Right HumanRequestInvalidDecisionDurably ->
            pure
                ( Left
                    ( HumanRequestResolutionConflict
                        "decision is not one of the allowed options"
                    )
                )
        Right (HumanRequestResolvedDurably request) ->
            resolveLocal (Just request)
        Right HumanRequestLocalOnly ->
            resolveLocal Nothing
  where
    resolveLocal durableRequest = do
        now <- getCurrentTime
        atomically do
            state <- readTVar supervisor.supervisorState
            case Map.lookup requestId state.statePendingInputs of
                Nothing ->
                    pure $
                        maybe
                            (Left HumanRequestResolutionNotFound)
                            Right
                            durableRequest
                Just pending
                    | pending.pendingInputView.humanRequestBoundary /= boundary ->
                        pure (Left HumanRequestResolutionNotFound)
                    | durableRequest == Nothing
                        && not
                            ( validHumanAnswer
                                pending.pendingInputView.humanRequestOptions
                                response.humanResponseDecision
                            ) ->
                        pure
                            ( Left
                                ( HumanRequestResolutionConflict
                                    "decision is not one of the allowed options"
                                )
                            )
                    | otherwise -> do
                        accepted <-
                            tryPutTMVar
                                pending.pendingInputReply
                                (Right response)
                        if not accepted
                            then
                                pure
                                    ( Left
                                        ( HumanRequestResolutionConflict
                                            "request has already been resolved"
                                        )
                                    )
                            else
                                case completePendingInputInState
                                        supervisor.supervisorConfig
                                        now
                                        requestId
                                        state of
                                    Nothing ->
                                        pure
                                            (Left HumanRequestResolutionNotFound)
                                    Just (request, event, state') -> do
                                        writeTVar
                                            supervisor.supervisorState
                                            state'
                                        publishToSubscribers state' event
                                        pure (Right request)

lookupTurnAgents
    :: Supervisor
    -> AccessBoundary
    -> TurnId
    -> IO (Maybe Value)
lookupTurnAgents supervisor boundary turnId =
    atomically (do
        state <- readTVar supervisor.supervisorState
        pure do
            slot <- Map.lookup turnId state.stateTurns
            if slot.turnSlotRecord.turnRecordBoundary == boundary
                then Just slot.turnSlotAgents
                else Nothing)
        >>= sequence

publishEvent
    :: Supervisor
    -> AccessBoundary
    -> Text
    -> Maybe TurnId
    -> Maybe Text
    -> Value
    -> IO ()
publishEvent supervisor boundary eventType turnId sessionId value = do
    now <- getCurrentTime
    atomically do
        state <- readTVar supervisor.supervisorState
        when (not state.stateClosed) do
            let (event, state') = appendEventToState
                    supervisor.supervisorConfig
                    now
                    boundary
                    eventType
                    turnId
                    sessionId
                    value
                    state
            writeTVar supervisor.supervisorState state'
            publishToSubscribers state' event

subscribeEvents
    :: Supervisor
    -> AccessBoundary
    -> Maybe Integer
    -> IO
        (Either
            EventSubscriptionError
            (EventSubscription (TBQueue ServerEvent)))
subscribeEvents supervisor boundary lastEventId =
    atomically do
        state <- readTVar supervisor.supervisorState
        let subscribers = state.stateSubscribers
            tenantId = boundary.accessTenantId
            tenantSubscriberCount =
                length
                    [ ()
                    | subscriber <- Map.elems subscribers
                    , subscriber.eventSubscriberBoundary.accessTenantId
                        == tenantId
                    ]
        if Map.size subscribers
            >= supervisor.supervisorConfig.supervisorMaxEventSubscribers
            then pure (Left EventSubscriberLimitReached)
            else if tenantSubscriberCount
                >= supervisor.supervisorConfig.supervisorMaxEventSubscribersPerTenant
                then pure (Left EventSubscriberTenantLimitReached)
                else Right <$> createSubscription state
  where
    createSubscription state = do
        let buffer =
                Map.findWithDefault
                    (EventBuffer Seq.empty 1)
                    boundary
                    state.stateEvents
            replay = buffer.eventBufferReplay
            oldest = (.serverEventId) <$> Seq.lookup 0 replay
            latest = (.serverEventId) <$> Seq.lookup
                (Seq.length replay - 1)
                replay
            resetRequired = case (lastEventId, oldest) of
                (Nothing, _) -> False
                (Just requested, Nothing) -> requested > 0
                (Just requested, Just firstId) ->
                    requested < firstId - 1
                        || maybe False (requested >) latest
            replayEvents = case lastEventId of
                Nothing -> []
                Just requested ->
                    filter
                        ((> requested) . (.serverEventId))
                        (toList replay)
            subscriberId = state.stateNextSubscriberId
        channel <-
            newTBQueue
                (fromIntegral
                    supervisor.supervisorConfig.supervisorEventReplayLimit)
        writeTVar supervisor.supervisorState state
            { stateNextSubscriberId = subscriberId + 1
            , stateSubscribers =
                Map.insert
                    subscriberId
                    EventSubscriber
                        { eventSubscriberBoundary = boundary
                        , eventSubscriberQueue = channel
                        }
                    state.stateSubscribers
            }
        pure EventSubscription
            { subscriptionReplay = replayEvents
            , subscriptionResetRequired = resetRequired
            , subscriptionLatestEventId = latest
            , subscriptionChannel = channel
            , subscriptionClose =
                atomically $
                    modifyTVar'
                        supervisor.supervisorState
                        (\current ->
                            current
                                { stateSubscribers =
                                    Map.delete
                                        subscriberId
                                        current.stateSubscribers
                                })
            }

dispatcherLoop :: Supervisor -> IO ()
dispatcherLoop supervisor = do
    next <- atomically (reserveRunnable supervisor)
    case next of
        Nothing -> pure ()
        Just (turnId, spec) -> do
            spawnTrackedWorker supervisor turnId spec
            dispatcherLoop supervisor

cancellationMonitorLoop :: Supervisor -> IO ()
cancellationMonitorLoop supervisor = do
    threadDelay cancellationPollIntervalMicros
    pending <- atomically do
        state <- readTVar supervisor.supervisorState
        if state.stateClosed
            then pure Nothing
            else
                pure . Just $
                    [ slot.turnSlotRecord
                    | slot <- Map.elems state.stateTurns
                    , not slot.turnSlotCancelling
                    , isActiveStatus
                        slot.turnSlotRecord.turnRecordStatus
                    ]
    case pending of
        Nothing -> pure ()
        Just records -> do
            checks <-
                forConcurrentlyBatched
                    cancellationCheckBatchSize
                    records \record -> do
                    result <-
                        tryAny $
                            timeout
                                cancellationCheckTimeoutMicros
                                (supervisor.supervisorPersistence.turnPersistenceShouldCancel record)
                    pure (record, result)
            forM_ checks \case
                (_, Right (Just (Right False))) -> pure ()
                (record, _) ->
                    -- A process which cannot confirm its durable owner fence
                    -- must stop local side effects before another instance can
                    -- reclaim the expired turn. Terminal persistence continues
                    -- independently so one unavailable tenant cannot stall
                    -- ownership checks for every other tenant.
                    spawnCancellationTask supervisor record
            cancellationMonitorLoop supervisor

forConcurrentlyBatched
    :: Int
    -> [input]
    -> (input -> IO output)
    -> IO [output]
forConcurrentlyBatched batchSize values action
    | batchSize <= 0 =
        fail "concurrent batch size must be positive"
    | otherwise =
        go values
  where
    go [] = pure []
    go remaining = do
        let (batch, rest) = splitAt batchSize remaining
        batchResults <- forConcurrently batch action
        restResults <- go rest
        pure (batchResults <> restResults)

spawnCancellationTask :: Supervisor -> TurnRecord -> IO ()
spawnCancellationTask supervisor record = mask \restore -> do
    start <- newEmptyMVar
    task <-
        async do
            taskThreadId <- myThreadId
            (readMVar start >> restore cancelRecord)
                `finally`
                    atomically
                        ( modifyTVar'
                            supervisor.supervisorState
                            ( \state ->
                                state
                                    { stateCancellationTasks =
                                        deleteCancellationTaskIfOwned
                                            record.turnRecordId
                                            taskThreadId
                                            state.stateCancellationTasks
                                    }
                            )
                        )
    accepted <- atomically do
        state <- readTVar supervisor.supervisorState
        if
            state.stateClosed
                || Map.member
                    record.turnRecordId
                    state.stateCancellationTasks
            then pure False
            else do
                writeTVar
                    supervisor.supervisorState
                    state
                        { stateCancellationTasks =
                            Map.insert
                                record.turnRecordId
                                task
                                state.stateCancellationTasks
                        }
                pure True
    if accepted
        then putMVar start ()
        else do
            cancel task
            void (waitCatch task)
  where
    cancelRecord =
        void $
            tryAny $
                void $
                    cancelTurn
                        supervisor
                        record.turnRecordBoundary
                        record.turnRecordId

-- | Delete only the task whose own finalizer is running. A rejected duplicate
-- must leave the incumbent registered so shutdown can still cancel and join it.
deleteCancellationTaskIfOwned
    :: TurnId
    -> ThreadId
    -> Map TurnId (Async ())
    -> Map TurnId (Async ())
deleteCancellationTaskIfOwned turnId ownerThreadId =
    Map.update
        ( \registered ->
            if asyncThreadId registered == ownerThreadId
                then Nothing
                else Just registered
        )
        turnId

reserveRunnable :: Supervisor -> STM (Maybe (TurnId, TurnSpec))
reserveRunnable supervisor = do
    state <- readTVar supervisor.supervisorState
    if state.stateClosed
        then pure Nothing
        else if Map.size state.stateWorkers
            >= supervisor.supervisorConfig.supervisorMaxConcurrentTurns
            then retry
            else case pickRunnable
                supervisor.supervisorConfig
                state.stateTurns
                state.stateActiveSessions
                state.stateWorkers
                state.stateLastScheduledTenant
                state.stateQueue of
                    Nothing -> retry
                    Just (turnId, remaining, tenantId) ->
                        case Map.lookup turnId state.stateTurns of
                            Nothing -> do
                                writeTVar
                                    supervisor.supervisorState
                                    state { stateQueue = remaining }
                                reserveRunnable supervisor
                            Just slot -> do
                                let sessionId =
                                        slot.turnSlotRecord.turnRecordSessionId
                                    record =
                                        slot.turnSlotRecord
                                            { turnRecordStatus = TurnRunning }
                                    state' = state
                                        { stateQueue = remaining
                                        , stateTurns =
                                            Map.insert
                                                turnId
                                                slot
                                                    { turnSlotRecord = record
                                                    , turnSlotSpec =
                                                        slot.turnSlotSpec
                                                            { turnSpecPrompt =
                                                                ""
                                                            , turnSpecImages = []
                                                            , turnSpecFiles = []
                                                            }
                                                    }
                                                state.stateTurns
                                        , stateActiveSessions =
                                            Set.insert
                                                ( slot.turnSlotRecord.turnRecordBoundary
                                                , sessionId
                                                )
                                                state.stateActiveSessions
                                        , stateLastScheduledTenant =
                                            Just tenantId
                                        }
                                writeTVar supervisor.supervisorState state'
                                pure (Just (turnId, slot.turnSlotSpec))

markTurnStarted
    :: Supervisor
    -> UTCTime
    -> TurnId
    -> TurnSpec
    -> STM Bool
markTurnStarted supervisor now turnId spec = do
    state <- readTVar supervisor.supervisorState
    case Map.lookup turnId state.stateTurns of
        Just slot
            | slot.turnSlotRecord.turnRecordStatus == TurnRunning
            , not slot.turnSlotCancelling
            , slot.turnSlotRecord.turnRecordBoundary
                == spec.turnSpecBoundary
            , slot.turnSlotRecord.turnRecordSessionId
                == spec.turnSpecSessionId -> do
                let withStarted =
                        state
                            { stateTurns = Map.adjust
                                (\slot ->
                                    slot
                                        { turnSlotRecord =
                                            slot.turnSlotRecord
                                                { turnRecordStartedAt = Just now
                                                }
                                        })
                                turnId
                                state.stateTurns
                            }
                    (event, state') = appendEventToState
                        supervisor.supervisorConfig
                        now
                        spec.turnSpecBoundary
                        "turn.started"
                        (Just turnId)
                        (Just spec.turnSpecSessionId)
                        (object [])
                        withStarted
                writeTVar supervisor.supervisorState state'
                publishToSubscribers state' event
                pure True
        _ -> pure False

spawnTrackedWorker :: Supervisor -> TurnId -> TurnSpec -> IO ()
spawnTrackedWorker supervisor turnId spec = mask \restore -> do
    gate <- atomically newEmptyTMVar
    worker <- async do
        atomically (takeTMVar gate)
        restore (executeTurn supervisor turnId spec)
    accepted <- atomically do
        state <- readTVar supervisor.supervisorState
        case Map.lookup turnId state.stateTurns of
            Just slot
                | not state.stateClosed
                , slot.turnSlotRecord.turnRecordStatus == TurnRunning
                , slot.turnSlotRecord.turnRecordBoundary
                    == spec.turnSpecBoundary
                , slot.turnSlotRecord.turnRecordSessionId
                    == spec.turnSpecSessionId
                , Set.member
                    (spec.turnSpecBoundary, spec.turnSpecSessionId)
                    state.stateActiveSessions -> do
                        writeTVar supervisor.supervisorState
                            state
                                { stateWorkers =
                                    Map.insert
                                        turnId worker state.stateWorkers
                                }
                        putTMVar gate ()
                        pure True
            _ -> pure False
    if accepted
        then pure ()
        else do
            cancel worker
            void (waitCatch worker)

executeTurn :: Supervisor -> TurnId -> TurnSpec -> IO ()
executeTurn supervisor turnId spec =
    finally guardedRun finish
  where
    control = TurnControl
        { turnControlEmit = \eventType value ->
            publishTurnEvent
                supervisor
                spec.turnSpecBoundary
                turnId
                spec.turnSpecSessionId
                eventType
                value
        , turnControlRequestInput =
            requestTurnInput supervisor turnId spec
        , turnControlRegisterCancel = \action ->
            atomically $
                modifyTVar'
                    supervisor.supervisorState
                    (setTurnCancellation turnId action)
        , turnControlSetAgents = \agents ->
            atomically $
                modifyTVar'
                    supervisor.supervisorState
                    (setTurnAgents turnId agents)
        }

    guardedRun = do
        let Supervisor { supervisorBoundaryGuard = boundaryGuard } =
                supervisor
        guarded <-
            tryAny $
                boundaryGuard
                    spec.turnSpecBoundary
                    runAndFinalize
                :: IO (Either SomeException (Either Text ()))
        case guarded of
            Left _ ->
                finalizeTurnStateOnly
                    supervisor
                    turnId
                    "gateway boundary validation failed"
            Right (Left err) ->
                finalizeTurnStateOnly supervisor turnId err
            Right (Right ()) -> pure ()

    runAndFinalize = do
        startedAt <- getCurrentTime
        current <- lookupTurn supervisor spec.turnSpecBoundary turnId
        case current of
            Nothing -> pure ()
            Just record ->
                supervisor.supervisorPersistence.turnPersistenceStarted
                    record
                    startedAt
                    >>= \case
                        Left err ->
                            finalizeTurn
                                supervisor
                                turnId
                                spec
                                ( Left
                                    ( "could not persist turn start: "
                                        <> err
                                    )
                                )
                        Right () -> do
                            started <-
                                atomically $
                                    markTurnStarted
                                        supervisor
                                        startedAt
                                        turnId
                                        spec
                            when started do
                                outcome <-
                                    tryAny
                                        (supervisor.supervisorRunner control spec) ::
                                        IO
                                            ( Either
                                                SomeException
                                                (Either Text TurnExecutionOutput)
                                            )
                                let result = case outcome of
                                        Left _ ->
                                            Left
                                                "agent turn terminated unexpectedly"
                                        Right value -> value
                                finalizeTurn supervisor turnId spec result

    finish =
        atomically $
            modifyTVar' supervisor.supervisorState \state ->
                let cancellationOwnsTerminal =
                        maybe
                            False
                            (.turnSlotCancelling)
                            (Map.lookup turnId state.stateTurns)
                 in state
                    { stateWorkers =
                        Map.delete turnId state.stateWorkers
                    , stateActiveSessions =
                        if cancellationOwnsTerminal
                            then state.stateActiveSessions
                            else
                                Set.delete
                                    ( spec.turnSpecBoundary
                                    , spec.turnSpecSessionId
                                    )
                                    state.stateActiveSessions
                    }

finalizeTurn ::
    Supervisor ->
    TurnId ->
    TurnSpec ->
    Either Text TurnExecutionOutput ->
    IO ()
finalizeTurn supervisor turnId spec result = do
    now <- getCurrentTime
    current <- atomically do
        state <- readTVar supervisor.supervisorState
        pure do
            slot <- Map.lookup turnId state.stateTurns
            if slot.turnSlotRecord.turnRecordBoundary
                == spec.turnSpecBoundary
                && not slot.turnSlotCancelling
                then Just slot.turnSlotRecord
                else Nothing
    case current of
        Nothing -> pure ()
        Just record
            | isTerminalStatus record.turnRecordStatus ->
                pure ()
        Just record -> do
            canonical <-
                persistTerminalEventually
                    supervisor
                    record
                    now
                    (either TurnErrored TurnSucceeded result)
            atomically $
                publishTerminalRecord supervisor canonical

publishTerminalRecord
    :: Supervisor
    -> TurnRecord
    -> STM ()
publishTerminalRecord supervisor canonical = do
    state <- readTVar supervisor.supervisorState
    case Map.lookup canonical.turnRecordId state.stateTurns of
        Nothing -> pure ()
        Just slot
            | slot.turnSlotRecord == canonical ->
                pure ()
            | otherwise -> do
                let eventType = case canonical.turnRecordStatus of
                        TurnCompleted -> "turn.completed"
                        TurnFailed -> "turn.failed"
                        TurnCancelled -> "turn.cancelled"
                        _ -> "turn.failed"
                    eventAt =
                        maybe
                            canonical.turnRecordCreatedAt
                            id
                            canonical.turnRecordFinishedAt
                    (event, state') = appendEventToState
                        supervisor.supervisorConfig
                        eventAt
                        canonical.turnRecordBoundary
                        eventType
                        (Just canonical.turnRecordId)
                        (Just canonical.turnRecordSessionId)
                        (object ["error" .= canonical.turnRecordError])
                        state
                            { stateTurns =
                                Map.insert
                                    canonical.turnRecordId
                                    slot
                                        { turnSlotRecord = canonical
                                        , turnSlotCancelling = False
                                        , turnSlotCancel = Nothing
                                        }
                                    state.stateTurns
                            , stateActiveSessions =
                                Set.delete
                                    ( canonical.turnRecordBoundary
                                    , canonical.turnRecordSessionId
                                    )
                                    state.stateActiveSessions
                            }
                writeTVar supervisor.supervisorState state'
                publishToSubscribers state' event

releaseCancellationClaim :: Supervisor -> TurnRecord -> STM ()
releaseCancellationClaim supervisor record =
    modifyTVar' supervisor.supervisorState \state ->
        state
            { stateTurns =
                Map.adjust
                    ( \slot ->
                        if
                            slot.turnSlotCancelling
                                && slot.turnSlotRecord == record
                            then
                                slot
                                    { turnSlotCancelling = False
                                    , turnSlotCancel = Nothing
                                    }
                            else slot
                    )
                    record.turnRecordId
                    state.stateTurns
            }

-- A failed boundary guard must not invoke an event callback for the stale
-- credential. Retain only a terminal in-memory state, still scoped to the
-- originally admitted boundary.
finalizeTurnStateOnly
    :: Supervisor
    -> TurnId
    -> Text
    -> IO ()
finalizeTurnStateOnly supervisor turnId err = do
    now <- getCurrentTime
    current <- atomically do
        state <- readTVar supervisor.supervisorState
        pure do
            slot <- Map.lookup turnId state.stateTurns
            if slot.turnSlotCancelling
                then Nothing
                else Just slot.turnSlotRecord
    forM_ current \record -> do
        canonical <-
            persistTerminalEventually
                supervisor
                record
                now
                (TurnErrored err)
        atomically $
            modifyTVar' supervisor.supervisorState \state ->
                state
                    { stateTurns =
                        Map.adjust
                            (\slot ->
                                slot
                                    { turnSlotRecord = canonical
                                    , turnSlotCancelling = False
                                    , turnSlotCancel = Nothing
                                    })
                            turnId
                            state.stateTurns
                    }

persistTerminalEventually ::
    Supervisor ->
    TurnRecord ->
    UTCTime ->
    TurnTerminalOutcome ->
    IO TurnRecord
persistTerminalEventually supervisor record finishedAt outcome =
    go terminalPersistenceInitialRetryMicros
  where
    go retryDelay =
        supervisor.supervisorPersistence.turnPersistenceTerminal
            record
            finishedAt
            outcome
            >>= \case
                Right canonical -> pure canonical
                Left _ -> do
                    threadDelay retryDelay
                    go
                        ( min
                            terminalPersistenceMaximumRetryMicros
                            (retryDelay * 2)
                        )

requestTurnInput ::
    Supervisor ->
    TurnId ->
    TurnSpec ->
    HumanRequestSpec ->
    IO (Either Text HumanResponse)
requestTurnInput supervisor turnId spec requestSpec =
    mask \restore -> do
        now <- getCurrentTime
        requestId <- RequestId <$> newUUIDv7Text
        reply <- atomically newEmptyTMVar
        let boundedRequestSpec = HumanRequestSpec
                { humanRequestSpecKind =
                    requestSpec.humanRequestSpecKind
                , humanRequestSpecPrompt =
                    boundedSupervisorText
                        requestSpec.humanRequestSpecPrompt
                , humanRequestSpecOptions =
                    map boundedSupervisorText
                        (take 100 requestSpec.humanRequestSpecOptions)
                }
            request = HumanRequest
                { humanRequestId = requestId
                , humanRequestTurnId = turnId
                , humanRequestSessionId =
                    spec.turnSpecSessionId
                , humanRequestBoundary =
                    spec.turnSpecBoundary
                , humanRequestKind =
                    boundedRequestSpec.humanRequestSpecKind
                , humanRequestPrompt =
                    boundedRequestSpec.humanRequestSpecPrompt
                , humanRequestOptions =
                    boundedRequestSpec.humanRequestSpecOptions
                , humanRequestCreatedAt = now
                }
        if LazyByteString.length (encode request)
            > maximumHumanRequestBytes
            then
                pure
                    ( Left
                        "human request exceeds the public size limit"
                    )
            else do
                current <- atomically (currentTurnRecord supervisor turnId)
                case current of
                    Left err -> pure (Left err)
                    Right record ->
                        supervisor.supervisorPersistence.turnPersistenceCreateHumanRequest
                            record
                            request
                            >>= \case
                                Left err -> pure (Left err)
                                Right () -> do
                                    result <-
                                        restore
                                            (installAndWait record request reply)
                                            `onException` cleanup
                                                record
                                                requestId
                                                HumanRequestAbandoned
                                    cleanup
                                        record
                                        requestId
                                        ( case result of
                                            Left _ -> HumanRequestAbandoned
                                            Right _ -> HumanResponseConsumed
                                        )
                                    pure result
  where
    cleanup record requestId disposition =
        void $
            timeout humanRequestCleanupTimeoutMicros $
                supervisor.supervisorPersistence.turnPersistenceDeleteHumanRequest
                    record
                    requestId
                    disposition

    installAndWait record request reply = do
        installed <- atomically do
            state <- readTVar supervisor.supervisorState
            case Map.lookup turnId state.stateTurns of
                Nothing -> pure (Left "turn not found")
                Just slot
                    | slot.turnSlotCancelling
                        || isTerminalStatus
                            slot.turnSlotRecord.turnRecordStatus ->
                        pure (Left "turn is no longer running")
                    | otherwise -> do
                        let pending = PendingInput
                                { pendingInputView = request
                                , pendingInputReply = reply
                                }
                            waitingRecord =
                                slot.turnSlotRecord
                                    { turnRecordStatus =
                                        TurnWaitingForInput
                                    }
                            (event, state') = appendEventToState
                                supervisor.supervisorConfig
                                request.humanRequestCreatedAt
                                spec.turnSpecBoundary
                                "request.created"
                                (Just turnId)
                                (Just spec.turnSpecSessionId)
                                (toJSONRequest request)
                                state
                                    { statePendingInputs =
                                        Map.insert
                                            request.humanRequestId
                                            pending
                                            state.statePendingInputs
                                    , stateTurns =
                                        Map.insert
                                            turnId
                                            slot
                                                { turnSlotRecord =
                                                    waitingRecord
                                                }
                                            state.stateTurns
                                    }
                        writeTVar supervisor.supervisorState state'
                        publishToSubscribers state' event
                        pure (Right ())
        case installed of
            Left err -> pure (Left err)
            Right () ->
                waitForHumanResponse
                    supervisor
                    record
                    request
                    reply

currentTurnRecord ::
    Supervisor ->
    TurnId ->
    STM (Either Text TurnRecord)
currentTurnRecord supervisor turnId = do
    state <- readTVar supervisor.supervisorState
    case Map.lookup turnId state.stateTurns of
        Nothing -> pure (Left "turn not found")
        Just slot
            | slot.turnSlotCancelling
                || isTerminalStatus
                    slot.turnSlotRecord.turnRecordStatus ->
                pure (Left "turn is no longer running")
            | otherwise -> pure (Right slot.turnSlotRecord)

waitForHumanResponse ::
    Supervisor ->
    TurnRecord ->
    HumanRequest ->
    TMVar (Either Text HumanResponse) ->
    IO (Either Text HumanResponse)
waitForHumanResponse supervisor record request reply =
    timeout
        humanResponsePollIntervalMicros
        (atomically (takeTMVar reply))
        >>= \case
            Just response -> pure response
            Nothing ->
                supervisor.supervisorPersistence.turnPersistenceLoadHumanResponse
                    record
                    request.humanRequestId
                    >>= \case
                        Left err -> pure (Left err)
                        Right Nothing ->
                            waitForHumanResponse
                                supervisor
                                record
                                request
                                reply
                        Right (Just response) -> do
                            now <- getCurrentTime
                            completed <- atomically do
                                state <- readTVar supervisor.supervisorState
                                case completePendingInputInState
                                        supervisor.supervisorConfig
                                        now
                                        request.humanRequestId
                                        state of
                                    Nothing -> pure False
                                    Just (_, event, state') -> do
                                        writeTVar
                                            supervisor.supervisorState
                                            state'
                                        publishToSubscribers state' event
                                        pure True
                            pure
                                ( if completed
                                    then Right response
                                    else
                                        Left
                                            "turn is no longer waiting for input"
                                )

setTurnCancellation :: TurnId -> IO () -> SupervisorState -> SupervisorState
setTurnCancellation turnId action state =
    state
        { stateTurns =
            Map.adjust
                (\slot ->
                    if
                        isActiveStatus slot.turnSlotRecord.turnRecordStatus
                            && not slot.turnSlotCancelling
                        then slot { turnSlotCancel = Just action }
                        else slot)
                turnId
                state.stateTurns
        }

setTurnAgents :: TurnId -> IO Value -> SupervisorState -> SupervisorState
setTurnAgents turnId agents state =
    state
        { stateTurns =
            Map.adjust
                (\slot -> slot { turnSlotAgents = agents })
                turnId
                state.stateTurns
        }

pickRunnable
    :: SupervisorConfig
    -> Map TurnId TurnSlot
    -> Set (AccessBoundary, Text)
    -> Map TurnId (Async ())
    -> Maybe TenantId
    -> Seq TurnId
    -> Maybe (TurnId, Seq TurnId, TenantId)
pickRunnable config turns activeSessions workers lastTenant queue =
    case preferred <> fallback of
        (turnId, tenantId) : _ ->
            Just (turnId, Seq.filter (/= turnId) queue, tenantId)
        [] -> Nothing
  where
    eligible =
        [ (turnId, boundary.accessTenantId)
        | turnId <- toList queue
        , Just slot <- [Map.lookup turnId turns]
        , slot.turnSlotRecord.turnRecordStatus == TurnQueued
        , let boundary = slot.turnSlotRecord.turnRecordBoundary
        , Set.notMember
            (boundary, slot.turnSlotRecord.turnRecordSessionId)
            activeSessions
        , runningForTenant boundary.accessTenantId turns workers
            < config.supervisorMaxConcurrentTurnsPerTenant
        ]
    preferred =
        filter (\(_, tenantId) -> Just tenantId /= lastTenant) eligible
    fallback = case preferred of
        [] -> eligible
        _ -> []

queuedForTenant :: TenantId -> Map TurnId TurnSlot -> Int
queuedForTenant tenantId turns =
    length
        [ ()
        | slot <- Map.elems turns
        , slot.turnSlotRecord.turnRecordStatus == TurnQueued
        , slot.turnSlotRecord.turnRecordBoundary.accessTenantId == tenantId
        ]

-- Keep decoded attachment payloads bounded while they wait in the in-memory queue.
-- Running turns clear their queued copy before execution.
maximumQueuedAttachmentBytes, maximumQueuedAttachmentBytesPerTenant :: Int
maximumQueuedAttachmentBytes = 80 * 1024 * 1024
maximumQueuedAttachmentBytesPerTenant = 40 * 1024 * 1024

turnAttachmentBytes :: TurnSpec -> Int
turnAttachmentBytes spec =
    sum (map (\image -> ByteString.length image.imageBytes) spec.turnSpecImages)
        + sum (map (\file -> ByteString.length file.fileBytes) spec.turnSpecFiles)

queuedAttachmentBytes :: Map TurnId TurnSlot -> Int
queuedAttachmentBytes =
    sum
        . map (\slot -> turnAttachmentBytes slot.turnSlotSpec)
        . filter (\slot -> slot.turnSlotRecord.turnRecordStatus == TurnQueued)
        . Map.elems

queuedAttachmentBytesForTenant :: TenantId -> Map TurnId TurnSlot -> Int
queuedAttachmentBytesForTenant tenantId =
    sum
        . map (\slot -> turnAttachmentBytes slot.turnSlotSpec)
        . filter
            ( \slot ->
                slot.turnSlotRecord.turnRecordStatus == TurnQueued
                    && slot.turnSlotRecord.turnRecordBoundary.accessTenantId
                        == tenantId
            )
        . Map.elems

runningForTenant
    :: TenantId
    -> Map TurnId TurnSlot
    -> Map TurnId (Async ())
    -> Int
runningForTenant tenantId turns workers =
    length
        [ ()
        | turnId <- Map.keys workers
        , Just slot <- [Map.lookup turnId turns]
        , slot.turnSlotRecord.turnRecordBoundary.accessTenantId == tenantId
        ]

publishTurnEvent
    :: Supervisor
    -> AccessBoundary
    -> TurnId
    -> Text
    -> Text
    -> Value
    -> IO ()
publishTurnEvent
        supervisor boundary turnId sessionId eventType value = do
    now <- getCurrentTime
    atomically do
        state <- readTVar supervisor.supervisorState
        case Map.lookup turnId state.stateTurns of
            Just slot
                | not state.stateClosed
                , not slot.turnSlotCancelling
                , slot.turnSlotRecord.turnRecordBoundary == boundary
                , slot.turnSlotRecord.turnRecordSessionId == sessionId
                , isActiveStatus slot.turnSlotRecord.turnRecordStatus -> do
                    let (event, state') = appendEventToState
                            supervisor.supervisorConfig
                            now
                            boundary
                            eventType
                            (Just turnId)
                            (Just sessionId)
                            value
                            state
                    writeTVar supervisor.supervisorState state'
                    publishToSubscribers state' event
            _ -> pure ()

appendEventToState
    :: SupervisorConfig
    -> UTCTime
    -> AccessBoundary
    -> Text
    -> Maybe TurnId
    -> Maybe Text
    -> Value
    -> SupervisorState
    -> (ServerEvent, SupervisorState)
appendEventToState config now boundary eventType turnId sessionId value state =
    (event, state')
  where
    buffer =
        Map.findWithDefault
            (EventBuffer Seq.empty 1)
            boundary
            state.stateEvents
    event = ServerEvent
        { serverEventId = buffer.eventBufferNextId
        , serverEventBoundary = boundary
        , serverEventType = eventType
        , serverEventTurnId = turnId
        , serverEventSessionId = sessionId
        , serverEventData = value
        , serverEventAt = now
        }
    replay =
        trimReplay
            config.supervisorEventReplayLimit
            (buffer.eventBufferReplay |> event)
    buffer' = EventBuffer
        { eventBufferReplay = replay
        , eventBufferNextId = buffer.eventBufferNextId + 1
        }
    state' =
        state
            { stateEvents =
                Map.insert boundary buffer' state.stateEvents
            }

trimReplay :: Int -> Seq a -> Seq a
trimReplay limit values =
    Seq.drop (max 0 (Seq.length values - limit)) values

publishToSubscribers :: SupervisorState -> ServerEvent -> STM ()
publishToSubscribers state event =
    forM_ (Map.elems state.stateSubscribers) \subscriber ->
        when
            (subscriber.eventSubscriberBoundary
                == event.serverEventBoundary) do
                full <- isFullTBQueue subscriber.eventSubscriberQueue
                when full (void (readTBQueue subscriber.eventSubscriberQueue))
                writeTBQueue subscriber.eventSubscriberQueue event

sessionBusy
    :: AccessBoundary
    -> Text
    -> Map TurnId TurnSlot
    -> Bool
sessionBusy boundary sessionId =
    any
        (\slot ->
            slot.turnSlotRecord.turnRecordBoundary == boundary
                && slot.turnSlotRecord.turnRecordSessionId == sessionId
                && isActiveStatus slot.turnSlotRecord.turnRecordStatus)
        . Map.elems

pruneTerminalTurns :: Map TurnId TurnSlot -> Map TurnId TurnSlot
pruneTerminalTurns turns =
    Map.fromList
        [ (slot.turnSlotRecord.turnRecordId, slot)
        | slot <- active <> retainedTerminal
        ]
  where
    slots = Map.elems turns
    active =
        filter
            (isActiveStatus . (.turnSlotRecord.turnRecordStatus))
            slots
    retainedTerminal =
        take maximumRetainedTerminalTurns $
            sortOn
                (Down . (.turnSlotRecord.turnRecordCreatedAt))
                (filter
                    (isTerminalStatus . (.turnSlotRecord.turnRecordStatus))
                    slots)

maximumRetainedTerminalTurns :: Int
maximumRetainedTerminalTurns = 1000

isActiveStatus :: TurnStatus -> Bool
isActiveStatus = \case
    TurnQueued -> True
    TurnRunning -> True
    TurnWaitingForInput -> True
    TurnCompleted -> False
    TurnFailed -> False
    TurnCancelled -> False

isTerminalStatus :: TurnStatus -> Bool
isTerminalStatus = not . isActiveStatus

cancelSlot
    :: UTCTime
    -> TurnSlot
    -> Map TurnId TurnSlot
    -> Map TurnId TurnSlot
cancelSlot _now slot =
    Map.insert
        slot.turnSlotRecord.turnRecordId
        slot
            { turnSlotCancelling = True
            , turnSlotCancel = Nothing
            }

cancelledRecord
    :: UTCTime
    -> TurnRecord
    -> TurnRecord
cancelledRecord now record =
    record
        { turnRecordStatus = TurnCancelled
        , turnRecordFinishedAt = Just now
        , turnRecordError = Nothing
        }

terminalRecord
    :: UTCTime
    -> TurnTerminalOutcome
    -> TurnRecord
    -> TurnRecord
terminalRecord finishedAt outcome record =
    case outcome of
        TurnSucceeded _ ->
            record
                { turnRecordStatus = TurnCompleted
                , turnRecordFinishedAt = Just finishedAt
                , turnRecordError = Nothing
                }
        TurnErrored err ->
            record
                { turnRecordStatus = TurnFailed
                , turnRecordFinishedAt = Just finishedAt
                , turnRecordError = Just (boundedSupervisorText err)
                }
        TurnWasCancelled ->
            cancelledRecord finishedAt record

resumeWaitingTurn
    :: UTCTime
    -> TurnSlot
    -> TurnSlot
resumeWaitingTurn _now slot
    | slot.turnSlotRecord.turnRecordStatus == TurnWaitingForInput =
        slot
            { turnSlotRecord =
                slot.turnSlotRecord
                    { turnRecordStatus = TurnRunning }
            }
    | otherwise = slot

completePendingInputInState ::
    SupervisorConfig ->
    UTCTime ->
    RequestId ->
    SupervisorState ->
    Maybe (HumanRequest, ServerEvent, SupervisorState)
completePendingInputInState config now requestId state = do
    pending <- Map.lookup requestId state.statePendingInputs
    let request = pending.pendingInputView
        turns' =
            Map.adjust
                (resumeWaitingTurn now)
                request.humanRequestTurnId
                state.stateTurns
        withoutPending =
            state
                { statePendingInputs =
                    Map.delete requestId state.statePendingInputs
                , stateTurns = turns'
                }
        (event, state') =
            appendEventToState
                config
                now
                request.humanRequestBoundary
                "request.resolved"
                (Just request.humanRequestTurnId)
                (Just request.humanRequestSessionId)
                (object ["requestId" .= requestId.unRequestId])
                withoutPending
    pure (request, event, state')

validHumanAnswer :: [Text] -> Text -> Bool
validHumanAnswer options answer =
    null options || answer `elem` options

boundedSupervisorText :: Text -> Text
boundedSupervisorText value
    | Text.length value <= 16384 = value
    | otherwise = Text.take 16383 value <> "…"

terminalPersistenceInitialRetryMicros :: Int
terminalPersistenceInitialRetryMicros = 100 * 1000

terminalPersistenceMaximumRetryMicros :: Int
terminalPersistenceMaximumRetryMicros = 2 * 1000 * 1000

cancellationPollIntervalMicros :: Int
cancellationPollIntervalMicros = 100 * 1000

cancellationCheckTimeoutMicros :: Int
cancellationCheckTimeoutMicros = 5 * 1000 * 1000

cancellationCheckBatchSize :: Int
cancellationCheckBatchSize = 4

humanResponsePollIntervalMicros :: Int
humanResponsePollIntervalMicros = 100 * 1000

humanRequestCleanupTimeoutMicros :: Int
humanRequestCleanupTimeoutMicros = 1000 * 1000

maximumHumanRequestBytes :: Int64
maximumHumanRequestBytes = 64 * 1024

maximumHumanRequestPageSize :: Int
maximumHumanRequestPageSize = 200

terminalPersistenceShutdownTimeoutMicros :: Int
terminalPersistenceShutdownTimeoutMicros = 5 * 1000 * 1000

cancelHookTimeoutMicros :: Int
cancelHookTimeoutMicros = 1000 * 1000

toJSONRequest :: HumanRequest -> Value
toJSONRequest request = object
    [ "request" .= request
    ]

toJSONEmptyArray :: Value
toJSONEmptyArray = toJSON ([] :: [Value])
