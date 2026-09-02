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
    , TurnControl(..)
    , TurnRunner
    , TurnBoundaryGuard
    , newSupervisor
    , newSupervisorWithBoundaryGuard
    , closeSupervisor
    , submitTurn
    , submitTurnChecked
    , cancelTurn
    , lookupTurn
    , listTurns
    , sessionHasActiveTurn
    , withSessionMutation
    , listHumanRequests
    , resolveHumanRequest
    , lookupTurnAgents
    , publishEvent
    , subscribeEvents
    ) where

import Agent.Server.Types
import Control.Concurrent.Async
    ( Async
    , async
    , cancel
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
    , tryAny
    )
import Control.Monad (forM_, void, when)
import Data.Aeson (Value, object, toJSON, (.=))
import Data.Foldable (toList)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Ord (Down(..))
import Data.Sequence (Seq, ViewL(..), (|>))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime, getCurrentTime)
import System.Timeout (timeout)

data SupervisorConfig = SupervisorConfig
    { supervisorMaxConcurrentTurns :: !Int
    , supervisorMaxQueuedTurns :: !Int
    , supervisorEventReplayLimit :: !Int
    }
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

type TurnRunner = TurnControl -> TurnSpec -> IO (Either Text ())

-- | Run an action only while the exact admitted gateway boundary is leased.
--
-- Production uses the credential turn lease here so the runtime invocation
-- and its terminal state/event commit share one uninterrupted boundary.
type TurnBoundaryGuard =
    forall value.
    GatewayBoundary ->
    IO value ->
    IO (Either Text value)

data TurnSlot = TurnSlot
    { turnSlotSpec :: !TurnSpec
    , turnSlotRecord :: !TurnRecord
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
    { eventSubscriberBoundary :: !GatewayBoundary
    , eventSubscriberQueue :: !(TBQueue ServerEvent)
    }

data SupervisorState = SupervisorState
    { stateNextTurnId :: !Integer
    , stateNextRequestId :: !Integer
    , stateQueue :: !(Seq TurnId)
    , stateTurns :: !(Map TurnId TurnSlot)
    , stateActiveSessions :: !(Set (GatewayBoundary, Text))
    , stateWorkers :: !(Map TurnId (Async ()))
    , statePendingInputs :: !(Map RequestId PendingInput)
    , stateEvents :: !(Map GatewayBoundary EventBuffer)
    , stateNextSubscriberId :: !Integer
    , stateSubscribers :: !(Map Integer EventSubscriber)
    , stateClosed :: !Bool
    }

data Supervisor = Supervisor
    { supervisorConfig :: !SupervisorConfig
    , supervisorRunner :: !TurnRunner
    , supervisorBoundaryGuard :: !TurnBoundaryGuard
    , supervisorState :: !(TVar SupervisorState)
    , supervisorDispatcher :: !(MVar (Async ()))
    }

newSupervisor :: SupervisorConfig -> TurnRunner -> IO Supervisor
newSupervisor config =
    newSupervisorWithBoundaryGuard config
        (\_ action -> Right <$> action)

newSupervisorWithBoundaryGuard
    :: SupervisorConfig
    -> TurnBoundaryGuard
    -> TurnRunner
    -> IO Supervisor
newSupervisorWithBoundaryGuard config boundaryGuard runner
    | config.supervisorMaxConcurrentTurns < 1 =
        fail "supervisorMaxConcurrentTurns must be positive"
    | config.supervisorMaxQueuedTurns < 1 =
        fail "supervisorMaxQueuedTurns must be positive"
    | config.supervisorEventReplayLimit < 1 =
        fail "supervisorEventReplayLimit must be positive"
    | otherwise = mask \restore -> do
        state <- newTVarIO SupervisorState
            { stateNextTurnId = 1
            , stateNextRequestId = 1
            , stateQueue = Seq.empty
            , stateTurns = Map.empty
            , stateActiveSessions = Set.empty
            , stateWorkers = Map.empty
            , statePendingInputs = Map.empty
            , stateEvents = Map.empty
            , stateNextSubscriberId = 1
            , stateSubscribers = Map.empty
            , stateClosed = False
            }
        dispatcherVar <- newEmptyMVar
        let supervisor = Supervisor
                { supervisorConfig = config
                , supervisorRunner = runner
                , supervisorBoundaryGuard = boundaryGuard
                , supervisorState = state
                , supervisorDispatcher = dispatcherVar
                }
        dispatcher <- async (restore (dispatcherLoop supervisor))
        putMVar dispatcherVar dispatcher
        pure supervisor

closeSupervisor :: Supervisor -> IO ()
closeSupervisor supervisor = mask \restore -> do
    now <- getCurrentTime
    (workers, cancellations, replies) <- atomically do
        state <- readTVar supervisor.supervisorState
        if state.stateClosed
            then pure ([], [], [])
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
                    (events, state') = foldr
                        (\slot (eventsAcc, stateAcc) ->
                            let (event, nextState) =
                                    appendEventToState
                                        supervisor.supervisorConfig
                                        now
                                        slot.turnSlotRecord.turnRecordBoundary
                                        "turn.cancelled"
                                        (Just
                                            slot.turnSlotRecord.turnRecordId)
                                        (Just
                                            slot.turnSlotRecord.turnRecordSessionId)
                                        (object [])
                                        stateAcc
                            in (event : eventsAcc, nextState))
                        ([], state
                            { stateClosed = True
                            , stateQueue = Seq.empty
                            , stateTurns = cancelledTurns
                            , statePendingInputs = Map.empty
                            })
                        activeSlots
                writeTVar supervisor.supervisorState state'
                forM_
                    (sortOn (.serverEventId) events)
                    (publishToSubscribers state')
                pure
                    ( Map.elems state.stateWorkers
                    , cancellationActions
                    , pendingReplies
                    )
    forM_ replies \reply ->
        atomically (void (tryPutTMVar reply (Left "server is shutting down")))
    let interruptInBand =
            forM_ cancellations \action ->
                void $
                    timeout cancelHookTimeoutMicros
                        (void (tryAny (restore action)))
        stopWorkers = do
            forM_ workers cancel
            forM_ workers (void . waitCatch)
        stopDispatcher = do
            dispatcher <- readMVar supervisor.supervisorDispatcher
            cancel dispatcher
            void (waitCatch dispatcher)
    -- A stuck provider interrupt must not skip structured worker teardown.
    (interruptInBand `finally` stopWorkers)
        `finally` stopDispatcher

submitTurn
    :: Supervisor
    -> TurnSpec
    -> IO (Either SubmitError TurnRecord)
submitTurn supervisor = enqueueTurn supervisor False

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

enqueueTurn
    :: Supervisor
    -> Bool
    -> TurnSpec
    -> IO (Either SubmitError TurnRecord)
enqueueTurn supervisor reservationHeld spec = do
    now <- getCurrentTime
    atomically do
        state <- readTVar supervisor.supervisorState
        let key = (spec.turnSpecBoundary, spec.turnSpecSessionId)
            reserved = Set.member key state.stateActiveSessions
        if state.stateClosed
            then pure (Left SubmitSupervisorClosed)
            else if reservationHeld && not reserved
                then pure (Left SubmitSessionBusy)
            else if
                (not reservationHeld && reserved)
                    || sessionBusy
                        spec.turnSpecBoundary
                        spec.turnSpecSessionId
                        state.stateTurns
                then pure (Left SubmitSessionBusy)
                else if Seq.length state.stateQueue
                    >= supervisor.supervisorConfig.supervisorMaxQueuedTurns
                    then pure (Left SubmitQueueFull)
                    else do
                        let turnId =
                                TurnId
                                    ("turn-"
                                        <> Text.pack
                                            (show state.stateNextTurnId))
                            record = TurnRecord
                                { turnRecordId = turnId
                                , turnRecordSessionId =
                                    spec.turnSpecSessionId
                                , turnRecordBoundary =
                                    spec.turnSpecBoundary
                                , turnRecordStatus = TurnQueued
                                , turnRecordCreatedAt = now
                                , turnRecordStartedAt = Nothing
                                , turnRecordFinishedAt = Nothing
                                , turnRecordError = Nothing
                                }
                            slot = TurnSlot
                                { turnSlotSpec = spec
                                , turnSlotRecord = record
                                , turnSlotCancel = Nothing
                                , turnSlotAgents = pure toJSONEmptyArray
                                }
                            withTurn = state
                                { stateNextTurnId =
                                    state.stateNextTurnId + 1
                                , stateQueue = state.stateQueue |> turnId
                                , stateTurns =
                                    Map.insert
                                        turnId
                                        slot
                                        (pruneTerminalTurns state.stateTurns)
                                }
                            (event, state') = appendEventToState
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

cancelTurn
    :: Supervisor
    -> GatewayBoundary
    -> TurnId
    -> IO (Either Text TurnRecord)
cancelTurn supervisor boundary turnId = mask \restore -> do
    now <- getCurrentTime
    outcome <- atomically do
        state <- readTVar supervisor.supervisorState
        case Map.lookup turnId state.stateTurns of
            Nothing -> pure (Left "turn not found")
            Just slot
                | slot.turnSlotRecord.turnRecordBoundary /= boundary ->
                    pure (Left "turn not found")
                | not
                    (isActiveStatus
                        slot.turnSlotRecord.turnRecordStatus) ->
                    pure (Right (slot.turnSlotRecord, Nothing, Nothing))
                | otherwise -> do
                    let record = cancelledRecord now slot.turnSlotRecord
                        slot' = slot { turnSlotRecord = record }
                        worker =
                            Map.lookup turnId state.stateWorkers
                        (inputs, retainedInputs) =
                            Map.partition
                                ((== turnId)
                                    . (.pendingInputView.humanRequestTurnId))
                                state.statePendingInputs
                        (event, state') = appendEventToState
                            supervisor.supervisorConfig
                            now
                            boundary
                            "turn.cancelled"
                            (Just turnId)
                            (Just record.turnRecordSessionId)
                            (object [])
                            state
                                { stateQueue =
                                    Seq.filter (/= turnId) state.stateQueue
                                , stateTurns =
                                    Map.insert turnId slot' state.stateTurns
                                , statePendingInputs = retainedInputs
                                , stateActiveSessions =
                                    case worker of
                                        Nothing ->
                                            Set.delete
                                                ( boundary
                                                , record.turnRecordSessionId
                                                )
                                                state.stateActiveSessions
                                        Just _ ->
                                            state.stateActiveSessions
                                }
                    forM_ (Map.elems inputs) \input ->
                        void
                            (tryPutTMVar
                                input.pendingInputReply
                                (Left "turn cancelled"))
                    writeTVar supervisor.supervisorState state'
                    publishToSubscribers state' event
                    pure
                        (Right
                            ( record
                            , slot.turnSlotCancel
                            , worker
                            ))
    case outcome of
        Left err -> pure (Left err)
        Right (record, cancellation, worker) -> do
            let stopWorker =
                    forM_ worker \running -> do
                        cancel running
                        void (waitCatch running)
                interruptInBand =
                    forM_ cancellation \action ->
                        void $
                            timeout cancelHookTimeoutMicros
                                (void (tryAny (restore action)))
            -- Preserve in-band cancellation when it is responsive, but always
            -- deliver and join the structured cancellation.
            interruptInBand `finally` stopWorker
            pure (Right record)

lookupTurn
    :: Supervisor
    -> GatewayBoundary
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
    -> GatewayBoundary
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
    -> GatewayBoundary
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
    -> GatewayBoundary
    -> Text
    -> IO value
    -> IO (Either SessionMutationError value)
withSessionMutation supervisor boundary sessionId action =
    mask \restore -> do
        acquired <- atomically do
            state <- readTVar supervisor.supervisorState
            let key = (boundary, sessionId)
            if state.stateClosed
                then pure (Left SessionMutationSupervisorClosed)
                else if
                    Set.member key state.stateActiveSessions
                        || sessionBusy boundary sessionId state.stateTurns
                    then pure (Left SessionMutationBusy)
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
    -> GatewayBoundary
    -> IO [HumanRequest]
listHumanRequests supervisor boundary =
    atomically do
        state <- readTVar supervisor.supervisorState
        pure $
            sortOn (Down . (.humanRequestCreatedAt))
                [ request
                | pending <- Map.elems state.statePendingInputs
                , let request = pending.pendingInputView
                , request.humanRequestBoundary == boundary
                ]

resolveHumanRequest
    :: Supervisor
    -> GatewayBoundary
    -> RequestId
    -> HumanResponse
    -> IO (Either Text HumanRequest)
resolveHumanRequest supervisor boundary requestId response = do
    now <- getCurrentTime
    atomically do
        state <- readTVar supervisor.supervisorState
        case Map.lookup requestId state.statePendingInputs of
            Nothing -> pure (Left "request not found")
            Just pending
                | pending.pendingInputView.humanRequestBoundary /= boundary ->
                    pure (Left "request not found")
                | not
                    (validHumanAnswer
                        pending.pendingInputView.humanRequestOptions
                        response.humanResponseDecision) ->
                    pure (Left "decision is not one of the allowed options")
                | otherwise -> do
                    accepted <-
                        tryPutTMVar pending.pendingInputReply (Right response)
                    if not accepted
                        then pure (Left "request has already been resolved")
                        else do
                            let request = pending.pendingInputView
                                turns' = Map.adjust
                                    (resumeWaitingTurn now)
                                    request.humanRequestTurnId
                                    state.stateTurns
                                (event, state') = appendEventToState
                                    supervisor.supervisorConfig
                                    now
                                    boundary
                                    "request.resolved"
                                    (Just request.humanRequestTurnId)
                                    (Just request.humanRequestSessionId)
                                    (object
                                        [ "requestId"
                                            .= requestId.unRequestId
                                        ])
                                    state
                                        { statePendingInputs =
                                            Map.delete
                                                requestId
                                                state.statePendingInputs
                                        , stateTurns = turns'
                                        }
                            writeTVar supervisor.supervisorState state'
                            publishToSubscribers state' event
                            pure (Right request)

lookupTurnAgents
    :: Supervisor
    -> GatewayBoundary
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
    -> GatewayBoundary
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
    -> GatewayBoundary
    -> Maybe Integer
    -> IO (EventSubscription (TBQueue ServerEvent))
subscribeEvents supervisor boundary lastEventId =
    atomically do
        state <- readTVar supervisor.supervisorState
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

reserveRunnable :: Supervisor -> STM (Maybe (TurnId, TurnSpec))
reserveRunnable supervisor = do
    state <- readTVar supervisor.supervisorState
    if state.stateClosed
        then pure Nothing
        else if Map.size state.stateWorkers
            >= supervisor.supervisorConfig.supervisorMaxConcurrentTurns
            then retry
            else case pickRunnable
                state.stateTurns
                state.stateActiveSessions
                state.stateQueue of
                    Nothing -> retry
                    Just (turnId, remaining) ->
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
                                                            }
                                                    }
                                                state.stateTurns
                                        , stateActiveSessions =
                                            Set.insert
                                                ( slot.turnSlotRecord.turnRecordBoundary
                                                , sessionId
                                                )
                                                state.stateActiveSessions
                                        }
                                writeTVar supervisor.supervisorState state'
                                pure (Just (turnId, slot.turnSlotSpec))

markTurnStarted
    :: Supervisor
    -> UTCTime
    -> TurnId
    -> TurnSpec
    -> STM ()
markTurnStarted supervisor now turnId spec =
    do
        state <- readTVar supervisor.supervisorState
        let withStarted = state
                { stateTurns = Map.adjust
                    (\slot ->
                        slot
                            { turnSlotRecord =
                                slot.turnSlotRecord
                                    { turnRecordStartedAt = Just now }
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
        atomically $
            markTurnStarted supervisor startedAt turnId spec
        outcome <-
            tryAny (supervisor.supervisorRunner control spec)
                :: IO (Either SomeException (Either Text ()))
        let result = case outcome of
                Left _ -> Left "agent turn terminated unexpectedly"
                Right value -> value
        finalizeTurn supervisor turnId spec result

    finish =
        atomically $
            modifyTVar' supervisor.supervisorState \state ->
                state
                    { stateWorkers =
                        Map.delete turnId state.stateWorkers
                    , stateActiveSessions =
                        Set.delete
                            ( spec.turnSpecBoundary
                            , spec.turnSpecSessionId
                            )
                            state.stateActiveSessions
                    }

finalizeTurn
    :: Supervisor
    -> TurnId
    -> TurnSpec
    -> Either Text ()
    -> IO ()
finalizeTurn supervisor turnId spec result = do
    now <- getCurrentTime
    atomically do
        state <- readTVar supervisor.supervisorState
        case Map.lookup turnId state.stateTurns of
            Nothing -> pure ()
            Just slot
                | isTerminalStatus
                    slot.turnSlotRecord.turnRecordStatus ->
                    pure ()
                | otherwise -> do
                    let (status, eventType, turnError) = case result of
                            Left err ->
                                ( TurnFailed
                                , "turn.failed"
                                , Just (boundedSupervisorText err)
                                )
                            Right () ->
                                (TurnCompleted, "turn.completed", Nothing)
                        record =
                            slot.turnSlotRecord
                                { turnRecordStatus = status
                                , turnRecordFinishedAt = Just now
                                , turnRecordError = turnError
                                }
                        (event, state') = appendEventToState
                            supervisor.supervisorConfig
                            now
                            spec.turnSpecBoundary
                            eventType
                            (Just turnId)
                            (Just spec.turnSpecSessionId)
                            (object ["error" .= turnError])
                            state
                                { stateTurns =
                                    Map.insert
                                        turnId
                                        slot
                                            { turnSlotRecord = record
                                            , turnSlotCancel = Nothing
                                            }
                                        state.stateTurns
                                }
                    writeTVar supervisor.supervisorState state'
                    publishToSubscribers state' event

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
    atomically $
        modifyTVar' supervisor.supervisorState \state ->
            state
                { stateTurns =
                    Map.adjust
                        (\slot ->
                            if isTerminalStatus
                                slot.turnSlotRecord.turnRecordStatus
                                then slot
                                else slot
                                    { turnSlotRecord =
                                        slot.turnSlotRecord
                                            { turnRecordStatus = TurnFailed
                                            , turnRecordFinishedAt = Just now
                                            , turnRecordError =
                                                Just
                                                    (boundedSupervisorText err)
                                            }
                                    , turnSlotCancel = Nothing
                                    })
                        turnId
                        state.stateTurns
                }

requestTurnInput
    :: Supervisor
    -> TurnId
    -> TurnSpec
    -> HumanRequestSpec
    -> IO (Either Text HumanResponse)
requestTurnInput supervisor turnId spec requestSpec = do
    now <- getCurrentTime
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
    requestResult <- atomically do
        state <- readTVar supervisor.supervisorState
        case Map.lookup turnId state.stateTurns of
            Nothing -> pure (Left "turn not found")
            Just slot
                | isTerminalStatus
                    slot.turnSlotRecord.turnRecordStatus ->
                    pure (Left "turn is no longer running")
                | otherwise -> do
                    let requestId =
                            RequestId
                                ("request-"
                                    <> Text.pack
                                        (show state.stateNextRequestId))
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
                        pending = PendingInput
                            { pendingInputView = request
                            , pendingInputReply = reply
                            }
                        record =
                            slot.turnSlotRecord
                                { turnRecordStatus = TurnWaitingForInput }
                        (event, state') = appendEventToState
                            supervisor.supervisorConfig
                            now
                            spec.turnSpecBoundary
                            "request.created"
                            (Just turnId)
                            (Just spec.turnSpecSessionId)
                            (toJSONRequest request)
                            state
                                { stateNextRequestId =
                                    state.stateNextRequestId + 1
                                , statePendingInputs =
                                    Map.insert
                                        requestId
                                        pending
                                        state.statePendingInputs
                                , stateTurns =
                                    Map.insert
                                        turnId
                                        slot { turnSlotRecord = record }
                                        state.stateTurns
                                }
                    writeTVar supervisor.supervisorState state'
                    publishToSubscribers state' event
                    pure (Right ())
    case requestResult of
        Left err -> pure (Left err)
        Right () -> atomically (takeTMVar reply)

setTurnCancellation :: TurnId -> IO () -> SupervisorState -> SupervisorState
setTurnCancellation turnId action state =
    state
        { stateTurns =
            Map.adjust
                (\slot ->
                    if isActiveStatus slot.turnSlotRecord.turnRecordStatus
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
    :: Map TurnId TurnSlot
    -> Set (GatewayBoundary, Text)
    -> Seq TurnId
    -> Maybe (TurnId, Seq TurnId)
pickRunnable turns activeSessions = go Seq.empty
  where
    go prefix queue = case Seq.viewl queue of
        EmptyL -> Nothing
        turnId :< rest ->
            case Map.lookup turnId turns of
                Nothing -> go prefix rest
                Just slot
                    | slot.turnSlotRecord.turnRecordStatus /= TurnQueued ->
                        go prefix rest
                    | Set.notMember
                        ( slot.turnSlotRecord.turnRecordBoundary
                        , slot.turnSlotRecord.turnRecordSessionId
                        )
                        activeSessions ->
                        Just (turnId, prefix <> rest)
                    | otherwise -> go (prefix |> turnId) rest

publishTurnEvent
    :: Supervisor
    -> GatewayBoundary
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
    -> GatewayBoundary
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
    :: GatewayBoundary
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
cancelSlot now slot =
    Map.insert
        slot.turnSlotRecord.turnRecordId
        slot
            { turnSlotRecord =
                cancelledRecord now slot.turnSlotRecord
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

validHumanAnswer :: [Text] -> Text -> Bool
validHumanAnswer options answer =
    null options || answer `elem` options

boundedSupervisorText :: Text -> Text
boundedSupervisorText value
    | Text.length value <= 16384 = value
    | otherwise = Text.take 16383 value <> "…"

cancelHookTimeoutMicros :: Int
cancelHookTimeoutMicros = 1000 * 1000

toJSONRequest :: HumanRequest -> Value
toJSONRequest request = object
    [ "request" .= request
    ]

toJSONEmptyArray :: Value
toJSONEmptyArray = toJSON ([] :: [Value])
