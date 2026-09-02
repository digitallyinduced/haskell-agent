-- | Bounded, process-local supervision for active HTTP turns.
--
-- Durable conversation history remains in PostgreSQL. This module owns only
-- live execution state, human-input waits, and the bounded SSE replay window.
module Agent.Server.Supervisor
    ( Supervisor
    , SupervisorConfig(..)
    , SubmitError(..)
    , TurnControl(..)
    , TurnRunner
    , newSupervisor
    , closeSupervisor
    , submitTurn
    , cancelTurn
    , lookupTurn
    , listTurns
    , sessionHasActiveTurn
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
    , TChan
    , TMVar
    , TVar
    , atomically
    , dupTChan
    , modifyTVar'
    , newBroadcastTChanIO
    , newEmptyTMVar
    , newTVarIO
    , putTMVar
    , readTVar
    , retry
    , takeTMVar
    , tryPutTMVar
    , writeTChan
    , writeTVar
    )
import Control.Exception.Safe
    ( SomeException
    , finally
    , mask
    , tryAny
    )
import Control.Monad (forM_, void)
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

data SupervisorConfig = SupervisorConfig
    { supervisorMaxConcurrentTurns :: !Int
    , supervisorMaxQueuedTurns :: !Int
    , supervisorEventReplayLimit :: !Int
    }
    deriving (Eq, Show)

data SubmitError
    = SubmitQueueFull
    | SubmitSessionBusy
    | SubmitSupervisorClosed
    deriving (Eq, Show)

data TurnControl = TurnControl
    { turnControlEmit :: !(Text -> Value -> IO ())
    , turnControlRequestInput
        :: !(HumanRequestSpec -> IO (Either Text Text))
    , turnControlRegisterCancel :: !(IO () -> IO ())
    , turnControlSetAgents :: !(Value -> IO ())
    }

type TurnRunner = TurnControl -> TurnSpec -> IO (Either Text ())

data TurnSlot = TurnSlot
    { turnSlotSpec :: !TurnSpec
    , turnSlotRecord :: !TurnRecord
    , turnSlotCancel :: !(Maybe (IO ()))
    , turnSlotAgents :: !Value
    }

data PendingInput = PendingInput
    { pendingInputView :: !HumanRequest
    , pendingInputReply :: !(TMVar (Either Text Text))
    }

data EventBuffer = EventBuffer
    { eventBufferReplay :: !(Seq ServerEvent)
    }

data SupervisorState = SupervisorState
    { stateNextTurnId :: !Integer
    , stateNextRequestId :: !Integer
    , stateNextEventId :: !Integer
    , stateQueue :: !(Seq TurnId)
    , stateTurns :: !(Map TurnId TurnSlot)
    , stateActiveSessions :: !(Set Text)
    , stateWorkers :: !(Map TurnId (Async ()))
    , statePendingInputs :: !(Map RequestId PendingInput)
    , stateEvents :: !(Map GatewayBoundary EventBuffer)
    , stateClosed :: !Bool
    }

data Supervisor = Supervisor
    { supervisorConfig :: !SupervisorConfig
    , supervisorRunner :: !TurnRunner
    , supervisorState :: !(TVar SupervisorState)
    , supervisorBroadcast :: !(TChan ServerEvent)
    , supervisorDispatcher :: !(MVar (Async ()))
    }

newSupervisor :: SupervisorConfig -> TurnRunner -> IO Supervisor
newSupervisor config runner
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
            , stateNextEventId = 1
            , stateQueue = Seq.empty
            , stateTurns = Map.empty
            , stateActiveSessions = Set.empty
            , stateWorkers = Map.empty
            , statePendingInputs = Map.empty
            , stateEvents = Map.empty
            , stateClosed = False
            }
        broadcast <- newBroadcastTChanIO
        dispatcherVar <- newEmptyMVar
        let supervisor = Supervisor
                { supervisorConfig = config
                , supervisorRunner = runner
                , supervisorState = state
                , supervisorBroadcast = broadcast
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
                forM_ events (writeTChan supervisor.supervisorBroadcast)
                pure
                    ( Map.elems state.stateWorkers
                    , cancellationActions
                    , pendingReplies
                    )
    forM_ replies \reply ->
        atomically (void (tryPutTMVar reply (Left "server is shutting down")))
    -- Ask the runtime to interrupt in-band before the structured cancellation.
    forM_ cancellations \action -> void (tryAny action)
    forM_ workers cancel
    forM_ workers (void . waitCatch)
    dispatcher <- readMVar supervisor.supervisorDispatcher
    cancel dispatcher
    void (restore (waitCatch dispatcher))

submitTurn
    :: Supervisor
    -> TurnSpec
    -> IO (Either SubmitError TurnRecord)
submitTurn supervisor spec = do
    now <- getCurrentTime
    atomically do
        state <- readTVar supervisor.supervisorState
        if state.stateClosed
            then pure (Left SubmitSupervisorClosed)
            else if sessionBusy spec.turnSpecSessionId state.stateTurns
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
                                , turnSlotAgents = toJSONEmptyArray
                                }
                            withTurn = state
                                { stateNextTurnId =
                                    state.stateNextTurnId + 1
                                , stateQueue = state.stateQueue |> turnId
                                , stateTurns =
                                    Map.insert turnId slot state.stateTurns
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
                        writeTChan supervisor.supervisorBroadcast event
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
                                }
                    forM_ (Map.elems inputs) \input ->
                        void
                            (tryPutTMVar
                                input.pendingInputReply
                                (Left "turn cancelled"))
                    writeTVar supervisor.supervisorState state'
                    writeTChan supervisor.supervisorBroadcast event
                    pure
                        (Right
                            ( record
                            , slot.turnSlotCancel
                            , Map.lookup turnId state.stateWorkers
                            ))
    case outcome of
        Left err -> pure (Left err)
        Right (record, cancellation, worker) -> do
            forM_ cancellation \action -> void (tryAny (restore action))
            forM_ worker cancel
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
            any
                (\slot ->
                    slot.turnSlotRecord.turnRecordBoundary == boundary
                        && slot.turnSlotRecord.turnRecordSessionId == sessionId
                        && isActiveStatus
                            slot.turnSlotRecord.turnRecordStatus)
                (Map.elems state.stateTurns)

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
    -> Text
    -> IO (Either Text HumanRequest)
resolveHumanRequest supervisor boundary requestId answer = do
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
                        answer) ->
                    pure (Left "decision is not one of the allowed options")
                | otherwise -> do
                    accepted <-
                        tryPutTMVar pending.pendingInputReply (Right answer)
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
                            writeTChan supervisor.supervisorBroadcast event
                            pure (Right request)

lookupTurnAgents
    :: Supervisor
    -> GatewayBoundary
    -> TurnId
    -> IO (Maybe Value)
lookupTurnAgents supervisor boundary turnId =
    atomically do
        state <- readTVar supervisor.supervisorState
        pure do
            slot <- Map.lookup turnId state.stateTurns
            if slot.turnSlotRecord.turnRecordBoundary == boundary
                then Just slot.turnSlotAgents
                else Nothing

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
        writeTChan supervisor.supervisorBroadcast event

subscribeEvents
    :: Supervisor
    -> GatewayBoundary
    -> Maybe Integer
    -> IO (EventSubscription (TChan ServerEvent))
subscribeEvents supervisor boundary lastEventId =
    atomically do
        state <- readTVar supervisor.supervisorState
        let buffer =
                Map.findWithDefault
                    (EventBuffer Seq.empty)
                    boundary
                    state.stateEvents
            replay = buffer.eventBufferReplay
            oldest = (.serverEventId) <$> Seq.lookup 0 replay
            resetRequired = case (lastEventId, oldest) of
                (Nothing, _) -> False
                (Just requested, Nothing) -> requested > 0
                (Just requested, Just firstId) -> requested < firstId - 1
            replayEvents = case lastEventId of
                Nothing -> []
                Just requested ->
                    filter
                        ((> requested) . (.serverEventId))
                        (toList replay)
        channel <- dupTChan supervisor.supervisorBroadcast
        pure EventSubscription
            { subscriptionReplay = replayEvents
            , subscriptionResetRequired = resetRequired
            , subscriptionChannel = channel
            }

dispatcherLoop :: Supervisor -> IO ()
dispatcherLoop supervisor = do
    next <- atomically (reserveRunnable supervisor)
    case next of
        Nothing -> pure ()
        Just (turnId, spec) -> do
            startedAt <- getCurrentTime
            atomically $
                markTurnStarted supervisor startedAt turnId spec
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
                                                slot { turnSlotRecord = record }
                                                state.stateTurns
                                        , stateActiveSessions =
                                            Set.insert
                                                sessionId
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
        writeTChan supervisor.supervisorBroadcast event

spawnTrackedWorker :: Supervisor -> TurnId -> TurnSpec -> IO ()
spawnTrackedWorker supervisor turnId spec = mask \restore -> do
    gate <- atomically newEmptyTMVar
    worker <- async do
        atomically (takeTMVar gate)
        restore (executeTurn supervisor turnId spec)
    atomically do
        modifyTVar' supervisor.supervisorState \state ->
            state
                { stateWorkers =
                    Map.insert turnId worker state.stateWorkers
                }
        putTMVar gate ()

executeTurn :: Supervisor -> TurnId -> TurnSpec -> IO ()
executeTurn supervisor turnId spec =
    finally run finish
  where
    control = TurnControl
        { turnControlEmit = \eventType value ->
            publishEvent
                supervisor
                spec.turnSpecBoundary
                eventType
                (Just turnId)
                (Just spec.turnSpecSessionId)
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

    run = do
        outcome <-
            tryAny (supervisor.supervisorRunner control spec)
                :: IO (Either SomeException (Either Text ()))
        case outcome of
            Left _ -> finalizeTurn
                supervisor turnId spec
                (Left "agent turn terminated unexpectedly")
            Right result -> finalizeTurn supervisor turnId spec result

    finish =
        atomically $
            modifyTVar' supervisor.supervisorState \state ->
                state
                    { stateWorkers =
                        Map.delete turnId state.stateWorkers
                    , stateActiveSessions =
                        Set.delete
                            spec.turnSpecSessionId
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
                            Left err -> (TurnFailed, "turn.failed", Just err)
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
                    writeTChan supervisor.supervisorBroadcast event

requestTurnInput
    :: Supervisor
    -> TurnId
    -> TurnSpec
    -> HumanRequestSpec
    -> IO (Either Text Text)
requestTurnInput supervisor turnId spec requestSpec = do
    now <- getCurrentTime
    reply <- atomically newEmptyTMVar
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
                                requestSpec.humanRequestSpecKind
                            , humanRequestPrompt =
                                requestSpec.humanRequestSpecPrompt
                            , humanRequestOptions =
                                requestSpec.humanRequestSpecOptions
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
                    writeTChan supervisor.supervisorBroadcast event
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

setTurnAgents :: TurnId -> Value -> SupervisorState -> SupervisorState
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
    -> Set Text
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
                        slot.turnSlotRecord.turnRecordSessionId
                        activeSessions ->
                        Just (turnId, prefix <> rest)
                    | otherwise -> go (prefix |> turnId) rest

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
            (EventBuffer Seq.empty)
            boundary
            state.stateEvents
    event = ServerEvent
        { serverEventId = state.stateNextEventId
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
    buffer' = EventBuffer { eventBufferReplay = replay }
    state' =
        state
            { stateNextEventId = state.stateNextEventId + 1
            , stateEvents =
                Map.insert boundary buffer' state.stateEvents
            }

trimReplay :: Int -> Seq a -> Seq a
trimReplay limit values =
    Seq.drop (max 0 (Seq.length values - limit)) values

sessionBusy :: Text -> Map TurnId TurnSlot -> Bool
sessionBusy sessionId =
    any
        (\slot ->
            slot.turnSlotRecord.turnRecordSessionId == sessionId
                && isActiveStatus slot.turnSlotRecord.turnRecordStatus)
        . Map.elems

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

toJSONRequest :: HumanRequest -> Value
toJSONRequest request = object
    [ "request" .= request
    ]

toJSONEmptyArray :: Value
toJSONEmptyArray = toJSON ([] :: [Value])
