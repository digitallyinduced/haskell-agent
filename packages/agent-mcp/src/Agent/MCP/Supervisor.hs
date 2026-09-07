module Agent.MCP.Supervisor
    ( newMcpSupervisor
    , newMcpSupervisorWith
    , acquireMcpFleet
    , acquireMcpFleetWithProgress
    , acquireMcpFleetProgressive
    , acquireMcpFleetWith
    , releaseMcpFleetLease
    , closeMcpSupervisor
    , restartMcpSupervisor
    ) where

import Agent.MCP.Client (exceptionSummary)
import Agent.Concurrent (forConcurrentlyBounded_)
import Agent.MCP.Fleet
    ( closeMcpFleet
    , mcpFleetStatuses
    , resolveEffectiveCwds
    , sameServerConfigs
    , startMcpFleetProgressiveHooks
    , startMcpFleetWithProgressHooks
    )
import Agent.MCP.Types
import Control.Concurrent.Async
    ( Async
    , cancel
    , waitCatch
    , withAsyncWithUnmask
    )
import Control.Concurrent.MVar
    ( modifyMVar
    , modifyMVar_
    , newMVar
    )
import Control.Concurrent.STM
    ( TMVar
    , atomically
    , newEmptyTMVarIO
    , readTMVar
    , tryPutTMVar
    )
import Control.Exception.Safe
    ( SomeException
    , mask
    , onException
    , tryAny
    )
import Control.Monad (unless, void)
import Data.IORef
    ( atomicModifyIORef'
    , newIORef
    )
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as Text

newMcpSupervisor :: IO McpSupervisor
newMcpSupervisor = newMcpSupervisorWith defaultMcpHostHooks

-- | A supervisor whose fleets share the given host hooks (elicitation UI,
-- client identity).
newMcpSupervisorWith :: McpHostHooks -> IO McpSupervisor
newMcpSupervisorWith hooks = do
    state <- newMVar McpSupervisorState
        { supervisorClosed = False
        , supervisorNextLeaseId = 1
        , supervisorEntries = []
        , supervisorPending = []
        }
    pure McpSupervisor
        { supervisorState = state
        , supervisorHooks = hooks
        }

acquireMcpFleet
    :: McpSupervisor
    -> [McpServerConfig]
    -> IO McpFleetLease
acquireMcpFleet supervisor configs =
    resolveEffectiveCwds configs >>= acquireMcpFleetWith supervisor False
        (startMcpFleetWithProgressHooks supervisor.supervisorHooks (const (pure ())))

acquireMcpFleetWithProgress
    :: McpSupervisor
    -> ([Text] -> IO ())
    -> [McpServerConfig]
    -> IO McpFleetLease
acquireMcpFleetWithProgress supervisor report configs =
    resolveEffectiveCwds configs >>= acquireMcpFleetWith supervisor False
        (startMcpFleetWithProgressHooks supervisor.supervisorHooks report)

acquireMcpFleetProgressive
    :: McpSupervisor
    -> ([McpServerStatus] -> IO ())
    -> [McpServerConfig]
    -> IO McpFleetLease
acquireMcpFleetProgressive supervisor report configs =
    resolveEffectiveCwds configs >>= acquireMcpFleetWith supervisor True
        (startMcpFleetProgressiveHooks supervisor.supervisorHooks report)

data McpAcquireRequest = McpAcquireRequest
    { acquireProgressive :: Bool
    , acquireStart :: [McpServerConfig] -> IO McpFleet
    , acquireConfigs :: [McpServerConfig]
    }

data McpAcquirePlan = McpAcquirePlan
    { acquireDecision :: McpAcquireDecision
    , acquireObsolete :: [McpFleet]
    }

acquireMcpFleetWith
    :: McpSupervisor
    -> Bool
    -> ([McpServerConfig] -> IO McpFleet)
    -> [McpServerConfig]
    -> IO McpFleetLease
acquireMcpFleetWith supervisor progressive start configs =
  mask \restore -> do
    let request = McpAcquireRequest
            { acquireProgressive = progressive
            , acquireStart = start
            , acquireConfigs = configs
            }
    plan <- prepareMcpAcquire supervisor request
    case plan.acquireDecision of
        UseReady acquired -> do
            mapM_ closeMcpFleet plan.acquireObsolete
            makeMcpFleetLease supervisor acquired
        WaitPending entryId completion -> do
            mapM_ closeMcpFleet plan.acquireObsolete
            waitPendingMcpAcquire supervisor entryId
                (restore (atomically (readTMVar completion)))
        StartPending entryId completion workerSlot -> do
            startPendingMcpAcquire
                supervisor
                request
                entryId
                completion
                workerSlot
                plan.acquireObsolete
                (restore . waitCatch)

prepareMcpAcquire
    :: McpSupervisor
    -> McpAcquireRequest
    -> IO McpAcquirePlan
prepareMcpAcquire supervisor request =
    modifyMVar supervisor.supervisorState \state ->
        if state.supervisorClosed
            then ioError (userError "MCP supervisor closed")
            else do
                (healthyEntries, failedIdle) <-
                    partitionMcpReusable state.supervisorEntries
                matching <- findMatchingMcpEntry request healthyEntries
                case matching of
                    Just entry -> do
                        let retained = entry
                                { supervisorEntryLeases =
                                    entry.supervisorEntryLeases + 1
                                }
                            otherIdle =
                                idleMcpEntriesExcept entry.supervisorEntryId
                                    healthyEntries
                            removed = failedIdle <> otherIdle
                        pure
                            ( state
                                { supervisorEntries =
                                    replaceMcpEntry retained
                                        (withoutMcpEntries removed
                                            healthyEntries)
                                }
                            , McpAcquirePlan
                                { acquireDecision =
                                    UseReady
                                        ( entry.supervisorEntryId
                                        , entry.supervisorEntryFleet
                                        )
                                , acquireObsolete =
                                    map (.supervisorEntryFleet) removed
                                }
                            )
                    Nothing ->
                        preparePendingMcpAcquire
                            request state healthyEntries failedIdle

preparePendingMcpAcquire
    :: McpAcquireRequest
    -> McpSupervisorState
    -> [McpSupervisorEntry]
    -> [McpSupervisorEntry]
    -> IO (McpSupervisorState, McpAcquirePlan)
preparePendingMcpAcquire request state healthyEntries failedIdle =
    case findPendingMcpAcquire request state.supervisorPending of
        Just pending -> do
            let retained = pending
                    { supervisorPendingLeases =
                        pending.supervisorPendingLeases + 1
                    }
            pure
                ( state
                    { supervisorEntries = healthyEntries
                    , supervisorPending =
                        replaceMcpPending retained state.supervisorPending
                    }
                , McpAcquirePlan
                    { acquireDecision =
                        WaitPending
                            pending.supervisorPendingId
                            pending.supervisorPendingResult
                    , acquireObsolete =
                        map (.supervisorEntryFleet) failedIdle
                    }
                )
        Nothing -> do
            completion <- newEmptyTMVarIO
            workerSlot <- newEmptyTMVarIO
            let pending = McpSupervisorPending
                    { supervisorPendingId = state.supervisorNextLeaseId
                    , supervisorPendingProgressive =
                        request.acquireProgressive
                    , supervisorPendingConfigs = request.acquireConfigs
                    , supervisorPendingResult = completion
                    , supervisorPendingWorker = workerSlot
                    , supervisorPendingLeases = 1
                    }
                healthyIdle = filter isIdleMcpEntry healthyEntries
                removed = failedIdle <> healthyIdle
            pure
                ( state
                    { supervisorNextLeaseId =
                        state.supervisorNextLeaseId + 1
                    , supervisorEntries =
                        filter (not . isIdleMcpEntry) healthyEntries
                    , supervisorPending =
                        pending : state.supervisorPending
                    }
                , McpAcquirePlan
                    { acquireDecision =
                        StartPending
                            pending.supervisorPendingId
                            completion
                            workerSlot
                    , acquireObsolete =
                        map (.supervisorEntryFleet) removed
                    }
                )

waitPendingMcpAcquire
    :: McpSupervisor
    -> Int
    -> IO (Either Text McpFleet)
    -> IO McpFleetLease
waitPendingMcpAcquire supervisor entryId waitForFleet = do
    result <-
        waitForFleet
            `onException` releaseSupervisorEntry supervisor entryId
    either
        (ioError . userError . Text.unpack)
        (\fleet -> makeMcpFleetLease supervisor (entryId, fleet))
        result

startPendingMcpAcquire
    :: McpSupervisor
    -> McpAcquireRequest
    -> Int
    -> TMVar (Either Text McpFleet)
    -> TMVar (Async (Either Text McpFleet))
    -> [McpFleet]
    -> ( Async (Either Text McpFleet)
         -> IO (Either SomeException (Either Text McpFleet))
       )
    -> IO McpFleetLease
startPendingMcpAcquire
    supervisor request entryId completion workerSlot obsolete waitForWorker = do
        outcome <-
            (withAsyncWithUnmask
                (\unmask ->
                    tryAny
                        (unmask
                            (request.acquireStart request.acquireConfigs))
                        >>= \case
                            Left exception ->
                                pure (Left (exceptionSummary exception))
                            Right fleet -> pure (Right fleet))
                \worker -> do
                    atomically $ void (tryPutTMVar workerSlot worker)
                    mapM_ closeMcpFleet obsolete
                    waitForWorker worker)
                `onException`
                    failMcpPending supervisor entryId completion
                        "MCP fleet startup cancelled"
        finishPendingMcpAcquire
            supervisor request entryId completion outcome

finishPendingMcpAcquire
    :: McpSupervisor
    -> McpAcquireRequest
    -> Int
    -> TMVar (Either Text McpFleet)
    -> Either SomeException (Either Text McpFleet)
    -> IO McpFleetLease
finishPendingMcpAcquire supervisor request entryId completion = \case
    Left exception -> do
        let err = exceptionSummary exception
        failMcpPending supervisor entryId completion err
        ioError (userError (Text.unpack err))
    Right (Left err) -> do
        failMcpPending supervisor entryId completion err
        ioError (userError (Text.unpack err))
    Right (Right fleet) -> do
        accepted <-
            publishMcpPending supervisor request entryId completion fleet
                `onException` closeMcpFleet fleet
        if accepted
            then makeMcpFleetLease supervisor (entryId, fleet)
            else do
                closeMcpFleet fleet
                ioError (userError "MCP supervisor closed")

makeMcpFleetLease
    :: McpSupervisor
    -> (Int, McpFleet)
    -> IO McpFleetLease
makeMcpFleetLease supervisor (entryId, fleet) = do
    released <- newIORef False
    let release =
            atomicModifyIORef' released (\done -> (True, done))
                >>= \alreadyReleased ->
                    unless alreadyReleased $
                        releaseSupervisorEntry supervisor entryId
    pure McpFleetLease
        { mcpLeaseFleet = fleet
        , mcpLeaseRelease = release
        }

findPendingMcpAcquire
    :: McpAcquireRequest
    -> [McpSupervisorPending]
    -> Maybe McpSupervisorPending
findPendingMcpAcquire request = go
  where
    go
        :: [McpSupervisorPending]
        -> Maybe McpSupervisorPending
    go [] = Nothing
    go (pending : rest)
        | pending.supervisorPendingProgressive
            == request.acquireProgressive
        , sameServerConfigs
            pending.supervisorPendingConfigs request.acquireConfigs =
            Just pending
        | otherwise = go rest

replaceMcpPending
    :: McpSupervisorPending
    -> [McpSupervisorPending]
    -> [McpSupervisorPending]
replaceMcpPending replacement =
    map \pending ->
        if pending.supervisorPendingId == replacement.supervisorPendingId
            then replacement
            else pending

failMcpPending
    :: McpSupervisor
    -> Int
    -> TMVar (Either Text McpFleet)
    -> Text
    -> IO ()
failMcpPending supervisor entryId completion err =
    modifyMVar_ supervisor.supervisorState \state -> do
        atomically $ void (tryPutTMVar completion (Left err))
        pure state
            { supervisorPending =
                filter
                    ((/= entryId) . (.supervisorPendingId))
                    state.supervisorPending
            }

publishMcpPending
    :: McpSupervisor
    -> McpAcquireRequest
    -> Int
    -> TMVar (Either Text McpFleet)
    -> McpFleet
    -> IO Bool
publishMcpPending supervisor request entryId completion fleet =
    modifyMVar supervisor.supervisorState \state ->
        case find
            ((== entryId) . (.supervisorPendingId))
            state.supervisorPending of
            Nothing -> do
                atomically $
                    void (tryPutTMVar completion
                        (Left "MCP supervisor closed"))
                pure (state, False)
            Just pending
                | state.supervisorClosed -> do
                    atomically $
                        void (tryPutTMVar completion
                            (Left "MCP supervisor closed"))
                    pure
                        ( state
                            { supervisorPending =
                                filter
                                    ((/= entryId) . (.supervisorPendingId))
                                    state.supervisorPending
                            }
                        , False
                        )
                | otherwise -> do
                    let entry = McpSupervisorEntry
                            { supervisorEntryId = entryId
                            , supervisorEntryProgressive =
                                request.acquireProgressive
                            , supervisorEntryConfigs =
                                request.acquireConfigs
                            , supervisorEntryFleet = fleet
                            , supervisorEntryLeases =
                                pending.supervisorPendingLeases
                            }
                    atomically $
                        void (tryPutTMVar completion (Right fleet))
                    pure
                        ( state
                            { supervisorEntries =
                                entry : state.supervisorEntries
                            , supervisorPending =
                                filter
                                    ((/= entryId) . (.supervisorPendingId))
                                    state.supervisorPending
                            }
                        , True
                        )

findMatchingMcpEntry
    :: McpAcquireRequest
    -> [McpSupervisorEntry]
    -> IO (Maybe McpSupervisorEntry)
findMatchingMcpEntry request = go
  where
    go :: [McpSupervisorEntry] -> IO (Maybe McpSupervisorEntry)
    go [] = pure Nothing
    go (entry : rest)
        | entry.supervisorEntryProgressive /= request.acquireProgressive
            || not
                (sameServerConfigs
                    entry.supervisorEntryConfigs request.acquireConfigs) =
                go rest
        | otherwise = do
            statuses <- mcpFleetStatuses entry.supervisorEntryFleet
            if all mcpStatusReusable statuses
                then pure (Just entry)
                else go rest

replaceMcpEntry
    :: McpSupervisorEntry
    -> [McpSupervisorEntry]
    -> [McpSupervisorEntry]
replaceMcpEntry replacement =
    map \entry ->
        if entry.supervisorEntryId == replacement.supervisorEntryId
            then replacement
            else entry

isIdleMcpEntry :: McpSupervisorEntry -> Bool
isIdleMcpEntry entry = entry.supervisorEntryLeases == 0

idleMcpEntriesExcept
    :: Int
    -> [McpSupervisorEntry]
    -> [McpSupervisorEntry]
idleMcpEntriesExcept retainedId =
    filter \entry ->
        isIdleMcpEntry entry && entry.supervisorEntryId /= retainedId

withoutMcpEntries
    :: [McpSupervisorEntry]
    -> [McpSupervisorEntry]
    -> [McpSupervisorEntry]
withoutMcpEntries removed =
    filter \entry ->
        all
            ((/= entry.supervisorEntryId) . (.supervisorEntryId))
            removed

partitionMcpReusable
    :: [McpSupervisorEntry]
    -> IO ([McpSupervisorEntry], [McpSupervisorEntry])
partitionMcpReusable = go [] []
  where
    go
        :: [McpSupervisorEntry]
        -> [McpSupervisorEntry]
        -> [McpSupervisorEntry]
        -> IO ([McpSupervisorEntry], [McpSupervisorEntry])
    go healthy failed [] =
        pure (reverse healthy, reverse failed)
    go healthy failed (entry : rest) = do
        statuses <- mcpFleetStatuses entry.supervisorEntryFleet
        let reusable =
                all
                    (\status -> case status.mcpStatusState of
                        McpFailed _ -> False
                        McpClosed -> False
                        _ -> True)
                    statuses
        if reusable || entry.supervisorEntryLeases > 0
            then go (entry : healthy) failed rest
            else go healthy (entry : failed) rest

mcpStatusReusable :: McpServerStatus -> Bool
mcpStatusReusable status = case status.mcpStatusState of
    McpFailed _ -> False
    McpClosed -> False
    _ -> True

releaseMcpFleetLease :: McpFleetLease -> IO ()
releaseMcpFleetLease = (.mcpLeaseRelease)

releaseSupervisorEntry :: McpSupervisor -> Int -> IO ()
releaseSupervisorEntry supervisor entryId = do
    modifyMVar_ supervisor.supervisorState \state ->
        pure state
            { supervisorEntries =
                map releaseOne state.supervisorEntries
            , supervisorPending =
                map releasePending state.supervisorPending
            }
  where
    releaseOne :: McpSupervisorEntry -> McpSupervisorEntry
    releaseOne entry
        | entry.supervisorEntryId == entryId =
            entry
                { supervisorEntryLeases =
                    max 0 (entry.supervisorEntryLeases - 1)
                }
        | otherwise = entry

    releasePending :: McpSupervisorPending -> McpSupervisorPending
    releasePending pending
        | pending.supervisorPendingId == entryId =
            pending
                { supervisorPendingLeases =
                    max 0 (pending.supervisorPendingLeases - 1)
                }
        | otherwise = pending

closeMcpSupervisor :: McpSupervisor -> IO ()
closeMcpSupervisor supervisor = do
    (fleets, pending) <-
        modifyMVar supervisor.supervisorState \state ->
            if state.supervisorClosed
                then pure (state, ([], []))
                else
                    pure
                        ( state
                            { supervisorClosed = True
                            , supervisorEntries = []
                            , supervisorPending = []
                            }
                        , ( map (.supervisorEntryFleet)
                                state.supervisorEntries
                          , state.supervisorPending
                          )
                        )
    atomically $
        mapM_
            (\entry ->
                void (tryPutTMVar entry.supervisorPendingResult
                    (Left "MCP supervisor closed")))
            pending
    forConcurrentlyBounded_ 8
        (\entry -> do
            worker <- atomically
                (readTMVar entry.supervisorPendingWorker)
            cancel worker
            void (waitCatch worker))
        pending
    forConcurrentlyBounded_ 4 closeMcpFleet fleets

-- | Discard every cached fleet while keeping the supervisor reusable. Callers
-- must ensure no active lease is in use; the native engine serializes this
-- operation in its idle loop.
restartMcpSupervisor :: McpSupervisor -> IO ()
restartMcpSupervisor supervisor = do
    (fleets, pending) <-
        modifyMVar supervisor.supervisorState \state ->
            if state.supervisorClosed
                then ioError (userError "MCP supervisor closed")
                else if
                    any ((> 0) . (.supervisorEntryLeases))
                        state.supervisorEntries
                        || any ((> 0) . (.supervisorPendingLeases))
                            state.supervisorPending
                then ioError (userError
                    "cannot restart MCP supervisor while a lease is active")
                else
                    pure
                        ( state
                            { supervisorEntries = []
                            , supervisorPending = []
                            }
                        , ( map (.supervisorEntryFleet)
                                state.supervisorEntries
                          , state.supervisorPending
                          )
                        )
    atomically $
        mapM_
            (\entry ->
                void (tryPutTMVar entry.supervisorPendingResult
                    (Left "MCP supervisor restarted")))
            pending
    forConcurrentlyBounded_ 8
        (\entry -> do
            worker <- atomically
                (readTMVar entry.supervisorPendingWorker)
            cancel worker
            void (waitCatch worker))
        pending
    forConcurrentlyBounded_ 4 closeMcpFleet fleets
