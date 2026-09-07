-- | Registry state, admission, and child supervision.
module Agent.Subagents.Registry.Internal.Core where


import Agent.Cancel
    ( newCancelFlag
    , requestCancel
    , resetCancel
    , waitCancel
    )
import Agent.Concurrent
    ( forConcurrentlyBounded_ )
import Agent.InterAgentMessage
    ( InterAgentMessage(..)
    , InterAgentMessageContent
    , InterAgentMessageType(..)
    , plainInterAgentContent
    )
import Agent.Loop (LoopError(..), LoopEvent, LoopResult(..))
import System.OsPath (OsPath)
import Agent.Subagents.Format (formatCompletionNotice, isFinalStatus)
import Agent.Subagents.Types
    ( RunSubagent
    , RootTurnId(..)
    , SubagentConfig(..)
    , SubagentId(..)
    , SubagentSpawnEnv(..)
    , SubagentStatus(..)
    )
import Control.Concurrent.Async (Async, async, cancel, race, waitCatch)
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Concurrent.STM
import Control.Exception.Safe
    ( SomeException
    , catchAny
    , mask
    , onException
    , throwIO
    , tryAny
    )
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Acquire (allocateAcquire, withAcquire)
import Data.IORef
import Data.List (groupBy, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (Down(..))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Numeric (showHex)
import Agent.Subagents.TaskPath
    ( TaskPath
    , joinTaskPath
    , taskPathRoot
    , taskPathText
    )
import Agent.Subagents.Registry.Internal.Types


newSubagentRegistry
    :: SubagentConfig
    -> OsPath
    -> RunSubagent
    -> (SubagentId -> LoopEvent -> IO ())
    -> IO SubagentRegistry
newSubagentRegistry config cwd run onEvent = do
    agents <- newTVarIO Map.empty
    paths <- newTVarIO Map.empty
    live <- newTVarIO 0
    rootTurnSpawnCounts <- newTVarIO Map.empty
    nextUpdateSeq <- newTVarIO 0
    waitCursors <- newTVarIO Map.empty
    activeWaits <- newTVarIO Map.empty
    closed <- newTVarIO False
    nextSubagentId <- newTVarIO 0
    nextRootTurnId <- newTVarIO 0
    abortedRootTurns <- newTVarIO Set.empty
    configVar <- newTVarIO config
        { maxConcurrent = max 1 config.maxConcurrent
        , maxSpawnedPerTurn = fmap (max 1) config.maxSpawnedPerTurn
        }
    lifecycle <- newMVar ()
    runRef <- newIORef run
    onCompleteRef <- newIORef (\_ _ -> pure ())
    onSettledRef <- newIORef (\_ _ -> pure ())
    pure SubagentRegistry
        { registryAgents = agents
        , registryPaths = paths
        , registryLiveCount = live
        , registryRootTurnSpawnCounts = rootTurnSpawnCounts
        , registryNextUpdateSeq = nextUpdateSeq
        , registryWaitCursors = waitCursors
        , registryActiveWaits = activeWaits
        , registryConfig = configVar
        , registryRunRef = runRef
        , registryOnEvent = onEvent
        , registryOnCompleteRef = onCompleteRef
        , registryOnSettledRef = onSettledRef
        , registryCwd = cwd
        , registryClosed = closed
        , registryNextSubagentId = nextSubagentId
        , registryNextRootTurnId = nextRootTurnId
        , registryAbortedRootTurns = abortedRootTurns
        , registryLifecycle = lifecycle
        }

setSubagentRunner :: SubagentRegistry -> RunSubagent -> IO ()
setSubagentRunner registry = writeIORef registry.registryRunRef

-- | Snapshot of the registry's current admission limits.
subagentConfig :: SubagentRegistry -> IO SubagentConfig
subagentConfig registry = readTVarIO registry.registryConfig

-- | Raise or lower the live concurrent-agent cap. Already-running agents
-- keep their slots; the new limit applies to the next spawn or follow-up.
setMaxConcurrent :: SubagentRegistry -> Int -> IO ()
setMaxConcurrent registry limit =
    atomically $
        modifyTVar' registry.registryConfig \config ->
            config { maxConcurrent = max 1 limit }

-- | Invoked when a child reaches a final status (completed / errored /
-- interrupted). Used to deliver parent-facing completion notices.
setSubagentOnComplete
    :: SubagentRegistry
    -> (SubagentId -> SubagentStatus -> IO ())
    -> IO ()
setSubagentOnComplete registry = writeIORef registry.registryOnCompleteRef

-- | Invoked synchronously after a child publishes a final turn status,
-- including completions routed directly to another subagent, and before the
-- child's supervisor can begin queued follow-up work. A first transition to
-- 'Closed' is reported after the supervisor has stopped; that callback runs
-- under the registry lifecycle lock and must not call lifecycle-mutating
-- registry operations.
setSubagentOnSettled
    :: SubagentRegistry
    -> (SubagentId -> SubagentStatus -> IO ())
    -> IO ()
setSubagentOnSettled registry = writeIORef registry.registryOnSettledRef

beginRootTurn :: SubagentRegistry -> IO RootTurnId
beginRootTurn registry = atomically do
    next <- readTVar registry.registryNextRootTurnId
    let rootTurnId = RootTurnId (next + 1)
    writeTVar registry.registryNextRootTurnId (next + 1)
    pure rootTurnId

closeSubagentRegistry :: SubagentRegistry -> IO ()
closeSubagentRegistry registry =
    withMVar registry.registryLifecycle \_ ->
        closeSubagentRegistryLocked registry

closeSubagentRegistryLocked :: SubagentRegistry -> IO ()
closeSubagentRegistryLocked registry = do
    records <- atomically do
        writeTVar registry.registryClosed True
        Map.elems <$> readTVar registry.registryAgents
    mapM_
        (forConcurrentlyBounded_ 8 (shutdownRecord registry))
        (groupBy
            (\left right -> left.recordDepth == right.recordDepth)
            (sortOn (Down . (.recordDepth)) records))

-- | Shut down live children and reopen the registry for a fresh session.
resetSubagentRegistry :: SubagentRegistry -> IO ()
resetSubagentRegistry registry =
    withMVar registry.registryLifecycle \_ -> do
        closeSubagentRegistryLocked registry
        atomically do
            writeTVar registry.registryAgents Map.empty
            writeTVar registry.registryPaths Map.empty
            writeTVar registry.registryLiveCount 0
            writeTVar registry.registryRootTurnSpawnCounts Map.empty
            writeTVar registry.registryNextUpdateSeq 0
            writeTVar registry.registryWaitCursors Map.empty
            writeTVar registry.registryActiveWaits Map.empty
            writeTVar registry.registryAbortedRootTurns Set.empty
            writeTVar registry.registryClosed False

spawnSubagent
    :: SubagentRegistry
    -> Maybe SubagentId
    -> Int
    -> Text
    -> Maybe Text
    -> IO (Either Text SubagentId)
spawnSubagent registry =
    spawnSubagentWithCwd registry registry.registryCwd

spawnSubagentWithCwd
    :: SubagentRegistry
    -> OsPath
    -> Maybe SubagentId
    -> Int
    -> Text
    -> Maybe Text
    -> IO (Either Text SubagentId)
spawnSubagentWithCwd registry =
    spawnSubagentWithCwdForTurn registry Nothing

spawnSubagentWithCwdForTurn
    :: SubagentRegistry
    -> Maybe RootTurnId
    -> OsPath
    -> Maybe SubagentId
    -> Int
    -> Text
    -> Maybe Text
    -> IO (Either Text SubagentId)
spawnSubagentWithCwdForTurn registry rootTurnId childCwd =
    spawnSubagentWithCwdPreparedForTurn
        registry rootTurnId childCwd (\_ -> pure mempty)

-- | Run host preparation after admission but before the supervisor starts.
spawnSubagentWithCwdPrepared
    :: SubagentRegistry
    -> OsPath
    -> (SubagentId -> IO SubagentLease)
    -> Maybe SubagentId
    -> Int
    -> Text
    -> Maybe Text
    -> IO (Either Text SubagentId)
spawnSubagentWithCwdPrepared registry =
    spawnSubagentWithCwdPreparedForTurn registry Nothing

spawnSubagentWithCwdPreparedForTurn
    :: SubagentRegistry
    -> Maybe RootTurnId
    -> OsPath
    -> (SubagentId -> IO SubagentLease)
    -> Maybe SubagentId
    -> Int
    -> Text
    -> Maybe Text
    -> IO (Either Text SubagentId)
spawnSubagentWithCwdPreparedForTurn
        registry rootTurnId childCwd beforeStart
        parentId parentDepth message nickname = do
    agentId <- newSubagentId registry
    fmap (fmap fst) $
        spawnSubagentAtWithIdPreparedForTurn
            registry rootTurnId childCwd beforeStart agentId
            parentId taskPathRoot parentDepth (taskNameForAgentId agentId)
                (plainInterAgentContent message) nickname

-- | Spawn with an explicit parent path and task_name (Codex multi-agent v2).
spawnSubagentAt
    :: SubagentRegistry
    -> Maybe SubagentId
    -> TaskPath
    -> Int
    -> Text
    -> InterAgentMessageContent
    -> Maybe Text
    -> IO (Either Text (SubagentId, TaskPath))
spawnSubagentAt registry =
    spawnSubagentAtForTurn registry Nothing

spawnSubagentAtForTurn
    :: SubagentRegistry
    -> Maybe RootTurnId
    -> Maybe SubagentId
    -> TaskPath
    -> Int
    -> Text
    -> InterAgentMessageContent
    -> Maybe Text
    -> IO (Either Text (SubagentId, TaskPath))
spawnSubagentAtForTurn registry rootTurnId =
    spawnSubagentAtPreparedForTurn registry rootTurnId (\_ -> pure mempty)

spawnSubagentAtPreparedForTurn
    :: SubagentRegistry
    -> Maybe RootTurnId
    -> (SubagentId -> IO SubagentLease)
    -> Maybe SubagentId
    -> TaskPath
    -> Int
    -> Text
    -> InterAgentMessageContent
    -> Maybe Text
    -> IO (Either Text (SubagentId, TaskPath))
spawnSubagentAtPreparedForTurn registry rootTurnId beforeStart =
    spawnSubagentAtWithCwdPreparedForTurn
        registry rootTurnId registry.registryCwd beforeStart

spawnSubagentAtWithCwdPrepared
    :: SubagentRegistry
    -> OsPath
    -> (SubagentId -> IO SubagentLease)
    -> Maybe SubagentId
    -> TaskPath
    -> Int
    -> Text
    -> InterAgentMessageContent
    -> Maybe Text
    -> IO (Either Text (SubagentId, TaskPath))
spawnSubagentAtWithCwdPrepared registry =
    spawnSubagentAtWithCwdPreparedForTurn registry Nothing

spawnSubagentAtWithCwdPreparedForTurn
    :: SubagentRegistry
    -> Maybe RootTurnId
    -> OsPath
    -> (SubagentId -> IO SubagentLease)
    -> Maybe SubagentId
    -> TaskPath
    -> Int
    -> Text
    -> InterAgentMessageContent
    -> Maybe Text
    -> IO (Either Text (SubagentId, TaskPath))
spawnSubagentAtWithCwdPreparedForTurn
        registry rootTurnId childCwd beforeStart
        parentId parentPath parentDepth taskName content nickname = do
    agentId <- newSubagentId registry
    spawnSubagentAtWithIdPreparedForTurn
        registry rootTurnId childCwd beforeStart agentId
        parentId parentPath parentDepth taskName content nickname

spawnSubagentAtWithIdPreparedForTurn
    :: SubagentRegistry
    -> Maybe RootTurnId
    -> OsPath
    -> (SubagentId -> IO SubagentLease)
    -> SubagentId
    -> Maybe SubagentId
    -> TaskPath
    -> Int
    -> Text
    -> InterAgentMessageContent
    -> Maybe Text
    -> IO (Either Text (SubagentId, TaskPath))
spawnSubagentAtWithIdPreparedForTurn
        registry rootTurnId childCwd beforeStart agentId
        parentId requestedParentPath requestedParentDepth taskName content nickname = do
    cancelFlag <- newCancelFlag
    mailbox <- newTQueueIO
    asyncVar <- newTVarIO Nothing
    previousVar <- newTVarIO Nothing
    lastUpdateVar <- newTVarIO Nothing
    admitted <- withMVar registry.registryLifecycle \_ -> atomically do
        closed <- readTVar registry.registryClosed
        aborted <- isRootTurnAborted registry rootTurnId
        if closed
            then pure (Left "Subagent registry is closed.")
            else if aborted
                then pure (Left "Root turn was aborted.")
                else do
                    agents <- readTVar registry.registryAgents
                    parent <- resolveParentSTM
                        agents parentId requestedParentPath requestedParentDepth
                    case parent of
                        Left err -> pure (Left err)
                        Right (parentPath, nextDepth) -> do
                            config <- readTVar registry.registryConfig
                            budgetExceeded <- case
                                    (rootTurnId, config.maxSpawnedPerTurn) of
                                (Just turnId, Just limit) -> do
                                    counts <- readTVar
                                        registry.registryRootTurnSpawnCounts
                                    pure $
                                        if Map.findWithDefault 0 turnId counts >= limit
                                            then Just limit
                                            else Nothing
                                _ -> pure Nothing
                            case (budgetExceeded, config.maxDepth) of
                                (Just limit, _) -> pure $ Left
                                    ("Subagent budget reached for this turn (maximum "
                                        <> Text.pack (show limit)
                                        <> "). Solve the remaining task yourself.")
                                (_, Just limit) | nextDepth > limit ->
                                    pure $ Left
                                        ("Agent depth limit reached (maximum depth "
                                            <> Text.pack (show limit)
                                            <> "). Solve the task yourself.")
                                _ -> case joinTaskPath parentPath taskName of
                                    Left err -> pure (Left err)
                                    Right childPath -> do
                                        paths <- readTVar registry.registryPaths
                                        if Map.member childPath paths
                                            then pure $ Left $
                                                "task path already in use: "
                                                    <> taskPathText childPath
                                            else do
                                                live <- readTVar registry.registryLiveCount
                                                if live >= config.maxConcurrent
                                                    then pure $ Left $
                                                        "Concurrent subagent limit reached: "
                                                            <> Text.pack
                                                                (show config.maxConcurrent)
                                                            <> " agents are already active."
                                                    else do
                                                        let work = SubagentWork
                                                                { workRootTurnId = rootTurnId
                                                                , workMessage = InterAgentMessage
                                                                    { messageAuthor =
                                                                        taskPathText parentPath
                                                                    , messageRecipient =
                                                                        taskPathText childPath
                                                                    , messageType = NewTaskMessage
                                                                    , messageContent = content
                                                                    }
                                                                }
                                                        phaseVar <- newTVar
                                                            (AgentPending work)
                                                        let record = SubagentRecord
                                                                { recordId = agentId
                                                                , recordParent = parentId
                                                                , recordDepth = nextDepth
                                                                , recordNickname = nickname
                                                                , recordPhase = phaseVar
                                                                , recordCancel = cancelFlag
                                                                , recordMailbox = mailbox
                                                                , recordAsync = asyncVar
                                                                , recordPreviousResponseId = previousVar
                                                                , recordLastUpdate = lastUpdateVar
                                                                , recordTaskPath = childPath
                                                                , recordCwd = childCwd
                                                                }
                                                        modifyTVar'
                                                            registry.registryLiveCount (+ 1)
                                                        case rootTurnId of
                                                            Just turnId ->
                                                                modifyTVar'
                                                                    registry.registryRootTurnSpawnCounts
                                                                    (Map.insertWith (+) turnId 1)
                                                            Nothing -> pure ()
                                                        writeTVar registry.registryAgents
                                                            (Map.insert agentId record agents)
                                                        writeTVar registry.registryPaths
                                                            (Map.insert childPath agentId paths)
                                                        pure (Right record)
    case admitted of
        Left err -> pure (Left err)
        Right record -> mask \restore ->
            (do
                prepared <- tryAny (restore (beforeStart agentId))
                case prepared of
                    Left (exc :: SomeException) -> do
                        rollbackAdmission registry record
                        pure $ Left $
                            "Failed to prepare subagent: " <> Text.pack (show exc)
                    Right lease ->
                        startPrepared restore record lease)
                `onException` rollbackAdmission registry record
  where
    startPrepared restore record lease = do
        started <-
            restore
                (withMVar registry.registryLifecycle \_ ->
                    startRecordSupervisor registry record lease)
                `onException` shutdownRecord registry record
        case started of
            Left err -> do
                rollbackAdmission registry record
                pure (Left err)
            Right () ->
                pure (Right (agentId, record.recordTaskPath))

resolveParentSTM
    :: Map SubagentId SubagentRecord
    -> Maybe SubagentId
    -> TaskPath
    -> Int
    -> STM (Either Text (TaskPath, Int))
resolveParentSTM _ Nothing requestedPath requestedDepth
    | requestedPath == taskPathRoot && requestedDepth == 0 =
        pure (Right (taskPathRoot, 1))
    | otherwise =
        pure (Left "root spawn has inconsistent parent context")
resolveParentSTM agents (Just parentId) _ _ =
    case Map.lookup parentId agents of
        Nothing -> pure (Left "Parent subagent is closed or missing.")
        Just parent -> do
            status <- phaseStatus <$> readTVar parent.recordPhase
            if status == Closed || status == NotFound
                then pure (Left "Parent subagent is closed or missing.")
                else pure (Right (parent.recordTaskPath, parent.recordDepth + 1))

taskNameForAgentId :: SubagentId -> Text
taskNameForAgentId agentId =
    "a" <> Text.filter (/= '-') agentId.unSubagentId

rollbackAdmission :: SubagentRegistry -> SubagentRecord -> IO ()
rollbackAdmission registry record = do
    atomically do
        modifyTVar' registry.registryAgents (Map.delete record.recordId)
        modifyTVar' registry.registryPaths $
            deleteOwnedPath record.recordTaskPath record.recordId
        releaseSlotSTM registry record
        writeTVar record.recordPhase AgentClosed
    stopRecordSupervisor record

deleteOwnedPath :: TaskPath -> SubagentId -> Map TaskPath SubagentId -> Map TaskPath SubagentId
deleteOwnedPath key expected mappings =
    case Map.lookup key mappings of
        Just actual | actual == expected -> Map.delete key mappings
        _ -> mappings

runSupervisor :: SubagentRegistry -> SubagentRecord -> IO ()
runSupervisor registry record = awaitWork
  where
    awaitWork =
        atomically (takeStartedWork record) >>= \case
            Nothing -> pure ()
            Just work -> do
                resetCancel record.recordCancel
                runWork work

    runWork work = do
        let onEvent = registry.registryOnEvent record.recordId
            env = SubagentSpawnEnv
                { subId = record.recordId
                , subDepth = record.recordDepth
                , subParentId = record.recordParent
                , subCwd = record.recordCwd
                , subCancel = record.recordCancel
                , subRootTurnId = work.workRootTurnId
                }
        previous <- atomically $ readTVar record.recordPreviousResponseId
        run <- readIORef registry.registryRunRef
        raced <- race
            (waitCancel record.recordCancel)
            (tryAny (run env previous work.workMessage onEvent))
        let result = case raced of
                Left () -> Right (Left (LoopCancelled []))
                Right completed -> completed
            status = case result of
                Left (exc :: SomeException) ->
                    Errored (Text.pack (show exc))
                Right (Left LoopCancelled{}) -> Interrupted
                Right (Left err) -> Errored (Text.pack (show err))
                Right (Right loopResult) -> Completed loopResult.finalText
        case result of
            Right (Right loopResult) ->
                atomically $
                    writeTVar record.recordPreviousResponseId
                        (Just loopResult.finalResponseId)
            _ -> pure ()
        atomically (nextSupervisorStep registry record) >>= \case
            SupervisorStop -> pure ()
            SupervisorIdle -> awaitWork
            SupervisorMessage nextWork -> do
                resetCancel record.recordCancel
                runWork nextWork
            SupervisorComplete -> do
                notifyRoot <- atomically $
                    publishCompletionSTM registry record status
                whenIO notifyRoot do
                    _ <- tryAny $
                        notifyComplete
                            registry record.recordId work.workRootTurnId status
                    pure ()
                notifySettled registry record.recordId status
                atomically (finishSupervisorStep registry record status) >>= \case
                    SupervisorStop -> pure ()
                    SupervisorIdle -> awaitWork
                    SupervisorMessage nextWork -> do
                        resetCancel record.recordCancel
                        runWork nextWork
                    SupervisorComplete -> pure ()

takeStartedWork
    :: SubagentRecord
    -> STM (Maybe SubagentWork)
takeStartedWork record = do
    readTVar record.recordPhase >>= \case
        AgentClosed -> pure Nothing
        AgentPending work -> do
            writeTVar record.recordPhase (AgentRunning work.workRootTurnId)
            pure (Just work)
        AgentIdle{} -> retry
        AgentRunning{} -> retry
        AgentInterrupting{} -> retry

publishCompletionSTM :: SubagentRegistry -> SubagentRecord -> SubagentStatus -> STM Bool
publishCompletionSTM registry record status = do
    nextSeq <- readTVar registry.registryNextUpdateSeq
    let updateSeq = nextSeq + 1
    writeTVar registry.registryNextUpdateSeq updateSeq
    writeTVar record.recordLastUpdate (Just (updateSeq, status))
    routeCompletionSTM registry record status

-- | Publish a status for a turn settled administratively before its
-- supervisor starts.  This wakes untargeted waiters just like a normal
-- completion, but deliberately does not route a completion message to the
-- parent (there was no model turn to report).
publishDirectUpdateSTM
    :: SubagentRegistry
    -> SubagentRecord
    -> SubagentStatus
    -> STM ()
publishDirectUpdateSTM registry record status = do
    nextSeq <- readTVar registry.registryNextUpdateSeq
    let updateSeq = nextSeq + 1
    writeTVar registry.registryNextUpdateSeq updateSeq
    writeTVar record.recordLastUpdate (Just (updateSeq, status))

routeCompletionSTM :: SubagentRegistry -> SubagentRecord -> SubagentStatus -> STM Bool
routeCompletionSTM registry record status =
    case record.recordParent of
        Nothing -> pure True
        Just parentId -> do
            awaited <-
                completionIsAwaitedSTM registry (Just parentId) record.recordId
            agents <- readTVar registry.registryAgents
            if awaited
                then pure False
                else case Map.lookup parentId agents of
                    Nothing -> pure True
                    Just parent -> routeToParent parent
  where
    routeToParent parent = do
        parentStatus <- phaseStatus <$> readTVar parent.recordPhase
        if parentStatus == Running || parentStatus == Pending
            then do
                rootTurnId <- phaseRootTurnId <$> readTVar record.recordPhase
                writeTQueue parent.recordMailbox
                    SubagentWork
                        { workRootTurnId = rootTurnId
                        , workMessage = completionMessage record parent status
                        }
                pure False
            else pure True

completionIsAwaitedSTM
    :: SubagentRegistry
    -> Maybe SubagentId
    -> SubagentId
    -> STM Bool
completionIsAwaitedSTM registry caller childId = do
    waits <- readTVar registry.registryActiveWaits
    pure $ case Map.lookup caller waits of
        Nothing -> False
        Just [] -> True
        Just targets -> childId `elem` targets

completionMessage :: SubagentRecord -> SubagentRecord -> SubagentStatus -> InterAgentMessage
completionMessage child parent status =
    InterAgentMessage
        (taskPathText child.recordTaskPath)
        (taskPathText parent.recordTaskPath)
        QueuedMessage
        (plainInterAgentContent (formatCompletionNotice child.recordId status))

startRecordSupervisor
    :: SubagentRegistry
    -> SubagentRecord
    -> SubagentLease
    -> IO (Either Text ())
startRecordSupervisor registry record lease =
    mask \restore -> do
        canStart <- atomically do
            closed <- readTVar registry.registryClosed
            phase <- readTVar record.recordPhase
            aborted <- isRootTurnAborted registry (phaseRootTurnId phase)
            agents <- readTVar registry.registryAgents
            paths <- readTVar registry.registryPaths
            current <- readTVar record.recordAsync
            pure $
                not closed
                    && not aborted
                    && phaseStatus phase /= Closed
                    && Map.member record.recordId agents
                    && maybe True (== record.recordId)
                        (Map.lookup record.recordTaskPath paths)
                    && maybe True (const False) current
        if not canStart
            then do
                releaseSubagentLease lease
                pure (Left "Subagent closed before its supervisor started.")
            else do
                ready <- newEmptyTMVarIO
                started <- tryAny $ async $
                    supervisorAction ready
                case started of
                    Left (exception :: SomeException) -> do
                        releaseSubagentLease lease
                        pure (Left ("Failed to start subagent: " <> Text.pack (show exception)))
                    Right supervisor -> do
                        atomically $ writeTVar record.recordAsync (Just supervisor)
                        ownership <- restore (atomically (takeTMVar ready))
                            `onException` stopRecordSupervisor record
                        case ownership of
                            Left err -> do
                                stopRecordSupervisor record
                                pure (Left err)
                            Right () -> pure (Right ())
  where
    supervisorAction ready =
        mask \restoreSupervisor ->
            (runResourceT do
                case lease of
                    SubagentLease acquire -> void (allocateAcquire acquire)
                liftIO $ atomically $ putTMVar ready (Right ())
                liftIO $ restoreSupervisor (runSupervisor registry record))
            `catchAny` \exception -> do
                atomically $ void $ tryPutTMVar ready $ Left $
                    "Failed to start subagent: " <> Text.pack (show exception)
                throwIO exception

releaseSubagentLease :: SubagentLease -> IO ()
releaseSubagentLease (SubagentLease acquire) =
    withAcquire acquire (const (pure ()))

stopAsync :: Async () -> IO ()
stopAsync supervisor = do
    cancel supervisor
    _ <- waitCatch supervisor
    pure ()

takeRecordSupervisor :: SubagentRecord -> IO (Maybe (Async ()))
takeRecordSupervisor record =
    atomically do
        current <- readTVar record.recordAsync
        writeTVar record.recordAsync Nothing
        pure current

stopRecordSupervisor :: SubagentRecord -> IO ()
stopRecordSupervisor record = do
    supervisor <- takeRecordSupervisor record
    mapM_ stopAsync supervisor

data SupervisorStep
    = SupervisorStop
    | SupervisorIdle
    | SupervisorComplete
    | SupervisorMessage !SubagentWork

nextSupervisorStep
    :: SubagentRegistry
    -> SubagentRecord
    -> STM SupervisorStep
nextSupervisorStep registry record = do
    supervisorStep registry record (pure SupervisorComplete)

finishSupervisorStep
    :: SubagentRegistry
    -> SubagentRecord
    -> SubagentStatus
    -> STM SupervisorStep
finishSupervisorStep registry record status = do
    supervisorStep registry record do
        transitionToIdleSTM registry record status
        pure SupervisorIdle

supervisorStep
    :: SubagentRegistry
    -> SubagentRecord
    -> STM SupervisorStep
    -> STM SupervisorStep
supervisorStep registry record onIdle =
    readTVar record.recordPhase >>= \case
        AgentClosed -> release SupervisorStop
        AgentInterrupting{} -> do
            transitionToIdleSTM registry record Interrupted
            pure SupervisorIdle
        AgentRunning{} ->
            tryReadTQueue record.recordMailbox >>= \case
                Nothing -> onIdle
                Just work -> do
                    writeTVar record.recordPhase
                        (AgentRunning work.workRootTurnId)
                    pure (SupervisorMessage work)
        AgentPending{} -> retry
        AgentIdle{} -> pure SupervisorIdle
  where
    release step = do
        releaseSlotSTM registry record
        pure step

transitionToIdleSTM
    :: SubagentRegistry
    -> SubagentRecord
    -> SubagentStatus
    -> STM ()
transitionToIdleSTM registry record status = do
    releaseSlotSTM registry record
    writeTVar record.recordPhase (AgentIdle status Nothing)

notifyComplete
    :: SubagentRegistry
    -> SubagentId
    -> Maybe RootTurnId
    -> SubagentStatus
    -> IO ()
notifyComplete registry agentId rootTurnId status
    | isFinalStatus status && status /= Closed && status /= NotFound = do
        shouldNotify <- atomically do
            closed <- readTVar registry.registryClosed
            agents <- readTVar registry.registryAgents
            case Map.lookup agentId agents of
                Nothing -> pure False
                Just record -> do
                    phase <- readTVar record.recordPhase
                    aborted <- isRootTurnAborted registry rootTurnId
                    pure $
                        not closed
                            && not aborted
                            && phaseStatus phase == Running
                            && phaseRootTurnId phase == rootTurnId
        whenIO shouldNotify do
            onComplete <- readIORef registry.registryOnCompleteRef
            onComplete agentId status
    | otherwise = pure ()

notifySettled
    :: SubagentRegistry
    -> SubagentId
    -> SubagentStatus
    -> IO ()
notifySettled registry agentId status
    | isFinalStatus status && status /= NotFound = do
        onSettled <- readIORef registry.registryOnSettledRef
        void $ tryAny $ onSettled agentId status
    | otherwise = pure ()

releaseSlotSTM :: SubagentRegistry -> SubagentRecord -> STM ()
releaseSlotSTM registry record = do
    phase <- readTVar record.recordPhase
    whenSTM (phaseHoldsSlot phase) do
        live <- readTVar registry.registryLiveCount
        writeTVar registry.registryLiveCount (max 0 (live - 1))

whenSTM :: Bool -> STM () -> STM ()
whenSTM True action = action
whenSTM False _ = pure ()

scheduleIdleWork
    :: SubagentRegistry
    -> SubagentRecord
    -> SubagentWork
    -> STM (Either Text ())
scheduleIdleWork registry record work = do
    closed <- readTVar registry.registryClosed
    if closed
        then pure (Left "Subagent registry is closed.")
        else do
            phase <- readTVar record.recordPhase
            case phase of
                AgentIdle{} -> do
                    live <- readTVar registry.registryLiveCount
                    config <- readTVar registry.registryConfig
                    if live >= config.maxConcurrent
                        then pure $ Left $
                            "Concurrent subagent limit reached: "
                                <> Text.pack (show config.maxConcurrent)
                                <> " agents are already active."
                        else do
                            modifyTVar' registry.registryLiveCount (+ 1)
                            writeTVar record.recordPhase (AgentPending work)
                            pure (Right ())
                AgentClosed -> pure (Left "agent is closed")
                AgentPending{} ->
                    pure (Left "Subagent already has pending work.")
                AgentRunning{} ->
                    pure (Left "Subagent is already running.")
                AgentInterrupting{} ->
                    pure (Left "Subagent is still interrupting.")

descendants :: Map SubagentId SubagentRecord -> SubagentId -> [SubagentRecord]
descendants agents parentId =
    let kids = [r | r <- Map.elems agents, r.recordParent == Just parentId]
    in concatMap
        (\kid -> descendants agents kid.recordId <> [kid])
        kids

shutdownRecord :: SubagentRegistry -> SubagentRecord -> IO ()
shutdownRecord registry record = do
    requestCancel record.recordCancel
    transitioned <- atomically do
        phase <- readTVar record.recordPhase
        releaseSlotSTM registry record
        writeTVar record.recordPhase AgentClosed
        void $ flushTQueue record.recordMailbox
        pure $ case phase of
            AgentClosed -> False
            _ -> True
    stopRecordSupervisor record
    whenIO transitioned $
        notifySettled registry record.recordId Closed

whenIO :: Bool -> IO () -> IO ()
whenIO True action = action
whenIO False _ = pure ()

isRootTurnAborted :: SubagentRegistry -> Maybe RootTurnId -> STM Bool
isRootTurnAborted _ Nothing = pure False
isRootTurnAborted registry (Just rootTurnId) =
    Set.member rootTurnId <$> readTVar registry.registryAbortedRootTurns

newSubagentId :: SubagentRegistry -> IO SubagentId
newSubagentId registry = do
    n <- atomically do
        current <- readTVar registry.registryNextSubagentId
        let next = current + 1
        writeTVar registry.registryNextSubagentId next
        pure next
    now <- getCurrentTime
    let micros = floor (utcTimeToPOSIXSeconds now * 1000000) :: Integer
        hex = showHex (micros `mod` 0x100000000) ""
        pad = replicate (8 - length hex) '0' <> hex
    pure $ SubagentId $ Text.pack ("agent-" <> pad <> "-" <> show n)
