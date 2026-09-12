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
import Control.Concurrent.Async
    ( asyncWithUnmask, concurrently_, link, race, replicateConcurrently_, wait, waitSTM, withAsync
    )
import Control.Concurrent.MVar (newEmptyMVar, newMVar, putMVar, readMVar, withMVar)
import Control.Concurrent.STM
import qualified Control.Exception as Exception
import Control.Exception.Safe
    ( SomeException
    , finally
    , mask
    , onException
    , tryAny
    )
import Control.Monad (void)
import qualified Agent.ResourceScope as ResourceScope
import Data.Acquire (withAcquire)
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
newSubagentRegistry config cwd run onEvent = mask \_ -> do
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
    resources <- ResourceScope.newResourceScope
    executor <- newEmptyMVar
    executorStopped <- newTVarIO False
    cleanupQueue <- newTQueueIO
    let registry = SubagentRegistry { registryAgents = agents
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
        , registryResources = resources
        , registryExecutor = executor
        , registryExecutorStopped = executorStopped
        , registryCleanupQueue = cleanupQueue
        }
    -- The constructor returns before the service ends. This single root is
    -- retained by the registry and joined by close; every executor child below
    -- it has a lexical structured-concurrency scope.
    worker <- asyncWithUnmask (\unmask -> unmask $
        runRegistryExecutor registry
            `finally` ResourceScope.closeResourceScope resources)
        `onException` ResourceScope.closeResourceScope resources
    putMVar executor worker
    pure registry

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
-- child's executor can begin queued follow-up work. A first transition to
-- 'Closed' is reported after execution and resource release finish; that callback runs
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
    withMVar registry.registryLifecycle \_ -> do
        closeSubagentRegistryLocked registry
        atomically $ writeTVar registry.registryExecutorStopped True
        readMVar registry.registryExecutor >>= wait

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
-- A terminally closed registry must be replaced, not reset.
resetSubagentRegistry :: SubagentRegistry -> IO ()
resetSubagentRegistry registry =
    withMVar registry.registryLifecycle \_ -> do
        stopped <- readTVarIO registry.registryExecutorStopped
        whenIO stopped $ ioError (userError "Cannot reset a terminally closed subagent registry.")
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

-- | Run host preparation after admission but before turn execution starts.
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
    executionVar <- newTVarIO False
    leaseVar <- newTVarIO Nothing
    cleanupVar <- newTVarIO Nothing
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
                                                                , recordExecution = executionVar
                                                                , recordLease = leaseVar
                                                                , recordCleanup = cleanupVar
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
                    acquireRecordResources registry record lease)
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
rollbackAdmission registry record =
    withMVar registry.registryLifecycle \_ ->
        rollbackAdmissionLocked registry record

rollbackAdmissionLocked :: SubagentRegistry -> SubagentRecord -> IO ()
rollbackAdmissionLocked registry record = do
    atomically do
        releaseSlotSTM registry record
        writeTVar record.recordPhase AgentClosed
    -- Keep the record discoverable by registry shutdown if rollback is
    -- interrupted while the service is releasing its resources.
    releaseRecordResources registry record
    atomically do
        agents <- readTVar registry.registryAgents
        case Map.lookup record.recordId agents of
            Just current | current.recordExecution == record.recordExecution -> do
                writeTVar registry.registryAgents (Map.delete record.recordId agents)
                modifyTVar' registry.registryPaths $
                    deleteOwnedPath record.recordTaskPath record.recordId
            _ -> pure ()

deleteOwnedPath :: TaskPath -> SubagentId -> Map TaskPath SubagentId -> Map TaskPath SubagentId
deleteOwnedPath key expected mappings =
    case Map.lookup key mappings of
        Just actual | actual == expected -> Map.delete key mappings
        _ -> mappings

runRecordTurn :: SubagentRegistry -> SubagentRecord -> SubagentWork -> IO ()
runRecordTurn registry record initialWork = runWork initialWork
  where
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
        atomically (nextTurnStep registry record) >>= \case
            TurnStop -> pure ()
            TurnIdle -> pure ()
            TurnMessage nextWork -> do
                resetForQueuedWork
                runWork nextWork
            TurnComplete -> do
                notifyRoot <- atomically $
                    publishCompletionSTM registry record status
                whenIO notifyRoot do
                    _ <- tryAny $
                        notifyComplete
                            registry record.recordId work.workRootTurnId status
                    pure ()
                notifySettled registry record.recordId status
                atomically (finishTurnStep registry record status) >>= \case
                    TurnStop -> pure ()
                    TurnIdle -> pure ()
                    TurnMessage nextWork -> do
                        resetForQueuedWork
                        runWork nextWork
                    TurnComplete -> pure ()

    resetForQueuedWork = do
        resetCancel record.recordCancel
        interrupted <- atomically $
            readTVar record.recordPhase >>= \case
                AgentInterrupting{} -> pure True
                AgentClosed -> pure True
                _ -> pure False
        whenIO interrupted (requestCancel record.recordCancel)

publishCompletionSTM :: SubagentRegistry -> SubagentRecord -> SubagentStatus -> STM Bool
publishCompletionSTM registry record status = do
    nextSeq <- readTVar registry.registryNextUpdateSeq
    let updateSeq = nextSeq + 1
    writeTVar registry.registryNextUpdateSeq updateSeq
    writeTVar record.recordLastUpdate (Just (updateSeq, status))
    routeCompletionSTM registry record status

-- | Publish a status for a turn settled administratively before its
-- execution starts. This wakes untargeted waiters just like a normal
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

acquireRecordResources
    :: SubagentRegistry
    -> SubagentRecord
    -> SubagentLease
    -> IO (Either Text ())
acquireRecordResources registry record lease =
    mask \_ -> do
        canStart <- atomically do
            closed <- readTVar registry.registryClosed
            phase <- readTVar record.recordPhase
            aborted <- isRootTurnAborted registry (phaseRootTurnId phase)
            agents <- readTVar registry.registryAgents
            paths <- readTVar registry.registryPaths
            current <- readTVar record.recordLease
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
                pure (Left "Subagent closed before resource acquisition.")
            else do
                acquired <- tryAny $ case lease of
                    SubagentLease acquire ->
                        ResourceScope.allocateAcquire registry.registryResources acquire
                case acquired of
                    Left (exception :: SomeException) -> do
                        pure (Left ("Failed to start subagent: " <> Text.pack (show exception)))
                    Right (key, ()) -> do
                        atomically do
                            writeTVar record.recordLease (Just key)
                            writeTVar record.recordCleanup Nothing
                        pure (Right ())

releaseSubagentLease :: SubagentLease -> IO ()
releaseSubagentLease (SubagentLease acquire) =
    withAcquire acquire (const (pure ()))

-- | Closing callers wait for service-owned cleanup. Cancelling a waiter cannot
-- interrupt resource release, and every retry observes the same acknowledgement.
releaseRecordResources :: SubagentRegistry -> SubagentRecord -> IO ()
releaseRecordResources registry record = do
    completion <- atomically $
        readTVar record.recordCleanup >>= \case
            Just completion -> pure completion
            Nothing -> do
                completion <- newEmptyTMVar
                writeTVar record.recordCleanup (Just completion)
                writeTQueue registry.registryCleanupQueue (record, completion)
                pure completion
    executor <- readMVar registry.registryExecutor
    atomically $
        readTMVar completion `orElse` do
            waitSTM executor
            throwSTM (userError "Subagent executor stopped before resource cleanup completed.")

-- | Worker scopes grow only when the configured concurrency high-water mark
-- increases. Completed turns reuse workers; idle agents own no threads.
runRegistryExecutor :: SubagentRegistry -> IO ()
runRegistryExecutor registry =
    concurrently_ (growWorkers 0) (replicateConcurrently_ 8 cleanupWorker)
  where
    growWorkers count = do
        grow <- atomically do
            stopped <- readTVar registry.registryExecutorStopped
            config <- readTVar registry.registryConfig
            if stopped then pure False
            else if count < config.maxConcurrent then pure True
            else retry
        whenIO grow $
            withAsync turnWorker \worker -> do
                link worker
                growWorkers (count + 1)

    turnWorker = do
        selected <- atomically do
            stopped <- readTVar registry.registryExecutorStopped
            if stopped then pure Nothing
            else readTVar registry.registryAgents >>= selectPending . Map.elems
        case selected of
            Nothing -> pure ()
            Just (record, work) -> do
                -- Administrative close also covers callbacks outside the
                -- provider's per-turn cancellation race. Its scope is joined
                -- before the resource-release worker can proceed.
                (do
                    result <- race (atomically $ waitClosed record)
                        -- Contain child outcomes, including async-classified
                        -- exceptions, inside the child's scope. Cancellation of
                        -- the reusable executor itself still propagates.
                        (Exception.try (runRecordTurn registry record work))
                    case result of
                        Right (Left (exception :: SomeException)) ->
                            atomically $
                                readTVar record.recordPhase >>= \case
                                    AgentClosed -> pure ()
                                    _ -> do
                                        let status = Errored (Text.pack (show exception))
                                        publishDirectUpdateSTM registry record status
                                        transitionToIdleSTM registry record status
                        _ -> pure ())
                    `finally` atomically (writeTVar record.recordExecution False)
                turnWorker

    selectPending :: [SubagentRecord] -> STM (Maybe (SubagentRecord, SubagentWork))
    selectPending [] = retry
    selectPending (record : remaining) = do
        executing <- readTVar record.recordExecution
        lease <- readTVar record.recordLease
        phase <- readTVar record.recordPhase
        case (executing, lease, phase) of
            (False, Just _, AgentPending work) -> do
                writeTVar record.recordExecution True
                writeTVar record.recordPhase (AgentRunning work.workRootTurnId)
                pure (Just (record, work))
            _ -> selectPending remaining

    waitClosed :: SubagentRecord -> STM ()
    waitClosed record = readTVar record.recordPhase >>= \case
        AgentClosed -> pure ()
        _ -> retry

    cleanupWorker = do
        job <- atomically $
            (Just <$> readTQueue registry.registryCleanupQueue)
                `orElse` do
                    stopped <- readTVar registry.registryExecutorStopped
                    check stopped
                    pure Nothing
        case job of
            Nothing -> pure ()
            Just (record, completion) -> do
                atomically $ readTVar record.recordExecution >>= check . not
                key <- readTVarIO record.recordLease
                void $ tryAny $ mapM_ ResourceScope.releaseResource key
                atomically do
                    writeTVar record.recordLease Nothing
                    putTMVar completion ()
                cleanupWorker

data TurnStep
    = TurnStop
    | TurnIdle
    | TurnComplete
    | TurnMessage !SubagentWork

nextTurnStep
    :: SubagentRegistry
    -> SubagentRecord
    -> STM TurnStep
nextTurnStep registry record = do
    turnStep registry record (pure TurnComplete)

finishTurnStep
    :: SubagentRegistry
    -> SubagentRecord
    -> SubagentStatus
    -> STM TurnStep
finishTurnStep registry record status = do
    turnStep registry record do
        transitionToIdleSTM registry record status
        pure TurnIdle

turnStep
    :: SubagentRegistry
    -> SubagentRecord
    -> STM TurnStep
    -> STM TurnStep
turnStep registry record onIdle =
    readTVar record.recordPhase >>= \case
        AgentClosed -> release TurnStop
        AgentInterrupting{} -> do
            transitionToIdleSTM registry record Interrupted
            pure TurnIdle
        AgentRunning{} ->
            tryReadTQueue record.recordMailbox >>= \case
                Nothing -> onIdle
                Just work -> do
                    writeTVar record.recordPhase
                        (AgentRunning work.workRootTurnId)
                    pure (TurnMessage work)
        AgentPending{} -> retry
        AgentIdle{} -> pure TurnIdle
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
    releaseRecordResources registry record
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
