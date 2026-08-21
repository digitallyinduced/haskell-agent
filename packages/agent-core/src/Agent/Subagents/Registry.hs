-- | Nestable subagent registry.
--
-- Shared state (agent map, status, mailboxes, admission count) lives in STM.
-- IO is only used to allocate ids, start child 'Async' loops,
-- and wait with timeouts via 'threadDelay'.
module Agent.Subagents.Registry
    ( SubagentRegistry
    , newSubagentRegistry
    , setSubagentRunner
    , setSubagentOnComplete
    , beginRootTurn
    , abortRootTurn
    , closeSubagentRegistry
    , interruptActiveSubagents
    , resetSubagentRegistry
    , spawnSubagent
    , spawnSubagentWithCwd
    , spawnSubagentWithCwdForTurn
    , spawnSubagentWithCwdPrepared
    , spawnSubagentWithCwdPreparedForTurn
    , spawnSubagentAt
    , spawnSubagentAtForTurn
    , spawnSubagentAtWithCwdPrepared
    , restoreSubagent
    , restoreSubagentAt
    , restoreSubagentWithCwd
    , restoreSubagentAtWithCwd
    , waitSubagents
    , waitSubagentsFrom
    , waitAnyLive
    , sendInput
    , sendInputMessage
    , sendInputMessageForTurn
    , queueMessage
    , queueMessageFrom
    , queueMessageFromForTurn
    , closeSubagent
    , interruptSubagent
    , resumeSubagent
    , getStatus
    , getPreviousResponseId
    , getSubagentCwd
    , getSubagentIdentity
    , setPreviousResponseId
    , getTaskPath
    , resolveAgentTarget
    , listLive
    , listAgents
    ) where

import Agent.Cancel
    ( CancelFlag
    , newCancelFlag
    , requestCancel
    , resetCancel
    )
import Agent.InterAgentMessage
    ( InterAgentMessage(..)
    , InterAgentMessageContent
    , InterAgentMessageType(..)
    , plainInterAgentContent
    )
import Agent.Loop (LoopError(..), LoopEvent, LoopResult(..))
import Agent.OsPath (OsPath)
import Agent.ResourceScope
    ( ResourceKey
    , ResourceScope
    , allocateResource
    , closeResourceScope
    , newResourceScope
    , releaseResource
    )
import Agent.Subagents.Format (formatCompletionNotice, isFinalStatus)
import Agent.Subagents.Types
    ( RunSubagent
    , RootTurnId(..)
    , SubagentConfig(..)
    , SubagentId(..)
    , SubagentIdentity(..)
    , SubagentSpawnEnv(..)
    , SubagentStatus(..)
    , maxWaitTimeoutMs
    , minWaitTimeoutMs
    )
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, race, waitCatch)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM
import Control.Exception.Safe (SomeException, finally, mask, onException, tryAny)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word64)
import Numeric (showHex)
import Agent.Subagents.TaskPath
    ( TaskPath
    , joinTaskPath
    , resolveTaskPath
    , taskPathRoot
    , taskPathText
    )
import System.IO.Unsafe (unsafePerformIO)

data SubagentRecord = SubagentRecord
    { recordId :: !SubagentId
    , recordParent :: !(Maybe SubagentId)
    , recordDepth :: !Int
    , recordNickname :: !(Maybe Text)
    , recordStatus :: !(TVar SubagentStatus)
    , recordCancel :: !CancelFlag
    , recordMailbox :: !(TQueue SubagentWork)
    , recordAsync :: !(TVar (Maybe (ResourceKey, Async ())))
    , recordRootTurnId :: !(TVar (Maybe RootTurnId))
      -- | Whether this agent currently occupies a concurrency slot.
    , recordSlotHeld :: !(TVar Bool)
      -- | Last successful response id for conversation continuity.
    , recordPreviousResponseId :: !(TVar (Maybe Text))
    , recordLastUpdate :: !(TVar (Maybe (Int, SubagentStatus)))
    , recordTaskPath :: !TaskPath
    , recordCwd :: !OsPath
    }

data SubagentWork = SubagentWork
    { workRootTurnId :: !(Maybe RootTurnId)
    , workMessage :: !InterAgentMessage
    }

data SubagentRegistry = SubagentRegistry
    { registryAgents :: !(TVar (Map SubagentId SubagentRecord))
    , registryPaths :: !(TVar (Map TaskPath SubagentId))
    , registryLiveCount :: !(TVar Int)
    , registryNextUpdateSeq :: !(TVar Int)
    , registryWaitCursors :: !(TVar (Map (Maybe SubagentId) Int))
    , registryActiveWaits :: !(TVar (Map (Maybe SubagentId) [SubagentId]))
    , registryConfig :: !SubagentConfig
    , registryRunRef :: !(IORef RunSubagent)
    , registryOnEvent :: !(SubagentId -> LoopEvent -> IO ())
    , registryOnCompleteRef :: !(IORef (SubagentId -> SubagentStatus -> IO ()))
    , registryCwd :: !OsPath
    , registryClosed :: !(TVar Bool)
    , registryNextRootTurnId :: !(TVar Word64)
    , registryAbortedRootTurns :: !(TVar (Set RootTurnId))
    , registryLifecycle :: !(MVar ())
    , registryResources :: !(IORef ResourceScope)
    }

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
    nextUpdateSeq <- newTVarIO 0
    waitCursors <- newTVarIO Map.empty
    activeWaits <- newTVarIO Map.empty
    closed <- newTVarIO False
    nextRootTurnId <- newTVarIO 0
    abortedRootTurns <- newTVarIO Set.empty
    lifecycle <- newMVar ()
    resources <- newIORef =<< newResourceScope
    runRef <- newIORef run
    onCompleteRef <- newIORef (\_ _ -> pure ())
    pure SubagentRegistry
        { registryAgents = agents
        , registryPaths = paths
        , registryLiveCount = live
        , registryNextUpdateSeq = nextUpdateSeq
        , registryWaitCursors = waitCursors
        , registryActiveWaits = activeWaits
        , registryConfig = config
            { maxConcurrent = max 1 config.maxConcurrent
            }
        , registryRunRef = runRef
        , registryOnEvent = onEvent
        , registryOnCompleteRef = onCompleteRef
        , registryCwd = cwd
        , registryClosed = closed
        , registryNextRootTurnId = nextRootTurnId
        , registryAbortedRootTurns = abortedRootTurns
        , registryLifecycle = lifecycle
        , registryResources = resources
        }

setSubagentRunner :: SubagentRegistry -> RunSubagent -> IO ()
setSubagentRunner registry = writeIORef registry.registryRunRef

-- | Invoked when a child reaches a final status (completed / errored /
-- interrupted). Used to deliver parent-facing completion notices.
setSubagentOnComplete
    :: SubagentRegistry
    -> (SubagentId -> SubagentStatus -> IO ())
    -> IO ()
setSubagentOnComplete registry = writeIORef registry.registryOnCompleteRef

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
    mapM_ (shutdownRecord registry) records
    resources <- readIORef registry.registryResources
    closeResourceScope resources

-- | Shut down live children and reopen the registry for a fresh session.
resetSubagentRegistry :: SubagentRegistry -> IO ()
resetSubagentRegistry registry =
    withMVar registry.registryLifecycle \_ -> do
        closeSubagentRegistryLocked registry
        resources <- newResourceScope
        writeIORef registry.registryResources resources
        atomically do
            writeTVar registry.registryAgents Map.empty
            writeTVar registry.registryPaths Map.empty
            writeTVar registry.registryLiveCount 0
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
        registry rootTurnId childCwd (\_ -> pure ())

-- | Run host preparation after admission but before the worker starts.
spawnSubagentWithCwdPrepared
    :: SubagentRegistry
    -> OsPath
    -> (SubagentId -> IO ())
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
    -> (SubagentId -> IO ())
    -> Maybe SubagentId
    -> Int
    -> Text
    -> Maybe Text
    -> IO (Either Text SubagentId)
spawnSubagentWithCwdPreparedForTurn
        registry rootTurnId childCwd beforeStart
        parentId parentDepth message nickname = do
    parentPath <- case parentId of
        Nothing -> pure taskPathRoot
        Just pid -> do
            mpath <- getTaskPath registry pid
            pure (fromMaybe taskPathRoot mpath)
    agentIdPreview <- newSubagentId
    let taskName =
            "a"
                <> Text.filter (\c -> c /= '-') agentIdPreview.unSubagentId
    -- Reuse the generated id by spawning with an explicit path helper that
    -- still allocates internally; uniqueness comes from the id-derived name.
    fmap (fmap fst) $
        spawnSubagentAtWithCwdPreparedForTurn
            registry rootTurnId childCwd beforeStart
            parentId parentPath parentDepth taskName
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
    spawnSubagentAtWithCwdPreparedForTurn
        registry rootTurnId registry.registryCwd (\_ -> pure ())

spawnSubagentAtWithCwdPrepared
    :: SubagentRegistry
    -> OsPath
    -> (SubagentId -> IO ())
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
    -> (SubagentId -> IO ())
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
    let nextDepth = parentDepth + 1
        cfg = registry.registryConfig
    case cfg.maxDepth of
        Just limit | nextDepth > limit ->
            pure $ Left "Agent depth limit reached. Solve the task yourself."
        _ -> case joinTaskPath parentPath taskName of
            Left err -> pure (Left err)
            Right childPath -> do
                agentId <- newSubagentId
                cancelFlag <- newCancelFlag
                mailbox <- newTQueueIO
                statusVar <- newTVarIO Pending
                asyncVar <- newTVarIO Nothing
                rootTurnVar <- newTVarIO rootTurnId
                slotHeld <- newTVarIO True
                previousVar <- newTVarIO Nothing
                lastUpdateVar <- newTVarIO Nothing
                let record = SubagentRecord
                        { recordId = agentId
                        , recordParent = parentId
                        , recordDepth = nextDepth
                        , recordNickname = nickname
                        , recordStatus = statusVar
                        , recordCancel = cancelFlag
                        , recordMailbox = mailbox
                        , recordAsync = asyncVar
                        , recordRootTurnId = rootTurnVar
                        , recordSlotHeld = slotHeld
                        , recordPreviousResponseId = previousVar
                        , recordLastUpdate = lastUpdateVar
                        , recordTaskPath = childPath
                        , recordCwd = childCwd
                        }
                admitted <- withMVar registry.registryLifecycle \_ -> atomically do
                    closed <- readTVar registry.registryClosed
                    aborted <- isRootTurnAborted registry rootTurnId
                    if closed
                        then pure (Left "Subagent registry is closed.")
                        else if aborted
                            then pure (Left "Root turn was aborted.")
                        else do
                            parentCheck <-
                                validateParentSTM
                                    registry parentId parentPath parentDepth
                            case parentCheck of
                                Left err -> pure (Left err)
                                Right () -> do
                                    paths <- readTVar registry.registryPaths
                                    if Map.member childPath paths
                                        then pure $ Left $
                                            "task path already in use: " <> taskPathText childPath
                                        else do
                                            live <- readTVar registry.registryLiveCount
                                            if live >= cfg.maxConcurrent
                                                then pure $ Left $
                                                    "Concurrent subagent limit reached: "
                                                        <> Text.pack (show cfg.maxConcurrent)
                                                        <> " agents are already open. Close finished agents before spawning more."
                                                else do
                                                    modifyTVar' registry.registryLiveCount (+ 1)
                                                    modifyTVar' registry.registryAgents (Map.insert agentId record)
                                                    modifyTVar' registry.registryPaths (Map.insert childPath agentId)
                                                    pure (Right ())
                case admitted of
                    Left err -> pure (Left err)
                    Right () -> mask \restore ->
                        (do
                            prepared <- tryAny (restore (beforeStart agentId))
                            case prepared of
                                Left (exc :: SomeException) -> do
                                    rollbackAdmission registry record
                                    pure $ Left $
                                        "Failed to prepare subagent: "
                                            <> Text.pack (show exc)
                                Right () ->
                                    startPrepared restore agentId childPath record)
                            `onException` rollbackAdmission registry record
  where
    startPrepared restore agentId childPath record = do
        let work = SubagentWork
                { workRootTurnId = rootTurnId
                , workMessage = InterAgentMessage
                    { messageAuthor = taskPathText parentPath
                    , messageRecipient = taskPathText childPath
                    , messageType = NewTaskMessage
                    , messageContent = content
                    }
                }
        started <-
            restore
                (withMVar registry.registryLifecycle \_ ->
                    startRecordWorker registry record work)
                `onException` shutdownRecord registry record
        case started of
            Left err -> do
                rollbackAdmission registry record
                pure (Left err)
            Right () ->
                pure (Right (agentId, childPath))

rollbackAdmission :: SubagentRegistry -> SubagentRecord -> IO ()
rollbackAdmission registry record = atomically do
    modifyTVar' registry.registryAgents (Map.delete record.recordId)
    modifyTVar' registry.registryPaths $
        deleteOwnedPath record.recordTaskPath record.recordId
    held <- readTVar record.recordSlotHeld
    whenSTM held do
        writeTVar record.recordSlotHeld False
        live <- readTVar registry.registryLiveCount
        writeTVar registry.registryLiveCount (max 0 (live - 1))
    writeTVar record.recordStatus Closed

deleteOwnedPath :: TaskPath -> SubagentId -> Map TaskPath SubagentId -> Map TaskPath SubagentId
deleteOwnedPath key expected mappings =
    case Map.lookup key mappings of
        Just actual | actual == expected -> Map.delete key mappings
        _ -> mappings

validateParentSTM
    :: SubagentRegistry
    -> Maybe SubagentId
    -> TaskPath
    -> Int
    -> STM (Either Text ())
validateParentSTM registry parentId parentPath parentDepth =
    case parentId of
        Nothing ->
            if parentPath == taskPathRoot && parentDepth == 0
                then pure (Right ())
                else pure (Left "root spawn has inconsistent parent context")
        Just pid -> do
            agents <- readTVar registry.registryAgents
            case Map.lookup pid agents of
                Nothing -> pure (Left "Parent subagent is closed or missing.")
                Just parent -> do
                    status <- readTVar parent.recordStatus
                    if status == Closed || status == NotFound
                        then pure (Left "Parent subagent is closed or missing.")
                        else if parent.recordTaskPath /= parentPath
                            || parent.recordDepth /= parentDepth
                            then pure (Left "parent agent context does not match registry")
                            else pure (Right ())

runWorker :: SubagentRegistry -> SubagentRecord -> SubagentWork -> IO ()
runWorker registry record firstWork = do
    mayRun <- atomically do
        closed <- readTVar registry.registryClosed
        current <- readTVar record.recordStatus
        if closed || current == Closed || current == Interrupted
            then pure False
            else do
                writeTVar record.recordStatus Running
                pure True
    let onEvent = registry.registryOnEvent record.recordId
        loop work = do
            atomically $ writeTVar record.recordRootTurnId work.workRootTurnId
            let env = SubagentSpawnEnv
                    { subId = record.recordId
                    , subDepth = record.recordDepth
                    , subParentId = record.recordParent
                    , subCwd = record.recordCwd
                    , subCancel = record.recordCancel
                    , subRootTurnId = work.workRootTurnId
                    }
            previous <- atomically $ readTVar record.recordPreviousResponseId
            run <- readIORef registry.registryRunRef
            result <- tryAny (run env previous work.workMessage onEvent)
            let status = case result of
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
            next <- atomically $ nextWorkerStep registry record
            case next of
                WorkerClosed -> pure ()
                WorkerMessage nextWork -> do
                    resetCancel record.recordCancel
                    loop nextWork
                WorkerComplete -> do
                    notifyRoot <- atomically $
                        publishCompletionSTM registry record status
                    whenIO notifyRoot do
                        _ <- tryAny $
                            notifyComplete
                                registry record.recordId work.workRootTurnId status
                        pure ()
                    atomically (finishOrContinue record status) >>= \case
                        Nothing -> pure ()
                        Just nextWork -> do
                            resetCancel record.recordCancel
                            loop nextWork
    whenIO mayRun (loop firstWork)

publishCompletionSTM :: SubagentRegistry -> SubagentRecord -> SubagentStatus -> STM Bool
publishCompletionSTM registry record status = do
    nextSeq <- readTVar registry.registryNextUpdateSeq
    let updateSeq = nextSeq + 1
    writeTVar registry.registryNextUpdateSeq updateSeq
    writeTVar record.recordLastUpdate (Just (updateSeq, status))
    routeCompletionSTM registry record status

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
        parentStatus <- readTVar parent.recordStatus
        if parentStatus == Running || parentStatus == Pending
            then do
                rootTurnId <- readTVar record.recordRootTurnId
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

startRecordWorker
    :: SubagentRegistry
    -> SubagentRecord
    -> SubagentWork
    -> IO (Either Text ())
startRecordWorker registry record work =
    mask \_ -> do
        canStart <- atomically do
            closed <- readTVar registry.registryClosed
            status <- readTVar record.recordStatus
            aborted <- isRootTurnAborted registry work.workRootTurnId
            agents <- readTVar registry.registryAgents
            paths <- readTVar registry.registryPaths
            current <- readTVar record.recordAsync
            pure $
                not closed
                    && not aborted
                    && status /= Closed
                    && Map.member record.recordId agents
                    && maybe True (== record.recordId)
                        (Map.lookup record.recordTaskPath paths)
                    && maybe True (const False) current
        if not canStart
            then pure (Left "Subagent closed before its worker started.")
            else do
                gate <- newEmptyTMVarIO
                resources <- readIORef registry.registryResources
                started <- tryAny $
                    allocateResource resources
                        (async do
                            atomically (takeTMVar gate)
                            runWorker registry record work)
                        stopAsync
                case started of
                    Left (exception :: SomeException) ->
                        pure (Left ("Failed to start subagent: " <> Text.pack (show exception)))
                    Right worker -> do
                        atomically do
                            writeTVar record.recordAsync (Just worker)
                            writeTVar record.recordRootTurnId work.workRootTurnId
                            writeTVar record.recordStatus Running
                            putTMVar gate ()
                        pure (Right ())

stopAsync :: Async () -> IO ()
stopAsync worker = do
    cancel worker
    _ <- waitCatch worker
    pure ()

takeRecordWorker :: SubagentRecord -> IO (Maybe (ResourceKey, Async ()))
takeRecordWorker record =
    atomically do
        current <- readTVar record.recordAsync
        writeTVar record.recordAsync Nothing
        pure current

releaseRecordWorker :: SubagentRecord -> IO ()
releaseRecordWorker record = do
    worker <- takeRecordWorker record
    mapM_ (releaseResource . fst) worker

finishRecordWorker :: SubagentRecord -> IO ()
finishRecordWorker record = do
    worker <- takeRecordWorker record
    mapM_
        (\(key, child) -> do
            _ <- waitCatch child
            releaseResource key)
        worker

data WorkerStep
    = WorkerClosed
    | WorkerComplete
    | WorkerMessage !SubagentWork

nextWorkerStep
    :: SubagentRegistry
    -> SubagentRecord
    -> STM WorkerStep
nextWorkerStep registry record = do
    closed <- readTVar registry.registryClosed
    current <- readTVar record.recordStatus
    if closed || current == Closed || current == Interrupted
        then do
            whenSTM closed (writeTVar record.recordStatus Closed)
            pure WorkerClosed
        else do
            empty <- isEmptyTQueue record.recordMailbox
            if empty
                then pure WorkerComplete
                else do
                    work <- readTQueue record.recordMailbox
                    writeTVar record.recordRootTurnId work.workRootTurnId
                    writeTVar record.recordStatus Running
                    pure (WorkerMessage work)

finishOrContinue
    :: SubagentRecord
    -> SubagentStatus
    -> STM (Maybe SubagentWork)
finishOrContinue record status = do
    current <- readTVar record.recordStatus
    if current == Closed || current == Interrupted
        then pure Nothing
        else do
            empty <- isEmptyTQueue record.recordMailbox
            if empty
                then do
                    writeTVar record.recordStatus status
                    pure Nothing
                else do
                    work <- readTQueue record.recordMailbox
                    writeTVar record.recordRootTurnId work.workRootTurnId
                    writeTVar record.recordStatus Running
                    pure (Just work)

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
                    current <- readTVar record.recordStatus
                    owner <- readTVar record.recordRootTurnId
                    aborted <- isRootTurnAborted registry rootTurnId
                    pure $
                        not closed
                            && not aborted
                            && current == Running
                            && owner == rootTurnId
        whenIO shouldNotify do
            onComplete <- readIORef registry.registryOnCompleteRef
            onComplete agentId status
    | otherwise = pure ()

releaseSlot :: SubagentRegistry -> SubagentRecord -> IO ()
releaseSlot registry record = atomically do
    held <- readTVar record.recordSlotHeld
    whenSTM held do
        writeTVar record.recordSlotHeld False
        live <- readTVar registry.registryLiveCount
        writeTVar registry.registryLiveCount (max 0 (live - 1))

whenSTM :: Bool -> STM () -> STM ()
whenSTM True action = action
whenSTM False _ = pure ()

acquireSlot :: SubagentRegistry -> SubagentRecord -> STM (Either Text ())
acquireSlot registry record = do
    closed <- readTVar registry.registryClosed
    if closed
        then pure (Left "Subagent registry is closed.")
        else do
            held <- readTVar record.recordSlotHeld
            if held
                then pure (Right ())
                else do
                    live <- readTVar registry.registryLiveCount
                    if live >= registry.registryConfig.maxConcurrent
                        then pure $ Left $
                            "Concurrent subagent limit reached: "
                                <> Text.pack (show registry.registryConfig.maxConcurrent)
                                <> " agents are already open."
                        else do
                            modifyTVar' registry.registryLiveCount (+ 1)
                            writeTVar record.recordSlotHeld True
                            pure (Right ())

-- | Wait until any target reaches a final status (or timeout). Returns the
-- status map for every requested id. Matches Codex v1: multiple targets mean
-- "whichever finishes first".
waitSubagents
    :: SubagentRegistry
    -> [SubagentId]
    -> Int
    -> IO (Map SubagentId SubagentStatus, Bool)
waitSubagents registry = waitSubagentsFrom registry Nothing

waitSubagentsFrom
    :: SubagentRegistry
    -> Maybe SubagentId
    -> [SubagentId]
    -> Int
    -> IO (Map SubagentId SubagentStatus, Bool)
waitSubagentsFrom registry caller targets timeoutMs =
    finally wait unregister
  where
    wait = do
        atomically $
            modifyTVar' registry.registryActiveWaits
                (Map.insert caller targets)
        let clamped = max minWaitTimeoutMs (min maxWaitTimeoutMs (max 1 timeoutMs))
            waitForFinal = atomically do
                statuses <- mapM (readStatusSTM registry) targets
                let pairs = zip targets statuses
                if any (isFinalStatus . snd) pairs
                    then pure (Map.fromList pairs)
                    else retry
            waitForTimeout = do
                threadDelay (clamped * 1000)
                atomically do
                    statuses <- mapM (readStatusSTM registry) targets
                    pure (Map.fromList (zip targets statuses))
        race waitForFinal waitForTimeout >>= \case
            Left statuses -> pure (statuses, False)
            Right statuses -> pure (statuses, True)
    unregister =
        atomically $
            modifyTVar' registry.registryActiveWaits (Map.delete caller)

sendInput
    :: SubagentRegistry
    -> SubagentId
    -> Text
    -> Bool
    -> IO (Either Text Text)
sendInput registry agentId message interrupt =
    sendInputMessage registry taskPathRoot agentId
        (plainInterAgentContent message) interrupt

sendInputMessage
    :: SubagentRegistry
    -> TaskPath
    -> SubagentId
    -> InterAgentMessageContent
    -> Bool
    -> IO (Either Text Text)
sendInputMessage registry senderPath agentId content interrupt = do
    sendInputMessageForTurn registry Nothing senderPath agentId content interrupt

sendInputMessageForTurn
    :: SubagentRegistry
    -> Maybe RootTurnId
    -> TaskPath
    -> SubagentId
    -> InterAgentMessageContent
    -> Bool
    -> IO (Either Text Text)
sendInputMessageForTurn registry rootTurnId senderPath agentId content interrupt =
    withMVar registry.registryLifecycle \_ -> do
        mrecord <- atomically $
            Map.lookup agentId <$> readTVar registry.registryAgents
        case mrecord of
            Nothing -> pure (Left ("unknown agent id: " <> agentId.unSubagentId))
            Just record -> do
                status <- atomically $ readTVar record.recordStatus
                let work = SubagentWork
                        { workRootTurnId = rootTurnId
                        , workMessage = InterAgentMessage
                            { messageAuthor = taskPathText senderPath
                            , messageRecipient = taskPathText record.recordTaskPath
                            , messageType = FollowUpMessage
                            , messageContent = content
                            }
                        }
                case status of
                    Closed -> pure (Left "agent is closed")
                    NotFound -> pure (Left "agent not found")
                    Running -> queue record work
                    Pending -> queue record work
                    _ -> restart record work
  where
    queue
        :: SubagentRecord
        -> SubagentWork
        -> IO (Either Text Text)
    queue record work = do
        whenIO interrupt (requestCancel record.recordCancel)
        queued <- atomically do
            aborted <- isRootTurnAborted registry rootTurnId
            if aborted
                then pure False
                else writeTQueue record.recordMailbox work >> pure True
        pure $ if queued
            then Right "queued"
            else Left "Root turn was aborted."

    restart
        :: SubagentRecord
        -> SubagentWork
        -> IO (Either Text Text)
    restart record work = do
        admitted <- atomically do
            aborted <- isRootTurnAborted registry rootTurnId
            if aborted
                then pure (Left "Root turn was aborted.")
                else acquireSlot registry record
        case admitted of
            Left err -> pure (Left err)
            Right () -> do
                resetCancel record.recordCancel
                finishRecordWorker record
                started <- startRecordWorker registry record work
                case started of
                    Left err -> do
                        releaseSlot registry record
                        pure (Left err)
                    Right () -> pure (Right "queued")

whenIO :: Bool -> IO () -> IO ()
whenIO True action = action
whenIO False _ = pure ()

closeSubagent
    :: SubagentRegistry
    -> SubagentId
    -> IO (Either Text SubagentStatus)
closeSubagent registry agentId =
    withMVar registry.registryLifecycle \_ -> do
        mrecord <- atomically $
            Map.lookup agentId <$> readTVar registry.registryAgents
        case mrecord of
            Nothing -> pure (Left ("unknown agent id: " <> agentId.unSubagentId))
            Just record -> do
                previous <- atomically $ readTVar record.recordStatus
                toClose <- atomically do
                    agents <- readTVar registry.registryAgents
                    pure (record : descendants agents record.recordId)
                mapM_ (shutdownRecord registry) toClose
                pure (Right previous)

descendants :: Map SubagentId SubagentRecord -> SubagentId -> [SubagentRecord]
descendants agents parentId =
    let kids = [r | r <- Map.elems agents, r.recordParent == Just parentId]
    in kids <> concatMap (\kid -> descendants agents kid.recordId) kids

shutdownRecord :: SubagentRegistry -> SubagentRecord -> IO ()
shutdownRecord registry record = do
    requestCancel record.recordCancel
    atomically $ writeTVar record.recordStatus Closed
    releaseRecordWorker record
    releaseSlot registry record

abortRootTurn :: SubagentRegistry -> RootTurnId -> IO ()
abortRootTurn registry rootTurnId =
    withMVar registry.registryLifecycle \_ -> do
        records <- atomically do
            modifyTVar' registry.registryAbortedRootTurns (Set.insert rootTurnId)
            agents <- Map.elems <$> readTVar registry.registryAgents
            fmap concat $ mapM selectOwned agents
        mapM_ (interruptRecordForTurn registry rootTurnId) records
  where
    selectOwned :: SubagentRecord -> STM [SubagentRecord]
    selectOwned record = do
        discardQueuedWork rootTurnId record.recordMailbox
        owner <- readTVar record.recordRootTurnId
        status <- readTVar record.recordStatus
        pure
            [ record
            | owner == Just rootTurnId
            , status == Pending || status == Running
            ]

interruptRecordForTurn :: SubagentRegistry -> RootTurnId -> SubagentRecord -> IO ()
interruptRecordForTurn registry rootTurnId record = do
    ownedWorker <- atomically do
        owner <- readTVar record.recordRootTurnId
        if owner == Just rootTurnId
            then do
                writeTVar record.recordStatus Interrupted
                worker <- readTVar record.recordAsync
                writeTVar record.recordAsync Nothing
                pure (Just worker)
            else pure Nothing
    case ownedWorker of
        Nothing -> pure ()
        Just mworker -> do
            requestCancel record.recordCancel
            mapM_ (releaseResource . fst) mworker
            atomically do
                owner <- readTVar record.recordRootTurnId
                whenSTM (owner == Just rootTurnId) $
                    writeTVar record.recordStatus Interrupted
            releaseSlot registry record

discardQueuedWork :: RootTurnId -> TQueue SubagentWork -> STM ()
discardQueuedWork rootTurnId mailbox = do
    queued <- flushTQueue mailbox
    mapM_ (writeTQueue mailbox)
        [ work
        | work <- queued
        , work.workRootTurnId /= Just rootTurnId
        ]

isRootTurnAborted :: SubagentRegistry -> Maybe RootTurnId -> STM Bool
isRootTurnAborted _ Nothing = pure False
isRootTurnAborted registry (Just rootTurnId) =
    Set.member rootTurnId <$> readTVar registry.registryAbortedRootTurns

-- | Stop every currently pending/running child while keeping completed agent
-- records and the registry available for later turns. The registry is closed
-- during the transition so a descendant cannot publish a newly-created worker
-- after the abort snapshot has been taken.
interruptActiveSubagents :: SubagentRegistry -> IO ()
interruptActiveSubagents registry =
    withMVar registry.registryLifecycle \_ -> do
        (wasClosed, records) <- atomically do
            wasClosed <- readTVar registry.registryClosed
            writeTVar registry.registryClosed True
            agents <- Map.elems <$> readTVar registry.registryAgents
            records <- filterMSTM isActiveRecord agents
            pure (wasClosed, records)
        mapM_ (interruptRecord registry) records
            `finally`
                atomically (writeTVar registry.registryClosed wasClosed)
  where
    isActiveRecord :: SubagentRecord -> STM Bool
    isActiveRecord record = do
        status <- readTVar record.recordStatus
        pure (status == Pending || status == Running)

interruptRecord :: SubagentRegistry -> SubagentRecord -> IO ()
interruptRecord registry record = do
    requestCancel record.recordCancel
    atomically do
        writeTVar record.recordStatus Interrupted
        _ <- flushTQueue record.recordMailbox
        pure ()
    releaseRecordWorker record
    atomically $ writeTVar record.recordStatus Interrupted
    releaseSlot registry record

filterMSTM :: (a -> STM Bool) -> [a] -> STM [a]
filterMSTM predicate = fmap reverse . go []
  where
    go kept [] = pure kept
    go kept (value : rest) = do
        include <- predicate value
        go (if include then value : kept else kept) rest

-- | Re-admit a previously persisted agent that is not currently in the
-- in-memory map (e.g. after close, or across a process restart within the
-- same session directory). Does not start a worker; callers follow with
-- 'sendInput'. Does not consume a concurrency slot until the next turn.
restoreSubagent
    :: SubagentRegistry
    -> SubagentId
    -> Maybe SubagentId
    -> Int
    -> Maybe Text
    -> Maybe Text
    -> IO (Either Text SubagentId)
restoreSubagent registry =
    restoreSubagentWithCwd registry registry.registryCwd

restoreSubagentAt
    :: SubagentRegistry
    -> SubagentId
    -> Maybe SubagentId
    -> TaskPath
    -> Int
    -> Maybe Text
    -> Maybe Text
    -> IO (Either Text SubagentId)
restoreSubagentAt registry =
    restoreSubagentAtWithCwd registry registry.registryCwd

restoreSubagentWithCwd
    :: SubagentRegistry
    -> OsPath
    -> SubagentId
    -> Maybe SubagentId
    -> Int
    -> Maybe Text
    -> Maybe Text
    -> IO (Either Text SubagentId)
restoreSubagentWithCwd registry childCwd agentId parentId depth nickname previous =
    restoreSubagentAtWithCwd
        registry childCwd agentId parentId taskPathRoot depth nickname previous

restoreSubagentAtWithCwd
    :: SubagentRegistry
    -> OsPath
    -> SubagentId
    -> Maybe SubagentId
    -> TaskPath
    -> Int
    -> Maybe Text
    -> Maybe Text
    -> IO (Either Text SubagentId)
restoreSubagentAtWithCwd
        registry childCwd agentId parentId taskPath depth nickname previous =
    withMVar registry.registryLifecycle \_ -> do
        closed <- atomically $ readTVar registry.registryClosed
        if closed
            then pure (Left "Subagent registry is closed.")
            else do
                existing <- atomically $
                    Map.lookup agentId <$> readTVar registry.registryAgents
                case existing of
                    Just record -> restoreExisting record
                    Nothing -> restoreMissing
  where
    restoreExisting record = do
        status <- atomically $ readTVar record.recordStatus
        case status of
            Running -> pure (Left "cannot restore a running subagent")
            Pending -> pure (Left "cannot restore a pending subagent")
            NotFound -> pure (Left "cannot restore a missing subagent record")
            _ -> do
                finishRecordWorker record
                releaseSlot registry record
                resetCancel record.recordCancel
                atomically do
                    writeTVar record.recordStatus (Completed Nothing)
                    writeTVar record.recordPreviousResponseId previous
                    writeTVar record.recordRootTurnId Nothing
                pure (Right agentId)

    restoreMissing = do
        cancelFlag <- newCancelFlag
        mailbox <- newTQueueIO
        statusVar <- newTVarIO (Completed Nothing)
        asyncVar <- newTVarIO Nothing
        rootTurnVar <- newTVarIO Nothing
        slotHeld <- newTVarIO False
        previousVar <- newTVarIO previous
        lastUpdateVar <- newTVarIO Nothing
        let record = SubagentRecord
                { recordId = agentId
                , recordParent = parentId
                , recordDepth = depth
                , recordNickname = nickname
                , recordStatus = statusVar
                , recordCancel = cancelFlag
                , recordMailbox = mailbox
                , recordAsync = asyncVar
                , recordRootTurnId = rootTurnVar
                , recordSlotHeld = slotHeld
                , recordPreviousResponseId = previousVar
                , recordLastUpdate = lastUpdateVar
                , recordTaskPath = taskPath
                , recordCwd = childCwd
                }
        inserted <- atomically do
            closed <- readTVar registry.registryClosed
            if closed
                then pure (Left "Subagent registry is closed.")
                else do
                    paths <- readTVar registry.registryPaths
                    case Map.lookup taskPath paths of
                        Just owner | owner /= agentId ->
                            pure $ Left $
                                "task path already in use: " <> taskPathText taskPath
                        _ -> do
                            modifyTVar' registry.registryAgents
                                (Map.insert agentId record)
                            whenSTM (taskPath /= taskPathRoot) $
                                modifyTVar' registry.registryPaths
                                    (Map.insert taskPath agentId)
                            pure (Right agentId)
        pure inserted

resumeSubagent
    :: SubagentRegistry
    -> SubagentId
    -> IO (Either Text SubagentStatus)
resumeSubagent registry agentId =
    withMVar registry.registryLifecycle \_ -> do
        closed <- atomically $ readTVar registry.registryClosed
        if closed
            then pure (Left "Subagent registry is closed.")
            else do
                mrecord <- atomically $
                    Map.lookup agentId <$> readTVar registry.registryAgents
                case mrecord of
                    Nothing ->
                        pure (Left ("unknown agent id: " <> agentId.unSubagentId))
                    Just record -> do
                        status <- atomically $ readTVar record.recordStatus
                        case status of
                            Closed -> do
                                resetCancel record.recordCancel
                                atomically do
                                    writeTVar record.recordStatus (Completed Nothing)
                                    writeTVar record.recordRootTurnId Nothing
                                pure (Right (Completed Nothing))
                            other -> pure (Right other)

getStatus :: SubagentRegistry -> SubagentId -> IO SubagentStatus
getStatus registry agentId = atomically (readStatusSTM registry agentId)

getPreviousResponseId :: SubagentRegistry -> SubagentId -> IO (Maybe Text)
getPreviousResponseId registry agentId = atomically do
    agents <- readTVar registry.registryAgents
    case Map.lookup agentId agents of
        Nothing -> pure Nothing
        Just record -> readTVar record.recordPreviousResponseId

getSubagentCwd :: SubagentRegistry -> SubagentId -> IO (Maybe OsPath)
getSubagentCwd registry agentId = atomically do
    agents <- readTVar registry.registryAgents
    pure ((.recordCwd) <$> Map.lookup agentId agents)

getSubagentIdentity :: SubagentRegistry -> SubagentId -> IO (Maybe SubagentIdentity)
getSubagentIdentity registry agentId = atomically do
    agents <- readTVar registry.registryAgents
    case Map.lookup agentId agents of
        Nothing -> pure Nothing
        Just record ->
            pure $ Just $
                SubagentIdentity
                    record.recordParent
                    record.recordDepth
                    record.recordTaskPath

setPreviousResponseId :: SubagentRegistry -> SubagentId -> Text -> IO ()
setPreviousResponseId registry agentId responseId = atomically do
    agents <- readTVar registry.registryAgents
    case Map.lookup agentId agents of
        Nothing -> pure ()
        Just record ->
            writeTVar record.recordPreviousResponseId (Just responseId)

readStatusSTM :: SubagentRegistry -> SubagentId -> STM SubagentStatus
readStatusSTM registry agentId = do
    agents <- readTVar registry.registryAgents
    case Map.lookup agentId agents of
        Nothing -> pure NotFound
        Just record -> readTVar record.recordStatus

listLive :: SubagentRegistry -> IO [(SubagentId, SubagentStatus)]
listLive registry = atomically do
    agents <- readTVar registry.registryAgents
    mapM
        (\record -> do
            status <- readTVar record.recordStatus
            pure (record.recordId, status))
        (Map.elems agents)

newSubagentId :: IO SubagentId
newSubagentId = do
    n <- atomicModifyIORef' subagentIdCounter \i -> (i + 1, i + 1)
    now <- getCurrentTime
    let micros = floor (utcTimeToPOSIXSeconds now * 1000000) :: Integer
        hex = showHex (micros `mod` 0x100000000) ""
        pad = replicate (8 - length hex) '0' <> hex
    pure $ SubagentId $ Text.pack ("agent-" <> pad <> "-" <> show n)

subagentIdCounter :: IORef Int
subagentIdCounter = unsafePerformIO (newIORef (0 :: Int))
{-# NOINLINE subagentIdCounter #-}

getTaskPath :: SubagentRegistry -> SubagentId -> IO (Maybe TaskPath)
getTaskPath registry agentId = atomically do
    agents <- readTVar registry.registryAgents
    pure (fmap (.recordTaskPath) (Map.lookup agentId agents))

resolveAgentTarget
    :: SubagentRegistry
    -> TaskPath
    -> Text
    -> IO (Either Text SubagentId)
resolveAgentTarget registry callerPath target
    | "agent-" `Text.isPrefixOf` target = do
        status <- getStatus registry (SubagentId target)
        pure $ case status of
            NotFound -> Left ("unknown agent id: " <> target)
            _ -> Right (SubagentId target)
    | otherwise = case resolveTaskPath callerPath target of
        Left err -> pure (Left err)
        Right path -> atomically do
            paths <- readTVar registry.registryPaths
            pure $ case Map.lookup path paths of
                Just agentId -> Right agentId
                Nothing -> Left ("unknown task path: " <> taskPathText path)

listAgents
    :: SubagentRegistry
    -> Maybe Text
    -> IO [(TaskPath, SubagentId, SubagentStatus)]
listAgents registry pathPrefix = atomically do
    agents <- readTVar registry.registryAgents
    let prefix = maybe "" Text.strip pathPrefix
    fmap concat $ mapM
        (\record -> do
            status <- readTVar record.recordStatus
            let pathText = taskPathText record.recordTaskPath
                keep =
                    status /= Closed
                        && status /= NotFound
                        && (Text.null prefix || prefix `Text.isPrefixOf` pathText)
            pure $ if keep
                then [(record.recordTaskPath, record.recordId, status)]
                else [])
        (Map.elems agents)

interruptSubagent
    :: SubagentRegistry
    -> SubagentId
    -> IO (Either Text SubagentStatus)
interruptSubagent registry agentId = do
    mrecord <- atomically $ Map.lookup agentId <$> readTVar registry.registryAgents
    case mrecord of
        Nothing -> pure (Left ("unknown agent id: " <> agentId.unSubagentId))
        Just record -> do
            previous <- atomically $ readTVar record.recordStatus
            requestCancel record.recordCancel
            pure (Right previous)

-- | Queue a message without starting a new turn when idle (v2 send_message).
queueMessage
    :: SubagentRegistry
    -> SubagentId
    -> Text
    -> IO (Either Text Text)
queueMessage registry agentId message =
    queueMessageFrom registry taskPathRoot agentId
        (plainInterAgentContent message)

queueMessageFrom
    :: SubagentRegistry
    -> TaskPath
    -> SubagentId
    -> InterAgentMessageContent
    -> IO (Either Text Text)
queueMessageFrom registry senderPath agentId content = do
    queueMessageFromForTurn registry Nothing senderPath agentId content

queueMessageFromForTurn
    :: SubagentRegistry
    -> Maybe RootTurnId
    -> TaskPath
    -> SubagentId
    -> InterAgentMessageContent
    -> IO (Either Text Text)
queueMessageFromForTurn registry rootTurnId senderPath agentId content = do
    mrecord <- atomically $ Map.lookup agentId <$> readTVar registry.registryAgents
    case mrecord of
        Nothing -> pure (Left ("unknown agent id: " <> agentId.unSubagentId))
        Just record -> do
            status <- atomically $ readTVar record.recordStatus
            case status of
                Closed -> pure (Left "agent is closed")
                NotFound -> pure (Left "agent not found")
                _ -> do
                    let work = SubagentWork
                            { workRootTurnId = rootTurnId
                            , workMessage = InterAgentMessage
                                { messageAuthor = taskPathText senderPath
                                , messageRecipient = taskPathText record.recordTaskPath
                                , messageType = QueuedMessage
                                , messageContent = content
                                }
                            }
                    atomically do
                        aborted <- isRootTurnAborted registry rootTurnId
                        if aborted
                            then pure (Left "Root turn was aborted.")
                            else writeTQueue record.recordMailbox work >> pure (Right "queued")

-- | Wait until any live non-final agent reaches a final status (or timeout).
waitAnyLive
    :: SubagentRegistry
    -> Maybe SubagentId
    -> Int
    -> IO (Map SubagentId SubagentStatus, Bool)
waitAnyLive registry caller timeoutMs = do
    let clamped = max minWaitTimeoutMs (min maxWaitTimeoutMs (max 1 timeoutMs))
        wait = do
            atomically $
                modifyTVar' registry.registryActiveWaits
                    (Map.insert caller [])
            race
                (atomically (takeAgentUpdatesSTM registry caller))
                (threadDelay (clamped * 1000))
                >>= \case
                    Left statuses -> pure (statuses, False)
                    Right () -> pure (Map.empty, True)
        unregister =
            atomically $
                modifyTVar' registry.registryActiveWaits (Map.delete caller)
    finally wait unregister

takeAgentUpdatesSTM
    :: SubagentRegistry
    -> Maybe SubagentId
    -> STM (Map SubagentId SubagentStatus)
takeAgentUpdatesSTM registry caller = do
    cursors <- readTVar registry.registryWaitCursors
    let cursor = Map.findWithDefault 0 caller cursors
    agents <- readTVar registry.registryAgents
    updates <- fmap concat $ mapM (recordUpdateAfter caller cursor) (Map.elems agents)
    case updates of
        [] -> retry
        _ -> do
            let latest = maximum (map (\(_, seqNo, _) -> seqNo) updates)
                statuses =
                    Map.fromList
                        [ (agentId, status)
                        | (agentId, _, status) <- updates
                        ]
            writeTVar registry.registryWaitCursors (Map.insert caller latest cursors)
            pure statuses

recordUpdateAfter
    :: Maybe SubagentId
    -> Int
    -> SubagentRecord
    -> STM [(SubagentId, Int, SubagentStatus)]
recordUpdateAfter caller cursor record
    | caller == Just record.recordId = pure []
    | otherwise = do
        update <- readTVar record.recordLastUpdate
        pure $ case update of
            Just (seqNo, status) | seqNo > cursor ->
                [(record.recordId, seqNo, status)]
            _ -> []
