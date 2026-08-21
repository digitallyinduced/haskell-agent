-- | Nestable subagent registry.
--
-- Shared state (agent map, status, mailboxes, admission count) lives in STM.
-- IO is only used to allocate ids, start explicitly tracked child threads,
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

import Agent.Cancel (CancelFlag, newCancelFlag, requestCancel)
import Agent.InterAgentMessage
    ( InterAgentMessage(..)
    , InterAgentMessageContent
    , InterAgentMessageType(..)
    , plainInterAgentContent
    )
import Agent.Loop (LoopError(..), LoopEvent, LoopResult(..))
import Agent.OsPath (OsPath)
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
import Control.Concurrent
    ( ThreadId
    , forkFinally
    , killThread
    , threadDelay
    )
import Control.Concurrent.Async (race)
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
    , recordCancel :: !(TVar CancelFlag)
    , recordMailbox :: !(TQueue SubagentWork)
    , recordWorker :: !(TVar (Maybe Worker))
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

data Worker = Worker
    { workerThreadId :: !(TMVar (Maybe ThreadId))
    , workerDone :: !(TMVar ())
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
closeSubagentRegistry registry = do
    records <- atomically do
        writeTVar registry.registryClosed True
        Map.elems <$> readTVar registry.registryAgents
    mapM_ (shutdownRecord registry) records

-- | Shut down live children and reopen the registry for a fresh session.
resetSubagentRegistry :: SubagentRegistry -> IO ()
resetSubagentRegistry registry = do
    closeSubagentRegistry registry
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
        parentId parentPath parentDepth taskName content nickname =
    mask \restore ->
        spawnSubagentAtWithCwdPreparedForTurnMasked
            restore registry rootTurnId childCwd beforeStart
            parentId parentPath parentDepth taskName content nickname

spawnSubagentAtWithCwdPreparedForTurnMasked
    :: (IO () -> IO ())
    -> SubagentRegistry
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
spawnSubagentAtWithCwdPreparedForTurnMasked
        restore registry rootTurnId childCwd beforeStart
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
                cancelVar <- newTVarIO cancelFlag
                mailbox <- newTQueueIO
                statusVar <- newTVarIO Pending
                workerVar <- newTVarIO Nothing
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
                        , recordCancel = cancelVar
                        , recordMailbox = mailbox
                        , recordWorker = workerVar
                        , recordRootTurnId = rootTurnVar
                        , recordSlotHeld = slotHeld
                        , recordPreviousResponseId = previousVar
                        , recordLastUpdate = lastUpdateVar
                        , recordTaskPath = childPath
                        , recordCwd = childCwd
                        }
                admitted <- atomically do
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
                                            "task path already in use: "
                                                <> taskPathText childPath
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
                    Right () ->
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
            startRecordWorker registry record
                (restore (runWorker registry record work))
                `onException` shutdownRecord registry record
        if started
            then pure (Right (agentId, childPath))
            else do
                rollbackAdmission registry record
                pure (Left "Subagent closed before its worker started.")

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
                Nothing -> pure (Left "parent agent not found")
                Just parent -> do
                    status <- readTVar parent.recordStatus
                    if status == Running || status == Pending
                        then
                            if parent.recordTaskPath == parentPath
                                && parent.recordDepth == parentDepth
                                then pure (Right ())
                                else pure (Left "parent agent context does not match registry")
                        else pure (Left "parent agent is not running")

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

deleteOwnedPath :: TaskPath -> SubagentId -> Map TaskPath SubagentId -> Map TaskPath SubagentId
deleteOwnedPath key expected mappings =
    case Map.lookup key mappings of
        Just actual | actual == expected -> Map.delete key mappings
        _ -> mappings

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
            cancelFlag <- atomically $ readTVar record.recordCancel
            let env = SubagentSpawnEnv
                    { subId = record.recordId
                    , subDepth = record.recordDepth
                    , subParentId = record.recordParent
                    , subCwd = record.recordCwd
                    , subCancel = cancelFlag
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
            (next, notifyRoot) <- atomically do
                closed <- readTVar registry.registryClosed
                current <- readTVar record.recordStatus
                if closed || current == Closed
                    then do
                        writeTVar record.recordStatus Closed
                        pure (Nothing, False)
                    else if current == Interrupted
                        then pure (Nothing, False)
                    else do
                        empty <- isEmptyTQueue record.recordMailbox
                        if empty
                            then do
                                notify <- publishCompletionSTM registry record status
                                pure (Nothing, notify)
                            else do
                                nextWork <- readTQueue record.recordMailbox
                                writeTVar record.recordRootTurnId nextWork.workRootTurnId
                                writeTVar record.recordStatus Running
                                pure (Just nextWork, False)
            case next of
                -- Completed/errored/interrupted agents stay open and keep their
                -- concurrency slot until close_agent, matching Codex v1.
                Nothing ->
                    whenIO notifyRoot
                        (notifyRootComplete registry record.recordId status)
                Just nextWork -> loop nextWork
    whenIO mayRun (loop firstWork)

publishCompletionSTM :: SubagentRegistry -> SubagentRecord -> SubagentStatus -> STM Bool
publishCompletionSTM registry record status = do
    nextSeq <- readTVar registry.registryNextUpdateSeq
    let updateSeq = nextSeq + 1
    writeTVar registry.registryNextUpdateSeq updateSeq
    writeTVar record.recordStatus status
    writeTVar record.recordLastUpdate (Just (updateSeq, status))
    routeCompletionSTM registry record status

routeCompletionSTM :: SubagentRegistry -> SubagentRecord -> SubagentStatus -> STM Bool
routeCompletionSTM registry record status =
    case record.recordParent of
        Nothing ->
            not <$> completionIsAwaitedSTM registry Nothing record.recordId
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

-- | Record ownership before forking. The thread id is published immediately
-- afterward, so shutdown can safely race startup and still cancel and join.
startRecordWorker
    :: SubagentRegistry
    -> SubagentRecord
    -> IO ()
    -> IO Bool
startRecordWorker registry record action = mask \restore -> do
    -- A completed turn publishes its status just before its worker exits.
    -- Wait for that short tail before replacing the tracked worker.
    atomically do
        current <- readTVar record.recordWorker
        case current of
            Nothing -> pure ()
            Just worker -> readTMVar worker.workerDone
    threadIdVar <- newEmptyTMVarIO
    done <- newEmptyTMVarIO
    let worker = Worker
            { workerThreadId = threadIdVar
            , workerDone = done
            }
    started <- atomically do
        closed <- readTVar registry.registryClosed
        status <- readTVar record.recordStatus
        rootTurnId <- readTVar record.recordRootTurnId
        aborted <- isRootTurnAborted registry rootTurnId
        current <- readTVar record.recordWorker
        case current of
            Just _ -> pure False
            Nothing
                | closed || aborted || status == Closed || status == Interrupted -> pure False
                | otherwise -> do
                    writeTVar record.recordWorker (Just worker)
                    pure True
    if started
        then do
            tid <- forkFinally (restore action) (\_ -> finishWorker record worker)
                `onException` do
                    atomically $ putTMVar threadIdVar Nothing
                    finishWorker record worker
            atomically $ putTMVar threadIdVar (Just tid)
            pure True
        else pure False

finishWorker :: SubagentRecord -> Worker -> IO ()
finishWorker record worker = atomically do
    writeTVar record.recordWorker Nothing
    putTMVar worker.workerDone ()

stopWorker :: Worker -> IO ()
stopWorker worker = do
    threadId <- atomically $ readTMVar worker.workerThreadId
    mapM_ killThread threadId
    atomically $ readTMVar worker.workerDone

notifyRootComplete :: SubagentRegistry -> SubagentId -> SubagentStatus -> IO ()
notifyRootComplete registry agentId status
    | isFinalStatus status && status /= Closed && status /= NotFound = do
        shouldNotify <- atomically do
            closed <- readTVar registry.registryClosed
            agents <- readTVar registry.registryAgents
            case Map.lookup agentId agents of
                Nothing -> pure False
                Just record -> do
                    current <- readTVar record.recordStatus
                    pure (not closed && current == status)
        whenIO shouldNotify do
            onComplete <- readIORef registry.registryOnCompleteRef
            onComplete agentId status
    | otherwise = pure ()

releaseSlot :: SubagentRegistry -> SubagentRecord -> IO ()
releaseSlot registry record = atomically (releaseSlotSTM registry record)

releaseSlotSTM :: SubagentRegistry -> SubagentRecord -> STM ()
releaseSlotSTM registry record = do
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

data SendKick
    = KickNone
    | KickStart !SubagentStatus !Bool
    | KickFail !Text

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
    mask \restore ->
        sendInputMessageForTurnMasked
            restore registry rootTurnId senderPath agentId content interrupt

sendInputMessageForTurnMasked
    :: (IO () -> IO ())
    -> SubagentRegistry
    -> Maybe RootTurnId
    -> TaskPath
    -> SubagentId
    -> InterAgentMessageContent
    -> Bool
    -> IO (Either Text Text)
sendInputMessageForTurnMasked
        restore registry rootTurnId senderPath agentId content interrupt = do
    freshCancel <- newCancelFlag
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
                                , messageType = FollowUpMessage
                                , messageContent = content
                                }
                            }
                    activeCancel <- atomically $ readTVar record.recordCancel
                    whenIO interrupt (requestCancel activeCancel)
                    -- Admit before mutating status/mailbox so a failed resume
                    -- does not leave the agent stuck in Running with no worker.
                    kick <- atomically do
                        aborted <- isRootTurnAborted registry rootTurnId
                        current <- readTVar record.recordStatus
                        if aborted
                            then pure (KickFail "Root turn was aborted.")
                            else case current of
                                Running -> do
                                    writeTQueue record.recordMailbox work
                                    pure KickNone
                                Pending -> do
                                    writeTQueue record.recordMailbox work
                                    pure KickNone
                                _ -> do
                                    worker <- readTVar record.recordWorker
                                    case worker of
                                        Just _ -> retry
                                        Nothing -> do
                                            heldBefore <- readTVar record.recordSlotHeld
                                            admitted <- acquireSlot registry record
                                            case admitted of
                                                Left err -> pure (KickFail err)
                                                Right () -> do
                                                    writeTVar record.recordCancel freshCancel
                                                    writeTVar record.recordRootTurnId rootTurnId
                                                    writeTVar record.recordStatus Running
                                                    pure (KickStart current (not heldBefore))
                    case kick of
                        KickFail err -> pure (Left err)
                        KickNone -> pure (Right "queued")
                        KickStart previous acquired -> do
                            started <-
                                startRecordWorker registry record
                                    (restore (runWorker registry record work))
                                    `onException`
                                        rollbackFollowup registry record previous acquired
                            if started
                                then pure (Right "queued")
                                else do
                                    rollbackFollowup registry record previous acquired
                                    pure (Left "agent was closed before its worker started")

whenIO :: Bool -> IO () -> IO ()
whenIO True action = action
whenIO False _ = pure ()

rollbackFollowup :: SubagentRegistry -> SubagentRecord -> SubagentStatus -> Bool -> IO ()
rollbackFollowup registry record previous acquired = atomically do
    status <- readTVar record.recordStatus
    whenSTM (status == Running) (writeTVar record.recordStatus previous)
    whenSTM acquired (releaseSlotSTM registry record)

closeSubagent
    :: SubagentRegistry
    -> SubagentId
    -> IO (Either Text SubagentStatus)
closeSubagent registry agentId = do
    mrecord <- atomically $ Map.lookup agentId <$> readTVar registry.registryAgents
    case mrecord of
        Nothing -> pure (Left ("unknown agent id: " <> agentId.unSubagentId))
        Just record -> do
            (previous, toClose) <- atomically do
                previous <- readTVar record.recordStatus
                agents <- readTVar registry.registryAgents
                let records = record : descendants agents record.recordId
                mapM_ (\item -> writeTVar item.recordStatus Closed) records
                pure (previous, records)
            mapM_ (shutdownRecord registry) toClose
            pure (Right previous)

descendants :: Map SubagentId SubagentRecord -> SubagentId -> [SubagentRecord]
descendants agents parentId =
    let kids = [r | r <- Map.elems agents, r.recordParent == Just parentId]
    in kids <> concatMap (\kid -> descendants agents kid.recordId) kids

shutdownRecord :: SubagentRegistry -> SubagentRecord -> IO ()
shutdownRecord registry record = do
    cancelFlag <- atomically $ readTVar record.recordCancel
    requestCancel cancelFlag
    mworker <- atomically do
        writeTVar record.recordStatus Closed
        readTVar record.recordWorker
    case mworker of
        Nothing -> pure ()
        Just worker -> stopWorker worker
    releaseSlot registry record

abortRootTurn :: SubagentRegistry -> RootTurnId -> IO ()
abortRootTurn registry rootTurnId = do
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
                worker <- readTVar record.recordWorker
                pure (Just worker)
            else pure Nothing
    case ownedWorker of
        Nothing -> pure ()
        Just mworker -> do
            cancelFlag <- atomically $ readTVar record.recordCancel
            requestCancel cancelFlag
            case mworker of
                Nothing -> pure ()
                Just worker -> stopWorker worker
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
interruptActiveSubagents registry = do
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
    cancelFlag <- atomically $ readTVar record.recordCancel
    requestCancel cancelFlag
    mworker <- atomically do
        writeTVar record.recordStatus Interrupted
        _ <- flushTQueue record.recordMailbox
        readTVar record.recordWorker
    case mworker of
        Nothing -> pure ()
        Just worker -> stopWorker worker
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
restoreSubagentAtWithCwd registry childCwd agentId parentId taskPath depth nickname previous = do
    freshCancel <- newCancelFlag
    existing <- atomically $ Map.lookup agentId <$> readTVar registry.registryAgents
    case existing of
        Just record -> do
            -- Same-process resume after close: reopen without consuming a slot.
            atomically do
                closed <- readTVar registry.registryClosed
                status <- readTVar record.recordStatus
                worker <- readTVar record.recordWorker
                if closed
                    then pure (Left "Subagent registry is closed.")
                    else if status == Running || status == Pending
                        then pure (Left "agent is already running")
                        else case worker of
                            Just _ ->
                                pure (Left "agent shutdown is still in progress")
                            Nothing -> do
                                writeTVar record.recordStatus (Completed Nothing)
                                writeTVar record.recordCancel freshCancel
                                writeTVar record.recordPreviousResponseId previous
                                writeTVar record.recordRootTurnId Nothing
                                pure (Right agentId)
        Nothing -> do
            cancelVar <- newTVarIO freshCancel
            mailbox <- newTQueueIO
            statusVar <- newTVarIO (Completed Nothing)
            workerVar <- newTVarIO Nothing
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
                    , recordCancel = cancelVar
                    , recordMailbox = mailbox
                    , recordWorker = workerVar
                    , recordRootTurnId = rootTurnVar
                    , recordSlotHeld = slotHeld
                    , recordPreviousResponseId = previousVar
                    , recordLastUpdate = lastUpdateVar
                    , recordTaskPath = taskPath
                    , recordCwd = childCwd
                    }
            atomically do
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

resumeSubagent
    :: SubagentRegistry
    -> SubagentId
    -> IO (Either Text SubagentStatus)
resumeSubagent registry agentId = do
    mrecord <- atomically $ Map.lookup agentId <$> readTVar registry.registryAgents
    case mrecord of
        Nothing -> pure (Left ("unknown agent id: " <> agentId.unSubagentId))
        Just record -> do
            status <- atomically $ readTVar record.recordStatus
            case status of
                Closed -> do
                    atomically $ writeTVar record.recordStatus (Completed Nothing)
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
            cancelFlag <- atomically $ readTVar record.recordCancel
            requestCancel cancelFlag
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
