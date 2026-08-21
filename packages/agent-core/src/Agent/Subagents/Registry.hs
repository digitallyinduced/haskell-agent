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
    , restoreSubagentWithCwd
    , waitSubagents
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
import Agent.Subagents.Format (isFinalStatus)
import Agent.Subagents.Types
    ( RunSubagent
    , RootTurnId(..)
    , SubagentConfig(..)
    , SubagentId(..)
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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as TextRead
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
    , recordWorker :: !(TVar (Maybe Worker))
    , recordRootTurnId :: !(TVar (Maybe RootTurnId))
      -- | Whether this agent currently occupies a concurrency slot.
    , recordSlotHeld :: !(TVar Bool)
      -- | Last successful response id for conversation continuity.
    , recordPreviousResponseId :: !(TVar (Maybe Text))
    , recordTaskPath :: !TaskPath
    , recordCwd :: !OsPath
    }

data SubagentWork = SubagentWork
    { workRootTurnId :: !(Maybe RootTurnId)
    , workMessage :: !InterAgentMessage
    }

data Worker = Worker
    { workerThreadId :: !(TMVar ThreadId)
    , workerDone :: !(TMVar ())
    }

data SubagentRegistry = SubagentRegistry
    { registryAgents :: !(TVar (Map SubagentId SubagentRecord))
    , registryPaths :: !(TVar (Map TaskPath SubagentId))
    , registryLiveCount :: !(TVar Int)
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
    closed <- newTVarIO False
    nextRootTurnId <- newTVarIO 0
    abortedRootTurns <- newTVarIO Set.empty
    runRef <- newIORef run
    onCompleteRef <- newIORef (\_ _ -> pure ())
    pure SubagentRegistry
        { registryAgents = agents
        , registryPaths = paths
        , registryLiveCount = live
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
    agentId <- newSubagentId
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
    agentId <- newSubagentId
    spawnSubagentAtWithIdPreparedForTurn
        registry rootTurnId childCwd beforeStart agentId
        parentId parentPath parentDepth taskName content nickname

spawnSubagentAtWithIdPreparedForTurn
    :: SubagentRegistry
    -> Maybe RootTurnId
    -> OsPath
    -> (SubagentId -> IO ())
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
    statusVar <- newTVarIO Pending
    workerVar <- newTVarIO Nothing
    rootTurnVar <- newTVarIO rootTurnId
    slotHeld <- newTVarIO True
    previousVar <- newTVarIO Nothing
    admitted <- atomically do
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
                        Right (parentPath, nextDepth) ->
                            case registry.registryConfig.maxDepth of
                                Just limit | nextDepth > limit ->
                                    pure $ Left
                                        "Agent depth limit reached. Solve the task yourself."
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
                                                if live >= registry.registryConfig.maxConcurrent
                                                    then pure $ Left $
                                                        "Concurrent subagent limit reached: "
                                                            <> Text.pack
                                                                (show registry.registryConfig.maxConcurrent)
                                                            <> " agents are already open. Close finished agents before spawning more."
                                                    else do
                                                        let record = SubagentRecord
                                                                { recordId = agentId
                                                                , recordParent = parentId
                                                                , recordDepth = nextDepth
                                                                , recordNickname = nickname
                                                                , recordStatus = statusVar
                                                                , recordCancel = cancelFlag
                                                                , recordMailbox = mailbox
                                                                , recordWorker = workerVar
                                                                , recordRootTurnId = rootTurnVar
                                                                , recordSlotHeld = slotHeld
                                                                , recordPreviousResponseId = previousVar
                                                                , recordTaskPath = childPath
                                                                , recordCwd = childCwd
                                                                }
                                                        modifyTVar'
                                                            registry.registryLiveCount (+ 1)
                                                        writeTVar registry.registryAgents
                                                            (Map.insert agentId record agents)
                                                        writeTVar registry.registryPaths
                                                            (Map.insert childPath agentId paths)
                                                        pure (Right (record, parentPath))
    case admitted of
        Left err -> pure (Left err)
        Right (record, parentPath) -> mask \restore ->
            (do
                prepared <- tryAny (restore (beforeStart agentId))
                case prepared of
                    Left (exc :: SomeException) -> do
                        rollbackAdmission registry record
                        pure $ Left $
                            "Failed to prepare subagent: " <> Text.pack (show exc)
                    Right () ->
                        startPrepared restore parentPath record)
                `onException` rollbackAdmission registry record
  where
    startPrepared restore parentPath record = do
        let work = SubagentWork
                { workRootTurnId = rootTurnId
                , workMessage = InterAgentMessage
                    { messageAuthor = taskPathText parentPath
                    , messageRecipient = taskPathText record.recordTaskPath
                    , messageType = NewTaskMessage
                    , messageContent = content
                    }
                }
        started <-
            restore
                (startRecordWorker registry record
                    (runWorker registry record work))
                `onException` shutdownRecord registry record
        if started
            then pure (Right (agentId, record.recordTaskPath))
            else do
                rollbackAdmission registry record
                pure (Left "Subagent closed before its worker started.")

resolveParentSTM
    :: Map SubagentId SubagentRecord
    -> Maybe SubagentId
    -> TaskPath
    -> Int
    -> STM (Either Text (TaskPath, Int))
resolveParentSTM _ Nothing requestedPath requestedDepth =
    pure (Right (requestedPath, requestedDepth + 1))
resolveParentSTM agents (Just parentId) _ _ =
    case Map.lookup parentId agents of
        Nothing -> pure (Left ("unknown parent agent id: " <> parentId.unSubagentId))
        Just parent -> do
            status <- readTVar parent.recordStatus
            if status == Closed
                then pure (Left "parent agent is closed")
                else pure (Right (parent.recordTaskPath, parent.recordDepth + 1))

taskNameForAgentId :: SubagentId -> Text
taskNameForAgentId agentId =
    "a" <> Text.filter (/= '-') agentId.unSubagentId

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
            next <- atomically do
                closed <- readTVar registry.registryClosed
                current <- readTVar record.recordStatus
                if closed || current == Closed
                    then do
                        writeTVar record.recordStatus Closed
                        pure Nothing
                    else do
                        empty <- isEmptyTQueue record.recordMailbox
                        if empty
                            then do
                                writeTVar record.recordStatus status
                                pure Nothing
                            else do
                                nextWork <- readTQueue record.recordMailbox
                                writeTVar record.recordRootTurnId nextWork.workRootTurnId
                                writeTVar record.recordStatus Running
                                pure (Just nextWork)
            case next of
                -- Completed/errored/interrupted agents stay open and keep their
                -- concurrency slot until close_agent, matching Codex v1.
                Nothing -> notifyComplete registry record.recordId status
                Just nextWork -> loop nextWork
    whenIO mayRun (loop firstWork)

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
                `onException` finishWorker record worker
            atomically $ putTMVar threadIdVar tid
            pure True
        else pure False

finishWorker :: SubagentRecord -> Worker -> IO ()
finishWorker record worker = atomically do
    writeTVar record.recordWorker Nothing
    putTMVar worker.workerDone ()

stopWorker :: Worker -> IO ()
stopWorker worker = do
    threadId <- atomically $ readTMVar worker.workerThreadId
    killThread threadId
    atomically $ readTMVar worker.workerDone

notifyComplete :: SubagentRegistry -> SubagentId -> SubagentStatus -> IO ()
notifyComplete registry agentId status
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
waitSubagents registry targets timeoutMs = do
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

data SendKick
    = KickNone
    | KickStart
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
sendInputMessageForTurn registry rootTurnId senderPath agentId content interrupt = do
    admitted <- atomically do
        closed <- readTVar registry.registryClosed
        aborted <- isRootTurnAborted registry rootTurnId
        if closed
            then pure (Left "Subagent registry is closed.")
            else if aborted
                then pure (Left "Root turn was aborted.")
                else do
                    agents <- readTVar registry.registryAgents
                    case Map.lookup agentId agents of
                        Nothing ->
                            pure (Left ("unknown agent id: " <> agentId.unSubagentId))
                        Just record -> do
                            current <- readTVar record.recordStatus
                            if current == Closed
                                then pure (Left "agent is closed")
                                else do
                                    let work = SubagentWork
                                            { workRootTurnId = rootTurnId
                                            , workMessage = InterAgentMessage
                                                { messageAuthor =
                                                    taskPathText senderPath
                                                , messageRecipient =
                                                    taskPathText record.recordTaskPath
                                                , messageType = FollowUpMessage
                                                , messageContent = content
                                                }
                                            }
                                    kick <- case current of
                                        Running -> do
                                            writeTQueue record.recordMailbox work
                                            pure KickNone
                                        Pending -> do
                                            writeTQueue record.recordMailbox work
                                            pure KickNone
                                        _ -> do
                                            slot <- acquireSlot registry record
                                            case slot of
                                                Left err -> pure (KickFail err)
                                                Right () -> do
                                                    writeTQueue record.recordMailbox work
                                                    writeTVar record.recordRootTurnId rootTurnId
                                                    writeTVar record.recordStatus Running
                                                    pure KickStart
                                    pure (Right (record, kick))
    case admitted of
        Left err -> pure (Left err)
        Right (record, kick) -> do
            whenIO interrupt (requestCancel record.recordCancel)
            case kick of
                KickFail err -> pure (Left err)
                KickNone -> pure (Right "queued")
                KickStart -> do
                    started <-
                        startRecordWorker registry record do
                            msg <- atomically $ readTQueue record.recordMailbox
                            runWorker registry record msg
                    if started
                        then pure (Right "queued")
                        else pure (Left "agent was closed before its worker started")

whenIO :: Bool -> IO () -> IO ()
whenIO True action = action
whenIO False _ = pure ()

closeSubagent
    :: SubagentRegistry
    -> SubagentId
    -> IO (Either Text SubagentStatus)
closeSubagent registry agentId = do
    prepared <- atomically do
        agents <- readTVar registry.registryAgents
        case Map.lookup agentId agents of
            Nothing -> pure (Left ("unknown agent id: " <> agentId.unSubagentId))
            Just record -> do
                previous <- readTVar record.recordStatus
                workers <- prepareShutdownSTM registry
                    (record : descendants agents record.recordId)
                pure (Right (previous, workers))
    case prepared of
        Left err -> pure (Left err)
        Right (previous, workers) -> do
            stopPrepared workers
            pure (Right previous)

descendants :: Map SubagentId SubagentRecord -> SubagentId -> [SubagentRecord]
descendants agents parentId =
    let kids = [r | r <- Map.elems agents, r.recordParent == Just parentId]
    in kids <> concatMap (\kid -> descendants agents kid.recordId) kids

shutdownRecord :: SubagentRegistry -> SubagentRecord -> IO ()
shutdownRecord registry record = do
    prepared <- atomically $ prepareShutdownSTM registry [record]
    stopPrepared prepared

prepareShutdownSTM
    :: SubagentRegistry
    -> [SubagentRecord]
    -> STM [(SubagentRecord, Maybe Worker)]
prepareShutdownSTM registry records = do
    prepared <- mapM prepare records
    let released = length [() | (_, _, True) <- prepared]
    modifyTVar' registry.registryLiveCount (\live -> max 0 (live - released))
    pure [(record, worker) | (record, worker, _) <- prepared]
  where
    prepare
        :: SubagentRecord
        -> STM (SubagentRecord, Maybe Worker, Bool)
    prepare record = do
        writeTVar record.recordStatus Closed
        writeTVar record.recordRootTurnId Nothing
        _ <- flushTQueue record.recordMailbox
        worker <- readTVar record.recordWorker
        held <- readTVar record.recordSlotHeld
        writeTVar record.recordSlotHeld False
        pure (record, worker, held)

stopPrepared :: [(SubagentRecord, Maybe Worker)] -> IO ()
stopPrepared = mapM_ \(record, worker) -> do
    requestCancel record.recordCancel
    case worker of
        Nothing -> pure ()
        Just active -> stopWorker active

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
            requestCancel record.recordCancel
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
    requestCancel record.recordCancel
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

restoreSubagentWithCwd
    :: SubagentRegistry
    -> OsPath
    -> SubagentId
    -> Maybe SubagentId
    -> Int
    -> Maybe Text
    -> Maybe Text
    -> IO (Either Text SubagentId)
restoreSubagentWithCwd registry childCwd agentId parentId depth nickname previous = do
    cancelFlag <- newCancelFlag
    mailbox <- newTQueueIO
    statusVar <- newTVarIO (Completed Nothing)
    workerVar <- newTVarIO Nothing
    rootTurnVar <- newTVarIO Nothing
    slotHeld <- newTVarIO False
    previousVar <- newTVarIO previous
    result <- atomically do
        closed <- readTVar registry.registryClosed
        if closed
            then pure (Left "Subagent registry is closed.")
            else do
                agents <- readTVar registry.registryAgents
                case Map.lookup agentId agents of
                    Just record -> do
                        status <- readTVar record.recordStatus
                        case status of
                            Closed -> do
                                writeTVar record.recordStatus (Completed Nothing)
                                writeTVar record.recordPreviousResponseId previous
                                writeTVar record.recordRootTurnId Nothing
                                modifyTVar' registry.registryPaths
                                    (Map.insert record.recordTaskPath agentId)
                                pure (Right agentId)
                            _ -> pure (Right agentId)
                    Nothing -> do
                        parent <- resolveParentSTM
                            agents parentId taskPathRoot (max 0 (depth - 1))
                        case parent of
                            Left err -> pure (Left err)
                            Right (parentPath, actualDepth) ->
                                case joinTaskPath parentPath (taskNameForAgentId agentId) of
                                    Left err -> pure (Left err)
                                    Right childPath -> do
                                        paths <- readTVar registry.registryPaths
                                        case Map.lookup childPath paths of
                                            Just other | other /= agentId ->
                                                pure $ Left $
                                                    "task path already in use: "
                                                        <> taskPathText childPath
                                            _ -> do
                                                let record = SubagentRecord
                                                        { recordId = agentId
                                                        , recordParent = parentId
                                                        , recordDepth = actualDepth
                                                        , recordNickname = nickname
                                                        , recordStatus = statusVar
                                                        , recordCancel = cancelFlag
                                                        , recordMailbox = mailbox
                                                        , recordWorker = workerVar
                                                        , recordRootTurnId = rootTurnVar
                                                        , recordSlotHeld = slotHeld
                                                        , recordPreviousResponseId = previousVar
                                                        , recordTaskPath = childPath
                                                        , recordCwd = childCwd
                                                        }
                                                writeTVar registry.registryAgents
                                                    (Map.insert agentId record agents)
                                                writeTVar registry.registryPaths
                                                    (Map.insert childPath agentId paths)
                                                pure (Right agentId)
    case result of
        Left err -> pure (Left err)
        Right restored -> do
            restoreSubagentIndex restored
            pure (Right restored)

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

restoreSubagentIndex :: SubagentId -> IO ()
restoreSubagentIndex agentId =
    case TextRead.decimal (snd (Text.breakOnEnd "-" agentId.unSubagentId)) of
        Right (index, rest) | Text.null rest ->
            atomicModifyIORef' subagentIdCounter
                (\current -> (max current index, ()))
        _ -> pure ()

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
queueMessageFromForTurn registry rootTurnId senderPath agentId content =
    atomically do
        closed <- readTVar registry.registryClosed
        aborted <- isRootTurnAborted registry rootTurnId
        if closed
            then pure (Left "Subagent registry is closed.")
            else if aborted
                then pure (Left "Root turn was aborted.")
                else do
                    agents <- readTVar registry.registryAgents
                    case Map.lookup agentId agents of
                        Nothing ->
                            pure (Left ("unknown agent id: " <> agentId.unSubagentId))
                        Just record -> do
                            status <- readTVar record.recordStatus
                            if status == Closed
                                then pure (Left "agent is closed")
                                else do
                                    let work = SubagentWork
                                            { workRootTurnId = rootTurnId
                                            , workMessage = InterAgentMessage
                                                { messageAuthor =
                                                    taskPathText senderPath
                                                , messageRecipient =
                                                    taskPathText record.recordTaskPath
                                                , messageType = QueuedMessage
                                                , messageContent = content
                                                }
                                            }
                                    writeTQueue record.recordMailbox work
                                    pure (Right "queued")

-- | Wait until any live non-final agent reaches a final status (or timeout).
waitAnyLive
    :: SubagentRegistry
    -> Int
    -> IO (Map SubagentId SubagentStatus, Bool)
waitAnyLive registry timeoutMs = do
    nonFinal <- fmap (map fst . filter (not . isFinalStatus . snd)) (listLive registry)
    if null nonFinal
        then do
            pairs <- listLive registry
            pure (Map.fromList pairs, False)
        else waitSubagents registry nonFinal timeoutMs
