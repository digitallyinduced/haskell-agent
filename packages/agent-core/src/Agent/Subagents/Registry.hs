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
    , closeSubagentRegistry
    , interruptActiveSubagents
    , resetSubagentRegistry
    , spawnSubagent
    , spawnSubagentWithCwd
    , spawnSubagentAt
    , restoreSubagent
    , restoreSubagentWithCwd
    , waitSubagents
    , waitAnyLive
    , sendInput
    , sendInputMessage
    , queueMessage
    , queueMessageFrom
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
import Data.Maybe (fromMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
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
    , recordMailbox :: !(TQueue InterAgentMessage)
    , recordWorker :: !(TVar (Maybe Worker))
      -- | Whether this agent currently occupies a concurrency slot.
    , recordSlotHeld :: !(TVar Bool)
      -- | Last successful response id for conversation continuity.
    , recordPreviousResponseId :: !(TVar (Maybe Text))
    , recordTaskPath :: !TaskPath
    , recordCwd :: !OsPath
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
spawnSubagentWithCwd registry childCwd parentId parentDepth message nickname = do
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
        spawnSubagentAtWithCwd
            registry childCwd parentId parentPath parentDepth taskName
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
    spawnSubagentAtWithCwd registry registry.registryCwd

spawnSubagentAtWithCwd
    :: SubagentRegistry
    -> OsPath
    -> Maybe SubagentId
    -> TaskPath
    -> Int
    -> Text
    -> InterAgentMessageContent
    -> Maybe Text
    -> IO (Either Text (SubagentId, TaskPath))
spawnSubagentAtWithCwd registry childCwd parentId parentPath parentDepth taskName content nickname = do
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
                workerVar <- newTVarIO Nothing
                slotHeld <- newTVarIO True
                previousVar <- newTVarIO Nothing
                let record = SubagentRecord
                        { recordId = agentId
                        , recordParent = parentId
                        , recordDepth = nextDepth
                        , recordNickname = nickname
                        , recordStatus = statusVar
                        , recordCancel = cancelFlag
                        , recordMailbox = mailbox
                        , recordWorker = workerVar
                        , recordSlotHeld = slotHeld
                        , recordPreviousResponseId = previousVar
                        , recordTaskPath = childPath
                        , recordCwd = childCwd
                        }
                admitted <- atomically do
                    closed <- readTVar registry.registryClosed
                    if closed
                        then pure (Left "Subagent registry is closed.")
                        else do
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
                    Right () -> do
                        let message = InterAgentMessage
                                { messageAuthor = taskPathText parentPath
                                , messageRecipient = taskPathText childPath
                                , messageType = NewTaskMessage
                                , messageContent = content
                                }
                        started <-
                            startRecordWorker registry record
                                (runWorker registry record message)
                                `onException` shutdownRecord registry record
                        if started
                            then pure (Right (agentId, childPath))
                            else do
                                shutdownRecord registry record
                                pure (Left "Subagent closed before its worker started.")

runWorker :: SubagentRegistry -> SubagentRecord -> InterAgentMessage -> IO ()
runWorker registry record firstPrompt = do
    mayRun <- atomically do
        closed <- readTVar registry.registryClosed
        current <- readTVar record.recordStatus
        if closed || current == Closed
            then pure False
            else do
                writeTVar record.recordStatus Running
                pure True
    let env = SubagentSpawnEnv
            { subId = record.recordId
            , subDepth = record.recordDepth
            , subParentId = record.recordParent
            , subCwd = record.recordCwd
            , subCancel = record.recordCancel
            }
        onEvent = registry.registryOnEvent record.recordId
        loop prompt = do
            previous <- atomically $ readTVar record.recordPreviousResponseId
            run <- readIORef registry.registryRunRef
            result <- tryAny (run env previous prompt onEvent)
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
                                msg <- readTQueue record.recordMailbox
                                writeTVar record.recordStatus Running
                                pure (Just msg)
            case next of
                -- Completed/errored/interrupted agents stay open and keep their
                -- concurrency slot until close_agent, matching Codex v1.
                Nothing -> notifyComplete registry record.recordId status
                Just msg -> loop msg
    whenIO mayRun (loop firstPrompt)

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
        current <- readTVar record.recordWorker
        case current of
            Just _ -> pure False
            Nothing
                | closed || status == Closed -> pure False
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
    mrecord <- atomically $ Map.lookup agentId <$> readTVar registry.registryAgents
    case mrecord of
        Nothing -> pure (Left ("unknown agent id: " <> agentId.unSubagentId))
        Just record -> do
            status <- atomically $ readTVar record.recordStatus
            case status of
                Closed -> pure (Left "agent is closed")
                NotFound -> pure (Left "agent not found")
                _ -> do
                    let message = InterAgentMessage
                            { messageAuthor = taskPathText senderPath
                            , messageRecipient = taskPathText record.recordTaskPath
                            , messageType = FollowUpMessage
                            , messageContent = content
                            }
                    whenIO interrupt (requestCancel record.recordCancel)
                    -- Admit before mutating status/mailbox so a failed resume
                    -- does not leave the agent stuck in Running with no worker.
                    kick <- atomically do
                        current <- readTVar record.recordStatus
                        case current of
                            Running -> do
                                writeTQueue record.recordMailbox message
                                pure KickNone
                            Pending -> do
                                writeTQueue record.recordMailbox message
                                pure KickNone
                            _ -> do
                                admitted <- acquireSlot registry record
                                case admitted of
                                    Left err -> pure (KickFail err)
                                    Right () -> do
                                        writeTQueue record.recordMailbox message
                                        writeTVar record.recordStatus Running
                                        pure KickStart
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
    mrecord <- atomically $ Map.lookup agentId <$> readTVar registry.registryAgents
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
    mworker <- atomically do
        writeTVar record.recordStatus Closed
        readTVar record.recordWorker
    case mworker of
        Nothing -> pure ()
        Just worker -> stopWorker worker
    releaseSlot registry record

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
    existing <- atomically $ Map.lookup agentId <$> readTVar registry.registryAgents
    case existing of
        Just record -> do
            -- Same-process resume after close: reopen without consuming a slot.
            atomically do
                writeTVar record.recordStatus (Completed Nothing)
                writeTVar record.recordPreviousResponseId previous
                writeTVar record.recordWorker Nothing
            pure (Right agentId)
        Nothing -> do
            cancelFlag <- newCancelFlag
            mailbox <- newTQueueIO
            statusVar <- newTVarIO (Completed Nothing)
            workerVar <- newTVarIO Nothing
            slotHeld <- newTVarIO False
            previousVar <- newTVarIO previous
            let record = SubagentRecord
                    { recordId = agentId
                    , recordParent = parentId
                    , recordDepth = depth
                    , recordNickname = nickname
                    , recordStatus = statusVar
                    , recordCancel = cancelFlag
                    , recordMailbox = mailbox
                    , recordWorker = workerVar
                    , recordSlotHeld = slotHeld
                    , recordPreviousResponseId = previousVar
                    , recordTaskPath = taskPathRoot
                    , recordCwd = childCwd
                    }
            atomically do
                closed <- readTVar registry.registryClosed
                if closed
                    then pure ()
                    else modifyTVar' registry.registryAgents (Map.insert agentId record)
            closed <- atomically $ readTVar registry.registryClosed
            if closed
                then pure (Left "Subagent registry is closed.")
                else pure (Right agentId)

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
    mrecord <- atomically $ Map.lookup agentId <$> readTVar registry.registryAgents
    case mrecord of
        Nothing -> pure (Left ("unknown agent id: " <> agentId.unSubagentId))
        Just record -> do
            status <- atomically $ readTVar record.recordStatus
            case status of
                Closed -> pure (Left "agent is closed")
                NotFound -> pure (Left "agent not found")
                _ -> do
                    let message = InterAgentMessage
                            { messageAuthor = taskPathText senderPath
                            , messageRecipient = taskPathText record.recordTaskPath
                            , messageType = QueuedMessage
                            , messageContent = content
                            }
                    atomically $ writeTQueue record.recordMailbox message
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
