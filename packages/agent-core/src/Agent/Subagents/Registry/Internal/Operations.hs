-- | Registry messaging, waiting, interruption, restoration, and queries.
module Agent.Subagents.Registry.Internal.Operations where


import Agent.Cancel
    ( CancelFlag
    , newCancelFlag
    , requestCancel
    , resetCancel
    , waitCancel
    )
import Agent.Concurrent
    ( forConcurrentlyBounded_
    , mapConcurrentlyBounded
    )
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
    , SubagentIdentity(..)
    , SubagentAccessProfile(..)
    , SubagentSpawnEnv(..)
    , SubagentStatus(..)
    , maxWaitTimeoutMs
    , minWaitTimeoutMs
    )
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, race, waitCatch)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM
import Control.Exception.Safe
    ( SomeException
    , catchAny
    , finally
    , mask
    , onException
    , throwIO
    , tryAny
    )
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Acquire (Acquire, allocateAcquire, mkAcquire, withAcquire)
import Data.IORef
import Data.List (groupBy, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (Down(..))
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
import Agent.Subagents.Registry.Internal.Core
import Agent.Subagents.Registry.Internal.Types

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
                        status <- atomically $
                            phaseStatus <$> readTVar record.recordPhase
                        let work = SubagentWork
                                { workRootTurnId = rootTurnId
                                , workMessage = InterAgentMessage
                                    { messageAuthor = taskPathText senderPath
                                    , messageRecipient =
                                        taskPathText record.recordTaskPath
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
        queued <- atomically do
            aborted <- isRootTurnAborted registry rootTurnId
            if aborted
                then pure False
                else writeTQueue record.recordMailbox work >> pure True
        whenIO (queued && interrupt) (requestCancel record.recordCancel)
        pure $ if queued
            then Right "queued"
            else Left "Root turn was aborted."

    restart
        :: SubagentRecord
        -> SubagentWork
        -> IO (Either Text Text)
    restart record work = do
        resetCancel record.recordCancel
        admitted <- atomically do
            aborted <- isRootTurnAborted registry rootTurnId
            if aborted
                then pure (Left "Root turn was aborted.")
                else scheduleIdleWork registry record work
        case admitted of
            Left err -> pure (Left err)
            Right () -> pure (Right "queued")

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
                previous <- atomically $
                    phaseStatus <$> readTVar record.recordPhase
                toClose <- atomically do
                    agents <- readTVar registry.registryAgents
                    pure (descendants agents record.recordId <> [record])
                mapM_ (shutdownRecord registry) toClose
                pure (Right previous)

abortRootTurn :: SubagentRegistry -> RootTurnId -> IO ()
abortRootTurn registry rootTurnId =
    withMVar registry.registryLifecycle \_ -> do
        records <- atomically do
            modifyTVar' registry.registryAbortedRootTurns (Set.insert rootTurnId)
            agents <- Map.elems <$> readTVar registry.registryAgents
            fmap concat $ mapM selectOwned agents
        settled <- mapConcurrentlyBounded 8
            (interruptRecordForTurn registry rootTurnId)
            records
        mapM_
            (\record -> notifySettled registry record.recordId Interrupted)
            [record | (record, True) <- zip records settled]
  where
    selectOwned :: SubagentRecord -> STM [SubagentRecord]
    selectOwned record = do
        discardQueuedWork rootTurnId record.recordMailbox
        phase <- readTVar record.recordPhase
        pure
            [ record
            | phaseRootTurnId phase == Just rootTurnId
            , phaseStatus phase == Pending || phaseStatus phase == Running
            ]

interruptRecordForTurn :: SubagentRegistry -> RootTurnId -> SubagentRecord -> IO Bool
interruptRecordForTurn registry rootTurnId record = do
    disposition <- atomically do
        phase <- readTVar record.recordPhase
        if phaseRootTurnId phase /= Just rootTurnId
            then pure InterruptUnchanged
            else case phase of
                AgentPending{} -> do
                    releaseSlotSTM registry record
                    writeTVar record.recordPhase
                        (AgentIdle Interrupted (Just rootTurnId))
                    pure InterruptSettled
                AgentRunning owner -> do
                    writeTVar record.recordPhase (AgentInterrupting owner)
                    pure InterruptWait
                AgentInterrupting{} -> pure InterruptWait
                AgentIdle{} -> pure InterruptUnchanged
                AgentClosed -> pure InterruptUnchanged
    case disposition of
        InterruptUnchanged -> pure False
        InterruptSettled -> pure True
        InterruptWait -> do
            requestCancel record.recordCancel
            atomically $ waitForReleasedSlot record
            pure True

waitForReleasedSlot :: SubagentRecord -> STM ()
waitForReleasedSlot record = do
    phase <- readTVar record.recordPhase
    whenSTM (phaseHoldsSlot phase) retry

discardQueuedWork :: RootTurnId -> TQueue SubagentWork -> STM ()
discardQueuedWork rootTurnId mailbox = do
    queued <- flushTQueue mailbox
    mapM_ (writeTQueue mailbox)
        [ work
        | work <- queued
        , work.workRootTurnId /= Just rootTurnId
        ]

-- | Stop every currently pending/running child while keeping completed agent
-- records and the registry available for later turns. The registry is closed
-- during the transition so a descendant cannot publish newly-started work
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
        (do
            settled <- mapConcurrentlyBounded 8
                (interruptRecord registry)
                records
            mapM_
                (\record -> notifySettled registry record.recordId Interrupted)
                [record | (record, True) <- zip records settled])
            `finally`
                atomically (writeTVar registry.registryClosed wasClosed)
  where
    isActiveRecord :: SubagentRecord -> STM Bool
    isActiveRecord record = do
        status <- phaseStatus <$> readTVar record.recordPhase
        pure (status == Pending || status == Running)

interruptRecord :: SubagentRegistry -> SubagentRecord -> IO Bool
interruptRecord registry record = do
    disposition <- atomically do
        phase <- readTVar record.recordPhase
        void $ flushTQueue record.recordMailbox
        case phase of
            AgentPending{} -> do
                transitionToIdleSTM registry record Interrupted
                pure InterruptSettled
            AgentRunning owner -> do
                writeTVar record.recordPhase (AgentInterrupting owner)
                pure InterruptWait
            AgentInterrupting{} -> pure InterruptWait
            -- The record may have completed after the active snapshot was
            -- taken. Preserve that published final status rather than
            -- replacing it with an administrative interruption.
            AgentIdle{} -> pure InterruptUnchanged
            AgentClosed -> pure InterruptUnchanged
    case disposition of
        InterruptUnchanged -> pure False
        InterruptSettled -> pure True
        InterruptWait -> do
            requestCancel record.recordCancel
            atomically $ waitForReleasedSlot record
            pure True

data InterruptDisposition
    = InterruptUnchanged
    | InterruptSettled
    | InterruptWait

filterMSTM :: (a -> STM Bool) -> [a] -> STM [a]
filterMSTM predicate = fmap reverse . go []
  where
    go kept [] = pure kept
    go kept (value : rest) = do
        include <- predicate value
        go (if include then value : kept else kept) rest

-- | Re-admit a previously persisted agent that is not currently in the
-- in-memory map (e.g. after close, or across a process restart within the
-- same session directory). Starts an idle supervisor; callers follow with
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

restoreSubagentAtStatus
    :: SubagentRegistry
    -> SubagentId
    -> Maybe SubagentId
    -> TaskPath
    -> Int
    -> Maybe Text
    -> Maybe Text
    -> SubagentStatus
    -> IO (Either Text SubagentId)
restoreSubagentAtStatus registry =
    restoreSubagentAtWithCwdStatus registry registry.registryCwd

restoreSubagentWithCwd
    :: SubagentRegistry
    -> OsPath
    -> SubagentId
    -> Maybe SubagentId
    -> Int
    -> Maybe Text
    -> Maybe Text
    -> IO (Either Text SubagentId)
restoreSubagentWithCwd
        registry childCwd agentId parentId depth nickname previous =
    restoreSubagentResolvedWithCwd
        registry childCwd agentId parentId nickname previous
        (Completed Nothing) \agents ->
            resolveParentSTM agents parentId taskPathRoot (max 0 (depth - 1))
                >>= \case
                    Left err -> pure (Left err)
                    Right (parentPath, actualDepth) ->
                        pure $
                            fmap
                                (\childPath -> (childPath, actualDepth))
                                (joinTaskPath parentPath (taskNameForAgentId agentId))

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
    restoreSubagentAtWithCwdStatus
        registry childCwd agentId parentId taskPath depth nickname previous
        (Completed Nothing)

restoreSubagentAtWithCwdStatus
    :: SubagentRegistry
    -> OsPath
    -> SubagentId
    -> Maybe SubagentId
    -> TaskPath
    -> Int
    -> Maybe Text
    -> Maybe Text
    -> SubagentStatus
    -> IO (Either Text SubagentId)
restoreSubagentAtWithCwdStatus
        registry childCwd agentId parentId taskPath depth nickname previous restoredStatus =
    restoreSubagentResolvedWithCwd
        registry childCwd agentId parentId nickname previous restoredStatus
        (\_ -> pure (Right (taskPath, depth)))

restoreSubagentResolvedWithCwd
    :: SubagentRegistry
    -> OsPath
    -> SubagentId
    -> Maybe SubagentId
    -> Maybe Text
    -> Maybe Text
    -> SubagentStatus
    -> (Map SubagentId SubagentRecord -> STM (Either Text (TaskPath, Int)))
    -> IO (Either Text SubagentId)
restoreSubagentResolvedWithCwd
        registry childCwd agentId parentId nickname previous
        restoredStatus resolveIdentity = do
    result <-
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
    case result of
        Left err -> pure (Left err)
        Right restored -> do
            restoreSubagentIndex registry restored
            pure (Right restored)
  where
    normalizedStatus = case restoredStatus of
        Pending -> Interrupted
        Running -> Interrupted
        NotFound -> Interrupted
        status -> status

    normalizedPhase = case normalizedStatus of
        Closed -> AgentClosed
        status -> AgentIdle status Nothing

    restoreExisting record = do
        status <- atomically $
            phaseStatus <$> readTVar record.recordPhase
        case status of
            Running -> pure (Left "cannot restore a running subagent")
            Pending -> pure (Left "cannot restore a pending subagent")
            NotFound -> pure (Left "cannot restore a missing subagent record")
            _ -> do
                resetCancel record.recordCancel
                atomically do
                    releaseSlotSTM registry record
                    writeTVar record.recordPhase normalizedPhase
                    writeTVar record.recordPreviousResponseId previous
                    modifyTVar' registry.registryPaths
                        (Map.insert record.recordTaskPath agentId)
                restarted <- if status == Closed
                    then do
                        atomically $
                            writeTVar record.recordPhase
                                (AgentIdle normalizedStatus Nothing)
                        startRecordSupervisor registry record mempty
                    else pure (Right ())
                case restarted of
                    Left err -> do
                        atomically $ writeTVar record.recordPhase AgentClosed
                        pure (Left err)
                    Right () -> pure (Right agentId)

    restoreMissing = do
        cancelFlag <- newCancelFlag
        mailbox <- newTQueueIO
        phaseVar <- newTVarIO normalizedPhase
        asyncVar <- newTVarIO Nothing
        previousVar <- newTVarIO previous
        lastUpdateVar <- newTVarIO Nothing
        accessProfileVar <- newTVarIO SubagentFullAccess
        restored <- atomically do
            closed <- readTVar registry.registryClosed
            if closed
                then pure (Left "Subagent registry is closed.")
                else do
                    agents <- readTVar registry.registryAgents
                    identity <- resolveIdentity agents
                    case identity of
                        Left err -> pure (Left err)
                        Right (resolvedPath, resolvedDepth) -> do
                            paths <- readTVar registry.registryPaths
                            case Map.lookup resolvedPath paths of
                                Just owner | owner /= agentId ->
                                    pure $ Left $
                                        "task path already in use: "
                                            <> taskPathText resolvedPath
                                _ -> do
                                    let record = SubagentRecord
                                            { recordId = agentId
                                            , recordParent = parentId
                                            , recordDepth = resolvedDepth
                                            , recordNickname = nickname
                                            , recordPhase = phaseVar
                                            , recordCancel = cancelFlag
                                            , recordMailbox = mailbox
                                            , recordAsync = asyncVar
                                            , recordPreviousResponseId = previousVar
                                            , recordLastUpdate = lastUpdateVar
                                            , recordTaskPath = resolvedPath
                                            , recordCwd = childCwd
                                            , recordAccessProfile =
                                                accessProfileVar
                                            }
                                    writeTVar registry.registryAgents
                                        (Map.insert agentId record agents)
                                    whenSTM (resolvedPath /= taskPathRoot) $
                                        writeTVar registry.registryPaths
                                            (Map.insert resolvedPath agentId paths)
                                    pure (Right record)
        case restored of
            Left err -> pure (Left err)
            Right record -> do
                atomically $ case normalizedPhase of
                    AgentClosed ->
                        writeTVar record.recordPhase
                            (AgentIdle normalizedStatus Nothing)
                    _ -> pure ()
                startRecordSupervisor registry record mempty >>= \case
                    Left err -> do
                        rollbackAdmission registry record
                        pure (Left err)
                    Right () -> pure (Right agentId)

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

-- | Return the host-enforced profile for a resident subagent.
getSubagentAccessProfile
    :: SubagentRegistry
    -> SubagentId
    -> IO (Maybe SubagentAccessProfile)
getSubagentAccessProfile registry agentId = atomically do
    agents <- readTVar registry.registryAgents
    traverse (readTVar . (.recordAccessProfile)) (Map.lookup agentId agents)

-- | Monotonically restrict an admitted/restored child.
--
-- The operation intentionally cannot upgrade a read-only record back to full
-- access.  Spawn preparation calls this before the supervisor starts; restore
-- paths call it before accepting any follow-up.
restrictSubagentAccess
    :: SubagentRegistry
    -> SubagentId
    -> SubagentAccessProfile
    -> IO (Either Text ())
restrictSubagentAccess registry agentId requested = atomically do
    agents <- readTVar registry.registryAgents
    case Map.lookup agentId agents of
        Nothing -> pure (Left ("unknown agent id: " <> agentId.unSubagentId))
        Just record -> do
            current <- readTVar record.recordAccessProfile
            let restricted
                    | current == SubagentReadOnly = SubagentReadOnly
                    | requested == SubagentReadOnly = SubagentReadOnly
                    | otherwise = SubagentFullAccess
            writeTVar record.recordAccessProfile restricted
            pure (Right ())

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
        Just record -> phaseStatus <$> readTVar record.recordPhase

restoreSubagentIndex :: SubagentRegistry -> SubagentId -> IO ()
restoreSubagentIndex registry agentId =
    case TextRead.decimal (snd (Text.breakOnEnd "-" agentId.unSubagentId)) of
        Right (index, rest) | Text.null rest ->
            atomically $
                modifyTVar' registry.registryNextSubagentId (max index)
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
            status <- phaseStatus <$> readTVar record.recordPhase
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
interruptSubagent registry agentId =
    -- Serialize pending settlement and its callback with lifecycle-mutating
    -- operations, so a follow-up cannot race the transition or callback.
    withMVar registry.registryLifecycle \_ -> do
        mrecord <-
            atomically $ Map.lookup agentId <$> readTVar registry.registryAgents
        case mrecord of
            Nothing -> pure (Left ("unknown agent id: " <> agentId.unSubagentId))
            Just record -> do
                (previous, settledPending) <- atomically do
                    phase <- readTVar record.recordPhase
                    case phase of
                        AgentPending{} -> do
                            releaseSlotSTM registry record
                            tryReadTQueue record.recordMailbox >>= \case
                                Just nextWork -> do
                                    modifyTVar' registry.registryLiveCount (+ 1)
                                    writeTVar record.recordPhase
                                        (AgentPending nextWork)
                                Nothing ->
                                    writeTVar record.recordPhase
                                        (AgentIdle Interrupted
                                            (phaseRootTurnId phase))
                            publishDirectUpdateSTM registry record Interrupted
                            pure (phaseStatus phase, True)
                        _ -> pure (phaseStatus phase, False)
                if settledPending
                    then notifySettled registry record.recordId Interrupted
                    else requestCancel record.recordCancel
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
    withMVar registry.registryLifecycle \_ ->
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
                                pure (Left
                                    ("unknown agent id: " <> agentId.unSubagentId))
                            Just record -> do
                                status <- phaseStatus <$> readTVar record.recordPhase
                                if status == Closed
                                    then pure (Left "agent is closed")
                                    else do
                                        let work = SubagentWork
                                                { workRootTurnId = rootTurnId
                                                , workMessage = InterAgentMessage
                                                    { messageAuthor =
                                                        taskPathText senderPath
                                                    , messageRecipient =
                                                        taskPathText
                                                            record.recordTaskPath
                                                    , messageType = QueuedMessage
                                                    , messageContent = content
                                                    }
                                                }
                                        writeTQueue record.recordMailbox work
                                        pure (Right "queued")

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
