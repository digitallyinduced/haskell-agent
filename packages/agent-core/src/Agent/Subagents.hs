-- | Nestable subagent registry.
--
-- Shared state (agent map, status, mailboxes, admission count) lives in STM.
-- IO is only used to allocate ids, start child 'Async' loops, and wait with
-- timeouts via 'threadDelay'.
module Agent.Subagents
    ( SubagentId(..)
    , SubagentStatus(..)
    , SubagentConfig(..)
    , SubagentSpawnEnv(..)
    , RunSubagent
    , SubagentRegistry
    , defaultSubagentConfig
    , defaultMaxConcurrent
    , defaultWaitTimeoutMs
    , minWaitTimeoutMs
    , maxWaitTimeoutMs
    , newSubagentRegistry
    , setSubagentRunner
    , setSubagentOnComplete
    , closeSubagentRegistry
    , spawnSubagent
    , restoreSubagent
    , waitSubagents
    , sendInput
    , closeSubagent
    , resumeSubagent
    , getStatus
    , getPreviousResponseId
    , listLive
    , encodeStatus
    , isFinalStatus
    , formatCompletionNotice
    ) where

import Agent.Cancel (CancelFlag, newCancelFlag, requestCancel)
import Agent.Loop (LoopError(..), LoopEvent, LoopResult(..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, waitCatch)
import Control.Concurrent.STM
import Control.Exception.Safe (SomeException, tryAny)
import Data.Aeson ((.=), object)
import qualified Data.Aeson as Aeson
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Numeric (showHex)
import System.IO.Unsafe (unsafePerformIO)

newtype SubagentId = SubagentId { unSubagentId :: Text }
    deriving (Eq, Ord, Show)

instance Aeson.ToJSON SubagentId where
    toJSON (SubagentId text) = Aeson.String text

instance Aeson.FromJSON SubagentId where
    parseJSON = Aeson.withText "SubagentId" (pure . SubagentId)

data SubagentStatus
    = Pending
    | Running
    | Completed !(Maybe Text)
    | Errored !Text
    | Interrupted
    | Closed
    | NotFound
    deriving (Eq, Show)

data SubagentConfig = SubagentConfig
    { maxConcurrent :: !Int
      -- | 'Nothing' means unlimited nesting depth.
    , maxDepth :: !(Maybe Int)
    } deriving (Eq, Show)

defaultMaxConcurrent :: Int
defaultMaxConcurrent = 6

defaultSubagentConfig :: SubagentConfig
defaultSubagentConfig = SubagentConfig
    { maxConcurrent = defaultMaxConcurrent
    , maxDepth = Nothing
    }

minWaitTimeoutMs :: Int
minWaitTimeoutMs = 10000

maxWaitTimeoutMs :: Int
maxWaitTimeoutMs = 3600 * 1000

defaultWaitTimeoutMs :: Int
defaultWaitTimeoutMs = 30000

data SubagentSpawnEnv = SubagentSpawnEnv
    { subId :: !SubagentId
    , subDepth :: !Int
    , subParentId :: !(Maybe SubagentId)
    , subCwd :: !FilePath
    , subCancel :: !CancelFlag
    }

-- | CLI/provider callback that runs one child agent loop for a prompt.
-- The 'Maybe Text' is the child's previous response id from an earlier turn
-- (so 'send_input' continues the same conversation).
type RunSubagent =
    SubagentSpawnEnv
    -> Maybe Text
    -> Text
    -> (LoopEvent -> IO ())
    -> IO (Either LoopError LoopResult)

data SubagentRecord = SubagentRecord
    { recordId :: !SubagentId
    , recordParent :: !(Maybe SubagentId)
    , recordDepth :: !Int
    , recordNickname :: !(Maybe Text)
    , recordStatus :: !(TVar SubagentStatus)
    , recordCancel :: !CancelFlag
    , recordMailbox :: !(TQueue Text)
    , recordAsync :: !(TVar (Maybe (Async ())))
      -- | Whether this agent currently occupies a concurrency slot.
    , recordSlotHeld :: !(TVar Bool)
      -- | Last successful response id for conversation continuity.
    , recordPreviousResponseId :: !(TVar (Maybe Text))
    }

data SubagentRegistry = SubagentRegistry
    { registryAgents :: !(TVar (Map SubagentId SubagentRecord))
    , registryLiveCount :: !(TVar Int)
    , registryConfig :: !SubagentConfig
    , registryRunRef :: !(IORef RunSubagent)
    , registryOnEvent :: !(SubagentId -> LoopEvent -> IO ())
    , registryOnCompleteRef :: !(IORef (SubagentId -> SubagentStatus -> IO ()))
    , registryCwd :: !FilePath
    , registryClosed :: !(TVar Bool)
    }

newSubagentRegistry
    :: SubagentConfig
    -> FilePath
    -> RunSubagent
    -> (SubagentId -> LoopEvent -> IO ())
    -> IO SubagentRegistry
newSubagentRegistry config cwd run onEvent = do
    agents <- newTVarIO Map.empty
    live <- newTVarIO 0
    closed <- newTVarIO False
    runRef <- newIORef run
    onCompleteRef <- newIORef (\_ _ -> pure ())
    pure SubagentRegistry
        { registryAgents = agents
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

spawnSubagent
    :: SubagentRegistry
    -> Maybe SubagentId
    -> Int
    -> Text
    -> Maybe Text
    -> IO (Either Text SubagentId)
spawnSubagent registry parentId parentDepth message nickname = do
    let nextDepth = parentDepth + 1
        cfg = registry.registryConfig
    case cfg.maxDepth of
        Just limit | nextDepth > limit ->
            pure $ Left "Agent depth limit reached. Solve the task yourself."
        _ -> do
            agentId <- newSubagentId
            cancelFlag <- newCancelFlag
            mailbox <- newTQueueIO
            statusVar <- newTVarIO Pending
            asyncVar <- newTVarIO Nothing
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
                    , recordAsync = asyncVar
                    , recordSlotHeld = slotHeld
                    , recordPreviousResponseId = previousVar
                    }
            admitted <- atomically do
                closed <- readTVar registry.registryClosed
                if closed
                    then pure (Left "Subagent registry is closed.")
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
                                pure (Right ())
            case admitted of
                Left err -> pure (Left err)
                Right () -> do
                    child <- async (runWorker registry record message)
                    atomically $ writeTVar asyncVar (Just child)
                    pure (Right agentId)

runWorker :: SubagentRegistry -> SubagentRecord -> Text -> IO ()
runWorker registry record firstPrompt = do
    atomically $ writeTVar record.recordStatus Running
    let env = SubagentSpawnEnv
            { subId = record.recordId
            , subDepth = record.recordDepth
            , subParentId = record.recordParent
            , subCwd = registry.registryCwd
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
                Nothing -> do
                    notifyComplete registry record.recordId status
                Just msg -> loop msg
    loop firstPrompt

notifyComplete :: SubagentRegistry -> SubagentId -> SubagentStatus -> IO ()
notifyComplete registry agentId status
    | isFinalStatus status && status /= Closed && status /= NotFound = do
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
    done <- newEmptyTMVarIO
    waiter <- async $ atomically do
        statuses <- mapM (readStatusSTM registry) targets
        let pairs = zip targets statuses
        if any (isFinalStatus . snd) pairs
            then putTMVar done (Map.fromList pairs, False)
            else retry
    timer <- async do
        threadDelay (clamped * 1000)
        atomically do
            statuses <- mapM (readStatusSTM registry) targets
            _ <- tryPutTMVar done (Map.fromList (zip targets statuses), True)
            pure ()
    result <- atomically (takeTMVar done)
    cancel waiter
    cancel timer
    pure result

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
sendInput registry agentId message interrupt = do
    mrecord <- atomically $ Map.lookup agentId <$> readTVar registry.registryAgents
    case mrecord of
        Nothing -> pure (Left ("unknown agent id: " <> agentId.unSubagentId))
        Just record -> do
            status <- atomically $ readTVar record.recordStatus
            case status of
                Closed -> pure (Left "agent is closed")
                NotFound -> pure (Left "agent not found")
                _ -> do
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
                            child <- async do
                                msg <- atomically $ readTQueue record.recordMailbox
                                runWorker registry record msg
                            atomically $ writeTVar record.recordAsync (Just child)
                            pure (Right "queued")

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
    masync <- atomically do
        writeTVar record.recordStatus Closed
        a <- readTVar record.recordAsync
        writeTVar record.recordAsync Nothing
        pure a
    case masync of
        Nothing -> pure ()
        Just child -> do
            cancel child
            _ <- tryAny (waitCatch child)
            pure ()
    releaseSlot registry record

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
restoreSubagent registry agentId parentId depth nickname previous = do
    existing <- atomically $ Map.lookup agentId <$> readTVar registry.registryAgents
    case existing of
        Just _ -> pure (Right agentId)
        Nothing -> do
            cancelFlag <- newCancelFlag
            mailbox <- newTQueueIO
            statusVar <- newTVarIO (Completed Nothing)
            asyncVar <- newTVarIO Nothing
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
                    , recordAsync = asyncVar
                    , recordSlotHeld = slotHeld
                    , recordPreviousResponseId = previousVar
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

isFinalStatus :: SubagentStatus -> Bool
isFinalStatus = \case
    Completed _ -> True
    Errored _ -> True
    Interrupted -> True
    Closed -> True
    NotFound -> True
    Pending -> False
    Running -> False

encodeStatus :: SubagentStatus -> Aeson.Value
encodeStatus = \case
    Pending -> Aeson.String "pending_init"
    Running -> Aeson.String "running"
    Interrupted -> Aeson.String "interrupted"
    Closed -> Aeson.String "shutdown"
    NotFound -> Aeson.String "not_found"
    Completed text -> object ["completed" .= text]
    Errored err -> object ["errored" .= err]

-- | Parent-facing notice when a child finishes without an active wait_agent.
formatCompletionNotice :: SubagentId -> SubagentStatus -> Text
formatCompletionNotice agentId status =
    "<subagent_notification>\n"
        <> "agent_id: "
        <> agentId.unSubagentId
        <> "\nstatus: "
        <> statusSummary status
        <> "\n</subagent_notification>"

statusSummary :: SubagentStatus -> Text
statusSummary = \case
    Completed (Just text) -> "completed\nfinal: " <> text
    Completed Nothing -> "completed"
    Errored err -> "errored\nerror: " <> err
    Interrupted -> "interrupted"
    Closed -> "shutdown"
    Pending -> "pending_init"
    Running -> "running"
    NotFound -> "not_found"

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
