-- | Durable suspension admission. Provider connections never sleep until a
-- deadline: the child yields after its tool batch and releases its turn slot.
module Agent.Subagents.Registry.Internal.Suspension where

import Agent.Cancel (CancelFlag, newCancelFlag, requestTurnYield)
import Agent.InterAgentMessage (InterAgentMessage)
import Agent.Subagents.Registry.Internal.Types
import Agent.Subagents.Types
import Control.Concurrent.MVar (withMVar)
import Control.Concurrent.STM
import Control.Exception.Safe (bracket, throwIO)
import Data.IORef (readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime, getCurrentTime)

setSubagentSuspensionPersistence :: SubagentRegistry -> SuspensionPersistence -> IO ()
setSubagentSuspensionPersistence registry =
    writeIORef registry.registrySuspensionPersistence . Just

-- | Scoped soft-cancel signal, independent of administrative hard cancellation.
withSuspensionSignal :: SubagentRegistry -> SubagentId -> (CancelFlag -> IO a) -> IO a
withSuspensionSignal registry agentId = bracket acquire release
  where
    acquire = do
        signal <- newCancelFlag
        atomically $ modifyTVar' registry.registrySuspensionSignals (Map.insert agentId signal)
        pure signal
    release _ =
        atomically $ modifyTVar' registry.registrySuspensionSignals (Map.delete agentId)

requestSubagentSuspension
    :: SubagentRegistry -> SubagentId -> UTCTime -> Text -> IO (Either Text ())
requestSubagentSuspension registry agentId deadline reason =
    withMVar registry.registryLifecycle \_ -> do
        now <- getCurrentTime
        storage <- readIORef registry.registrySuspensionPersistence
        eligible <- atomically do
            closed <- readTVar registry.registryClosed
            agents <- readTVar registry.registryAgents
            signals <- readTVar registry.registrySuspensionSignals
            case (closed, Map.lookup agentId agents, Map.lookup agentId signals) of
                (False, Just record, Just signal) -> do
                    phase <- readTVar record.recordPhase
                    pure $ case phase of
                        AgentRunning{} -> Just signal
                        _ -> Nothing
                _ -> pure Nothing
        if deadline <= now
            then pure (Left "Suspension deadline must be in the future.")
            else if Text.null (Text.strip reason) || Text.length reason > 4000
                then pure (Left "Suspension reason must contain 1–4000 characters.")
                else case (storage, eligible) of
                    (Nothing, _) -> pure (Left "Durable suspension storage is not configured.")
                    (_, Nothing) -> pure (Left "Only an active child turn can suspend itself.")
                    (Just _, Just signal) -> do
                        atomically $ modifyTVar' registry.registrySuspensions
                            (Map.insert agentId (deadline, reason))
                        requestTurnYield signal
                        pure (Right ())

-- | Called only after the runner has returned and checkpointed completed tool
-- results. The host can now atomically save that transcript and suspended state.
commitRequestedSuspension :: SubagentRegistry -> SubagentId -> IO ()
commitRequestedSuspension registry agentId =
    withMVar registry.registryLifecycle \_ -> do
        request <- atomically $ Map.lookup agentId <$> readTVar registry.registrySuspensions
        case request of
            Nothing -> pure ()
            Just (deadline, reason) -> do
                storage <- readIORef registry.registrySuspensionPersistence
                result <- case storage of
                    Nothing -> pure (Left "Durable suspension storage is not configured.")
                    Just persistence -> persistence.persistSuspension agentId deadline reason
                case result of
                    Right () -> pure ()
                    Left err -> do
                        atomically $ modifyTVar' registry.registrySuspensions (Map.delete agentId)
                        throwIO (userError (Text.unpack err))

persistResume :: SubagentRegistry -> SubagentId -> InterAgentMessage -> IO (Either Text ())
persistResume registry agentId message =
    readIORef registry.registrySuspensionPersistence >>= \case
        Nothing -> pure (Left "Durable suspension storage is not configured.")
        Just storage -> storage.persistSuspensionResume agentId message

cancelSuspension :: SubagentRegistry -> SubagentRecord -> IO ()
cancelSuspension registry record = do
    requested <- atomically $ Map.member record.recordId <$> readTVar registry.registrySuspensions
    status <- atomically $ phaseStatus <$> readTVar record.recordPhase
    let suspended = case status of
            Suspended{} -> True
            _ -> False
    if requested || suspended
        then do
            storage <- readIORef registry.registrySuspensionPersistence
            result <- case storage of
                Nothing -> pure (Left "Durable suspension storage is not configured.")
                Just persistence -> persistence.persistSuspensionCancellation record.recordId
            either (throwIO . userError . Text.unpack) pure result
            atomically $ modifyTVar' registry.registrySuspensions (Map.delete record.recordId)
        else pure ()
