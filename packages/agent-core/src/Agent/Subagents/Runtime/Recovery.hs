{-# LANGUAGE RankNTypes, OverloadedRecordDot, BlockArguments, LambdaCase #-}
-- | Durable tree recovery independent of database, transport, and domain.
module Agent.Subagents.Runtime.Recovery
    ( SubagentSnapshot (..)
    , RecoveryStore (..)
    , RecoveryConfig (..)
    , durableRuntimeHooks
    , restoreStoredSubagent
    , reconcileStoredSubagents
    , pumpStoredSubagents
    ) where

import Agent.Subagents
import Agent.Subagents.Runtime (RuntimeHandle (..), RuntimeHooks (..))
import Agent.Subagents.TaskPath (TaskPath)
import Agent.InterAgentMessage (InterAgentMessage)
import Control.Monad (forM_, unless, void, when)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import System.OsPath (OsPath)

-- | Metadata only: dormant agents never require loading their transcripts.
data SubagentSnapshot = SubagentSnapshot
    { agentId :: !SubagentId
    , parentId :: !(Maybe SubagentId)
    , taskPath :: !TaskPath
    , depth :: !Int
    , status :: !SubagentStatus
    , initialMessage :: !Text
    , wakeRequested :: !Bool
    }

-- | Transactions must encompass the status write and domain settlement.
-- Locking serializes reconciliation with the runner's admitted-input commit.
data RecoveryStore = RecoveryStore
    { loadSnapshots :: IO [SubagentSnapshot]
    , loadSnapshot :: SubagentId -> IO (Maybe SubagentSnapshot)
    , transaction :: forall a. IO a -> IO a
    , lockSnapshot :: SubagentId -> IO ()
    , persistStatus :: SubagentId -> SubagentStatus -> IO ()
    , persistRunning :: SubagentId -> InterAgentMessage -> IO ()
    , persistSuspension :: SubagentId -> UTCTime -> Text -> IO (Either Text ())
    , persistResume :: SubagentId -> InterAgentMessage -> IO (Either Text ())
    }

data RecoveryConfig = RecoveryConfig
    { store :: RecoveryStore
    , withOwnership :: forall a. IO a -> IO (Maybe a)
    , executionAllowed :: IO Bool
    , runner :: RuntimeHandle -> RunSubagent
    , settle :: SubagentId -> SubagentStatus -> IO ()
    , notify :: SubagentId -> SubagentStatus -> IO ()
    , cwd :: OsPath
    , wakeMessage :: Text
    }

durableRuntimeHooks :: RecoveryConfig -> RuntimeHooks
durableRuntimeHooks config@RecoveryConfig { withOwnership = ownership } = RuntimeHooks
    { withOwnership = ownership
    , initialize = \handle -> do
        allowed <- config.executionAllowed
        unless allowed (fail "Cannot restore a stopped or unauthorized agent tree")
        setSubagentRunner handle.registry \environment previous message onEvent -> do
            config.store.persistRunning environment.subId message
            config.runner handle environment previous message onEvent
        setSubagentSuspensionPersistence handle.registry SuspensionPersistence
            { persistSuspension = config.store.persistSuspension
            , persistSuspensionResume = config.store.persistResume
            , persistSuspensionCancellation = \agentId -> do
                commitStatus config agentId Closed
                pure (Right ())
            }
        setSubagentOnSettled handle.registry \agentId status -> do
            commitStatus config agentId status
            config.notify agentId status
        snapshots <- config.store.loadSnapshots
        forM_ snapshots \snapshot ->
            restoreSnapshot config.cwd handle snapshot >>= either (fail . Text.unpack) pure
        pumpStoredSubagents config handle
    , pumpRunnable = \handle -> do
        allowed <- config.executionAllowed
        if allowed
            then do
                reconcileStoredSubagents config handle
                pumpStoredSubagents config handle
                pure True
            else do
                -- Access loss is cancellation, not process retirement.
                closeSubagentRegistry handle.registry
                snapshots <- config.store.loadSnapshots
                forM_ snapshots \snapshot -> commitStatus config snapshot.agentId Closed
                pure False
    }

commitStatus :: RecoveryConfig -> SubagentId -> SubagentStatus -> IO ()
commitStatus config agentId status =
    withStoreTransaction config.store do
        config.store.persistStatus agentId status
        config.settle agentId status

-- | A persisted running attempt resumes as interrupted, never as a phantom
-- live worker. Pending work follows the same admission path.
restoredStatus :: SubagentStatus -> SubagentStatus
restoredStatus Pending = Interrupted
restoredStatus Running = Interrupted
restoredStatus status = status

restoreSnapshot :: OsPath -> RuntimeHandle -> SubagentSnapshot -> IO (Either Text ())
restoreSnapshot cwd handle snapshot =
    fmap (const ()) <$> restoreSubagentAtWithCwdStatus handle.registry cwd
        snapshot.agentId snapshot.parentId snapshot.taskPath snapshot.depth
        Nothing Nothing (restoredStatus snapshot.status)

restoreStoredSubagent
    :: RecoveryStore -> OsPath -> RuntimeHandle -> SubagentId -> IO (Either Text ())
restoreStoredSubagent store cwd handle agentId = do
    current <- getStatus handle.registry agentId
    if current /= NotFound then pure (Right ()) else do
        stored <- store.loadSnapshot agentId
        case stored of
            Nothing -> pure (Left "Unknown or permanently closed subagent")
            Just snapshot | snapshot.status == Closed -> pure (Left "Unknown or permanently closed subagent")
            Just snapshot -> restoreSnapshot cwd handle snapshot

-- | Retry failed settlement without repeating model execution. Read durable
-- status after acquiring its lock, not from a potentially stale inventory.
reconcileStoredSubagents :: RecoveryConfig -> RuntimeHandle -> IO ()
reconcileStoredSubagents config handle = do
    inventory <- config.store.loadSnapshots
    forM_ inventory \snapshot -> do
        changed <- withStoreTransaction config.store do
            config.store.lockSnapshot snapshot.agentId
            stored <- config.store.loadSnapshot snapshot.agentId
            current <- getStatus handle.registry snapshot.agentId
            case stored of
                Just stored | settledStatus current -> do
                    let changed = restoredStatus stored.status /= current
                    when changed (config.store.persistStatus snapshot.agentId current)
                    config.settle snapshot.agentId current
                    pure (if changed then Just current else Nothing)
                _ -> pure Nothing
        forM_ changed (config.notify snapshot.agentId)
  where
    settledStatus = \case
        Completed {} -> True
        Errored {} -> True
        Closed -> True
        Suspended {} -> True
        _ -> False

-- | Admission pressure leaves inputs in durable storage for the next pass.
-- Only persistRunning acknowledges the input actually admitted by the registry.
pumpStoredSubagents :: RecoveryConfig -> RuntimeHandle -> IO ()
pumpStoredSubagents config handle = do
    snapshots <- config.store.loadSnapshots
    forM_ snapshots \snapshot -> do
        current <- getStatus handle.registry snapshot.agentId
        let recover = current == Interrupted && snapshot.status `elem` [Pending, Running]
            wake = snapshot.wakeRequested && case current of
                Suspended {} -> True
                Completed {} -> True
                Errored {} -> True
                Interrupted -> True
                _ -> False
        when (recover || wake) $
            void (sendInput handle.registry snapshot.agentId
                (if recover then snapshot.initialMessage else config.wakeMessage) False)

withStoreTransaction :: RecoveryStore -> IO a -> IO a
withStoreTransaction RecoveryStore { transaction = action } = action
