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
    , cancellationRequested :: !Bool
    , settled :: !Bool
    -- ^ 'settle' committed after the last write of this status.
    }

-- | Transactions must encompass the status write and domain settlement.
-- Locking serializes reconciliation with the runner's admitted-input commit.
-- The transaction supplies a fresh callback bundle bound to its connection;
-- entering a transaction cannot rebind closures captured in the outer store.
-- Every status write clears a child's 'settled' mark; 'settle' sets it.
data RecoveryStore = RecoveryStore
    { loadSnapshots :: IO [SubagentSnapshot]
    , loadSnapshot :: SubagentId -> IO (Maybe SubagentSnapshot)
    , transaction :: forall a. (RecoveryStore -> IO a) -> IO a
    , lockSnapshot :: SubagentId -> IO ()
    , persistStatus :: SubagentId -> SubagentStatus -> IO ()
    , persistRunning :: SubagentId -> InterAgentMessage -> IO ()
    , persistSuspension :: SubagentId -> UTCTime -> Text -> IO (Either Text ())
    , persistResume :: SubagentId -> InterAgentMessage -> IO (Either Text ())
    , settle :: SubagentId -> SubagentStatus -> IO ()
    }

data RecoveryConfig = RecoveryConfig
    { store :: RecoveryStore
    , withOwnership :: forall a. IO a -> IO (Maybe a)
    , withOperation :: forall a. IO a -> IO a
    , withExecution :: forall a. IO a -> IO a
    , executionAllowed :: IO Bool
    , runner :: RuntimeHandle -> RunSubagent
    , notify :: SubagentId -> SubagentStatus -> IO ()
    , cwd :: OsPath
    , wakeMessage :: Text
    }

durableRuntimeHooks :: RecoveryConfig -> RuntimeHooks
durableRuntimeHooks config@RecoveryConfig { withOwnership = ownership, withOperation = operation, withExecution = execution } = RuntimeHooks
    { withOwnership = ownership
    , withOperation = operation
    , withExecution = execution
    , initialize = \handle -> withHandleExecution handle do
        allowed <- config.executionAllowed
        unless allowed (fail "Cannot restore a stopped or unauthorized agent tree")
        setSubagentRunner handle.registry \environment previous message onEvent -> withHandleExecution handle do
            withHandleOperation handle (config.store.persistRunning environment.subId message)
            config.runner handle environment previous message onEvent
        setSubagentSuspensionPersistence handle.registry SuspensionPersistence
            { persistSuspension = \agentId deadline reason ->
                withHandleOperation handle (config.store.persistSuspension agentId deadline reason)
            , persistSuspensionResume = \agentId message ->
                withHandleOperation handle (config.store.persistResume agentId message)
            , persistSuspensionCancellation = \agentId -> withHandleOperation handle do
                commitStatus config agentId Closed
                pure (Right ())
            }
        setSubagentOnSettled handle.registry \agentId status -> withHandleOperation handle do
            commitStatus config agentId status
            config.notify agentId status
        snapshots <- config.store.loadSnapshots
        forM_ snapshots \snapshot ->
            restoreSnapshot config.cwd handle snapshot >>= either (fail . Text.unpack) pure
        processCancellations config handle
        pumpStoredSubagents config handle
    , pumpRunnable = \handle -> withHandleExecution handle do
        allowed <- config.executionAllowed
        if allowed
            then do
                processCancellations config handle
                reconcileStoredSubagents config handle
                pumpStoredSubagents config handle
                pure True
            else do
                -- Access loss is cancellation, not process retirement.
                closeSubagentRegistry handle.registry
                snapshots <- config.store.loadSnapshots
                forM_ snapshots \snapshot -> withHandleOperation handle (commitStatus config snapshot.agentId Closed)
                pure False
    }

commitStatus :: RecoveryConfig -> SubagentId -> SubagentStatus -> IO ()
commitStatus config agentId status =
    withStoreTransaction config.store \store -> do
        store.persistStatus agentId status
        store.settle agentId status

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
restoreStoredSubagent store cwd handle agentId = withHandleExecution handle do
    current <- getStatus handle.registry agentId
    if current /= NotFound then pure (Right ()) else do
        stored <- store.loadSnapshot agentId
        case stored of
            Nothing -> pure (Left "Unknown or permanently closed subagent")
            Just snapshot | snapshot.status == Closed || snapshot.cancellationRequested ->
                pure (Left "Unknown or permanently closed subagent")
            Just snapshot -> restoreSnapshot cwd handle snapshot

-- | Retry failed settlement without repeating model execution. Read durable
-- status after acquiring its lock, not from a potentially stale inventory.
-- The inventory only rules work out: a finished child that a tree keeps for
-- later replies costs no transaction once its settlement has committed.
reconcileStoredSubagents :: RecoveryConfig -> RuntimeHandle -> IO ()
reconcileStoredSubagents config handle = withHandleExecution handle do
    inventory <- config.store.loadSnapshots
    forM_ inventory \snapshot -> do
        observed <- getStatus handle.registry snapshot.agentId
        when (mayNeedReconciliation snapshot observed) do
            changed <- withHandleOperation handle $ withStoreTransaction config.store \store -> do
                store.lockSnapshot snapshot.agentId
                stored <- store.loadSnapshot snapshot.agentId
                current <- getStatus handle.registry snapshot.agentId
                case stored of
                    Just stored | not stored.cancellationRequested && settledStatus current -> do
                        let changed = restoredStatus stored.status /= current
                        when changed (store.persistStatus snapshot.agentId current)
                        store.settle snapshot.agentId current
                        pure (if changed then Just current else Nothing)
                    _ -> pure Nothing
            forM_ changed (\status -> withHandleOperation handle (config.notify snapshot.agentId status))

-- | False only where the locked transaction would at most repeat a committed
-- settlement: the registry reports no settled status, 'processCancellations'
-- owns the child, or the registry still reports its settled stored status.
mayNeedReconciliation :: SubagentSnapshot -> SubagentStatus -> Bool
mayNeedReconciliation snapshot observed =
    not snapshot.cancellationRequested
        && settledStatus observed
        && not (snapshot.settled && restoredStatus snapshot.status == observed)

settledStatus :: SubagentStatus -> Bool
settledStatus = \case
    Completed {} -> True
    Errored {} -> True
    Closed -> True
    Suspended {} -> True
    _ -> False

-- | Admission pressure leaves inputs in durable storage for the next pass.
-- Only persistRunning acknowledges the input actually admitted by the registry.
pumpStoredSubagents :: RecoveryConfig -> RuntimeHandle -> IO ()
pumpStoredSubagents config handle = withHandleExecution handle do
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
        when (not snapshot.cancellationRequested && (recover || wake)) $
            void (sendInput handle.registry snapshot.agentId
                -- Only an unstarted task needs its original assignment.
                -- A running task already has a transcript, potentially with
                -- newer instructions that the original prompt would override.
                (if recover && snapshot.status == Pending
                    then snapshot.initialMessage else config.wakeMessage) False)

-- | Cancellation remains requested until the actual worker has been joined.
-- Never hold a database row lock while joining: the worker may need that row
-- to finish its persistence callback.
processCancellations :: RecoveryConfig -> RuntimeHandle -> IO ()
processCancellations config handle = do
    snapshots <- config.store.loadSnapshots
    forM_ snapshots \snapshot -> when snapshot.cancellationRequested do
        current <- getStatus handle.registry snapshot.agentId
        unless (current == NotFound) $
            closeSubagent handle.registry snapshot.agentId >>= either (fail . Text.unpack) (const (pure ()))
        withHandleOperation handle (commitStatus config snapshot.agentId Closed)

withStoreTransaction :: RecoveryStore -> (RecoveryStore -> IO a) -> IO a
withStoreTransaction RecoveryStore { transaction = action } = action

-- Guards can nest (initialization pumps stored work, runners persist sleep)
-- and execute concurrently. Hosts must reserve rather than hold an exclusive
-- lock across the action. Tool failures converted into values must be guarded
-- at the tool boundary too; a returned LoopError is not an IO exception.
withHandleOperation :: RuntimeHandle -> IO a -> IO a
withHandleOperation RuntimeHandle { withOperation = operation } = operation

-- Lifetime reservations are released after local execution exits, including
-- cancellation. They do not imply that an interrupted external effect is safe
-- to repeat; those effects must use the narrower operation guard above.
withHandleExecution :: RuntimeHandle -> IO a -> IO a
withHandleExecution RuntimeHandle { withExecution = execution } = execution
