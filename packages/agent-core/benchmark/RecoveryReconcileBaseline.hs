{-# LANGUAGE RankNTypes #-}
-- | The reconciliation pass before settled children were skipped, kept as
-- the baseline of 'RecoveryReconcile'. Every child cost one store
-- transaction per pass, and every settled child repeated its settlement.
module RecoveryReconcileBaseline (baselineReconcileStoredSubagents) where

import Agent.Subagents
import Agent.Subagents.Runtime (RuntimeHandle (..))
import Agent.Subagents.Runtime.Recovery
import Control.Monad (forM_, when)

baselineReconcileStoredSubagents :: RecoveryConfig -> RuntimeHandle -> IO ()
baselineReconcileStoredSubagents config handle = withHandleExecution handle do
    inventory <- config.store.loadSnapshots
    forM_ inventory \snapshot -> do
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
  where
    settledStatus = \case
        Completed {} -> True
        Errored {} -> True
        Closed -> True
        Suspended {} -> True
        _ -> False

restoredStatus :: SubagentStatus -> SubagentStatus
restoredStatus Pending = Interrupted
restoredStatus Running = Interrupted
restoredStatus status = status

withStoreTransaction :: RecoveryStore -> (RecoveryStore -> IO a) -> IO a
withStoreTransaction RecoveryStore { transaction = action } = action

withHandleOperation :: RuntimeHandle -> IO a -> IO a
withHandleOperation RuntimeHandle { withOperation = operation } = operation

withHandleExecution :: RuntimeHandle -> IO a -> IO a
withHandleExecution RuntimeHandle { withExecution = execution } = execution
