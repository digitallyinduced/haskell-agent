-- | Persistence of resident child-agent transcripts.
module Agent.CLI.Subagents.Runtime.Storage
    ( flushAllSubagentSnapshots
    , persistAndEvictSubagentSessionWithStatus
    , persistSubagentSnapshotWithStatus
    , syncStoreRootFromPlan
    ) where

import Agent.CLI.Btw (trimDanglingToolSuffix)
import Agent.CLI.SubagentStore
    ( SubagentStateSnapshot(..)
    , SubagentTarget(..)
    , saveSubagentState
    )
import Agent.CLI.Subagents.Runtime.Types
    ( SubagentSession(..)
    , SubagentStoreRoot
    )
import Agent.GrokBuild.Dialect.Task
    ( GrokSubagentSpecs
    , lookupAgentModel
    , lookupAgentReasoningEffort
    , lookupAgentType
    )
import Agent.Subagents
    ( SubagentAccessProfile(..)
    , SubagentId
    , SubagentRegistry
    , SubagentStatus(..)
    , getPreviousResponseId
    , getStatus
    , getSubagentCwd
    , getSubagentAccessProfile
    , getSubagentIdentity
    )
import Agent.Tools.PlanMode (PlanModeEnv, readPlanSessionDir)
import Control.Concurrent.MVar (modifyMVar, withMVar)
import Control.Monad (void, when)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import System.OsPath (OsPath)

-- | Prefer an explicit store root; otherwise fall back to planMode's session dir.
syncStoreRootFromPlan :: SubagentStoreRoot -> PlanModeEnv -> IO ()
syncStoreRootFromPlan storeRootRef planMode = do
    mroot <- readIORef storeRootRef
    case mroot of
        Just _ -> pure ()
        Nothing -> do
            sessionDir <- readPlanSessionDir planMode
            case sessionDir of
                Just dir -> writeIORef storeRootRef (Just dir)
                Nothing -> pure ()

persistSubagentSnapshotWithStatus
    :: SubagentStoreRoot
    -> SubagentRegistry
    -> GrokSubagentSpecs
    -> SubagentId
    -> SubagentStatus
    -> SubagentSession
    -> IO ()
persistSubagentSnapshotWithStatus
        storeRootRef registry typesRef agentId status session = do
    mroot <- readIORef storeRootRef
    case mroot of
        Nothing -> pure ()
        Just sessionDir ->
            void $
                saveSubagentSnapshotWithStatus
                    sessionDir registry typesRef agentId status session

-- | Persist a final snapshot, then release its parsed transcript payload.
persistAndEvictSubagentSessionWithStatus
    :: SubagentStoreRoot
    -> SubagentRegistry
    -> GrokSubagentSpecs
    -> SubagentId
    -> SubagentStatus
    -> SubagentSession
    -> IO (Either Text Bool)
persistAndEvictSubagentSessionWithStatus
        storeRootRef registry typesRef agentId status session =
    modifyMVar session.subSessionHydrated \hydrated ->
        if not hydrated || not (evictableStatus status)
            then pure (hydrated, Right False)
            else readIORef storeRootRef >>= \case
                Nothing -> pure (True, Right False)
                Just sessionDir ->
                    saveSubagentSnapshotWithStatus
                        sessionDir registry typesRef agentId status session >>= \case
                            Left err -> pure (True, Left err)
                            Right () -> do
                                pinned <- readIORef session.subSessionPinned
                                if pinned
                                    then pure (True, Right False)
                                    else do
                                        writeIORef session.subSessionTranscript []
                                        writeIORef session.subSessionContextTokens Nothing
                                        pure (False, Right True)
  where
    evictableStatus = \case
        Completed{} -> True
        Errored{} -> True
        Interrupted -> True
        Closed -> True
        Pending -> False
        Running -> False
        NotFound -> False

saveSubagentSnapshotWithStatus
    :: OsPath
    -> SubagentRegistry
    -> GrokSubagentSpecs
    -> SubagentId
    -> SubagentStatus
    -> SubagentSession
    -> IO (Either Text ())
saveSubagentSnapshotWithStatus
        sessionDir registry typesRef agentId status session = do
    items <- trimDanglingToolSuffix <$> readIORef session.subSessionTranscript
    previous <- getPreviousResponseId registry agentId
    agentType <- lookupAgentType typesRef agentId
    agentModel <- lookupAgentModel typesRef agentId
    reasoningEffort <- lookupAgentReasoningEffort typesRef agentId
    agentCwd <- getSubagentCwd registry agentId
    identity <- getSubagentIdentity registry agentId
    accessProfile <-
        fromMaybe SubagentFullAccess
            <$> getSubagentAccessProfile registry agentId
    saveSubagentState sessionDir agentId SubagentStateSnapshot
        { snapshotItems = items
        , snapshotPreviousResponseId = previous
        , snapshotStatus = status
        , snapshotTarget = SubagentTarget
            { targetProvider = session.subSessionProvider
            , targetConnection = session.subSessionConnection
            , targetEffectiveModel = session.subSessionEffectiveModel
            , targetDialect = session.subSessionDialect
            }
        , snapshotAgentType = agentType
        , snapshotAgentModel = agentModel
        , snapshotReasoningEffort = reasoningEffort
        , snapshotCwd = agentCwd
        , snapshotIdentity = identity
        , snapshotAccessProfile = accessProfile
        }

flushAllSubagentSnapshots
    :: SubagentStoreRoot
    -> SubagentRegistry
    -> IORef (Map SubagentId SubagentSession)
    -> GrokSubagentSpecs
    -> IO ()
flushAllSubagentSnapshots storeRootRef registry sessionsRef typesRef = do
    sessions <- readIORef sessionsRef
    mapM_
        (\(agentId, session) ->
            withMVar session.subSessionHydrated \hydrated ->
                when hydrated do
                    status <- getStatus registry agentId
                    persistSubagentSnapshotWithStatus
                        storeRootRef registry typesRef agentId status session)
        (Map.toList sessions)
