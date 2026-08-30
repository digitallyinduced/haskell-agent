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
import Agent.Loop (BackendSnapshot(..), emptyBackendSnapshot)
import Agent.CLI.Subagents.Runtime.Types
    ( SubagentResidency(..)
    , SubagentSession(..)
    , SubagentStoreRoot
    )
import Agent.GrokBuild.Dialect.Task
    ( GrokSubagentSpecs
    , lookupAgentModel
    , lookupAgentReasoningEffort
    , lookupAgentType
    )
import Agent.Subagents
    ( SubagentId
    , SubagentRegistry
    , SubagentStatus(..)
    , getPreviousResponseId
    , getStatus
    , getSubagentCwd
    , getSubagentIdentity
    )
import Agent.Tools.PlanMode (PlanModeEnv(..))
import Control.Concurrent.MVar (modifyMVar, withMVar)
import Control.Monad (void, when)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import System.OsPath (OsPath)

-- | Prefer an explicit store root; otherwise fall back to planMode's session dir.
syncStoreRootFromPlan :: SubagentStoreRoot -> PlanModeEnv -> IO ()
syncStoreRootFromPlan storeRootRef planMode = do
    mroot <- readIORef storeRootRef
    case mroot of
        Just _ -> pure ()
        Nothing -> do
            sessionDir <- readIORef planMode.planSessionDir
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
    modifyMVar session.subSessionResidency \residency ->
        if residency == SessionEvicted || not (evictableStatus status)
            then pure (residency, Right False)
            else readIORef storeRootRef >>= \case
                Nothing -> pure (residency, Right False)
                Just sessionDir ->
                    saveSubagentSnapshotWithStatus
                        sessionDir registry typesRef agentId status session >>= \case
                            Left err -> pure (residency, Left err)
                            Right () ->
                                case residency of
                                    SessionPinned ->
                                        pure (SessionPinned, Right False)
                                    SessionResident -> do
                                        writeIORef session.subSessionTranscript
                                            emptyBackendSnapshot
                                        writeIORef session.subSessionContextTokens Nothing
                                        pure (SessionEvicted, Right True)
                                    SessionEvicted ->
                                        pure (SessionEvicted, Right False)
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
    items <-
        trimDanglingToolSuffix . (.backendItems)
            <$> readIORef session.subSessionTranscript
    previous <- getPreviousResponseId registry agentId
    agentType <- lookupAgentType typesRef agentId
    agentModel <- lookupAgentModel typesRef agentId
    reasoningEffort <- lookupAgentReasoningEffort typesRef agentId
    agentCwd <- getSubagentCwd registry agentId
    identity <- getSubagentIdentity registry agentId
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
            withMVar session.subSessionResidency \residency ->
                when (residency /= SessionEvicted) do
                    status <- getStatus registry agentId
                    persistSubagentSnapshotWithStatus
                        storeRootRef registry typesRef agentId status session)
        (Map.toList sessions)
