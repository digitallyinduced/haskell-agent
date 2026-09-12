-- | Session-owned persistence, task plans, scratch storage, and leases.
-- Frontends adapt presentation and repository-specific worktree ownership at
-- the hooks below; acquisition and reverse-order cleanup stay together here.
module Agent.Runtime.Session.Resources
    ( SessionResourcesRequest(..)
    , SessionResourceHooks(..)
    , SessionResources(..)
    , SessionResourceError(..)
    , prepareSessionResources
    ) where

import Agent.OpenAI.ImageGeneration
    ( ImageGenerationHistory, newImageGenerationHistory, recordImageGenerationResponseItems )
import Agent.ResourceScope (allocateResource, closeResourceScope, newResourceScope)
import Agent.Runtime.Session
    ( Persistence, SessionTurn
    , acquireSessionTempLease, allocateSessionTemp, cleanupPendingPersistence
    , loadCurrentTaskPlan, persistenceTempDir, releaseSessionTempLease
    , removeSessionTemp, taskPlanHooksForPersistence )
import Agent.Runtime.Session.History (foldSessionItems)
import Agent.Runtime.Session.Preparation (PersistenceRequest(..), prepareSessionPersistence)
import Agent.Tools.TaskPlan (TaskPlanEnv, newTaskPlanEnv)
import Agent.Tools.Types
    ( ToolEnv(toolOutputMemoryStore), setToolSessionTmp, clearMemoryOutputArtifacts )
import Control.Exception.Safe (Exception, bracketOnError, throwIO)
import Data.Text (Text)
import System.OsPath (OsPath)

data SessionResourcesRequest = SessionResourcesRequest
    { resourcePersistenceRequest :: PersistenceRequest
    , resourceToolEnv :: ToolEnv
    , resourceResumedTurns :: [SessionTurn]
    }

-- | Hooks run synchronously within the acquisition scope. Host preparation
-- must not retain unowned resources; the worktree acquisition returns its
-- finalizer, which is registered atomically with acquisition.
data SessionResourceHooks host = SessionResourceHooks
    { resourcePersistenceReady :: Persistence -> IO ()
    , resourceTaskPlanReady :: Persistence -> IO ()
    , resourcePrepareHost :: OsPath -> IO host
    , resourceAcquireWorktreeLease :: IO (IO ())
    }

data SessionResources host = SessionResources
    { resourcePersistence :: Persistence
    , resourceTaskPlan :: TaskPlanEnv
    , resourceSessionTmp :: OsPath
    , resourceImageGenerationHistory :: ImageGenerationHistory
    , resourceHost :: host
    , resourceCleanup :: IO ()
    }

newtype SessionResourceError = SessionResourceError Text
    deriving (Show)

instance Exception SessionResourceError

-- | The caller must transfer the returned cleanup to its enclosing resource
-- owner. Partial startup failures and cancellation release completed resources.
prepareSessionResources
    :: SessionResourcesRequest
    -> SessionResourceHooks host
    -> IO (SessionResources host)
prepareSessionResources SessionResourcesRequest{..} hooks =
    bracketOnError newResourceScope closeResourceScope \scope -> do
        (_, resourcePersistence) <- allocateResource scope
            (prepareSessionPersistence resourcePersistenceRequest)
            cleanupPendingPersistence
        hooks.resourcePersistenceReady resourcePersistence
        initialTaskPlan <- loadCurrentTaskPlan resourcePersistence >>= \case
            Left err -> throwIO (SessionResourceError ("Failed to load current task plan: " <> err))
            Right plan -> pure plan
        resourceTaskPlan <- newTaskPlanEnv initialTaskPlan
            (taskPlanHooksForPersistence resourcePersistence)
        hooks.resourceTaskPlanReady resourcePersistence
        resourceSessionTmp <- persistenceTempDir resourcePersistence >>= \case
            Just tempDir -> pure tempDir
            Nothing -> do
                (_, (_, tempDir)) <- allocateResource scope
                    (allocateSessionTemp root)
                    (\(sessionId, _) -> do
                        _ <- removeSessionTemp root sessionId
                        pure ())
                pure tempDir
        setToolSessionTmp resourceToolEnv (Just resourceSessionTmp)
        _ <- allocateResource scope
            (pure ())
            (\() -> clearMemoryOutputArtifacts resourceToolEnv.toolOutputMemoryStore)
        resourceImageGenerationHistory <- newImageGenerationHistory
        recordImageGenerationResponseItems resourceImageGenerationHistory
            (foldSessionItems resourceResumedTurns)
        resourceHost <- hooks.resourcePrepareHost resourceSessionTmp
        _ <- allocateResource scope hooks.resourceAcquireWorktreeLease id
        _ <- allocateResource scope
            (acquireSessionTempLease root resourceSessionTmp >>= \case
                Left err -> throwIO (SessionResourceError err)
                Right lease -> pure lease)
            (mapM_ releaseSessionTempLease)
        let resourceCleanup = closeResourceScope scope
        pure SessionResources{..}
  where
    root = resourcePersistenceRequest.persistenceRoot
