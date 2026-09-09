-- | Persistent session scratch and its leases. Acquisition and release ordering
-- live together; the outer resource scope owns the returned cleanup action.
module Agent.CLI.Runtime.Orchestration.Tools.Scratch
    ( ScratchRuntime(..)
    , prepareScratchRuntime
    , startStaleResourceCleanup
    ) where

import Agent.CLI.Error (formatException)
import Agent.CLI.Config (HarnessConfig(..), WorktreeConfig(..))
import Agent.CLI.ExternalSession (defaultExternalSessionEnv, externalSessionTool)
import Agent.CLI.ManagedTurn (ManagedTurnRequest(..))
import Agent.CLI.Models (ModelTarget(..))
import Agent.CLI.Options (CliOptions(..))
import Agent.CLI.Runtime.HistorySource (emptyFullscreenHistoryPage, loadFullscreenHistoryPage)
import Agent.CLI.Runtime.Orchestration.Startup (reportStartupWarning)
import Agent.CLI.Runtime.Orchestration.Tools.Collaboration
import Agent.CLI.Runtime.Orchestration.Tools.Model
import Agent.CLI.Runtime.Orchestration.Tools.Request
import Agent.CLI.Runtime.Orchestration.Types (AgentProcessRuntime(..), NativeRunCapabilities(..))
import Agent.CLI.Runtime.Persistence (preparePersistence)
import Agent.CLI.Session
    ( Persistence, SessionTempCleanupReport(..)
    , acquireSessionTempLease, allocateSessionTemp, cleanupPendingPersistence
    , cleanupStaleSessionTemps, defaultSessionTempKeepCount
    , loadCurrentTaskPlan, persistenceTempDir, releaseSessionTempLease
    , removeSessionTemp, taskPlanHooksForPersistence )
import Agent.CLI.Session.History (foldSessionItems)
import Agent.CLI.Session.Runtime.Types (StartupRuntime(..))
import Agent.CLI.Session.Selection (loadPrompt, reservedSessionId)
import Agent.CLI.Startup.Auth (startupDie)
import Agent.CLI.TUI.App (clearFullscreenHistorySource, setFullscreenHistorySource)
import Agent.CLI.TUI.History (HistoryGeneration(..))
import Agent.CLI.Worktree
    ( WorktreeCleanupReport(..), acquireWorktreeLease, gcWorktreesWithActivity
    , releaseWorktreeLease, worktreeRoot )
import Agent.CLI.Worktree.Provenance (loadWorktreeActivity)
import Agent.OpenAI.ImageGeneration
    ( ImageGenerationHistory, newImageGenerationHistory, recordImageGenerationResponseItems )
import Agent.OsPath (unsafeToFilePath)
import Agent.ResourceScope (allocateResource, closeResourceScope, newResourceScope)
import Agent.Store.Postgres (trustedPool)
import Agent.Tools.TaskPlan (TaskPlanEnv, newTaskPlanEnv)
import Agent.Tools.Types
    ( AppTool, ToolEnv(toolOutputMemoryStore), setToolSessionTmp, clearMemoryOutputArtifacts )
import Control.Concurrent.Async (concurrently)
import Control.Exception.Safe (SomeException, bracketOnError, try)
import Control.Monad (forM_)
import Data.IORef (writeIORef)
import Data.Maybe (isNothing)
import qualified Data.Text as Text
import System.OsPath (OsPath)

data ScratchRuntime = ScratchRuntime
    { scratchPromptRequest :: Maybe ManagedTurnRequest
    , scratchPersistence :: Persistence
    , scratchTaskPlan :: TaskPlanEnv
    , scratchSessionTmp :: OsPath
    , scratchImageGenerationHistory :: ImageGenerationHistory
    , scratchExternalSessionTools :: [AppTool]
    , scratchCleanup :: IO ()
    }

prepareScratchRuntime
    :: AgentToolsRequest windowTitleResult
    -> ToolStartup
    -> ToolModelRuntime
    -> CollaborationRuntime
    -> IO ScratchRuntime
prepareScratchRuntime AgentToolsRequest
    { options
    , startup
    , root
    , gatewayIdentity
    , transition
    , cwd
    , fullscreen
    , baseToolEnv
    , resumed
    , home
    } ToolStartup
    { toolNativeCapabilities = nativeCapabilities
    } ToolModelRuntime
    { toolInferredTarget = inferredTarget
    , toolDialectId = dialectId
    , toolEffortText = effortText
    } CollaborationRuntime
    { collaborationPersistSlotRef = persistSlotRef
    } = bracketOnError newResourceScope closeResourceScope \scratchScope -> do
    scratchPromptRequest <- loadPrompt options
    let promptText =
            fmap (\request -> request.managedTurnText) scratchPromptRequest
    (_, scratchPersistence) <-
        allocateResource scratchScope
            (preparePersistence
                (trustedPool startup.startupDatabaseStore)
                startup
                options
                root
                inferredTarget { targetDialect = dialectId }
                gatewayIdentity
                (isNothing transition)
                cwd
                effortText
                promptText
                resumed)
            cleanupPendingPersistence
    writeIORef persistSlotRef scratchPersistence
    initialTaskPlan <-
        loadCurrentTaskPlan scratchPersistence >>= \case
            Left err ->
                startupDie startup
                    ("Failed to load current task plan: " <> err)
            Right plan -> pure plan
    scratchTaskPlan <-
        newTaskPlanEnv
            initialTaskPlan
            (taskPlanHooksForPersistence scratchPersistence)
    forM_ fullscreen \runtime ->
        reservedSessionId scratchPersistence >>= \case
            Nothing ->
                clearFullscreenHistorySource runtime
            Just sessionId ->
                setFullscreenHistorySource
                    runtime
                    sessionId
                    (loadFullscreenHistoryPage
                        (trustedPool startup.startupDatabaseStore)
                        root
                        sessionId)
                    (emptyFullscreenHistoryPage
                        (HistoryGeneration 0))
    scratchSessionTmp <-
        persistenceTempDir scratchPersistence >>= \case
            Just tempDir -> pure tempDir
            Nothing -> do
                (_, (_, tempDir)) <-
                    allocateResource scratchScope
                        (allocateSessionTemp root)
                        (\(sessionId, _) -> do
                            _ <- removeSessionTemp root sessionId
                            pure ())
                pure tempDir
    setToolSessionTmp baseToolEnv (Just scratchSessionTmp)
    _ <- allocateResource scratchScope
        (pure ())
        (\() -> clearMemoryOutputArtifacts baseToolEnv.toolOutputMemoryStore)
    scratchImageGenerationHistory <- newImageGenerationHistory
    forM_ resumed \(_, turns) ->
        recordImageGenerationResponseItems
            scratchImageGenerationHistory
            (foldSessionItems turns)
    scratchExternalSessionTools <-
        if options.optSkills
            && nativeCapabilities.nativeHostExtensions
            then do
                env <-
                    defaultExternalSessionEnv
                        baseToolEnv
                        (unsafeToFilePath cwd)
                        (unsafeToFilePath scratchSessionTmp)
                        (unsafeToFilePath home)
                pure [externalSessionTool env]
            else pure []
    _ <- allocateResource scratchScope
        (acquireWorktreeLease (worktreeRoot home) cwd >>= \case
            Left err -> startupDie startup err
            Right lease -> pure lease)
        (mapM_ releaseWorktreeLease)
    _ <- allocateResource scratchScope
        (acquireSessionTempLease root scratchSessionTmp >>= \case
            Left err -> startupDie startup err
            Right lease -> pure lease)
        (mapM_ releaseSessionTempLease)
    let scratchCleanup = closeResourceScope scratchScope
    pure ScratchRuntime{..}

startStaleResourceCleanup
    :: AgentToolsRequest windowTitleResult
    -> OsPath
    -> IO ()
startStaleResourceCleanup AgentToolsRequest
    { processRuntime
    , startup
    , root
    , cwd
    , home
    } sessionTmp = do
    -- Housekeeping may inspect hundreds of worktrees and invoke Git for each
    -- candidate. It must never delay interactive startup.
    _ <- processRuntime.processStartCleanup do
        cleanupResult <- try @_ @SomeException do
            (worktreeReport, tempReport) <- concurrently
                (gcWorktreesWithActivity
                    (loadWorktreeActivity (trustedPool startup.startupDatabaseStore) (worktreeRoot home))
                    (worktreeRoot home)
                    startup.startupHarnessConfig.configWorktree.worktreeInactiveDays
                    False
                    [cwd])
                (cleanupStaleSessionTemps
                    root
                    defaultSessionTempKeepCount
                    [sessionTmp])
            pure (worktreeReport, tempReport)
        case cleanupResult of
            Left exception ->
                reportStartupWarning startup
                    ("stale resource cleanup failed: "
                        <> formatException exception)
            Right (worktreeReport, tempReport) -> do
                forM_ worktreeReport.cleanupFailures \(path, err) ->
                    reportStartupWarning startup
                        ("could not clean stale worktree "
                            <> Text.pack (unsafeToFilePath path)
                            <> ": "
                            <> err)
                forM_ tempReport.tempCleanupFailures \(path, err) ->
                    reportStartupWarning startup
                        ("could not clean stale session scratch directory "
                            <> Text.pack (unsafeToFilePath path)
                            <> ": "
                            <> err)
    pure ()
