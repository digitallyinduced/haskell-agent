-- | Persistent session scratch and its leases. Acquisition and release ordering
-- live together; the outer resource scope owns the returned cleanup action.
module Agent.CLI.Runtime.Orchestration.Tools.Scratch
    ( ScratchRuntime(..)
    , prepareScratchRuntime
    , startStaleResourceCleanup
    ) where

import Agent.Runtime.Error (formatException)
import Agent.Runtime.Config (HarnessConfig(..), WorktreeConfig(..))
import Agent.CLI.ExternalSession (defaultExternalSessionEnv, externalSessionTool)
import Agent.Runtime.ManagedTurn (ManagedTurnRequest(..))
import Agent.Runtime.Models (ModelTarget(..))
import Agent.CLI.Options (CliOptions(..))
import Agent.CLI.Runtime.HistorySource (emptyFullscreenHistoryPage, loadFullscreenHistoryPage)
import Agent.CLI.Runtime.Orchestration.Startup (reportStartupWarning)
import Agent.CLI.Runtime.Orchestration.Tools.Collaboration
import Agent.CLI.Runtime.Orchestration.Tools.Model
import Agent.CLI.Runtime.Orchestration.Tools.Request
import Agent.CLI.Runtime.Orchestration.Types (AgentProcessRuntime(..), NativeRunCapabilities(..))
import Agent.CLI.Runtime.Persistence (persistenceRequest, announceResumedSession)
import Agent.Runtime.Session
    ( Persistence, SessionTempCleanupReport(..)
    , cleanupStaleSessionTemps, defaultSessionTempKeepCount
    )
import Agent.Runtime.Session.Resources
    ( SessionResourcesRequest(..), SessionResourceHooks(..)
    , SessionResources(..), SessionResourceError(..), prepareSessionResources )
import Agent.CLI.Session.Runtime.Types (StartupRuntime(..))
import Agent.CLI.Session.Selection (loadPrompt, reservedSessionId)
import Agent.CLI.Startup.Auth (startupDie)
import Agent.CLI.TUI.App (clearFullscreenHistorySource, setFullscreenHistorySource)
import Agent.CLI.TUI.History (HistoryGeneration(..))
import Agent.CLI.Worktree
    ( WorktreeCleanupReport(..), acquireWorktreeLease, gcWorktreesWithActivity
    , releaseWorktreeLease, worktreeRoot )
import Agent.CLI.Worktree.Provenance (loadWorktreeActivity)
import Agent.OpenAI.ImageGeneration (ImageGenerationHistory)
import Agent.OsPath (unsafeToFilePath)
import Agent.Store.Postgres (trustedPool)
import Agent.Tools.TaskPlan (TaskPlanEnv)
import Agent.Tools.Types (AppTool)
import Control.Concurrent.Async (concurrently)
import Control.Exception.Safe (SomeException, catch, try)
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
    } = do
    scratchPromptRequest <- loadPrompt options
    let promptText =
            fmap (\request -> request.managedTurnText) scratchPromptRequest
        request = SessionResourcesRequest
            { resourcePersistenceRequest = persistenceRequest
                (trustedPool startup.startupDatabaseStore)
                options root inferredTarget { targetDialect = dialectId }
                gatewayIdentity (isNothing transition) cwd effortText promptText resumed
            , resourceToolEnv = baseToolEnv
            , resourceResumedTurns = maybe [] snd resumed
            }
        hooks = SessionResourceHooks
            { resourcePersistenceReady = \persistence -> do
                forM_ resumed \(meta, _) -> announceResumedSession startup meta
                writeIORef persistSlotRef persistence
            , resourceTaskPlanReady = \persistence ->
                forM_ fullscreen \runtime ->
                    reservedSessionId persistence >>= \case
                        Nothing -> clearFullscreenHistorySource runtime
                        Just sessionId ->
                            setFullscreenHistorySource runtime sessionId
                                (loadFullscreenHistoryPage
                                    (trustedPool startup.startupDatabaseStore) root sessionId)
                                (emptyFullscreenHistoryPage (HistoryGeneration 0))
            , resourcePrepareHost = \sessionTmp ->
                if options.optSkills && nativeCapabilities.nativeHostExtensions
                    then do
                        env <- defaultExternalSessionEnv baseToolEnv
                            (unsafeToFilePath cwd)
                            (unsafeToFilePath sessionTmp)
                            (unsafeToFilePath home)
                        pure [externalSessionTool env]
                    else pure []
            , resourceAcquireWorktreeLease =
                acquireWorktreeLease (worktreeRoot home) cwd >>= \case
                    Left err -> startupDie startup err
                    Right lease -> pure (mapM_ releaseWorktreeLease lease)
            }
    resources <- prepareSessionResources request hooks
        `catch` \(SessionResourceError err) -> startupDie startup err
    let scratchPersistence = resources.resourcePersistence
        scratchTaskPlan = resources.resourceTaskPlan
        scratchSessionTmp = resources.resourceSessionTmp
        scratchImageGenerationHistory = resources.resourceImageGenerationHistory
        scratchExternalSessionTools = resources.resourceHost
        scratchCleanup = resources.resourceCleanup
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
