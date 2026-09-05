module Agent.CLI.Runtime.Orchestration.Tools (runAgentTools) where

import Agent.CLI.AccountPicker ()
import Agent.CLI.AccountSelection ()
import Agent.CLI.Afk ()
import Agent.CLI.AgentSessions
    ( agentSessionTools,
      launchSessionThread,
      sessionThreadStatus,
      AgentSessionToolsEnv(toolsSessionStatus, AgentSessionToolsEnv,
                           toolsPool, toolsRoot, toolsProvider, toolsConnection, toolsModel,
                           toolsTransportModel, toolsDialect, toolsAllowedModels,
                           toolsResolveModelOption,
                           toolsGatewayIdentity, toolsCwd, toolsEffort,
                           toolsCurrentSessionId, toolsLaunchTurn) )
import Agent.CLI.AgentViewport ()
import Agent.CLI.Approval ()
import Agent.CLI.Artifact ()
import Agent.CLI.Auth
    ( LoadedAuth(loadedProvider, loadedTokenProvider),
      hasOpenAiAuth,
      isGatewayLoadedAuth,
      loadAuth )
import Agent.CLI.Clipboard ()
import Agent.CLI.CodeModeRuntime ()
import Agent.CLI.Command ()
import qualified Agent.CLI.ComputerUse as ComputerUse
import Agent.CLI.Compaction ()
import Agent.CLI.Config
    ( HarnessConfig(..),
      McpServerConfig(..),
      useProgressiveMcp )
import Agent.Connectivity ()
import Agent.CLI.Database ( databaseTools )
import Agent.CLI.Database.Store
    (DatabaseScopes, databaseToolsEnvForStore)
import Agent.CLI.Dialects
    ( CodingTools(..),
      codingToolsForWithTypes,
      filterBashTools,
      filterGhciTools )
import Agent.CLI.Error ( formatException )
import Agent.CLI.ExternalSession
    ( defaultExternalSessionEnv
    , externalSessionTool
    )
import Agent.CLI.GatewayClient
    ( GatewayCredential
    , GatewayModelAccess
    , cachedGatewayModels
    , gatewayCredentialIdentity
    )
import Agent.CLI.GatewayModels (modelOptionsForGatewayModels)
import Agent.CLI.GatewayBridge ( managedGatewayTools )
import Agent.CLI.Input ()
import Agent.CLI.Interrupt (InterruptState)
import Agent.CLI.LearnedSkills ( learnedSkillTools )
import Agent.CLI.LearnedSkills.Store
    ( learnedSkillToolsEnvForStore
    , loadApplicableLearnedSkillsForStore
    , successfulLearnedSkillsPreload
    )
import Agent.CLI.Login ()
import Agent.CLI.Lsp
    ( LspStartup(..), closeLspRuntime, lspRuntimeTool, newLspRuntime )
import Agent.CLI.ManagedTurn ( ManagedTurnRequest(..) )
import Agent.CLI.McpManager ()
import Agent.CLI.McpOAuthStore (mcpOAuthStorePath)
import Agent.CLI.McpElicitation (cliMcpElicitation)
import Agent.CLI.McpStatus
    ( formatMcpInstructionsNotice,
      formatMcpModelNoticeFor,
      formatMcpProgress,
      summarizeMcpStatuses )
import Agent.CLI.ModelConfig
    ( ModelCatalog
    , ResponsesConnection(..)
    , builtinConnectionId
    )
import Agent.CLI.Models
    ( defaultModelFor,
      rawModelOption,
      resolveConfiguredModel,
      resolveModelOptionById,
      resolvePersistedDialect,
      ModelOption(modelTarget),
      ModelTarget(targetProvider, targetConnectionId, targetModelId, targetDialect,
                  targetWireModelId) )
import Agent.CLI.Options
    ( ApprovalPolicy(ApproveAll, PromptMutating),
      defaultEffortFor,
      isOneShot,
      normalizeReasoningEffortForDialect,
      resolveApprovalPolicy,
      resolveComputerUseEnabled,
      CliOptions(optYolo, optModel, optEffort, optMaxConcurrentAgents,
                 optGhci, optBash, optNoYolo, optSkills) )
import Agent.CLI.PendingInputs
    ( PendingInputs
    , PendingNoticeKind(..)
    , enqueuePendingInput
    , enqueuePendingNotice
    , newPendingInputs
    )
import Agent.CLI.Project (ProjectModel(..), ProjectSettings(..))
import Agent.CLI.Plan
    ( cliPlanHooks
    , resumedPlanNeedsApproval
    )
import Agent.CLI.Prompt
    ( mcpInstructionsForRequest
    , subscriptionSubagentModelGuidance
    )
import Agent.CLI.PromptHooks
    ( fullscreenAwareImageHooks, fullscreenAwarePlanHooks, fullscreenAwareSecretHooks )
import Agent.CLI.Provider.OpenAI ()
import Agent.CLI.Provider.Switch ()
import Agent.CLI.ProviderAvailability ()
import Agent.CLI.ProviderFallback ()
import Agent.CLI.ProviderTransition (PendingTurn, ProviderTransition)
import Agent.CLI.Recap ()
import Agent.CLI.Render ()
import Agent.CLI.ReplMode ()
import Agent.CLI.Request ()
import Agent.CLI.Resume ()
import Agent.CLI.Runtime.HistorySource
    ( emptyFullscreenHistoryPage, loadFullscreenHistoryPage )
import Agent.CLI.Runtime.Orchestration.Background
    ( runInProcessSessionTurn )
import Agent.CLI.Runtime.Orchestration.Types
    ( AgentProcessRuntime(..)
    , AgentRunMode
    , NativeDiscoveryContext(..)
    , NativeInteractionMode(..)
    , NativeRunCapabilities(..)
    , NativeRunHooks(..)
    , fullNativeRunCapabilities
    , nativeLoadsHostWorkspaceContext
    , nativePreparedDiscovery
    )
import Agent.CLI.Runtime.Orchestration.Restart ()
import Agent.CLI.Resume
    ( SessionInitialContext
        ( initialContextMayRestoreSnapshot
        , initialContextNeeded
        )
    , resolveSessionInitialContext
    )
import Agent.CLI.Runtime.Orchestration.Session (runAgentSession)
import Agent.CLI.Runtime.Orchestration.Startup
    ( reportStartupWarning )
import Agent.CLI.Runtime.Persistence ( preparePersistence )
import Agent.CLI.Runtime.Recap ()
import Agent.CLI.Runtime.Repl ()
import Agent.CLI.Runtime.Types (DevResult, RunResult)
import Agent.CLI.Secret ( promptSecretLine )
import Agent.CLI.Session
    ( allocateSessionTemp,
      acquireSessionTempLease,
      cleanupStaleSessionTemps,
      cleanupPendingPersistence,
      defaultSessionTempKeepCount,
      listSessions,
      persistenceTempDir,
      releaseSessionTempLease,
      removeSessionTemp,
      loadCurrentTaskPlan,
      sessionLegacySubagentTarget,
      taskPlanHooksForPersistence,
      SessionTempCleanupReport(..),
      LegacySubagentTarget,
      Persistence(PersistenceDisabled),
      SessionHandle(sessionDir, sessionMeta),
      SessionMeta(metaId, metaTransportModel, metaProvider,
                  metaConnection, metaModel, metaDialect, metaEffort, metaCwd),
      SessionTurn(turnAssistantText) )
import Agent.CLI.Session.Attachments ( putImagePreview )
import Agent.CLI.Session.Choices ()
import Agent.CLI.Session.History (foldSessionItems)
import Agent.CLI.Session.Lifecycle ()
import Agent.CLI.Session.Runtime.Types
    ( InitialContextPreload(..)
    , StartupRuntime(startupFullscreen, startupBackground,
                     startupFinished, startupDatabaseStore,
                     startupHarnessConfig,
                     startupSessionState, startupNativeHooks,
                     startupStdinTty) )
import Agent.CLI.Session.Selection
    ( currentSessionId, loadPrompt, reservedSessionId )
import Agent.CLI.SessionAdmin ()
import Agent.CLI.SessionEnv ()
import Agent.CLI.SessionLock
    ( SessionLock,
      acquireSessionLock,
      releaseSessionLock,
      sessionLockFilePath,
      sessionLockPath )
import Agent.CLI.SessionState ( SessionState(sessionPreviewId) )
import Agent.CLI.SessionTitle ()
import Agent.CLI.Startup.Auth
    ( markStartupStage, setStartupNotice, startupDie )
import Agent.CLI.StartupContext ( preloadAgentsContext )
import Agent.CLI.Style ()
import Agent.CLI.Subagents.Runtime
    ( flushAllSubagentSnapshots,
      persistAndEvictSubagentSessionWithStatus,
      prepareCollaborationSpawn,
      restoreAgentFromDisk )
import Agent.CLI.Subagents.Runtime.Types
    ( SubagentSession
    , SubagentStoreRoot
    )
import Agent.CLI.TUI.App
    ( FullscreenRuntime,
      clearFullscreenHistorySource,
      emitUiEvent,
      setFullscreenHistorySource )
import Agent.CLI.TUI.History
    ( HistoryGeneration(HistoryGeneration) )
import Agent.CLI.TUI.SessionHistory ()
import Agent.CLI.Terminal ( resolveColor )
import Agent.CLI.Tools ()
import Agent.CLI.Turn ()
import Agent.CLI.Usage ()
import Agent.CLI.WebFetch
    ( WebFetchRuntime
    , closeWebFetchRuntime
    , newWebFetchRuntime
    , webFetchRuntimeTool
    )
import Agent.CLI.Worktree
    ( acquireWorktreeLease,
      cleanupStaleWorktrees,
      createManagedWorktree,
      defaultWorktreeKeepCount,
      releaseWorktreeLease,
      removeWorktree,
      worktreeRoot,
      WorktreeCleanupReport(..) )
import Agent.Cancel ()
import Agent.Claude ()
import Agent.Dialect
    ( Dialect
    , dialectForId
    , DialectId(CodexDialect, GrokBuildDialect)
    )
import Agent.Error (ApiError)
import Agent.GrokBuild.Dialect.Goal ()
import Agent.GrokBuild.Dialect.Runtime ()
import Agent.GrokBuild.Dialect.Task
    ( GrokSubagentSpecs
    , grokRootChildModels
    )
import Agent.GrokBuild.Dialect.Workflow ()
import Agent.Loop
    ( TurnInput(UserMessage, AgentMessage),
      LoopError(LoopNoResponseId) )
import Agent.OpenAI.Compaction ()
import Agent.OpenAI.ImageGeneration
    ( ImageGenerationHistory
    , clearImageGenerationHistory
    , imageGenerationTool
    , newImageGenerationHistory
    , recordImageGenerationImages
    , recordImageGenerationResponseItems
    )
import Agent.OpenAI.Usage ()
import Agent.OpenAI.WebSocketClient ()
import Agent.OpenRouter.LoopBackend ()
import Agent.OsPath ( unsafeToFilePath )
import Agent.Provider
    ( Credential, TokenProvider,
      Provider(XAIProvider, OpenRouterProvider, OpenAIProvider)
    , tokenProviderBillingMode
    )
import Agent.ReasoningEffort
    ( parseReasoningEffort, reasoningEffortText )
import Agent.Responses.GenericBackend ()
import Agent.Responses.GenericClient ( GenericClientOptions(..) )
import Agent.Responses.Types ( ResponseItem )
import Agent.ResourceScope
    ( allocateResource
    , allocateFourResourcesConcurrently
    , releaseResource
    , withResourceScope
    )
import Agent.Skills
    ( SkillCatalog
    , SkillInvocation
    )
import Agent.Store.Postgres ( trustedPool )
import Agent.Store.Types ()
import Agent.Subagents
    ( RootTurnId,
      formatCompletionNotice,
      closeSubagentRegistry,
      newSubagentRegistry,
      setSubagentOnComplete,
      setSubagentOnSettled,
      interruptActiveSubagents,
      defaultMaxConcurrent,
      defaultSubagentConfig,
      SubagentConfig(maxConcurrent),
      SubagentId,
      SubagentRegistry )
import Agent.Subagents.TaskPath ( taskPathRoot )
import Agent.TUI.Model ( UiEvent(UiSetNotice) )
import Agent.TUI.Motion ()
import Agent.Tools.MultiAgents
    ( CollaborationModelTarget(..)
    , MultiAgentContext(..)
    , SubagentWorktree(..)
    )
import Agent.Tools.PlanMode
    ( PlanModeEnv(planSessionDir, planStateRef),
      activatePlanMode,
      PlanModeHooks(planAskQuestion, PlanModeHooks, planConfirmEnter,
                    planDecideExit),
      PlanDecision(PlanCancel),
      PlanModeState(PlanPending) )
import Agent.Tools.Secret
    ( SecretPrompt(..), SecretPromptHooks(..) )
import Agent.Tools.ShowImage
    ( ImageDisplayHooks(..), ImageDisplayRequest(..) )
import Agent.Tools.TaskPlan (TaskPlanEnv, newTaskPlanEnv)
import Agent.Tools.Types
    ( AppTool
    , AppToolGroup(..)
    , ToolEnv(..)
    , appToolsFromGroups
    , setToolSessionTmp
    , withToolHumanInputWait
    )
import Agent.XAI.LoopBackend ()
import Control.Applicative ( (<|>) )
import Control.Concurrent.Async ( concurrently, concurrently_ )
import Control.Concurrent.Chan ()
import Control.Concurrent.MVar ()
import Control.Concurrent.STM ()
import Control.Exception ()
import Control.Exception.Safe
    ( SomeException, bracketOnError, finally, onException, throwIO, try )
import Control.Monad ( forM_, join, unless, when )
import Data.Functor ()
import Data.IORef
    (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List ()
import Data.Map.Strict (Map)
import Data.Maybe ( isNothing, fromMaybe, isJust )
import Data.Set (Set)
import Data.Text (Text)
import Data.Time.Clock ()
import System.Console.ANSI ()
import System.Console.ANSI.Codes ()
import System.Directory.OsPath ()
import System.Environment ()
import System.Exit ()
import System.IO (Handle)
import System.Info (os)
import System.OsPath (OsPath)
import qualified Data.ByteString as BS ()
import qualified Agent.Responses.GenericClient as GenericResponses
    ()
import qualified Agent.MCP as MCP
    ( McpSupervisor,
      acquireMcpFleetProgressive,
      acquireMcpFleetWithProgress,
      mcpFleetGrokMetaTools,
      mcpFleetInstructions,
      mcpFleetMetaTools,
      mcpFleetResourceTools,
      mcpFleetStatuses,
      mcpFleetTools,
      releaseMcpFleetLease,
      McpFleet(mcpFleetWarnings),
      McpFleetLease(mcpLeaseFleet),
      McpServerConfig(mcpServerRequestTimeoutSeconds, McpServerConfig,
                      mcpServerName, mcpServerUrl, mcpServerCommand, mcpServerArgs, mcpServerCwd,
                      mcpServerEnv, mcpServerStartupTimeoutSeconds, mcpServerProtocol) )
import qualified Data.Map.Strict as Map
    ( toAscList, empty, lookup, notMember )
import qualified Agent.OpenAI.Auth as OpenAI ()
import qualified Agent.OpenRouter as OpenRouter
    ( ClientOptions
    , clientOptionsFromEnv
    , mapModel
    )
import qualified Agent.OpenRouter.Usage as OpenRouterUsage ()
import qualified Agent.Provider as Provider ()
import qualified Agent.CLI.Session.Lifecycle as SessionLifecycle ()
import qualified Agent.CLI.Session.Runner as SessionRunner ()
import qualified Data.Set as Set ()
import qualified Data.Text as Text ( intercalate, pack, strip, unpack )
import qualified Data.Text.IO as Text ()
import qualified Agent.XAI.Options as XAI ()
import qualified Agent.XAI.Client as XAIClient ()
import qualified Agent.XAI.Request as XAIRequest ()
import qualified Agent.XAI.Usage as XAIUsage ()

data AgentToolsRequest windowTitleResult = AgentToolsRequest
    { runAgentChild :: AgentRunMode -> CliOptions -> IO DevResult
    , loaded :: LoadedAuth
    , connectedGateway :: Maybe GatewayCredential
    , learnAboutUserRequested :: Bool
    , customBearerToken :: Maybe Text
    , activeAccountIdRef :: IORef Text
    , activeAccountRef :: IORef Text
    , activeSelectionRef :: IORef Text
    , baseToolEnv :: ToolEnv
    , catalog :: ModelCatalog
    , initialSkills :: SkillCatalog
    , gatewayModelsRef :: IORef (Maybe GatewayModelAccess)
    , gatewayIdentity :: Maybe Text
    , checkStartupUsageInBackground :: Bool
    , configuredOptionTarget :: Maybe ModelTarget
    , customResponses :: Maybe (Text, ResponsesConnection)
    , cwd :: OsPath
    , databaseScopes :: DatabaseScopes
    , escPaused :: IORef Bool
    , fullscreen :: Maybe FullscreenRuntime
    , home :: OsPath
    , interrupt :: InterruptState
    , isTty :: Bool
    , mcpSupervisor :: MCP.McpSupervisor
    , options :: CliOptions
    , pendingTurn :: Maybe PendingTurn
    , preferredOpenAiAccountRef :: IORef (Maybe Text)
    , processRuntime :: AgentProcessRuntime
    , projectRoot :: OsPath
    , projectSettings :: ProjectSettings
    , projectTarget :: Maybe ModelTarget
    , resolveActiveAccountLabel :: Credential -> IO Text
    , resumeLock :: Maybe SessionLock
    , resumed :: Maybe (SessionMeta, [SessionTurn])
    , resumedTarget :: Maybe ModelTarget
    , root :: OsPath
    , selectHttpAccount :: Text -> IO (Either ApiError Text)
    , selectableTokenProvider :: TokenProvider
    , setWindowTitle :: Text -> IO windowTitleResult
    , startup :: StartupRuntime
    , stateDirectory :: FilePath
    , stderrHandle :: Handle
    , targetHint :: Maybe ModelTarget
    , tokenProvider :: TokenProvider
    , transition :: Maybe ProviderTransition
    , transitionTarget :: Maybe ModelTarget
    , uiRuntimeRef :: IORef (Maybe FullscreenRuntime)
    , unavailableProviders :: Set Provider
    }

data ToolStartup = ToolStartup
    { toolNativeCapabilities :: NativeRunCapabilities
    , toolOpenRouterOptions :: OpenRouter.ClientOptions
    , toolHarnessConfig :: HarnessConfig
    , toolGatewaySelection :: Maybe ModelOption
    , toolGatewayAllowedChildModels :: Maybe [Text]
    }

data ToolHostHooks = ToolHostHooks
    { toolPlanHooks :: PlanModeHooks
    , toolSecretHooks :: Maybe SecretPromptHooks
    , toolImageHooks :: Maybe ImageDisplayHooks
    }

data ToolModelRuntime = ToolModelRuntime
    { toolProvider :: Provider
    , toolModel :: Text
    , toolTransportModel :: Text -> Text
    , toolInferredTarget :: ModelTarget
    , toolCustomGenericOptions :: Maybe GenericClientOptions
    , toolDialectId :: DialectId
    , toolDialect :: Dialect
    , toolResumeTargetChanged :: Bool
    , toolRefreshDialectContext :: Bool
    , toolLegacySubagentTarget :: Maybe LegacySubagentTarget
    , toolEffortText :: Text
    , toolPolicy :: ApprovalPolicy
    , toolClaudeBypassEnabled :: Bool
    }

data CollaborationRuntime = CollaborationRuntime
    { collaborationActiveSessionLock :: IORef (Maybe SessionLock)
    , collaborationPersistSlotRef :: IORef Persistence
    , collaborationSubagentSessions
        :: IORef (Map SubagentId SubagentSession)
    , collaborationSubagentStoreRoot :: SubagentStoreRoot
    , collaborationSubagentForkSource
        :: IORef (Maybe (IO [ResponseItem]))
    , collaborationPendingNotices :: PendingInputs
    , collaborationRegistry :: SubagentRegistry
    , collaborationRootTurnRef :: IORef (Maybe RootTurnId)
    , collaborationAgentTypes :: GrokSubagentSpecs
    , collaborationOpenAiChild :: Maybe TokenProvider
    , collaborationAllowedChildModels :: Maybe [Text]
    , collaborationChildModelAllowed :: Maybe (Text -> IO Bool)
    , collaborationResolveChildModel
        :: Maybe (Text -> IO (Maybe CollaborationModelTarget))
    , collaborationGatewayChildModelOption
        :: Maybe (Text -> IO (Maybe ModelOption))
    , collaborationCreateWorktree
        :: OsPath -> IO (Either Text SubagentWorktree)
    , collaborationContext :: Maybe MultiAgentContext
    , collaborationCloseAgents :: IO ()
    }

data ScratchRuntime = ScratchRuntime
    { scratchPromptRequest :: Maybe ManagedTurnRequest
    , scratchPersistence :: Persistence
    , scratchTaskPlan :: TaskPlanEnv
    , scratchSessionTmp :: OsPath
    , scratchImageGenerationHistory :: ImageGenerationHistory
    , scratchExternalSessionTools :: [AppTool]
    , scratchCleanup :: IO ()
    }

data McpRuntime = McpRuntime
    { runtimeMcpServerConfigs :: [MCP.McpServerConfig]
    , runtimeProgressiveMcp :: Bool
    , runtimeMcpFleet :: MCP.McpFleet
    , runtimeMcpInstructions :: [(Text, Text)]
    , runtimeCloseMcp :: IO ()
    }

data LocalToolRuntime = LocalToolRuntime
    { localCoding :: CodingTools
    , localInitialSkills :: SkillCatalog
    }

data CodingRuntime = CodingRuntime
    { runtimeCoding :: CodingTools
    , runtimeExtraTools :: [AppTool]
    , runtimeCloseExtraTools :: IO ()
    }

data SessionControlRuntime = SessionControlRuntime
    { controlGhciEnabledRef :: IORef Bool
    , controlBashEnabledRef :: IORef Bool
    , controlSkillsRef :: IORef SkillCatalog
    , controlSkillInvocationsRef :: IORef [SkillInvocation]
    , controlCodeModeCloseRef :: IORef (IO ())
    , controlClaimCurrentSession :: SessionHandle -> IO ()
    , controlSessionTools :: [AppTool]
    }

data SessionToolsRuntime = SessionToolsRuntime
    { sessionAllTools :: [AppTool]
    , sessionTools :: [AppTool]
    , sessionMcpTools :: [AppTool]
    , sessionDatabaseTools :: [AppTool]
    , sessionLearnedSkillTools :: [AppTool]
    , sessionGatewayTools :: [AppTool]
    , sessionPlanMode :: PlanModeEnv
    , sessionNoteDirectory :: OsPath -> IO ()
    , sessionCloseAll :: IO ()
    , sessionResumedPlanPending :: Bool
    }

runAgentTools
    :: (AgentRunMode -> CliOptions -> IO DevResult)
    -> LoadedAuth
    -> Maybe GatewayCredential
    -> Bool
    -> Maybe Text
    -> IORef Text
    -> IORef Text
    -> IORef Text
    -> ToolEnv
    -> ModelCatalog
    -> SkillCatalog
    -> IORef (Maybe GatewayModelAccess)
    -> Maybe Text
    -> Bool
    -> Maybe ModelTarget
    -> Maybe (Text, ResponsesConnection)
    -> OsPath
    -> DatabaseScopes
    -> IORef Bool
    -> Maybe FullscreenRuntime
    -> OsPath
    -> InterruptState
    -> Bool
    -> MCP.McpSupervisor
    -> CliOptions
    -> Maybe PendingTurn
    -> IORef (Maybe Text)
    -> AgentProcessRuntime
    -> OsPath
    -> ProjectSettings
    -> Maybe ModelTarget
    -> (Credential -> IO Text)
    -> Maybe SessionLock
    -> Maybe (SessionMeta, [SessionTurn])
    -> Maybe ModelTarget
    -> OsPath
    -> (Text -> IO (Either ApiError Text))
    -> TokenProvider
    -> (Text -> IO windowTitleResult)
    -> StartupRuntime
    -> FilePath
    -> Handle
    -> Maybe ModelTarget
    -> TokenProvider
    -> Maybe ProviderTransition
    -> Maybe ModelTarget
    -> IORef (Maybe FullscreenRuntime)
    -> Set Provider
    -> IO RunResult
runAgentTools
    runAgentChild
    loaded
    connectedGateway
    learnAboutUserRequested
    customBearerToken
    activeAccountIdRef
    activeAccountRef
    activeSelectionRef
    baseToolEnv
    catalog
    initialSkills
    gatewayModelsRef
    gatewayIdentity
    checkStartupUsageInBackground
    configuredOptionTarget
    customResponses
    cwd
    databaseScopes
    escPaused
    fullscreen
    home
    interrupt
    isTty
    mcpSupervisor
    options
    pendingTurn
    preferredOpenAiAccountRef
    processRuntime
    projectRoot
    projectSettings
    projectTarget
    resolveActiveAccountLabel
    resumeLock
    resumed
    resumedTarget
    root
    selectHttpAccount
    selectableTokenProvider
    setWindowTitle
    startup
    stateDirectory
    stderrHandle
    targetHint
    tokenProvider
    transition
    transitionTarget
    uiRuntimeRef
    unavailableProviders
    =
    runAgentToolsRequest AgentToolsRequest{..}

runAgentToolsRequest
    :: AgentToolsRequest windowTitleResult
    -> IO RunResult
runAgentToolsRequest request = withResourceScope \resourceScope -> do
    toolStartup <- loadToolStartup request
    let toolModelRuntime = resolveToolModel request toolStartup
        toolHostHooks =
            buildToolHostHooks request toolStartup toolModelRuntime
    collaborationRuntime <-
        newCollaborationRuntime request toolStartup toolModelRuntime
    (scratchKey, acquiredScratchRuntime) <-
        allocateResource
            resourceScope
            (prepareScratchRuntime
                request
                toolStartup
                toolModelRuntime
                collaborationRuntime)
            (.scratchCleanup)
    let scratchRuntime =
            acquiredScratchRuntime
                { scratchCleanup = releaseResource scratchKey }
    (acquiredResources, (initialContext, initialContextPreload)) <-
        concurrently
            ( allocateFourResourcesConcurrently
                resourceScope
                (acquireMcpRuntime
                    request
                    toolStartup
                    toolModelRuntime
                    collaborationRuntime
                    scratchRuntime)
                (.runtimeCloseMcp)
                (acquireLocalToolRuntime
                    request
                    toolModelRuntime
                    toolHostHooks
                    collaborationRuntime
                    scratchRuntime)
                (.localCoding.codingClose)
                (acquireWebFetchRuntime
                    request
                    toolStartup
                    toolModelRuntime)
                (mapM_ closeWebFetchRuntime)
                (acquireLspStartup
                    request
                    toolStartup
                    toolModelRuntime)
                (mapM_ closeLspRuntime . (.lspStartupRuntime))
            )
            (prepareInitialContextPreload request toolModelRuntime)
    let ( (mcpKey, acquiredMcpRuntime)
          , (localToolKey, acquiredLocalToolRuntime)
          , (webFetchKey, webFetchRuntime)
          , (lspKey, lspStartup)
          ) = acquiredResources
        mcpRuntime =
            acquiredMcpRuntime
                { runtimeCloseMcp = releaseResource mcpKey }
        localToolRuntime =
            acquiredLocalToolRuntime
                { localCoding =
                    acquiredLocalToolRuntime.localCoding
                        { codingClose = releaseResource localToolKey }
                }
        lspRuntime = lspStartup.lspStartupRuntime
        runtimeCoding = localToolRuntime.localCoding
        runtimeExtraTools =
            maybe [] (pure . webFetchRuntimeTool) webFetchRuntime
                <> maybe [] (pure . lspRuntimeTool) lspRuntime
        runtimeCloseExtraTools =
            concurrently_
                (releaseResource lspKey)
                (releaseResource webFetchKey)
        codingRuntime = CodingRuntime{..}
    mapM_
        (reportStartupWarning request.startup)
        lspStartup.lspStartupWarnings
    installCollaborationCallbacks request collaborationRuntime
    sessionControlRuntime <-
        newSessionControlRuntime
            request
            toolStartup
            toolModelRuntime
            collaborationRuntime
            localToolRuntime.localInitialSkills
    sessionToolsRuntime <-
        assembleSessionToolsRuntime
            request
            toolStartup
            toolModelRuntime
            toolHostHooks
            collaborationRuntime
            scratchRuntime
            mcpRuntime
            codingRuntime
            sessionControlRuntime
    launchAgentToolsSession
        request
        toolStartup
        toolModelRuntime
        toolHostHooks
        collaborationRuntime
        scratchRuntime
        mcpRuntime
        codingRuntime
        initialContext
        initialContextPreload
        sessionControlRuntime
        sessionToolsRuntime

loadToolStartup
    :: AgentToolsRequest windowTitleResult
    -> IO ToolStartup
loadToolStartup request@AgentToolsRequest
    { loaded
    , connectedGateway
    , gatewayIdentity
    , startup
    } = do
    let toolNativeCapabilities =
            maybe
                fullNativeRunCapabilities
                (.nativeCapabilities)
                startup.startupNativeHooks
    toolOpenRouterOptions <- OpenRouter.clientOptionsFromEnv
    -- Tool resources and initial-context discovery now share one startup
    -- frontier, so attribute the elapsed interval to both.
    markStartupStage startup "Loading tools and context…"
    when (isGatewayLoadedAuth loaded /= isJust gatewayIdentity) $
        startupDie startup
            "gateway session binding and loaded credentials disagree"
    when ((gatewayCredentialIdentity <$> connectedGateway) /= gatewayIdentity) $
        startupDie startup
            "gateway credential snapshot and session binding disagree"
    let toolHarnessConfig = startup.startupHarnessConfig
    (toolGatewaySelection, toolGatewayAllowedChildModels) <-
        selectGatewayModels request
    pure ToolStartup{..}

selectGatewayModels
    :: AgentToolsRequest windowTitleResult
    -> IO (Maybe ModelOption, Maybe [Text])
selectGatewayModels AgentToolsRequest
    { loaded
    , catalog
    , gatewayModelsRef
    , options
    , transitionTarget
    , configuredOptionTarget
    , resumedTarget
    , projectTarget
    , targetHint
    , startup
    }
    | not (isGatewayLoadedAuth loaded) = pure (Nothing, Nothing)
    | otherwise = do
        access <-
            readIORef gatewayModelsRef >>= \case
                Nothing ->
                    startupDie startup
                        "The organization gateway model catalog is unavailable."
                Just value -> pure value
        cachedGatewayModels access >>= \case
            Nothing ->
                startupDie startup
                    "The organization gateway model catalog is unavailable."
            Just models ->
                case modelOptionsForGatewayModels catalog models of
                    [] ->
                        startupDie startup
                            "The organization gateway does not offer any models."
                    firstAvailable : remainingAvailable -> do
                        let available = firstAvailable : remainingAvailable
                            resolveTarget target =
                                resolveModelOptionById
                                    available
                                    target.targetModelId
                        selected <- case options.optModel of
                            Just requested ->
                                case resolveModelOptionById available requested of
                                    Nothing ->
                                        startupDie startup $
                                            "Model '"
                                                <> requested
                                                <> "' is not available through your organization gateway."
                                    Just selected -> pure selected
                            Nothing ->
                                pure $
                                    fromMaybe firstAvailable $
                                        (transitionTarget >>= resolveTarget)
                                            <|> (configuredOptionTarget >>= resolveTarget)
                                            <|> (resumedTarget >>= resolveTarget)
                                            <|> (projectTarget >>= resolveTarget)
                                            <|> (targetHint >>= resolveTarget)
                        pure
                            ( Just selected
                            , Just (map (.modelTarget.targetModelId) available)
                            )

resolveToolModel
    :: AgentToolsRequest windowTitleResult
    -> ToolStartup
    -> ToolModelRuntime
resolveToolModel AgentToolsRequest
    { loaded
    , catalog
    , targetHint
    , options
    , customResponses
    , customBearerToken
    , resumed
    , projectSettings
    , transitionTarget
    , isTty
    , startup
    } ToolStartup
    { toolOpenRouterOptions = openRouterOptions
    , toolGatewaySelection = gatewaySelection
    } =
    ToolModelRuntime{..}
  where
    toolProvider = loaded.loadedProvider
    fallbackModel =
        fromMaybe
            (error "validated default model is missing")
            (defaultModelFor catalog toolProvider)
    unrestrictedModel =
        fromMaybe
            (maybe fallbackModel (.targetModelId) targetHint)
            options.optModel
    toolModel =
        maybe
            unrestrictedModel
            (.modelTarget.targetModelId)
            gatewaySelection
    rawTarget = (rawModelOption toolProvider toolModel).modelTarget
    inferredTarget0 =
        maybe
            (fromMaybe rawTarget targetHint)
            (.modelTarget)
            gatewaySelection
    toolTransportModel = case customResponses of
        Just _ ->
            \name ->
                case resolveConfiguredModel catalog name of
                    Just option
                        | option.modelTarget.targetConnectionId
                            == inferredTarget0.targetConnectionId ->
                            option.modelTarget.targetWireModelId
                    _
                        | name == toolModel ->
                            inferredTarget0.targetWireModelId
                        | otherwise -> name
        _ -> case toolProvider of
            OpenRouterProvider -> OpenRouter.mapModel openRouterOptions
            _ -> id
    toolInferredTarget =
        inferredTarget0
            { targetWireModelId =
                if inferredTarget0.targetConnectionId
                    == builtinConnectionId OpenRouterProvider
                    && inferredTarget0.targetWireModelId
                        == inferredTarget0.targetModelId
                    then toolTransportModel toolModel
                    else inferredTarget0.targetWireModelId
            }
    toolCustomGenericOptions = do
        (_, responses) <- customResponses
        pure GenericClientOptions
            { baseUrl = Text.unpack responses.responsesBaseUrl
            , model = toolInferredTarget.targetWireModelId
            , bearerToken = customBearerToken
            , requestTimeoutSeconds =
                responses.responsesRequestTimeoutSeconds
            }
    persistedTarget = case fst <$> resumed of
        Just meta ->
            Just
                ( meta.metaDialect
                , meta.metaTransportModel
                )
        Nothing -> do
            remembered <- projectSettings.settingsLastModel
            let target = remembered.projectModelTarget
            if target.targetProvider == toolProvider
                then Just
                    ( target.targetDialect
                    , Just target.targetWireModelId
                    )
                else Nothing
    resolvedPersistedTarget =
        (\(storedDialect, storedTransportModel) ->
            resolvePersistedDialect
                storedDialect
                storedTransportModel
                toolInferredTarget)
            <$> persistedTarget
    mappedTargetChanged = maybe False snd resolvedPersistedTarget
    toolDialectId = case gatewaySelection of
        Just selected -> selected.modelTarget.targetDialect
        Nothing -> case transitionTarget of
            Just target -> target.targetDialect
            Nothing -> case options.optModel of
                Just _ -> toolInferredTarget.targetDialect
                Nothing
                    | mappedTargetChanged -> toolInferredTarget.targetDialect
                    | otherwise ->
                        maybe
                            toolInferredTarget.targetDialect
                            fst
                            resolvedPersistedTarget
    toolDialect = dialectForId toolDialectId
    toolResumeTargetChanged = case fst <$> resumed of
        Just meta ->
            toolProvider /= meta.metaProvider
                || toolInferredTarget.targetConnectionId /= meta.metaConnection
                || toolModel /= meta.metaModel
                || mappedTargetChanged
                || toolDialectId /= meta.metaDialect
        Nothing -> False
    toolRefreshDialectContext = case fst <$> resumed of
        Just meta -> toolDialectId /= meta.metaDialect
        Nothing -> False
    toolLegacySubagentTarget =
        sessionLegacySubagentTarget . fst <$> resumed
    effort =
        normalizeReasoningEffortForDialect toolDialectId $
            fromMaybe
                (maybe
                    (defaultEffortFor toolProvider)
                    (either
                        (const (defaultEffortFor toolProvider))
                        id
                        . parseReasoningEffort
                        . (.metaEffort))
                    (fst <$> resumed))
                options.optEffort
    toolEffortText = reasoningEffortText effort
    toolPolicy = case startup.startupNativeHooks of
        Just hooks -> case hooks.nativeInteractionMode of
            NativeYolo -> ApproveAll
            NativeAsk -> PromptMutating
            NativePlan -> PromptMutating
        Nothing ->
            resolveApprovalPolicy options isTty
                projectSettings.settingsAutoApprove
    toolClaudeBypassEnabled =
        case startup.startupNativeHooks of
            Just hooks ->
                hooks.nativeInteractionMode == NativeYolo
            Nothing ->
                not options.optNoYolo
                    && (options.optYolo
                        || projectSettings.settingsAutoApprove)

buildToolHostHooks
    :: AgentToolsRequest windowTitleResult
    -> ToolStartup
    -> ToolModelRuntime
    -> ToolHostHooks
buildToolHostHooks AgentToolsRequest
    { interrupt
    , escPaused
    , stderrHandle
    , uiRuntimeRef
    , options
    , isTty
    , startup
    } ToolStartup
    { toolNativeCapabilities = nativeCapabilities
    } ToolModelRuntime
    { toolProvider = provider
    } =
    ToolHostHooks{..}
  where
    basePlanHooks
        | Just hooks <- startup.startupNativeHooks =
            hooks.nativePlanHooks
        | startup.startupBackground =
            PlanModeHooks
                { planConfirmEnter = \_ -> pure False
                , planDecideExit = \_ -> pure PlanCancel
                , planAskQuestion = \_ _ -> pure Nothing
                }
        | otherwise =
            cliPlanHooks
                provider interrupt escPaused (resolveColor stderrHandle)
    toolPlanHooks = fullscreenAwarePlanHooks uiRuntimeRef basePlanHooks
    baseSecretHooks = SecretPromptHooks \request ->
        Right <$> promptSecretLine
            escPaused
            request.secretPromptMessage
            request.secretPromptPurpose
    toolSecretHooks
        | not nativeCapabilities.nativeHostExtensions
            || isOneShot options || not isTty = Nothing
        | otherwise =
            Just (fullscreenAwareSecretHooks uiRuntimeRef baseSecretHooks)
    -- Outside the retained TUI, agent-displayed images print inline with the
    -- same graphics path as pasted attachments.
    baseImageHooks = ImageDisplayHooks \request -> do
        color <- resolveColor stderrHandle
        putImagePreview
            startup.startupSessionState.sessionPreviewId
            color
            [request.displayImage]
        pure (Right ())
    toolImageHooks
        | not nativeCapabilities.nativeHostExtensions || not isTty =
            Nothing
        | otherwise =
            Just (fullscreenAwareImageHooks uiRuntimeRef baseImageHooks)

newCollaborationRuntime
    :: AgentToolsRequest windowTitleResult
    -> ToolStartup
    -> ToolModelRuntime
    -> IO CollaborationRuntime
newCollaborationRuntime AgentToolsRequest
    { resumeLock
    , options
    , projectSettings
    , cwd
    , gatewayModelsRef
    , catalog
    , home
    , tokenProvider
    } ToolStartup
    { toolNativeCapabilities = nativeCapabilities
    , toolHarnessConfig = harnessConfig
    , toolGatewayAllowedChildModels = gatewayAllowedChildModels
    } ToolModelRuntime
    { toolProvider = provider
    , toolTransportModel = transportModel
    , toolInferredTarget = inferredTarget
    , toolDialectId = dialectId
    , toolLegacySubagentTarget = legacySubagentTarget
    , toolEffortText = effortText
    } = do
    -- Plan mode itself is process-local, while the assistant's proposed plan
    -- is durable in the session transcript. Reconstruct the approval phase
    -- before entering the REPL so a resumed Codex session cannot interpret
    -- the user's approval as ordinary steering input.
    -- Keep inferred startup, resume, and delegated-agent targets session-local.
    -- Live top-level model/provider switches persist their selection in
    -- Agent.CLI.Provider.Switch instead.
    collaborationActiveSessionLock <- newIORef resumeLock
    collaborationPersistSlotRef <- newIORef PersistenceDisabled
    -- Per-subagent transcripts / previous ids, shared across send_input / task.
    collaborationSubagentSessions <- newIORef Map.empty
    collaborationSubagentStoreRoot <- newIORef Nothing
    collaborationSubagentForkSource <-
        newIORef (Nothing :: Maybe (IO [ResponseItem]))
    collaborationPendingNotices <- newPendingInputs
    let maxConcurrentAgents =
            fromMaybe defaultMaxConcurrent $
                options.optMaxConcurrentAgents
                    <|> projectSettings.settingsMaxConcurrentAgents
                    <|> harnessConfig.configMaxConcurrentAgents
    collaborationRegistry <- newSubagentRegistry
        defaultSubagentConfig { maxConcurrent = maxConcurrentAgents }
        cwd
        (\_ _ _ _ -> pure $ Left LoopNoResponseId)
        (\_ _ -> pure ())
    collaborationRootTurnRef <- newIORef (Nothing :: Maybe RootTurnId)
    collaborationAgentTypes <- newIORef Map.empty
    collaborationOpenAiChild <-
        if not nativeCapabilities.nativeCollaboration
            then pure Nothing
            else case provider of
                XAIProvider -> do
                    available <- hasOpenAiAuth
                    if not available
                        then pure Nothing
                        else loadAuth (Just OpenAIProvider) >>= \case
                            Left _ -> pure Nothing
                            Right openaiLoaded ->
                                pure (Just openaiLoaded.loadedTokenProvider)
                _ -> pure Nothing
    let collaborationAllowedChildModels =
            case gatewayAllowedChildModels of
                Just modelIds -> Just modelIds
                Nothing -> case provider of
                    XAIProvider ->
                        Just
                            (grokRootChildModels
                                (isJust collaborationOpenAiChild))
                    _ -> Nothing
        collaborationChildModelAllowed
            | Just resolve <- collaborationGatewayChildModelOption =
                Just \modelId -> isJust <$> resolve modelId
            | otherwise = Nothing
        collaborationResolveChildModel
            | Just resolve <- collaborationGatewayChildModelOption =
                Just \modelId ->
                    fmap toCollaborationTarget <$> resolve modelId
            | otherwise = Nothing
        toCollaborationTarget option =
            let target = option.modelTarget
            in CollaborationModelTarget
                { collaborationTargetProvider = target.targetProvider
                , collaborationTargetConnection = target.targetConnectionId
                , collaborationTargetEffectiveModel =
                    target.targetWireModelId
                , collaborationTargetDialect = target.targetDialect
                }
        collaborationGatewayChildModelOption
            | isNothing gatewayAllowedChildModels = Nothing
            | otherwise =
                Just \requested ->
                    readIORef gatewayModelsRef >>= \case
                        Nothing -> pure Nothing
                        Just access ->
                            cachedGatewayModels access >>= \case
                                Nothing -> pure Nothing
                                Just models ->
                                    pure
                                        (resolveModelOptionById
                                            (modelOptionsForGatewayModels
                                                catalog models)
                                            (Text.strip requested))
        sendToRoot message = do
            enqueuePendingInput
                collaborationPendingNotices
                (AgentMessage message) >>= \case
                    Left err -> pure (Left err)
                    Right () -> pure (Right "queued")
        collaborationCreateWorktree source =
            createManagedWorktree home source >>= \case
                Left err -> pure (Left err)
                Right path -> pure $ Right SubagentWorktree
                    { subagentWorktreePath = path
                    , subagentWorktreeCleanup =
                        removeWorktree source path >>= \case
                            Left err -> pure (Left err)
                            Right () -> pure (Right ())
                    }
        collaborationContext
            | not nativeCapabilities.nativeCollaboration = Nothing
            | otherwise = Just MultiAgentContext
                { multiRegistry = collaborationRegistry
                , multiCwd = cwd
                , multiSelfId = Nothing
                , multiDepth = 0
                , multiTaskPath = taskPathRoot
                , multiRootTurnId = readIORef collaborationRootTurnRef
                , multiResumeFromDisk = Just
                    (restoreAgentFromDisk
                        provider
                        inferredTarget.targetConnectionId
                        transportModel
                        inferredTarget.targetWireModelId
                        dialectId
                        legacySubagentTarget
                        collaborationSubagentStoreRoot
                        collaborationRegistry
                        collaborationSubagentSessions
                        collaborationResolveChildModel
                        collaborationAgentTypes)
                , multiCreateWorktree = Just collaborationCreateWorktree
                , multiPrepareSpawn = Just
                    (prepareCollaborationSpawn
                        provider
                        inferredTarget.targetConnectionId
                        transportModel
                        inferredTarget.targetWireModelId
                        effortText
                        dialectId
                        legacySubagentTarget
                        collaborationSubagentSessions
                        collaborationSubagentStoreRoot
                        collaborationAgentTypes
                        collaborationSubagentForkSource)
                , multiSendToRoot = Just sendToRoot
                , multiSpawnModelGuidance =
                    if isJust gatewayAllowedChildModels
                        then Nothing
                        else
                            subscriptionSubagentModelGuidance
                                provider
                                (tokenProviderBillingMode tokenProvider)
                , multiAllowedChildModels =
                    collaborationAllowedChildModels
                , multiResolveChildModel =
                    collaborationResolveChildModel
                , multiChildModelAllowed =
                    collaborationChildModelAllowed
                }
        collaborationCloseAgents =
            case collaborationContext of
                Just ctx -> do
                    interruptActiveSubagents ctx.multiRegistry
                    flushAllSubagentSnapshots
                        collaborationSubagentStoreRoot
                        ctx.multiRegistry
                        collaborationSubagentSessions
                        collaborationAgentTypes
                    closeSubagentRegistry ctx.multiRegistry
                Nothing -> pure ()
    pure CollaborationRuntime{..}

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
    scratchPersistence <-
        preparePersistence
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
            resumed
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
    (scratchSessionTmp, ephemeralSessionId) <-
        persistenceTempDir scratchPersistence >>= \case
            Just tempDir -> pure (tempDir, Nothing)
            Nothing -> do
                (sessionId, tempDir) <- allocateSessionTemp root
                pure (tempDir, Just sessionId)
    setToolSessionTmp baseToolEnv (Just scratchSessionTmp)
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
    let cleanupAllocatedScratch = do
            cleanupPendingPersistence scratchPersistence
            forM_ ephemeralSessionId \sessionId -> do
                _ <- removeSessionTemp root sessionId
                pure ()
    worktreeLease <-
        acquireWorktreeLease (worktreeRoot home) cwd >>= \case
            Left err -> do
                cleanupAllocatedScratch
                startupDie startup err
            Right lease -> pure lease
    sessionTempLease <-
        (acquireSessionTempLease root scratchSessionTmp
            `onException`
                (mapM_ releaseWorktreeLease worktreeLease
                    >> cleanupAllocatedScratch)) >>= \case
                Left err -> do
                    mapM_ releaseWorktreeLease worktreeLease
                    cleanupAllocatedScratch
                    startupDie startup err
                Right lease -> pure lease
    let scratchCleanup =
            mapM_ releaseSessionTempLease sessionTempLease
                `finally`
                    (mapM_ releaseWorktreeLease worktreeLease
                        `finally` cleanupAllocatedScratch)
    pure ScratchRuntime{..}

mcpConfiguration
    :: AgentToolsRequest windowTitleResult
    -> ToolStartup
    -> ([MCP.McpServerConfig], Bool)
mcpConfiguration AgentToolsRequest
    { cwd
    , home
    , options
    } ToolStartup
    { toolNativeCapabilities = nativeCapabilities
    , toolHarnessConfig = harnessConfig
    } =
    ( serverConfigs
    , useProgressiveMcp
        harnessConfig.configMcpInitStrategy
        (isOneShot options)
    )
  where
    serverConfigs =
        [ MCP.McpServerConfig
            { MCP.mcpServerName = label
            , MCP.mcpServerUrl = config.mcpUrl
            , MCP.mcpServerCommand = Text.unpack config.mcpCommand
            , MCP.mcpServerArgs = map Text.unpack config.mcpArgs
            , MCP.mcpServerCwd =
                Just $
                    maybe (unsafeToFilePath cwd) Text.unpack config.mcpCwd
            , MCP.mcpServerEnv =
                [ (Text.unpack name, Text.unpack value)
                | (name, value) <- Map.toAscList config.mcpEnv
                ] <> case config.mcpUrl of
                    Just url
                        | Map.notMember
                            "MCP_OAUTH_TOKEN_FILE"
                            config.mcpEnv ->
                            [ ( "MCP_OAUTH_TOKEN_FILE"
                              , unsafeToFilePath
                                    (mcpOAuthStorePath home url)
                              )
                            ]
                    _ -> []
            , MCP.mcpServerStartupTimeoutSeconds =
                config.mcpStartupTimeoutSeconds
            , MCP.mcpServerRequestTimeoutSeconds =
                config.mcpRequestTimeoutSeconds
            , MCP.mcpServerProtocol = config.mcpProtocol
            }
        | (label, config) <-
            Map.toAscList harnessConfig.configMcpServers
        , config.mcpEnabled
        , nativeCapabilities.nativeHostExtensions
        ]

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
            (sessions, sessionWarnings) <-
                listSessions
                    (trustedPool startup.startupDatabaseStore)
                    root
            let protectedWorktrees =
                    -- Persisted sessions must remain resumable. A worktree
                    -- becomes collectible after its session is deleted.
                    cwd : map (.metaCwd) sessions
            (worktreeReport, tempReport) <- concurrently
                (if null sessionWarnings
                    then cleanupStaleWorktrees
                        (worktreeRoot home)
                        defaultWorktreeKeepCount
                        protectedWorktrees
                    -- A partial session catalog cannot prove that an old
                    -- checkout is unreferenced.
                    else pure mempty)
                (cleanupStaleSessionTemps
                    root
                    defaultSessionTempKeepCount
                    [sessionTmp])
            pure (sessionWarnings, worktreeReport, tempReport)
        case cleanupResult of
            Left exception ->
                reportStartupWarning startup
                    ("stale resource cleanup failed: "
                        <> formatException exception)
            Right (sessionWarnings, worktreeReport, tempReport) -> do
                mapM_ (reportStartupWarning startup) sessionWarnings
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

acquireMcpRuntime
    :: AgentToolsRequest windowTitleResult
    -> ToolStartup
    -> ToolModelRuntime
    -> CollaborationRuntime
    -> ScratchRuntime
    -> IO McpRuntime
acquireMcpRuntime request@AgentToolsRequest
    { processRuntime
    , startup
    , options
    , isTty
    , escPaused
    , uiRuntimeRef
    , baseToolEnv
    , mcpSupervisor
    } toolStartup ToolModelRuntime
    { toolDialectId = dialectId
    } CollaborationRuntime
    { collaborationPendingNotices = pendingNotices
    } ScratchRuntime
    { scratchSessionTmp = sessionTmp
    } = do
    let (runtimeMcpServerConfigs, runtimeProgressiveMcp) =
            mcpConfiguration request toolStartup
    startStaleResourceCleanup request sessionTmp
    mcpStatusPhaseRef <- newIORef (Nothing :: Maybe Bool)
    mcpFleetRef <- newIORef (Nothing :: Maybe MCP.McpFleet)
    writeIORef processRuntime.processMcpElicitation
        (if isOneShot options || not isTty
            then Nothing
            else Just \elicitation ->
                withToolHumanInputWait baseToolEnv $
                    cliMcpElicitation escPaused uiRuntimeRef elicitation)
    let enqueueMcpSnapshot statuses =
            unless (null statuses) do
                instructions <-
                    readIORef mcpFleetRef
                        >>= maybe (pure []) MCP.mcpFleetInstructions
                enqueuePendingNotice pendingNotices PendingMcpNotice
                    (UserMessage
                        (formatMcpModelNoticeFor dialectId statuses
                            <> formatMcpInstructionsNotice instructions))
                    >>= either (reportStartupWarning startup) pure
        reportProgressiveMcp statuses = do
            finished <- readIORef startup.startupFinished
            unless finished do
                setStartupNotice startup.startupFullscreen
                    (formatMcpProgress statuses)
                -- A callback can race with finishStartup between the read and
                -- the UI update. Clear a late notice if startup won the race.
                readIORef startup.startupFinished >>= \nowFinished ->
                    when nowFinished $
                        forM_ startup.startupFullscreen \runtime ->
                            emitUiEvent runtime (UiSetNotice Nothing)
            let (connecting, _, _) = summarizeMcpStatuses statuses
                isConnecting = connecting > 0
            settled <-
                atomicModifyIORef' mcpStatusPhaseRef \previous ->
                    (Just isConnecting, previous == Just True && not isConnecting)
            when settled (enqueueMcpSnapshot statuses)
    let acquireMcpLease =
            try @_ @SomeException
                (if runtimeProgressiveMcp
                    then
                        MCP.acquireMcpFleetProgressive
                            mcpSupervisor
                            reportProgressiveMcp
                            runtimeMcpServerConfigs
                    else
                        MCP.acquireMcpFleetWithProgress
                            mcpSupervisor
                            (\names ->
                                setStartupNotice startup.startupFullscreen
                                    (if null names
                                        then "Loading built-in tools…"
                                        else
                                            "Loading tools: "
                                                <> Text.intercalate ", " names
                                                <> "…"))
                            runtimeMcpServerConfigs)
                >>= \case
                    Left exception ->
                        startupDie startup
                            ("Failed to initialize MCP tools: "
                                <> Text.pack (show exception))
                    Right lease -> pure lease
    bracketOnError
        acquireMcpLease
        MCP.releaseMcpFleetLease
        \runtimeMcpLease -> do
            let runtimeMcpFleet = runtimeMcpLease.mcpLeaseFleet
                runtimeCloseMcp =
                    MCP.releaseMcpFleetLease runtimeMcpLease
            writeIORef mcpFleetRef (Just runtimeMcpFleet)
            when runtimeProgressiveMcp $
                MCP.mcpFleetStatuses runtimeMcpFleet >>= enqueueMcpSnapshot
            currentMcpInstructions <- MCP.mcpFleetInstructions runtimeMcpFleet
            let runtimeMcpInstructions =
                    mcpInstructionsForRequest
                        runtimeProgressiveMcp
                        currentMcpInstructions
            mapM_
                (reportStartupWarning startup)
                runtimeMcpFleet.mcpFleetWarnings
            setStartupNotice startup.startupFullscreen "Loading built-in tools…"
            pure McpRuntime{..}

acquireLocalToolRuntime
    :: AgentToolsRequest windowTitleResult
    -> ToolModelRuntime
    -> ToolHostHooks
    -> CollaborationRuntime
    -> ScratchRuntime
    -> IO LocalToolRuntime
acquireLocalToolRuntime AgentToolsRequest
    { startup
    , baseToolEnv
    , initialSkills
    } ToolModelRuntime
    { toolDialect = dialect
    } ToolHostHooks
    { toolPlanHooks = planHooks
    , toolSecretHooks = secretHooks
    , toolImageHooks = imageHooks
    } CollaborationRuntime
    { collaborationContext = multiCtx
    , collaborationAgentTypes = agentTypesRef
    } ScratchRuntime
    { scratchTaskPlan = taskPlan
    } = do
    let preparedDiscovery =
            startup.startupNativeHooks
                >>= nativePreparedDiscovery . (.nativeWorkspaceDiscovery)
        codingToolEnv =
            case preparedDiscovery of
                Just context ->
                    baseToolEnv
                        { toolCwd = context.nativeDiscoveryProjectRoot }
                Nothing -> baseToolEnv
    bracketOnError
        (codingToolsForWithTypes
            dialect
            codingToolEnv
            (Just planHooks)
            (Just taskPlan)
            secretHooks
            imageHooks
            multiCtx
            agentTypesRef)
        (.codingClose)
        \localCoding -> do
            let localInitialSkills = initialSkills
            pure LocalToolRuntime{..}

acquireWebFetchRuntime
    :: AgentToolsRequest windowTitleResult
    -> ToolStartup
    -> ToolModelRuntime
    -> IO (Maybe WebFetchRuntime)
acquireWebFetchRuntime AgentToolsRequest
    { startup
    , baseToolEnv
    } ToolStartup
    { toolNativeCapabilities = nativeCapabilities
    , toolHarnessConfig = harnessConfig
    } ToolModelRuntime
    { toolDialectId = dialectId
    }
    | not nativeCapabilities.nativeHostExtensions
        || dialectId /= GrokBuildDialect =
        pure Nothing
    | otherwise =
        newWebFetchRuntime
            harnessConfig.configWebFetch
            baseToolEnv >>= \case
                Left err ->
                    startupDie startup
                        ("Failed to initialize web_fetch: " <> err)
                Right runtime -> pure runtime

acquireLspStartup
    :: AgentToolsRequest windowTitleResult
    -> ToolStartup
    -> ToolModelRuntime
    -> IO LspStartup
acquireLspStartup AgentToolsRequest
    { baseToolEnv
    } ToolStartup
    { toolNativeCapabilities = nativeCapabilities
    , toolHarnessConfig = harnessConfig
    } ToolModelRuntime
    { toolDialectId = dialectId
    }
    | not nativeCapabilities.nativeHostExtensions
        || dialectId /= GrokBuildDialect =
        pure LspStartup
            { lspStartupRuntime = Nothing
            , lspStartupWarnings = []
            }
    | otherwise =
        newLspRuntime harnessConfig.configLsp baseToolEnv

prepareInitialContextPreload
    :: AgentToolsRequest windowTitleResult
    -> ToolModelRuntime
    -> IO (SessionInitialContext, InitialContextPreload)
prepareInitialContextPreload AgentToolsRequest
    { options
    , startup
    , databaseScopes
    , home
    , cwd
    , resumed
    , transition
    } ToolModelRuntime
    { toolDialect = dialect
    , toolResumeTargetChanged = resumeTargetChanged
    , toolRefreshDialectContext = refreshDialectContext
    } = do
    (preloadedAgentsContext, preloadedLearnedSkills) <-
        concurrently preloadAgents preloadLearnedSkills
    pure (contextRequirements, InitialContextPreload{..})
  where
    contextRequirements =
        resolveSessionInitialContext
            (isJust transition)
            resumeTargetChanged
            resumed
    loadsHostWorkspaceContext =
        maybe
            True
            ( nativeLoadsHostWorkspaceContext
                . (.nativeWorkspaceDiscovery)
            )
            startup.startupNativeHooks
    preloadAgents
        | loadsHostWorkspaceContext
            && ( contextRequirements.initialContextNeeded
                || refreshDialectContext
               ) =
            if refreshDialectContext
                || not contextRequirements.initialContextMayRestoreSnapshot
                then preloadAgentsContext options dialect home cwd
                else pure Nothing
        | otherwise = pure Nothing
    preloadLearnedSkills
        | contextRequirements.initialContextNeeded =
            successfulLearnedSkillsPreload
                <$> loadApplicableLearnedSkillsForStore
                    startup.startupDatabaseStore
                    databaseScopes
        | otherwise = pure Nothing

installCollaborationCallbacks
    :: AgentToolsRequest windowTitleResult
    -> CollaborationRuntime
    -> IO ()
installCollaborationCallbacks AgentToolsRequest
    { startup
    } CollaborationRuntime
    { collaborationContext = multiCtx
    , collaborationPendingNotices = pendingNotices
    , collaborationSubagentSessions = subagentSessions
    , collaborationSubagentStoreRoot = subagentStoreRoot
    , collaborationAgentTypes = agentTypesRef
    } =
    case multiCtx of
        Just ctx -> do
            setSubagentOnComplete ctx.multiRegistry \agentId status -> do
                enqueuePendingNotice pendingNotices PendingSubagentNotice
                    (UserMessage (formatCompletionNotice agentId status))
                    >>= either (reportStartupWarning startup) pure
            setSubagentOnSettled ctx.multiRegistry \agentId status -> do
                sessions <- readIORef subagentSessions
                case Map.lookup agentId sessions of
                    Just session -> do
                        _ <-
                            persistAndEvictSubagentSessionWithStatus
                                subagentStoreRoot
                                ctx.multiRegistry
                                agentTypesRef
                                agentId
                                status
                                session
                        pure ()
                    Nothing -> pure ()
        Nothing -> pure ()

newSessionControlRuntime
    :: AgentToolsRequest windowTitleResult
    -> ToolStartup
    -> ToolModelRuntime
    -> CollaborationRuntime
    -> SkillCatalog
    -> IO SessionControlRuntime
newSessionControlRuntime AgentToolsRequest
    { options
    , startup
    , root
    , gatewayIdentity
    , cwd
    , runAgentChild
    , processRuntime
    } ToolStartup
    { toolNativeCapabilities = nativeCapabilities
    , toolGatewayAllowedChildModels = gatewayAllowedChildModels
    } ToolModelRuntime
    { toolProvider = provider
    , toolInferredTarget = inferredTarget
    , toolModel = model
    , toolDialectId = dialectId
    , toolEffortText = effortText
    , toolPolicy = policy
    } CollaborationRuntime
    { collaborationActiveSessionLock = activeSessionLock
    , collaborationPersistSlotRef = persistSlotRef
    , collaborationGatewayChildModelOption = gatewayChildModelOption
    } initialSkills = do
    controlGhciEnabledRef <- newIORef options.optGhci
    controlBashEnabledRef <- newIORef options.optBash
    controlSkillsRef <- newIORef initialSkills
    controlSkillInvocationsRef <- newIORef []
    controlCodeModeCloseRef <- newIORef (pure ())
    let controlClaimCurrentSession handle = do
            let desired = sessionLockPath handle.sessionDir
            readIORef activeSessionLock >>= \case
                Just current
                    | sessionLockFilePath current == desired -> pure ()
                previous ->
                    acquireSessionLock
                        handle.sessionDir
                        handle.sessionMeta.metaId >>= \case
                            Left err ->
                                throwIO (userError (Text.unpack err))
                            Right lock -> do
                                writeIORef activeSessionLock (Just lock)
                                mapM_ releaseSessionLock previous
        sessionToolsEnv = AgentSessionToolsEnv
            { toolsPool = trustedPool startup.startupDatabaseStore
            , toolsRoot = root
            , toolsProvider = provider
            , toolsConnection = inferredTarget.targetConnectionId
            , toolsModel = model
            , toolsTransportModel = inferredTarget.targetWireModelId
            , toolsDialect = dialectId
            , toolsAllowedModels = gatewayAllowedChildModels
            , toolsResolveModelOption = gatewayChildModelOption
            , toolsGatewayIdentity = gatewayIdentity
            , toolsCwd = cwd
            , toolsEffort = effortText
            , toolsCurrentSessionId =
                readIORef persistSlotRef >>= currentSessionId
            , toolsLaunchTurn = \handle message -> do
                ghciEnabled <- readIORef controlGhciEnabledRef
                bashEnabled <- readIORef controlBashEnabledRef
                let action =
                        runInProcessSessionTurn
                            runAgentChild
                            options
                            policy
                            ghciEnabled
                            bashEnabled
                            handle
                            message
                if isOneShot options
                    then
                        try @_ @SomeException action >>= \case
                            Left err -> pure (Left (formatException err))
                            Right (Left err) -> pure (Left err)
                            Right (Right ()) ->
                                pure
                                    (Right
                                        ("completed session "
                                            <> handle.sessionMeta.metaId))
                    else
                        launchSessionThread
                            processRuntime.processSessionThreads
                            handle.sessionMeta.metaId
                            action
            , toolsSessionStatus =
                sessionThreadStatus processRuntime.processSessionThreads
            }
        -- Persisted agent-session tools recursively start another native
        -- runtime, so they require an explicit collaboration capability from
        -- the embedding.
        controlSessionTools
            | not nativeCapabilities.nativeCollaboration = []
            | otherwise = agentSessionTools sessionToolsEnv
    pure SessionControlRuntime{..}

assembleSessionToolsRuntime
    :: AgentToolsRequest windowTitleResult
    -> ToolStartup
    -> ToolModelRuntime
    -> ToolHostHooks
    -> CollaborationRuntime
    -> ScratchRuntime
    -> McpRuntime
    -> CodingRuntime
    -> SessionControlRuntime
    -> IO SessionToolsRuntime
assembleSessionToolsRuntime AgentToolsRequest
    { startup
    , databaseScopes
    , gatewayIdentity
    , options
    , loaded
    , tokenProvider
    , baseToolEnv
    , resumed
    } ToolStartup
    { toolNativeCapabilities = nativeCapabilities
    } ToolModelRuntime
    { toolProvider = provider
    , toolDialectId = dialectId
    , toolInferredTarget = inferredTarget
    } ToolHostHooks
    { toolImageHooks = imageHooks
    } CollaborationRuntime
    { collaborationPersistSlotRef = persistSlotRef
    , collaborationSubagentStoreRoot = subagentStoreRoot
    , collaborationActiveSessionLock = activeSessionLock
    , collaborationCloseAgents = closeAgents
    } ScratchRuntime
    { scratchPromptRequest = promptRequest
    , scratchImageGenerationHistory = imageGenerationHistory
    , scratchExternalSessionTools = externalSessionAppTools
    , scratchCleanup = cleanupScratch
    } McpRuntime
    { runtimeMcpServerConfigs = mcpServerConfigs
    , runtimeProgressiveMcp = progressiveMcp
    , runtimeMcpFleet = mcpFleet
    , runtimeCloseMcp = closeMcp
    } CodingRuntime
    { runtimeCoding = coding
    , runtimeExtraTools = extraTools
    , runtimeCloseExtraTools = closeExtraTools
    } SessionControlRuntime
    { controlSkillInvocationsRef = skillInvocationsRef
    , controlCodeModeCloseRef = codeModeCloseRef
    , controlSessionTools = persistedSessionTools
    } = do
    let sessionMcpTools =
            if null mcpServerConfigs
                then []
                else
                    (if dialectId == GrokBuildDialect
                        then MCP.mcpFleetGrokMetaTools mcpFleet
                        else if progressiveMcp
                            then MCP.mcpFleetMetaTools mcpFleet
                            else MCP.mcpFleetTools mcpFleet)
                        <> MCP.mcpFleetResourceTools mcpFleet
        databaseToolsEnv =
            databaseToolsEnvForStore
                startup.startupDatabaseStore
                databaseScopes
                (readIORef persistSlotRef >>= currentSessionId)
                gatewayIdentity
        learnedSkillToolsEnv =
            learnedSkillToolsEnvForStore
                startup.startupDatabaseStore
                databaseScopes
                (readIORef persistSlotRef >>= reservedSessionId)
        sessionGatewayTools = maybe [] managedGatewayTools promptRequest
        sessionDatabaseTools = databaseTools databaseToolsEnv
        sessionLearnedSkillTools =
            learnedSkillTools skillInvocationsRef learnedSkillToolsEnv
        nativeToolGroups =
            maybe [] (.nativeToolGroups) startup.startupNativeHooks
        computerTools =
            [ ComputerUse.computerUseTool
            | provider == OpenAIProvider
            , os == "darwin"
            ]
        activeComputerTools =
            [ tool
            | resolveComputerUseEnabled options startup.startupStdinTty
            , tool <- computerTools
            ]
        imageGenerationTools =
            [ imageGenerationTool
                tokenProvider
                baseToolEnv
                imageGenerationHistory
                imageHooks
            | provider == OpenAIProvider
            , dialectId == CodexDialect
            , nativeCapabilities.nativeHostExtensions
            , not (isGatewayLoadedAuth loaded)
            , inferredTarget.targetConnectionId
                == builtinConnectionId OpenAIProvider
            ]
        surroundingToolGroupsFor selectedComputerTools =
            [ ExecutionToolGroup extraTools
            , ExecutionToolGroup sessionMcpTools
            , HostToolGroup persistedSessionTools
            , HostToolGroup sessionGatewayTools
            , HostToolGroup sessionDatabaseTools
            , HostToolGroup sessionLearnedSkillTools
            , HostToolGroup externalSessionAppTools
            ]
                <> nativeToolGroups
                <> [ HostToolGroup imageGenerationTools
                   , ExecutionToolGroup selectedComputerTools
                   ]
        allToolGroups =
            coding.codingAppToolGroups
                <> surroundingToolGroupsFor computerTools
        activeCodingGroups =
            map filterCodingExecution coding.codingAppToolGroups
        activeToolGroups =
            activeCodingGroups
                <> surroundingToolGroupsFor activeComputerTools
        filterCodingExecution = \case
            ExecutionToolGroup appTools ->
                ExecutionToolGroup $
                    filterGhciTools options.optGhci
                        (filterBashTools options.optBash appTools)
            hostGroup@(HostToolGroup _) -> hostGroup
        composeToolGroups groups =
            case startup.startupNativeHooks of
                Nothing -> appToolsFromGroups groups
                Just hooks -> hooks.nativeComposeTools groups
        sessionPlanMode = coding.codingPlanMode
        sessionResumedPlanPending =
            case resumed of
                Just (_, turns) ->
                    resumedPlanNeedsApproval
                        (map (.turnAssistantText) turns)
                Nothing -> False
        -- Keep planSessionDir and subagent store root in sync.
        sessionNoteDirectory dir = do
            writeIORef sessionPlanMode.planSessionDir (Just dir)
            writeIORef subagentStoreRoot (Just dir)
        sessionCloseAll =
            closeAgents
                `finally`
                    ((readIORef activeSessionLock
                        >>= mapM_ releaseSessionLock)
                        `finally`
                            (closeExtraTools
                                `finally`
                                    (closeMcp
                                        `finally`
                                            (coding.codingClose
                                                `finally`
                                                    (join
                                                        (readIORef
                                                            codeModeCloseRef)
                                                        `finally`
                                                            cleanupScratch)))))
        sessionAllTools = composeToolGroups allToolGroups
        sessionTools = composeToolGroups activeToolGroups
    pure SessionToolsRuntime{..}

launchAgentToolsSession
    :: AgentToolsRequest windowTitleResult
    -> ToolStartup
    -> ToolModelRuntime
    -> ToolHostHooks
    -> CollaborationRuntime
    -> ScratchRuntime
    -> McpRuntime
    -> CodingRuntime
    -> SessionInitialContext
    -> InitialContextPreload
    -> SessionControlRuntime
    -> SessionToolsRuntime
    -> IO RunResult
launchAgentToolsSession AgentToolsRequest{..} ToolStartup
    { toolOpenRouterOptions = openRouterOptions
    } ToolModelRuntime
    { toolProvider = provider
    , toolModel = model
    , toolTransportModel = transportModel
    , toolInferredTarget = inferredTarget
    , toolCustomGenericOptions = customGenericOptions
    , toolDialect = dialect
    , toolResumeTargetChanged = resumeTargetChanged
    , toolRefreshDialectContext = refreshDialectContext
    , toolLegacySubagentTarget = legacySubagentTarget
    , toolEffortText = effortText
    , toolPolicy = policy
    , toolClaudeBypassEnabled = claudeBypassEnabled
    } ToolHostHooks
    { toolPlanHooks = planHooks
    } CollaborationRuntime
    { collaborationSubagentSessions = subagentSessions
    , collaborationSubagentStoreRoot = subagentStoreRoot
    , collaborationSubagentForkSource = subagentForkSource
    , collaborationPendingNotices = pendingNotices
    , collaborationRegistry = registry
    , collaborationRootTurnRef = rootTurnRef
    , collaborationAgentTypes = agentTypesRef
    , collaborationOpenAiChild = openaiChild
    , collaborationAllowedChildModels = allowedChildModels
    , collaborationChildModelAllowed = childModelAllowed
    , collaborationResolveChildModel = resolveCollaborationChildModel
    , collaborationCreateWorktree = createSubagentWorktree
    , collaborationContext = multiCtx
    } ScratchRuntime
    { scratchPromptRequest = promptRequest
    , scratchPersistence = persist
    , scratchSessionTmp = sessionTmp
    , scratchImageGenerationHistory = imageGenerationHistory
    } McpRuntime
    { runtimeMcpFleet = mcpFleet
    , runtimeMcpInstructions = mcpInstructions
    } CodingRuntime
    { runtimeCoding = coding
    , runtimeExtraTools = extraTools
    } initialContext initialContextPreload SessionControlRuntime
    { controlGhciEnabledRef = ghciEnabledRef
    , controlBashEnabledRef = bashEnabledRef
    , controlSkillsRef = skillsRef
    , controlSkillInvocationsRef = skillInvocationsRef
    , controlCodeModeCloseRef = codeModeCloseRef
    , controlClaimCurrentSession = claimCurrentSession
    , controlSessionTools = agentSessionAppTools
    } SessionToolsRuntime
    { sessionAllTools = allTools
    , sessionTools = tools
    , sessionMcpTools = mcpTools
    , sessionDatabaseTools = databaseAppTools
    , sessionLearnedSkillTools = learnedSkillAppTools
    , sessionGatewayTools = gatewayTools
    , sessionPlanMode = planMode
    , sessionNoteDirectory = noteSessionDir
    , sessionCloseAll = closeAll
    , sessionResumedPlanPending = resumedPlanPending
    } = do
    when resumedPlanPending (activatePlanMode planMode)
    forM_ startup.startupNativeHooks \hooks ->
        when (hooks.nativeInteractionMode == NativePlan) $
            writeIORef planMode.planStateRef PlanPending
    runAgentSession
        loaded
        connectedGateway
        learnAboutUserRequested
        sessionTmp
        activeAccountIdRef
        activeAccountRef
        activeSelectionRef
        agentTypesRef
        allTools
        (recordImageGenerationImages imageGenerationHistory)
        (clearImageGenerationHistory imageGenerationHistory)
        bashEnabledRef
        catalog
        gatewayModelsRef
        checkStartupUsageInBackground
        claimCurrentSession
        claudeBypassEnabled
        closeAll
        codeModeCloseRef
        coding
        createSubagentWorktree
        customGenericOptions
        cwd
        databaseAppTools
        databaseScopes
        initialContext
        initialContextPreload
        dialect
        effortText
        escPaused
        extraTools
        fullscreen
        gatewayTools
        ghciEnabledRef
        allowedChildModels
        resolveCollaborationChildModel
        childModelAllowed
        home
        inferredTarget
        interrupt
        learnedSkillAppTools
        legacySubagentTarget
        mcpFleet
        mcpInstructions
        mcpTools
        model
        multiCtx
        noteSessionDir
        openRouterOptions
        openaiChild
        options
        pendingNotices
        pendingTurn
        persist
        planHooks
        planMode
        policy
        preferredOpenAiAccountRef
        projectRoot
        promptRequest
        provider
        refreshDialectContext
        registry
        resolveActiveAccountLabel
        resumeTargetChanged
        resumed
        root
        rootTurnRef
        selectHttpAccount
        selectableTokenProvider
        agentSessionAppTools
        setWindowTitle
        skillInvocationsRef
        skillsRef
        startup
        stateDirectory
        stderrHandle
        subagentForkSource
        subagentSessions
        subagentStoreRoot
        tokenProvider
        baseToolEnv
        tools
        transition
        transportModel
        unavailableProviders
