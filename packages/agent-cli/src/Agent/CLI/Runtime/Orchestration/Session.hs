module Agent.CLI.Runtime.Orchestration.Session
    ( AgentSessionRequest(..)
    , runAgentSession
    ) where

import Agent.Runtime.Session.Request
    ( SessionRequestState
    , newSessionRequestState
    , readSessionRequestParams
    )
import Agent.CLI.ActiveAccount
    ( ActiveAccount(..)
    , ActiveAccountRef
    , modifyActiveAccount
    , readActiveAccount
    , writeActiveAccount
    )
import Agent.CLI.CancelWatch (StdinControl)
import Agent.CLI.McpSampling (mcpSamplingHandler)
import Agent.Accounts.Auth
    ( LoadedAuth(loadedTokenProvider, loadedOpenAiPool)
    , isGatewayLoadedAuth
    , gatewayLoadedAuthForProvider
    , gatewayTokenProviderForProvider
    )
import Agent.CLI.Claude
    ( approveClaudeRegisteredTool
    , handleClaudePermissionRequest
    , ClaudeSessionRuntimeSlot
    , newClaudeSessionRuntimeSlot
    )
import Agent.CLI.CodeModeRuntime
    ( CodeModeRuntimePlan(..)
    , CodeModeSessionRuntime(..)
    , CodexCatalogSession(..)
    , codeModeRuntimeFor
    , codeModeRuntimePlan
    , filterStartupUnavailableTools
    , loadCodexCatalogModelInfo
    )
import Agent.Tools.CodeMode.Tool
    ( ToolMode(CodeOnlyToolMode, ConventionalToolMode)
    )
import Agent.Runtime.Compaction.Provider
    ( AutomaticCompactionBoundary
    , CompactOutcome
    , CompactionInstall(CompactionNotInstalled)
    , OccupancySnapshot
    )
import Agent.Runtime.Database.Store (DatabaseScopes)
import Agent.CLI.Dialects (CodingTools(..))
import Agent.Runtime.Error (formatApiErrorAt)
import Agent.Runtime.GatewayClient
    ( GatewayCredential(gatewayBaseUrl)
    , GatewayModelAccess
    , gatewayCredentialIdentity
    )
import Agent.CLI.Interrupt
    ( InterruptState
    , catchUserInterrupt
    , retryUserInterruptOnce
    , withCtrlCHandler
    )
import Agent.Runtime.ManagedTurn ( ManagedTurnRequest(..) )
import Agent.Runtime.ModelConfig
    ( ModelCatalog
    , catalogContextWindowForTransport
    , catalogSupportsAsyncToolCallsForTransport
    , organizationGatewayConnectionId
    )
import Agent.Runtime.Models (ModelTarget(targetConnectionId))
import Agent.CLI.Options
    ( ApprovalPolicy
    , CodeModeOption(..)
    , isOneShot
    , CliOptions(optCodeMode, optCompactThreshold, optShowRawReasoning)
    )
import Agent.CLI.PendingInputs (PendingInputs, withPendingInputs)
import Agent.CLI.Project ( ModelSwitchScope(..) )
import Agent.CLI.Prompt
    ( codexEnvironmentContext,
      subscriptionSubagentModelGuidance,
      appendMcpInstructions,
      systemPromptForCatalogModelWithHostedSearch,
      systemPromptForToolsWithHostedSearch )
import Agent.CLI.Provider.Switch (chooseStartupProviderTransition, prepareTransitionBackend)
import Agent.CLI.ProviderAvailability ( probeLoadedAvailability )
import Agent.CLI.ProviderFallback ( isProviderUnavailable )
import Agent.CLI.ProviderTransition
    ( PendingTurn, ProviderTransition(transitionCause), TransitionCause(AutomaticFallback) )
import Agent.CLI.Render ( putTextLn )
import Agent.Runtime.ProviderRequest
    ( requestParams
    , setRequestInstructionsAndTools
    , setRequestPromptCacheKey
    )
import Agent.CLI.Resume
    ( SessionInitialContext(..)
    )
import Agent.Runtime.Providers
    ( withProviderRuntime )
import Agent.Runtime.Providers.Types
    ( ProviderConfig(..), OpenAiConfig(..), OpenAiAccounts(..)
    , OpenRouterConfig(..), ClaudeConfig(..), ProviderHost(..)
    , ProviderCompaction(..), ProviderRuntime(..)
    , ProviderAccountSelection(..), ProviderSubagents(..)
    )
import Agent.CLI.Runtime.Orchestration.Startup
    ( clearNativeProgress, mcpToolCollision, reportStartupWarning, finishStartup )
import Agent.CLI.Runtime.Orchestration.Types
    ( NativeRunCapabilities(..)
    , NativeRunHooks(nativeCapabilities, nativeWorkspaceDiscovery)
    , fullNativeRunCapabilities
    , nativeLoadsHostWorkspaceContext
    )
import Agent.CLI.Runtime.Recap (runSessionRecap, runSessionTurnSummary)
import Agent.CLI.Runtime.Repl
    ( finishTurn, preparePromptSkillInputsWithPaste, repl, replWithDraft, runPendingTurn )
import qualified Agent.CLI.Session.Runner as SessionRunner
import Agent.CLI.Runtime.Types ( RunResult(RunQuit, RunProviderStartFailed, RunSwitchProvider) )
import Agent.Runtime.Session
    ( addSessionUsage,
      ensureSession,
      ensurePersistenceSessionId,
      compatibleSessionPromptSnapshot,
      resumeHint,
      sessionTitleFromPrompt,
      sessionUsageFromTurns,
      Persistence(..),
      PersistenceState(PersistenceActive, PersistencePending),
      LegacySubagentTarget,
      SessionHandle(sessionDir, sessionMeta),
      SessionMeta(metaId, metaPromptSnapshot, metaTitle),
      SessionTurn,
      SessionPromptSnapshot(..) )
import Agent.Runtime.Session.History
    ( LiveConversation
    , currentLiveTranscriptGeneration,
      durableTranscriptCheckpoint,
      evictLiveTranscript,
      readLiveTranscript,
      replaceLiveConversation )
import Agent.CLI.Session.Runtime.Types
    ( SessionBackend(..)
    , InitialContextPreload(..)
    , SessionRequest(codexCatalogSession, SessionRequest, catalog,
                     gatewayModelsRef, modelInfo,
                     connectionId, gatewayIdentity,
                     options, provider, dialect, commitAttributionModel,
                     commitAttributionEffort, policyRef, allTools, refreshTools,
                     claudeRuntimeSlot, claudeBridgeTools,
                     recordImageGenerationInputs, clearImageGenerationHistory,
                     suspendGhci, resetToolSessionTemp, grokRuntime,
                     mcpRegistrations, mcpWarnings,
                     mcpInstructions, mcpFleet,
                     ghciEnabledRef, bashEnabledRef, toolEnv, planMode, taskPlan,
                     startup,
                     learnAboutUserRequested, databaseScopes, promptRequest,
                     pendingTurn, unavailableProviders, startupUnavailable, paramsRef,
                     conversationRef, needsInitialContext, queueInitialContext,
                     initialContextPreload, initialGrokContext, persist,
                     contextOccupancyRef, currentContextWindow,
                     startupWindowTitle, automaticCompactionRef,
                     workspace, tokenProvider, openAiPool, startupContext,
                     automaticCompactionHookRef, skillsRef, skillInvocationsRef,
                     stdinControl, interrupt, multiCtx, rootTurnRef, subagentSessions,
                     pendingNotices, storeRoot, agentTypes, legacyTarget, usageRef,
                     accountRef, accountLabel,
                     selectAccount, onPersisted, compactRunner, codeModeRuntime),
      StartupRuntime(startupBackground, startupDatabaseStore,
                     startupNetworkRecovery, startupSessionState,
                     startupNativeHooks) )
import Agent.CLI.Session.Selection ( reservedSessionId )
import Agent.CLI.Session.Workspace (WorkspaceContext(..))
import Agent.CLI.SessionState ( SessionState(sessionConversation) )
import Agent.CLI.Startup.Auth ( markStartupStage, startupDie )
import Agent.CLI.StartupContext
    ( AgentsContextNotice(..), loadAgentsContextWithPreload )
import Agent.CLI.Style ( cliWindowTitle, roleMuted, glyphWarn, roleWarn )
import Agent.CLI.Subagents.Runtime
    ( runCodexSubagent, runHttpSubagent, runXaiParentSubagent
    , runGatewaySubagent, runXaiSubagent
    , SubagentRuntime(subagentOpenAiChild, SubagentRuntime,
                      subagentOptions, subagentNetworkRecovery,
                      subagentGhciEnabled, subagentBashEnabled,
                      subagentPolicy, subagentPlanHooks, subagentSkillRoots,
                      subagentAllowedRoots, subagentToolResourceArbiter, subagentRootAccessRequest,
                      subagentParams, subagentMcpTools, subagentRegistry,
                      subagentSessions, subagentStoreRoot, subagentTypes,
                      subagentLegacyTarget, subagentConnection, subagentMapModel,
                      subagentCreateWorktree, subagentSessionTmp,
                      subagentOutputMemoryStore, subagentOutputMemoryCap,
                      subagentSpawnModelGuidance, subagentAllowedChildModels,
                      subagentResolveChildModel, subagentChildModelAllowed) )
import Agent.CLI.Subagents.Runtime.Types
    (SubagentSession, SubagentStoreRoot)
import Agent.CLI.TUI.App
    ( FullscreenRuntime, withFullscreenSuspended, emitUiEvent )
import Agent.CLI.Terminal ( resolveColor )
import Agent.CLI.Tools
    ( schemasFromAppToolsCodeModeWithHostedSearchAndAsyncCapability
    , schemasFromAppToolsWithHostedSearchAndAsyncCapability
    )
import Agent.Tools.OutputArtifact (finalizeToolOutput)
import qualified Control.Exception.Safe as Safe
import qualified Data.Aeson as Aeson
import Agent.Claude
    ( ClaudeCodeAuth, loadClaudeCodeAuth, loadClaudeCodeGatewayAuth )
import Agent.Claude.Control
    ( ClaudeCodeHostHandlers(..), ClaudeCodeMcpRequest(..), defaultClaudeCodeHostHandlers )
import Agent.CLI.ClaudeGatewayProxy (withClaudeGatewayProxy)
import Agent.Dialect (Dialect, dialectId, dialectForId)
import Agent.Error (ApiError)
import Agent.GrokBuild.Dialect.Task (GrokSubagentSpecs)
import Agent.Loop
    ( ImageAttachment
    , LoopError(LoopUnexpected)
    , TokenUsage
    , TurnInput
    , addTokenUsage
    , emptyTokenUsage
    , defaultLoopDispatch
    )
import Agent.OpenAI.WebSocketClient (CodexAuthFailed(..))
import Agent.OpenAI.Models.Types ( ModelInfo, resolvedContextWindow )
import Agent.OpenRouter.Options (ClientOptions)
import Agent.Provider
    (Credential(..), Provider(..), TokenProvider,
     tokenProviderBillingMode)
import Agent.ResourceScope (ResourceScope, allocateAcquire, logSlowCleanup)
import Agent.Responses.GenericClient (GenericClientOptions)
import Agent.Responses.Types
    (ResponseItem, ResponseCreateParams(model))
import Agent.Skills (SkillCatalog, SkillInvocation)
import Agent.Store.Postgres ( trustedPool )
import Agent.Subagents (SubagentRegistry, setSubagentRunner)
import Agent.Subagents.Types (RootTurnId, SubagentId)
import Agent.TUI.Model (UiEvent(UiSystemMessage))
import Agent.Tools.MultiAgents
    (CollaborationModelTarget(..), MultiAgentContext(..), SubagentWorktree)
import Agent.XAI.LoopBackend (xaiBackendWithClientOptions)
import qualified Agent.XAI.Options as XAI
import Agent.Tools.PlanMode (PlanModeEnv, PlanModeHooks)
import Agent.ToolDispatch (canonicalToolName, ToolDispatchConfig(..))
import Agent.Tools.Types
    ( AppTool(..)
    , ToolSchema(..)
    , ToolEnv(toolAllowedRoots, toolRootAccessRequest, toolSkillRoots, toolSessionTmp, toolResourceArbiter, toolOutputMemoryStore, toolOutputMemoryCap)
    )
import Control.Applicative ( (<|>) )
import Control.Concurrent.Async ( waitSTM, withAsync )
import Control.Concurrent.STM ( STM, retry )
import Control.Exception.Safe ( mask_, finally )
import Control.Monad ( forM_, void, when )
import Data.Acquire (mkAcquire)
import Data.Functor ( (<&>) )
import Data.IORef
    ( IORef,
      modifyIORef',
      newIORef,
      readIORef,
      writeIORef )
import Data.Map.Strict (Map)
import Data.Maybe ( isJust, isNothing, fromMaybe )
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock ( getCurrentTime, utctDay )
import System.Environment ( getProgName )
import System.IO (Handle, stderr)
import System.Mem ( performMajorGC )
import System.OsPath (OsPath)
import qualified Agent.MCP as MCP
import qualified Data.Text.IO as Text ( hPutStr )

data AgentSessionRequest windowTitleResult = AgentSessionRequest
    { loaded :: LoadedAuth
    , connectedGateway :: Maybe GatewayCredential
    , learnAboutUserRequested :: Bool
    , sessionTmp :: OsPath
    , activeAccountRef :: ActiveAccountRef
    , agentTypesRef :: GrokSubagentSpecs
    , allTools :: [AppTool]
    , recordImageGenerationInputs :: [ImageAttachment] -> IO ()
    , clearImageGenerationHistory :: IO ()
    , bashEnabledRef :: IORef Bool
    , catalog :: ModelCatalog
    , gatewayModelsRef :: IORef (Maybe GatewayModelAccess)
    , checkStartupUsageInBackground :: Bool
    , claimCurrentSession :: SessionHandle -> IO ()
    , claudeBypassEnabled :: Bool
    , codeModeResourceScope :: ResourceScope
    , coding :: CodingTools
    , createSubagentWorktree
        :: OsPath -> IO (Either Text SubagentWorktree)
    , customGenericOptions :: Maybe GenericClientOptions
    , workspace :: !WorkspaceContext
    , databaseAppTools :: [AppTool]
    , databaseScopes :: DatabaseScopes
    , initialContext :: SessionInitialContext
    , initialContextPreload :: InitialContextPreload
    , dialect :: Dialect
    , effortText :: Text
    , stdinControl :: StdinControl
    , extraTools :: [AppTool]
    , fullscreen :: Maybe FullscreenRuntime
    , gatewayTools :: [AppTool]
    , ghciEnabledRef :: IORef Bool
    , allowedChildModels :: Maybe [Text]
    , resolveChildModel
        :: Maybe (Text -> IO (Maybe CollaborationModelTarget))
    , childModelAllowed :: Maybe (Text -> IO Bool)
    , inferredTarget :: ModelTarget
    , interrupt :: InterruptState
    , learnedSkillAppTools :: [AppTool]
    , legacySubagentTarget :: Maybe LegacySubagentTarget
    , mcpFleet :: MCP.McpFleet
    , mcpRegistrations :: [MCP.McpToolRegistration]
    , mcpInstructions :: [(Text, Text)]
    , mcpTools :: [AppTool]
    , mcpSamplingRef
        :: IORef (Maybe
            (MCP.McpSamplingRequest
                -> IO (Either Text MCP.McpSamplingResult)))
    , model :: Text
    , multiCtx :: Maybe MultiAgentContext
    , noteSessionDir :: OsPath -> IO ()
    , openRouterOptions :: ClientOptions
    , openaiChild :: Maybe TokenProvider
    , options :: CliOptions
    , pendingNotices :: PendingInputs
    , pendingTurn :: Maybe PendingTurn
    , persist :: Persistence
    , planHooks :: PlanModeHooks
    , planMode :: PlanModeEnv
    , policy :: ApprovalPolicy
    , preferredOpenAiAccountRef :: IORef (Maybe Text)
    , promptRequest :: Maybe ManagedTurnRequest
    , provider :: Provider
    , refreshDialectContext :: Bool
    , registry :: SubagentRegistry
    , resolveActiveAccountLabel :: Credential -> IO Text
    , resumeTargetChanged :: Bool
    , resumed :: Maybe (SessionMeta, [SessionTurn])
    , root :: OsPath
    , rootTurnRef :: IORef (Maybe RootTurnId)
    , selectHttpAccount :: Text -> IO (Either ApiError Text)
    , selectableTokenProvider :: TokenProvider
    , sessionTools :: [AppTool]
    , setWindowTitle :: Text -> IO windowTitleResult
    , skillInvocationsRef :: IORef [SkillInvocation]
    , skillsRef :: IORef SkillCatalog
    , startup :: StartupRuntime
    , stateDirectory :: FilePath
    , stderrHandle :: Handle
    , subagentForkSource :: IORef (Maybe (IO [ResponseItem]))
    , subagentSessions :: IORef (Map SubagentId SubagentSession)
    , subagentStoreRoot :: SubagentStoreRoot
    , tokenProvider :: TokenProvider
    , toolEnv :: ToolEnv
    , tools :: [AppTool]
    , transition :: Maybe ProviderTransition
    , transportModel :: Text -> Text
    , unavailableProviders :: Set Provider
    }

data SessionCodeRuntime = SessionCodeRuntime
    { sessionModelInfo :: Maybe ModelInfo
    , sessionLoadsHostWorkspaceContext :: Bool
    , sessionCodeModeRuntime :: Maybe CodeModeSessionRuntime
    , sessionCatalogSession :: Maybe CodexCatalogSession
    , sessionRegistryTools :: [AppTool]
    , sessionRefreshTools :: [AppTool]
    , sessionBaseParams :: ResponseCreateParams
    , sessionReservedId :: Maybe Text
    , sessionEnvironmentContext :: Maybe Text
    }

data SessionPromptRuntime = SessionPromptRuntime
    { sessionCodeRuntime :: SessionCodeRuntime
    , sessionParams :: ResponseCreateParams
    , sessionParamsRef :: SessionRequestState
    , sessionPolicyRef :: IORef ApprovalPolicy
    , sessionClaudeRuntimeSlot :: ClaudeSessionRuntimeSlot
    , sessionClaudeBridgeTools :: [AppTool]
    , sessionAutomaticCompactionRef
        :: IORef (Maybe AutomaticCompactionBoundary)
    , sessionAutomaticCompactionHookRef
        :: IORef
            (CompactOutcome -> [TurnInput] -> IO CompactionInstall)
    , sessionInitialItems :: [ResponseItem]
    , sessionResumeNeedsFreshContext :: Bool
    , sessionInitialPrevious :: Maybe Text
    , sessionNeedsInitialContext :: Bool
    , sessionRestoredPromptSnapshot :: Maybe SessionPromptSnapshot
    , sessionQueueInitialContext :: Bool
    }

data SessionLiveRuntime = SessionLiveRuntime
    { sessionConversationRef :: IORef LiveConversation
    , sessionContextTokensRef :: IORef (Maybe OccupancySnapshot)
    , sessionStartupContext :: IORef (Maybe Text)
    , sessionUsageRef :: IORef TokenUsage
    , sessionStartupWindowTitle :: Text
    , sessionRecordCompactionUsage :: TokenUsage -> IO ()
    , sessionSubagentRuntime :: SubagentRuntime
    }

runAgentSession
    :: AgentSessionRequest windowTitleResult
    -> IO RunResult
runAgentSession request = do
    validateSessionMcpTools request
    codeRuntime <- prepareSessionCodeRuntime request
    promptRuntime <- prepareSessionPromptRuntime request codeRuntime
    liveRuntime <- prepareSessionLiveRuntime request promptRuntime
    launchPreparedSession request promptRuntime liveRuntime

validateSessionMcpTools
    :: AgentSessionRequest windowTitleResult
    -> IO ()
validateSessionMcpTools AgentSessionRequest
    { coding
    , extraTools
    , sessionTools
    , gatewayTools
    , databaseAppTools
    , learnedSkillAppTools
    , mcpRegistrations
    , startup
    } =
    case
            mcpToolCollision
                ( coding.codingAppTools
                    ++ extraTools
                    ++ sessionTools
                    ++ gatewayTools
                    ++ databaseAppTools
                    ++ learnedSkillAppTools
                )
                mcpRegistrations
        of
            Just err ->
                startupDie startup
                    ("Failed to initialize MCP tools: " <> err)
            Nothing -> pure ()

prepareSessionCodeRuntime
    :: AgentSessionRequest windowTitleResult
    -> IO SessionCodeRuntime
prepareSessionCodeRuntime AgentSessionRequest
    { loaded
    , stateDirectory
    , provider
    , dialect
    , selectableTokenProvider
    , model
    , catalog
    , inferredTarget
    , startup
    , options
    , tools
    , codeModeResourceScope
    , allTools
    , effortText
    , workspace = WorkspaceContext{cwd}
    , sessionTmp
    , persist
    , mcpInstructions
    } = do
    today <- utctDay <$> getCurrentTime
    -- Catalog models provide the per-model instructions template. Codex
    -- catalog @tool_mode@ selects full code mode; local --code-mode is only
    -- the fallback when the catalog omits a recognized selector.
    sessionModelInfo <-
        loadCodexCatalogModelInfo
            stateDirectory
            provider
            dialect
            (if isGatewayLoadedAuth loaded
                then Nothing
                else Just selectableTokenProvider)
            model
    let nativeCapabilities =
            maybe
                fullNativeRunCapabilities
                (.nativeCapabilities)
                startup.startupNativeHooks
        sessionLoadsHostWorkspaceContext =
            maybe
                True
                (nativeLoadsHostWorkspaceContext
                    . (.nativeWorkspaceDiscovery))
                startup.startupNativeHooks
        includeHostedSearch =
            nativeCapabilities.nativeProviderHostedTools
        allowFullCodeMode = options.optCodeMode /= CodeModeDisabled
        codeModeFallback = case options.optCodeMode of
            CodeModeEnabled -> CodeOnlyToolMode
            _ -> ConventionalToolMode
        codeModePlan =
            codeModeRuntimePlan
                allowFullCodeMode
                codeModeFallback
                sessionModelInfo
        initializeCodeMode
            | not nativeCapabilities.nativeHostExtensions =
                pure (Right Nothing)
            | otherwise =
                codeModeRuntimeFor codeModePlan sessionModelInfo tools
        codeModeFallbackWarning =
            case codeModePlan of
                PlanImageGenerationCodeMode ->
                    "image generation code mode unavailable; \
                    \disabling image generation: "
                _ ->
                    "code mode unavailable; falling back to compatible \
                    \direct tools: "
    -- Attach cleanup during acquisition, before warnings or subsequent startup
    -- work can throw. The enclosing session owns this scope.
    (_, initializedCodeMode) <-
        allocateAcquire codeModeResourceScope $
            mkAcquire initializeCodeMode \case
                Left _ -> pure ()
                Right runtime -> logSlowCleanup "code mode runtime" $ mapM_ (.codeModeClose) runtime
    (sessionCodeModeRuntime, suppressDirectImageGeneration) <-
        case initializedCodeMode of
            Left err -> do
                reportStartupWarning startup
                    (codeModeFallbackWarning <> err)
                pure (Nothing, True)
            Right runtime -> pure (runtime, False)
    sessionReservedId <- reservedSessionId persist
    let providerTools =
            filterStartupUnavailableTools
                suppressDirectImageGeneration
                tools
        -- Keep toggle-disabled tools available for later refreshes, but never
        -- re-advertise a tool whose startup runtime failed to initialize.
        sessionRefreshTools =
            filterStartupUnavailableTools
                suppressDirectImageGeneration
                allTools
        sessionCatalogSession = sessionModelInfo <&> \info ->
            CodexCatalogSession
                { catalogInstructionsFor = \toolNames sessionTmpDir ->
                    systemPromptForCatalogModelWithHostedSearch
                        includeHostedSearch
                        dialect
                        model
                        effortText
                        info
                        toolNames
                        sessionTmpDir
                , catalogEnvironmentContext =
                    codexEnvironmentContext cwd today Nothing Nothing
                }
        instructions =
            appendMcpInstructions mcpInstructions case sessionCatalogSession of
                Just catalogSession ->
                    catalogSession.catalogInstructionsFor
                        (map (.appToolName) providerTools)
                        (Just sessionTmp)
                Nothing ->
                    systemPromptForToolsWithHostedSearch
                        includeHostedSearch
                        dialect
                        model
                        effortText
                        (map (.appToolName) providerTools)
                        cwd
                        (Just sessionTmp)
                        today
                        (isOneShot options)
        modelSupportsAsync =
            catalogSupportsAsyncToolCallsForTransport
                catalog
                inferredTarget.targetConnectionId
                model
        wireSchemas = case sessionCodeModeRuntime of
            Just codeMode ->
                schemasFromAppToolsCodeModeWithHostedSearchAndAsyncCapability
                    includeHostedSearch
                    modelSupportsAsync
                    dialect
                    ( codeMode.codeModeWireTools
                        <> codeMode.codeModeDirectTools
                    )
            Nothing ->
                schemasFromAppToolsWithHostedSearchAndAsyncCapability
                    includeHostedSearch
                    modelSupportsAsync
                    dialect
                    providerTools
        sessionEnvironmentContext =
            (.catalogEnvironmentContext) <$> sessionCatalogSession
        sessionRegistryTools =
            allTools
                <> maybe [] (.codeModeWireTools) sessionCodeModeRuntime
        sessionBaseParams =
            requestParams provider model instructions wireSchemas effortText
    pure SessionCodeRuntime{..}

prepareSessionPromptRuntime
    :: AgentSessionRequest windowTitleResult
    -> SessionCodeRuntime
    -> IO SessionPromptRuntime
prepareSessionPromptRuntime AgentSessionRequest
    { provider
    , inferredTarget
    , dialect
    , workspace = WorkspaceContext{cwd}
    , resumed
    , initialContext
    , policy
    , allTools
    , persist
    , startup
    } sessionCodeRuntime = do
    let compatiblePromptSnapshot =
            compatibleSessionPromptSnapshot
                provider
                inferredTarget.targetConnectionId
                (dialectId dialect)
                cwd
                sessionCodeRuntime.sessionReservedId
                sessionCodeRuntime.sessionBaseParams
                (resumed >>= \(meta, _) -> meta.metaPromptSnapshot)
        sessionParams = case compatiblePromptSnapshot of
            Just snapshot ->
                setRequestPromptCacheKey
                    snapshot.promptSnapshotCacheKey
                    (setRequestInstructionsAndTools
                        snapshot.promptSnapshotInstructions
                        (Just snapshot.promptSnapshotTools)
                        sessionCodeRuntime.sessionBaseParams)
            Nothing ->
                maybe sessionCodeRuntime.sessionBaseParams
                    (`setRequestPromptCacheKey`
                        sessionCodeRuntime.sessionBaseParams)
                    sessionCodeRuntime.sessionReservedId
        sessionInitialItems = initialContext.initialContextItems
        initialTurns = maybe [] snd resumed
        sessionResumeNeedsFreshContext =
            initialContext.initialContextResumeNeedsFresh
        sessionInitialPrevious = initialContext.initialContextPrevious
        sessionNeedsInitialContext =
            initialContext.initialContextNeeded
        sessionRestoredPromptSnapshot
            | null initialTurns && isNothing sessionInitialPrevious =
                compatiblePromptSnapshot
            | otherwise = Nothing
        sessionQueueInitialContext =
            sessionNeedsInitialContext
                && isNothing sessionRestoredPromptSnapshot
        sessionClaudeBridgeTools =
            filter isClaudeBridgeTool allTools
    sessionParamsRef <-
        newSessionRequestState persist sessionParams
            >>= either (startupDie startup) pure
    sessionPolicyRef <- newIORef policy
    sessionClaudeRuntimeSlot <- newClaudeSessionRuntimeSlot
    sessionAutomaticCompactionRef <- newIORef Nothing
    sessionAutomaticCompactionHookRef <-
        newIORef (\_outcome _inputs -> pure CompactionNotInstalled)
    pure SessionPromptRuntime{..}

isClaudeBridgeTool :: AppTool -> Bool
isClaudeBridgeTool tool =
    canonicalToolName tool.appToolName
        `notElem`
            [ "shell_command", "run_terminal_cmd"
            , "read_file", "write_file", "grep", "glob"
            , "search_replace", "apply_patch", "write_stdin"
            , "web_fetch", "web_search"
            ]
        && case tool.appToolSchema of
            JsonFunctionSchema{} -> True
            RawJsonFunctionSchema{} -> True
            _ -> False

sessionCatalogContextWindowForParams
    :: AgentSessionRequest windowTitleResult
    -> (Text -> Text)
    -> ResponseCreateParams
    -> Maybe Int
sessionCatalogContextWindowForParams AgentSessionRequest
    { catalog
    , inferredTarget
    } mapTransportModel params = do
    currentModel <- params.model
    catalogContextWindowForTransport
        catalog
        inferredTarget.targetConnectionId
        currentModel
        (mapTransportModel currentModel)

sessionCurrentModelContextWindow
    :: AgentSessionRequest windowTitleResult
    -> SessionPromptRuntime
    -> (Text -> Text)
    -> IO (Maybe Int)
sessionCurrentModelContextWindow request promptRuntime mapTransportModel = do
    currentParams <- readSessionRequestParams promptRuntime.sessionParamsRef
    pure $
        sessionCatalogContextWindowForParams
            request
            mapTransportModel
            currentParams

sessionContextWindowForParams
    :: AgentSessionRequest windowTitleResult
    -> (Text -> Text)
    -> Int
    -> ResponseCreateParams
    -> Int
sessionContextWindowForParams request mapTransportModel fallback params =
    fromMaybe fallback
        (sessionCatalogContextWindowForParams
            request
            mapTransportModel
            params)

buildSessionSubagentRuntime
    :: AgentSessionRequest windowTitleResult
    -> SessionPromptRuntime
    -> SubagentRuntime
buildSessionSubagentRuntime AgentSessionRequest
    { options
    , startup
    , ghciEnabledRef
    , bashEnabledRef
    , planHooks
    , toolEnv
    , mcpTools
    , registry
    , subagentSessions
    , subagentStoreRoot
    , agentTypesRef
    , legacySubagentTarget
    , inferredTarget
    , transportModel
    , createSubagentWorktree
    , provider
    , allowedChildModels
    , tokenProvider
    , resolveChildModel
    , childModelAllowed
    , openaiChild
    } promptRuntime =
    SubagentRuntime
        { subagentOptions = options
        , subagentNetworkRecovery = startup.startupNetworkRecovery
        , subagentGhciEnabled = ghciEnabledRef
        , subagentBashEnabled = bashEnabledRef
        , subagentPolicy = promptRuntime.sessionPolicyRef
        , subagentPlanHooks = planHooks
        , subagentSkillRoots = toolEnv.toolSkillRoots
        , subagentAllowedRoots = toolEnv.toolAllowedRoots
        , subagentToolResourceArbiter = toolEnv.toolResourceArbiter
        , subagentRootAccessRequest = toolEnv.toolRootAccessRequest
        , subagentParams = promptRuntime.sessionParamsRef
        , subagentMcpTools = mcpTools
        , subagentRegistry = registry
        , subagentSessions = subagentSessions
        , subagentStoreRoot = subagentStoreRoot
        , subagentTypes = agentTypesRef
        , subagentLegacyTarget = legacySubagentTarget
        , subagentConnection = inferredTarget.targetConnectionId
        , subagentMapModel = transportModel
        , subagentCreateWorktree = Just createSubagentWorktree
        , subagentSessionTmp = toolEnv.toolSessionTmp
        , subagentOutputMemoryStore = toolEnv.toolOutputMemoryStore
        , subagentOutputMemoryCap = toolEnv.toolOutputMemoryCap
        , subagentSpawnModelGuidance =
            if inferredTarget.targetConnectionId == organizationGatewayConnectionId
                || (provider == OpenAIProvider && isJust allowedChildModels)
                then Nothing
                else
                    subscriptionSubagentModelGuidance
                        provider
                        (tokenProviderBillingMode tokenProvider)
        , subagentAllowedChildModels = allowedChildModels
        , subagentResolveChildModel = resolveChildModel
        , subagentChildModelAllowed = childModelAllowed
        , subagentOpenAiChild = openaiChild
        }

prepareSessionLiveRuntime
    :: AgentSessionRequest windowTitleResult
    -> SessionPromptRuntime
    -> IO SessionLiveRuntime
prepareSessionLiveRuntime request@AgentSessionRequest
    { startup
    , subagentForkSource
    , setWindowTitle
    , resumed
    } promptRuntime = do
    let sessionConversationRef =
            startup.startupSessionState.sessionConversation
    void $
        replaceLiveConversation
            sessionConversationRef
            promptRuntime.sessionInitialPrevious
            promptRuntime.sessionInitialItems
    sessionContextTokensRef <- newIORef Nothing
    writeIORef subagentForkSource
        (Just (readLiveTranscript sessionConversationRef))
    let sessionStartupWindowTitle = sessionWindowTitle request
    _ <- setWindowTitle sessionStartupWindowTitle
    markStartupStage startup "Loading instructions…"
    sessionStartupContext <-
        loadSessionStartupContext request promptRuntime
    -- Fullscreen sessions load skills after Brick has taken over the
    -- terminal. Minimal and one-shot sessions initialize them synchronously
    -- before their first prompt or turn.
    sessionUsageRef <- newIORef $ case resumed of
        Just (meta, turns) -> sessionUsageFromTurns meta turns
        Nothing -> emptyTokenUsage
    evictResumedConversation
        request
        sessionConversationRef
    claimPersistedSession request
    let sessionRecordCompactionUsage =
            recordSessionCompactionUsage request sessionUsageRef
        sessionSubagentRuntime =
            buildSessionSubagentRuntime request promptRuntime
    pure SessionLiveRuntime{..}

sessionWindowTitle
    :: AgentSessionRequest windowTitleResult
    -> Text
sessionWindowTitle AgentSessionRequest
    { resumed
    , promptRequest
    , workspace = WorkspaceContext{cwd}
    } =
    cliWindowTitle cwd titleHint
  where
    titleHint = case resumed of
        Just (meta, _) -> Just meta.metaTitle
        Nothing ->
            fmap
                (\request ->
                    sessionTitleFromPrompt request.managedTurnText)
                promptRequest

loadSessionStartupContext
    :: AgentSessionRequest windowTitleResult
    -> SessionPromptRuntime
    -> IO (IORef (Maybe Text))
loadSessionStartupContext AgentSessionRequest
    { resumed
    , transition
    , stderrHandle
    , fullscreen
    , options
    , dialect
    , workspace = WorkspaceContext{home, cwd}
    , initialContextPreload
    , refreshDialectContext
    } promptRuntime =
    case promptRuntime.sessionRestoredPromptSnapshot of
        Just snapshot ->
            newIORef snapshot.promptSnapshotGeneratedContext
        Nothing
            | not
                promptRuntime.sessionCodeRuntime.sessionLoadsHostWorkspaceContext ->
                newIORef
                    promptRuntime.sessionCodeRuntime.sessionEnvironmentContext
            | otherwise ->
                loadAgentsContextWithPreload
                    stderrHandle
                    fullscreen
                    agentsContextNotice
                    options
                    dialect
                    home
                    cwd
                    initialItems
                    initialPrevious
                    promptRuntime.sessionCodeRuntime.sessionEnvironmentContext
                    initialContextPreload.preloadedAgentsContext
  where
    agentsContextNotice
        | isNothing resumed && isNothing transition =
            ReportAgentsContextLoaded
        | otherwise =
            SuppressAgentsContextLoaded
    refreshContext =
        refreshDialectContext
            || promptRuntime.sessionResumeNeedsFreshContext
    initialItems
        | refreshContext = []
        | otherwise = promptRuntime.sessionInitialItems
    initialPrevious
        | refreshContext = Nothing
        | otherwise = promptRuntime.sessionInitialPrevious

evictResumedConversation
    :: AgentSessionRequest windowTitleResult
    -> IORef LiveConversation
    -> IO ()
evictResumedConversation AgentSessionRequest
    { resumed
    , startup
    , root
    } conversationRef =
    forM_ resumed \(meta, _) -> do
        generation <-
            currentLiveTranscriptGeneration conversationRef
        evicted <-
            evictLiveTranscript
                conversationRef
                generation
                (durableTranscriptCheckpoint
                    (trustedPool startup.startupDatabaseStore)
                    root
                    meta.metaId)
        when evicted performMajorGC

recordSessionCompactionUsage
    :: AgentSessionRequest windowTitleResult
    -> IORef TokenUsage
    -> TokenUsage
    -> IO ()
recordSessionCompactionUsage AgentSessionRequest
    { persist
    , claimCurrentSession
    } usageRef usage =
    when (usage /= emptyTokenUsage) $
        mask_ do
            case persist of
                PersistenceDisabled -> pure ()
                PersistenceEnabled slotRef -> do
                    handle <- ensureSession slotRef
                    claimCurrentSession handle
                    updated <- addSessionUsage usage handle
                    writeIORef slotRef (PersistenceActive updated)
            modifyIORef' usageRef (`addTokenUsage` usage)

claimPersistedSession
    :: AgentSessionRequest windowTitleResult
    -> IO ()
claimPersistedSession AgentSessionRequest
    { persist
    , claimCurrentSession
    , noteSessionDir
    } =
    case persist of
        PersistenceEnabled slotRef ->
            readIORef slotRef >>= \case
                PersistenceActive handle -> do
                    claimCurrentSession handle
                    noteSessionDir handle.sessionDir
                PersistencePending _ _ _ -> pure ()
        PersistenceDisabled -> pure ()

buildProviderSessionRequest
    :: AgentSessionRequest windowTitleResult
    -> SessionPromptRuntime
    -> SessionLiveRuntime
    -> Maybe (STM ApiError)
    -> ProviderRuntime
    -> SessionRequest
buildProviderSessionRequest
    request
    promptRuntime
    liveRuntime
    startupUnavailable
    runtime =
        let (sessionTokenProvider, sessionOpenAiPool, sessionSelectAccount) =
                case runtime.accountSelection of
                    NoAccountSelection -> (Nothing, Nothing, Nothing)
                    OpenAiAccountSelection pool select ->
                        (Just request.tokenProvider, pool, select)
                    HttpAccountSelection ->
                        ( Just request.tokenProvider
                        , request.loaded.loadedOpenAiPool
                        , if isGatewayLoadedAuth request.loaded
                                || (request.provider == XAIProvider
                                    && isJust request.customGenericOptions)
                            then Nothing
                            else Just request.selectHttpAccount
                        )
        in
        SessionRequest
            { catalog = request.catalog
            , gatewayModelsRef = request.gatewayModelsRef
            , claudeRuntimeSlot =
                promptRuntime.sessionClaudeRuntimeSlot
            , claudeBridgeTools =
                promptRuntime.sessionClaudeBridgeTools
            , modelInfo =
                promptRuntime.sessionCodeRuntime.sessionModelInfo
            , connectionId =
                request.inferredTarget.targetConnectionId
            , gatewayIdentity =
                gatewayCredentialIdentity <$> request.connectedGateway
            , options = request.options
            , provider = request.provider
            , dialect = request.dialect
            , commitAttributionModel = request.model
            , commitAttributionEffort = request.effortText
            , policyRef = promptRuntime.sessionPolicyRef
            , allTools =
                promptRuntime.sessionCodeRuntime.sessionRegistryTools
            , refreshTools =
                promptRuntime.sessionCodeRuntime.sessionRefreshTools
            , recordImageGenerationInputs =
                request.recordImageGenerationInputs
            , clearImageGenerationHistory =
                request.clearImageGenerationHistory
            , suspendGhci = request.coding.codingSuspendGhci
            , resetToolSessionTemp =
                request.coding.codingResetSessionTemp
            , grokRuntime = request.coding.codingGrokRuntime
            , mcpRegistrations =
                request.mcpRegistrations
            , mcpWarnings = request.mcpFleet.mcpFleetWarnings
            , mcpInstructions = request.mcpInstructions
            , mcpFleet = Just request.mcpFleet
            , ghciEnabledRef = request.ghciEnabledRef
            , bashEnabledRef = request.bashEnabledRef
            , toolEnv = request.toolEnv
            , planMode = request.planMode
            , taskPlan = request.coding.codingTaskPlan
            , startup = request.startup
            , learnAboutUserRequested =
                request.learnAboutUserRequested
            , databaseScopes = request.databaseScopes
            , promptRequest = request.promptRequest
            , pendingTurn = request.pendingTurn
            , unavailableProviders = request.unavailableProviders
            , startupUnavailable
            , paramsRef = promptRuntime.sessionParamsRef
            , conversationRef = liveRuntime.sessionConversationRef
            , contextOccupancyRef =
                liveRuntime.sessionContextTokensRef
            , currentContextWindow = do
                configured <- runtime.currentContextWindow
                pure $
                    configured
                        <|> (resolvedContextWindow
                            =<< promptRuntime.sessionCodeRuntime.sessionModelInfo)
            , automaticCompactionRef =
                promptRuntime.sessionAutomaticCompactionRef
            , needsInitialContext =
                promptRuntime.sessionNeedsInitialContext
            , queueInitialContext =
                promptRuntime.sessionQueueInitialContext
            , initialContextPreload = request.initialContextPreload
            , initialGrokContext =
                promptRuntime.sessionRestoredPromptSnapshot
                    >>= (.promptSnapshotGrokContext)
            , persist = request.persist
            , startupWindowTitle =
                liveRuntime.sessionStartupWindowTitle
            , workspace = request.workspace
            , tokenProvider = sessionTokenProvider
            , openAiPool = sessionOpenAiPool
            , startupContext = liveRuntime.sessionStartupContext
            , automaticCompactionHookRef =
                promptRuntime.sessionAutomaticCompactionHookRef
            , skillsRef = request.skillsRef
            , skillInvocationsRef = request.skillInvocationsRef
            , stdinControl = request.stdinControl
            , interrupt = request.interrupt
            , multiCtx = request.multiCtx
            , rootTurnRef = request.rootTurnRef
            , subagentSessions = request.subagentSessions
            , pendingNotices = request.pendingNotices
            , storeRoot = request.subagentStoreRoot
            , agentTypes = request.agentTypesRef
            , legacyTarget = request.legacySubagentTarget
            , usageRef = liveRuntime.sessionUsageRef
            , accountRef = request.activeAccountRef
            , accountLabel = request.resolveActiveAccountLabel
            , selectAccount = sessionSelectAccount
            , onPersisted = request.claimCurrentSession
            , compactRunner = runtime.compactRunner
            , codeModeRuntime =
                promptRuntime.sessionCodeRuntime.sessionCodeModeRuntime
            , codexCatalogSession =
                promptRuntime.sessionCodeRuntime.sessionCatalogSession
            }

withSessionStartupAvailability
    :: AgentSessionRequest windowTitleResult
    -> Bool
    -> (Maybe (STM ApiError) -> IO result)
    -> IO result
withSessionStartupAvailability AgentSessionRequest
    { loaded
    , tokenProvider
    } shouldProbeAtStartup action
        | shouldProbeAtStartup
        , not (isGatewayLoadedAuth loaded) =
            withAsync
                (probeLoadedAvailability
                    loaded{loadedTokenProvider = tokenProvider})
                \availability -> do
                    let startupUnavailable =
                            waitSTM availability >>= \case
                                Left err
                                    | isProviderUnavailable err ->
                                        pure err
                                _ -> retry
                    action (Just startupUnavailable)
        | otherwise = action Nothing

runSessionWithInterruptHandling
    :: AgentSessionRequest windowTitleResult
    -> String
    -> IO RunResult
    -> IO RunResult
runSessionWithInterruptHandling AgentSessionRequest
    { startup
    , interrupt
    , fullscreen
    , persist
    } progName action
        | startup.startupBackground = action
        | otherwise =
            withCtrlCHandler interrupt $
                withResumeHintOnQuit fullscreen progName persist action

launchPreparedSession
    :: AgentSessionRequest windowTitleResult
    -> SessionPromptRuntime
    -> SessionLiveRuntime
    -> IO RunResult
launchPreparedSession request promptRuntime liveRuntime = do
    progName <- getProgName
    markStartupStage request.startup "Connecting to provider…"
    let shouldProbeAtStartup =
            request.checkStartupUsageInBackground
                && isNothing request.promptRequest
    runSessionWithInterruptHandling request progName $
        withSessionStartupAvailability request shouldProbeAtStartup
            \startupUnavailable ->
                launchProvider request promptRuntime liveRuntime
                    shouldProbeAtStartup startupUnavailable

sessionRunnerContinuation :: SessionRunner.SessionRunnerContinuation
sessionRunnerContinuation =
    SessionRunner.SessionRunnerContinuation
        { runnerRepl = repl
        , runnerReplWithDraft = replWithDraft
        , runnerRunPendingTurn = runPendingTurn
        , runnerFinishTurn = finishTurn
        , runnerFinishStartup = finishStartup
        , runnerPreparePromptSkillInputs = preparePromptSkillInputsWithPaste
        , runnerRunSessionRecap = runSessionRecap
        , runnerRunSessionTurnSummary = runSessionTurnSummary
        }

runSession
    :: SessionRequest
    -> SessionBackend
    -> IO RunResult
runSession = SessionRunner.runSession sessionRunnerContinuation

-- | The session owns composition and presentation. Provider runtimes only
-- supply transport capabilities, scoped around this continuation.
launchProvider
    :: AgentSessionRequest windowTitleResult
    -> SessionPromptRuntime
    -> SessionLiveRuntime
    -> Bool
    -> Maybe (STM ApiError)
    -> IO RunResult
launchProvider request promptRuntime liveRuntime shouldProbeAtStartup startupUnavailable = do
    let nativeCapabilities = maybe fullNativeRunCapabilities (.nativeCapabilities)
            request.startup.startupNativeHooks
        host = ProviderHost
            { networkRecovery = request.startup.startupNetworkRecovery
            , compaction = ProviderCompaction
                { paramsRef = promptRuntime.sessionParamsRef
                , contextTokensRef = liveRuntime.sessionContextTokensRef
                , contextWindowForParams = sessionContextWindowForParams request
                , currentModelContextWindow = sessionCurrentModelContextWindow request promptRuntime
                , conversationRef = liveRuntime.sessionConversationRef
                , installAutomaticCompact = \outcome inputs ->
                    readIORef promptRuntime.sessionAutomaticCompactionHookRef
                        >>= \hook -> hook outcome inputs
                , taskPlan = request.coding.codingTaskPlan
                , recordCompactionUsage = liveRuntime.sessionRecordCompactionUsage
                , compactThreshold = request.options.optCompactThreshold
                }
            }
        use runtime = do
            installProviderSubagents request liveRuntime runtime.subagents
            let sessionBackend = runtime.sessionBackend
                noticingBackend = case request.provider of
                    ClaudeCodeProvider -> sessionBackend.backend
                    _ -> withPendingInputs request.pendingNotices sessionBackend.backend
            previousSampling <- readIORef request.mcpSamplingRef
            let installSampling = case previousSampling of
                    Nothing -> pure ()
                    Just _ ->
                        writeIORef request.mcpSamplingRef $
                            Just (mcpSamplingHandler request.model sessionBackend.btwBackend)
                restoreSampling =
                    writeIORef request.mcpSamplingRef previousSampling
            installSampling
            (do
                activeBackend <- prepareTransitionBackend
                    (if request.startup.startupBackground
                        then SessionLocalSwitch else TopLevelSwitch)
                    request.workspace.home
                    request.workspace.projectRoot
                    request.transition
                    request.persist
                    noticingBackend
                runSession
                    (buildProviderSessionRequest request promptRuntime liveRuntime
                        startupUnavailable runtime)
                    sessionBackend{backend = activeBackend})
                `finally` restoreSampling
    config <- prepareProviderConfig request promptRuntime nativeCapabilities
    case request.provider of
        OpenAIProvider ->
            Safe.try @_ @CodexAuthFailed (withProviderRuntime config host use)
                >>= handleOpenAiStartupResult request nativeCapabilities shouldProbeAtStartup
        _ -> withProviderRuntime config host use

prepareProviderConfig
    :: AgentSessionRequest windowTitleResult
    -> SessionPromptRuntime
    -> NativeRunCapabilities
    -> IO ProviderConfig
prepareProviderConfig request promptRuntime nativeCapabilities = case request.provider of
    OpenAIProvider -> pure $ OpenAiProviderConfig OpenAiConfig
        { tokenProvider = request.tokenProvider
        , showRawReasoning = request.options.optShowRawReasoning
        , transportModel = request.transportModel
        , accounts = OpenAiAccounts
            { selectablePool = if isGatewayLoadedAuth request.loaded
                then Nothing else request.loaded.loadedOpenAiPool
            , readActiveAccountId =
                (.activeAccountId) <$> readActiveAccount request.activeAccountRef
            , resolveAccountLabel = request.resolveActiveAccountLabel
            , installAccount = \credential label ->
                writeActiveAccount request.activeAccountRef ActiveAccount
                    { activeAccountId = credential.accountId
                    , activeSelectionId = credential.accountId
                    , activeAccountLabel = label
                    }
            , preferAccount = writeIORef request.preferredOpenAiAccountRef . Just
            }
        }
    XAIProvider -> pure $ XaiProviderConfig request.tokenProvider
        nativeCapabilities.nativeProviderHostedTools
        request.connectedGateway
    GeminiProvider -> pure $ GeminiProviderConfig request.tokenProvider
    OpenRouterProvider -> pure $ OpenRouterProviderConfig OpenRouterConfig
        { tokenProvider = request.tokenProvider
        , clientOptions = request.openRouterOptions
        , genericOptions = request.customGenericOptions
        , model = request.model
        , transportModel = request.transportModel
        }
    ClaudeCodeProvider -> do
        when (not nativeCapabilities.nativeProviderNativeTools) $
            startupDie request.startup "Claude Code is unavailable in this runtime"
        mcpServer <- case MCP.createInProcessMcpServer "haskell-agent" "0.1.0"
                (defaultLoopDispatch
                    { toolDispatchFinalizeOutput = \call output ->
                        finalizeToolOutput request.toolEnv call output
                    })
                (approveClaudeRegisteredTool promptRuntime.sessionClaudeRuntimeSlot)
                promptRuntime.sessionClaudeBridgeTools of
            Left err -> startupDie request.startup err
            Right server -> pure server
        pure $ ClaudeProviderConfig ClaudeConfig
            { withAuth = withSelectedClaudeAuth request.connectedGateway request.loaded
                (startupDie request.startup)
            , cwd = request.workspace.cwd
            , initialPrevious = promptRuntime.sessionInitialPrevious
            , transportModel = request.transportModel
            , hostHandlers = defaultClaudeCodeHostHandlers
                { canUseTool = Just $ handleClaudePermissionRequest
                    promptRuntime.sessionClaudeRuntimeSlot
                , handleMcpMessage = Just \mcpRequest ->
                    if mcpRequest.serverName /= "haskell-agent"
                        then pure Aeson.Null
                        else MCP.handleInProcessMcpMessage mcpServer mcpRequest.message
                            >>= pure . fromMaybe (Aeson.object [])
                , mcpToolNames = MCP.inProcessMcpToolNames mcpServer
                , nativeToolsEnabled = nativeCapabilities.nativeProviderNativeTools
                }
            , onConnected = \label -> do
                when request.claudeBypassEnabled $ do
                    let notice = "Claude Code --yolo is active; host catastrophic-command and Plan Mode denies remain enforced."
                    case request.fullscreen of
                        Just runtime -> emitUiEvent runtime (UiSystemMessage notice)
                        Nothing -> do
                            color <- resolveColor request.stderrHandle
                            putTextLn request.stderrHandle $
                                roleWarn color $ glyphWarn <> notice
                modifyActiveAccount request.activeAccountRef \current ->
                    current { activeAccountLabel = label }
            }

installProviderSubagents
    :: AgentSessionRequest windowTitleResult
    -> SessionLiveRuntime
    -> ProviderSubagents
    -> IO ()
installProviderSubagents request liveRuntime capabilities =
    case request.multiCtx of
        Nothing -> pure ()
        Just ctx -> do
            let runtime = liveRuntime.sessionSubagentRuntime
                install = setSubagentRunner ctx.multiRegistry
            case capabilities of
                NoProviderSubagents -> pure ()
                _ | Just credential <- request.connectedGateway ->
                    install $ runGatewaySubagent runtime
                        (gatewayRunner credential ctx.multiSendToRoot)
                CodexSubagents gatewayOnly -> install $
                    runCodexSubagent gatewayOnly runtime request.selectableTokenProvider
                        ctx.multiSendToRoot
                HttpSubagents makeBackend -> install $
                    runHttpSubagent runtime request.dialect request.provider
                        ctx.multiSendToRoot makeBackend
                XaiSubagents contextWindow threshold makeBackend -> install $
                    runXaiParentSubagent runtime request.dialect ctx.multiSendToRoot
                        contextWindow threshold makeBackend
  where
    gatewayRunner credential sendToRoot target runtime env previous prompt onEvent =
        case gatewayLoadedAuthForProvider
                (Just target.collaborationTargetProvider) credential of
            Left err -> pure (Left (LoopUnexpected err))
            Right loaded ->
                let tokens = gatewayTokenProviderForProvider
                        target.collaborationTargetProvider credential
                        loaded.loadedTokenProvider
                in case target.collaborationTargetProvider of
                    OpenAIProvider ->
                        runCodexSubagent True runtime tokens sendToRoot
                            env previous prompt onEvent
                    XAIProvider ->
                        let nativeCapabilities =
                                maybe fullNativeRunCapabilities (.nativeCapabilities)
                                    request.startup.startupNativeHooks
                            options = (XAI.gatewayClientOptions
                                (Text.unpack credential.gatewayBaseUrl))
                                    { XAI.hostedXSearchEnabled =
                                        nativeCapabilities.nativeProviderHostedTools
                                    }
                            contextWindow params =
                                let childModel = fromMaybe
                                        target.collaborationTargetEffectiveModel
                                        params.model
                                in fromMaybe XAI.grokDefaultContextWindow $
                                    catalogContextWindowForTransport
                                        request.catalog
                                        target.collaborationTargetConnection
                                        childModel
                                        childModel
                            threshold params =
                                let window = contextWindow params
                                in max 1 $ min window $
                                    fromMaybe
                                        (XAI.grokAutoCompactTokenLimit
                                            (fromMaybe options.defaultModel params.model)
                                            window)
                                        request.options.optCompactThreshold
                            optionsFor params =
                                options
                                    { XAI.autoCompactTokenLimit = Just (threshold params) }
                            makeBackend params =
                                xaiBackendWithClientOptions optionsFor tokens (pure params)
                        in runXaiSubagent runtime
                            (dialectForId target.collaborationTargetDialect)
                            sendToRoot contextWindow threshold makeBackend
                            env previous prompt onEvent
                    _ -> pure (Left (LoopUnexpected
                        "This provider requires a separate child session; it cannot run as an in-process gateway subagent."))

handleOpenAiStartupResult
    :: AgentSessionRequest windowTitleResult
    -> NativeRunCapabilities
    -> Bool
    -> Either CodexAuthFailed RunResult
    -> IO RunResult
handleOpenAiStartupResult request nativeCapabilities shouldProbeAtStartup = \case
    Right result -> pure result
    Left (CodexAuthFailed err) ->
        let startupFailure = do
                now <- getCurrentTime
                startupDie request.startup (formatApiErrorAt now err)
        in case request.transition of
            Just active | AutomaticFallback _ <- active.transitionCause ->
                pure (RunProviderStartFailed err)
            _ | shouldProbeAtStartup
              , not (isGatewayLoadedAuth request.loaded)
              , isProviderUnavailable err ->
                chooseStartupProviderTransition
                    nativeCapabilities.nativeProviderFallback
                    request.catalog
                    request.workspace.projectRoot
                    request.fullscreen
                    (tokenProviderBillingMode request.tokenProvider)
                    request.provider request.model request.unavailableProviders
                    Nothing err >>= \case
                        Just next -> pure (RunSwitchProvider next)
                        Nothing -> startupFailure
            _ -> startupFailure

withSelectedClaudeAuth
    :: Maybe GatewayCredential
    -> LoadedAuth
    -> (Text -> IO value)
    -> (ClaudeCodeAuth -> IO value)
    -> IO value
withSelectedClaudeAuth connectedGateway loaded onError action
    -- Preserve the credential snapshot that selected the session's catalog.
    | not (isGatewayLoadedAuth loaded) =
        loadClaudeCodeAuth >>= either onError action
    | otherwise = case connectedGateway of
        Nothing -> onError "No organization gateway credential is connected."
        Just credential -> do
            result <- withClaudeGatewayProxy credential \transport ->
                loadClaudeCodeGatewayAuth transport >>= either onError action
            either onError pure result

-- | Print a copy-pasteable --resume line whenever the CLI session quits or
-- crashes. Ctrl-C is normalized to the same graceful 'RunQuit' result as
-- :q/Ctrl-D so every exit path reports the persisted session exactly once.
withResumeHintOnQuit
    :: Maybe FullscreenRuntime
    -> String
    -> Persistence
    -> IO RunResult
    -> IO RunResult
withResumeHintOnQuit fullscreen progName persist action = do
    result <-
        catchUserInterrupt action (pure RunQuit)
            `Safe.onException`
                printCrashResumeHint fullscreen progName persist
    case result of
        RunQuit -> do
            case fullscreen of
                Nothing -> printResumeHint progName persist
                Just runtime ->
                    withFullscreenSuspended runtime
                        (printResumeHint progName persist)
        _ -> pure ()
    -- An interrupt is the requested, graceful end of the CLI session.
    -- Returning lets the surrounding brackets restore the SIGINT handler
    -- and close tools without GHC's top-level exception handler printing
    -- "user interrupt" and a backtrace.
    pure result

-- | Report only a session that is already durable. In particular, do not try
-- to materialize pending persistence while handling a crash: the exception
-- may itself be a storage failure. This handler is best-effort so it can never
-- replace the original exception.
printCrashResumeHint
    :: Maybe FullscreenRuntime
    -> String
    -> Persistence
    -> IO ()
printCrashResumeHint fullscreen progName persist =
    (do
        sessionId <- activePersistenceSessionId persist
        forM_ sessionId \identifier ->
            case fullscreen of
                Nothing -> printResumeHintForId progName identifier
                Just runtime ->
                    withFullscreenSuspended runtime
                        (printResumeHintForId progName identifier)
    ) `Safe.catchAny` \_ -> pure ()

activePersistenceSessionId :: Persistence -> IO (Maybe Text)
activePersistenceSessionId = \case
    PersistenceDisabled -> pure Nothing
    PersistenceEnabled slotRef ->
        readIORef slotRef <&> \case
            PersistencePending{} -> Nothing
            PersistenceActive handle -> Just handle.sessionMeta.metaId

printResumeHint
    :: String
    -> Persistence
    -> IO ()
printResumeHint progName persist = do
    -- A commit interrupted before publication is safe to adopt once. Keep the
    -- retry bounded so a later double Ctrl-C can still force a hung exit.
    sessionId <- retryUserInterruptOnce (ensurePersistenceSessionId persist)
    forM_ sessionId (printResumeHintForId progName)

printResumeHintForId :: String -> Text -> IO ()
printResumeHintForId progName sessionId = do
    -- Drop an in-place "Thinking…" status so the hint is its own line.
    Text.hPutStr stderr "\r\ESC[K"
    clearNativeProgress stderr
    color <- resolveColor stderr
    putTextLn stderr
        (roleMuted color (resumeHint progName sessionId))
