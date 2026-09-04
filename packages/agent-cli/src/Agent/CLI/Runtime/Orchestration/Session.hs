module Agent.CLI.Runtime.Orchestration.Session (runAgentSession) where

import Agent.CLI.AccountPicker ()
import Agent.CLI.AccountSelection ()
import Agent.CLI.Afk ()
import Agent.CLI.AgentSessions ()
import Agent.CLI.AgentViewport ()
import Agent.CLI.Approval ()
import Agent.CLI.Artifact ()
import Agent.CLI.Auth
    ( LoadedAuth(loadedTokenProvider)
    , isGatewayLoadedAuth
    )
import Agent.CLI.Clipboard ()
import Agent.CLI.Claude
    ( ClaudeSessionRuntimeSlot
    , newClaudeSessionRuntimeSlot
    )
import Agent.CLI.CodeModeRuntime
    ( CodeModeSessionRuntime(..),
      CodexCatalogSession(..),
      codeModeSessionRuntimeFor,
      imageGenerationCodeModeRuntimeFor,
      loadCodexCatalogModelInfo )
import Agent.CLI.Command ()
import Agent.CLI.Compaction
    ( AutomaticCompactionBoundary
    , CompactOutcome
    , CompactionInstall(CompactionNotInstalled)
    , OccupancySnapshot
    )
import Agent.CLI.Config ()
import Agent.Connectivity ()
import Agent.CLI.Database ()
import Agent.CLI.Database.Store (DatabaseScopes)
import Agent.CLI.Dialects (CodingTools(..))
import Agent.CLI.Error ()
import Agent.CLI.GatewayClient
    ( GatewayCredential
    , GatewayModelAccess
    , gatewayCredentialIdentity
    )
import Agent.CLI.GatewayBridge ()
import Agent.CLI.Input ()
import Agent.CLI.Interrupt
    ( InterruptState
    , catchUserInterrupt
    , retryUserInterruptOnce
    , withCtrlCHandler
    )
import Agent.CLI.LearnedSkills ()
import Agent.CLI.LearnedSkills.Store ()
import Agent.CLI.Login ()
import Agent.CLI.Lsp ()
import Agent.CLI.ManagedTurn ( ManagedTurnRequest(..) )
import Agent.CLI.McpManager ()
import Agent.CLI.McpStatus ()
import Agent.CLI.ModelConfig
    (ModelCatalog, catalogContextWindowForTransport)
import Agent.CLI.Models (ModelTarget(targetConnectionId))
import Agent.CLI.Options
    ( ApprovalPolicy
    , isOneShot
    , CliOptions(optCodeMode)
    )
import Agent.CLI.PendingInputs (PendingInputs)
import Agent.CLI.Plan ()
import Agent.CLI.Project ( ModelSwitchScope(..) )
import Agent.CLI.Prompt
    ( codexEnvironmentContext,
      subscriptionSubagentModelGuidance,
      appendMcpInstructions,
      systemPromptForCatalogModelWithHostedSearch,
      systemPromptForToolsWithHostedSearch )
import Agent.CLI.PromptHooks ()
import Agent.CLI.Provider.OpenAI ()
import Agent.CLI.Provider.Switch ()
import Agent.CLI.ProviderAvailability ( probeLoadedAvailability )
import Agent.CLI.ProviderFallback ( isProviderUnavailable )
import Agent.CLI.ProviderTransition (PendingTurn, ProviderTransition)
import Agent.CLI.Recap ()
import Agent.CLI.Render ( putTextLn )
import Agent.CLI.ReplMode ()
import Agent.CLI.Request
    ( requestParams
    , setRequestInstructionsAndTools
    , setRequestPromptCacheKey
    )
import Agent.CLI.Resume ( resumeNeedsGeneratedContext )
import Agent.CLI.Runtime.HistorySource ()
import Agent.CLI.Runtime.Orchestration.Background ()
import Agent.CLI.Runtime.Orchestration.Providers
    ( runAgentProviders )
import Agent.CLI.Runtime.Orchestration.Restart ()
import Agent.CLI.Runtime.Orchestration.Startup
    ( clearNativeProgress, mcpToolCollision, reportStartupWarning )
import Agent.CLI.Runtime.Orchestration.Types
    ( NativeRunCapabilities(..)
    , NativeRunHooks(nativeCapabilities, nativeWorkspaceDiscovery)
    , fullNativeRunCapabilities
    , nativeLoadsHostWorkspaceContext
    )
import Agent.CLI.Runtime.Persistence ()
import Agent.CLI.Runtime.Recap ()
import Agent.CLI.Runtime.Repl ()
import Agent.CLI.Runtime.Types ( RunResult(RunQuit) )
import Agent.CLI.Secret ()
import Agent.CLI.Session
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
      SessionHandle(sessionDir),
      SessionMeta(metaId, metaLastResponseId, metaPromptSnapshot, metaTitle),
      SessionTurn,
      SessionPromptSnapshot(..) )
import Agent.CLI.Session.Attachments ()
import Agent.CLI.Session.Choices ()
import Agent.CLI.Session.History
    ( LiveConversation
    , currentLiveTranscriptGeneration,
      durableTranscriptCheckpoint,
      evictLiveTranscript,
      foldSessionItems,
      readLiveTranscript,
      replaceLiveConversation )
import Agent.CLI.Session.Lifecycle ()
import Agent.CLI.Session.Runtime.Types
    ( SessionRequest(codexCatalogSession, SessionRequest, catalog,
                     gatewayModelsRef, modelInfo,
                     connectionId, gatewayIdentity,
                     options, provider, dialect, commitAttributionModel,
                     commitAttributionEffort, policyRef, allTools,
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
                     initialGrokContext, persist,
                     contextOccupancyRef, currentContextWindow,
                     startupWindowTitle, automaticCompactionRef,
                     projectRoot, home, cwd, tokenProvider, openAiPool, startupContext,
                     automaticCompactionHookRef, skillsRef, skillInvocationsRef,
                     escPaused, interrupt, multiCtx, rootTurnRef, subagentSessions,
                     pendingNotices, storeRoot, agentTypes, legacyTarget, usageRef,
                     accountRef, accountIdRef, selectionRef, accountLabel,
                     selectAccount, onPersisted, compactRunner, codeModeNestedSlot),
      StartupRuntime(startupBackground, startupDatabaseStore,
                     startupNetworkRecovery, startupSessionState,
                     startupNativeHooks) )
import Agent.CLI.Session.Selection ( reservedSessionId )
import Agent.CLI.SessionAdmin ()
import Agent.CLI.SessionEnv ()
import Agent.CLI.SessionLock ()
import Agent.CLI.SessionState ( SessionState(sessionConversation) )
import Agent.CLI.SessionTitle ()
import Agent.CLI.Skills ()
import Agent.CLI.Startup.Auth ( markStartupStage, startupDie )
import Agent.CLI.StartupContext
    ( AgentsContextNotice(..), loadAgentsContext )
import Agent.CLI.Style ( cliWindowTitle, roleMuted )
import Agent.CLI.Subagents.Runtime
    ( SubagentRuntime(subagentOpenAiChild, SubagentRuntime,
                      subagentOptions, subagentNetworkRecovery,
                      subagentGhciEnabled, subagentBashEnabled,
                      subagentPolicy, subagentPlanHooks, subagentSkillRoots,
                      subagentAllowedRoots, subagentRootAccessRequest,
                      subagentParams, subagentMcpTools, subagentRegistry,
                      subagentSessions, subagentStoreRoot, subagentTypes,
                      subagentLegacyTarget, subagentConnection, subagentMapModel,
                      subagentCreateWorktree, subagentSessionTmp,
                      subagentSpawnModelGuidance, subagentAllowedChildModels,
                      subagentResolveChildModel, subagentChildModelAllowed) )
import Agent.CLI.Subagents.Runtime.Types
    (SubagentSession, SubagentStoreRoot)
import Agent.CLI.TUI.App
    ( FullscreenRuntime, withFullscreenSuspended )
import Agent.CLI.TUI.History ()
import Agent.CLI.TUI.SessionHistory ()
import Agent.CLI.Terminal ( resolveColor )
import Agent.CLI.Tools
    ( schemasFromAppToolsCodeModeWithHostedSearch
    , schemasFromAppToolsWithHostedSearch
    )
import Agent.CLI.Turn ()
import Agent.CLI.Usage ()
import Agent.CLI.WebFetch ()
import Agent.CLI.Worktree ()
import Agent.Cancel ()
import Agent.Claude ()
import Agent.Dialect (Dialect, dialectId)
import Agent.Error (ApiError)
import Agent.GrokBuild.Dialect.Goal ()
import Agent.GrokBuild.Dialect.Runtime ()
import Agent.GrokBuild.Dialect.Task (GrokSubagentSpecs)
import Agent.GrokBuild.Dialect.Workflow ()
import Agent.Loop
    ( ImageAttachment
    , TokenUsage
    , TurnInput
    , addTokenUsage
    , emptyTokenUsage
    )
import Agent.OpenAI.Compaction ()
import Agent.OpenAI.ImageGeneration ( imageGenerationToolName )
import Agent.OpenAI.Usage ()
import Agent.OpenAI.WebSocketClient ()
import Agent.OpenAI.Models.Types ( ModelInfo, resolvedContextWindow )
import Agent.OpenRouter.LoopBackend ()
import Agent.OpenRouter.Options (ClientOptions)
import Agent.OsPath ()
import Agent.Provider
    (Credential, Provider(OpenAIProvider), TokenProvider,
     tokenProviderBillingMode)
import Agent.ReasoningEffort ()
import Agent.Responses.GenericBackend ()
import Agent.Responses.GenericClient (GenericClientOptions)
import Agent.Responses.Types
    (ResponseItem, ResponseCreateParams(model))
import Agent.Skills (SkillCatalog, SkillInvocation)
import Agent.Store.Postgres ( trustedPool )
import Agent.Store.Types ()
import Agent.Subagents (SubagentRegistry)
import Agent.Subagents.Types (RootTurnId, SubagentId)
import Agent.Subagents.TaskPath ()
import Agent.TUI.Model ()
import Agent.TUI.Motion ()
import Agent.Tools.MultiAgents
    (CollaborationModelTarget, MultiAgentContext, SubagentWorktree)
import Agent.Tools.PlanMode (PlanModeEnv, PlanModeHooks)
import Agent.Tools.Secret ()
import Agent.ToolDispatch (canonicalToolName)
import Agent.Tools.Types
    ( AppTool(..)
    , ToolSchema(..)
    , ToolEnv(toolAllowedRoots, toolRootAccessRequest, toolSkillRoots, toolSessionTmp)
    )
import Agent.XAI.LoopBackend ()
import Control.Applicative ( (<|>) )
import Control.Concurrent.Async ( waitSTM, withAsync )
import Control.Concurrent.Chan ()
import Control.Concurrent.MVar ()
import Control.Concurrent.STM ( STM, retry )
import Control.Exception ()
import Control.Exception.Safe ( mask_, finally )
import Control.Monad ( forM_, void, when )
import Data.Functor ( (<&>) )
import Data.IORef
    ( IORef,
      modifyIORef',
      newIORef,
      readIORef,
      writeIORef )
import Data.List ()
import Data.Map.Strict (Map)
import Data.Maybe ( isJust, isNothing, fromMaybe )
import Data.Set (Set)
import Data.Text (Text)
import Data.Time.Clock ( getCurrentTime, utctDay )
import System.Console.ANSI ()
import System.Console.ANSI.Codes ()
import System.Directory.OsPath ()
import System.Environment ( getProgName )
import System.Exit ()
import System.IO (Handle, stderr)
import System.Mem ( performMajorGC )
import System.OsPath (OsPath)
import qualified Data.ByteString as BS ()
import qualified Agent.Responses.GenericClient as GenericResponses
    ()
import qualified Agent.MCP as MCP
import qualified Data.Map.Strict as Map ()
import qualified Agent.OpenAI.Auth as OpenAI (Pool)
import qualified Agent.OpenRouter as OpenRouter ()
import qualified Agent.OpenRouter.Usage as OpenRouterUsage ()
import qualified Agent.Provider as Provider ()
import qualified Agent.CLI.Session.Lifecycle as SessionLifecycle ()
import qualified Agent.CLI.Session.Runner as SessionRunner ()
import qualified Data.Set as Set ()
import qualified Data.Text as Text ( unpack )
import qualified Data.Text.IO as Text ( hPutStr )
import qualified Agent.XAI.Options as XAI ()
import qualified Agent.XAI.Client as XAIClient ()
import qualified Agent.XAI.Request as XAIRequest ()
import qualified Agent.XAI.Usage as XAIUsage ()

data AgentSessionRequest closeResult windowTitleResult = AgentSessionRequest
    { loaded :: LoadedAuth
    , connectedGateway :: Maybe GatewayCredential
    , learnAboutUserRequested :: Bool
    , sessionTmp :: OsPath
    , activeAccountIdRef :: IORef Text
    , activeAccountRef :: IORef Text
    , activeSelectionRef :: IORef Text
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
    , closeAll :: IO closeResult
    , codeModeCloseRef :: IORef (IO ())
    , coding :: CodingTools
    , createSubagentWorktree
        :: OsPath -> IO (Either Text SubagentWorktree)
    , customGenericOptions :: Maybe GenericClientOptions
    , cwd :: OsPath
    , databaseAppTools :: [AppTool]
    , databaseScopes :: DatabaseScopes
    , dialect :: Dialect
    , effortText :: Text
    , escPaused :: IORef Bool
    , extraTools :: [AppTool]
    , fullscreen :: Maybe FullscreenRuntime
    , gatewayTools :: [AppTool]
    , ghciEnabledRef :: IORef Bool
    , allowedChildModels :: Maybe [Text]
    , resolveChildModel
        :: Maybe (Text -> IO (Maybe CollaborationModelTarget))
    , childModelAllowed :: Maybe (Text -> IO Bool)
    , home :: OsPath
    , inferredTarget :: ModelTarget
    , interrupt :: InterruptState
    , learnedSkillAppTools :: [AppTool]
    , legacySubagentTarget :: Maybe LegacySubagentTarget
    , mcpFleet :: MCP.McpFleet
    , mcpInstructions :: [(Text, Text)]
    , mcpTools :: [AppTool]
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
    , projectRoot :: OsPath
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
    , sessionBaseParams :: ResponseCreateParams
    , sessionReservedId :: Maybe Text
    , sessionEnvironmentContext :: Maybe Text
    }

data SessionPromptRuntime = SessionPromptRuntime
    { sessionCodeRuntime :: SessionCodeRuntime
    , sessionParams :: ResponseCreateParams
    , sessionParamsRef :: IORef ResponseCreateParams
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
    :: LoadedAuth
    -> Maybe GatewayCredential
    -> Bool
    -> OsPath
    -> IORef Text
    -> IORef Text
    -> IORef Text
    -> GrokSubagentSpecs
    -> [AppTool]
    -> ([ImageAttachment] -> IO ())
    -> IO ()
    -> IORef Bool
    -> ModelCatalog
    -> IORef (Maybe GatewayModelAccess)
    -> Bool
    -> (SessionHandle -> IO ())
    -> Bool
    -> IO closeResult
    -> IORef (IO ())
    -> CodingTools
    -> (OsPath -> IO (Either Text SubagentWorktree))
    -> Maybe GenericClientOptions
    -> OsPath
    -> [AppTool]
    -> DatabaseScopes
    -> Dialect
    -> Text
    -> IORef Bool
    -> [AppTool]
    -> Maybe FullscreenRuntime
    -> [AppTool]
    -> IORef Bool
    -> Maybe [Text]
    -> Maybe (Text -> IO (Maybe CollaborationModelTarget))
    -> Maybe (Text -> IO Bool)
    -> OsPath
    -> ModelTarget
    -> InterruptState
    -> [AppTool]
    -> Maybe LegacySubagentTarget
    -> MCP.McpFleet
    -> [(Text, Text)]
    -> [AppTool]
    -> Text
    -> Maybe MultiAgentContext
    -> (OsPath -> IO ())
    -> ClientOptions
    -> Maybe TokenProvider
    -> CliOptions
    -> PendingInputs
    -> Maybe PendingTurn
    -> Persistence
    -> PlanModeHooks
    -> PlanModeEnv
    -> ApprovalPolicy
    -> IORef (Maybe Text)
    -> OsPath
    -> Maybe ManagedTurnRequest
    -> Provider
    -> Bool
    -> SubagentRegistry
    -> (Credential -> IO Text)
    -> Bool
    -> Maybe (SessionMeta, [SessionTurn])
    -> OsPath
    -> IORef (Maybe RootTurnId)
    -> (Text -> IO (Either ApiError Text))
    -> TokenProvider
    -> [AppTool]
    -> (Text -> IO titleResult)
    -> IORef [SkillInvocation]
    -> IORef SkillCatalog
    -> StartupRuntime
    -> FilePath
    -> Handle
    -> IORef (Maybe (IO [ResponseItem]))
    -> IORef (Map SubagentId SubagentSession)
    -> SubagentStoreRoot
    -> TokenProvider
    -> ToolEnv
    -> [AppTool]
    -> Maybe ProviderTransition
    -> (Text -> Text)
    -> Set Provider
    -> IO RunResult
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
    recordImageGenerationInputs
    clearImageGenerationHistory
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
    dialect
    effortText
    escPaused
    extraTools
    fullscreen
    gatewayTools
    ghciEnabledRef
    allowedChildModels
    resolveChildModel
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
    sessionTools
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
    toolEnv
    tools
    transition
    transportModel
    unavailableProviders
    =
    runAgentSessionRequest AgentSessionRequest{..}

runAgentSessionRequest
    :: AgentSessionRequest closeResult windowTitleResult
    -> IO RunResult
runAgentSessionRequest request@AgentSessionRequest{closeAll} =
    flip finally closeAll do
        validateSessionMcpTools request
        codeRuntime <- prepareSessionCodeRuntime request
        promptRuntime <- prepareSessionPromptRuntime request codeRuntime
        liveRuntime <- prepareSessionLiveRuntime request promptRuntime
        launchPreparedSession request promptRuntime liveRuntime

validateSessionMcpTools
    :: AgentSessionRequest closeResult windowTitleResult
    -> IO ()
validateSessionMcpTools AgentSessionRequest
    { coding
    , extraTools
    , sessionTools
    , gatewayTools
    , databaseAppTools
    , learnedSkillAppTools
    , mcpFleet
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
                mcpFleet.mcpFleetRegistrations
        of
            Just err ->
                startupDie startup
                    ("Failed to initialize MCP tools: " <> err)
            Nothing -> pure ()

prepareSessionCodeRuntime
    :: AgentSessionRequest closeResult windowTitleResult
    -> IO SessionCodeRuntime
prepareSessionCodeRuntime AgentSessionRequest
    { loaded
    , stateDirectory
    , provider
    , dialect
    , selectableTokenProvider
    , model
    , startup
    , options
    , tools
    , codeModeCloseRef
    , allTools
    , effortText
    , cwd
    , sessionTmp
    , persist
    , mcpInstructions
    } = do
    today <- utctDay <$> getCurrentTime
    -- Catalog models provide the per-model instructions template. Full code
    -- mode remains opt-in, while code_mode_only models still route the
    -- reserved image-generation tool through exec.
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
        initializeCodeMode
            | not nativeCapabilities.nativeHostExtensions =
                pure (Right Nothing)
            | options.optCodeMode =
                codeModeSessionRuntimeFor sessionModelInfo tools
            | otherwise =
                imageGenerationCodeModeRuntimeFor sessionModelInfo tools
        codeModeFallbackWarning
            | options.optCodeMode =
                "code mode unavailable; falling back to compatible \
                \direct tools: "
            | otherwise =
                "image generation code mode unavailable; \
                \disabling image generation: "
    (sessionCodeModeRuntime, suppressDirectImageGeneration) <-
        initializeCodeMode >>= \case
            Left err -> do
                reportStartupWarning startup
                    (codeModeFallbackWarning <> err)
                pure (Nothing, True)
            Right runtime -> pure (runtime, False)
    writeIORef codeModeCloseRef
        (maybe (pure ()) (.codeModeClose) sessionCodeModeRuntime)
    sessionReservedId <- reservedSessionId persist
    let providerTools
            | suppressDirectImageGeneration =
                filter
                    ((/= imageGenerationToolName) . (.appToolName))
                    tools
            | otherwise = tools
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
        wireSchemas = case sessionCodeModeRuntime of
            Just codeMode ->
                schemasFromAppToolsCodeModeWithHostedSearch
                    includeHostedSearch
                    dialect
                    ( codeMode.codeModeWireTools
                        <> codeMode.codeModeDirectTools
                    )
            Nothing ->
                schemasFromAppToolsWithHostedSearch
                    includeHostedSearch
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
    :: AgentSessionRequest closeResult windowTitleResult
    -> SessionCodeRuntime
    -> IO SessionPromptRuntime
prepareSessionPromptRuntime AgentSessionRequest
    { provider
    , inferredTarget
    , dialect
    , cwd
    , resumed
    , transition
    , resumeTargetChanged
    , policy
    , allTools
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
        sessionInitialItems = maybe [] (foldSessionItems . snd) resumed
        initialTurns = maybe [] snd resumed
        sessionResumeNeedsFreshContext =
            resumeNeedsGeneratedContext initialTurns
        sessionInitialPrevious = case transition of
            Just _ -> Nothing
            Nothing
                | resumeTargetChanged -> Nothing
                | otherwise ->
                    resumed >>= \(meta, _) -> meta.metaLastResponseId
        sessionNeedsInitialContext =
            sessionResumeNeedsFreshContext
                || (null initialTurns && isNothing sessionInitialPrevious)
        sessionRestoredPromptSnapshot
            | null initialTurns && isNothing sessionInitialPrevious =
                compatiblePromptSnapshot
            | otherwise = Nothing
        sessionQueueInitialContext =
            sessionNeedsInitialContext
                && isNothing sessionRestoredPromptSnapshot
        sessionClaudeBridgeTools =
            filter isClaudeBridgeTool allTools
    sessionParamsRef <- newIORef sessionParams
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
    :: AgentSessionRequest closeResult windowTitleResult
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
    :: AgentSessionRequest closeResult windowTitleResult
    -> SessionPromptRuntime
    -> (Text -> Text)
    -> IO (Maybe Int)
sessionCurrentModelContextWindow request promptRuntime mapTransportModel = do
    currentParams <- readIORef promptRuntime.sessionParamsRef
    pure $
        sessionCatalogContextWindowForParams
            request
            mapTransportModel
            currentParams

sessionContextWindowForParams
    :: AgentSessionRequest closeResult windowTitleResult
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
    :: AgentSessionRequest closeResult windowTitleResult
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
        , subagentSpawnModelGuidance =
            if provider == OpenAIProvider && isJust allowedChildModels
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
    :: AgentSessionRequest closeResult windowTitleResult
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
    :: AgentSessionRequest closeResult windowTitleResult
    -> Text
sessionWindowTitle AgentSessionRequest
    { resumed
    , promptRequest
    , cwd
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
    :: AgentSessionRequest closeResult windowTitleResult
    -> SessionPromptRuntime
    -> IO (IORef (Maybe Text))
loadSessionStartupContext AgentSessionRequest
    { resumed
    , transition
    , stderrHandle
    , fullscreen
    , options
    , dialect
    , home
    , cwd
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
                loadAgentsContext
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
    :: AgentSessionRequest closeResult windowTitleResult
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
    :: AgentSessionRequest closeResult windowTitleResult
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
    :: AgentSessionRequest closeResult windowTitleResult
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
    :: AgentSessionRequest closeResult windowTitleResult
    -> SessionPromptRuntime
    -> SessionLiveRuntime
    -> Maybe (STM ApiError)
    -> Maybe TokenProvider
    -> Maybe OpenAI.Pool
    -> Maybe (Text -> IO (Either ApiError Text))
    -> IO (Maybe Int)
    -> (Maybe Text -> IO (Either Text CompactOutcome))
    -> SessionRequest
buildProviderSessionRequest
    request
    promptRuntime
    liveRuntime
    startupUnavailable
    sessionTokenProvider
    sessionOpenAiPool
    sessionSelectAccount
    sessionContextWindow
    sessionCompactRunner =
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
            , recordImageGenerationInputs =
                request.recordImageGenerationInputs
            , clearImageGenerationHistory =
                request.clearImageGenerationHistory
            , suspendGhci = request.coding.codingSuspendGhci
            , resetToolSessionTemp =
                request.coding.codingResetSessionTemp
            , grokRuntime = request.coding.codingGrokRuntime
            , mcpRegistrations =
                request.mcpFleet.mcpFleetRegistrations
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
                configured <- sessionContextWindow
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
            , initialGrokContext =
                promptRuntime.sessionRestoredPromptSnapshot
                    >>= (.promptSnapshotGrokContext)
            , persist = request.persist
            , startupWindowTitle =
                liveRuntime.sessionStartupWindowTitle
            , projectRoot = request.projectRoot
            , home = request.home
            , cwd = request.cwd
            , tokenProvider = sessionTokenProvider
            , openAiPool = sessionOpenAiPool
            , startupContext = liveRuntime.sessionStartupContext
            , automaticCompactionHookRef =
                promptRuntime.sessionAutomaticCompactionHookRef
            , skillsRef = request.skillsRef
            , skillInvocationsRef = request.skillInvocationsRef
            , escPaused = request.escPaused
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
            , accountIdRef = request.activeAccountIdRef
            , selectionRef = request.activeSelectionRef
            , accountLabel = request.resolveActiveAccountLabel
            , selectAccount = sessionSelectAccount
            , onPersisted = request.claimCurrentSession
            , compactRunner = sessionCompactRunner
            , codeModeNestedSlot =
                (.codeModeNestedSlot)
                    <$> promptRuntime.sessionCodeRuntime.sessionCodeModeRuntime
            , codexCatalogSession =
                promptRuntime.sessionCodeRuntime.sessionCatalogSession
            }

withSessionStartupAvailability
    :: AgentSessionRequest closeResult windowTitleResult
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
    :: AgentSessionRequest closeResult windowTitleResult
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
    :: AgentSessionRequest closeResult windowTitleResult
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
                runAgentProviders
                    (if request.startup.startupBackground
                        then SessionLocalSwitch
                        else TopLevelSwitch)
                    request.loaded
                    request.connectedGateway
                    (buildProviderSessionRequest
                        request
                        promptRuntime
                        liveRuntime)
                    request.activeAccountIdRef
                    request.activeAccountRef
                    request.activeSelectionRef
                    request.catalog
                    request.claudeBypassEnabled
                    liveRuntime.sessionContextTokensRef
                    (sessionContextWindowForParams request)
                    liveRuntime.sessionConversationRef
                    (sessionCurrentModelContextWindow
                        request
                        promptRuntime)
                    request.customGenericOptions
                    request.cwd
                    request.dialect
                    request.fullscreen
                    promptRuntime.sessionAutomaticCompactionHookRef
                    request.coding.codingTaskPlan
                    request.home
                    promptRuntime.sessionInitialPrevious
                    request.model
                    request.multiCtx
                    request.openRouterOptions
                    request.options
                    promptRuntime.sessionParams
                    promptRuntime.sessionParamsRef
                    request.pendingNotices
                    request.persist
                    request.preferredOpenAiAccountRef
                    request.projectRoot
                    request.provider
                    liveRuntime.sessionRecordCompactionUsage
                    request.resolveActiveAccountLabel
                    request.selectHttpAccount
                    request.selectableTokenProvider
                    shouldProbeAtStartup
                    request.startup
                    startupUnavailable
                    request.stderrHandle
                    liveRuntime.sessionSubagentRuntime
                    request.tokenProvider
                    request.transition
                    request.transportModel
                    request.unavailableProviders

-- | Print a copy-pasteable --resume line whenever the CLI session quits.
-- Ctrl-C is normalized to the same graceful 'RunQuit' result as :q/Ctrl-D so
-- every exit path reports the persisted session exactly once.
withResumeHintOnQuit
    :: Maybe FullscreenRuntime
    -> String
    -> Persistence
    -> IO RunResult
    -> IO RunResult
withResumeHintOnQuit fullscreen progName persist action = do
    result <- catchUserInterrupt action (pure RunQuit)
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

printResumeHint
    :: String
    -> Persistence
    -> IO ()
printResumeHint progName persist = do
    -- A commit interrupted before publication is safe to adopt once. Keep the
    -- retry bounded so a later double Ctrl-C can still force a hung exit.
    sessionId <- retryUserInterruptOnce (ensurePersistenceSessionId persist)
    case sessionId of
        Nothing -> pure ()
        Just sessionId -> do
            -- Drop an in-place "Thinking…" status so the hint is its own line.
            Text.hPutStr stderr "\r\ESC[K"
            clearNativeProgress stderr
            color <- resolveColor stderr
            putTextLn stderr
                (roleMuted color (resumeHint progName sessionId))
