module Agent.CLI.Runtime.Orchestration.Providers (runAgentProviders) where

import Agent.CLI.AccountPicker ()
import Agent.CLI.AccountSelection ()
import Agent.CLI.Afk ()
import Agent.CLI.AgentSessions ()
import Agent.CLI.AgentViewport ()
import Agent.CLI.Approval ()
import Agent.CLI.Artifact ()
import Agent.CLI.Auth.Types (LoadedAuth)
import Agent.CLI.Clipboard ()
import Agent.CLI.CodeModeRuntime ()
import Agent.CLI.Command ()
import Agent.CLI.Compaction
    ( CompactOutcome
    , CompactionInstall
    , OccupancySnapshot
    )
import Agent.CLI.Config ()
import Agent.CLI.Database ()
import Agent.CLI.Database.Store ()
import Agent.CLI.Dialects ()
import Agent.CLI.GatewayBridge ()
import Agent.CLI.GatewayClient (GatewayCredential)
import Agent.CLI.Input ()
import Agent.CLI.LearnedSkills ()
import Agent.CLI.LearnedSkills.Store ()
import Agent.CLI.Login ()
import Agent.CLI.Lsp ()
import Agent.CLI.ManagedTurn ()
import Agent.CLI.McpManager ()
import Agent.CLI.McpStatus ()
import Agent.CLI.ModelConfig (ModelCatalog)
import Agent.CLI.Models ()
import Agent.CLI.Options (CliOptions)
import Agent.CLI.PendingInputs (PendingInputs)
import Agent.CLI.Plan ()
import Agent.CLI.Project (ModelSwitchScope)
import Agent.CLI.Prompt ()
import Agent.CLI.PromptHooks ()
import Agent.CLI.ProviderAvailability ()
import Agent.CLI.ProviderTransition (ProviderTransition)
import Agent.CLI.Recap ()
import Agent.CLI.ReplMode ()
import Agent.CLI.Request ()
import Agent.CLI.Resume ()
import Agent.CLI.Runtime.HistorySource ()
import Agent.CLI.Runtime.Orchestration.Background ()
import Agent.CLI.Runtime.Orchestration.Concurrent ()
import Agent.CLI.Runtime.Orchestration.Providers.Claude
    ( runClaudeProvider
    )
import Agent.CLI.Runtime.Orchestration.Providers.Gemini
    ( runGeminiProvider
    )
import Agent.CLI.Runtime.Orchestration.Providers.OpenAI
    ( runOpenAiProvider
    )
import Agent.CLI.Runtime.Orchestration.Providers.OpenRouter
    ( runOpenRouterProvider
    )
import Agent.CLI.Runtime.Orchestration.Providers.Types
    ( AgentProviderRequest(..)
    )
import Agent.CLI.Runtime.Orchestration.Providers.XAI
    ( runXaiProvider
    )
import Agent.CLI.Runtime.Orchestration.Restart ()
import Agent.CLI.Runtime.Orchestration.Types
    ( NativeRunCapabilities(..)
    , NativeRunHooks(..)
    , fullNativeRunCapabilities
    )
import Agent.CLI.Runtime.Persistence ()
import Agent.CLI.Runtime.Types (RunResult)
import Agent.CLI.Secret ()
import Agent.CLI.Session.Attachments ()
import Agent.CLI.Session.Choices ()
import Agent.CLI.Session.History (LiveConversation)
import Agent.CLI.Session.Lifecycle ()
import Agent.CLI.Session.Runtime.Types
    ( SessionRequest
    , StartupRuntime(..)
    )
import Agent.CLI.Session.Selection ()
import Agent.CLI.Session.Types (Persistence)
import Agent.CLI.SessionAdmin ()
import Agent.CLI.SessionEnv ()
import Agent.CLI.SessionLock ()
import Agent.CLI.SessionState ()
import Agent.CLI.SessionTitle ()
import Agent.CLI.Skills ()
import Agent.CLI.Startup.Auth (startupDie)
import Agent.CLI.StartupContext ()
import Agent.CLI.Subagents.Runtime.Types (SubagentRuntime)
import Agent.CLI.TUI.App (FullscreenRuntime)
import Agent.CLI.TUI.History ()
import Agent.CLI.TUI.SessionHistory ()
import Agent.CLI.Tools ()
import Agent.CLI.Turn ()
import Agent.CLI.Usage ()
import Agent.CLI.WebFetch ()
import Agent.CLI.Worktree ()
import Agent.Cancel ()
import Agent.Dialect (Dialect)
import Agent.Error (ApiError)
import Agent.GrokBuild.Dialect.Goal ()
import Agent.GrokBuild.Dialect.Runtime ()
import Agent.GrokBuild.Dialect.Task ()
import Agent.GrokBuild.Dialect.Workflow ()
import Agent.Loop (TokenUsage, TurnInput)
import Agent.OpenAI.Auth (Pool)
import Agent.OpenAI.Compaction ()
import Agent.OpenAI.Usage ()
import Agent.OpenRouter.Options (ClientOptions)
import Agent.Provider
    ( Credential
    , Provider(..)
    , TokenProvider
    )
import Agent.ReasoningEffort ()
import Agent.Responses.GenericClient ()
import Agent.Responses.Types (ResponseCreateParams)
import Agent.Skills ()
import Agent.Store.Postgres ()
import Agent.Store.Types ()
import Agent.Subagents.TaskPath ()
import Agent.Tools.MultiAgents ()
import Agent.Tools.MultiAgents (MultiAgentContext)
import Agent.Tools.PlanMode ()
import Agent.Tools.Secret ()
import Agent.Tools.TaskPlan (TaskPlanEnv)
import Agent.Tools.Types ()
import Agent.TUI.Motion ()
import Agent.XAI.Usage ()
import Control.Applicative ()
import Control.Concurrent.STM (STM)
import Control.Exception ()
import Data.ByteString ()
import Data.Functor ()
import Data.IORef (IORef)
import Data.List ()
import Data.Set (Set)
import Data.Text (Text)
import System.Console.ANSI ()
import System.Console.ANSI.Codes ()
import System.Directory.OsPath ()
import System.Environment ()
import System.Exit ()
import System.IO (Handle)
import System.OsPath (OsPath)
import System.OsPath ()
import qualified Agent.OpenRouter.Usage ()
import qualified Agent.Provider ()
import qualified Data.Map.Strict ()
import qualified Data.Set ()
import qualified Agent.Responses.GenericClient as GenericResponses

runAgentProviders
    :: ModelSwitchScope
    -> LoadedAuth
    -> Maybe GatewayCredential
    -> (Maybe (STM ApiError)
        -> Maybe TokenProvider
        -> Maybe Pool
        -> Maybe (Text -> IO (Either ApiError Text))
        -> IO (Maybe Int)
        -> (Maybe Text -> IO (Either Text CompactOutcome))
        -> SessionRequest)
    -> IORef Text
    -> IORef Text
    -> IORef Text
    -> ModelCatalog
    -> Bool
    -> IORef (Maybe OccupancySnapshot)
    -> ((Text -> Text) -> Int -> ResponseCreateParams -> Int)
    -> IORef LiveConversation
    -> ((Text -> Text) -> IO (Maybe Int))
    -> Maybe GenericResponses.GenericClientOptions
    -> OsPath
    -> Dialect
    -> Maybe FullscreenRuntime
    -> IORef (CompactOutcome -> [TurnInput] -> IO CompactionInstall)
    -> Maybe TaskPlanEnv
    -> OsPath
    -> Maybe Text
    -> Text
    -> Maybe MultiAgentContext
    -> ClientOptions
    -> CliOptions
    -> ResponseCreateParams
    -> IORef ResponseCreateParams
    -> PendingInputs
    -> Persistence
    -> IORef (Maybe Text)
    -> OsPath
    -> Provider
    -> (TokenUsage -> IO ())
    -> (Credential -> IO Text)
    -> (Text -> IO (Either ApiError Text))
    -> TokenProvider
    -> Bool
    -> StartupRuntime
    -> Maybe (STM ApiError)
    -> Handle
    -> SubagentRuntime
    -> TokenProvider
    -> Maybe ProviderTransition
    -> (Text -> Text)
    -> Set Provider
    -> IO RunResult
runAgentProviders
    modelSwitchScope
    loaded
    connectedGateway
    sessionRequest
    activeAccountIdRef
    activeAccountRef
    activeSelectionRef
    catalog
    claudeBypassEnabled
    contextTokensRef
    contextWindowForParams
    conversationRef
    currentModelContextWindow
    customGenericOptions
    cwd
    dialect
    fullscreen
    automaticCompactionHookRef
    taskPlan
    home
    initialPrevious
    model
    multiCtx
    openRouterOptions
    options
    _params
    paramsRef
    pendingNotices
    persist
    preferredOpenAiAccountRef
    projectRoot
    provider
    recordCompactionUsage
    resolveActiveAccountLabel
    selectHttpAccount
    selectableTokenProvider
    shouldProbeAtStartup
    startup
    startupUnavailable
    stderrHandle
    subagentRuntime
    tokenProvider
    transition
    transportModel
    unavailableProviders
    =
        runAgentProvidersRequest AgentProviderRequest{..}

runAgentProvidersRequest
    :: AgentProviderRequest
    -> IO RunResult
runAgentProvidersRequest request@AgentProviderRequest{provider, startup} =
    let nativeCapabilities =
            maybe
                fullNativeRunCapabilities
                (.nativeCapabilities)
                startup.startupNativeHooks
    in case provider of
        OpenAIProvider ->
            runOpenAiProvider request nativeCapabilities
        XAIProvider ->
            runXaiProvider request nativeCapabilities
        GeminiProvider ->
            runGeminiProvider request
        ClaudeCodeProvider
            | not nativeCapabilities.nativeProviderNativeTools ->
                startupDie startup
                    "Claude Code is unavailable in this runtime"
            | otherwise ->
                runClaudeProvider request nativeCapabilities
        OpenRouterProvider ->
            runOpenRouterProvider request
