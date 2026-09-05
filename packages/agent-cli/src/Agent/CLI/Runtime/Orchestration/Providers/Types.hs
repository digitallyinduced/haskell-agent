module Agent.CLI.Runtime.Orchestration.Providers.Types
    ( AgentProviderRequest(..)
    ) where

import Agent.CLI.ActiveAccount
    ( ActiveAccountRef
    )
import Agent.CLI.Auth.Types (LoadedAuth)
import Agent.CLI.Compaction
    ( CompactOutcome
    , CompactionInstall
    , OccupancySnapshot
    )
import Agent.CLI.GatewayClient (GatewayCredential)
import Agent.CLI.ModelConfig (ModelCatalog)
import Agent.CLI.Options (CliOptions)
import Agent.CLI.PendingInputs (PendingInputs)
import Agent.CLI.Project (ModelSwitchScope)
import Agent.CLI.ProviderTransition (ProviderTransition)
import Agent.CLI.Session.History (LiveConversation)
import Agent.CLI.Session.Runtime.Types (SessionRequest, StartupRuntime)
import Agent.CLI.Session.Types (Persistence)
import Agent.CLI.Subagents.Runtime.Types (SubagentRuntime)
import Agent.CLI.TUI.App (FullscreenRuntime)
import Agent.Dialect (Dialect)
import Agent.Error (ApiError)
import Agent.Loop (TokenUsage, TurnInput)
import Agent.OpenAI.Auth (Pool)
import Agent.OpenRouter.Options (ClientOptions)
import Agent.Provider (Credential, Provider, TokenProvider)
import Agent.Responses.Types (ResponseCreateParams)
import Agent.Tools.MultiAgents (MultiAgentContext)
import Agent.Tools.TaskPlan (TaskPlanEnv)
import Control.Concurrent.STM (STM)
import Data.IORef (IORef)
import Data.Set (Set)
import Data.Text (Text)
import System.IO (Handle)
import System.OsPath (OsPath)
import qualified Agent.Responses.GenericClient as GenericResponses

data AgentProviderRequest = AgentProviderRequest
    { modelSwitchScope :: ModelSwitchScope
    , loaded :: LoadedAuth
    , connectedGateway :: Maybe GatewayCredential
    , sessionRequest
        :: Maybe (STM ApiError)
        -> Maybe TokenProvider
        -> Maybe Pool
        -> Maybe (Text -> IO (Either ApiError Text))
        -> IO (Maybe Int)
        -> (Maybe Text -> IO (Either Text CompactOutcome))
        -> SessionRequest
    , activeAccountRef :: ActiveAccountRef
    , catalog :: ModelCatalog
    , claudeBypassEnabled :: Bool
    , contextTokensRef :: IORef (Maybe OccupancySnapshot)
    , contextWindowForParams
        :: (Text -> Text)
        -> Int
        -> ResponseCreateParams
        -> Int
    , conversationRef :: IORef LiveConversation
    , currentModelContextWindow
        :: (Text -> Text)
        -> IO (Maybe Int)
    , customGenericOptions :: Maybe GenericResponses.GenericClientOptions
    , cwd :: OsPath
    , dialect :: Dialect
    , fullscreen :: Maybe FullscreenRuntime
    , automaticCompactionHookRef
        :: IORef
            (CompactOutcome -> [TurnInput] -> IO CompactionInstall)
    , taskPlan :: Maybe TaskPlanEnv
    , home :: OsPath
    , initialPrevious :: Maybe Text
    , model :: Text
    , multiCtx :: Maybe MultiAgentContext
    , openRouterOptions :: ClientOptions
    , options :: CliOptions
    , paramsRef :: IORef ResponseCreateParams
    , pendingNotices :: PendingInputs
    , persist :: Persistence
    , preferredOpenAiAccountRef :: IORef (Maybe Text)
    , projectRoot :: OsPath
    , provider :: Provider
    , recordCompactionUsage :: TokenUsage -> IO ()
    , resolveActiveAccountLabel :: Credential -> IO Text
    , selectHttpAccount :: Text -> IO (Either ApiError Text)
    , selectableTokenProvider :: TokenProvider
    , shouldProbeAtStartup :: Bool
    , startup :: StartupRuntime
    , startupUnavailable :: Maybe (STM ApiError)
    , stderrHandle :: Handle
    , subagentRuntime :: SubagentRuntime
    , tokenProvider :: TokenProvider
    , transition :: Maybe ProviderTransition
    , transportModel :: Text -> Text
    , unavailableProviders :: Set Provider
    }
