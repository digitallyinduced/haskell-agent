-- | Inputs supplied by the outer runtime to tool/session startup.
module Agent.CLI.Runtime.Orchestration.Tools.Request
    ( AgentToolsRequest(..)
    ) where

import Agent.CLI.ActiveAccount (ActiveAccountRef)
import Agent.CLI.Auth (LoadedAuth)
import Agent.CLI.CancelWatch (StdinControl)
import Agent.CLI.Database.Store (DatabaseScopes)
import Agent.CLI.GatewayClient (GatewayCredential, GatewayModelAccess)
import Agent.CLI.Interrupt (InterruptState)
import Agent.CLI.ModelConfig (ModelCatalog, ResponsesConnection)
import Agent.CLI.Models (ModelTarget)
import Agent.CLI.Options (CliOptions)
import Agent.CLI.Project (ProjectSettings)
import Agent.CLI.ProviderTransition (PendingTurn, ProviderTransition)
import Agent.CLI.Runtime.Orchestration.Types (AgentProcessRuntime, AgentRunMode)
import Agent.CLI.Runtime.Types (DevResult)
import Agent.CLI.Session (SessionMeta, SessionTurn)
import Agent.CLI.Session.Runtime.Types (StartupRuntime)
import Agent.CLI.SessionLock (SessionLock)
import Agent.CLI.TUI.App (FullscreenRuntime)
import Agent.Error (ApiError)
import qualified Agent.MCP as MCP
import Agent.Provider (Credential, Provider, TokenProvider)
import Agent.Skills (SkillCatalog)
import Agent.Tools.Types (ToolEnv)
import Data.IORef (IORef)
import Data.Set (Set)
import Data.Text (Text)
import System.IO (Handle)
import System.OsPath (OsPath)

data AgentToolsRequest windowTitleResult = AgentToolsRequest
    { runAgentChild :: AgentRunMode -> CliOptions -> IO DevResult
    , loaded :: LoadedAuth
    , connectedGateway :: Maybe GatewayCredential
    , learnAboutUserRequested :: Bool
    , customBearerToken :: Maybe Text
    , activeAccountRef :: ActiveAccountRef
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
    , stdinControl :: StdinControl
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
