-- | Mutable session state shared by the REPL, one-shot turns, and plan mode.
module Agent.CLI.SessionEnv
    ( SessionEnv(..)
    ) where

import Agent.CLI.Interrupt (InterruptState)
import Agent.CLI.AgentViewport (AgentViewportEnv)
import Agent.CLI.GatewayClient (GatewayModelAccess)
import Agent.CLI.ModelConfig (ModelCatalog)
import Agent.CLI.Btw (BtwBackendFactory)
import Agent.CLI.Recap (RecapRequest)
import Agent.CLI.Command (ShellMode)
import Agent.CLI.Compaction
    ( AutomaticCompactionBoundary
    , CompactOutcome
    , OccupancySnapshot
    )
import Agent.CLI.Options (ApprovalPolicy)
import Agent.CLI.Render (RenderConfig)
import Agent.CLI.Session (Persistence, SessionHandle)
import Agent.CLI.Session.History (LiveConversation)
import Agent.CLI.SessionTitle (SessionTitleManager)
import Agent.CLI.Terminal (TerminalCapabilities)
import Agent.CLI.SteeringInputs (SteeringInputs)
import Agent.CLI.TUI.App (FullscreenRuntime)
import Agent.Dialect (Dialect)
import Agent.Error (ApiError)
import Agent.GrokBuild.Dialect.Runtime (GrokRuntimeControl)
import Agent.Loop (ImageAttachment, LoopConfig, TokenUsage)
import Agent.MCP (McpFleet, McpToolRegistration)
import qualified Agent.OpenAI.Auth as OpenAI
import Agent.Responses.Types (ResponseCreateParams)
import Agent.OpenAI.Models.Types (ModelInfo)
import System.OsPath (OsPath)
import Agent.Provider (Credential, Provider, TokenProvider)
import Agent.CLI.ProviderTransition (PendingTurn)
import Agent.Skills (SkillCatalog, SkillInvocation)
import Agent.Subagents (RootTurnId)
import Agent.Tools.PlanMode (PlanModeEnv)
import Agent.Store.Postgres.Connection (StorePool)
import Data.IORef (IORef)
import Data.Set (Set)
import Data.Text (Text)
import Control.Concurrent.STM (STM)

data SessionEnv = SessionEnv
    { sessionLoop :: !LoopConfig
    , sessionSteeringInputs :: !SteeringInputs
    , sessionModelInfo :: !(Maybe ModelInfo)
    , sessionBtwBackend :: !BtwBackendFactory
    , sessionQueueRecap :: !(RecapRequest -> IO ())
    , sessionCompact :: !(Maybe Text -> IO (Either Text CompactOutcome))
    , sessionRender :: !RenderConfig
    , sessionProvider :: !Provider
    , sessionConnection :: !Text
    , sessionGatewayIdentity :: !(Maybe Text)
    , sessionModelCatalog :: !ModelCatalog
    , sessionGatewayModels :: !(IORef (Maybe GatewayModelAccess))
    , sessionDialect :: !Dialect
    , sessionRecordImageGenerationInputs :: !([ImageAttachment] -> IO ())
    , sessionUnavailableProviders :: !(IORef (Set Provider))
    , sessionStartupUnavailable :: !(IORef (Maybe (STM ApiError)))
    , sessionConversation :: !(IORef LiveConversation)
    , sessionAutomaticCompaction
        :: !(IORef (Maybe AutomaticCompactionBoundary))
    , sessionParams :: !(IORef ResponseCreateParams)
    , sessionContextOccupancy :: !(IORef (Maybe OccupancySnapshot))
    , sessionContextWindow :: !(IO (Maybe Int))
    , sessionPolicy :: !(IORef ApprovalPolicy)
    , sessionPersist :: !Persistence
    , sessionDatabasePool :: !StorePool
    , sessionTitleManager :: !SessionTitleManager
    , sessionTitleTurnCount :: !(IORef Int)
    , sessionPlanMode :: !PlanModeEnv
    , sessionProjectRoot :: !OsPath
    , sessionCwd :: !OsPath
    , sessionHome :: !OsPath
    , sessionMcpRegistrations :: ![McpToolRegistration]
    , sessionMcpWarnings :: ![Text]
    , sessionMcpFleet :: !(Maybe McpFleet)
    , sessionSetTempDir :: !(OsPath -> IO ())
    , sessionTokenProvider :: !(Maybe TokenProvider)
    , sessionOpenAiPool :: !(Maybe OpenAI.Pool)
    , sessionStartupContext :: !(IORef (Maybe Text))
    , sessionGrokFirstTurnContext :: !(IORef (Maybe Text))
    , sessionSkills :: !(IORef SkillCatalog)
    , sessionSkillInvocations :: !(IORef [SkillInvocation])
    , sessionRefreshSkills :: !(Bool -> IO ())
    , sessionActiveToolNames :: !(IO [Text])
    , sessionGrokRuntime :: !(Maybe GrokRuntimeControl)
    , sessionShellMode :: !(IO ShellMode)
    , sessionSetShellMode :: !(ShellMode -> IO Text)
    , sessionBackground :: !Bool
    , sessionEscPaused :: !(IORef Bool)
    , sessionDraft :: !(IORef Text)
    , sessionPreviewId :: !(IORef Int)
    , sessionInterrupt :: !InterruptState
    , sessionRestartEffort :: !(IORef (Maybe Text))
    , sessionLastFailedTurn :: !(IORef (Maybe PendingTurn))
    , sessionStoreRoot :: !(IORef (Maybe OsPath))
    , sessionUsage :: !(IORef TokenUsage)
    , sessionAccount :: !(IORef Text)
    , sessionAccountId :: !(IORef Text)
    , sessionAccountSelectionId :: !(IORef Text)
    , sessionAccountLabel :: !(Credential -> IO Text)
    , sessionSelectAccount
        :: !(Maybe (Text -> IO (Either ApiError Text)))
    , sessionLastAssistant :: !(IORef (Maybe Text))
    , sessionTerminal :: !TerminalCapabilities
    , sessionFullscreen :: !(Maybe FullscreenRuntime)
    , sessionSetWindowTitle :: !(Text -> IO ())
    , sessionBeginWindowTitleBusy :: !(IO ())
    , sessionEndWindowTitleBusy :: !(IO ())
    , sessionBeginTurnActivity :: !(IO ())
    , sessionEndTurnActivity :: !(IO ())
    , sessionAgentViewport :: !(Maybe AgentViewportEnv)
    , sessionBeginSubagentTurn :: !(IO (Maybe RootTurnId))
    , sessionFinishSubagentTurn :: !(Maybe RootTurnId -> IO ())
    , sessionAbortSubagentTurn :: !(Maybe RootTurnId -> IO ())
    , sessionConcurrentLimit :: !(IO Int)
    , sessionSetConcurrentLimit :: !(Int -> IO Text)
    , sessionOnPersisted :: !(SessionHandle -> IO ())
    , sessionReset :: !(IO ())
    }
