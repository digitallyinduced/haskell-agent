-- | Mutable session state shared by the REPL, one-shot turns, and plan mode.
module Agent.CLI.SessionEnv
    ( PreparedWorkspaceEnvironment(..)
    , SessionEnv(..)
    ) where

import Agent.CLI.Session.Request
    ( SessionRequestState
    )
import Agent.CLI.ActiveAccount (ActiveAccountRef)
import Agent.CLI.CancelWatch (StdinControl)
import Agent.CLI.Interrupt (InterruptState)
import Agent.CLI.AgentViewport (AgentViewportEnv)
import Agent.CLI.GatewayClient (GatewayModelAccess)
import Agent.CLI.ModelConfig (ModelCatalog)
import Agent.CLI.Btw (BtwBackendFactory)
import Agent.CLI.Recap (RecapRequest)
import Agent.CLI.Command (ShellMode)
import Agent.CLI.Compaction
    ( CompactOutcome
    , OccupancySnapshot
    )
import Agent.CLI.Options (ApprovalPolicy)
import Agent.CLI.Render (RenderConfig)
import Agent.CLI.Session (Persistence, SessionHandle)
import Agent.CLI.Session.Observation (SessionObservationPublisher)
import Agent.Runtime.SessionState (SessionState)
import Agent.CLI.Session.Workspace (WorkspaceContext)
import Agent.CLI.SessionTitle (SessionTitleManager)
import Agent.CLI.Terminal (TerminalCapabilities)
import Agent.CLI.SteeringInputs (SteeringInputs)
import Agent.CLI.TUI.App (FullscreenRuntime)
import Agent.Dialect (Dialect)
import Agent.Error (ApiError)
import Agent.GrokBuild.Dialect.Runtime (GrokRuntimeControl)
import Agent.Loop (ImageAttachment, LoopConfig)
import Agent.MCP (McpFleet, McpToolRegistration)
import qualified Agent.OpenAI.Auth as OpenAI
import Agent.OpenAI.Models.Types (ModelInfo)
import System.OsPath (OsPath)
import Agent.Provider (Credential, Provider, TokenProvider)
import Agent.CLI.ProviderTransition (PendingTurn)
import Agent.Skills (SkillCatalog, SkillInvocation)
import Agent.Subagents (RootTurnId)
import Agent.Tools.PlanMode (PlanModeEnv)
import Agent.Tools.TaskPlan (TaskPlanEnv)
import Agent.Store.Postgres.Connection (StorePool)
import Data.IORef (IORef)
import Data.Set (Set)
import Data.Text (Text)
import Control.Concurrent.STM (STM)

data PreparedWorkspaceEnvironment = PreparedWorkspaceEnvironment
    { preparedOperatingSystem :: !Text
    , preparedShell :: !Text
    }

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
    , sessionState :: !SessionState
    , sessionParams :: !(SessionRequestState)
    , sessionContextOccupancy :: !(IORef (Maybe OccupancySnapshot))
    , sessionContextWindow :: !(IO (Maybe Int))
    , sessionPolicy :: !(IORef ApprovalPolicy)
    , sessionPersist :: !Persistence
    -- | Scoped publisher for the current CLI turn, shared with the normalized
    -- presentation callback. Nested follow-ups reuse the same service.
    , sessionObservationPublisher :: !(IORef (Maybe SessionObservationPublisher))
    , sessionObservationEnabled :: !Bool
    , sessionDatabasePool :: !StorePool
    , sessionTitleManager :: !SessionTitleManager
    , sessionTitleTurnCount :: !(IORef Int)
    , sessionPlanMode :: !PlanModeEnv
    , sessionTaskPlan :: !(Maybe TaskPlanEnv)
    , sessionWorkspace :: !WorkspaceContext
    , sessionProviderFallback :: !Bool
    -- | Prepared environment facts avoid inspecting the host workspace.
    -- 'Nothing' requests ordinary local discovery.
    , sessionPreparedWorkspaceEnvironment
        :: !(Maybe PreparedWorkspaceEnvironment)
    , sessionMcpRegistrations :: ![McpToolRegistration]
    , sessionMcpWarnings :: ![Text]
    , sessionMcpFleet :: !(Maybe McpFleet)
    , sessionSetTempDir :: !(OsPath -> IO ())
    , sessionTokenProvider :: !(Maybe TokenProvider)
    , sessionOpenAiPool :: !(Maybe OpenAI.Pool)
    , sessionSkills :: !(IORef SkillCatalog)
    , sessionSkillInvocations :: !(IORef [SkillInvocation])
    , sessionRefreshSkills :: !(Bool -> IO ())
    , sessionActiveToolNames :: !(IO [Text])
    , sessionGrokRuntime :: !(Maybe GrokRuntimeControl)
    , sessionShellMode :: !(IO ShellMode)
    , sessionSetShellMode :: !(ShellMode -> IO Text)
    , sessionComputerUseEnabled :: !(IO Bool)
    , sessionSetComputerUseEnabled :: !(Bool -> IO Text)
    , sessionRefreshRequestParams :: !(IO ())
    , sessionBackground :: !Bool
    , sessionStdinControl :: !StdinControl
    , sessionDraft :: !(IORef Text)
    , sessionPreviewId :: !(IORef Int)
    , sessionInterrupt :: !InterruptState
    , sessionRestartEffort :: !(IORef (Maybe Text))
    , sessionLastFailedTurn :: !(IORef (Maybe PendingTurn))
    , sessionStoreRoot :: !(IORef (Maybe OsPath))
    , sessionAccount :: !ActiveAccountRef
    , sessionAccountLabel :: !(Credential -> IO Text)
    , sessionSelectAccount
        :: !(Maybe (Text -> IO (Either ApiError Text)))
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
