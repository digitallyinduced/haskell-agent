-- | Typed inputs shared by provider startup and the session loop.
module Agent.CLI.Session.Runtime.Types
    ( SessionBackend(..)
    , SessionRequest(..)
    , StartupCancelled(..)
    , StartupFailure(..)
    , StartupRuntime(..)
    ) where

import Agent.CLI.AgentViewport
    ( AgentEntry
    , AgentTarget
    )
import Agent.CLI.Claude (ClaudeSessionRuntimeSlot)
import Agent.CLI.Config (HarnessConfig)
import Agent.CLI.Session.History (LiveConversation)
import Agent.CLI.Btw (BtwBackendFactory)
import Agent.CLI.CodeModeRuntime
    ( CodeModeSessionRuntime
    , CodexCatalogSession
    )
import Agent.CLI.Compaction
    ( AutomaticCompactionBoundary
    , CompactOutcome
    , CompactionInstall
    , OccupancySnapshot
    )
import Agent.CLI.Database.Store (DatabaseScopes)
import Agent.CLI.GatewayClient (GatewayModelAccess)
import Agent.CLI.Interrupt (InterruptState)
import Agent.CLI.ManagedTurn (ManagedTurnRequest)
import Agent.CLI.ModelConfig (ModelCatalog)
import Agent.Connectivity.NetworkPath (NetworkRecovery)
import Agent.OpenAI.Models.Types (ModelInfo)
import Agent.CLI.Options
    ( ApprovalPolicy
    , CliOptions
    )
import Agent.CLI.ProviderTransition (PendingTurn)
import Agent.CLI.Runtime.Orchestration.Types (NativeRunHooks)
import Agent.CLI.PendingInputs (PendingInputs)
import Agent.CLI.Session
    ( LegacySubagentTarget
    , Persistence
    , SessionHandle
    )
import Agent.CLI.SessionState (SessionState)
import Agent.CLI.Subagents.Runtime
    ( SubagentSession
    , SubagentStoreRoot
    )
import Agent.CLI.Terminal (TerminalCapabilities)
import Agent.CLI.TUI.App (FullscreenRuntime)
import Agent.Dialect (Dialect)
import Agent.Error (ApiError)
import Agent.GrokBuild.Dialect.Runtime (GrokRuntimeControl)
import Agent.GrokBuild.Dialect.Task (GrokSubagentSpecs)
import Agent.Loop
    ( Backend
    , ImageAttachment
    , TokenUsage
    , TurnInput
    )
import qualified Agent.MCP as MCP
import qualified Agent.OpenAI.Auth as OpenAI
import Agent.Provider
    ( Credential
    , Provider
    , TokenProvider
    )
import Agent.Responses.Types (ResponseCreateParams)
import Agent.Skills
    ( SkillCatalog
    , SkillInvocation
    )
import Agent.Store.Postgres (Store)
import Agent.Subagents
    ( RootTurnId
    , SubagentId
    )
import Agent.Tools.MultiAgents (MultiAgentContext)
import Agent.Tools.PlanMode (PlanModeEnv)
import Agent.Tools.TaskPlan (TaskPlanEnv)
import Agent.Tools.Types
    ( AppTool
    , ToolEnv
    )
import Control.Concurrent.STM (STM)
import Control.Exception.Safe (Exception(..))
import Data.IORef (IORef)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock
    ( NominalDiffTime
    , UTCTime
    )
import System.IO (Handle)
import System.OsPath (OsPath)

data SessionBackend = SessionBackend
    { backend :: !Backend
    , btwBackend :: !BtwBackendFactory
    , interruptBackend :: !(IO ())
    , resetBackendState :: !(IO ())
    }

data SessionRequest = SessionRequest
    { catalog :: !ModelCatalog
    , gatewayModelsRef :: !(IORef (Maybe GatewayModelAccess))
    , claudeRuntimeSlot :: !ClaudeSessionRuntimeSlot
    , claudeBridgeTools :: ![AppTool]
    , modelInfo :: !(Maybe ModelInfo)
    , connectionId :: !Text
    , gatewayIdentity :: !(Maybe Text)
    , options :: !CliOptions
    , provider :: !Provider
    , dialect :: !Dialect
    , commitAttributionModel :: !Text
    , commitAttributionEffort :: !Text
    , policyRef :: !(IORef ApprovalPolicy)
    , allTools :: ![AppTool]
      -- | Full toggleable tool surface, minus tools unavailable at startup.
      -- Unlike allTools, this list governs provider availability and refreshes.
    , refreshTools :: ![AppTool]
    , recordImageGenerationInputs :: !([ImageAttachment] -> IO ())
    , clearImageGenerationHistory :: !(IO ())
    , suspendGhci :: !(IO ())
    , resetToolSessionTemp :: !(OsPath -> IO ())
    , grokRuntime :: !(Maybe GrokRuntimeControl)
    , mcpRegistrations :: ![MCP.McpToolRegistration]
    , mcpWarnings :: ![Text]
    , mcpInstructions :: ![(Text, Text)]
    , mcpFleet :: !(Maybe MCP.McpFleet)
    , ghciEnabledRef :: !(IORef Bool)
    , bashEnabledRef :: !(IORef Bool)
    , toolEnv :: !ToolEnv
    , planMode :: !PlanModeEnv
    , taskPlan :: !(Maybe TaskPlanEnv)
    , startup :: !StartupRuntime
    , learnAboutUserRequested :: !Bool
    , databaseScopes :: !DatabaseScopes
    , promptRequest :: !(Maybe ManagedTurnRequest)
    , pendingTurn :: !(Maybe PendingTurn)
    , unavailableProviders :: !(Set Provider)
    , startupUnavailable :: !(Maybe (STM ApiError))
    , paramsRef :: !(IORef ResponseCreateParams)
    , conversationRef :: !(IORef LiveConversation)
    , contextOccupancyRef :: !(IORef (Maybe OccupancySnapshot))
    , currentContextWindow :: !(IO (Maybe Int))
    , automaticCompactionRef
        :: !(IORef (Maybe AutomaticCompactionBoundary))
    , needsInitialContext :: !Bool
    , queueInitialContext :: !Bool
    , initialGrokContext :: !(Maybe Text)
    , persist :: !Persistence
    , startupWindowTitle :: !Text
    , projectRoot :: !OsPath
    , home :: !OsPath
    , cwd :: !OsPath
    , tokenProvider :: !(Maybe TokenProvider)
    , openAiPool :: !(Maybe OpenAI.Pool)
    , startupContext :: !(IORef (Maybe Text))
    , automaticCompactionHookRef
        :: !(IORef
            (CompactOutcome -> [TurnInput] -> IO CompactionInstall))
    , skillsRef :: !(IORef SkillCatalog)
    , skillInvocationsRef :: !(IORef [SkillInvocation])
    , escPaused :: !(IORef Bool)
    , interrupt :: !InterruptState
    , multiCtx :: !(Maybe MultiAgentContext)
    , rootTurnRef :: !(IORef (Maybe RootTurnId))
    , subagentSessions :: !(IORef (Map SubagentId SubagentSession))
    , pendingNotices :: !PendingInputs
    , storeRoot :: !SubagentStoreRoot
    , agentTypes :: !GrokSubagentSpecs
    , legacyTarget :: !(Maybe LegacySubagentTarget)
    , usageRef :: !(IORef TokenUsage)
    , accountRef :: !(IORef Text)
    , accountIdRef :: !(IORef Text)
    , selectionRef :: !(IORef Text)
    , accountLabel :: !(Credential -> IO Text)
    , selectAccount :: !(Maybe (Text -> IO (Either ApiError Text)))
    , onPersisted :: !(SessionHandle -> IO ())
    , compactRunner :: !(Maybe Text -> IO (Either Text CompactOutcome))
      -- | Code-mode projection and late-bound nested dispatcher. The runner
      -- uses the projection to rebuild direct schemas when tools are toggled.
    , codeModeRuntime :: !(Maybe CodeModeSessionRuntime)
      -- | Catalog-instruction context for OpenAI models with a catalog entry.
    , codexCatalogSession :: !(Maybe CodexCatalogSession)
    }

data StartupRuntime = StartupRuntime
    { startupToolEnv :: !ToolEnv
    , startupHarnessConfig :: !HarnessConfig
    , startupNetworkRecovery :: !(Maybe NetworkRecovery)
    , startupDatabaseStore :: !Store
    , startupInterrupt :: !InterruptState
    , startupEscPaused :: !(IORef Bool)
    , startupUiRuntimeRef :: !(IORef (Maybe FullscreenRuntime))
    , startupFullscreen :: !(Maybe FullscreenRuntime)
    , startupTerminal :: !TerminalCapabilities
    , startupStdout :: !Handle
    , startupStderr :: !Handle
    , startupBackground :: !Bool
    , startupUseColor :: !Bool
    , startupStderrTty :: !Bool
    , startupStdinTty :: !Bool
    , startupStdoutTty :: !Bool
    , startupFullscreenReused :: !Bool
    , startupAgentSnapshot :: !(IORef (IO (AgentTarget, [AgentEntry])))
    , startupAgentSelect :: !(IORef (AgentTarget -> IO ()))
    , startupRestartEffort :: !(IORef (Text -> IO ()))
    , startupStartedAt :: !UTCTime
    , startupTimings :: !(IORef [(Text, NominalDiffTime)])
    , startupSyntaxLoadDuration :: !(IORef (Maybe NominalDiffTime))
    , startupFinished :: !(IORef Bool)
    , startupSessionState :: !SessionState
    , startupNativeHooks :: !(Maybe NativeRunHooks)
    }

newtype StartupFailure = StartupFailure Text
    deriving (Show)

instance Exception StartupFailure where
    displayException (StartupFailure message) = Text.unpack message

data StartupCancelled = StartupCancelled
    deriving (Show)

instance Exception StartupCancelled
