-- | Mutable session state shared by the REPL, one-shot turns, and plan mode.
module Agent.CLI.SessionEnv
    ( SessionEnv(..)
    ) where

import Agent.CLI.Interrupt (InterruptState)
import Agent.CLI.AgentViewport (AgentViewportEnv)
import Agent.CLI.ModelConfig (ModelCatalog)
import Agent.CLI.Btw (BtwBackendFactory)
import Agent.CLI.Compaction (CompactOutcome)
import Agent.CLI.Options (ApprovalPolicy)
import Agent.CLI.Render (RenderConfig)
import Agent.CLI.Session (Persistence, SessionHandle)
import Agent.CLI.SessionState (SessionState)
import Agent.CLI.SessionTitle (SessionTitleManager)
import Agent.CLI.Terminal (TerminalCapabilities)
import Agent.CLI.TUI.App (FullscreenRuntime)
import Agent.Dialect (Dialect)
import Agent.Error (ApiError)
import Agent.Loop (ImageAttachment, LoopConfig)
import qualified Agent.OpenAI.Auth as OpenAI
import Agent.Responses.Types (ResponseCreateParams)
import System.OsPath (OsPath)
import Agent.Provider (Credential, Provider, TokenProvider)
import Agent.Subagents (RootTurnId)
import Agent.Tools.PlanMode (PlanModeEnv)
import Data.IORef (IORef)
import Data.Text (Text)
import Control.Concurrent.STM (STM)

data SessionEnv = SessionEnv
    { sessionLoop :: !LoopConfig
    , sessionBtwBackend :: !BtwBackendFactory
    , sessionCompact :: !(Maybe Text -> IO (Either Text CompactOutcome))
    , sessionRender :: !RenderConfig
    , sessionProvider :: !Provider
    , sessionConnection :: !Text
    , sessionModelCatalog :: !ModelCatalog
    , sessionDialect :: !Dialect
    , sessionUnavailableProviders :: !(IORef [Provider])
    , sessionStartupUnavailable :: !(IORef (Maybe (STM ApiError)))
    , sessionState :: !(IORef SessionState)
    , sessionPrinted :: !(IORef Bool)
    , sessionParams :: !(IORef ResponseCreateParams)
    , sessionPolicy :: !(IORef ApprovalPolicy)
    , sessionPersist :: !Persistence
    , sessionTitleManager :: !SessionTitleManager
    , sessionTitleTurnCount :: !(IORef Int)
    , sessionPlanMode :: !PlanModeEnv
    , sessionProjectRoot :: !OsPath
    , sessionCwd :: !OsPath
    , sessionHome :: !OsPath
    , sessionSetTempDir :: !(OsPath -> IO ())
    , sessionTokenProvider :: !(Maybe TokenProvider)
    , sessionOpenAiPool :: !(Maybe OpenAI.Pool)
    , sessionRefreshSkills :: !(Bool -> IO ())
    , sessionEscPaused :: !(IORef Bool)
    , sessionAttachments :: !(IORef [ImageAttachment])
    , sessionPreviewId :: !(IORef Int)
    , sessionInterrupt :: !InterruptState
    , sessionRestartEffort :: !(IORef (Maybe Text))
    , sessionStoreRoot :: !(IORef (Maybe OsPath))
    , sessionAccount :: !(IORef Text)
    , sessionAccountId :: !(IORef Text)
    , sessionAccountSelectionId :: !(IORef Text)
    , sessionAccountLabel :: !(Credential -> IO Text)
    , sessionSelectAccount
        :: !(Maybe (Text -> IO (Either ApiError Text)))
    , sessionTerminal :: !TerminalCapabilities
    , sessionFullscreen :: !(Maybe FullscreenRuntime)
    , sessionSetWindowTitle :: !(Text -> IO ())
    , sessionAgentViewport :: !(Maybe AgentViewportEnv)
    , sessionBeginSubagentTurn :: !(IO (Maybe RootTurnId))
    , sessionFinishSubagentTurn :: !(Maybe RootTurnId -> IO ())
    , sessionAbortSubagentTurn :: !(Maybe RootTurnId -> IO ())
    , sessionOnPersisted :: !(SessionHandle -> IO ())
    , sessionReset :: !(IO ())
    }
