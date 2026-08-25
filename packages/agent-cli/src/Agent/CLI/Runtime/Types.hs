-- | Results shared by the CLI lifecycle, provider runtime, and session loop.
module Agent.CLI.Runtime.Types
    ( DevResult(..)
    , PendingTurnPresentation(..)
    , PreparedAgent(..)
    , RunResult(..)
    , StartupCancelled(..)
    , StartupFailure(..)
    , StartupRuntime(..)
    ) where

import Agent.CLI.AgentViewport
    ( AgentEntry
    , AgentTarget
    )
import Agent.CLI.Interrupt (InterruptState)
import Agent.CLI.ProviderTransition (ProviderTransition)
import Agent.CLI.SessionState (SessionState)
import Agent.CLI.Terminal (TerminalCapabilities)
import Agent.CLI.TUI.App (FullscreenRuntime)
import Agent.Error (ApiError)
import Agent.Provider (Provider)
import Agent.Store.Postgres (Store)
import Agent.Tools.Types (ToolEnv)
import Control.Exception.Safe (Exception)
import Data.IORef (IORef)
import Data.Text (Text)
import Data.Time.Clock
    ( NominalDiffTime
    , UTCTime
    )
import System.OsPath (OsPath)

-- | How the GHCi-driven agent REPL finished.
data DevResult
    = DevQuit
    | DevReload Text
    deriving (Eq, Show)

data RunResult
    = RunQuit
    | RunRestart Text
    | RunReload Text
    | RunSwitchProvider ProviderTransition
    | RunProviderStartFailed ApiError
    | RunResumeSession Text
      -- ^ Persisted session id. Consumed after the current provider-specific
      -- backend shuts down before starting the selected session.
    | RunSwitchWorktree OsPath Provider Text Text
      -- ^ Fresh worktree path. Starts a new session after the current backend
      -- and fullscreen UI have shut down, retaining provider, model, and effort.

data PreparedAgent = PreparedAgent
    { preparedFullscreen :: !(Maybe FullscreenRuntime)
    , preparedRun :: !(IO RunResult)
    }

data PendingTurnPresentation
    = SubmitPendingTurn
    | RestartPendingTurn
    | ContinuePendingTurn

data StartupRuntime = StartupRuntime
    { startupToolEnv :: !ToolEnv
    , startupDatabaseStore :: !Store
    , startupInterrupt :: !InterruptState
    , startupEscPaused :: !(IORef Bool)
    , startupUiRuntimeRef :: !(IORef (Maybe FullscreenRuntime))
    , startupFullscreen :: !(Maybe FullscreenRuntime)
    , startupTerminal :: !TerminalCapabilities
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
    }

newtype StartupFailure = StartupFailure String
    deriving (Show)

instance Exception StartupFailure

data StartupCancelled = StartupCancelled
    deriving (Show)

instance Exception StartupCancelled
