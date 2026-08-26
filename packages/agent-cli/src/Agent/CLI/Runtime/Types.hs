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

import Agent.CLI.ProviderTransition (ProviderTransition)
import Agent.CLI.Session.Runtime.Types
    ( StartupCancelled(..)
    , StartupFailure(..)
    , StartupRuntime(..)
    )
import Agent.CLI.TUI.App (FullscreenRuntime)
import Agent.Error (ApiError)
import Agent.Provider (Provider)
import Agent.ReasoningEffort (ReasoningEffort)
import Data.Text (Text)
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
    | RunSwitchWorktree OsPath Provider Text ReasoningEffort
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
