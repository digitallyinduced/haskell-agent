-- | Mutable session state shared by the REPL, one-shot turns, and plan mode.
module Agent.CLI.SessionEnv
    ( SessionEnv(..)
    ) where

import Agent.CLI.Interrupt (InterruptState)
import Agent.CLI.AgentViewport (AgentViewportEnv)
import Agent.CLI.Btw (BtwBackendFactory)
import Agent.CLI.Options (ApprovalPolicy)
import Agent.CLI.Render (RenderConfig)
import Agent.CLI.Session (Persistence, SessionHandle)
import Agent.CLI.SessionTitle (SessionTitleManager)
import Agent.CLI.Terminal (TerminalCapabilities)
import Agent.CLI.TUI.App (FullscreenRuntime)
import Agent.Loop (ImageAttachment, LoopConfig, TokenUsage)
import qualified Agent.OpenAI.Auth as OpenAI
import Agent.Responses.Types (ResponseCreateParams, ResponseItem)
import Agent.OsPath (OsPath)
import Agent.Provider (Provider, TokenProvider)
import Agent.Skills (SkillCatalog, SkillInvocation)
import Agent.Subagents (RootTurnId)
import Agent.Tools.PlanMode (PlanModeEnv)
import Data.IORef (IORef)
import Data.Text (Text)

data SessionEnv = SessionEnv
    { sessionLoop :: !LoopConfig
    , sessionBtwBackend :: !BtwBackendFactory
    , sessionRender :: !RenderConfig
    , sessionProvider :: !Provider
    , sessionUnavailableProviders :: !(IORef [Provider])
    , sessionPrevious :: !(IORef (Maybe Text))
    , sessionPrinted :: !(IORef Bool)
    , sessionParams :: !(IORef ResponseCreateParams)
    , sessionPolicy :: !(IORef ApprovalPolicy)
    , sessionTranscript :: !(IORef [ResponseItem])
    , sessionPersist :: !Persistence
    , sessionTitleManager :: !SessionTitleManager
    , sessionTitleTurnCount :: !(IORef Int)
    , sessionPlanMode :: !PlanModeEnv
    , sessionProjectRoot :: !OsPath
    , sessionCwd :: !OsPath
    , sessionHome :: !OsPath
    , sessionTokenProvider :: !(Maybe TokenProvider)
    , sessionOpenAiPool :: !(Maybe OpenAI.Pool)
    , sessionStartupContext :: !(IORef (Maybe Text))
    , sessionSkills :: !(IORef SkillCatalog)
    , sessionSkillInvocations :: !(IORef [SkillInvocation])
    , sessionRefreshSkills :: !(Bool -> IO ())
    , sessionEscPaused :: !(IORef Bool)
    , sessionAttachments :: !(IORef [ImageAttachment])
    , sessionPreviewId :: !(IORef Int)
    , sessionInterrupt :: !InterruptState
    , sessionStoreRoot :: !(IORef (Maybe OsPath))
    , sessionUsage :: !(IORef TokenUsage)
    , sessionLastAssistant :: !(IORef (Maybe Text))
    , sessionTerminal :: !TerminalCapabilities
    , sessionFullscreen :: !(Maybe FullscreenRuntime)
    , sessionAgentViewport :: !(Maybe AgentViewportEnv)
    , sessionBeginSubagentTurn :: !(IO (Maybe RootTurnId))
    , sessionFinishSubagentTurn :: !(Maybe RootTurnId -> IO ())
    , sessionAbortSubagentTurn :: !(Maybe RootTurnId -> IO ())
    , sessionOnPersisted :: !(SessionHandle -> IO ())
    , sessionReset :: !(IO ())
    }
