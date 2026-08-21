-- | Mutable session state shared by the REPL, one-shot turns, and plan mode.
module Agent.CLI.SessionEnv
    ( SessionEnv(..)
    ) where

import Agent.CLI.Interrupt (InterruptState)
import Agent.CLI.AgentViewport (AgentViewportEnv)
import Agent.CLI.Btw (BtwBackendFactory)
import Agent.CLI.Options (ApprovalPolicy)
import Agent.CLI.Render (RenderConfig)
import Agent.CLI.Session (SessionCreate, SessionHandle)
import Agent.Loop (ImageAttachment, LoopConfig, TokenUsage)
import Agent.OpenAI.Responses.Types (ResponseCreateParams, ResponseItem)
import Agent.OsPath (OsPath)
import Agent.Provider (Provider, TokenProvider)
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
    , sessionPersist :: !(Maybe (IORef (Either SessionCreate SessionHandle)))
    , sessionPlanMode :: !PlanModeEnv
    , sessionProjectRoot :: !OsPath
    , sessionHome :: !OsPath
    , sessionTokenProvider :: !(Maybe TokenProvider)
    , sessionAgentsContext :: !(IORef (Maybe Text))
    , sessionEscPaused :: !(IORef Bool)
    , sessionAttachments :: !(IORef [ImageAttachment])
    , sessionPreviewId :: !(IORef Int)
    , sessionInterrupt :: !InterruptState
    , sessionStoreRoot :: !(IORef (Maybe OsPath))
    , sessionUsage :: !(IORef TokenUsage)
    , sessionAgentViewport :: !(Maybe AgentViewportEnv)
    , sessionAbortSubagents :: !(IO ())
    , sessionOnPersisted :: !(SessionHandle -> IO ())
    , sessionReset :: !(IO ())
    }
