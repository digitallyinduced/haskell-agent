module Agent.Codex.Dialect.Runtime
    ( CodexCodingTools(..)
    , newCodexCodingTools
    ) where

import Agent.Codex.Dialect.Shell
    ( closeCodexShellSession
    , newCodexShellSession
    )
import Agent.Codex.Dialect.Tools (codexTools)
import Agent.ResourceScope
    ( allocateResource
    , closeResourceScope
    , newResourceScope
    )
import Agent.Tools.Ghci
    ( closeGhciSession
    , newGhciSession
    , suspendGhciSession
    )
import Agent.Tools.MultiAgents (MultiAgentContext)
import Agent.Tools.PlanMode (PlanModeEnv, PlanModeHooks, newPlanModeEnv)
import Agent.Tools.Types (AppTool, ToolEnv(..))
import Control.Exception.Safe (onException)

data CodexCodingTools = CodexCodingTools
    { codexAppTools :: ![AppTool]
    , codexPlanMode :: !PlanModeEnv
    , codexSuspendGhci :: !(IO ())
    , codexClose :: !(IO ())
    }

newCodexCodingTools
    :: ToolEnv
    -> Maybe PlanModeHooks
    -> Maybe MultiAgentContext
    -> IO CodexCodingTools
newCodexCodingTools env hooks multi = do
    resources <- newResourceScope
    flip onException (closeResourceScope resources) do
        plan <- newPlanModeEnv env.toolCwd hooks
        (_, shellSession) <- allocateResource resources
            (newCodexShellSession env)
            closeCodexShellSession
        (_, ghci) <- allocateResource resources
            (newGhciSession env)
            closeGhciSession
        tools <- codexTools env shellSession ghci plan multi
        pure CodexCodingTools
            { codexAppTools = tools
            , codexPlanMode = plan
            , codexSuspendGhci = suspendGhciSession ghci
            , codexClose = closeResourceScope resources
            }
