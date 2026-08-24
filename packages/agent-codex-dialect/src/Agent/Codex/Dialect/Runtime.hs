module Agent.Codex.Dialect.Runtime
    ( CodexCodingTools(..)
    , newCodexCodingTools
    , newCodexCodingToolsWithGhciHelpers
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
    , newGhciSessionWithHelpers
    )
import Agent.Tools.MultiAgents (MultiAgentContext)
import Agent.Tools.PlanMode (PlanModeEnv, PlanModeHooks, newPlanModeEnv)
import Agent.Tools.Types (AppTool, ToolEnv(..))
import Control.Exception.Safe (onException)
import Data.Text (Text)

data CodexCodingTools = CodexCodingTools
    { codexAppTools :: ![AppTool]
    , codexPlanMode :: !PlanModeEnv
    , codexClose :: !(IO ())
    }

newCodexCodingTools
    :: ToolEnv
    -> Maybe PlanModeHooks
    -> Maybe MultiAgentContext
    -> IO CodexCodingTools
newCodexCodingTools env hooks multi = do
    newCodexCodingToolsWithGhciHelpers env hooks multi []

newCodexCodingToolsWithGhciHelpers
    :: ToolEnv
    -> Maybe PlanModeHooks
    -> Maybe MultiAgentContext
    -> [Text]
    -> IO CodexCodingTools
newCodexCodingToolsWithGhciHelpers env hooks multi ghciHelpers = do
    resources <- newResourceScope
    flip onException (closeResourceScope resources) do
        plan <- newPlanModeEnv env.toolCwd hooks
        (_, shellSession) <- allocateResource resources
            (newCodexShellSession env)
            closeCodexShellSession
        (_, ghci) <- allocateResource resources
            (if null ghciHelpers
                then newGhciSession env
                else newGhciSessionWithHelpers env ghciHelpers)
            closeGhciSession
        tools <- codexTools env shellSession ghci plan multi
        pure CodexCodingTools
            { codexAppTools = tools
            , codexPlanMode = plan
            , codexClose = closeResourceScope resources
            }
