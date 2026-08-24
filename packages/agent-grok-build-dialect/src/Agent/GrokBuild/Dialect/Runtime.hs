module Agent.GrokBuild.Dialect.Runtime
    ( GrokCodingTools(..)
    , newGrokCodingTools
    ) where

import Agent.GrokBuild.Dialect.Task (GrokSubagentSpecs)
import Agent.GrokBuild.Dialect.Tools
    ( closeGrokSession
    , grokTools
    , newGrokSession
    )
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

data GrokCodingTools = GrokCodingTools
    { grokAppTools :: ![AppTool]
    , grokPlanMode :: !PlanModeEnv
    , grokSuspendGhci :: !(IO ())
    , grokClose :: !(IO ())
    , grokAgentTypes :: !GrokSubagentSpecs
    }

newGrokCodingTools
    :: ToolEnv
    -> Maybe PlanModeHooks
    -> Maybe MultiAgentContext
    -> GrokSubagentSpecs
    -> IO GrokCodingTools
newGrokCodingTools env hooks multi typesRef = do
    resources <- newResourceScope
    flip onException (closeResourceScope resources) do
        plan <- newPlanModeEnv env.toolCwd hooks
        (_, session) <- allocateResource resources
            (newGrokSession env)
            closeGrokSession
        (_, ghci) <- allocateResource resources
            (newGhciSession env)
            closeGhciSession
        pure GrokCodingTools
            { grokAppTools = grokTools session ghci plan multi typesRef
            , grokPlanMode = plan
            , grokSuspendGhci = suspendGhciSession ghci
            , grokClose = closeResourceScope resources
            , grokAgentTypes = typesRef
            }
