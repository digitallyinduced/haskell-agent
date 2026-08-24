module Agent.GrokBuild.Dialect.Runtime
    ( GrokCodingTools(..)
    , newGrokCodingTools
    , newGrokCodingToolsWithGhciHelpers
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
    , newGhciSessionWithHelpers
    )
import Agent.Tools.MultiAgents (MultiAgentContext)
import Agent.Tools.PlanMode (PlanModeEnv, PlanModeHooks, newPlanModeEnv)
import Agent.Tools.Types (AppTool, ToolEnv(..))
import Control.Exception.Safe (onException)
import Data.Text (Text)

data GrokCodingTools = GrokCodingTools
    { grokAppTools :: ![AppTool]
    , grokPlanMode :: !PlanModeEnv
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
    newGrokCodingToolsWithGhciHelpers env hooks multi typesRef []

newGrokCodingToolsWithGhciHelpers
    :: ToolEnv
    -> Maybe PlanModeHooks
    -> Maybe MultiAgentContext
    -> GrokSubagentSpecs
    -> [Text]
    -> IO GrokCodingTools
newGrokCodingToolsWithGhciHelpers env hooks multi typesRef ghciHelpers = do
    resources <- newResourceScope
    flip onException (closeResourceScope resources) do
        plan <- newPlanModeEnv env.toolCwd hooks
        (_, session) <- allocateResource resources
            (newGrokSession env)
            closeGrokSession
        (_, ghci) <- allocateResource resources
            (if null ghciHelpers
                then newGhciSession env
                else newGhciSessionWithHelpers env ghciHelpers)
            closeGhciSession
        pure GrokCodingTools
            { grokAppTools = grokTools session ghci plan multi typesRef
            , grokPlanMode = plan
            , grokClose = closeResourceScope resources
            , grokAgentTypes = typesRef
            }
