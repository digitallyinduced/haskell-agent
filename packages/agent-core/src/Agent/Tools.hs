-- | Registered application tools: schema fragments plus a dispatch handler.
--
-- Provider-specific surfaces live in 'Agent.Tools.Grok' and
-- 'Agent.Tools.Codex'. OpenRouter reuses the Grok JSON function tools.
-- Shared filesystem, process, and GHCi helpers are in 'Agent.Tools.IO' and
-- 'Agent.Tools.Ghci'.
module Agent.Tools
    ( AppTool(..)
    , AppToolKind(..)
    , ToolEnv(..)
    , defaultToolEnv
    , appToolHandlers
    , CodingTools(..)
    , codingToolsFor
    , codingToolsForWithTypes
    , filterChildGrokTools
    ) where

import Agent.Provider (Provider(..))
import Agent.Subagents (SubagentId)
import Agent.Tools.Codex (codexTools)
import Agent.Tools.Ghci (closeGhciSession, newGhciSession)
import Agent.Tools.Grok (closeGrokSession, filterGrokToolsForType, grokTools, newGrokSession)
import Agent.Tools.MultiAgents (MultiAgentContext)
import Agent.Tools.PlanMode (PlanModeEnv, PlanModeHooks, newPlanModeEnv)
import Agent.Tools.Types
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

-- | Tools plus the session plan-mode env (shared across providers).
data CodingTools = CodingTools
    { codingAppTools :: ![AppTool]
    , codingPlanMode :: !PlanModeEnv
    , codingClose :: !(IO ())
      -- | Grok/OpenRouter: maps spawned agent ids to subagent_type.
    , codingAgentTypes :: !(IORef (Map SubagentId Text))
    }

-- | Tools advertised for a model vendor. Surfaces are never mixed.
-- Every provider gets a persistent GHCi session for 'run_ghci'.
-- Grok/OpenRouter also keep a persistent shell session. 'codingClose'
-- closes owned sessions; run it in 'finally'.
codingToolsFor
    :: Provider
    -> ToolEnv
    -> Maybe PlanModeHooks
    -> Maybe MultiAgentContext
    -> IO CodingTools
codingToolsFor provider env hooks multi = do
    typesRef <- newIORef Map.empty
    codingToolsForWithTypes provider env hooks multi typesRef

-- | Same as 'codingToolsFor', but reuses an existing agent-type map so the
-- host can wire resume-from-disk before tools are built.
codingToolsForWithTypes
    :: Provider
    -> ToolEnv
    -> Maybe PlanModeHooks
    -> Maybe MultiAgentContext
    -> IORef (Map SubagentId Text)
    -> IO CodingTools
codingToolsForWithTypes provider env hooks multi typesRef = do
    plan <- newPlanModeEnv env.toolCwd hooks
    case provider of
        XAIProvider -> do
            session <- newGrokSession env
            ghci <- newGhciSession env
            pure CodingTools
                { codingAppTools = grokTools session ghci plan multi typesRef
                , codingPlanMode = plan
                , codingClose = closeGrokSession session >> closeGhciSession ghci
                , codingAgentTypes = typesRef
                }
        OpenRouterProvider -> do
            session <- newGrokSession env
            ghci <- newGhciSession env
            pure CodingTools
                { codingAppTools = grokTools session ghci plan multi typesRef
                , codingPlanMode = plan
                , codingClose = closeGrokSession session >> closeGhciSession ghci
                , codingAgentTypes = typesRef
                }
        OpenAIProvider -> do
            ghci <- newGhciSession env
            tools <- codexTools env ghci plan multi
            pure CodingTools
                { codingAppTools = tools
                , codingPlanMode = plan
                , codingClose = closeGhciSession ghci
                , codingAgentTypes = typesRef
                }

-- | Re-export for CLI child runners.
filterChildGrokTools :: Text -> [AppTool] -> [AppTool]
filterChildGrokTools = filterGrokToolsForType

