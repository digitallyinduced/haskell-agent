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
    ) where

import Agent.Provider (Provider(..))
import Agent.Tools.Codex (codexTools)
import Agent.Tools.Ghci (closeGhciSession, newGhciSession)
import Agent.Tools.Grok (closeGrokSession, grokTools, newGrokSession)
import Agent.Tools.MultiAgents (MultiAgentContext)
import Agent.Tools.PlanMode (PlanModeEnv, PlanModeHooks, newPlanModeEnv)
import Agent.Tools.Types

-- | Tools plus the session plan-mode env (shared across providers).
data CodingTools = CodingTools
    { codingAppTools :: ![AppTool]
    , codingPlanMode :: !PlanModeEnv
    , codingClose :: !(IO ())
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
    plan <- newPlanModeEnv env.toolCwd hooks
    case provider of
        XAIProvider -> do
            session <- newGrokSession env
            ghci <- newGhciSession env
            pure CodingTools
                { codingAppTools = grokTools session ghci plan
                , codingPlanMode = plan
                , codingClose = closeGrokSession session >> closeGhciSession ghci
                }
        OpenRouterProvider -> do
            session <- newGrokSession env
            ghci <- newGhciSession env
            pure CodingTools
                { codingAppTools = grokTools session ghci plan
                , codingPlanMode = plan
                , codingClose = closeGrokSession session >> closeGhciSession ghci
                }
        OpenAIProvider -> do
            ghci <- newGhciSession env
            tools <- codexTools env ghci plan multi
            pure CodingTools
                { codingAppTools = tools
                , codingPlanMode = plan
                , codingClose = closeGhciSession ghci
                }
