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
    , codingToolsFor
    ) where

import Agent.Provider (Provider(..))
import Agent.Tools.Codex (codexTools)
import Agent.Tools.Ghci (closeGhciSession, newGhciSession)
import Agent.Tools.Grok (closeGrokSession, grokTools, newGrokSession)
import Agent.Tools.Types

-- | Tools advertised for a model vendor. Surfaces are never mixed.
-- Every provider gets a persistent GHCi session for 'run_ghci'.
-- Grok/OpenRouter also keep a persistent shell session. The second action
-- closes owned sessions; run it in 'finally'.
codingToolsFor :: Provider -> ToolEnv -> IO ([AppTool], IO ())
codingToolsFor provider env = case provider of
    XAIProvider -> do
        session <- newGrokSession env
        ghci <- newGhciSession env
        pure
            ( grokTools session ghci
            , closeGrokSession session >> closeGhciSession ghci
            )
    OpenRouterProvider -> do
        session <- newGrokSession env
        ghci <- newGhciSession env
        pure
            ( grokTools session ghci
            , closeGrokSession session >> closeGhciSession ghci
            )
    OpenAIProvider -> do
        ghci <- newGhciSession env
        tools <- codexTools env ghci
        pure (tools, closeGhciSession ghci)
