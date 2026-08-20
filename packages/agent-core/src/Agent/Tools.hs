-- | Registered application tools: schema fragments plus a dispatch handler.
--
-- Provider-specific surfaces live in 'Agent.Tools.Grok' and
-- 'Agent.Tools.Codex'. OpenRouter reuses the Grok JSON function tools.
-- Shared filesystem and process helpers are in 'Agent.Tools.IO'.
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
import Agent.Tools.Grok (closeGrokSession, grokTools, newGrokSession)
import Agent.Tools.Grok.Ghci (closeGhciSession, newGhciSession)
import Agent.Tools.Types

-- | Tools advertised for a model vendor. Surfaces are never mixed.
-- Grok/OpenRouter share one persistent shell and GHCi session for the process
-- lifetime. The second action closes both; run it in 'finally'.
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
        tools <- codexTools env
        pure (tools, pure ())
