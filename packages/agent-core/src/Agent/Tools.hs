-- | Registered application tools: schema fragments plus a dispatch handler.
--
-- Provider-specific surfaces live in 'Agent.Tools.Grok' and (later)
-- 'Agent.Tools.Codex'. Shared filesystem and process helpers are in
-- 'Agent.Tools.IO'.
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
import Agent.Tools.Grok (grokTools)
import Agent.Tools.Types

-- | Tools advertised for a model vendor. Surfaces are never mixed.
codingToolsFor :: Provider -> ToolEnv -> IO [AppTool]
codingToolsFor = \case
    XAIProvider -> pure . grokTools
    OpenAIProvider -> codexTools
