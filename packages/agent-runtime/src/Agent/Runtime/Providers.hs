-- | Scoped provider orchestration shared by terminal and non-terminal hosts.
module Agent.Runtime.Providers
    ( withProviderRuntime
    , module Agent.Runtime.Providers.Types
    ) where

import Agent.Runtime.Providers.Claude (withClaudeProvider)
import Agent.Runtime.Providers.Gemini (withGeminiProvider)
import Agent.Runtime.Providers.OpenAI (withOpenAiProvider)
import Agent.Runtime.Providers.OpenRouter (withOpenRouterProvider)
import Agent.Runtime.Providers.Types
import Agent.Runtime.Providers.XAI (withXaiProvider)

-- | Scope provider resources around a consumer. The consumer decides how to
-- compose the backend with session persistence, notices, and child agents.
withProviderRuntime
    :: ProviderConfig
    -> ProviderHost
    -> (ProviderRuntime -> IO a)
    -> IO a
withProviderRuntime config host use = case config of
    OpenAiProviderConfig openAi -> withOpenAiProvider openAi host use
    XaiProviderConfig tokens hostedTools gateway ->
        withXaiProvider tokens hostedTools gateway host use
    GeminiProviderConfig tokens -> withGeminiProvider tokens host use
    OpenRouterProviderConfig openRouter -> withOpenRouterProvider openRouter host use
    ClaudeProviderConfig claude -> withClaudeProvider claude host use
