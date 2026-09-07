module Agent.CLI.Runtime.Orchestration.Providers
    ( withProviderRuntime
    ) where

import Agent.CLI.Runtime.Orchestration.Providers.Claude (withClaudeProvider)
import Agent.CLI.Runtime.Orchestration.Providers.Gemini (withGeminiProvider)
import Agent.CLI.Runtime.Orchestration.Providers.OpenAI (withOpenAiProvider)
import Agent.CLI.Runtime.Orchestration.Providers.OpenRouter (withOpenRouterProvider)
import Agent.CLI.Runtime.Orchestration.Providers.Types
import Agent.CLI.Runtime.Orchestration.Providers.XAI (withXaiProvider)

-- | Scope provider resources around a consumer. The consumer decides how to
-- compose the backend with session persistence, notices, and child agents.
withProviderRuntime
    :: ProviderConfig
    -> ProviderHost
    -> (ProviderRuntime -> IO a)
    -> IO a
withProviderRuntime config host use = case config of
    OpenAiProviderConfig openAi -> withOpenAiProvider openAi host use
    XaiProviderConfig tokens hostedTools -> withXaiProvider tokens hostedTools host use
    GeminiProviderConfig tokens -> withGeminiProvider tokens host use
    OpenRouterProviderConfig openRouter -> withOpenRouterProvider openRouter host use
    ClaudeProviderConfig claude -> withClaudeProvider claude host use
