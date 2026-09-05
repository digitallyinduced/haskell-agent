module Agent.CLI.Runtime.Orchestration.Providers
    ( AgentProviderRequest(..)
    , runAgentProviders
    ) where

import Agent.CLI.Runtime.Orchestration.Providers.Claude
    ( runClaudeProvider
    )
import Agent.CLI.Runtime.Orchestration.Providers.Gemini
    ( runGeminiProvider
    )
import Agent.CLI.Runtime.Orchestration.Providers.OpenAI
    ( runOpenAiProvider
    )
import Agent.CLI.Runtime.Orchestration.Providers.OpenRouter
    ( runOpenRouterProvider
    )
import Agent.CLI.Runtime.Orchestration.Providers.Types
    ( AgentProviderRequest(..)
    )
import Agent.CLI.Runtime.Orchestration.Providers.XAI
    ( runXaiProvider
    )
import Agent.CLI.Runtime.Orchestration.Types
    ( NativeRunCapabilities(..)
    , NativeRunHooks(..)
    , fullNativeRunCapabilities
    )
import Agent.CLI.Runtime.Types (RunResult)
import Agent.CLI.Session.Runtime.Types
    ( StartupRuntime(..) )
import Agent.CLI.Startup.Auth (startupDie)
import Agent.Provider
    ( Provider(..) )

runAgentProviders
    :: AgentProviderRequest
    -> IO RunResult
runAgentProviders request@AgentProviderRequest{provider, startup} =
    let nativeCapabilities =
            maybe
                fullNativeRunCapabilities
                (.nativeCapabilities)
                startup.startupNativeHooks
    in case provider of
        OpenAIProvider ->
            runOpenAiProvider request nativeCapabilities
        XAIProvider ->
            runXaiProvider request nativeCapabilities
        GeminiProvider ->
            runGeminiProvider request
        ClaudeCodeProvider
            | not nativeCapabilities.nativeProviderNativeTools ->
                startupDie startup
                    "Claude Code is unavailable in this runtime"
            | otherwise ->
                runClaudeProvider request nativeCapabilities
        OpenRouterProvider ->
            runOpenRouterProvider request
