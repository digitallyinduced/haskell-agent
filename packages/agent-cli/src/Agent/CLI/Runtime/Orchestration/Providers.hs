module Agent.CLI.Runtime.Orchestration.Providers
    ( AgentProviderRequest(..)
    , runAgentProviders
    ) where

import Agent.CLI.AccountPicker ()
import Agent.CLI.AccountSelection ()
import Agent.CLI.Afk ()
import Agent.CLI.AgentSessions ()
import Agent.CLI.AgentViewport ()
import Agent.CLI.Approval ()
import Agent.CLI.Artifact ()
import Agent.CLI.Clipboard ()
import Agent.CLI.CodeModeRuntime ()
import Agent.CLI.Command ()
import Agent.CLI.Config ()
import Agent.CLI.Database ()
import Agent.CLI.Database.Store ()
import Agent.CLI.Dialects ()
import Agent.CLI.GatewayBridge ()
import Agent.CLI.Input ()
import Agent.CLI.LearnedSkills ()
import Agent.CLI.LearnedSkills.Store ()
import Agent.CLI.Login ()
import Agent.CLI.Lsp ()
import Agent.CLI.ManagedTurn ()
import Agent.CLI.McpManager ()
import Agent.CLI.McpStatus ()
import Agent.CLI.Models ()
import Agent.CLI.Plan ()
import Agent.CLI.Prompt ()
import Agent.CLI.PromptHooks ()
import Agent.CLI.ProviderAvailability ()
import Agent.CLI.Recap ()
import Agent.CLI.ReplMode ()
import Agent.CLI.Request ()
import Agent.CLI.Resume ()
import Agent.CLI.Runtime.HistorySource ()
import Agent.CLI.Runtime.Orchestration.Background ()
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
import Agent.CLI.Runtime.Orchestration.Restart ()
import Agent.CLI.Runtime.Orchestration.Types
    ( NativeRunCapabilities(..)
    , NativeRunHooks(..)
    , fullNativeRunCapabilities
    )
import Agent.CLI.Runtime.Persistence ()
import Agent.CLI.Runtime.Types (RunResult)
import Agent.CLI.Secret ()
import Agent.CLI.Session.Attachments ()
import Agent.CLI.Session.Choices ()
import Agent.CLI.Session.Lifecycle ()
import Agent.CLI.Session.Runtime.Types
    ( StartupRuntime(..) )
import Agent.CLI.Session.Selection ()
import Agent.CLI.SessionAdmin ()
import Agent.CLI.SessionEnv ()
import Agent.CLI.SessionLock ()
import Agent.CLI.SessionState ()
import Agent.CLI.SessionTitle ()
import Agent.CLI.Skills ()
import Agent.CLI.Startup.Auth (startupDie)
import Agent.CLI.StartupContext ()
import Agent.CLI.TUI.History ()
import Agent.CLI.TUI.SessionHistory ()
import Agent.CLI.Tools ()
import Agent.CLI.Turn ()
import Agent.CLI.Usage ()
import Agent.CLI.WebFetch ()
import Agent.CLI.Worktree ()
import Agent.Cancel ()
import Agent.GrokBuild.Dialect.Goal ()
import Agent.GrokBuild.Dialect.Runtime ()
import Agent.GrokBuild.Dialect.Task ()
import Agent.GrokBuild.Dialect.Workflow ()
import Agent.OpenAI.Compaction ()
import Agent.OpenAI.Usage ()
import Agent.Provider
    ( Provider(..) )
import Agent.ReasoningEffort ()
import Agent.Responses.GenericClient ()
import Agent.Skills ()
import Agent.Store.Postgres ()
import Agent.Store.Types ()
import Agent.Subagents.TaskPath ()
import Agent.Tools.MultiAgents ()
import Agent.Tools.PlanMode ()
import Agent.Tools.Secret ()
import Agent.Tools.Types ()
import Agent.TUI.Motion ()
import Agent.XAI.Usage ()
import Control.Applicative ()
import Control.Exception ()
import Data.ByteString ()
import Data.Functor ()
import Data.List ()
import System.Console.ANSI ()
import System.Console.ANSI.Codes ()
import System.Directory.OsPath ()
import System.Environment ()
import System.Exit ()
import System.OsPath ()
import qualified Agent.OpenRouter.Usage ()
import qualified Agent.Provider ()
import qualified Data.Map.Strict ()
import qualified Data.Set ()

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
