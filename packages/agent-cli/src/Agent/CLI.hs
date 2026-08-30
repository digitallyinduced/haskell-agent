-- | Public command-line entry point and compatibility façade.
module Agent.CLI
    ( BuildInfo(..)
    , DevResult(..)
    , agentBuildInfo
    , afterDev
    , accountSwitchTarget
    , applyReplMode
    , buildPromptState
    , cycleReplInteraction
    , devArgs
    , devMain
    , devMainResume
    , formatMcpModelNotice
    , formatMcpModelNoticeFor
    , formatMcpProgress
    , formatBuildInfo
    , formatBuildInfoCompact
    , formatReplStatusLine
    , formatRepositoryPath
    , formatStartupTimings
    , formatTokenUsage
    , formatEstimatedTokensPerSecond
    , formatTokensPerSecond
    , formatUsageWithRate
    , learnAboutUserOnboardingPrompt
    , run
    , withRestoredCurrentDirectory
    ) where

import Agent.CLI.Runtime
    ( BuildInfo(..)
    , accountSwitchTarget
    , agentBuildInfo
    , afterDev
    , applyReplMode
    , buildPromptState
    , cycleReplInteraction
    , devArgs
    , devMain
    , devMainResume
    , formatBuildInfo
    , formatBuildInfoCompact
    , formatRepositoryPath
    , formatStartupTimings
    , learnAboutUserOnboardingPrompt
    , run
    , withRestoredCurrentDirectory
    )
import Agent.CLI.McpStatus
    ( formatMcpModelNotice
    , formatMcpModelNoticeFor
    , formatMcpProgress
    )
import Agent.CLI.Runtime.Types (DevResult(..))
import Agent.CLI.Status
    ( formatEstimatedTokensPerSecond
    , formatReplStatusLine
    , formatTokenUsage
    , formatTokensPerSecond
    , formatUsageWithRate
    )
