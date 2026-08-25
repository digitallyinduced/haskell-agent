-- | Public command-line entry point and compatibility façade.
module Agent.CLI
    ( DevResult(..)
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
    , formatReplStatusLine
    , formatRepositoryPath
    , formatStartupTimings
    , formatTokenUsage
    , learnAboutUserOnboardingPrompt
    , run
    , withRestoredCurrentDirectory
    ) where

import Agent.CLI.Runtime
    ( accountSwitchTarget
    , afterDev
    , applyReplMode
    , buildPromptState
    , cycleReplInteraction
    , devArgs
    , devMain
    , devMainResume
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
    ( formatReplStatusLine
    , formatTokenUsage
    )
