-- | Reasoning effort values projected onto the Claude Code transport.
module Agent.Claude.ReasoningEffort
    ( ClaudeReasoningEffort(..)
    , claudeReasoningEffort
    , claudeReasoningEffortText
    ) where

import Agent.ReasoningEffort (ReasoningEffort(..))
import Data.Text (Text)

data ClaudeReasoningEffort
    = ClaudeDefault
    | ClaudeLow
    | ClaudeMedium
    | ClaudeHigh
    | ClaudeXHigh
    | ClaudeMax
    deriving (Eq, Ord, Show)

claudeReasoningEffort :: ReasoningEffort -> ClaudeReasoningEffort
claudeReasoningEffort = \case
    EffortNone -> ClaudeDefault
    EffortLow -> ClaudeLow
    EffortMedium -> ClaudeMedium
    EffortHigh -> ClaudeHigh
    EffortXHigh -> ClaudeXHigh
    EffortMax -> ClaudeMax

claudeReasoningEffortText :: ClaudeReasoningEffort -> Maybe Text
claudeReasoningEffortText = \case
    ClaudeDefault -> Nothing
    ClaudeLow -> Just "low"
    ClaudeMedium -> Just "medium"
    ClaudeHigh -> Just "high"
    ClaudeXHigh -> Just "xhigh"
    ClaudeMax -> Just "max"
