-- | Reasoning effort values accepted by the xAI Grok transport.
module Agent.XAI.ReasoningEffort
    ( GrokReasoningEffort(..)
    , grokReasoningEffort
    , grokReasoningEffortText
    ) where

import Agent.ReasoningEffort (ReasoningEffort(..))
import Data.Text (Text)

data GrokReasoningEffort
    = GrokNone
    | GrokLow
    | GrokMedium
    | GrokHigh
    | GrokXHigh
    deriving (Eq, Ord, Show)

-- | Grok has no @max@ value. Resumed or inherited @max@ state is normalized
-- to @high@ at this final defensive boundary.
grokReasoningEffort :: ReasoningEffort -> GrokReasoningEffort
grokReasoningEffort = \case
    EffortNone -> GrokNone
    EffortLow -> GrokLow
    EffortMedium -> GrokMedium
    EffortHigh -> GrokHigh
    EffortXHigh -> GrokXHigh
    EffortMax -> GrokHigh

grokReasoningEffortText :: GrokReasoningEffort -> Text
grokReasoningEffortText = \case
    GrokNone -> "low"
    GrokLow -> "low"
    GrokMedium -> "medium"
    GrokHigh -> "high"
    GrokXHigh -> "xhigh"
