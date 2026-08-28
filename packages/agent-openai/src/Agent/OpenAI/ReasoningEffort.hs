-- | Reasoning effort values accepted by the OpenAI transport.
module Agent.OpenAI.ReasoningEffort
    ( OpenAIReasoningEffort(..)
    , openAIReasoningEffort
    , openAIReasoningEffortText
    ) where

import Agent.ReasoningEffort (ReasoningEffort(..))
import Data.Text (Text)

data OpenAIReasoningEffort
    = OpenAINone
    | OpenAILow
    | OpenAIMedium
    | OpenAIHigh
    | OpenAIXHigh
    | OpenAIMax
    deriving (Eq, Ord, Show)

openAIReasoningEffort :: ReasoningEffort -> OpenAIReasoningEffort
openAIReasoningEffort = \case
    EffortNone -> OpenAINone
    EffortLow -> OpenAILow
    EffortMedium -> OpenAIMedium
    EffortHigh -> OpenAIHigh
    EffortXHigh -> OpenAIXHigh
    EffortMax -> OpenAIMax

openAIReasoningEffortText :: OpenAIReasoningEffort -> Text
openAIReasoningEffortText = \case
    OpenAINone -> "none"
    OpenAILow -> "low"
    OpenAIMedium -> "medium"
    OpenAIHigh -> "high"
    OpenAIXHigh -> "xhigh"
    OpenAIMax -> "max"
