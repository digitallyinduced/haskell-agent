-- | Provider-neutral reasoning effort and provider-specific wire domains.
--
-- Keep the canonical value in CLI and persistence state. Convert to text only
-- at protocol boundaries such as 'ReasoningConfig', whose schema is deliberately
-- open for forwards-compatible Responses API fields.
module Agent.ReasoningEffort
    ( ReasoningEffort(..)
    , OpenAIReasoningEffort(..)
    , GrokReasoningEffort(..)
    , ClaudeReasoningEffort(..)
    , reasoningEfforts
    , reasoningEffortText
    , parseReasoningEffort
    , openAIReasoningEffort
    , openAIReasoningEffortText
    , grokReasoningEffort
    , grokReasoningEffortText
    , claudeReasoningEffort
    , claudeReasoningEffortText
    ) where

import Data.Aeson (FromJSON(..), ToJSON(..), withText)
import Data.Text (Text)
import qualified Data.Text as Text

data ReasoningEffort
    = EffortNone
    | EffortLow
    | EffortMedium
    | EffortHigh
    | EffortXHigh
    | EffortMax
    deriving (Bounded, Enum, Eq, Ord, Show)

reasoningEfforts :: [ReasoningEffort]
reasoningEfforts = [minBound .. maxBound]

reasoningEffortText :: ReasoningEffort -> Text
reasoningEffortText = \case
    EffortNone -> "none"
    EffortLow -> "low"
    EffortMedium -> "medium"
    EffortHigh -> "high"
    EffortXHigh -> "xhigh"
    EffortMax -> "max"

parseReasoningEffort :: Text -> Either Text ReasoningEffort
parseReasoningEffort raw =
    let value = Text.toLower (Text.strip raw)
    in case value of
        "none" -> Right EffortNone
        "low" -> Right EffortLow
        "medium" -> Right EffortMedium
        "high" -> Right EffortHigh
        "xhigh" -> Right EffortXHigh
        "max" -> Right EffortMax
        _ -> Left
            ("effort must be none, low, medium, high, xhigh, or max (got "
                <> raw <> ")")

instance ToJSON ReasoningEffort where
    toJSON = toJSON . reasoningEffortText

instance FromJSON ReasoningEffort where
    parseJSON = withText "ReasoningEffort" $
        either (fail . Text.unpack) pure . parseReasoningEffort

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
