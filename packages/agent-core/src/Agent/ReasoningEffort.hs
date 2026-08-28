-- | Provider-neutral reasoning effort and provider-specific wire domains.
--
-- Keep the canonical value in CLI and persistence state. Convert to text only
-- at protocol boundaries such as 'ReasoningConfig', whose schema is deliberately
-- open for forwards-compatible Responses API fields.
module Agent.ReasoningEffort
    ( ReasoningEffort(..)
    , reasoningEfforts
    , reasoningEffortText
    , parseReasoningEffort
    , reasoningEffortDecoder
    ) where

import qualified Agent.Json.Decode as Json
import Data.Aeson (ToJSON(..))
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

reasoningEffortDecoder :: Json.Decoder ReasoningEffort
reasoningEffortDecoder = Json.withText $
        either (fail . Text.unpack) pure . parseReasoningEffort
