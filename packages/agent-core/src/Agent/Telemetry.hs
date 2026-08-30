-- | Provider-neutral metadata reported for a completed model submission.
--
-- Token totals remain in 'Agent.Loop.TokenUsage'; these values retain richer
-- provider diagnostics without forcing every backend to manufacture them.
module Agent.Telemetry
    ( ModelTelemetry(..)
    , TurnTelemetry(..)
    , modelTelemetryDecoder
    , turnTelemetryDecoder
    , turnTelemetryListDecoder
    , telemetrySummary
    ) where

import Agent.Json
    ( RawJson
    , rawJsonDecoder
    )
import qualified Agent.Json.Decode as Json
import Data.Aeson
    ( ToJSON(..)
    , object
    , (.=)
    )
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import Numeric (showFFloat)

-- | Usage and capacity metadata for one concrete model used during a
-- provider submission. Providers may use more than one model in a turn.
data ModelTelemetry = ModelTelemetry
    { modelInputTokens :: !Int
    , modelOutputTokens :: !Int
    , modelCacheReadInputTokens :: !Int
    , modelCacheCreationInputTokens :: !Int
    , modelWebSearchRequests :: !(Maybe Int)
    , modelCostUsd :: !(Maybe Double)
    , modelContextWindow :: !(Maybe Int)
    , modelMaxOutputTokens :: !(Maybe Int)
    , modelCanonicalName :: !(Maybe Text)
    , modelProviderName :: !(Maybe Text)
    } deriving (Eq, Show)

-- | Rich metadata for one completed provider submission.
data TurnTelemetry = TurnTelemetry
    { telemetryDurationMs :: !(Maybe Int)
    , telemetryApiDurationMs :: !(Maybe Int)
    , telemetryCostUsd :: !(Maybe Double)
    , telemetryStopReason :: !(Maybe Text)
    , telemetryProviderTurns :: !(Maybe Int)
    , telemetryModels :: !(Map Text ModelTelemetry)
    , telemetryStructuredOutput :: !(Maybe RawJson)
    } deriving (Eq, Show)

instance ToJSON ModelTelemetry where
    toJSON model = object
        [ "input_tokens" .= model.modelInputTokens
        , "output_tokens" .= model.modelOutputTokens
        , "cache_read_input_tokens" .= model.modelCacheReadInputTokens
        , "cache_creation_input_tokens" .=
            model.modelCacheCreationInputTokens
        , "web_search_requests" .= model.modelWebSearchRequests
        , "cost_usd" .= model.modelCostUsd
        , "context_window" .= model.modelContextWindow
        , "max_output_tokens" .= model.modelMaxOutputTokens
        , "canonical_model" .= model.modelCanonicalName
        , "provider" .= model.modelProviderName
        ]

instance ToJSON TurnTelemetry where
    toJSON telemetry = object
        [ "duration_ms" .= telemetry.telemetryDurationMs
        , "api_duration_ms" .= telemetry.telemetryApiDurationMs
        , "cost_usd" .= telemetry.telemetryCostUsd
        , "stop_reason" .= telemetry.telemetryStopReason
        , "provider_turns" .= telemetry.telemetryProviderTurns
        , "models" .= telemetry.telemetryModels
        , "structured_output" .= telemetry.telemetryStructuredOutput
        ]

modelTelemetryDecoder :: Json.Decoder ModelTelemetry
modelTelemetryDecoder = Json.object $
    ModelTelemetry
        <$> Json.atKey "input_tokens" nonNegativeInt
        <*> Json.atKey "output_tokens" nonNegativeInt
        <*> Json.atKey "cache_read_input_tokens" nonNegativeInt
        <*> Json.atKey "cache_creation_input_tokens" nonNegativeInt
        <*> Json.optionalKey "web_search_requests" nonNegativeInt
        <*> Json.optionalKey "cost_usd" nonNegativeDouble
        <*> Json.optionalKey "context_window" nonNegativeInt
        <*> Json.optionalKey "max_output_tokens" nonNegativeInt
        <*> Json.optionalKey "canonical_model" Json.text
        <*> Json.optionalKey "provider" Json.text

turnTelemetryDecoder :: Json.Decoder TurnTelemetry
turnTelemetryDecoder = Json.object $
    TurnTelemetry
        <$> Json.optionalKey "duration_ms" nonNegativeInt
        <*> Json.optionalKey "api_duration_ms" nonNegativeInt
        <*> Json.optionalKey "cost_usd" nonNegativeDouble
        <*> Json.optionalKey "stop_reason" Json.text
        <*> Json.optionalKey "provider_turns" nonNegativeInt
        <*> Json.defaultKey Map.empty "models"
            (Json.objectAsMap pure modelTelemetryDecoder)
        <*> Json.optionalKey "structured_output" rawJsonDecoder

turnTelemetryListDecoder :: Json.Decoder [TurnTelemetry]
turnTelemetryListDecoder = Json.list turnTelemetryDecoder

nonNegativeInt :: Json.Decoder Int
nonNegativeInt = do
    value <- Json.int
    if value >= 0
        then pure value
        else fail "expected a non-negative integer"

nonNegativeDouble :: Json.Decoder Double
nonNegativeDouble = do
    value <- Json.double
    if value >= 0 && not (isNaN value || isInfinite value)
        then pure value
        else fail "expected a finite non-negative number"

-- | Compact detail suitable for a CLI completion line. Empty metadata renders
-- no text, letting callers retain their existing generic status.
telemetrySummary :: TurnTelemetry -> Text
telemetrySummary telemetry =
    Text.intercalate " · " . catMaybes $
        [ formatCost <$> telemetry.telemetryCostUsd
        , formatDuration <$> telemetry.telemetryDurationMs
        , formatProviderTurns <$> telemetry.telemetryProviderTurns
        , ("stop " <>) <$> telemetry.telemetryStopReason
        , formatContext telemetry.telemetryModels
        ]
  where
    formatCost value =
        "$" <> Text.pack (showFFloat (Just 4) value "")
    formatDuration millis
        | millis < 1000 =
            Text.pack (show millis) <> "ms"
        | otherwise =
            Text.pack (showFFloat (Just 1)
                (fromIntegral millis / 1000 :: Double) "")
                <> "s"
    formatProviderTurns turns =
        Text.pack (show turns)
            <> if turns == 1 then " provider turn" else " provider turns"
    formatContext :: Map Text ModelTelemetry -> Maybe Text
    formatContext models =
        case
            [ (used, limit)
            | model <- Map.elems models
            , Just limit <- [model.modelContextWindow]
            , let used =
                    model.modelInputTokens
                        + model.modelCacheReadInputTokens
                        + model.modelCacheCreationInputTokens
            ] of
            [] -> Nothing
            contexts ->
                let (used, limit) = maximum contexts
                in Just
                    ("context "
                        <> Text.pack (show used)
                        <> "/"
                        <> Text.pack (show limit))
