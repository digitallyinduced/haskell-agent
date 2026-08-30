module Agent.TelemetrySpec (spec) where

import Agent.Json (rawJsonFromEncoding)
import qualified Agent.Json.Decode as Json
import Agent.Telemetry
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Map.Strict as Map
import Test.Hspec

spec :: Spec
spec = describe "provider turn telemetry" do
    it "round-trips rich per-model metadata" do
        let telemetry = sampleTelemetry
            encoded = LazyByteString.toStrict (Aeson.encode telemetry)
        Json.decodeEither turnTelemetryDecoder encoded
            `shouldBe` Right telemetry

    it "formats a compact completion summary" do
        telemetrySummary sampleTelemetry
            `shouldBe`
                "$0.0123 · 2.5s · 3 provider turns · stop end_turn · context 140/200000"

    it "rejects negative counters and non-finite costs" do
        Json.decodeText turnTelemetryDecoder
            "{\"duration_ms\":-1,\"models\":{}}"
            `shouldSatisfy` isLeft
        Json.decodeText turnTelemetryDecoder
            "{\"cost_usd\":1e999,\"models\":{}}"
            `shouldSatisfy` isLeft

sampleTelemetry :: TurnTelemetry
sampleTelemetry = TurnTelemetry
    { telemetryDurationMs = Just 2500
    , telemetryApiDurationMs = Just 2200
    , telemetryCostUsd = Just 0.0123
    , telemetryStopReason = Just "end_turn"
    , telemetryProviderTurns = Just 3
    , telemetryModels = Map.singleton "claude-test" ModelTelemetry
        { modelInputTokens = 100
        , modelOutputTokens = 20
        , modelCacheReadInputTokens = 30
        , modelCacheCreationInputTokens = 10
        , modelWebSearchRequests = Just 1
        , modelCostUsd = Just 0.0123
        , modelContextWindow = Just 200000
        , modelMaxOutputTokens = Just 32000
        , modelCanonicalName = Just "claude-test-202608"
        , modelProviderName = Just "firstParty"
        }
    , telemetryStructuredOutput =
        Just (rawJsonFromEncoding (Aeson.toEncoding
            (Aeson.object ["answer" Aeson..= (42 :: Int)])))
    }

isLeft :: Either a b -> Bool
isLeft = \case
    Left _ -> True
    Right _ -> False
