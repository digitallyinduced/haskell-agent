module Agent.XAI.UsageSpec (spec) where

import Agent.Provider (Credential(..), Provider(..))
import Agent.XAI.Usage
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.CaseInsensitive as CI
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Test.Hspec

spec :: Spec
spec = do
    describe "decodeGrokUsage" do
        it "decodes and floors the current subscription period" do
            let body = LBS.pack
                    "{\"config\":{\"creditUsagePercent\":31.9,\
                    \\"currentPeriod\":{\"type\":\"USAGE_PERIOD_TYPE_WEEKLY\",\
                    \\"start\":\"2026-08-20T00:00:00Z\",\
                    \\"end\":\"2026-08-21T00:00:00Z\"}}}"
            case decodeGrokUsage body of
                Left err -> expectationFailure (show err)
                Right snapshot -> do
                    snapshot.usedPercent `shouldBe` 31
                    snapshot.periodType `shouldBe` "USAGE_PERIOD_TYPE_WEEKLY"
                    snapshot.windowSeconds `shouldBe` 86400
                    weeklyLimitLeft snapshot `shouldBe` Just 69

        it "clamps percentages to the provider range" do
            let body = LBS.pack
                    "{\"config\":{\"creditUsagePercent\":140,\
                    \\"currentPeriod\":{\"start\":\"2026-08-20T00:00:00Z\",\
                    \\"end\":\"2026-08-21T00:00:00Z\"}}}"
            fmap (.usedPercent) (decodeGrokUsage body) `shouldBe` Right 100

        it "hides parser internals for unreadable responses" do
            decodeGrokUsage "not json"
                `shouldBe` Left "Grok returned an unreadable usage response."

        it "treats an omitted proto3 zero percentage as freshly reset usage" do
            let body = LBS.pack
                    "{\"config\":{\"isUnifiedBillingUser\":true,\
                    \\"currentPeriod\":{\"type\":\"USAGE_PERIOD_TYPE_WEEKLY\",\
                    \\"start\":\"2026-08-24T15:14:26Z\",\
                    \\"end\":\"2026-08-31T15:14:26Z\"}}}"
            case decodeGrokUsage body of
                Left err -> expectationFailure (show err)
                Right snapshot -> do
                    snapshot.usedPercent `shouldBe` 0
                    weeklyLimitLeft snapshot `shouldBe` Just 100

        it "does not label non-weekly periods as weekly" do
            let body = LBS.pack
                    "{\"config\":{\"creditUsagePercent\":20,\
                    \\"currentPeriod\":{\"type\":\"USAGE_PERIOD_TYPE_MONTHLY\",\
                    \\"start\":\"2026-08-01T00:00:00Z\",\
                    \\"end\":\"2026-09-01T00:00:00Z\"}}}"
            fmap weeklyLimitLeft (decodeGrokUsage body)
                `shouldBe` Right Nothing

    describe "fetchGrokUsageFrom" do
        it "uses the current Grok CLI proxy authentication contract" do
            headersSeen <- newEmptyMVar
            Warp.testWithApplication
                (pure (billingApp headersSeen))
                \port -> do
                    result <- fetchGrokUsageFrom
                        ("http://127.0.0.1:" <> show port <> "/v1/billing?format=credits")
                        testCredential
                    fmap (.usedPercent) result `shouldBe` Right 20
            headers <- takeMVar headersSeen
            lookup "Authorization" headers
                `shouldBe` Just "Bearer token-a"
            lookup "X-XAI-Token-Auth" headers
                `shouldBe` Just "xai-grok-cli"
            lookup "x-userid" headers
                `shouldBe` Just "user-a"
            lookup "X-Auth-Token" headers `shouldBe` Nothing

billingApp
    :: MVar [(Text, Text)]
    -> Wai.Application
billingApp headersSeen request respond = do
    putMVar headersSeen
        [ (Text.decodeUtf8 (CI.original name), Text.decodeUtf8 value)
        | (name, value) <- Wai.requestHeaders request
        ]
    respond $ Wai.responseLBS HTTP.status200
        [("Content-Type", "application/json")]
        "{\"config\":{\"creditUsagePercent\":20,\
        \\"currentPeriod\":{\"type\":\"USAGE_PERIOD_TYPE_WEEKLY\",\
        \\"start\":\"2026-08-24T00:00:00Z\",\
        \\"end\":\"2026-08-31T00:00:00Z\"}}}"

testCredential :: Credential
testCredential = Credential
    { accessToken = "token-a"
    , accountId = "user-a"
    , leaseId = Nothing
    , provider = XAIProvider
    }
