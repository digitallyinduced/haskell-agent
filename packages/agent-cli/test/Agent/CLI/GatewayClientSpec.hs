module Agent.CLI.GatewayClientSpec (spec) where

import Agent.CLI.GatewayClient
import Agent.Json.Decode qualified as Hermes
import Data.Text qualified as Text
import Test.Hspec

spec :: Spec
spec = describe "gateway device authorization" do
    it "decodes the device response contract" do
        let payload =
                "{\"device_code\":\"had_secret\",\"user_code\":\"ABCD-1234\",\
                \\"verification_uri\":\"https://gateway/connect/agent\",\
                \\"verification_uri_complete\":\"https://gateway/connect/agent?user_code=ABCD-1234\",\
                \\"expires_in\":600,\"interval\":5}"
        Hermes.decodeEither gatewayDeviceDecoder payload
            `shouldBe` Right
                GatewayDeviceAuthorization
                    { deviceCode = "had_secret"
                    , userCode = "ABCD-1234"
                    , verificationUri = "https://gateway/connect/agent"
                    , verificationUriComplete =
                        "https://gateway/connect/agent?user_code=ABCD-1234"
                    , expiresInSeconds = 600
                    , pollIntervalSeconds = 5
                    }

    it "decodes pending, slow-down, and successful polls" do
        Hermes.decodeEither gatewayPollDecoder
            "{\"error\":\"authorization_pending\",\"interval\":5}"
            `shouldBe` Right (GatewayAuthorizationPending (Just 5))
        Hermes.decodeEither gatewayPollDecoder
            "{\"error\":\"slow_down\",\"interval\":10}"
            `shouldBe` Right (GatewaySlowDown (Just 10))
        Hermes.decodeEither gatewayPollDecoder
            "{\"access_token\":\"secret\",\"websocket_url\":\"wss://gateway/v1/responses\"}"
            `shouldBe` Right
                (GatewayAuthorized "secret" "wss://gateway/v1/responses")

    it "redacts the bearer credential from Show" do
        let credential =
                GatewayCredential "https://gateway" "wss://gateway/v1/responses" "secret"
        show credential `shouldSatisfy` not . Text.isInfixOf "secret" . Text.pack

    it "allows local HTTP development without trusting lookalike hosts" do
        validateBaseUrl "http://localhost:8080"
            `shouldBe` Right "http://localhost:8080"
        validateBaseUrl "http://localhost.example"
            `shouldBe` Left
                "Gateway URL must use HTTPS (HTTP is allowed only for localhost development)."
