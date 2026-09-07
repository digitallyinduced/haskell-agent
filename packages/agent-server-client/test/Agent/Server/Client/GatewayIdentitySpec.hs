module Agent.Server.Client.GatewayIdentitySpec (spec) where

import Agent.Server.Client.GatewayIdentity
import Data.Aeson (eitherDecodeStrict')
import Data.Text qualified as Text
import Test.Hspec

spec :: Spec
spec = describe "gateway credential identity" do
    it "matches the stable public test vector" do
        gatewayCredentialIdentity sampleCredential
            `shouldBe` "gateway-sha256:Dpd-c7NJ8FNhmkgN6ohRz5X7k3fbLPxbm3fXQwlC5n8"

    it "normalizes equivalent gateway origins" do
        gatewayCredentialIdentity
            sampleCredential
                { gatewayBaseUrl = " HTTPS://GATEWAY.EXAMPLE:443/ "
                , gatewayWebSocketUrl = "wss://GATEWAY.EXAMPLE:443"
                }
            `shouldBe` gatewayCredentialIdentity sampleCredential

    it "changes with the private bearer without exposing it" do
        let replacement =
                gatewayCredentialIdentity
                    sampleCredential
                        { gatewayAccessToken = "replacement-secret"
                        }
            original = gatewayCredentialIdentity sampleCredential
        replacement `shouldNotBe` original
        original
            `shouldNotSatisfy` Text.isInfixOf sampleCredential.gatewayAccessToken

    it "binds query, user-info, and fragment URL material" do
        let original = gatewayCredentialIdentity sampleCredential
        gatewayCredentialIdentity
            sampleCredential
                { gatewayBaseUrl =
                    "https://tenant@gateway.example?organization=one#route"
                }
            `shouldNotBe` original
        gatewayCredentialIdentity
            sampleCredential
                { gatewayBaseUrl =
                    "https://tenant@gateway.example?organization=two#route"
                }
            `shouldNotBe` original

    it "uses unambiguous framing even when fields contain NUL" do
        gatewayCredentialIdentity
            sampleCredential
                { gatewayBaseUrl = "a\NULb"
                , gatewayWebSocketUrl = "c"
                }
            `shouldNotBe` gatewayCredentialIdentity
                sampleCredential
                    { gatewayBaseUrl = "a"
                    , gatewayWebSocketUrl = "b\NULc"
                    }

    it "decodes the installed gateway credential shape" do
        ( eitherDecodeStrict'
                "{\"base_url\":\"https://gateway.example\",\"websocket_url\":\"wss://gateway.example/v1/responses\",\"access_token\":\"gateway-bearer-secret\"}" ::
                Either String GatewayCredential
            )
            `shouldBe` Right sampleCredential

sampleCredential :: GatewayCredential
sampleCredential =
    GatewayCredential
        { gatewayBaseUrl = "https://gateway.example"
        , gatewayWebSocketUrl =
            "wss://gateway.example/v1/responses"
        , gatewayAccessToken = "gateway-bearer-secret"
        }
