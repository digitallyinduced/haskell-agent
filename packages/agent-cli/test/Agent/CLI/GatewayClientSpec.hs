module Agent.CLI.GatewayClientSpec (spec) where

import Agent.CLI.GatewayClient
import Control.Exception.Safe (bracket)
import Data.Aeson qualified as Aeson
import Data.Bits ((.&.))
import Data.Text qualified as Text
import System.Directory
    ( createDirectory
    , getTemporaryDirectory
    , removeFile
    , removePathForcibly
    )
import System.IO (hClose, openTempFile)
import System.OsPath (OsPath, decodeUtf, unsafeEncodeUtf)
import System.Posix.Files (fileMode, getFileStatus)
import Test.Hspec

spec :: Spec
spec = describe "gateway device authorization" do
    it "decodes the device response contract" do
        let payload =
                "{\"device_code\":\"had_secret\",\"user_code\":\"ABCD-1234\",\
                \\"verification_uri\":\"https://gateway/connect/agent\",\
                \\"verification_uri_complete\":\"https://gateway/connect/agent?user_code=ABCD-1234\",\
                \\"expires_in\":600,\"interval\":5}"
        Aeson.eitherDecodeStrict' payload
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
        Aeson.eitherDecodeStrict'
            "{\"error\":\"authorization_pending\",\"interval\":5}"
            `shouldBe` Right (GatewayAuthorizationPending (Just 5))
        Aeson.eitherDecodeStrict'
            "{\"error\":\"slow_down\",\"interval\":10}"
            `shouldBe` Right (GatewaySlowDown (Just 10))
        Aeson.eitherDecodeStrict'
            "{\"access_token\":\"secret\",\"websocket_url\":\"wss://gateway/v1/responses\"}"
            `shouldBe` Right
                (GatewayAuthorized "secret" "wss://gateway/v1/responses")

    it "redacts gateway secrets from Show" do
        let credential =
                GatewayCredential "https://gateway" "wss://gateway/v1/responses" "secret"
            authorization =
                GatewayDeviceAuthorization
                    { deviceCode = "device-secret"
                    , userCode = "USER-SECRET"
                    , verificationUri = "https://gateway/connect/agent"
                    , verificationUriComplete =
                        "https://gateway/connect/agent?user_code=USER-SECRET"
                    , expiresInSeconds = 600
                    , pollIntervalSeconds = 5
                    }
        show credential `shouldSatisfy` not . Text.isInfixOf "secret" . Text.pack
        let renderedAuthorization = Text.pack (show authorization)
            renderedPoll =
                Text.pack $
                    show
                        (GatewayAuthorized
                            "secret"
                            "wss://gateway/v1/responses")
        renderedAuthorization
            `shouldSatisfy` not . Text.isInfixOf "device-secret"
        renderedAuthorization
            `shouldSatisfy` not . Text.isInfixOf "USER-SECRET"
        renderedPoll `shouldSatisfy` not . Text.isInfixOf "secret"
        renderedPoll `shouldSatisfy` not . Text.isInfixOf "wss://gateway"

    it "allows local HTTP development without trusting lookalike hosts" do
        validateBaseUrl "http://localhost:8080"
            `shouldBe` Right "http://localhost:8080"
        validateBaseUrl "http://[::1]:8080"
            `shouldBe` Right "http://[::1]:8080"
        validateBaseUrl "http://localhost.example"
            `shouldBe` Left
                "Gateway URL must use HTTPS (HTTP is allowed only for localhost development)."
        validateBaseUrl "http://localhost:8080@evil.example"
            `shouldBe` Left
                "Gateway URL must use HTTPS (HTTP is allowed only for localhost development)."
        validateBaseUrl "https://gateway.example:99999"
            `shouldBe` Left
                "Gateway URL must use HTTPS (HTTP is allowed only for localhost development)."

    it "requires verification URLs to use the gateway origin" do
        let authorization =
                GatewayDeviceAuthorization
                    { deviceCode = "device-secret"
                    , userCode = "ABCD-1234"
                    , verificationUri =
                        "https://platform.digitallyinduced.com/connect/agent"
                    , verificationUriComplete =
                        "https://platform.digitallyinduced.com/connect/agent?user_code=ABCD-1234"
                    , expiresInSeconds = 600
                    , pollIntervalSeconds = 5
                    }
            validate =
                validateGatewayDeviceAuthorization
                    "https://platform.digitallyinduced.com"
        validate authorization `shouldBe` Right authorization
        validate
            authorization
                { verificationUriComplete = "file:///etc/passwd"
                }
            `shouldBe` Left
                "The gateway returned an invalid complete verification URL."
        validate
            authorization
                { verificationUri = "https://example.com/connect/agent"
                }
            `shouldBe` Left
                "The gateway returned a verification URL for a different origin."

    it "validates every persisted gateway credential before use" do
        let credential =
                GatewayCredential
                    "https://platform.digitallyinduced.com"
                    "wss://platform.digitallyinduced.com/v1/responses"
                    "secret"
        validateGatewayCredential credential `shouldBe` Right credential
        validateGatewayCredential
            credential
                { gatewayAccessToken = " "
                }
            `shouldBe` Left
                "The gateway credential contains an empty access token."
        validateGatewayCredential
            credential
                { gatewayWebSocketUrl =
                    "wss://example.com/v1/responses"
                }
            `shouldBe` Left
                "The gateway credential WebSocket URL uses a different origin."

    it "round-trips credentials through a mode-0600 file" $
        withTempHome \home -> do
            let credential =
                    GatewayCredential
                        "https://gateway"
                        "wss://gateway/v1/responses"
                        "secret"
            saveGatewayCredentialAt home credential `shouldReturn` Right ()
            loadGatewayCredentialAt home
                `shouldReturn` Right (Just credential)
            status <- getFileStatus
                (either (error . show) id
                    (decodeUtf (gatewayCredentialPath home)))
            fileMode status .&. 0o777 `shouldBe` 0o600

withTempHome :: (OsPath -> IO value) -> IO value
withTempHome =
    bracket create
        (removePathForcibly . either (error . show) id . decodeUtf)
  where
    create = do
        temporary <- getTemporaryDirectory
        (path, handle) <- openTempFile temporary "agent-gateway-client"
        hClose handle
        removeFile path
        createDirectory path
        pure (unsafeEncodeUtf path)
