module Agent.CLI.GatewayClientSpec (spec) where

import Agent.CLI.GatewayClient
import Agent.Json.Decode qualified as Hermes
import Control.Exception.Safe (bracket)
import Data.Bits ((.&.))
import Data.ByteString.Lazy qualified as LBS
import Data.Either (isLeft)
import Data.Text qualified as Text
import System.Directory
    ( createDirectory
    , createDirectoryIfMissing
    , getTemporaryDirectory
    , removeFile
    , removePathForcibly
    )
import System.FilePath (takeDirectory)
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
        show (GatewayAuthorized "secret" "wss://gateway/v1/responses")
            `shouldSatisfy` not . Text.isInfixOf "secret" . Text.pack
        show (GatewayDeviceAuthorization
                "device-secret"
                "USER-CODE"
                "https://gateway/connect"
                "https://gateway/connect?code=USER-CODE"
                600
                5)
            `shouldSatisfy` not . Text.isInfixOf "device-secret" . Text.pack

    it "allows local HTTP development without trusting lookalike hosts" do
        validateBaseUrl "http://localhost:8080"
            `shouldBe` Right "http://localhost:8080"
        validateBaseUrl "http://localhost.example"
            `shouldBe` Left
                "Gateway URL must use HTTPS (HTTP is allowed only for localhost development)."
        validateBaseUrl "https://" `shouldSatisfy` isLeft
        validateBaseUrl "https://user@gateway.example"
            `shouldSatisfy` isLeft
        validateBaseUrl "https://gateway.example?token=secret"
            `shouldSatisfy` isLeft

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

    it "rejects invalid gateway endpoints before persisting them" $
        withTempHome \home -> do
            saveGatewayCredentialAt
                home
                (GatewayCredential
                    "https://gateway"
                    "https://not-a-websocket.example"
                    "secret")
                `shouldReturn`
                    Left "gateway WebSocket URL must use wss"
            saveGatewayCredentialAt
                home
                (GatewayCredential
                    "https://gateway"
                    "wss://gateway/v1/responses"
                    "")
                `shouldReturn`
                    Left "Gateway access token cannot be empty."

    it "rejects decoded credentials that fail validation" $
        withTempHome \home -> do
            let path =
                    either (error . show) id
                        (decodeUtf (gatewayCredentialPath home))
            createDirectoryIfMissing True (takeDirectory path)
            LBS.writeFile path
                "{\"version\":1,\"base_url\":\"https://gateway\",\
                \\"websocket_url\":\"wss://gateway/v1/responses\",\
                \\"access_token\":\"\"}"
            loadGatewayCredentialAt home
                `shouldReturn`
                    Left "Gateway access token cannot be empty."

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
