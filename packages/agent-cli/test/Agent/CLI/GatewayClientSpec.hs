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

    it "builds the registered loopback Authorization Code + PKCE request" do
        gatewayAuthorizationUrl
            defaultGatewayBaseUrl
            "http://127.0.0.1:54321/oauth2callback"
            "0123456789abcdefghijklmnopqrstuvwxyz"
            "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"
            "Marc's Mac"
            `shouldBe` Right
                "https://platform.digitallyinduced.com/connect/agent/authorize\
                \?response_type=code\
                \&client_id=haskell-agent-cli\
                \&redirect_uri=http%3A%2F%2F127.0.0.1%3A54321%2Foauth2callback\
                \&state=0123456789abcdefghijklmnopqrstuvwxyz\
                \&code_challenge=E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM\
                \&code_challenge_method=S256\
                \&client_name=Marc%27s%20Mac"
        gatewayPkceChallenge
            "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"
            `shouldBe`
                "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"

    it "accepts only the exact IPv4 loopback redirect contract" do
        let authorize redirect =
                gatewayAuthorizationUrl
                    defaultGatewayBaseUrl
                    redirect
                    "0123456789abcdefghijklmnopqrstuvwxyz"
                    "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"
                    "Haskell Agent CLI"
        authorize "http://127.0.0.1:1/oauth2callback"
            `shouldSatisfy` not . isLeft
        authorize "http://localhost:54321/oauth2callback"
            `shouldBe` Left "Gateway OAuth redirect URI is invalid."
        authorize "http://127.0.0.1:54321/other"
            `shouldBe` Left "Gateway OAuth redirect URI is invalid."
        authorize "https://127.0.0.1:54321/oauth2callback"
            `shouldBe` Left "Gateway OAuth redirect URI is invalid."
        map authorize
            [ "http://127.0.0.1:0/oauth2callback"
            , "http://127.0.0.1:65536/oauth2callback"
            , "http://127.0.0.1:not-a-port/oauth2callback"
            , "http://127.0.0.1:54321/oauth2callback?next=evil"
            , "http://127.0.0.1:54321/oauth2callback#fragment"
            , "http://user@127.0.0.1:54321/oauth2callback"
            , "http://127.0.0.1.example:54321/oauth2callback"
            ]
            `shouldSatisfy` all isLeft

    it "validates callback method, path, singleton state, and errors" do
        let state = "0123456789abcdefghijklmnopqrstuvwxyz"
            callback target =
                validateGatewayAuthorizationCallback
                    state
                    ("GET " <> target <> " HTTP/1.1\r\nHost: 127.0.0.1\r\n\r\n")
        callback
            "/oauth2callback?code=hac_secret&state=0123456789abcdefghijklmnopqrstuvwxyz"
            `shouldBe` Right "hac_secret"
        callback
            "/oauth2callback?code=hac_secret&state=wrong"
            `shouldBe` Left "Gateway OAuth callback state mismatch."
        callback
            "/other?code=hac_secret&state=0123456789abcdefghijklmnopqrstuvwxyz"
            `shouldBe` Left "Gateway OAuth callback path is invalid."
        validateGatewayAuthorizationCallback
            state
            "POST /oauth2callback?code=hac_secret&state=0123456789abcdefghijklmnopqrstuvwxyz HTTP/1.1\r\n\r\n"
            `shouldBe` Left "Gateway OAuth callback must use GET."
        callback
            "/oauth2callback?error=access_denied&state=0123456789abcdefghijklmnopqrstuvwxyz"
            `shouldBe` Left
                "Gateway authorization was not granted: access_denied."
        callback
            "/oauth2callback?code=hac_secret&state=0123456789abcdefghijklmnopqrstuvwxyz&state=0123456789abcdefghijklmnopqrstuvwxyz"
            `shouldBe` Left
                "Gateway OAuth callback contains duplicate or invalid state parameters."
        callback
            "/oauth2callback?code=first&code=second&state=0123456789abcdefghijklmnopqrstuvwxyz"
            `shouldBe` Left
                "Gateway OAuth callback contains duplicate or invalid code parameters."
        callback
            "/oauth2callback?error=%3Cscript%3E&state=0123456789abcdefghijklmnopqrstuvwxyz"
            `shouldBe` Left "Gateway authorization was not granted."

    it "decodes and validates a same-origin bearer response" do
        let payload =
                "{\"access_token\":\"hag_secret\",\"token_type\":\"Bearer\",\
                \\"base_url\":\"https://platform.digitallyinduced.com\",\
                \\"websocket_url\":\"wss://platform.digitallyinduced.com/v1/responses\"}"
            response =
                GatewayAuthorizationCodeResponse
                    { authorizationAccessToken = "hag_secret"
                    , authorizationTokenType = "Bearer"
                    , authorizationResponseBaseUrl =
                        "https://platform.digitallyinduced.com"
                    , authorizationWebSocketUrl =
                        "wss://platform.digitallyinduced.com/v1/responses"
                    }
        Hermes.decodeEither
            gatewayAuthorizationCodeDecoder
            payload
            `shouldBe` Right response
        validateGatewayAuthorizationCodeResponse
            defaultGatewayBaseUrl response
            `shouldBe` Right
                GatewayCredential
                    { gatewayBaseUrl =
                        "https://platform.digitallyinduced.com"
                    , gatewayWebSocketUrl =
                        "wss://platform.digitallyinduced.com/v1/responses"
                    , gatewayAccessToken = "hag_secret"
                    }

    it "rejects OAuth responses with the wrong scheme or origin" do
        let response =
                GatewayAuthorizationCodeResponse
                    { authorizationAccessToken = "hag_secret"
                    , authorizationTokenType = "Bearer"
                    , authorizationResponseBaseUrl =
                        "https://platform.digitallyinduced.com"
                    , authorizationWebSocketUrl =
                        "wss://platform.digitallyinduced.com/v1/responses"
                    }
            validate =
                validateGatewayAuthorizationCodeResponse
                    defaultGatewayBaseUrl
        validate response { authorizationTokenType = "bearer" }
            `shouldBe`
                Left "The gateway returned an unsupported token type."
        validate
            response
                { authorizationResponseBaseUrl = "https://example.com"
                , authorizationWebSocketUrl =
                    "wss://example.com/v1/responses"
                }
            `shouldBe`
                Left
                    "The gateway returned a credential for a different origin."
        validate
            response
                { authorizationWebSocketUrl =
                    "wss://example.com/v1/responses"
                }
            `shouldBe`
                Left
                    "The gateway returned a WebSocket URL for a different origin."

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
        let browserResponse =
                GatewayAuthorizationCodeResponse
                    "oauth-secret"
                    "Bearer"
                    "https://gateway"
                    "wss://gateway/secret-websocket"
            rendered = Text.pack (show browserResponse)
        rendered `shouldSatisfy` not . Text.isInfixOf "oauth-secret"
        rendered `shouldSatisfy` not . Text.isInfixOf "secret-websocket"

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
