module Agent.CLI.GatewayClientSpec (spec) where

import Agent.CLI.GatewayClient
import Agent.Json.Decode qualified as Hermes
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.Async (cancel, poll, wait, waitCatch, withAsync)
import Control.Exception.Safe (bracket, throwString, tryAny)
import Control.Monad (void)
import Data.Aeson qualified as Aeson
import Data.Bits ((.&.))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Either (isLeft)
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Maybe (isNothing)
import Data.Text qualified as Text
import System.Directory
    ( createDirectory
    , createDirectoryIfMissing
    , doesFileExist
    , getTemporaryDirectory
    , removeFile
    , removePathForcibly
    )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath (takeDirectory, (</>))
import System.IO (hClose, openTempFile)
import System.OsPath (OsPath, decodeUtf, unsafeEncodeUtf)
import System.Posix.Files (fileMode, getFileStatus)
import System.Posix.Process (forkProcess, getProcessStatus)
import System.Posix.Signals (sigKILL, signalProcess)
import System.Timeout (timeout)
import Test.Hspec

decodeGatewayModels :: BS.ByteString -> Either String [GatewayModel]
decodeGatewayModels bytes =
    (.gatewayModelCatalogData)
        <$> ( Aeson.eitherDecodeStrict' bytes
                :: Either String GatewayModelCatalogResponse
            )

spec :: Spec
spec = describe "gateway device authorization" do
    it "binds sessions to the exact gateway credential without exposing it" do
        let first =
                GatewayCredential
                    "https://gateway.example"
                    "wss://gateway.example/v1/responses"
                    "gateway-bearer-secret"
            same = gatewayCredentialIdentity first
            replacement =
                gatewayCredentialIdentity
                    first { gatewayAccessToken = "replacement-secret" }
            otherGateway =
                gatewayCredentialIdentity
                    first
                        { gatewayBaseUrl = "https://other.example"
                        , gatewayWebSocketUrl =
                            "wss://other.example/v1/responses"
                        }
            equivalent =
                gatewayCredentialIdentity
                    first
                        { gatewayBaseUrl = " HTTPS://GATEWAY.EXAMPLE:443/ "
                        , gatewayWebSocketUrl =
                            "wss://GATEWAY.EXAMPLE:443"
                        }
        same `shouldBe` gatewayCredentialIdentity first
        equivalent `shouldBe` same
        replacement `shouldNotBe` same
        otherGateway `shouldNotBe` same
        same `shouldNotSatisfy`
            Text.isInfixOf first.gatewayAccessToken

    it "decodes, trims, and deduplicates the typed gateway catalog" do
        decodeGatewayModels
            "{\"object\":\"list\",\"data\":[{\"id\":\" gpt-5.6-sol \",\"protocol\":\"responses\"},{\"id\":\"\",\"protocol\":\"responses\"},{\"id\":\"gpt-5.6-sol\",\"protocol\":\"responses\"},{\"id\":\"sonnet\",\"protocol\":\"anthropic\"},{\"id\":\"bad id\",\"protocol\":\"responses\"}]}"
            `shouldBe`
                Right
                    [ GatewayModel "gpt-5.6-sol" GatewayResponsesProtocol
                    , GatewayModel "sonnet" GatewayAnthropicProtocol
                    ]

    it "rejects unknown gateway model protocols" do
        decodeGatewayModels
            "{\"object\":\"list\",\"data\":[{\"id\":\"model\",\"protocol\":\"router\"}]}"
            `shouldSatisfy` isLeft

    it "clears cached gateway models when refresh fails" do
        results <- newIORef
            [ Right
                [ GatewayModel " gpt-5.6-sol " GatewayResponsesProtocol
                , GatewayModel "" GatewayResponsesProtocol
                , GatewayModel "gpt-5.6-sol" GatewayResponsesProtocol
                , GatewayModel "sonnet" GatewayAnthropicProtocol
                ]
            , Left "gateway unavailable"
            ]
        access <-
            newGatewayModelAccessWith $
                atomicModifyIORef' results \case
                    result : remaining -> (remaining, result)
                    [] -> ([], Left "unexpected refresh")
        refreshGatewayModels access
            `shouldReturn`
                Right
                    [ GatewayModel "gpt-5.6-sol" GatewayResponsesProtocol
                    , GatewayModel "sonnet" GatewayAnthropicProtocol
                    ]
        cachedGatewayModels access
            `shouldReturn`
                Just
                    [ GatewayModel "gpt-5.6-sol" GatewayResponsesProtocol
                    , GatewayModel "sonnet" GatewayAnthropicProtocol
                    ]
        refreshGatewayModels access
            `shouldReturn` Left "gateway unavailable"
        cachedGatewayModels access `shouldReturn` Nothing

    it "clears cached gateway models when a fetch throws" do
        calls <- newIORef (0 :: Int)
        access <-
            newGatewayModelAccessWith do
                call <- atomicModifyIORef' calls \count ->
                    (count + 1, count)
                if call == 0
                    then
                        pure
                            (Right
                                [ GatewayModel
                                    "company-model"
                                    GatewayResponsesProtocol
                                ])
                    else throwString "transport details"
        refreshGatewayModels access
            `shouldReturn`
                Right
                    [GatewayModel "company-model" GatewayResponsesProtocol]
        refreshGatewayModels access
            `shouldReturn`
                Left "Could not refresh organization gateway models."
        cachedGatewayModels access `shouldReturn` Nothing

    it "does not expose a gateway bearer in model-list validation errors" do
        let credential =
                GatewayCredential
                    "not a gateway URL"
                    "wss://gateway/v1/responses"
                    "gateway-bearer-secret"
        fetchGatewayModels credential
            `shouldReturn` Left "Gateway credential is invalid."

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

    it "requires device verification URLs to stay on the gateway origin" do
        let device =
                GatewayDeviceAuthorization
                    { deviceCode = "had_secret"
                    , userCode = "ABCD-1234"
                    , verificationUri =
                        "https://platform.digitallyinduced.com/connect/agent"
                    , verificationUriComplete =
                        "https://platform.digitallyinduced.com/connect/agent?user_code=ABCD-1234"
                    , expiresInSeconds = 600
                    , pollIntervalSeconds = 5
                    }
        validateGatewayDeviceAuthorization defaultGatewayBaseUrl device
            `shouldBe` Right device
        validateGatewayDeviceAuthorization
            defaultGatewayBaseUrl
            device
                { verificationUriComplete =
                    "https://example.com/connect/agent?user_code=ABCD-1234"
                }
            `shouldBe`
                Left
                    "The gateway returned a verification URL for a different origin."

    it "rejects malformed native gateway inputs before network access" do
        startNativeGatewayAuthorization defaultGatewayBaseUrl ""
            `shouldReturn`
                Left
                    "Gateway client name must contain between 1 and 160 characters."
        pollNativeGatewayAuthorizationAndSave defaultGatewayBaseUrl ""
            `shouldReturn` Left "Gateway device code is invalid."
        exchangeNativeGatewayAuthorizationCode
            defaultGatewayBaseUrl
            "wrong-client"
            "authorization-code"
            "0123456789abcdefghijklmnopqrstuvwxyzABCDEFG"
            "haskell-agent-auth://gateway/callback"
            `shouldReturn` Left "Gateway OAuth client ID is invalid."
        exchangeNativeGatewayAuthorizationCode
            defaultGatewayBaseUrl
            "haskell-agent-macos"
            "authorization-code"
            "0123456789abcdefghijklmnopqrstuvwxyzABCDEFG"
            "https://example.com/callback"
            `shouldReturn` Left "Gateway OAuth redirect URI is invalid."

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

    it "allows an interactive caller to cancel the callback wait" do
        connectGatewayBrowserWithCancel
            defaultGatewayBaseUrl
            "Haskell Agent CLI"
            (const (pure True))
            (pure ())
            `shouldReturn`
                Left "Gateway browser authorization was cancelled."

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

    it "serializes credential writes with boundary-critical actions" $
        withTempHome \home -> do
            let initial =
                    GatewayCredential
                        "https://gateway"
                        "wss://gateway/v1/responses"
                        "initial-secret"
                replacement =
                    initial { gatewayAccessToken = "replacement-secret" }
            saveGatewayCredentialAt home initial `shouldReturn` Right ()
            locked <- newEmptyMVar
            release <- newEmptyMVar
            writerStarted <- newEmptyMVar
            withAsync
                (withGatewayCredentialLockAt home do
                    putMVar locked ()
                    takeMVar release)
                \holder -> do
                    takeMVar locked
                    withAsync
                        (putMVar writerStarted ()
                            >> saveGatewayCredentialAt home replacement)
                        \writer -> do
                            takeMVar writerStarted
                            threadDelay 100000
                            writerState <- poll writer
                            writerState `shouldSatisfy` isNothing
                            loadGatewayCredentialAt home
                                `shouldReturn` Right (Just initial)
                            putMVar release ()
                            wait holder
                            wait writer `shouldReturn` Right ()
            loadGatewayCredentialAt home
                `shouldReturn` Right (Just replacement)

    it "allows independent gateway credential leases to overlap" $
        withTempHome \home -> do
            firstEntered <- newEmptyMVar
            secondEntered <- newEmptyMVar
            release <- newEmptyMVar
            withAsync
                (withGatewayCredentialLeaseAt home do
                    putMVar firstEntered ()
                    takeMVar release)
                \first -> do
                    takeMVar firstEntered
                    withAsync
                        (withGatewayCredentialLeaseAt home do
                            putMVar secondEntered ()
                            takeMVar release)
                        \second -> do
                            timeout 1000000 (takeMVar secondEntered)
                                `shouldReturn` Just ()
                            putMVar release ()
                            putMVar release ()
                            wait first
                            wait second

    it "waits for every gateway credential lease before writing" $
        withTempHome \home -> do
            let initial =
                    GatewayCredential
                        "https://gateway"
                        "wss://gateway/v1/responses"
                        "initial-secret"
                replacement =
                    initial { gatewayAccessToken = "replacement-secret" }
            saveGatewayCredentialAt home initial `shouldReturn` Right ()
            firstEntered <- newEmptyMVar
            secondEntered <- newEmptyMVar
            releaseFirst <- newEmptyMVar
            releaseSecond <- newEmptyMVar
            withAsync
                (withGatewayCredentialLeaseAt home do
                    putMVar firstEntered ()
                    takeMVar releaseFirst)
                \first ->
                    withAsync
                        (withGatewayCredentialLeaseAt home do
                            putMVar secondEntered ()
                            takeMVar releaseSecond)
                        \second -> do
                            takeMVar firstEntered
                            takeMVar secondEntered
                            withAsync
                                (saveGatewayCredentialAt home replacement)
                                \writer -> do
                                    threadDelay 100000
                                    poll writer
                                        >>= (`shouldSatisfy` isNothing)
                                    putMVar releaseFirst ()
                                    wait first
                                    threadDelay 100000
                                    poll writer
                                        >>= (`shouldSatisfy` isNothing)
                                    putMVar releaseSecond ()
                                    wait second
                                    wait writer `shouldReturn` Right ()
            loadGatewayCredentialAt home
                `shouldReturn` Right (Just replacement)

    it "lets callbacks join an active lease phase, then hands off to a writer" $
        withTempHome \home -> do
            firstEntered <- newEmptyMVar
            releaseFirst <- newEmptyMVar
            writerEntered <- newEmptyMVar
            releaseWriter <- newEmptyMVar
            secondEntered <- newEmptyMVar
            releaseSecond <- newEmptyMVar
            thirdEntered <- newEmptyMVar
            withAsync
                (withGatewayCredentialLeaseAt home do
                    putMVar firstEntered ()
                    takeMVar releaseFirst)
                \first -> do
                    takeMVar firstEntered
                    withAsync
                        (withGatewayCredentialLockAt home do
                            putMVar writerEntered ()
                            takeMVar releaseWriter)
                        \writer -> do
                            threadDelay 100000
                            poll writer
                                >>= (`shouldSatisfy` isNothing)
                            withAsync
                                (withGatewayCredentialLeaseAt home $
                                    putMVar secondEntered ()
                                        >> takeMVar releaseSecond)
                                \second -> do
                                    timeout 1000000 (takeMVar secondEntered)
                                        `shouldReturn` Just ()
                                    putMVar releaseFirst ()
                                    wait first
                                    threadDelay 100000
                                    poll writer
                                        >>= (`shouldSatisfy` isNothing)
                                    putMVar releaseSecond ()
                                    wait second
                                    timeout 1000000
                                        (takeMVar writerEntered)
                                        `shouldReturn` Just ()
                                    withAsync
                                        (withGatewayCredentialLeaseAt home $
                                            putMVar thirdEntered ())
                                        \third -> do
                                            threadDelay 100000
                                            poll third
                                                >>= (`shouldSatisfy` isNothing)
                                            putMVar releaseWriter ()
                                            wait writer
                                            wait third
                                            takeMVar thirdEntered

    it "does not start a new turn lease ahead of a waiting writer" $
        withTempHome \home -> do
            callbackEntered <- newEmptyMVar
            releaseCallback <- newEmptyMVar
            writerEntered <- newEmptyMVar
            releaseWriter <- newEmptyMVar
            turnEntered <- newEmptyMVar
            withAsync
                (withGatewayCredentialLeaseAt home do
                    putMVar callbackEntered ()
                    takeMVar releaseCallback)
                \callbackLease -> do
                    takeMVar callbackEntered
                    withAsync
                        (withGatewayCredentialLockAt home do
                            putMVar writerEntered ()
                            takeMVar releaseWriter)
                        \writer -> do
                            threadDelay 100000
                            poll writer
                                >>= (`shouldSatisfy` isNothing)
                            withAsync
                                (withGatewayCredentialTurnLeaseAt home $
                                    putMVar turnEntered ())
                                \turnLease -> do
                                    threadDelay 100000
                                    poll turnLease
                                        >>= (`shouldSatisfy` isNothing)
                                    putMVar releaseCallback ()
                                    wait callbackLease
                                    timeout 1000000
                                        (takeMVar writerEntered)
                                        `shouldReturn` Just ()
                                    poll turnLease
                                        >>= (`shouldSatisfy` isNothing)
                                    putMVar releaseWriter ()
                                    wait writer
                                    wait turnLease
                                    takeMVar turnEntered

    it "releases a cancelled gateway credential lease" $
        withTempHome \home -> do
            entered <- newEmptyMVar
            blocked <- newEmptyMVar
            withAsync
                (withGatewayCredentialLeaseAt home do
                    putMVar entered ()
                    takeMVar blocked)
                \holder -> do
                    takeMVar entered
                    cancel holder
                    void (waitCatch holder)
                    timeout 1000000
                        (withGatewayCredentialLockAt home (pure ()))
                        `shouldReturn` Just ()

    it "unregisters a credential writer cancelled while waiting" $
        withTempHome \home -> do
            leaseEntered <- newEmptyMVar
            releaseLease <- newEmptyMVar
            writerStarted <- newEmptyMVar
            withAsync
                (withGatewayCredentialLeaseAt home do
                    putMVar leaseEntered ()
                    takeMVar releaseLease)
                \lease -> do
                    takeMVar leaseEntered
                    withAsync
                        (putMVar writerStarted ()
                            >> withGatewayCredentialLockAt home (pure ()))
                        \writer -> do
                            takeMVar writerStarted
                            threadDelay 100000
                            poll writer
                                >>= (`shouldSatisfy` isNothing)
                            cancel writer
                            void (waitCatch writer)
                    putMVar releaseLease ()
                    wait lease
                    timeout 1000000
                        (withGatewayCredentialLeaseAt home (pure ()))
                        `shouldReturn` Just ()

    it "keeps transition completion inside the credential writer boundary" $
        withTempHome \home ->
            withHomeEnvironment home do
                let credential =
                        GatewayCredential
                            "https://gateway"
                            "wss://gateway/v1/responses"
                            "transition-secret"
                saveGatewayCredentialAt home credential `shouldReturn` Right ()
                callbackEntered <- newEmptyMVar
                releaseCallback <- newEmptyMVar
                readerEntered <- newEmptyMVar
                withAsync
                    (removeGatewayCredentialWith do
                        loadGatewayCredentialAt home
                            `shouldReturn` Right Nothing
                        putMVar callbackEntered ()
                        takeMVar releaseCallback)
                    \transition -> do
                        takeMVar callbackEntered
                        withAsync
                            (withGatewayCredentialLeaseAt home $
                                putMVar readerEntered ())
                            \reader -> do
                                threadDelay 100000
                                poll reader
                                    >>= (`shouldSatisfy` isNothing)
                                putMVar releaseCallback ()
                                wait transition `shouldReturn` Right ()
                                timeout 1000000 (takeMVar readerEntered)
                                    `shouldReturn` Just ()
                                wait reader

    it "serializes credential writes against leases across processes" $
        withTempHome \home -> do
            let homePath =
                    either (error . show) id (decodeUtf home)
                locked = homePath </> "child-locked"
                release = homePath </> "release-child"
                credential =
                    GatewayCredential
                        "https://gateway"
                        "wss://gateway/v1/responses"
                        "cross-process-secret"
            bracket
                (forkProcess $
                    withGatewayCredentialLeaseAt home do
                        writeFile locked ""
                        waitForFile release)
                (\pid -> do
                    writeFile release ""
                    void (tryAny (signalProcess sigKILL pid))
                    void (tryAny (getProcessStatus False False pid)))
                \pid -> do
                    waitForFile locked
                    writerStarted <- newEmptyMVar
                    withAsync
                        (putMVar writerStarted ()
                            >> saveGatewayCredentialAt home credential)
                        \writer -> do
                            takeMVar writerStarted
                            threadDelay 100000
                            writerState <- poll writer
                            writerState `shouldSatisfy` isNothing
                            loadGatewayCredentialAt home
                                `shouldReturn` Right Nothing
                            writeFile release ""
                            _ <- getProcessStatus True False pid
                            wait writer `shouldReturn` Right ()
            loadGatewayCredentialAt home
                `shouldReturn` Right (Just credential)

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
                    "wss://other.example/v1/responses"
                    "secret")
                `shouldReturn`
                    Left
                        "Gateway base and WebSocket URLs must use the same origin."
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

waitForFile :: FilePath -> IO ()
waitForFile path = go (100 :: Int)
  where
    go remaining
        | remaining <= 0 =
            expectationFailure ("timed out waiting for " <> path)
        | otherwise =
            doesFileExist path >>= \case
                True -> pure ()
                False -> threadDelay 10000 >> go (remaining - 1)

withHomeEnvironment :: OsPath -> IO value -> IO value
withHomeEnvironment home action =
    bracket
        (lookupEnv "HOME")
        (\case
            Just previous -> setEnv "HOME" previous
            Nothing -> unsetEnv "HOME")
        \_ -> do
            setEnv "HOME" $
                either (error . show) id (decodeUtf home)
            action
