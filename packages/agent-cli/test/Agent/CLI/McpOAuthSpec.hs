module Agent.CLI.McpOAuthSpec (spec) where

import Agent.CLI.McpOAuth
import qualified Agent.MCP.OAuth as OAuth
import Control.Concurrent.Async (cancel, concurrently, waitCatch, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception.Safe (tryAny)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as Bytes
import qualified Data.ByteString.Lazy as LazyBytes
import Data.Either (isLeft)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import Network.HTTP.Client (defaultManagerSettings, httpLbs, newManager, parseRequest, responseStatus)
import Network.HTTP.Types (status200, status400, status401, status404, mkStatus)
import Network.HTTP.Types.URI (Query, parseQuery, renderQuery)
import Network.URI (parseURI, uriQuery)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "MCP OAuth host authorization" do
    it "rejects unsafe discovered browser and token endpoints before registration or browser presentation" do
        mapM_ (\unsafe -> mapM_ (\browserEndpoint -> do
            effects <- newIORef []
            let fixture request respond = do
                    if Wai.rawPathInfo request == "/register"
                        then modifyIORef' effects (<> ["registration"])
                        else pure ()
                    authorizationFixtureWithEndpoints
                        (if browserEndpoint then const unsafe else id)
                        (if browserEndpoint then id else const unsafe)
                        effects request respond
                host = McpOAuthHost
                    { oauthLoadPrevious = pure (Right Nothing)
                    , oauthOpenBrowser = \_ -> modifyIORef' effects (<> ["browser"]) >> pure (Right ())
                    }
            Warp.testWithApplication (pure fixture) \port -> do
                let endpoint = "http://127.0.0.1:" <> Text.pack (show port) <> "/mcp"
                result <- authorizeMcpWith host defaultLoginOptions Nothing endpoint
                result `shouldBe` Left "OAuth metadata contains an unsafe authorization or token endpoint."
                readIORef effects `shouldReturn` []
            ) [True, False])
            ["http://remote.example/authorize", "custom://example/authorize"
            , "https://user:secret@example.test/authorize"
            , "https://example.test/authorize#fragment", "https://example.test:99999/authorize"]

    it "never follows OAuth endpoint redirects or replays credentials" do
        mapM_ (\code -> do
            forwarded <- newIORef (0 :: Int)
            let destination _ respond = do
                    modifyIORef' forwarded (+ 1)
                    respond (Wai.responseLBS status200 [] "{}")
            Warp.testWithApplication (pure destination) \destinationPort -> do
                let location = Bytes.pack ("http://127.0.0.1:" <> show destinationPort <> "/capture")
                    redirect _ respond = respond (Wai.responseLBS (mkStatus code "Redirect") [("Location", location)] "")
                Warp.testWithApplication (pure redirect) \port -> do
                    let endpoint = "http://127.0.0.1:" <> Text.pack (show port)
                    manager <- newManager defaultManagerSettings
                    token <- OAuth.refreshAccessToken manager endpoint "client" "refresh-secret"
                    case token of
                        OAuth.OAuthTokenFailure _ -> pure ()
                        _ -> expectationFailure "Redirected refresh succeeded"
                    OAuth.registerClient manager endpoint ["http://127.0.0.1/callback"] []
                        >>= (`shouldSatisfy` isLeft)
                    OAuth.discoverAuthorizationServerMetadata manager endpoint
                        >>= (`shouldSatisfy` isLeft)
                    readIORef forwarded `shouldReturn` 0
            ) [301, 302, 303, 307, 308]

    it "rejects insecure or malformed token endpoints before sending secrets" do
        manager <- newManager defaultManagerSettings
        mapM_ (\endpoint -> do
            result <- OAuth.refreshAccessToken manager endpoint "client" "refresh-secret"
            case result of
                OAuth.OAuthTokenFailure message ->
                    message `shouldSatisfy` (not . Text.isInfixOf "refresh-secret")
                _ -> expectationFailure "Unsafe token endpoint accepted"
            ) ["http://remote.example/token", "https://user:password@example.test/token"
              , "https://example.test/token#fragment", "https://example.test:99999/token"]

    it "accepts an issuer-bound callback with the expected state" do
        validateMcpOAuthCallback True issuer "expected-state" success
            `shouldBe` Right ()

    it "rejects state from another authorization" do
        validateMcpOAuthCallback True issuer "different-state" success
            `shouldSatisfy` isLeft

    it "rejects an issuer from another authorization server" do
        validateMcpOAuthCallback True "https://other.example" "expected-state" success
            `shouldSatisfy` isLeft

    it "rejects duplicate security parameters" do
        mapM_ (\parameter ->
            validateMcpOAuthCallback True issuer "expected-state" (parameter : success)
                `shouldSatisfy` isLeft)
            [ ("state", Just "expected-state")
            , ("iss", Just issuerBytes)
            , ("code", Just "second-code")
            ]

    it "validates state on denied consent too" do
        let denied = filter ((/= "code") . fst) success <> [("error", Just "access_denied")]
        validateMcpOAuthCallback True issuer "expected-state" denied
            `shouldBe` Right ()
        validateMcpOAuthCallback True issuer "other-state" denied
            `shouldSatisfy` isLeft

    it "rejects simultaneous success and error and empty codes" do
        validateMcpOAuthCallback True issuer "expected-state"
            (success <> [("error", Just "access_denied")]) `shouldSatisfy` isLeft
        validateMcpOAuthCallback True issuer "expected-state"
            (filter ((/= "code") . fst) success <> [("code", Just "")]) `shouldSatisfy` isLeft

    it "allows absent issuer only when the server did not advertise issuer responses" do
        let withoutIssuer = filter ((/= "iss") . fst) success
        validateMcpOAuthCallback False issuer "expected-state" withoutIssuer
            `shouldBe` Right ()
        validateMcpOAuthCallback True issuer "expected-state" withoutIssuer
            `shouldSatisfy` isLeft

    it "does not treat a malformed optional issuer as absent" do
        let withoutIssuer = filter ((/= "iss") . fst) success
        validateMcpOAuthCallback False issuer "expected-state"
            (("iss", Just (Bytes.pack ['\255'])) : withoutIssuer) `shouldSatisfy` isLeft
        validateMcpOAuthCallback False issuer "expected-state"
            (("iss", Nothing) : withoutIssuer) `shouldSatisfy` isLeft

    it "rejects insecure endpoints before credential loading or presentation" do
        let host = McpOAuthHost
                { oauthLoadPrevious = expectationFailure "credentials must not be read" >> pure (Right Nothing)
                , oauthOpenBrowser = \_ -> expectationFailure "browser must not open" >> pure (Right ())
                }
        mapM_ (\endpoint -> do
            result <- authorizeMcpWith host defaultLoginOptions Nothing endpoint
            result `shouldSatisfy` isLeft)
            [ "http://example.com/mcp"
            , "https://user:secret@example.com/mcp"
            , "https://example.com/mcp#fragment"
            , "not-an-endpoint"
            ]

    it "authorizes two accounts at one endpoint without sharing records" do
        requests <- newIORef []
        Warp.testWithApplication (pure (authorizationFixture requests)) \port -> do
            let endpoint = "http://127.0.0.1:" <> Text.pack (show port) <> "/mcp"
            first <- authorizeFixture endpoint "account-a"
            second <- authorizeFixture endpoint "account-b"
            case (first, second) of
                (Right (firstTokens, firstExtra), Right (secondTokens, secondExtra)) -> do
                    firstTokens.tokenAccessToken `shouldBe` "token-account-a"
                    secondTokens.tokenAccessToken `shouldBe` "token-account-b"
                    firstExtra.extraResource `shouldBe` Just endpoint
                    secondExtra.extraResource `shouldBe` Just endpoint
                _ -> expectationFailure ("Authorization failed: " <> show (first, second))
            readIORef requests >>= (`shouldBe` ["account-a", "account-b"])

    it "does not expose token endpoint error bodies" do
        requests <- newIORef []
        Warp.testWithApplication (pure (authorizationFixture requests)) \port -> do
            let endpoint = "http://127.0.0.1:" <> Text.pack (show port) <> "/mcp"
            result <- authorizeFixture endpoint "rejected-account"
            result `shouldBe` Left "OAuth token exchange failed."

    it "reports browser presentation failure without exposing host details" do
        requests <- newIORef []
        Warp.testWithApplication (pure (authorizationFixture requests)) \port -> do
            let endpoint = "http://127.0.0.1:" <> Text.pack (show port) <> "/mcp"
                host = McpOAuthHost
                    { oauthLoadPrevious = pure (Right Nothing)
                    , oauthOpenBrowser = \_ -> pure (Left "private-host-details")
                    }
            result <- authorizeMcpWith host defaultLoginOptions Nothing endpoint
            result `shouldBe` Left "The authorization browser could not be opened."
            readIORef requests `shouldReturn` []

    it "propagates cancellation and closes the callback listener without exchanging tokens" do
        requests <- newIORef []
        Warp.testWithApplication (pure (authorizationFixture requests)) \port -> do
            authorization <- newEmptyMVar
            let endpoint = "http://127.0.0.1:" <> Text.pack (show port) <> "/mcp"
                host = McpOAuthHost
                    { oauthLoadPrevious = pure (Right Nothing)
                    , oauthOpenBrowser = \url -> putMVar authorization url >> pure (Right ())
                    }
            completed <- timeout (10 * 1000000) $
                withAsync (authorizeMcpWith host defaultLoginOptions Nothing endpoint) \operation -> do
                    url <- takeMVar authorization
                    (redirect, _, _) <- authorizationCallbackParameters endpoint url
                    cancel operation
                    waitCatch operation >>= (`shouldSatisfy` isLeft)
                    manager <- newManager defaultManagerSettings
                    request <- parseRequest (Bytes.unpack redirect)
                    tryAny (httpLbs request manager) >>= (`shouldSatisfy` isLeft)
            completed `shouldBe` Just ()
            readIORef requests `shouldReturn` []

    it "keeps authorization pending after an invalid callback and accepts the matching callback" do
        requests <- newIORef []
        Warp.testWithApplication (pure (authorizationFixture requests)) \port -> do
            let endpoint = "http://127.0.0.1:" <> Text.pack (show port) <> "/mcp"
            result <- authorizeFixtureWith endpoint "account-a" \redirect state issuer -> do
                manager <- newManager defaultManagerSettings
                request <- parseRequest (Bytes.unpack (redirect <> renderQuery True
                    [ ("state", Just (state <> "-incorrect"))
                    , ("iss", Just issuer)
                    , ("code", Just "untrusted-account")
                    ]))
                response <- httpLbs request manager
                responseStatus response `shouldBe` status400
            fmap ((.tokenAccessToken) . fst) result `shouldBe` Right "token-account-a"
            readIORef requests `shouldReturn` ["account-a"]
  where
    issuer = "https://authorization.example"
    issuerBytes = "https://authorization.example"
    success :: Query
    success =
        [ ("iss", Just issuerBytes)
        , ("state", Just "expected-state")
        , ("code", Just "authorization-code")
        ]

authorizeFixture
    :: Text.Text
    -> Bytes.ByteString
    -> IO (Either Text.Text (OAuth.OAuthTokenFile, OAuth.OAuthTokenFileExtra))
authorizeFixture endpoint account = do
    authorizeFixtureWith endpoint account (\_ _ _ -> pure ())

authorizeFixtureWith
    :: Text.Text
    -> Bytes.ByteString
    -> (Bytes.ByteString -> Bytes.ByteString -> Bytes.ByteString -> IO ())
    -> IO (Either Text.Text (OAuth.OAuthTokenFile, OAuth.OAuthTokenFileExtra))
authorizeFixtureWith endpoint account beforeCallback = do
    authorization <- newEmptyMVar
    let host = McpOAuthHost
            { oauthLoadPrevious = pure (Right Nothing)
            , oauthOpenBrowser = \url -> putMVar authorization url >> pure (Right ())
            }
        complete = do
            url <- takeMVar authorization
            (redirect, state, issuer) <- authorizationCallbackParameters endpoint url
            beforeCallback redirect state issuer
            let callback = redirect <> renderQuery True
                    [ ("state", Just state)
                    , ("iss", Just issuer)
                    , ("code", Just account)
                    ]
            manager <- newManager defaultManagerSettings
            request <- parseRequest (Bytes.unpack callback)
            _ <- httpLbs request manager
            pure ()
    result <- timeout (10 * 1000000) $
        concurrently (authorizeMcpWith host defaultLoginOptions Nothing endpoint) complete
    pure (maybe (Left "Fixture authorization timed out") fst result)

authorizationCallbackParameters
    :: Text.Text
    -> Text.Text
    -> IO (Bytes.ByteString, Bytes.ByteString, Bytes.ByteString)
authorizationCallbackParameters endpoint url = do
    uri <- maybe (fail "Invalid authorization URL") pure (parseURI (Text.unpack url))
    let query = parseQuery (Bytes.pack (uriQuery uri))
    redirect <- maybe (fail "Missing redirect") pure (lookup "redirect_uri" query >>= id)
    state <- maybe (fail "Missing state") pure (lookup "state" query >>= id)
    pure (redirect, state, Encoding.encodeUtf8 (Text.dropEnd 4 endpoint))

authorizationFixture :: IORef [Bytes.ByteString] -> Wai.Application
authorizationFixture = authorizationFixtureWithEndpoints id id

authorizationFixtureWithEndpoints
    :: (Text.Text -> Text.Text) -> (Text.Text -> Text.Text)
    -> IORef [Bytes.ByteString] -> Wai.Application
authorizationFixtureWithEndpoints browserEndpoint tokenEndpoint requests request respond = do
    let host = maybe "127.0.0.1" Encoding.decodeUtf8 (Wai.requestHeaderHost request)
        origin = "http://" <> host
        json value = respond (Wai.responseLBS status200 [("Content-Type", "application/json")] (Aeson.encode value))
    case Wai.rawPathInfo request of
        "/mcp" -> respond (Wai.responseLBS status401
            [("WWW-Authenticate", "Bearer resource_metadata=\"" <> Encoding.encodeUtf8 origin <> "/resource\"")] "")
        "/resource" -> json (Aeson.object
            [ "resource" Aeson..= (origin <> "/mcp")
            , "authorization_servers" Aeson..= [origin]
            ])
        "/.well-known/oauth-authorization-server" -> json (Aeson.object
            [ "issuer" Aeson..= origin
            , "authorization_endpoint" Aeson..= browserEndpoint (origin <> "/authorize")
            , "token_endpoint" Aeson..= tokenEndpoint (origin <> "/token")
            , "registration_endpoint" Aeson..= (origin <> "/register")
            , "code_challenge_methods_supported" Aeson..= ["S256" :: Text.Text]
            , "authorization_response_iss_parameter_supported" Aeson..= True
            ])
        "/register" -> json (Aeson.object ["client_id" Aeson..= ("fixture-client" :: Text.Text)])
        "/token" -> do
            body <- Wai.strictRequestBody request
            let query = parseQuery (LazyBytes.toStrict body)
                account = maybe "" id (lookup "code" query >>= id)
            modifyIORef' requests (<> [account])
            if account == "rejected-account"
                then respond (Wai.responseLBS status400 [("Content-Type", "application/json")]
                    "{\"error\":\"invalid_grant\",\"error_description\":\"credential-secret\"}")
                else json (Aeson.object
                [ "access_token" Aeson..= ("token-" <> Encoding.decodeUtf8 account)
                , "refresh_token" Aeson..= ("refresh-" <> Encoding.decodeUtf8 account)
                , "token_type" Aeson..= ("Bearer" :: Text.Text)
                , "expires_in" Aeson..= (3600 :: Int)
                ])
        _ -> respond (Wai.responseLBS status404 [] "")
