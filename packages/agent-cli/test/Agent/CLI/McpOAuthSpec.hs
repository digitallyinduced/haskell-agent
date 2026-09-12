module Agent.CLI.McpOAuthSpec (spec) where

import Agent.CLI.McpOAuth
import Agent.CLI.McpOAuthStore (loadMcpOAuthRecord, saveMcpOAuthRecord, mcpOAuthStorePath)
import qualified Agent.MCP.OAuth as OAuth
import Control.Concurrent.Async (cancel, concurrently, waitCatch, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception.Safe (bracket, tryAny)
import Control.Monad (forM_)
import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as Bytes
import qualified Data.ByteString.Lazy as LazyBytes
import Data.Either (isLeft)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import Network.HTTP.Client (defaultManagerSettings, httpLbs, newManager, parseRequest, responseStatus, responseHeaders, responseBody, method)
import Network.HTTP.Types (ResponseHeaders, status200, status400, status401, status404, status405, mkStatus)
import Network.HTTP.Types.URI (Query, parseQuery, renderQuery)
import Network.URI (parseURI, uriQuery)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.Directory as Directory
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.Posix.Temp (mkdtemp)
import System.OsPath (decodeUtf, unsafeEncodeUtf)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "MCP OAuth host authorization" do
    forM_ [False, True] \cli -> describe (if cli then "CLI workflow" else "runtime workflow") do
        it "retains a reused dynamic client secret and binds PKCE and resource through exchange" $
            withOAuthHome do
                registrations <- newIORef (0 :: Int)
                exchanges <- newIORef []
                urls <- newIORef []
                Warp.testWithApplication (pure (recordingFixture registrations exchanges)) \port -> do
                    let endpoint = "http://127.0.0.1:" <> Text.pack (show port) <> "/mcp"
                    first <- runRecordedAuthorization cli endpoint Nothing urls
                    let (previousFile, previousExtra) = first
                        previous = (previousFile, previousExtra { OAuth.extraScope = Just "requested-scope" })
                    second <- runRecordedAuthorization cli endpoint (Just previous) urls
                    readIORef registrations `shouldReturn` 1
                    snd second `shouldBe` snd first
                    (snd second).extraClientSecret `shouldBe` Just "dynamic-secret"
                    (snd first).extraScope `shouldBe` Just "server-granted"
                    (fst second).tokenAccessToken `shouldBe` "access-token"
                    (fst second).tokenRefreshToken `shouldBe` "refresh-token"
                    (fst second).tokenEndpoint `shouldBe` Text.dropEnd 4 endpoint <> "/token"
                    (fst second).tokenExpiresAt `shouldSatisfy` (/= Nothing)
                    authQueries <- readIORef urls
                    tokenQueries <- readIORef exchanges
                    length authQueries `shouldBe` 2
                    length tokenQueries `shouldBe` 2
                    lookup "scope" (last authQueries) `shouldBe` Just (Just "requested-scope")
                    forM_ (zip authQueries tokenQueries) \(authQuery, tokenQuery) -> do
                        lookup "client_secret" tokenQuery `shouldBe` Just (Just "dynamic-secret")
                        lookup "resource" authQuery `shouldBe` Just (Just (Encoding.encodeUtf8 endpoint))
                        lookup "resource" tokenQuery `shouldBe` lookup "resource" authQuery
                        lookup "redirect_uri" tokenQuery `shouldBe` lookup "redirect_uri" authQuery
                        lookup "code_challenge_method" authQuery `shouldBe` Just (Just "S256")
                        verifier <- maybe (fail "missing verifier") pure (lookup "code_verifier" tokenQuery >>= id)
                        let challenge = Base64.encodeUnpadded (BA.convert (hash verifier :: Digest SHA256))
                        lookup "code_challenge" authQuery `shouldBe` Just (Just challenge)

        forM_ ["missing resource", "different resource", "different issuer"] \binding ->
            it ("does not reuse registration or scopes with " <> binding) $
                withOAuthHome do
                    registrations <- newIORef (0 :: Int)
                    exchanges <- newIORef []
                    urls <- newIORef []
                    Warp.testWithApplication (pure (recordingFixture registrations exchanges)) \port -> do
                        let endpoint = "http://127.0.0.1:" <> Text.pack (show port) <> "/mcp"
                        (file, extra) <- runRecordedAuthorization cli endpoint Nothing urls
                        let previous = (file, extra
                                { OAuth.extraResource = case binding of
                                    "missing resource" -> Nothing
                                    "different resource" -> Just "https://other.example/mcp"
                                    _ -> extra.extraResource
                                , OAuth.extraIssuer = if binding == "different issuer"
                                    then Just "https://other.example" else extra.extraIssuer
                                , OAuth.extraScope = Just "private-old-scope"
                                })
                        _ <- runRecordedAuthorization cli endpoint (Just previous) urls
                        readIORef registrations `shouldReturn` 2
                        queries <- readIORef urls
                        lookup "scope" (last queries) `shouldBe` Nothing

        it "uses requested scopes when the token response omits scope, refresh token, and expiry" $
            withOAuthHome do
                registrations <- newIORef (0 :: Int)
                exchanges <- newIORef []
                urls <- newIORef []
                omitOptional <- newIORef False
                let fixture request respond = do
                        omit <- readIORef omitOptional
                        if omit && Wai.rawPathInfo request == "/token"
                            then respond (Wai.responseLBS status200 [("Content-Type", "application/json")]
                                "{\"access_token\":\"minimal-token\",\"token_type\":\"Bearer\"}")
                            else recordingFixture registrations exchanges request respond
                Warp.testWithApplication (pure fixture) \port -> do
                    let endpoint = "http://127.0.0.1:" <> Text.pack (show port) <> "/mcp"
                    first <- runRecordedAuthorization cli endpoint Nothing urls
                    modifyIORef' omitOptional (const True)
                    (file, extra) <- runRecordedAuthorization cli endpoint (Just first) urls
                    file.tokenAccessToken `shouldBe` "minimal-token"
                    file.tokenRefreshToken `shouldBe` ""
                    file.tokenExpiresAt `shouldBe` Nothing
                    extra.extraScope `shouldBe` Just "server-granted"
                    queries <- readIORef urls
                    lookup "scope" (last queries) `shouldBe` Just (Just "server-granted")

    it "aborts unreadable runtime storage with a redacted error but lets CLI warn and recover" $
        withOAuthHome do
            registrations <- newIORef (0 :: Int)
            exchanges <- newIORef []
            urls <- newIORef []
            notices <- newIORef []
            Warp.testWithApplication (pure (recordingFixture registrations exchanges)) \port -> do
                let endpoint = "http://127.0.0.1:" <> Text.pack (show port) <> "/mcp"
                    runtimeHost = McpOAuthHost
                        { oauthLoadPrevious = pure (Left "private-storage-detail")
                        , oauthOpenBrowser = \_ -> fail "browser must not open"
                        }
                authorizeMcpWith runtimeHost defaultLoginOptions Nothing endpoint
                    `shouldReturn` Left "MCP credentials could not be read from protected storage."
                readIORef registrations `shouldReturn` 0
                _ <- runRecordedAuthorization True endpoint Nothing urls
                home <- Directory.getHomeDirectory
                path <- decodeUtf (mcpOAuthStorePath (unsafeEncodeUtf home) endpoint)
                Bytes.writeFile path "invalid json"
                _ <- runRecordedAuthorizationWithSay (\notice -> modifyIORef' notices (<> [notice]))
                    True endpoint Nothing urls
                readIORef registrations `shouldReturn` 2
                readIORef notices >>= (`shouldSatisfy`
                    any (Text.isPrefixOf "Warning: ignoring unreadable MCP OAuth record:"))

    it "rejects an unsafe CLI server endpoint before any network request" $
        withOAuthHome do
            requests <- newIORef (0 :: Int)
            let fixture _ respond = do
                    modifyIORef' requests (+ 1)
                    respond (Wai.responseLBS status404 [] "")
                host = McpLoginHost
                    { mcpLoginSay = const (pure ())
                    , mcpLoginAuthorize = \_ _ -> fail "browser must not open"
                    }
            Warp.testWithApplication (pure fixture) \port -> do
                result <- loginMcpWithHost host defaultLoginOptions
                    ("http://user:secret@127.0.0.1:" <> Text.pack (show port) <> "/mcp")
                result `shouldSatisfy` isLeft
                readIORef requests `shouldReturn` 0

    it "rejects unsafe discovered endpoints in the CLI before registration or browser presentation" $
        withOAuthHome $
            forM_ [True, False] \browserEndpoint -> do
                effects <- newIORef []
                let unsafe = "https://user:secret@example.test/authorize"
                    fixture request respond = do
                        if Wai.rawPathInfo request == "/register"
                            then modifyIORef' effects (<> ["registration"])
                            else pure ()
                        authorizationFixtureWithEndpoints
                            (if browserEndpoint then const unsafe else id)
                            (if browserEndpoint then id else const unsafe)
                            effects request respond
                    host = McpLoginHost
                        { mcpLoginSay = const (pure ())
                        , mcpLoginAuthorize = \_ _ -> do
                            modifyIORef' effects (<> ["browser"])
                            pure (Left "unexpected browser")
                        }
                Warp.testWithApplication (pure fixture) \port -> do
                    result <- loginMcpWithHost host defaultLoginOptions
                        ("http://127.0.0.1:" <> Text.pack (show port) <> "/mcp")
                    result `shouldSatisfy` isLeft
                    readIORef effects `shouldReturn` []

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

    it "renders rejected callback pages without consuming pending authorization" do
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
                responseBody response `shouldBe` OAuth.oauthCallbackPage OAuth.OAuthCallbackRejected
                assertCallbackHeaders (responseHeaders response)
                LazyBytes.toStrict (responseBody response) `shouldSatisfy` (not . Bytes.isInfixOf "untrusted-account")
                notFoundRequest <- parseRequest (Bytes.unpack (redirect <> "/missing"))
                notFoundResponse <- httpLbs notFoundRequest manager
                responseStatus notFoundResponse `shouldBe` status404
                responseBody notFoundResponse `shouldBe` OAuth.oauthCallbackPage OAuth.OAuthCallbackNotFound
                assertCallbackHeaders (responseHeaders notFoundResponse)
                methodResponse <- httpLbs (request { method = "POST" }) manager
                responseStatus methodResponse `shouldBe` status405
                responseBody methodResponse `shouldBe` OAuth.oauthCallbackPage OAuth.OAuthCallbackMethodNotAllowed
                assertCallbackHeaders (responseHeaders methodResponse)
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
            response <- httpLbs request manager
            responseStatus response `shouldBe` status200
            responseBody response `shouldBe` OAuth.oauthCallbackSuccessPage
            assertCallbackHeaders (responseHeaders response)
            pure ()
    result <- timeout (10 * 1000000) $
        concurrently (authorizeMcpWith host defaultLoginOptions Nothing endpoint) complete
    pure (maybe (Left "Fixture authorization timed out") fst result)

assertCallbackHeaders :: ResponseHeaders -> Expectation
assertCallbackHeaders headers = do
    lookup "Content-Type" headers `shouldBe` Just "text/html; charset=utf-8"
    lookup "Cache-Control" headers `shouldBe` Just "no-store"
    lookup "Content-Security-Policy" headers
        `shouldBe` Just "default-src 'none'; style-src 'unsafe-inline'"
    lookup "X-Content-Type-Options" headers `shouldBe` Just "nosniff"

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

-- Exercise both public adapters, including CLI persistence, against the same
-- protocol fixture. A fresh HOME ensures runtime authorization never silently
-- writes credentials and CLI tests cannot touch the developer's credentials.
withOAuthHome :: IO a -> IO a
withOAuthHome action = do
    temporary <- Directory.getTemporaryDirectory
    bracket (mkdtemp (temporary </> "mcp-oauth-")) Directory.removePathForcibly \home ->
        bracket (lookupEnv "HOME") (maybe (unsetEnv "HOME") (setEnv "HOME")) \_ -> do
            setEnv "HOME" home
            action

runRecordedAuthorization
    :: Bool
    -> Text.Text
    -> Maybe (OAuth.OAuthTokenFile, OAuth.OAuthTokenFileExtra)
    -> IORef [Query]
    -> IO (OAuth.OAuthTokenFile, OAuth.OAuthTokenFileExtra)
runRecordedAuthorization = runRecordedAuthorizationWithSay (const (pure ()))

-- Keep the same fixture usable for a tmux/GHCi smoke with the real CLI
-- presentation: pass defaultMcpLoginHost.mcpLoginSay instead of silence.
runRecordedAuthorizationWithSay
    :: (Text.Text -> IO ())
    -> Bool
    -> Text.Text
    -> Maybe (OAuth.OAuthTokenFile, OAuth.OAuthTokenFileExtra)
    -> IORef [Query]
    -> IO (OAuth.OAuthTokenFile, OAuth.OAuthTokenFileExtra)
runRecordedAuthorizationWithSay say cli endpoint previous urls = do
    let complete url = do
            uri <- maybe (fail "invalid authorization URL") pure (parseURI (Text.unpack url))
            modifyIORef' urls (<> [parseQuery (Bytes.pack (uriQuery uri))])
            (redirect, state, issuer) <- authorizationCallbackParameters endpoint url
            manager <- newManager defaultManagerSettings
            request <- parseRequest (Bytes.unpack (redirect <> renderQuery True
                [("state", Just state), ("iss", Just issuer), ("code", Just "account")]))
            response <- httpLbs request manager
            responseStatus response `shouldBe` status200
    if cli
        then do
            forM_ previous \(file, extra) ->
                saveMcpOAuthRecord endpoint file extra `shouldReturn` Right ()
            let host = defaultMcpLoginHost
                    { mcpLoginSay = say
                    , mcpLoginAuthorize = \url await -> do
                        complete url
                        Right <$> timeout (10 * 1000000) await
                    }
            result <- loginMcpWithHost host defaultLoginOptions endpoint
            either (fail . Text.unpack) (const (pure ())) result
            loadMcpOAuthRecord endpoint >>= either (fail . Text.unpack)
                (maybe (fail "CLI did not persist tokens") pure)
        else do
            let host = McpOAuthHost
                    { oauthLoadPrevious = pure (Right previous)
                    , oauthOpenBrowser = \url -> complete url >> pure (Right ())
                    }
            result <- authorizeMcpWith host defaultLoginOptions Nothing endpoint
            loadMcpOAuthRecord endpoint `shouldReturn` Right Nothing
            either (fail . Text.unpack) pure result

recordingFixture :: IORef Int -> IORef [Query] -> Wai.Application
recordingFixture registrations exchanges request respond =
    let json value = respond (Wai.responseLBS status200 [("Content-Type", "application/json")] (Aeson.encode value))
    in case Wai.rawPathInfo request of
        "/register" -> do
            modifyIORef' registrations (+ 1)
            json (Aeson.object
                [ "client_id" Aeson..= ("fixture-client" :: Text.Text)
                , "client_secret" Aeson..= ("dynamic-secret" :: Text.Text)
                ])
        "/token" -> do
            body <- Wai.strictRequestBody request
            modifyIORef' exchanges (<> [parseQuery (LazyBytes.toStrict body)])
            json (Aeson.object
                [ "access_token" Aeson..= ("access-token" :: Text.Text)
                , "refresh_token" Aeson..= ("refresh-token" :: Text.Text)
                , "token_type" Aeson..= ("Bearer" :: Text.Text)
                , "expires_in" Aeson..= (3600 :: Int)
                , "scope" Aeson..= ("server-granted" :: Text.Text)
                ])
        _ -> do
            ignored <- newIORef []
            authorizationFixture ignored request respond
