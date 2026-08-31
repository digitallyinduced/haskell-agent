module Agent.Gemini.AuthSpec (spec) where

import Agent.Gemini.Auth
import Agent.Gemini.TestSupport (withLoopbackApplication)
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.Async (concurrently)
import Control.Exception.Safe (bracket)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isLeft)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Network.HTTP.Types
import qualified Network.Socket as Net
import qualified Network.Socket.ByteString as Net
import Network.Wai
import Network.Wai.Handler.Warp (Port)
import qualified System.Timeout as Timeout
import Test.Hspec
import Text.Read (readMaybe)

spec :: Spec
spec = do
    describe "Google OAuth" do
        it "builds an offline PKCE-S256 authorization URL" do
            let url = authorizationUrl
                    defaultOAuthOptions
                    "http://127.0.0.1:43127/oauth2callback"
                    "state-value"
                    "challenge-value"
            url `shouldSatisfy` Text.isInfixOf "access_type=offline"
            url `shouldSatisfy` Text.isInfixOf "code_challenge=challenge-value"
            url `shouldSatisfy` Text.isInfixOf "code_challenge_method=S256"
            url `shouldSatisfy` Text.isInfixOf "cloud-platform"

        it "computes the RFC 7636 S256 test vector" do
            pkceChallenge
                "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"
                `shouldBe` "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"

        it "validates state and extracts the callback code" do
            validateOAuthCallback
                "expected"
                "/oauth2callback?code=abc123&state=expected"
                `shouldBe` Right "abc123"
            validateOAuthCallback
                "expected"
                "/oauth2callback?code=abc123&state=attacker"
                `shouldBe` Left
                    "Google OAuth state mismatch; authorization was rejected"
            validateOAuthCallback
                "expected"
                "/oauth2callback?error=access_denied&state=attacker"
                `shouldBe` Left
                    "Google OAuth state mismatch; authorization was rejected"

        it "accepts a callback request split across TCP reads" do
            recorded <- newIORef []
            withLoopbackApplication (pure (tokenApp recorded)) \port -> do
                presentedUrl <- newEmptyMVar
                let options = (testOAuthOptions port) { timeoutSeconds = 5 }
                (result, ()) <- concurrently
                    (runLoopbackOAuth options (putMVar presentedUrl))
                    (takeMVar presentedUrl >>= sendSplitOAuthCallback)
                result `shouldBe` Right OAuthTokens
                    { accessToken = "access-initial"
                    , refreshToken = Just "refresh-initial"
                    , expiresInSeconds = Just 3600
                    , tokenType = Just "Bearer"
                    , scope = Nothing
                    }

        it "exchanges a code and refreshes without losing the refresh token" do
            recorded <- newIORef []
            withLoopbackApplication (pure (tokenApp recorded)) \port -> do
                let options = testOAuthOptions port
                exchangeAuthorizationCode
                    options
                    "http://127.0.0.1/callback"
                    "verifier"
                    "code"
                    `shouldReturn` Right OAuthTokens
                        { accessToken = "access-initial"
                        , refreshToken = Just "refresh-initial"
                        , expiresInSeconds = Just 3600
                        , tokenType = Just "Bearer"
                        , scope = Nothing
                        }
                refreshAccessToken options "refresh-old"
                    `shouldReturn` Right OAuthTokens
                        { accessToken = "access-refreshed"
                        , refreshToken = Just "refresh-old"
                        , expiresInSeconds = Just 1800
                        , tokenType = Nothing
                        , scope = Nothing
                        }
                forms <- readIORef recorded
                forms `shouldSatisfy`
                    any (Text.isInfixOf "code_verifier=verifier")
                forms `shouldSatisfy`
                    any (Text.isInfixOf "refresh_token=refresh-old")

        it "fetches the account email with a bearer token" do
            withLoopbackApplication (pure userInfoApp) \port ->
                fetchUserEmail (testOAuthOptions port) "secret-access"
                    `shouldReturn` Right "person@example.com"

        it "bounds authenticated endpoint waits" do
            withLoopbackApplication (pure slowApp) \port -> do
                let options = (testOAuthOptions port) { timeoutSeconds = 1 }
                result <- Timeout.timeout 2_000_000
                    (fetchUserEmail options "secret-access")
                result `shouldSatisfy` \case
                    Just (Left err) ->
                        "Google HTTP request failed" `Text.isInfixOf` err
                    _ -> False

        it "redacts tokens and the installed client secret from Show" do
            let renderedTokens = show
                    (OAuthTokens "access-secret" (Just "refresh-secret")
                        Nothing Nothing Nothing)
            renderedTokens `shouldNotContain` "access-secret"
            renderedTokens `shouldNotContain` "refresh-secret"
            show defaultOAuthOptions
                `shouldNotContain` Text.unpack defaultOAuthOptions.clientSecret

        it "does not expose token-form secrets in endpoint errors" do
            withLoopbackApplication (pure echoingTokenErrorApp) \port -> do
                result <- refreshAccessToken
                    (testOAuthOptions port)
                    "refresh-must-stay-secret"
                show result `shouldNotContain` "refresh-must-stay-secret"
                show result `shouldNotContain` "server-echoed-sensitive-body"

        it "redacts bearer tokens echoed by authenticated endpoints" do
            withLoopbackApplication (pure echoingBearerErrorApp) \port -> do
                result <- fetchUserEmail
                    (testOAuthOptions port)
                    "access-must-stay-secret"
                show result `shouldNotContain` "access-must-stay-secret"
                show result `shouldContain` "<redacted>"

        it "never forwards OAuth credentials or token forms across redirects" do
            redirected <- newIORef []
            withLoopbackApplication (pure (redirectTargetApp redirected))
                \targetPort ->
                    withLoopbackApplication
                        (pure (redirectingApp targetPort))
                        \originPort -> do
                            refreshAccessToken
                                (testOAuthOptions originPort)
                                "refresh-must-stay-secret"
                                >>= (`shouldSatisfy` isLeft)
                            fetchUserEmail
                                (testOAuthOptions originPort)
                                "access-must-stay-secret"
                                >>= (`shouldSatisfy` isLeft)
                            setupCodeAssist
                                (testCodeAssistOptions originPort)
                                "access-must-stay-secret"
                                >>= (`shouldSatisfy` isLeft)
            readIORef redirected `shouldReturn` []

    describe "GeminiAuthState" do
        it "round-trips through JSON and redacts credentials from Show" do
            let state = GeminiAuthState
                    { accessToken = "access-secret"
                    , refreshToken = Just "refresh-secret"
                    , expiresAt = Nothing
                    , email = "person@example.com"
                    , projectId = "managed-project"
                    , userTier = "free-tier"
                    }
            Aeson.eitherDecode (Aeson.encode state) `shouldBe` Right state
            show state `shouldNotContain` "access-secret"
            show state `shouldNotContain` "refresh-secret"

    describe "Code Assist setup" do
        it "uses an existing Code Assist project without onboarding" do
            calls <- newIORef []
            withLoopbackApplication (pure (existingUserApp calls)) \port -> do
                result <- setupCodeAssist (testCodeAssistOptions port) "bearer"
                result `shouldBe` Right CodeAssistUser
                    { projectId = "managed-project"
                    , userTier = "free-tier"
                    , userTierName = Just "Gemini Code Assist"
                    }
                readIORef calls `shouldReturn`
                    ["/v1internal:loadCodeAssist"]

        it "prefers paid-tier metadata for an existing account" do
            withLoopbackApplication (pure paidUserApp) \port ->
                setupCodeAssist (testCodeAssistOptions port) "bearer"
                    `shouldReturn` Right CodeAssistUser
                        { projectId = "paid-project"
                        , userTier = "paid-tier"
                        , userTierName = Just "Google AI plan"
                        }

        it "fills missing paid-tier fields from the current tier" do
            withLoopbackApplication (pure partialPaidUserApp) \port ->
                setupCodeAssist (testCodeAssistOptions port) "bearer"
                    `shouldReturn` Right CodeAssistUser
                        { projectId = "paid-project"
                        , userTier = "standard-tier"
                        , userTierName = Just "Google AI plan"
                        }

        it "presents one-time account validation and retries setup" do
            calls <- newIORef (0 :: Int)
            presented <- newIORef []
            withLoopbackApplication (pure (validationRequiredApp calls)) \port -> do
                result <- setupCodeAssistWithValidation
                    (testCodeAssistOptions port)
                    "bearer"
                    (Just (\url description ->
                        modifyIORef' presented (<> [(url, description)])))
                result `shouldBe` Right CodeAssistUser
                    { projectId = "validated-project"
                    , userTier = "free-tier"
                    , userTierName = Just "Validated"
                    }
                readIORef calls `shouldReturn` 2
                readIORef presented `shouldReturn`
                    [("https://accounts.example.test/validate", "Verify please")]

        it "includes the configured project for standard-tier onboarding" do
            bodies <- newIORef []
            withLoopbackApplication (pure (standardOnboardingApp bodies)) \port -> do
                let options = (testCodeAssistOptions port)
                        { configuredProject = Just "billing-project" }
                setupCodeAssist options "bearer"
                    `shouldReturn` Right CodeAssistUser
                        { projectId = "billing-project"
                        , userTier = "standard-tier"
                        , userTierName = Just "Standard"
                        }
                onboardBody <- lookup "/v1internal:onboardUser"
                    <$> readIORef bodies
                onboardBody `shouldSatisfy` maybe False
                    (BS.isInfixOf "\"cloudaicompanionProject\":\"billing-project\"")
                onboardBody `shouldSatisfy` maybe False
                    (BS.isInfixOf "\"duetProject\":\"billing-project\"")

        it "rejects a failed onboarding operation even with a configured project" do
            withLoopbackApplication (pure failedOnboardingApp) \port -> do
                let options = (testCodeAssistOptions port)
                        { configuredProject = Just "billing-project" }
                setupCodeAssist options "bearer"
                    `shouldReturn` Left
                        "Gemini Code Assist onboarding failed (code 9): precondition failed"

        it "redacts bearer tokens from decoded setup errors" do
            withLoopbackApplication (pure echoingOnboardingErrorApp) \port -> do
                let options = (testCodeAssistOptions port)
                        { configuredProject = Just "billing-project" }
                result <- setupCodeAssist options "secret-bearer"
                show result `shouldNotContain` "secret-bearer"
                show result `shouldContain` "<redacted>"

        it "onboards the default free tier and polls the operation" do
            calls <- newIORef []
            bodies <- newIORef []
            withLoopbackApplication (pure (onboardingApp calls bodies)) \port -> do
                result <- setupCodeAssist (testCodeAssistOptions port) "bearer"
                result `shouldBe` Right CodeAssistUser
                    { projectId = "new-managed-project"
                    , userTier = "free-tier"
                    , userTierName = Just "Free"
                    }
                readIORef calls `shouldReturn`
                    [ "/v1internal:loadCodeAssist"
                    , "/v1internal:onboardUser"
                    , "/v1internal/operations/onboarding"
                    ]
                requestBodies <- readIORef bodies
                let loadBody = lookup "/v1internal:loadCodeAssist" requestBodies
                    onboardBody = lookup "/v1internal:onboardUser" requestBodies
                loadBody `shouldSatisfy` maybe False
                    (not . BS.isInfixOf "cloudaicompanionProject")
                loadBody `shouldSatisfy` maybe False
                    (not . BS.isInfixOf "duetProject")
                onboardBody `shouldSatisfy` maybe False
                    (not . BS.isInfixOf "cloudaicompanionProject")
                onboardBody `shouldSatisfy` maybe False
                    (not . BS.isInfixOf "duetProject")

tokenApp :: IORef [Text] -> Application
tokenApp recorded request respond = do
    body <- strictRequestBody request
    let textBody = TextEncoding.decodeUtf8 (LBS.toStrict body)
    modifyIORef' recorded (<> [textBody])
    let refreshing = "refresh_token=refresh-old" `Text.isInfixOf` textBody
        response
            | refreshing = Aeson.object
                [ "access_token" Aeson..= ("access-refreshed" :: Text)
                , "expires_in" Aeson..= (1800 :: Int)
                ]
            | otherwise = Aeson.object
                [ "access_token" Aeson..= ("access-initial" :: Text)
                , "refresh_token" Aeson..= ("refresh-initial" :: Text)
                , "expires_in" Aeson..= (3600 :: Int)
                , "token_type" Aeson..= ("Bearer" :: Text)
                ]
    respond (jsonResponse status200 response)

userInfoApp :: Application
userInfoApp request respond
    | lookup "Authorization" (requestHeaders request)
        == Just "Bearer secret-access" =
            respond (jsonResponse status200
                (Aeson.object
                    ["email" Aeson..= ("person@example.com" :: Text)]))
    | otherwise = respond (responseLBS status401 [] "")

slowApp :: Application
slowApp _request respond = do
    threadDelay 3_000_000
    respond (responseLBS status200 [] "")

echoingTokenErrorApp :: Application
echoingTokenErrorApp request respond = do
    _ <- strictRequestBody request
    respond
        (responseLBS status400 [("Content-Type", "text/plain")]
            "server-echoed-sensitive-body")

echoingBearerErrorApp :: Application
echoingBearerErrorApp _request respond =
    respond
        (responseLBS status401 [("Content-Type", "text/plain")]
            "rejected access-must-stay-secret")

redirectingApp :: Port -> Application
redirectingApp targetPort _request respond =
    respond
        (responseLBS status307
            [("Location", BS8.pack (localBaseUrl targetPort <> "/captured"))]
            "")

redirectTargetApp :: IORef [BS.ByteString] -> Application
redirectTargetApp requests request respond = do
    _ <- strictRequestBody request
    modifyIORef' requests (<> [rawPathInfo request])
    respond (responseLBS status200 [] "")

existingUserApp :: IORef [Text] -> Application
existingUserApp calls request respond = do
    modifyIORef' calls (<> [TextEncoding.decodeUtf8 (rawPathInfo request)])
    respond (jsonResponse status200 (Aeson.object
        [ "currentTier" Aeson..= Aeson.object
            [ "id" Aeson..= ("free-tier" :: Text)
            , "name" Aeson..= ("Gemini Code Assist" :: Text)
            ]
        , "cloudaicompanionProject" Aeson..= ("managed-project" :: Text)
        ]))

paidUserApp :: Application
paidUserApp _request respond =
    respond (jsonResponse status200 (Aeson.object
        [ "currentTier" Aeson..= Aeson.object
            ["id" Aeson..= ("standard-tier" :: Text)]
        , "paidTier" Aeson..= Aeson.object
            [ "id" Aeson..= ("paid-tier" :: Text)
            , "name" Aeson..= ("Google AI plan" :: Text)
            ]
        , "cloudaicompanionProject" Aeson..= ("paid-project" :: Text)
        ]))

partialPaidUserApp :: Application
partialPaidUserApp _request respond =
    respond (jsonResponse status200 (Aeson.object
        [ "currentTier" Aeson..= Aeson.object
            [ "id" Aeson..= ("standard-tier" :: Text)
            , "name" Aeson..= ("Standard" :: Text)
            ]
        , "paidTier" Aeson..= Aeson.object
            ["name" Aeson..= ("Google AI plan" :: Text)]
        , "cloudaicompanionProject" Aeson..= ("paid-project" :: Text)
        ]))

validationRequiredApp :: IORef Int -> Application
validationRequiredApp calls _request respond = do
    attempt <- atomicModifyIORef' calls \value ->
        let next = value + 1
        in (next, next)
    if attempt == 1
        then respond (jsonResponse status200 (Aeson.object
            [ "ineligibleTiers" Aeson..=
                [ Aeson.object
                    [ "reasonCode" Aeson..= ("VALIDATION_REQUIRED" :: Text)
                    , "reasonMessage" Aeson..= ("Verify please" :: Text)
                    , "validationUrl" Aeson..=
                        ("https://accounts.example.test/validate" :: Text)
                    ]
                ]
            ]))
        else respond (jsonResponse status200 (Aeson.object
            [ "currentTier" Aeson..= Aeson.object
                [ "id" Aeson..= ("free-tier" :: Text)
                , "name" Aeson..= ("Validated" :: Text)
                ]
            , "cloudaicompanionProject" Aeson..=
                ("validated-project" :: Text)
            ]))

standardOnboardingApp
    :: IORef [(BS.ByteString, BS.ByteString)]
    -> Application
standardOnboardingApp bodies request respond = do
    body <- LBS.toStrict <$> strictRequestBody request
    let path = rawPathInfo request
    modifyIORef' bodies (<> [(path, body)])
    case path of
        "/v1internal:loadCodeAssist" ->
            respond (jsonResponse status200 (Aeson.object
                [ "allowedTiers" Aeson..=
                    [ Aeson.object
                        [ "id" Aeson..= ("standard-tier" :: Text)
                        , "name" Aeson..= ("Standard" :: Text)
                        , "isDefault" Aeson..= True
                        ]
                    ]
                ]))
        "/v1internal:onboardUser" ->
            respond (jsonResponse status200 (Aeson.object
                [ "done" Aeson..= True
                , "response" Aeson..= Aeson.object []
                ]))
        _ -> respond (responseLBS status404 [] "")

failedOnboardingApp :: Application
failedOnboardingApp request respond =
    case rawPathInfo request of
        "/v1internal:loadCodeAssist" ->
            respond (jsonResponse status200 (Aeson.object
                [ "allowedTiers" Aeson..=
                    [ Aeson.object
                        [ "id" Aeson..= ("standard-tier" :: Text)
                        , "isDefault" Aeson..= True
                        ]
                    ]
                ]))
        "/v1internal:onboardUser" ->
            respond (jsonResponse status200 (Aeson.object
                [ "done" Aeson..= True
                , "error" Aeson..= Aeson.object
                    [ "code" Aeson..= (9 :: Int)
                    , "message" Aeson..= ("precondition failed" :: Text)
                    ]
                ]))
        _ -> respond (responseLBS status404 [] "")

echoingOnboardingErrorApp :: Application
echoingOnboardingErrorApp request respond =
    case rawPathInfo request of
        "/v1internal:loadCodeAssist" ->
            respond (jsonResponse status200 (Aeson.object
                [ "allowedTiers" Aeson..=
                    [ Aeson.object
                        [ "id" Aeson..= ("standard-tier" :: Text)
                        , "isDefault" Aeson..= True
                        ]
                    ]
                ]))
        "/v1internal:onboardUser" ->
            respond (jsonResponse status200 (Aeson.object
                [ "done" Aeson..= True
                , "error" Aeson..= Aeson.object
                    [ "message" Aeson..=
                        ("rejected secret-bearer" :: Text)
                    ]
                ]))
        _ -> respond (responseLBS status404 [] "")

onboardingApp :: IORef [Text] -> IORef [(BS.ByteString, BS.ByteString)] -> Application
onboardingApp calls bodies request respond = do
    body <- LBS.toStrict <$> strictRequestBody request
    let path = rawPathInfo request
    modifyIORef' calls (<> [TextEncoding.decodeUtf8 path])
    modifyIORef' bodies (<> [(path, body)])
    case rawPathInfo request of
        "/v1internal:loadCodeAssist" ->
            respond (jsonResponse status200 (Aeson.object
                [ "allowedTiers" Aeson..=
                    [ Aeson.object
                        [ "id" Aeson..= ("free-tier" :: Text)
                        , "name" Aeson..= ("Free" :: Text)
                        , "isDefault" Aeson..= True
                        ]
                    ]
                ]))
        "/v1internal:onboardUser" ->
            respond (jsonResponse status200 (Aeson.object
                [ "name" Aeson..= ("operations/onboarding" :: Text)
                , "done" Aeson..= False
                ]))
        "/v1internal/operations/onboarding" ->
            respond (jsonResponse status200 (Aeson.object
                [ "done" Aeson..= True
                , "response" Aeson..= Aeson.object
                    [ "cloudaicompanionProject" Aeson..= Aeson.object
                        ["id" Aeson..= ("new-managed-project" :: Text)]
                    ]
                ]))
        _ -> respond (responseLBS status404 [] "")

testOAuthOptions :: Port -> OAuthOptions
testOAuthOptions port = defaultOAuthOptions
    { tokenEndpoint = localBaseUrl port <> "/token"
    , userInfoEndpoint = localBaseUrl port <> "/userinfo"
    , clientId = "test-client"
    , clientSecret = "test-secret"
    }

testCodeAssistOptions :: Port -> CodeAssistOptions
testCodeAssistOptions port = defaultCodeAssistOptions
    { baseUrl = localBaseUrl port <> "/v1internal"
    , pollIntervalMicros = 0
    , maxPollAttempts = 2
    }

localBaseUrl :: Port -> String
localBaseUrl port = "http://127.0.0.1:" <> show port

sendSplitOAuthCallback :: Text -> IO ()
sendSplitOAuthCallback authorization = do
    redirect <- maybe
        (expectationFailure "authorization URL has no redirect_uri" >> pure "")
        pure
        (queryParameter "redirect_uri" authorization)
    state <- maybe
        (expectationFailure "authorization URL has no state" >> pure "")
        pure
        (queryParameter "state" authorization)
    port <- maybe
        (expectationFailure "redirect_uri has no loopback port" >> pure 0)
        pure
        (loopbackPort redirect)
    bracket
        (Net.socket Net.AF_INET Net.Stream Net.defaultProtocol)
        Net.close
        \socket -> do
            Net.connect socket
                (Net.SockAddrInet
                    (fromIntegral port)
                    (Net.tupleToHostAddress (127, 0, 0, 1)))
            Net.sendAll socket
                "GET /oauth2callback?code=split"
            threadDelay 100_000
            Net.sendAll socket $
                "-code&state="
                    <> TextEncoding.encodeUtf8 state
                    <> " HTTP/1.1\r\nHost: 127.0.0.1\r\n\r\n"
            _ <- Net.recv socket 4096
            pure ()

queryParameter :: Text -> Text -> Maybe Text
queryParameter key url = do
    query <- Text.stripPrefix "?" (snd (Text.breakOn "?" url))
    lookup key (parseQueryText (TextEncoding.encodeUtf8 query)) >>= id

loopbackPort :: Text -> Maybe Int
loopbackPort redirect = do
    remainder <- Text.stripPrefix "http://127.0.0.1:" redirect
    let (port, _) = Text.breakOn "/" remainder
    readMaybe (Text.unpack port)

jsonResponse :: Status -> Aeson.Value -> Response
jsonResponse status value =
    responseLBS status
        [("Content-Type", "application/json")]
        (Aeson.encode value)
