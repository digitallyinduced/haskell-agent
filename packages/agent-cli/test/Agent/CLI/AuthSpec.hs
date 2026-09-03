module Agent.CLI.AuthSpec (spec) where

import Agent.CLI.Auth
import Agent.CLI.CredentialStore
import Agent.CLI.Dictation
    ( DictationAuthError(..)
    , DictationBackend(..)
    , loadDictationBackendAuth
    )
import Agent.CLI.GatewayClient
    ( GatewayCredential(..)
    , gatewayCredentialPath
    , saveGatewayCredentialAt
    )
import qualified Agent.CLI.Login as Login
import Agent.Error
    ( ApiError(..)
    , CredentialExhaustionReason(..)
    , credentialsExhausted
    )
import System.OsPath (OsPath, decodeUtf, unsafeEncodeUtf)
import Agent.OpenAI.Auth (AuthState(..))
import qualified Agent.OpenAI.Auth as OpenAI
import qualified Agent.OpenAI.Credential as OpenAICredential
import qualified Agent.Gemini.Auth as Gemini
import Agent.Provider
    ( AccountFailure(..)
    , BillingMode(..)
    , Credential(..)
    , FailedCredential(..)
    , Provider(..)
    , getNextToken
    , tokenProvider
    , tokenProviderBillingMode
    )
import qualified Agent.XAI.Auth as XAIAuth
import Control.Exception.Safe (bracket)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock
    ( UTCTime(..)
    , addUTCTime
    , diffUTCTime
    , getCurrentTime
    )
import System.Directory
    ( createDirectory
    , createDirectoryIfMissing
    , getTemporaryDirectory
    , removeFile
    , removePathForcibly
    )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import Test.Hspec

fromFilePath :: FilePath -> OsPath
fromFilePath = unsafeEncodeUtf

toFilePath :: OsPath -> FilePath
toFilePath path = either (error . show) id (decodeUtf path)

expectRightResult :: Show err => Either err value -> IO value
expectRightResult = \case
    Left err -> do
        expectationFailure ("expected Right, got " <> show err)
        fail "unreachable after expectation failure"
    Right value -> pure value

spec :: Spec
spec = do
    describe "authErrorNeedsOnboarding" do
        it "recognizes missing and invalid first-start credentials" do
            authErrorNeedsOnboarding
                "no credentials found. Set GROK_ACCESS_TOKEN, CODEX_ACCESS_TOKEN, or OPENROUTER_API_KEY, or place auth at ~/.grok/auth.json / ~/.codex/auth.json."
                `shouldBe` True
            authErrorNeedsOnboarding
                "no valid OpenAI credentials found: invalid auth JSON"
                `shouldBe` True
            authErrorNeedsOnboarding "credential store is unreadable"
                `shouldBe` False

    describe "geminiAuthErrorNeedsReconnect" do
        it "recognizes missing, malformed, and rejected Google credentials" do
            geminiAuthErrorNeedsReconnect
                "no credentials found. connect a Google account"
                `shouldBe` True
            geminiAuthErrorNeedsReconnect
                "cannot switch to gemini: managed Gemini OAuth credential contains invalid auth JSON; reconnect the account"
                `shouldBe` True
            geminiAuthErrorNeedsReconnect
                "cannot switch to gemini: Authentication failed. Google OAuth token request failed with HTTP 400"
                `shouldBe` True

        it "does not turn connection or rate-limit failures into login" do
            geminiAuthErrorNeedsReconnect
                "cannot switch to gemini: Authentication failed. Google HTTP request failed"
                `shouldBe` False
            geminiAuthErrorNeedsReconnect
                "cannot switch to gemini: Provider unavailable."
                `shouldBe` False

    describe "geminiStartupAuthNeedsReconnect" do
        it "recovers stale Gemini auth without hijacking generic onboarding" do
            geminiStartupAuthNeedsReconnect
                False
                "managed Gemini OAuth credential contains invalid auth JSON"
                `shouldBe` True
            geminiStartupAuthNeedsReconnect
                True
                "no credentials found. connect an account"
                `shouldBe` True
            geminiStartupAuthNeedsReconnect
                False
                "no credentials found. connect an account"
                `shouldBe` False
            geminiStartupAuthNeedsReconnect
                True
                "Google HTTP request failed"
                `shouldBe` False

    describe "classifyGeminiRefreshFailure" do
        it "separates rejected refresh grants from transient failures" do
            classifyGeminiRefreshFailure
                "Google OAuth token request failed with HTTP 400"
                `shouldBe` CredentialError
                    "Google OAuth token request failed with HTTP 400"
            classifyGeminiRefreshFailure
                "Google HTTP request failed"
                `shouldBe` ConnectionError "Google HTTP request failed"
            classifyGeminiRefreshFailure
                "Google OAuth token request failed with HTTP 503"
                `shouldBe` ConnectionError
                    "Google OAuth token request failed with HTTP 503"

    describe "loadAuth" do
        it "prefers OpenAI when automatic detection finds OpenAI and xAI auth" $
            withTempHome \_ ->
                withCleanOpenAiEnv $
                withCleanGrokEnv $
                withEnv "AGENT_BROKER_URL" Nothing do
                    storeOpenAiAccount
                        "openai"
                        "openai-account"
                        True
                        "openai-token"
                    storeManagedAccount
                        SubscriptionBilled
                        XAIProvider
                        "xai"
                        "xai-account"
                        "Grok"
                        True
                        "xai-token"
                    loadAuth Nothing >>= \case
                        Left err -> expectationFailure (Text.unpack err)
                        Right loaded ->
                            loaded.loadedProvider `shouldBe` OpenAIProvider

        it "uses a connected gateway for explicit OpenAI auth" $
            withTempHome \home -> do
                saveTestGateway home
                loadAuth (Just OpenAIProvider) >>= \case
                    Left err -> expectationFailure (Text.unpack err)
                    Right loaded -> do
                        loaded.loadedProvider `shouldBe` OpenAIProvider
                        loaded.loadedSelectionId
                            `shouldBe` Just gatewayAuthSelectionId

        it "keeps a gateway session gateway-only after rejection" $
            withTempHome \home ->
                withCleanOpenAiEnv do
                    saveTestGateway home
                    storeOpenAiAccount
                        "local-openai"
                        "local-account"
                        True
                        farFutureAccessToken
                    loadAuth (Just OpenAIProvider) >>= \case
                        Left err -> expectationFailure (Text.unpack err)
                        Right loaded -> do
                            case loaded.loadedOpenAiPool of
                                Nothing -> pure ()
                                Just _ ->
                                    expectationFailure
                                        "gateway auth must not attach a local account pool"
                            gatewayCredential <-
                                getNextToken
                                    loaded.loadedTokenProvider
                                    Nothing
                                    >>= expectRightResult
                            gatewayCredential.accountId
                                `shouldBe`
                                    "wss://gateway.example/v1/responses"
                            getNextToken
                                loaded.loadedTokenProvider
                                (Just FailedCredential
                                    { credential = gatewayCredential
                                    , failure =
                                        AccountAuthenticationRejected
                                    , failureReason =
                                        testAuthenticationReason
                                    })
                                `shouldReturn`
                                    Left
                                        (CredentialError
                                            "static credential was rejected")

        it "refuses a direct credential for an organization model" do
            let directCredential = Credential
                    { accessToken = "direct-token"
                    , accountId = "direct-account"
                    , leaseId = Nothing
                    , provider = OpenAIProvider
                    }
                guarded =
                    gatewayRouterTokenProvider
                        (staticCredentialProvider
                            SubscriptionBilled
                            directCredential)
            getNextToken guarded Nothing
                `shouldReturn`
                    Left
                        (CredentialError
                            "the connected gateway is unavailable; refusing to \
                            \send an organization model with direct OpenAI credentials")

        it "does not turn a rejected gateway into API-credit spending" $
            withTempHome \home ->
                withCleanOpenAiEnv do
                    saveTestGateway home
                    storeOpenAiAccountWithBilling
                        ApiBilled
                        "api-openai"
                        "api-account"
                        True
                        farFutureAccessToken
                    loadAuth (Just OpenAIProvider) >>= \case
                        Left err -> expectationFailure (Text.unpack err)
                        Right loaded -> do
                            case loaded.loadedOpenAiPool of
                                Nothing -> pure ()
                                Just _ ->
                                    expectationFailure
                                        "API-credit pool must not be attached"
                            gatewayCredential <-
                                getNextToken
                                    loaded.loadedTokenProvider
                                    Nothing
                                    >>= expectRightResult
                            getNextToken
                                loaded.loadedTokenProvider
                                (Just FailedCredential
                                    { credential = gatewayCredential
                                    , failure =
                                        AccountAuthenticationRejected
                                    , failureReason =
                                        testAuthenticationReason
                                    })
                                >>= \case
                                    Left (CredentialError _) -> pure ()
                                    other ->
                                        expectationFailure $
                                            "expected gateway rejection, got "
                                                <> show other

        it "fails closed when the gateway file is invalid" $
            withTempHome \home ->
                withCleanOpenAiEnv do
                    let path = toFilePath (gatewayCredentialPath home)
                    createDirectoryIfMissing True
                        (toFilePath home </> ".haskell-agent"
                            </> "credentials")
                    LBS.writeFile path "{not-json"
                    storeOpenAiAccount
                        "local-openai"
                        "local-account"
                        True
                        farFutureAccessToken
                    loadAuth (Just OpenAIProvider) >>= \case
                        Left err ->
                            err `shouldSatisfy`
                                Text.isPrefixOf "cannot load gateway credential:"
                        Right _ ->
                            expectationFailure
                                "expected an invalid gateway to block local auth"

        it "fails closed for automatic provider detection when the gateway file is invalid" $
            withTempHome \home ->
                withCleanOpenAiEnv $
                withCleanGrokEnv do
                    let path = toFilePath (gatewayCredentialPath home)
                    createDirectoryIfMissing True
                        (toFilePath home </> ".haskell-agent"
                            </> "credentials")
                    LBS.writeFile path "{not-json"
                    storeOpenAiAccountWithBilling
                        ApiBilled
                        "api-openai"
                        "api-account"
                        True
                        farFutureAccessToken
                    loadAuth (Just XAIProvider) >>= \case
                        Left err ->
                            err `shouldSatisfy`
                                Text.isInfixOf
                                    "cannot load gateway credential:"
                        Right _ ->
                            expectationFailure
                                "expected an invalid gateway to block automatic auth"

        it "does not use another subscription provider when the gateway file is invalid" $
            withTempHome \home ->
                withCleanOpenAiEnv $
                withCleanGrokEnv $
                withEnv "GROK_ACCESS_TOKEN" (Just "xai-token") do
                    let path = toFilePath (gatewayCredentialPath home)
                    createDirectoryIfMissing True
                        (toFilePath home </> ".haskell-agent"
                            </> "credentials")
                    LBS.writeFile path "{not-json"
                    loadAuth Nothing >>= \case
                        Left err ->
                            err `shouldSatisfy`
                                Text.isInfixOf
                                    "cannot load gateway credential:"
                        Right _ ->
                            expectationFailure
                                "expected an invalid gateway to block local auth"

        it "does not let an explicit provider bypass an invalid gateway" $
            withTempHome \home ->
                withCleanOpenAiEnv $
                withCleanGrokEnv $
                withEnv "GROK_ACCESS_TOKEN" (Just "xai-token") do
                    let path = toFilePath (gatewayCredentialPath home)
                    createDirectoryIfMissing True
                        (toFilePath home </> ".haskell-agent"
                            </> "credentials")
                    LBS.writeFile path "{not-json"
                    storeOpenAiAccountWithBilling
                        ApiBilled
                        "api-openai"
                        "api-account"
                        True
                        farFutureAccessToken
                    loadAuth Nothing >>= \case
                        Left err ->
                            err `shouldSatisfy`
                                Text.isInfixOf
                                    "cannot load gateway credential:"
                        Right _ ->
                            expectationFailure
                                "expected an invalid gateway to block local auth"

        it "does not let an explicit non-OpenAI provider bypass the gateway" $
            withTempHome \home ->
                withCleanGrokEnv $
                withEnv "GROK_ACCESS_TOKEN" (Just "xai-token") do
                    saveTestGateway home
                    loadAuth (Just XAIProvider) >>= \case
                        Left err ->
                            err `shouldSatisfy`
                                Text.isInfixOf
                                    "organization gateway is active"
                        Right _ ->
                            expectationFailure
                                "expected the gateway to block direct xAI auth"

        it "rejects an explicit provider against an exact gateway snapshot" do
            case gatewayLoadedAuthForProvider
                    (Just XAIProvider)
                    testGatewayCredential of
                Left err ->
                    err `shouldSatisfy`
                        Text.isInfixOf
                            "organization gateway is active"
                Right _ ->
                    expectationFailure
                        "expected exact gateway auth to block direct xAI"
            case gatewayLoadedAuthForProvider
                    (Just ClaudeCodeProvider)
                    testGatewayCredential of
                Right loaded ->
                    loaded.loadedProvider `shouldBe` ClaudeCodeProvider
                Left err ->
                    expectationFailure
                        ("expected gateway Claude auth, got " <> Text.unpack err)

    describe "loadOpenAiDictationAuth" do
        it "loads ChatGPT OAuth as subscription-billed OpenAI auth" $
            withTempHome \_ ->
                withCleanOpenAiEnv $
                withEnv
                    "CODEX_AUTH_JSON"
                    (Just
                        "{\"auth_mode\":\"chatgpt\",\
                        \\"tokens\":{\"access_token\":\"e30.eyJleHAiOjQxMDI0NDQ4MDB9.\",\
                        \\"refresh_token\":\"oauth-refresh\",\
                        \\"account_id\":\"account-oauth\"}}") do
                    shouldLoadOpenAiDictationCredential
                        SubscriptionBilled
                        "e30.eyJleHAiOjQxMDI0NDQ4MDB9."
                        "account-oauth"

        it "prefers ChatGPT OAuth over an API key" $
            withTempHome \_ ->
                withCleanOpenAiEnv $
                withEnv "OPENAI_API_KEY" (Just "sk-openai") $
                withEnv
                    "CODEX_AUTH_JSON"
                    (Just
                        "{\"auth_mode\":\"chatgpt\",\
                        \\"tokens\":{\"access_token\":\"e30.eyJleHAiOjQxMDI0NDQ4MDB9.\",\
                        \\"refresh_token\":\"oauth-refresh\",\
                        \\"account_id\":\"account-oauth\"}}") do
                    shouldLoadOpenAiDictationCredential
                        SubscriptionBilled
                        "e30.eyJleHAiOjQxMDI0NDQ4MDB9."
                        "account-oauth"

        it "loads OPENAI_API_KEY as API-billed OpenAI auth" $
            withTempHome \_ ->
                withCleanOpenAiEnv $
                withEnv "OPENAI_API_KEY" (Just "sk-openai") do
                    shouldLoadOpenAiDictationToken "sk-openai"

        it "loads Codex's API-key environment variable" $
            withTempHome \_ ->
                withCleanOpenAiEnv $
                withEnv "CODEX_API_KEY" (Just "sk-codex-env") do
                    shouldLoadOpenAiDictationToken "sk-codex-env"

        it "prefers OPENAI_API_KEY over CODEX_API_KEY" $
            withTempHome \_ ->
                withCleanOpenAiEnv $
                withEnv "CODEX_API_KEY" (Just "sk-codex-env") $
                withEnv "OPENAI_API_KEY" (Just "sk-openai") do
                    shouldLoadOpenAiDictationToken "sk-openai"

        it "prefers an explicit API key over a managed API account" $
            withTempHome \_ ->
                withCleanOpenAiEnv $
                withEnv "OPENAI_API_KEY" (Just "sk-openai") do
                    storeManagedAccount
                        ApiBilled
                        OpenAIProvider
                        "managed-openai"
                        "managed-account"
                        "Managed OpenAI"
                        True
                        "managed-key"
                    shouldLoadOpenAiDictationToken "sk-openai"

        it "loads Codex's OPENAI_API_KEY auth JSON field" $
            withTempHome \_ ->
                withCleanOpenAiEnv $
                withEnv
                    "CODEX_AUTH_JSON"
                    (Just
                        "{\"auth_mode\":\"apikey\",\
                        \\"OPENAI_API_KEY\":\"sk-codex\"}") do
                    shouldLoadOpenAiDictationToken "sk-codex"

        it "honors CODEX_HOME when reading Codex auth JSON" $
            withTempHome \home ->
                withCleanOpenAiEnv do
                    let codexHome = toFilePath home </> "custom-codex"
                    createDirectory codexHome
                    LBS.writeFile
                        (codexHome </> "auth.json")
                        "{\"OPENAI_API_KEY\":\"sk-codex-home\"}"
                    withEnv "CODEX_HOME" (Just codexHome) do
                        shouldLoadOpenAiDictationToken "sk-codex-home"

        it "honors CODEX_HOME for ChatGPT OAuth auth JSON" $
            withTempHome \home ->
                withCleanOpenAiEnv do
                    let codexHome = toFilePath home </> "custom-codex"
                    createDirectory codexHome
                    LBS.writeFile
                        (codexHome </> "auth.json")
                        "{\"auth_mode\":\"chatgpt\",\
                        \\"tokens\":{\"access_token\":\"e30.eyJleHAiOjQxMDI0NDQ4MDB9.\",\
                        \\"refresh_token\":\"oauth-refresh\",\
                        \\"account_id\":\"account-home\"}}"
                    withEnv "CODEX_HOME" (Just codexHome) do
                        shouldLoadOpenAiDictationCredential
                            SubscriptionBilled
                            "e30.eyJleHAiOjQxMDI0NDQ4MDB9."
                            "account-home"

        it "preserves an unreadable credential store instead of returning nothing" $
            withTempHome \home ->
                withCleanOpenAiEnv do
                    let storeDirectory =
                            toFilePath home
                                </> ".haskell-agent"
                                </> "credentials"
                    createDirectoryIfMissing True storeDirectory
                    writeFile
                        (storeDirectory </> "accounts.json")
                        "{not-json"
                    loadOpenAiDictationAuth >>= \case
                        Right _ ->
                            expectationFailure
                                "expected invalid OpenAI dictation auth"
                        Left err -> do
                            err
                                `shouldSatisfy`
                                    ("no valid OpenAI credentials found:"
                                        `Text.isPrefixOf`)
                            err
                                `shouldSatisfy`
                                    ("invalid credential store" `Text.isInfixOf`)

        it "preserves invalid managed OpenAI auth JSON instead of returning nothing" $
            withTempHome \_ ->
                withCleanOpenAiEnv do
                    upsertManagedCredential
                        (openAiMetadata
                            SubscriptionBilled
                            "broken-openai"
                            "broken-account"
                            True)
                        (ManagedSecret "broken-openai" "{not-json")
                        `shouldReturn` Right ()
                    loadOpenAiDictationAuth >>= \case
                        Right _ ->
                            expectationFailure
                                "expected invalid OpenAI dictation auth"
                        Left err ->
                            err
                                `shouldBe`
                                    "no valid OpenAI credentials found: \
                                    \managed OpenAI OAuth credential \
                                    \broken-openai contains invalid auth JSON"

        it "classifies an unreadable OpenAI store as an invalid dictation credential" $
            withTempHome \home ->
                withCleanOpenAiEnv do
                    let storeDirectory =
                            toFilePath home
                                </> ".haskell-agent"
                                </> "credentials"
                    createDirectoryIfMissing True storeDirectory
                    writeFile
                        (storeDirectory </> "accounts.json")
                        "{not-json"
                    loadDictationBackendAuth OpenAIDictation >>= \case
                        Left (DictationCredentialInvalid err) -> do
                            err
                                `shouldSatisfy`
                                    ("no valid OpenAI credentials found:"
                                        `Text.isPrefixOf`)
                            err
                                `shouldSatisfy`
                                    ("invalid credential store" `Text.isInfixOf`)
                        Left (DictationCredentialMissing err) ->
                            expectationFailure
                                ("expected invalid OpenAI dictation auth, got missing: "
                                    <> Text.unpack err)
                        Right _ ->
                            expectationFailure
                                "expected invalid OpenAI dictation auth"
    describe "Gemini loadAuth" do
        it "loads Google AI Studio keys with GOOGLE_API_KEY precedence" $
            withTempHome \_ ->
                withEnv "GOOGLE_API_KEY" (Just "google-key") $
                withEnv "GEMINI_API_KEY" (Just "gemini-key") do
                    loadAuth (Just GeminiProvider) >>= \case
                        Left err -> expectationFailure (Text.unpack err)
                        Right loaded -> do
                            loaded.loadedProvider `shouldBe` GeminiProvider
                            loaded.loadedSelectionId
                                `shouldBe` Just
                                    (externalAuthSelectionId
                                        GeminiProvider
                                        "environment")
                            tokenProviderBillingMode
                                loaded.loadedTokenProvider
                                `shouldBe` ApiBilled
                            getNextToken loaded.loadedTokenProvider Nothing
                                >>= \case
                                    Left apiError ->
                                        expectationFailure (show apiError)
                                    Right credential -> do
                                        credential.accessToken
                                            `shouldBe` "google-key"
                                        credential.accountId
                                            `shouldBe` "gemini"
                                        credential.provider
                                            `shouldBe` GeminiProvider

        it "loads a managed Google login as subscription-billed Code Assist auth" $
            withTempHome \_ ->
                withEnv "GOOGLE_API_KEY" Nothing $
                withEnv "GEMINI_API_KEY" Nothing do
                    now <- getCurrentTime
                    let state = Gemini.GeminiAuthState
                            { accessToken = "google-access"
                            , refreshToken = Just "google-refresh"
                            , expiresAt = Just (addUTCTime 3600 now)
                            , email = "person@example.com"
                            , projectId = "managed-project"
                            , userTier = "free-tier"
                            }
                        payload =
                            TextEncoding.decodeUtf8
                                (LBS.toStrict (Aeson.encode state))
                    upsertManagedCredential
                        ManagedCredential
                            { managedId = "gemini-google"
                            , managedProvider = GeminiProvider
                            , managedAccountId = "person@example.com"
                            , managedLabel = "person@example.com"
                            , managedBilling = SubscriptionBilled
                            , managedAuthKind = ManagedGeminiAuthJson
                            , managedEnabled = True
                            }
                        (ManagedSecret "gemini-google" payload)
                        `shouldReturn` Right ()
                    loadAuth (Just GeminiProvider) >>= \case
                        Left err -> expectationFailure (Text.unpack err)
                        Right loaded -> do
                            tokenProviderBillingMode
                                loaded.loadedTokenProvider
                                `shouldBe` SubscriptionBilled
                            loaded.loadedSelectionId
                                `shouldBe` Just
                                    (managedAuthSelectionId "gemini-google")
                            getNextToken loaded.loadedTokenProvider Nothing
                                `shouldReturn`
                                    Right Credential
                                        { accessToken = "google-access"
                                        , accountId = "person@example.com"
                                        , leaseId =
                                            Just
                                                "code-assist:managed-project"
                                        , provider = GeminiProvider
                                        }

    describe "probeLoadedAuth" do
        it "rejects auth whose accounts are currently cooling down" do
            let retryAt = UTCTime (fromGregorian 2026 8 21) 3600
                exhausted = LoadedAuth
                    { loadedProvider = XAIProvider
                    , loadedTokenProvider = tokenProvider SubscriptionBilled \_ ->
                        pure (Left (credentialsExhausted retryAt))
                    , loadedAccountLabel = pure . credentialAccountLabel
                    , loadedSelectionId = Nothing
                    , loadedOpenAiPool = Nothing
                    }
            result <- probeLoadedAuth exhausted
            case result of
                Left err -> err `shouldBe` credentialsExhausted retryAt
                Right _ -> expectationFailure "expected exhausted auth"

        it "returns and seeds the credential used for the account display" do
            calls <- newIORef (0 :: Int)
            let first = Credential "first" "account-first" Nothing XAIProvider
                second = Credential "second" "account-second" Nothing XAIProvider
                loaded = LoadedAuth
                    { loadedProvider = XAIProvider
                    , loadedTokenProvider =
                        tokenProvider SubscriptionBilled \_ -> do
                            call <- atomicModifyIORef' calls \count ->
                                (count + 1, count)
                            pure (Right (if call == 0 then first else second))
                    , loadedAccountLabel = pure . (.accountId)
                    , loadedSelectionId = Nothing
                    , loadedOpenAiPool = Nothing
                    }
            probeLoadedAuthCredential loaded >>= \case
                Left err ->
                    expectationFailure
                        ("expected usable credential, got " <> show err)
                Right (credential, usable) -> do
                    credential `shouldBe` first
                    getNextToken usable.loadedTokenProvider Nothing
                        `shouldReturn` Right first
                    getNextToken usable.loadedTokenProvider Nothing
                        `shouldReturn` Right second

    describe "loadAuthForAccount" do
        it "keeps the gateway selected when its route is selected" $
            withTempHome \home ->
                withCleanOpenAiEnv do
                    saveTestGateway home
                    storeOpenAiAccount
                        "local-openai"
                        "local-account"
                        True
                        farFutureAccessToken
                    loadAuthForAccount
                        OpenAIProvider
                        gatewayAuthSelectionId
                        >>= \case
                            Left err ->
                                expectationFailure (Text.unpack err)
                            Right loaded -> do
                                gatewayCredential <-
                                    getNextToken
                                        loaded.loadedTokenProvider
                                        Nothing
                                        >>= expectRightResult
                                getNextToken
                                    loaded.loadedTokenProvider
                                    (Just FailedCredential
                                        { credential = gatewayCredential
                                        , failure =
                                            AccountAuthenticationRejected
                                        , failureReason =
                                            testAuthenticationReason
                                        })
                                    `shouldReturn`
                                        Left
                                            (CredentialError
                                                "static credential was rejected")

        it "does not let a local OpenAI selection escape a connected gateway" $
            withTempHome \home ->
                withCleanOpenAiEnv do
                    saveTestGateway home
                    storeOpenAiAccount
                        "local-openai-a"
                        "local-account-a"
                        True
                        farFutureAccessToken
                    storeOpenAiAccount
                        "local-openai-b"
                        "local-account-b"
                        True
                        farFutureAccessToken
                    loadAuthForAccount
                        OpenAIProvider
                        "local-account-b"
                        >>= \case
                            Left err ->
                                err `shouldBe`
                                    "organization gateway is active; disconnect it before selecting another account"
                            Right _ ->
                                expectationFailure
                                    "expected the gateway to block a local account"

        it "does not let a selected non-OpenAI account escape a gateway" $
            withTempHome \home ->
                withCleanGrokEnv $
                withEnv "GROK_ACCESS_TOKEN" (Just "xai-token") do
                    saveTestGateway home
                    loadAuthForAccount
                        XAIProvider
                        (externalAuthSelectionId XAIProvider "environment")
                        >>= \case
                            Left err ->
                                err `shouldBe`
                                    "organization gateway is active; disconnect it before selecting another account"
                            Right _ ->
                                expectationFailure
                                    "expected the gateway to block another provider account"

        it "loads the selected managed Grok account" $
            withTempHome \_ ->
                withEnv "GROK_AUTH_JSON" Nothing $
                withEnv "GROK_ACCESS_TOKEN" Nothing do
                    storeManagedAccount
                        SubscriptionBilled
                        XAIProvider
                        "grok-a"
                        "account-a"
                        "first@example.com"
                        True
                        "token-a"
                    storeManagedAccount
                        SubscriptionBilled
                        XAIProvider
                        "grok-b"
                        "account-b"
                        "second@example.com"
                        True
                        "token-b"
                    loadAuthForAccount XAIProvider "account-b" >>= \case
                        Left err -> expectationFailure (Text.unpack err)
                        Right loaded -> do
                            (fmap
                                (fmap (.accountId))
                                (getNextToken
                                    loaded.loadedTokenProvider
                                    Nothing))
                                `shouldReturn` Right "account-b"
                            getNextToken loaded.loadedTokenProvider Nothing
                                >>= \case
                                    Left err ->
                                        expectationFailure (show err)
                                    Right credential ->
                                        loaded.loadedAccountLabel credential
                                            `shouldReturn`
                                                "second@example.com"

        it "loads the selected managed OpenRouter account" $
            withTempHome \_ ->
                withEnv "OPENROUTER_API_KEY" Nothing do
                    storeManagedAccount
                        ApiBilled
                        OpenRouterProvider
                        "router-a"
                        "account-a"
                        "OpenRouter A"
                        True
                        "key-a"
                    storeManagedAccount
                        ApiBilled
                        OpenRouterProvider
                        "router-b"
                        "account-b"
                        "OpenRouter B"
                        True
                        "key-b"
                    loadAuthForAccount
                        OpenRouterProvider
                        "account-b"
                        >>= \case
                            Left err ->
                                expectationFailure (Text.unpack err)
                            Right loaded ->
                                getNextToken
                                    loaded.loadedTokenProvider
                                    Nothing
                                    >>= \case
                                        Left err ->
                                            expectationFailure (show err)
                                        Right credential -> do
                                            credential.accountId
                                                `shouldBe` "account-b"
                                            loaded.loadedAccountLabel credential
                                                `shouldReturn`
                                                    "OpenRouter B"

        it "selects duplicate OpenRouter account labels by managed id" $
            withTempHome \_ ->
                withEnv "OPENROUTER_API_KEY" Nothing do
                    storeManagedAccount
                        ApiBilled
                        OpenRouterProvider
                        "router-a"
                        "shared-label"
                        "OpenRouter A"
                        True
                        "key-a"
                    storeManagedAccount
                        ApiBilled
                        OpenRouterProvider
                        "router-b"
                        "shared-label"
                        "OpenRouter B"
                        True
                        "key-b"
                    loadAuthForAccount
                        OpenRouterProvider
                        (managedAuthSelectionId "router-b")
                        >>= \case
                            Left err ->
                                expectationFailure (Text.unpack err)
                            Right loaded -> do
                                loaded.loadedSelectionId
                                    `shouldBe`
                                        Just
                                            (managedAuthSelectionId
                                                "router-b")
                                getNextToken
                                    loaded.loadedTokenProvider
                                    Nothing
                                    >>= \case
                                        Left err ->
                                            expectationFailure (show err)
                                        Right credential ->
                                            credential.accessToken
                                                `shouldBe` "key-b"

        it "selects duplicate Gemini API-key accounts by managed id" $
            withTempHome \_ ->
                withEnv "GOOGLE_API_KEY" Nothing $
                withEnv "GEMINI_API_KEY" Nothing do
                    storeManagedAccount
                        ApiBilled
                        GeminiProvider
                        "gemini-a"
                        "gemini"
                        "Google Gemini"
                        True
                        "key-a"
                    storeManagedAccount
                        ApiBilled
                        GeminiProvider
                        "gemini-b"
                        "gemini"
                        "Google Gemini"
                        True
                        "key-b"
                    loadAuthForAccount
                        GeminiProvider
                        (managedAuthSelectionId "gemini-b")
                        >>= \case
                            Left err ->
                                expectationFailure (Text.unpack err)
                            Right loaded -> do
                                loaded.loadedSelectionId
                                    `shouldBe`
                                        Just
                                            (managedAuthSelectionId
                                                "gemini-b")
                                getNextToken
                                    loaded.loadedTokenProvider
                                    Nothing
                                    >>= \case
                                        Left err ->
                                            expectationFailure (show err)
                                        Right credential ->
                                            credential.accessToken
                                                `shouldBe` "key-b"

        it "loads a Grok auth file even when a different env source exists" $
            withTempHome \home ->
                withEnv "GROK_AUTH_JSON" Nothing $
                withEnv "GROK_ACCESS_TOKEN" (Just "env-token") do
                    let grokDirectory = toFilePath home </> ".grok"
                        authPath = grokDirectory </> "auth.json"
                        selectionId =
                            externalAuthSelectionId
                                XAIProvider
                                (Text.pack authPath)
                    createDirectoryIfMissing True grokDirectory
                    LBS.writeFile authPath $
                        Aeson.encode $
                            Aeson.object
                                [ "access_token" .= ("file-token" :: Text)
                                ]
                    loadAuthForAccount XAIProvider selectionId >>= \case
                        Left err ->
                            expectationFailure (Text.unpack err)
                        Right loaded -> do
                            loaded.loadedSelectionId
                                `shouldBe` Just selectionId
                            getNextToken
                                loaded.loadedTokenProvider
                                Nothing
                                >>= \case
                                    Left err ->
                                        expectationFailure (show err)
                                    Right credential ->
                                        credential.accessToken
                                            `shouldBe` "file-token"

        it "rejects an unknown or disabled managed account" $
            withTempHome \_ ->
                withEnv "GROK_AUTH_JSON" Nothing $
                withEnv "GROK_ACCESS_TOKEN" Nothing do
                    storeManagedAccount
                        SubscriptionBilled
                        XAIProvider
                        "grok-disabled"
                        "account-disabled"
                        "Disabled"
                        False
                        "token-disabled"
                    loadAuthForAccount
                        XAIProvider
                        "account-disabled"
                        >>= \case
                            Left err ->
                                err `shouldBe`
                                    "no enabled xai credential found for account account-disabled"
                            Right _ ->
                                expectationFailure
                                    "expected disabled account selection to fail"

        it "uses the same stable id for an OpenRouter environment account" $
            withTempHome \_ ->
                withEnv "OPENROUTER_API_KEY" (Just "openrouter-key") do
                    let selectionId =
                            externalAuthSelectionId
                                OpenRouterProvider
                                "environment"
                    loadAuthForAccount
                        OpenRouterProvider
                        selectionId
                        >>= \case
                            Left err ->
                                expectationFailure (Text.unpack err)
                            Right loaded -> do
                                loaded.loadedSelectionId
                                    `shouldBe` Just selectionId
                                (fmap
                                    (fmap (.accountId))
                                    (getNextToken
                                        loaded.loadedTokenProvider
                                        Nothing))
                                    `shouldReturn` Right "openrouter"

    describe "discoverSelectableLoginAccounts" do
        it "shows a gateway in login management but not account selection" $
            withTempHome \home ->
                withCleanOpenAiEnv do
                    saveTestGateway home
                    storeOpenAiAccount
                        "local-openai"
                        "local-account"
                        True
                        farFutureAccessToken
                    managed <- Login.discoverLoginAccounts
                    selectable <- Login.discoverSelectableLoginAccounts
                    map (.loginSource) managed
                        `shouldSatisfy` elem "gateway"
                    map (.loginSource) selectable
                        `shouldSatisfy` not . elem "gateway"

        it "keeps an unreadable gateway visible so it can be disconnected" $
            withTempHome \home -> do
                createDirectoryIfMissing True
                    (toFilePath home </> ".haskell-agent"
                        </> "credentials")
                LBS.writeFile
                    (toFilePath (gatewayCredentialPath home))
                    "{not-json"
                accounts <- Login.discoverLoginAccounts
                case filter ((== "gateway") . (.loginSource)) accounts of
                    [gateway] ->
                        gateway.loginUsage
                            `shouldSatisfy` \case
                                Login.UsageUnavailable _ -> True
                                _ -> False
                    _ ->
                        expectationFailure
                            "expected one repairable gateway entry"

        it "does not let a disabled managed account shadow an external source" $
            withTempHome \_ ->
                withCleanGrokEnv $
                withEnv "GROK_ACCESS_TOKEN" (Just "external-token") do
                    storeManagedAccount
                        SubscriptionBilled
                        XAIProvider
                        "disabled-grok"
                        "grok"
                        "Disabled"
                        False
                        "disabled-token"
                    accounts <- Login.discoverSelectableLoginAccounts
                    let grokAccounts =
                            filter
                                ((== XAIProvider) . (.loginProvider))
                                accounts
                    map Login.loginAccountSelectionId grokAccounts
                        `shouldBe`
                            [ externalAuthSelectionId
                                XAIProvider
                                "environment"
                            ]

    describe "credentialAccountLabel" do
        it "prefers an email claim over the provider account id" do
            let token =
                    "e30.eyJlbWFpbCI6InBlcnNvbkBleGFtcGxlLmNvbSJ9."
            credentialAccountLabel
                Credential
                    { accessToken = token
                    , accountId = "acc-1234567890"
                    , leaseId = Nothing
                    , provider = OpenAIProvider
                    }
                `shouldBe` "person@example.com"

        it "shortens opaque account ids and names key-only providers" do
            credentialAccountLabel staleGrok `shouldBe` "acc-stale"
            credentialAccountLabel
                Credential
                    { accessToken = "key"
                    , accountId = "account-1234567890"
                    , leaseId = Nothing
                    , provider = OpenRouterProvider
                    }
                `shouldBe` "account-…"
            credentialAccountLabel
                Credential
                    { accessToken = "key"
                    , accountId = ""
                    , leaseId = Nothing
                    , provider = OpenRouterProvider
                    }
                `shouldBe` "OpenRouter"

    describe "preferredOpenAiTokenProvider" do
        it "keeps using the selected account until it fails" do
            pool <- OpenAI.newPool
                [ testAuthStateFor "acc-1"
                , testAuthStateFor "acc-2"
                ]
                (pure . Right)
            fallback <- OpenAICredential.poolTokenProvider pool
            preferred <- newIORef (Just "acc-2")
            let provider =
                    preferredOpenAiTokenProvider
                        preferred
                        pool
                        fallback

            first <- getNextToken provider Nothing
            second <- getNextToken provider Nothing

            fmap (.accountId) first `shouldBe` Right "acc-2"
            fmap (.accountId) second `shouldBe` Right "acc-2"

            getNextToken provider
                (Just
                    FailedCredential
                        { credential =
                            Credential
                                { accessToken = "token-acc-2"
                                , accountId = "acc-2"
                                , leaseId = Nothing
                                , provider = OpenAIProvider
                                }
                        , failure =
                            AccountRateLimited
                                { retryAfterSeconds = Just 60
                                }
                        , failureReason = testRateLimitReason (Just 60)
                        })
                >>= \case
                    Right credential ->
                        credential.accountId `shouldBe` "acc-1"
                    Left err ->
                        expectationFailure
                            ("expected fallback account, got " <> show err)
            readIORef preferred `shouldReturn` Nothing

        it "falls back when the preferred account is already cooling down" do
            pool <- OpenAI.newPool
                [ testAuthStateFor "acc-1"
                , testAuthStateFor "acc-2"
                ]
                (pure . Right)
            fallback <- OpenAICredential.poolTokenProvider pool
            preferred <- newIORef (Just "acc-2")
            let provider =
                    preferredOpenAiTokenProvider
                        preferred
                        pool
                        fallback

            OpenAI.reportRateLimit pool "acc-2" (Just 60)

            getNextToken provider Nothing >>= \case
                Right credential ->
                    credential.accountId `shouldBe` "acc-1"
                Left err ->
                    expectationFailure
                        ("expected fallback account, got " <> show err)
            readIORef preferred `shouldReturn` Nothing

        it "keeps the selection when another credential reports failure" do
            pool <- OpenAI.newPool
                [ testAuthStateFor "acc-1"
                , testAuthStateFor "acc-2"
                ]
                (pure . Right)
            fallback <- OpenAICredential.poolTokenProvider pool
            preferred <- newIORef (Just "acc-2")
            let provider =
                    preferredOpenAiTokenProvider
                        preferred
                        pool
                        fallback

            getNextToken provider
                (Just
                    FailedCredential
                        { credential =
                            Credential
                                { accessToken = "token-acc-1"
                                , accountId = "acc-1"
                                , leaseId = Nothing
                                , provider = OpenAIProvider
                                }
                        , failure =
                            AccountRateLimited
                                { retryAfterSeconds = Just 60
                                }
                        , failureReason = testRateLimitReason (Just 60)
                        })
                >>= \case
                    Right credential ->
                        credential.accountId `shouldBe` "acc-2"
                    Left err ->
                        expectationFailure
                            ("expected selected account, got " <> show err)
            readIORef preferred `shouldReturn` Just "acc-2"

    describe "OpenAI account pools" do
        it "loads every enabled managed account into one pool" $
            withTempHome openAiManagedPoolTest
        it "does not mix subscription and API-credit accounts" $
            withTempHome openAiBillingPoolTest
        it "uses API billing when only API-credit accounts exist" $
            withTempHome openAiApiBillingPoolTest
        it "discovers an account connected after the session started" $
            withTempHome openAiManagedPoolDiscoveryTest
        it "combines distinct managed and ~/.codex accounts" $
            withTempHome openAiManagedAndFilePoolTest
        it "deduplicates duplicate accounts with managed precedence" $
            withTempHome openAiDeduplicationTest
        it "does not let a disabled managed source shadow ~/.codex" $
            withTempHome openAiDisabledSourceTest
        it "uses the login email as the active account label" $
            withTempHome openAiAccountLabelTest

    describe "loaded billing" do
        it "classifies an OpenRouter API key as API-credit billed" $
            withTempHome \_ ->
                withEnv "OPENROUTER_API_KEY" (Just "openrouter-key") do
                    loadAuth (Just OpenRouterProvider) >>= \case
                        Left err -> expectationFailure (Text.unpack err)
                        Right loaded ->
                            tokenProviderBillingMode loaded.loadedTokenProvider
                                `shouldBe` ApiBilled

        it "does not trust managed metadata to make OpenRouter subscription billed" $
            withTempHome \_ -> do
                upsertManagedCredential
                    ManagedCredential
                        { managedId = "openrouter"
                        , managedProvider = OpenRouterProvider
                        , managedAccountId = "openrouter"
                        , managedLabel = "OpenRouter"
                        , managedBilling = SubscriptionBilled
                        , managedAuthKind = ManagedBearerToken
                        , managedEnabled = True
                        }
                    (ManagedSecret "openrouter" "openrouter-key")
                    `shouldReturn` Right ()
                loadAuth (Just OpenRouterProvider) >>= \case
                    Left err -> expectationFailure (Text.unpack err)
                    Right loaded ->
                        tokenProviderBillingMode loaded.loadedTokenProvider
                            `shouldBe` ApiBilled

        it "classifies an external Grok access token as subscription billed" $
            withTempHome \_ ->
                withCleanGrokEnv $
                    withEnv "GROK_ACCESS_TOKEN" (Just "grok-token") do
                        loadAuth (Just XAIProvider) >>= \case
                            Left err -> expectationFailure (Text.unpack err)
                            Right loaded ->
                                tokenProviderBillingMode
                                    loaded.loadedTokenProvider
                                    `shouldBe` SubscriptionBilled

        it "uses stored billing for a managed XAI bearer token" $
            withTempHome \_ ->
                withCleanGrokEnv do
                    upsertManagedCredential
                        ManagedCredential
                            { managedId = "xai-api"
                            , managedProvider = XAIProvider
                            , managedAccountId = "xai-api"
                            , managedLabel = "xAI API"
                            , managedBilling = ApiBilled
                            , managedAuthKind = ManagedBearerToken
                            , managedEnabled = True
                            }
                        (ManagedSecret "xai-api" "xai-api-key")
                        `shouldReturn` Right ()
                    loadAuth (Just XAIProvider) >>= \case
                        Left err -> expectationFailure (Text.unpack err)
                        Right loaded ->
                            tokenProviderBillingMode
                                loaded.loadedTokenProvider
                                `shouldBe` ApiBilled

    describe "openAIOAuthClientId" do
        it "uses the Codex public client id by default" do
            openAIOAuthClientId Nothing
                `shouldBe` "app_EMoamEEZ73f0CkXaXp7hrann"

        it "allows an application-specific override" do
            openAIOAuthClientId (Just "custom-client")
                `shouldBe` "custom-client"

    describe "xaiOAuthClientId" do
        it "uses the Grok CLI public client id by default" do
            xaiOAuthClientId Nothing
                `shouldBe` "b1a00492-073a-47ea-816f-4c329264a828"

        it "allows an application-specific override" do
            xaiOAuthClientId (Just "custom-client")
                `shouldBe` "custom-client"

    describe "openaiAuthStateFromJson" do
        it "reads the login file tokens object" do
            let encoded = Aeson.encode $ Aeson.object
                    [ "auth_mode" .= ("chatgpt" :: Text)
                    , "tokens" .= Aeson.object
                        [ "access_token" .= ("tok" :: Text)
                        , "refresh_token" .= ("ref" :: Text)
                        , "account_id" .= ("acc-1" :: Text)
                        , "id_token" .= ("id" :: Text)
                        ]
                    ]
            case openaiAuthStateFromJson epoch encoded of
                Just AuthState{accessToken, refreshToken, accountId, idToken} -> do
                    accessToken `shouldBe` "tok"
                    refreshToken `shouldBe` "ref"
                    accountId `shouldBe` "acc-1"
                    idToken `shouldBe` Just "id"
                Nothing -> expectationFailure "expected auth state"

        it "rejects objects without an access token" do
            openaiAuthStateFromJson epoch (Aeson.encode (Aeson.object []))
                `shouldSatisfy` isNothing

    describe "openAiAuthStateChanged" do
        it "detects access-token rotation without refresh-token rotation" do
            openAiAuthStateChanged
                (testAuthState "old-access" "same-refresh")
                (testAuthState "new-access" "same-refresh")
                `shouldBe` True
        it "does not report an unchanged state" do
            let state = testAuthState "access" "refresh"
            openAiAuthStateChanged state state `shouldBe` False

    describe "applyGrokAuthTokens" do
        it "updates grok CLI nested key/refresh fields without dropping profile data" do
            let expiresAt = addUTCTime 3600 epoch
                original = Aeson.object
                    [ "https://auth.x.ai::cli-id" .= Aeson.object
                        [ "auth_mode" .= ("oidc" :: Text)
                        , "email" .= ("marc@example.com" :: Text)
                        , "key" .= ("stale" :: Text)
                        , "oidc_client_id" .= ("cli-id" :: Text)
                        , "principal_id" .= ("user-1" :: Text)
                        , "principal_type" .= ("user" :: Text)
                        , "refresh_token" .= ("refresh-old" :: Text)
                        ]
                    ]
                state = GrokAuthState
                    "fresh" (Just "refresh-new") (Just "id-new") (Just expiresAt)
            case applyGrokAuthTokens state original of
                Nothing -> expectationFailure "expected patched grok CLI JSON"
                Just patched ->
                    case grokAuthStateFromJson epoch
                        (TextEncoding.decodeUtf8 (LBS.toStrict (Aeson.encode patched))) of
                        Nothing ->
                            expectationFailure "expected patched Grok auth state"
                        Just parsed -> do
                            parsed.grokAccessToken `shouldBe` "fresh"
                            parsed.grokRefreshToken `shouldBe` Just "refresh-new"
                            grokEmailFromAuthJson
                                (TextEncoding.decodeUtf8
                                    (LBS.toStrict (Aeson.encode patched)))
                                `shouldBe` Just "marc@example.com"
                            grokOAuthOptionsFromAuthJson "default"
                                (TextEncoding.decodeUtf8
                                    (LBS.toStrict (Aeson.encode patched)))
                                `shouldSatisfy` \options ->
                                    options.clientId == "cli-id"
                                        && options.principalType == Just "user"
                                        && options.principalId == Just "user-1"

        it "updates a flat access_token document" do
            let original = Aeson.object
                    [ "access_token" .= ("stale" :: Text)
                    , "refresh_token" .= ("refresh-old" :: Text)
                    ]
                state = GrokAuthState "fresh" (Just "refresh-new") Nothing Nothing
            applyGrokAuthTokens state original
                `shouldBe` Just
                    (Aeson.object
                        [ "access_token" .= ("fresh" :: Text)
                        , "refresh_token" .= ("refresh-new" :: Text)
                        ])

    describe "grokNeedsRefresh" do
        it "refreshes tokens at or inside the 10-minute skew" do
            grokNeedsRefresh epoch
                (GrokAuthState "tok" (Just "refresh") Nothing
                    (Just (addUTCTime 600 epoch)))
                `shouldBe` True
            grokNeedsRefresh epoch
                (GrokAuthState "tok" (Just "refresh") Nothing
                    (Just (addUTCTime 601 epoch)))
                `shouldBe` False

        it "does not refresh when expiry is unknown" do
            grokNeedsRefresh epoch
                (GrokAuthState "tok" (Just "refresh") Nothing Nothing)
                `shouldBe` False

    describe "managedGeminiTokenProvider" do
        it "refreshes an expiring Google token and persists the rotation" $
            withTempHome \_ -> do
                now <- getCurrentTime
                refreshes <- newIORef (0 :: Int)
                let metadata = ManagedCredential
                        { managedId = "gemini-google"
                        , managedProvider = GeminiProvider
                        , managedAccountId = "person@example.com"
                        , managedLabel = "person@example.com"
                        , managedBilling = SubscriptionBilled
                        , managedAuthKind = ManagedGeminiAuthJson
                        , managedEnabled = True
                        }
                    state = Gemini.GeminiAuthState
                        { accessToken = "stale"
                        , refreshToken = Just "refresh-old"
                        , expiresAt = Just (addUTCTime 60 now)
                        , email = "person@example.com"
                        , projectId = "managed-project"
                        , userTier = "free-tier"
                        }
                    secret = ManagedSecret
                        metadata.managedId
                        (geminiAuthStateToJson state)
                    refresh refreshToken = do
                        refreshToken `shouldBe` "refresh-old"
                        modifyIORef' refreshes (+ 1)
                        pure $ Right Gemini.OAuthTokens
                            { accessToken = "fresh"
                            , refreshToken = Just "refresh-new"
                            , expiresInSeconds = Just 3600
                            , tokenType = Just "Bearer"
                            , scope = Nothing
                            }
                    expected = Credential
                        { accessToken = "fresh"
                        , accountId = "person@example.com"
                        , leaseId = Just "code-assist:managed-project"
                        , provider = GeminiProvider
                        }
                upsertManagedCredential metadata secret
                    `shouldReturn` Right ()
                provider <- managedGeminiTokenProvider
                    metadata secret state refresh
                getNextToken provider Nothing `shouldReturn` Right expected
                getNextToken provider Nothing `shouldReturn` Right expected
                readIORef refreshes `shouldReturn` 1
                loadManagedCredentials >>= \case
                    Right [(_, stored)]
                        | Just persisted <-
                            geminiAuthStateFromJson stored.secretPayload -> do
                                persisted.accessToken `shouldBe` "fresh"
                                persisted.refreshToken
                                    `shouldBe` Just "refresh-new"
                    other ->
                        expectationFailure
                            ("expected persisted Gemini OAuth state, got "
                                <> show other)

        it "does not refresh a credential disabled on disk" $
            withTempHome \_ -> do
                now <- getCurrentTime
                refreshes <- newIORef (0 :: Int)
                let metadata = ManagedCredential
                        { managedId = "gemini-disabled"
                        , managedProvider = GeminiProvider
                        , managedAccountId = "person@example.com"
                        , managedLabel = "person@example.com"
                        , managedBilling = SubscriptionBilled
                        , managedAuthKind = ManagedGeminiAuthJson
                        , managedEnabled = True
                        }
                    state = Gemini.GeminiAuthState
                        { accessToken = "stale"
                        , refreshToken = Just "refresh-old"
                        , expiresAt = Just (addUTCTime 60 now)
                        , email = "person@example.com"
                        , projectId = "managed-project"
                        , userTier = "free-tier"
                        }
                    secret = ManagedSecret
                        metadata.managedId
                        (geminiAuthStateToJson state)
                    refresh _ = do
                        modifyIORef' refreshes (+ 1)
                        pure $ Left "refresh should not run"
                upsertManagedCredential metadata secret
                    `shouldReturn` Right ()
                provider <- managedGeminiTokenProvider
                    metadata secret state refresh
                setManagedCredentialEnabled metadata.managedId False
                    `shouldReturn` Right ()
                result <- getNextToken provider Nothing
                result `shouldBe` Left
                    (CredentialError
                        "managed Gemini credential is disabled: gemini-disabled")
                readIORef refreshes `shouldReturn` 0

    describe "grokCredentialFromAuthJson" do
        it "accepts a plain access_token object or a nested grok CLI map" do
            grokCredentialFromAuthJson "{\"access_token\":\"abc\"}"
                `shouldBe` Just "abc"
            grokCredentialFromAuthJson
                "{\"issuer::client\":{\"access_token\":\"nested-tok\"}}"
                `shouldBe` Just "nested-tok"

    describe "grokAuthStateFromJson" do
        it "loads refresh and absolute expiry state from managed JSON" do
            let expiresAt = addUTCTime 3600 epoch
                encoded = TextEncoding.decodeUtf8 . LBS.toStrict . Aeson.encode $
                    Aeson.object
                        [ "access_token" .= ("access" :: Text)
                        , "refresh_token" .= ("refresh" :: Text)
                        , "id_token" .= ("id" :: Text)
                        , "expires_at" .= expiresAt
                        ]
            case grokAuthStateFromJson epoch encoded of
                Just state -> do
                    state.grokAccessToken `shouldBe` "access"
                    state.grokRefreshToken `shouldBe` Just "refresh"
                    state.grokIdToken `shouldBe` Just "id"
                    state.grokExpiresAt `shouldBe` Just expiresAt
                Nothing -> expectationFailure "expected Grok auth state"

    describe "managedGrokTokenProvider" do
        it "refreshes an expiring token once and persists rotated tokens" $
            withTempHome \_ -> do
                now <- getCurrentTime
                refreshes <- newIORef (0 :: Int)
                let refresh refreshToken = do
                        refreshToken `shouldBe` "refresh-old"
                        modifyIORef' refreshes (+ 1)
                        pure (Right refreshedGrokTokens)
                    state = expiringGrokState now
                    secret = managedGrokSecretFor state
                upsertManagedCredential managedGrokMetadata secret
                    `shouldReturn` Right ()
                provider <- managedGrokTokenProvider
                    managedGrokMetadata
                    secret
                    state
                    refresh
                getNextToken provider Nothing `shouldReturn`
                    Right freshManagedGrok
                getNextToken provider Nothing `shouldReturn`
                    Right freshManagedGrok
                readIORef refreshes `shouldReturn` 1
                loaded <- loadManagedCredentials
                case loaded of
                    Right [(_, stored)] ->
                        case grokAuthStateFromJson now stored.secretPayload of
                            Just persisted -> do
                                persisted.grokAccessToken `shouldBe` "fresh"
                                persisted.grokRefreshToken
                                    `shouldBe` Just "refresh-new"
                            Nothing ->
                                expectationFailure
                                    "expected persisted Grok OAuth state"
                    other ->
                        expectationFailure
                            ("expected one managed credential, got "
                                <> show other)

        it "force-refreshes an unexpired token after auth rejection" $
            withTempHome \_ -> do
                now <- getCurrentTime
                let state = unexpiredGrokState now
                    secret = managedGrokSecretFor state
                upsertManagedCredential managedGrokMetadata secret
                    `shouldReturn` Right ()
                provider <- managedGrokTokenProvider
                    managedGrokMetadata secret state
                    (const (pure (Right refreshedGrokTokens)))
                getNextToken provider
                    (Just
                        (FailedCredential
                            staleManagedGrok
                            AccountAuthenticationRejected
                            testAuthenticationReason))
                    `shouldReturn` Right freshManagedGrok

        it "adopts a token rotated by another process without refreshing" $
            withTempHome \_ -> do
                now <- getCurrentTime
                refreshes <- newIORef (0 :: Int)
                let staleState = expiringGrokState now
                    staleSecret = managedGrokSecretFor staleState
                    currentState = adoptedGrokState now
                    refresh _ = do
                        modifyIORef' refreshes (+ 1)
                        pure (Right refreshedGrokTokens)
                upsertManagedCredential managedGrokMetadata
                    (managedGrokSecretFor currentState)
                    `shouldReturn` Right ()
                provider <- managedGrokTokenProvider
                    managedGrokMetadata staleSecret staleState refresh
                getNextToken provider Nothing `shouldReturn`
                    Right adoptedManagedGrok
                readIORef refreshes `shouldReturn` 0

    describe "externalGrokTokenProvider" do
        it "refreshes an expiring grok CLI file and preserves nested profile fields" $
            withTempHome \home -> do
                now <- getCurrentTime
                let path = grokAuthPath home
                    original = nestedGrokAuthJson "stale" "refresh-old"
                        (Just (addUTCTime (-1) now))
                writeGrokAuthFile home original
                refreshes <- newIORef (0 :: Int)
                let loaded = ExternalGrokLoaded
                        { grokSelectionId = "file"
                        , grokState = expiringGrokState now
                        , grokSource = GrokSourceFile path
                        , grokRawJson = Just original
                        }
                    refresh refreshToken = do
                        refreshToken `shouldBe` "refresh-old"
                        modifyIORef' refreshes (+ 1)
                        pure (Right refreshedGrokTokens)
                provider <- externalGrokTokenProvider loaded refresh
                getNextToken provider Nothing `shouldReturn`
                    Right (grokCredentialFor "fresh")
                getNextToken provider Nothing `shouldReturn`
                    Right (grokCredentialFor "fresh")
                readIORef refreshes `shouldReturn` 1
                persisted <- LBS.readFile (toFilePath path)
                grokAuthStateFromJson now
                    (TextEncoding.decodeUtf8 (LBS.toStrict persisted))
                    `shouldSatisfy` \case
                        Just state ->
                            state.grokAccessToken == "fresh"
                                && state.grokRefreshToken == Just "refresh-new"
                        Nothing -> False
                grokEmailFromAuthJson
                    (TextEncoding.decodeUtf8 (LBS.toStrict persisted))
                    `shouldBe` Just "marc@example.com"

        it "force-refreshes an unexpired file after auth rejection" $
            withTempHome \home -> do
                now <- getCurrentTime
                let path = grokAuthPath home
                    original = nestedGrokAuthJson "stale" "refresh-old"
                        (Just (addUTCTime 3600 now))
                    loaded = ExternalGrokLoaded
                        { grokSelectionId = "file"
                        , grokState = unexpiredGrokState now
                        , grokSource = GrokSourceFile path
                        , grokRawJson = Just original
                        }
                writeGrokAuthFile home original
                provider <- externalGrokTokenProvider loaded
                    (const (pure (Right refreshedGrokTokens)))
                getNextToken provider
                    (Just
                        (FailedCredential
                            staleGrok
                            AccountAuthenticationRejected
                            testAuthenticationReason))
                    `shouldReturn` Right (grokCredentialFor "fresh")

        it "adopts a token rotated on disk without refreshing" $
            withTempHome \home -> do
                now <- getCurrentTime
                refreshes <- newIORef (0 :: Int)
                let path = grokAuthPath home
                    stale = nestedGrokAuthJson "stale" "refresh-old"
                        (Just (addUTCTime (-1) now))
                    current = nestedGrokAuthJson "adopted" "refresh-adopted"
                        (Just (addUTCTime 3600 now))
                    loaded = ExternalGrokLoaded
                        { grokSelectionId = "file"
                        , grokState = expiringGrokState now
                        , grokSource = GrokSourceFile path
                        , grokRawJson = Just stale
                        }
                    refresh _ = do
                        modifyIORef' refreshes (+ 1)
                        pure (Right refreshedGrokTokens)
                writeGrokAuthFile home current
                provider <- externalGrokTokenProvider loaded refresh
                getNextToken provider Nothing `shouldReturn`
                    Right (grokCredentialFor "adopted")
                readIORef refreshes `shouldReturn` 0

        it "fails closed when an expired file has no refresh token" $
            withTempHome \home -> do
                now <- getCurrentTime
                let path = grokAuthPath home
                    expiresAt = addUTCTime (-1) now
                    raw = TextEncoding.decodeUtf8 . LBS.toStrict . Aeson.encode $
                        Aeson.object
                            [ "access_token" .= ("stale" :: Text)
                            , "expires_at" .= expiresAt
                            ]
                    state = GrokAuthState "stale" Nothing Nothing (Just expiresAt)
                    loaded = ExternalGrokLoaded
                        { grokSelectionId = "file"
                        , grokState = state
                        , grokSource = GrokSourceFile path
                        , grokRawJson = Just raw
                        }
                writeGrokAuthFile home raw
                provider <- externalGrokTokenProvider loaded
                    (\_ -> expectationFailure "refresh should not run"
                        >> fail "refresh")
                result <- getNextToken provider Nothing
                case result of
                    Left (CredentialError message) ->
                        Text.unpack message `shouldContain` "no refresh token"
                    other ->
                        expectationFailure
                            ("expected CredentialError, got " <> show other)

    describe "refreshGrokLoginPayload" do
        it "returns a still-valid grok CLI file without contacting xAI" $
            withTempHome \home -> do
                now <- getCurrentTime
                let path = grokAuthPath home
                    payload = nestedGrokAuthJson "live" "refresh-live"
                        (Just (addUTCTime 3600 now))
                writeGrokAuthFile home payload
                refreshGrokLoginPayload Nothing (Just path) payload >>= \case
                    Left err ->
                        expectationFailure ("expected live payload, got " <> show err)
                    Right (state, returned) -> do
                        state.grokAccessToken `shouldBe` "live"
                        grokNeedsRefresh now state `shouldBe` False
                        returned `shouldBe` payload

    describe "staticCredentialProvider" do
        it "preserves rate-limit cooldowns for managed bearer tokens" do
            before <- getCurrentTime
            result <- getNextToken
                (staticCredentialProvider SubscriptionBilled staleGrok)
                (Just
                    (FailedCredential
                        staleGrok
                        (AccountRateLimited (Just 7))
                        (testRateLimitReason (Just 7))))
            case result of
                Left CredentialsExhausted{retryAt} ->
                    diffUTCTime retryAt before `shouldSatisfy` (>= 7)
                other ->
                    expectationFailure
                        ("expected CredentialsExhausted, got " <> show other)

    describe "grokEmailFromAuthJson" do
        it "reads profile emails and nested id-token claims" do
            let token =
                    "e30.eyJlbWFpbCI6InBlcnNvbkBleGFtcGxlLmNvbSJ9."
            grokEmailFromAuthJson
                "{\"issuer::client\":{\"email\":\"profile@example.com\"}}"
                `shouldBe` Just "profile@example.com"
            grokEmailFromAuthJson
                ("{\"issuer::client\":{\"id_token\":\"" <> token <> "\"}}")
                `shouldBe` Just "person@example.com"

    describe "reloadableFileCredentialProvider" do
        it "returns the cached credential without reloading" do
            loads <- newIORef (0 :: Int)
            provider <- reloadableFileCredentialProvider
                XAIProvider SubscriptionBilled staleGrok
                (modifyIORef' loads (+ 1) >> pure (Just freshGrok))
            first <- getNextToken provider Nothing
            second <- getNextToken provider Nothing
            first `shouldBe` Right staleGrok
            second `shouldBe` Right staleGrok
            readIORef loads `shouldReturn` 0

        it "reloads after an authentication rejection" do
            loads <- newIORef (0 :: Int)
            provider <- reloadableFileCredentialProvider
                XAIProvider SubscriptionBilled staleGrok
                (modifyIORef' loads (+ 1) >> pure (Just freshGrok))
            reloaded <- getNextToken provider (Just FailedCredential
                { credential = staleGrok
                , failure = AccountAuthenticationRejected
                , failureReason = testAuthenticationReason
                })
            reloaded `shouldBe` Right freshGrok
            getNextToken provider Nothing `shouldReturn` Right freshGrok
            readIORef loads `shouldReturn` 1

        it "rejects an unchanged reload after authentication failure" do
            provider <- reloadableFileCredentialProvider
                XAIProvider SubscriptionBilled staleGrok
                (pure (Just staleGrok))
            result <- getNextToken provider (Just FailedCredential
                { credential = staleGrok
                , failure = AccountAuthenticationRejected
                , failureReason = testAuthenticationReason
                })
            case result of
                Left (CredentialError message) ->
                    Text.unpack message `shouldContain`
                        "reloaded credential is unchanged"
                other -> expectationFailure ("expected CredentialError, got " <> show other)

openAiManagedPoolTest :: OsPath -> IO ()
openAiManagedPoolTest _ =
    withCleanOpenAiEnv do
        storeOpenAiAccount "managed-a" "acc-a" True "token-a"
        storeOpenAiAccount "managed-b" "acc-b" True "token-b"
        storeOpenAiAccount "managed-disabled" "acc-disabled" False "token-disabled"
        loaded <- loadAuth (Just OpenAIProvider)
        expectLoadedBilling loaded `shouldReturn` SubscriptionBilled
        pool <- expectOpenAiPool loaded
        OpenAI.allAccountIds pool `shouldReturn` ["acc-b", "acc-a"]

openAiBillingPoolTest :: OsPath -> IO ()
openAiBillingPoolTest _ =
    withCleanOpenAiEnv do
        storeOpenAiAccount "subscription" "acc-subscription" True
            "subscription-token"
        storeOpenAiAccountWithBilling
            ApiBilled "api" "acc-api" True "api-token"
        loaded <- loadAuth (Just OpenAIProvider)
        expectLoadedBilling loaded `shouldReturn` SubscriptionBilled
        pool <- expectOpenAiPool loaded
        OpenAI.allAccountIds pool `shouldReturn` ["acc-subscription"]

openAiApiBillingPoolTest :: OsPath -> IO ()
openAiApiBillingPoolTest _ =
    withCleanOpenAiEnv do
        storeOpenAiAccountWithBilling
            ApiBilled "api" "acc-api" True "api-token"
        loaded <- loadAuth (Just OpenAIProvider)
        expectLoadedBilling loaded `shouldReturn` ApiBilled
        pool <- expectOpenAiPool loaded
        OpenAI.allAccountIds pool `shouldReturn` ["acc-api"]

openAiManagedPoolDiscoveryTest :: OsPath -> IO ()
openAiManagedPoolDiscoveryTest _ =
    withCleanOpenAiEnv do
        storeOpenAiAccount "managed-a" "acc-a" True "token-a"
        storeOpenAiAccount "managed-b" "acc-b" True "token-b"
        loaded <- loadAuth (Just OpenAIProvider)
        pool <- expectOpenAiPool loaded
        initialAccountIds <- OpenAI.allAccountIds pool
        initialAccountIds `shouldMatchList` ["acc-a", "acc-b"]
        mapM_ (\accountId -> OpenAI.reportRateLimit pool accountId (Just 60))
            initialAccountIds

        -- A non-JWT OAuth token is treated as requiring refresh. Keep this
        -- fixture valid far into the future so the checkout tests discovery,
        -- not the real OAuth endpoint.
        storeOpenAiAccount "managed-c" "acc-c" True
            "e30.eyJleHAiOjQxMDI0NDQ4MDB9."

        result <- OpenAI.getAccessToken pool
        case result of
            Right (_, accountId) -> accountId `shouldBe` "acc-c"
            Left err ->
                expectationFailure
                    ("expected newly connected account, got " <> show err)
        OpenAI.allAccountIds pool
            `shouldReturn` initialAccountIds <> ["acc-c"]

openAiManagedAndFilePoolTest :: OsPath -> IO ()
openAiManagedAndFilePoolTest home =
    withCleanOpenAiEnv do
        storeOpenAiAccount "managed-a" "acc-a" True "token-a"
        writeCodexAuthFile home "acc-file" "token-file"
        loaded <- loadAuth (Just OpenAIProvider)
        pool <- expectOpenAiPool loaded
        OpenAI.allAccountIds pool `shouldReturn` ["acc-a", "acc-file"]

openAiDeduplicationTest :: OsPath -> IO ()
openAiDeduplicationTest home =
    withCleanOpenAiEnv do
        storeOpenAiAccount "managed-a" "acc-a" True "managed-token"
        writeCodexAuthFile home "acc-a" "file-token"
        loaded <- loadAuth (Just OpenAIProvider)
        pool <- expectOpenAiPool loaded
        snapshots <- OpenAI.snapshotAccounts pool
        map ((.accountId) . (.snapshotAuth)) snapshots `shouldBe` ["acc-a"]
        map ((.accessToken) . (.snapshotAuth)) snapshots `shouldBe` ["managed-token"]

openAiDisabledSourceTest :: OsPath -> IO ()
openAiDisabledSourceTest home =
    withCleanOpenAiEnv do
        storeOpenAiAccount "managed-file" "acc-file" False "disabled-token"
        writeCodexAuthFile home "acc-file" "file-token"
        loaded <- loadAuth (Just OpenAIProvider)
        pool <- expectOpenAiPool loaded
        snapshots <- OpenAI.snapshotAccounts pool
        map ((.accessToken) . (.snapshotAuth)) snapshots `shouldBe` ["file-token"]

openAiAccountLabelTest :: OsPath -> IO ()
openAiAccountLabelTest home =
    withCleanOpenAiEnv do
        let codexDirectory = toFilePath home </> ".codex"
            accessToken :: Text
            accessToken = "e30.eyJleHAiOjQxMDI0NDQ4MDB9."
            idToken :: Text
            idToken =
                "e30.eyJlbWFpbCI6InBlcnNvbkBleGFtcGxlLmNvbSJ9."
        createDirectoryIfMissing True codexDirectory
        LBS.writeFile
            (codexDirectory </> "auth.json")
            (Aeson.encode $ Aeson.object
                [ "tokens" .= Aeson.object
                    [ "access_token" .= accessToken
                    , "refresh_token" .= ("refresh" :: Text)
                    , "account_id" .= ("acc-email" :: Text)
                    , "id_token" .= idToken
                    ]
                ])
        loadAuth (Just OpenAIProvider) >>= \case
            Left err -> expectationFailure (Text.unpack err)
            Right loaded ->
                getNextToken loaded.loadedTokenProvider Nothing >>= \case
                    Left err ->
                        expectationFailure
                            ("expected credential, got " <> show err)
                    Right credential ->
                        loaded.loadedAccountLabel credential
                            `shouldReturn` "person@example.com"

expectOpenAiPool :: Either Text LoadedAuth -> IO OpenAI.Pool
expectOpenAiPool = \case
    Left err -> expectationFailure (Text.unpack err) >> fail "missing pool"
    Right loaded -> case loaded.loadedOpenAiPool of
        Nothing -> expectationFailure "expected OpenAI account pool"
            >> fail "missing pool"
        Just pool -> pure pool

expectLoadedBilling :: Either Text LoadedAuth -> IO BillingMode
expectLoadedBilling = \case
    Left err -> expectationFailure (Text.unpack err)
        >> fail "missing loaded auth"
    Right loaded ->
        pure (tokenProviderBillingMode loaded.loadedTokenProvider)

storeOpenAiAccount :: Text -> Text -> Bool -> Text -> IO ()
storeOpenAiAccount credentialId accountId enabled token =
    storeOpenAiAccountWithBilling
        SubscriptionBilled credentialId accountId enabled token

storeOpenAiAccountWithBilling
    :: BillingMode
    -> Text
    -> Text
    -> Bool
    -> Text
    -> IO ()
storeOpenAiAccountWithBilling billing credentialId accountId enabled token =
    upsertManagedCredential
        (openAiMetadata billing credentialId accountId enabled)
        (ManagedSecret credentialId (authJson accountId token))
        `shouldReturn` Right ()

storeManagedAccount
    :: BillingMode
    -> Provider
    -> Text
    -> Text
    -> Text
    -> Bool
    -> Text
    -> IO ()
storeManagedAccount
    billing
    provider
    credentialId
    accountId
    label
    enabled
    token =
        upsertManagedCredential
            ManagedCredential
                { managedId = credentialId
                , managedProvider = provider
                , managedAccountId = accountId
                , managedLabel = label
                , managedBilling = billing
                , managedAuthKind = ManagedBearerToken
                , managedEnabled = enabled
                }
            (ManagedSecret credentialId token)
            `shouldReturn` Right ()

openAiMetadata
    :: BillingMode
    -> Text
    -> Text
    -> Bool
    -> ManagedCredential
openAiMetadata billing credentialId accountId enabled = ManagedCredential
    { managedId = credentialId
    , managedProvider = OpenAIProvider
    , managedAccountId = accountId
    , managedLabel = "ChatGPT"
    , managedBilling = billing
    , managedAuthKind = ManagedOpenAIAuthJson
    , managedEnabled = enabled
    }

writeCodexAuthFile :: OsPath -> Text -> Text -> IO ()
writeCodexAuthFile home accountId token = do
    let codexDirectory = toFilePath home </> ".codex"
    createDirectoryIfMissing True codexDirectory
    LBS.writeFile
        (codexDirectory </> "auth.json")
        (Aeson.encode (authValue accountId token))

authJson :: Text -> Text -> Text
authJson accountId token =
    TextEncoding.decodeUtf8 (LBS.toStrict (Aeson.encode (authValue accountId token)))

authValue :: Text -> Text -> Aeson.Value
authValue accountId token = Aeson.object
    [ "tokens" .= tokenObject accountId token
    ]

tokenObject :: Text -> Text -> Aeson.Value
tokenObject accountId token = Aeson.object
    [ "access_token" .= token
    , "refresh_token" .= ("refresh-" <> accountId)
    , "account_id" .= accountId
    ]

testAuthenticationReason :: CredentialExhaustionReason
testAuthenticationReason = ExhaustedByAuthentication
    { exhaustionErrorType = Nothing
    , exhaustionStatusCode = Just 401
    }

farFutureAccessToken :: Text
farFutureAccessToken =
    "e30.eyJleHAiOjQxMDI0NDQ4MDB9."

testRateLimitReason :: Maybe Int -> CredentialExhaustionReason
testRateLimitReason retryAfter = ExhaustedByRateLimit
    { exhaustionErrorType = Nothing
    , exhaustionStatusCode = Just 429
    , exhaustionRetryAfter = retryAfter
    }

staleGrok :: Credential
staleGrok = Credential
    { accessToken = "stale"
    , accountId = "acc-stale"
    , leaseId = Nothing
    , provider = XAIProvider
    }

freshGrok :: Credential
freshGrok = Credential
    { accessToken = "fresh"
    , accountId = "acc-fresh"
    , leaseId = Nothing
    , provider = XAIProvider
    }

managedGrokMetadata :: ManagedCredential
managedGrokMetadata =
    ManagedCredential "grok-test" XAIProvider "account" "Grok"
        SubscriptionBilled ManagedGrokAuthJson True

managedGrokSecretFor :: GrokAuthState -> ManagedSecret
managedGrokSecretFor state =
    ManagedSecret
        "grok-test"
        (TextEncoding.decodeUtf8
            (LBS.toStrict (Aeson.encode (grokAuthStateToJson state))))

expiringGrokState :: UTCTime -> GrokAuthState
expiringGrokState now =
    GrokAuthState "stale" (Just "refresh-old") (Just "id-old")
        (Just (addUTCTime (-1) now))

unexpiredGrokState :: UTCTime -> GrokAuthState
unexpiredGrokState now =
    GrokAuthState "stale" (Just "refresh-old") (Just "id-old")
        (Just (addUTCTime 3600 now))

staleManagedGrok :: Credential
staleManagedGrok =
    Credential "stale" "account" Nothing XAIProvider

adoptedGrokState :: UTCTime -> GrokAuthState
adoptedGrokState now =
    GrokAuthState "adopted" (Just "refresh-adopted") Nothing
        (Just (addUTCTime 3600 now))

adoptedManagedGrok :: Credential
adoptedManagedGrok =
    Credential "adopted" "account" Nothing XAIProvider

refreshedGrokTokens :: XAIAuth.OAuthTokens
refreshedGrokTokens =
    XAIAuth.OAuthTokens "fresh" (Just "refresh-new") (Just "id-new")
        (Just 3600)

freshManagedGrok :: Credential
freshManagedGrok =
    Credential "fresh" "account" Nothing XAIProvider

grokCredentialFor :: Text -> Credential
grokCredentialFor token =
    Credential token "grok" Nothing XAIProvider

grokAuthPath :: OsPath -> OsPath
grokAuthPath home =
    fromFilePath (toFilePath home </> ".grok" </> "auth.json")

writeGrokAuthFile :: OsPath -> Text -> IO ()
writeGrokAuthFile home raw = do
    createDirectoryIfMissing True (toFilePath home </> ".grok")
    LBS.writeFile
        (toFilePath home </> ".grok" </> "auth.json")
        (LBS.fromStrict (TextEncoding.encodeUtf8 raw))

nestedGrokAuthJson :: Text -> Text -> Maybe UTCTime -> Text
nestedGrokAuthJson token refresh expiresAt =
    TextEncoding.decodeUtf8 . LBS.toStrict . Aeson.encode $
        Aeson.object
            [ "https://auth.x.ai::cli-id" .= Aeson.object
                ( [ "auth_mode" .= ("oidc" :: Text)
                  , "email" .= ("marc@example.com" :: Text)
                  , "key" .= token
                  , "oidc_client_id" .= ("cli-id" :: Text)
                  , "refresh_token" .= refresh
                  ]
                    <> maybe
                        []
                        (\time -> ["expires_at" .= time])
                        expiresAt
                )
            ]

testAuthState :: Text -> Text -> AuthState
testAuthState access refresh =
    AuthState access refresh "acc-test" Nothing epoch

testAuthStateFor :: Text -> AuthState
testAuthStateFor accountId =
    AuthState
        ("token-" <> accountId)
        ("refresh-" <> accountId)
        accountId
        Nothing
        epoch

epoch :: UTCTime
epoch = UTCTime (fromGregorian 2026 1 1) 0

withEnv :: String -> Maybe String -> IO a -> IO a
withEnv name value action =
    bracket
        (do
            previous <- lookupEnv name
            set value
            pure previous)
        set
        (const action)
  where
    set = \case
        Just current -> setEnv name current
        Nothing -> unsetEnv name

withCleanOpenAiEnv :: IO a -> IO a
withCleanOpenAiEnv action =
    foldr
        (\name next -> withEnv name Nothing next)
        action
        [ "OPENAI_API_KEY"
        , "CODEX_API_KEY"
        , "CODEX_ACCESS_TOKEN"
        , "CODEX_AUTH_JSON"
        , "CODEX_HOME"
        , "CODEX_ACCOUNT_ID"
        , "CODEX_ID_TOKEN"
        , "OPENAI_OAUTH_CLIENT_ID"
        ]

shouldLoadOpenAiDictationToken :: Text -> Expectation
shouldLoadOpenAiDictationToken expected =
    shouldLoadOpenAiDictationCredential ApiBilled expected ""

shouldLoadOpenAiDictationCredential
    :: BillingMode
    -> Text
    -> Text
    -> Expectation
shouldLoadOpenAiDictationCredential billing expected expectedAccount =
    loadOpenAiDictationAuth >>= \case
        Left err ->
            expectationFailure
                ("expected an OpenAI dictation credential, got "
                    <> Text.unpack err)
        Right loaded -> do
            loaded.loadedProvider `shouldBe` OpenAIProvider
            tokenProviderBillingMode loaded.loadedTokenProvider
                `shouldBe` billing
            getNextToken loaded.loadedTokenProvider Nothing >>= \case
                Left err ->
                    expectationFailure (show err)
                Right credential -> do
                    credential.provider `shouldBe` OpenAIProvider
                    credential.accessToken `shouldBe` expected
                    credential.accountId `shouldBe` expectedAccount

withCleanGrokEnv :: IO a -> IO a
withCleanGrokEnv action =
    foldr
        (\name next -> withEnv name Nothing next)
        action
        [ "GROK_AUTH_JSON"
        , "GROK_ACCESS_TOKEN"
        , "XAI_OAUTH_CLIENT_ID"
        ]

withTempHome :: (OsPath -> IO a) -> IO a
withTempHome action =
    bracket create removePathForcibly \home ->
        withEnv "HOME" (Just home) (action (fromFilePath home))
  where
    create = do
        temporary <- getTemporaryDirectory
        (path, handle) <- openTempFile temporary "agent-auth"
        hClose handle
        removeFile path
        createDirectory path
        pure path

saveTestGateway :: OsPath -> IO ()
saveTestGateway home =
    saveGatewayCredentialAt
        home
        testGatewayCredential
        `shouldReturn` Right ()

testGatewayCredential :: GatewayCredential
testGatewayCredential = GatewayCredential
    { gatewayBaseUrl = "https://gateway.example"
    , gatewayWebSocketUrl = "wss://gateway.example/v1/responses"
    , gatewayAccessToken = "gateway-token"
    }
