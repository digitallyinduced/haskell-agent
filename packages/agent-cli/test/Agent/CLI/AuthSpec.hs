module Agent.CLI.AuthSpec (spec) where

import Agent.CLI.Auth
import Agent.CLI.CredentialStore
import Agent.Error (ApiError(..))
import System.OsPath (OsPath, decodeUtf, unsafeEncodeUtf)
import Agent.OpenAI.Auth (AuthState(..))
import qualified Agent.OpenAI.Auth as OpenAI
import qualified Agent.OpenAI.Credential as OpenAICredential
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

fromFilePath = unsafeEncodeUtf
toFilePath path = either (error . show) id (decodeUtf path)

spec :: Spec
spec = do
    describe "probeLoadedAuth" do
        it "rejects auth whose accounts are currently cooling down" do
            let retryAt = UTCTime (fromGregorian 2026 8 21) 3600
                exhausted = LoadedAuth
                    { loadedProvider = XAIProvider
                    , loadedTokenProvider = tokenProvider SubscriptionBilled \_ ->
                        pure (Left (CredentialsExhausted retryAt))
                    , loadedAccountLabel = pure . credentialAccountLabel
                    , loadedOpenAiPool = Nothing
                    }
            result <- probeLoadedAuth exhausted
            case result of
                Left err -> err `shouldBe` CredentialsExhausted retryAt
                Right _ -> expectationFailure "expected exhausted auth"

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
                        })
                >>= \case
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
                        (FailedCredential staleManagedGrok
                            AccountAuthenticationRejected))
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

    describe "staticCredentialProvider" do
        it "preserves rate-limit cooldowns for managed bearer tokens" do
            before <- getCurrentTime
            result <- getNextToken
                (staticCredentialProvider SubscriptionBilled staleGrok)
                (Just
                    (FailedCredential staleGrok
                        (AccountRateLimited (Just 7))))
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
        [ "CODEX_ACCESS_TOKEN"
        , "CODEX_AUTH_JSON"
        , "CODEX_ACCOUNT_ID"
        , "CODEX_ID_TOKEN"
        , "OPENAI_OAUTH_CLIENT_ID"
        ]

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
