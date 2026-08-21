module Agent.CLI.AuthSpec (spec) where

import Agent.CLI.Auth
import Agent.CLI.CredentialStore
import Agent.Error (ApiError(..), ErrorType(..))
import Agent.OpenAI.Auth (AuthState(..))
import Agent.Provider
    ( AccountFailure(..)
    , Credential(..)
    , FailedCredential(..)
    , Provider(..)
    , TokenProvider(..)
    , getNextToken
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
import qualified Data.Text.Encoding as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock
    ( UTCTime(..)
    , addUTCTime
    , diffUTCTime
    , getCurrentTime
    )
import System.Directory
    ( createDirectory
    , getTemporaryDirectory
    , removeFile
    , removePathForcibly
    )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.IO (hClose, openTempFile)
import Test.Hspec

spec :: Spec
spec = do
    describe "loadAuth" do
        it "returns Text errors for incomplete broker configuration" do
            withEnv "AGENT_BROKER_URL" (Just "http://127.0.0.1:1") $
                withEnv "AGENT_BROKER_TOKEN" Nothing do
                    result <- loadAuth Nothing
                    case result of
                        Left err ->
                            err `shouldBe`
                                "AGENT_BROKER_URL is set; also set AGENT_BROKER_TOKEN"
                        Right _ ->
                            expectationFailure "expected broker configuration failure"

    describe "probeLoadedAuth" do
        it "rejects auth whose accounts are currently cooling down" do
            let retryAt = UTCTime (fromGregorian 2026 8 21) 3600
                exhausted = LoadedAuth
                    { loadedProvider = XAIProvider
                    , loadedTokenProvider = TokenProvider \_ ->
                        pure (Left (CredentialsExhausted retryAt))
                    , loadedOpenAiPool = Nothing
                    }
            result <- probeLoadedAuth exhausted
            case result of
                Left err -> err `shouldBe` CredentialsExhausted retryAt
                Right _ -> expectationFailure "expected exhausted auth"

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
                encoded = Text.decodeUtf8 . LBS.toStrict . Aeson.encode $
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
                (staticCredentialProvider staleGrok)
                (Just
                    (FailedCredential staleGrok
                        (AccountRateLimited (Just 7))))
            case result of
                Left CredentialsExhausted{retryAt} ->
                    diffUTCTime retryAt before `shouldSatisfy` (>= 7)
                other ->
                    expectationFailure
                        ("expected CredentialsExhausted, got " <> show other)

    describe "reloadableFileCredentialProvider" do
        it "returns the cached credential without reloading" do
            loads <- newIORef (0 :: Int)
            provider <- reloadableFileCredentialProvider XAIProvider staleGrok
                (modifyIORef' loads (+ 1) >> pure (Just freshGrok))
            first <- getNextToken provider Nothing
            second <- getNextToken provider Nothing
            first `shouldBe` Right staleGrok
            second `shouldBe` Right staleGrok
            readIORef loads `shouldReturn` 0

        it "reloads after an authentication rejection" do
            loads <- newIORef (0 :: Int)
            provider <- reloadableFileCredentialProvider XAIProvider staleGrok
                (modifyIORef' loads (+ 1) >> pure (Just freshGrok))
            reloaded <- getNextToken provider (Just FailedCredential
                { credential = staleGrok
                , failure = AccountAuthenticationRejected
                })
            reloaded `shouldBe` Right freshGrok
            getNextToken provider Nothing `shouldReturn` Right freshGrok
            readIORef loads `shouldReturn` 1

        it "rejects an unchanged reload after authentication failure" do
            provider <- reloadableFileCredentialProvider XAIProvider staleGrok
                (pure (Just staleGrok))
            result <- getNextToken provider (Just FailedCredential
                { credential = staleGrok
                , failure = AccountAuthenticationRejected
                })
            case result of
                Left (ProviderError AuthenticationError message _) ->
                    Text.unpack message `shouldContain`
                        "reloaded credential is unchanged"
                other -> expectationFailure ("expected AuthenticationError, got " <> show other)

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
        ManagedSubscription ManagedGrokAuthJson True

managedGrokSecretFor :: GrokAuthState -> ManagedSecret
managedGrokSecretFor state =
    ManagedSecret
        "grok-test"
        (Text.decodeUtf8
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

epoch :: UTCTime
epoch = UTCTime (fromGregorian 2026 1 1) 0

withTempHome :: (FilePath -> IO a) -> IO a
withTempHome action =
    bracket create removePathForcibly \home ->
        withEnv "HOME" (Just home) (action home)
  where
    create = do
        temporary <- getTemporaryDirectory
        (path, handle) <- openTempFile temporary "agent-auth"
        hClose handle
        removeFile path
        createDirectory path
        pure path

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
