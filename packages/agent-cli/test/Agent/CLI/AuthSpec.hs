module Agent.CLI.AuthSpec (spec) where

import Agent.CLI.Auth
import Agent.CLI.CredentialStore
import Agent.Error (ApiError(..), ErrorType(..))
import Agent.OsPath (OsPath, fromFilePath, toFilePath)
import Agent.OpenAI.Auth (AuthState(..))
import qualified Agent.OpenAI.Auth as OpenAI
import Agent.Provider
    ( AccountFailure(..)
    , Credential(..)
    , FailedCredential(..)
    , Provider(..)
    , TokenProvider(..)
    , getNextToken
    )
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
import Data.Time.Clock (UTCTime(..))
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

    describe "OpenAI account pools" do
        it "loads every enabled managed account into one pool" $
            withTempHome openAiManagedPoolTest
        it "combines distinct managed and ~/.codex accounts" $
            withTempHome openAiManagedAndFilePoolTest
        it "deduplicates duplicate accounts with managed precedence" $
            withTempHome openAiDeduplicationTest
        it "does not let a disabled managed source shadow ~/.codex" $
            withTempHome openAiDisabledSourceTest

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

openAiManagedPoolTest :: OsPath -> IO ()
openAiManagedPoolTest _ =
    withCleanOpenAiEnv do
        storeOpenAiAccount "managed-a" "acc-a" True "token-a"
        storeOpenAiAccount "managed-b" "acc-b" True "token-b"
        storeOpenAiAccount "managed-disabled" "acc-disabled" False "token-disabled"
        loaded <- loadAuth (Just OpenAIProvider)
        pool <- expectOpenAiPool loaded
        OpenAI.allAccountIds pool `shouldReturn` ["acc-b", "acc-a"]

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

expectOpenAiPool :: Either Text LoadedAuth -> IO OpenAI.Pool
expectOpenAiPool = \case
    Left err -> expectationFailure (Text.unpack err) >> fail "missing pool"
    Right loaded -> case loaded.loadedOpenAiPool of
        Nothing -> expectationFailure "expected OpenAI account pool"
            >> fail "missing pool"
        Just pool -> pure pool

storeOpenAiAccount :: Text -> Text -> Bool -> Text -> IO ()
storeOpenAiAccount credentialId accountId enabled token =
    upsertManagedCredential
        (openAiMetadata credentialId accountId enabled)
        (ManagedSecret credentialId (authJson accountId token))
        `shouldReturn` Right ()

openAiMetadata :: Text -> Text -> Bool -> ManagedCredential
openAiMetadata credentialId accountId enabled = ManagedCredential
    { managedId = credentialId
    , managedProvider = OpenAIProvider
    , managedAccountId = accountId
    , managedLabel = "ChatGPT"
    , managedBilling = ManagedSubscription
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

testAuthState :: Text -> Text -> AuthState
testAuthState access refresh =
    AuthState access refresh "acc-test" Nothing epoch

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
        [ "AGENT_BROKER_URL"
        , "AGENT_BROKER_TOKEN"
        , "CODEX_ACCESS_TOKEN"
        , "CODEX_AUTH_JSON"
        , "CODEX_ACCOUNT_ID"
        , "CODEX_ID_TOKEN"
        , "OPENAI_OAUTH_CLIENT_ID"
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
