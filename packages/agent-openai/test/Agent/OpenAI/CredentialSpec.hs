module Agent.OpenAI.CredentialSpec (spec) where

import Agent.OpenAI.Auth (AuthState(..), newPool)
import qualified Agent.OpenAI.Auth as Auth
import Agent.OpenAI.Credential
import Agent.Broker
    ( newBrokerTokenProviderWith
    , newBrokerTokenProviderWithClock
    )
import Agent.Error (ApiError(..), ErrorType(..))
import Agent.Provider
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified "base64-bytestring" Data.ByteString.Base64 as B64
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Test.Hspec

spec :: Spec
spec = do
    describe "Credential" do
        it "redacts bearer tokens from Show output" do
            let credential = Credential
                    { accessToken = "super-secret-bearer-token"
                    , accountId = "acc-a"
                    , leaseId = Just "secret-lease-id"
                    , provider = OpenAIProvider
                    }

            show credential `shouldContain` "acc-a"
            show credential `shouldNotContain` "super-secret-bearer-token"
            show credential `shouldNotContain` "secret-lease-id"

    describe "accountFailureFromApiError" do
        it "classifies REST and structured usage-limit failures" do
            accountFailureFromApiError (HttpError 429 "limited")
                `shouldBe` Just (AccountRateLimited Nothing)
            accountFailureFromApiError
                (ProviderError UsageLimitReached "limited" (Just 120))
                `shouldBe` Just (AccountRateLimited (Just 120))
            accountFailureFromApiError
                (ProviderError UsageBalanceExhausted "balance exhausted" (Just 3600))
                `shouldBe` Just (AccountRateLimited (Just 3600))

        it "classifies authentication failures but not connection errors" do
            accountFailureFromApiError (HttpError 401 "rejected")
                `shouldBe` Just AccountAuthenticationRejected
            accountFailureFromApiError (ConnectionError "offline")
                `shouldBe` Nothing

    describe "runWithTokenProvider" do
        it "shares account failover orchestration across transports" do
            reportedFailures <- newIORef ([] :: [Maybe FailedCredential])
            let provider = TokenProvider \reported -> do
                    modifyIORef' reportedFailures (<> [reported])
                    pure $ Right $ case reported of
                        Nothing -> credentialFor "acc-a"
                        Just _ -> credentialFor "acc-b"
            result <- runWithTokenProvider provider \credential ->
                if credential.accountId == "acc-a"
                    then pure $ Left $ ProviderError UsageLimitReached "limited" (Just 90)
                    else pure $ Right credential.accountId

            result `shouldBe` Right "acc-b"
            readIORef reportedFailures `shouldReturn`
                [ Nothing
                , failed (credentialFor "acc-a") (AccountRateLimited (Just 90))
                ]

        it "does not poison credentials after transport failures" do
            acquisitions <- newIORef (0 :: Int)
            let provider = TokenProvider \_ -> do
                    modifyIORef' acquisitions (+ 1)
                    pure $ Right (credentialFor "acc-a")
            result <- runWithTokenProvider provider \_ ->
                pure (Left (ConnectionError "offline") :: Either ApiError Text)

            result `shouldBe` Left (ConnectionError "offline")
            readIORef acquisitions `shouldReturn` 1

    describe "seedTokenProvider" do
        it "uses an acquired replacement exactly once before delegating" do
            acquisitions <- newIORef (0 :: Int)
            let underlying = TokenProvider \_ -> do
                    call <- atomicModifyIORef' acquisitions (\n -> (n + 1, n + 1))
                    pure $ Right (credentialFor ("underlying-" <> showText call))
                replacement = (credentialFor "replacement")
                    { leaseId = Just "replacement-lease" }
            seeded <- seedTokenProvider underlying replacement

            first <- getNextToken seeded Nothing
            second <- getNextToken seeded Nothing

            first `shouldBe` Right replacement
            fmap (.accountId) second `shouldBe` Right "underlying-1"
            readIORef acquisitions `shouldReturn` 1

        it "forwards failure feedback without consuming the seed" do
            reported <- newIORef ([] :: [Maybe FailedCredential])
            let underlying = TokenProvider \failure -> do
                    modifyIORef' reported (<> [failure])
                    pure $ Right (credentialFor "after-feedback")
                replacement = (credentialFor "replacement")
                    { leaseId = Just "replacement-lease" }
                rejected = failed replacement AccountAuthenticationRejected
            seeded <- seedTokenProvider underlying replacement

            feedbackResult <- getNextToken seeded rejected
            firstOrdinary <- getNextToken seeded Nothing

            fmap (.accountId) feedbackResult `shouldBe` Right "after-feedback"
            firstOrdinary `shouldBe` Right replacement
            readIORef reported `shouldReturn` [rejected]

    describe "poolTokenProvider" do
        it "acquires a credential without exposing local pool details" do
            provider <- localProvider ["acc-a"] (pure . Right)

            result <- getNextToken provider Nothing

            fmap (.accountId) result `shouldBe` Right "acc-a"

        it "marks a limited account and immediately returns another" do
            provider <- localProvider ["acc-a", "acc-b"] (pure . Right)
            first <- expectCredential =<< getNextToken provider Nothing

            second <- expectCredential =<< getNextToken provider
                (Just FailedCredential
                    { credential = first
                    , failure = AccountRateLimited (Just 60)
                    })

            second.accountId `shouldNotBe` first.accountId

        it "returns CredentialsExhausted after every local account failed" do
            provider <- localProvider ["acc-a", "acc-b"] (pure . Right)
            first <- expectCredential =<< getNextToken provider Nothing
            second <- expectCredential =<< getNextToken provider
                (failed first (AccountRateLimited (Just 60)))

            exhausted <- getNextToken provider
                (failed second (AccountRateLimited (Just 60)))

            case exhausted of
                Left CredentialsExhausted{} -> pure ()
                other -> expectationFailure ("expected CredentialsExhausted, got " <> show other)

        it "force-refreshes a rejected local credential once" do
            refreshCalls <- newIORef (0 :: Int)
            let refresh :: AuthState -> IO (Either ApiError AuthState)
                refresh state = do
                    modifyIORef' refreshCalls (+ 1)
                    pure $ Right state { Auth.accessToken = freshToken "rotated" }
            provider <- localProvider ["acc-a"] refresh
            initial <- expectCredential =<< getNextToken provider Nothing

            rotated <- expectCredential =<< getNextToken provider
                (failed initial AccountAuthenticationRejected)

            rotated.accessToken `shouldNotBe` initial.accessToken
            readIORef refreshCalls `shouldReturn` 1

        it "does not repeatedly refresh a persistently rejected account" do
            refreshCalls <- newIORef (0 :: Int)
            let refresh :: AuthState -> IO (Either ApiError AuthState)
                refresh state = do
                    call <- atomicModifyIORef' refreshCalls (\n -> (n + 1, n + 1))
                    pure $ Right state
                        { Auth.accessToken = freshToken ("rotated-" <> showText call) }
            provider <- localProvider ["acc-a"] refresh
            initial <- expectCredential =<< getNextToken provider Nothing
            rotated <- expectCredential =<< getNextToken provider
                (failed initial AccountAuthenticationRejected)

            exhausted <- getNextToken provider
                (failed rotated AccountAuthenticationRejected)

            case exhausted of
                Left CredentialsExhausted{} -> pure ()
                other -> expectationFailure ("expected CredentialsExhausted, got " <> show other)
            readIORef refreshCalls `shouldReturn` 1

    describe "staticBearerProvider" do
        it "returns the same bearer without an account id" do
            result <- getNextToken (staticBearerProvider "router-key") Nothing

            case result of
                Right credential -> do
                    credential.accessToken `shouldBe` "router-key"
                    credential.accountId `shouldBe` ""
                    credential.leaseId `shouldBe` Nothing
                    credential.provider `shouldBe` OpenAIProvider
                Left err -> expectationFailure ("expected credential, got " <> show err)

        it "surfaces rate limits as CredentialsExhausted instead of cycling the same key" do
            now <- getCurrentTime
            let provider = staticBearerProvider "router-key"
            first <- expectCredential =<< getNextToken provider Nothing

            exhausted <- getNextToken provider
                (failed first (AccountRateLimited (Just 90)))

            case exhausted of
                Left CredentialsExhausted { retryAt } ->
                    retryAt `shouldSatisfy` (> addUTCTime 80 now)
                other -> expectationFailure ("expected CredentialsExhausted, got " <> show other)

        it "does not treat a rejected static key as recoverable" do
            let provider = staticBearerProvider "router-key"
            first <- expectCredential =<< getNextToken provider Nothing

            rejected <- getNextToken provider
                (failed first AccountAuthenticationRejected)

            case rejected of
                Left (ProviderError AuthenticationError _ _) -> pure ()
                other -> expectationFailure
                    ("expected AuthenticationError, got " <> show other)

    describe "broker TokenProvider" do
        it "passes structured failure feedback and exclusions while returning a replacement" do
            fetchCalls <- newIORef ([] :: [(Maybe FailedCredential, [Text])])
            let available = [credentialFor "acc-a", credentialFor "acc-b"]
                fetch reportedFailure excluded = do
                    modifyIORef' fetchCalls (<> [(reportedFailure, excluded)])
                    pure $ Right $ firstAvailable excluded available
            provider <- newBrokerTokenProviderWith fetch

            first <- expectCredential =<< getNextToken provider Nothing
            let reportedFailure = failed first (AccountRateLimited (Just 60))
            second <- expectCredential =<< getNextToken provider
                reportedFailure

            first.accountId `shouldBe` "acc-a"
            second.accountId `shouldBe` "acc-b"
            readIORef fetchCalls `shouldReturn`
                [ (Nothing, [])
                , (reportedFailure, ["acc-a"])
                ]

        it "retains broker cooldowns across independent acquisitions" do
            let available = [credentialFor "acc-a", credentialFor "acc-b"]
                fetch _failed excluded = pure $ Right $ firstAvailable excluded available
            provider <- newBrokerTokenProviderWith fetch
            first <- expectCredential =<< getNextToken provider Nothing
            _ <- expectCredential =<< getNextToken provider
                (failed first (AccountRateLimited (Just 60)))

            next <- expectCredential =<< getNextToken provider Nothing

            next.accountId `shouldBe` "acc-b"

        it "returns CredentialsExhausted when every broker credential is cooling down" do
            let available = [credentialFor "acc-a", credentialFor "acc-b"]
                fetch _failed excluded = pure $ Right $ firstAvailable excluded available
            provider <- newBrokerTokenProviderWith fetch
            first <- expectCredential =<< getNextToken provider Nothing
            second <- expectCredential =<< getNextToken provider
                (failed first (AccountRateLimited (Just 60)))

            exhausted <- getNextToken provider
                (failed second (AccountRateLimited (Just 60)))

            case exhausted of
                Left CredentialsExhausted{} -> pure ()
                other -> expectationFailure ("expected CredentialsExhausted, got " <> show other)

        it "re-probes broker recovery without losing the upstream reset time" do
            initialNow <- getCurrentTime
            nowRef <- newIORef initialNow
            let available = [credentialFor "acc-a"]
                fetch _failed excluded = pure $ Right $ firstAvailable excluded available
                upstreamRetrySeconds = 7 * 24 * 60 * 60
                upstreamRetryAt = addUTCTime (fromIntegral upstreamRetrySeconds) initialNow
            provider <- newBrokerTokenProviderWithClock (readIORef nowRef) fetch
            first <- expectCredential =<< getNextToken provider Nothing

            exhausted <- getNextToken provider
                (failed first (AccountRateLimited (Just upstreamRetrySeconds)))
            case exhausted of
                Left CredentialsExhausted { retryAt } -> retryAt `shouldBe` upstreamRetryAt
                other -> expectationFailure ("expected CredentialsExhausted, got " <> show other)

            writeIORef nowRef (addUTCTime 61 initialNow)
            recovered <- expectCredential =<< getNextToken provider Nothing

            recovered.accountId `shouldBe` "acc-a"

localProvider
    :: [Text]
    -> (AuthState -> IO (Either ApiError AuthState))
    -> IO TokenProvider
localProvider accountIds refresh = do
    states <- traverse freshAuth accountIds
    pool <- newPool states refresh
    poolTokenProvider pool

freshAuth :: Text -> IO AuthState
freshAuth accountId = do
    now <- getCurrentTime
    pure AuthState
        { accessToken = freshToken accountId
        , refreshToken = "refresh-" <> accountId
        , accountId
        , idToken = Nothing
        , lastRefresh = now
        }

freshToken :: Text -> Text
freshToken suffix =
    let payload = Aeson.encode (Aeson.object ["exp" .= (4_102_444_800 :: Int)])
        encoded = TextEncoding.decodeUtf8
            $ BS.filter (/= 0x3D)
            $ BS.map urlSafeByte
            $ B64.encode
            $ LBS.toStrict payload
    in "e30." <> encoded <> "." <> suffix
  where
    urlSafeByte 0x2B = 0x2D
    urlSafeByte 0x2F = 0x5F
    urlSafeByte byte = byte

expectCredential :: Either ApiError Credential -> IO Credential
expectCredential = \case
    Left err -> expectationFailure ("expected credential, got " <> show err) >> fail "missing credential"
    Right credential -> pure credential

failed :: Credential -> AccountFailure -> Maybe FailedCredential
failed credential failure = Just FailedCredential { credential, failure }

credentialFor :: Text -> Credential
credentialFor accountId = Credential
    { accessToken = "token-" <> accountId
    , accountId
    , leaseId = Just ("lease-" <> accountId)
    , provider = OpenAIProvider
    }

firstAvailable :: [Text] -> [Credential] -> Maybe Credential
firstAvailable excluded = go
  where
    go [] = Nothing
    go (credential : rest)
        | credential.accountId `elem` excluded = go rest
        | otherwise = Just credential

showText :: Show a => a -> Text
showText = Text.pack . show
