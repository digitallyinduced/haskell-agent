module Agent.OpenAI.CredentialSpec (spec) where

import Agent.OpenAI.Auth (AuthState(..), newPool)
import qualified Agent.OpenAI.Auth as Auth
import Agent.OpenAI.Credential
import Agent.Error
    ( ApiError(..)
    , CredentialExhaustionReason(..)
    , CredentialRefreshFailure(..)
    , ErrorType(..)
    )
import Agent.Provider
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified "base64-bytestring" Data.ByteString.Base64 as B64
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.MVar
    ( newEmptyMVar
    , putMVar
    , takeMVar
    )
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

        it "classifies explicit authentication failures only" do
            accountFailureFromApiError (HttpError 401 "rejected")
                `shouldBe` Just AccountAuthenticationRejected
            accountFailureFromApiError
                (ProviderError AuthenticationError "rejected" Nothing)
                `shouldBe` Just AccountAuthenticationRejected
            accountFailureFromApiError (HttpError 403 "model access denied")
                `shouldBe` Nothing
            accountFailureFromApiError
                (ProviderError PermissionError "model access denied" Nothing)
                `shouldBe` Nothing
            accountFailureFromApiError
                (CredentialError "credential file is invalid")
                `shouldBe` Nothing
            accountFailureFromApiError (ConnectionError "offline")
                `shouldBe` Nothing

    describe "runWithTokenProvider" do
        it "shares account failover orchestration across transports" do
            reportedFailures <- newIORef ([] :: [Maybe FailedCredential])
            let provider = tokenProvider SubscriptionBilled \reported -> do
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
                , Just FailedCredential
                    { credential = credentialFor "acc-a"
                    , failure = AccountRateLimited (Just 90)
                    , failureReason = ExhaustedByRateLimit
                        { exhaustionErrorType = Just UsageLimitReached
                        , exhaustionStatusCode = Nothing
                        , exhaustionRetryAfter = Just 90
                        }
                    }
                ]

        it "does not poison credentials after transport failures" do
            acquisitions <- newIORef (0 :: Int)
            let provider = tokenProvider SubscriptionBilled \_ -> do
                    modifyIORef' acquisitions (+ 1)
                    pure $ Right (credentialFor "acc-a")
            result <- runWithTokenProvider provider \_ ->
                pure (Left (ConnectionError "offline") :: Either ApiError Text)

            result `shouldBe` Left (ConnectionError "offline")
            readIORef acquisitions `shouldReturn` 1

        it "does not rotate accounts after an untyped HTTP 403" do
            acquisitions <- newIORef (0 :: Int)
            let forbidden = HttpError 403 "WebSocket handshake returned HTTP 403"
                provider = tokenProvider SubscriptionBilled \reported -> do
                    reported `shouldBe` Nothing
                    modifyIORef' acquisitions (+ 1)
                    pure $ Right (credentialFor "acc-a")

            result <- runWithTokenProvider provider \_ ->
                pure (Left forbidden :: Either ApiError Text)

            result `shouldBe` Left forbidden
            readIORef acquisitions `shouldReturn` 1

        it "reports an already-failed connection before choosing a replacement" do
            reportedFailures <- newIORef ([] :: [Maybe FailedCredential])
            let exhausted = credentialFor "acc-exhausted"
                initialFailure =
                    failed exhausted (AccountRateLimited (Just 120))
                provider = tokenProvider SubscriptionBilled \reported -> do
                    modifyIORef' reportedFailures (<> [reported])
                    pure $ Right (credentialFor "acc-healthy")

            result <- runWithTokenProviderAfter provider initialFailure
                (pure . Right . (.accountId))

            result `shouldBe` Right "acc-healthy"
            readIORef reportedFailures `shouldReturn` [initialFailure]

    describe "seedTokenProvider" do
        it "uses an acquired replacement exactly once before delegating" do
            acquisitions <- newIORef (0 :: Int)
            let underlying = tokenProvider SubscriptionBilled \_ -> do
                    call <- atomicModifyIORef' acquisitions (\n -> (n + 1, n + 1))
                    pure $ Right (credentialFor ("underlying-" <> showText call))
                replacement = (credentialFor "replacement")
                    { leaseId = Just "replacement-lease" }
            seeded <- seedTokenProvider underlying replacement

            tokenProviderBillingMode seeded `shouldBe` SubscriptionBilled
            first <- getNextToken seeded Nothing
            second <- getNextToken seeded Nothing

            first `shouldBe` Right replacement
            fmap (.accountId) second `shouldBe` Right "underlying-1"
            readIORef acquisitions `shouldReturn` 1

        it "forwards failure feedback without consuming the seed" do
            reported <- newIORef ([] :: [Maybe FailedCredential])
            let underlying = tokenProvider SubscriptionBilled \failure -> do
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

            tokenProviderBillingMode provider `shouldBe` SubscriptionBilled
            result <- getNextToken provider Nothing

            fmap (.accountId) result `shouldBe` Right "acc-a"

        it "marks a limited account and immediately returns another" do
            provider <- localProvider ["acc-a", "acc-b"] (pure . Right)
            first <- expectCredential =<< getNextToken provider Nothing

            second <- expectCredential =<< getNextToken provider
                (Just FailedCredential
                    { credential = first
                    , failure = AccountRateLimited (Just 60)
                    , failureReason = ExhaustedByRateLimit
                        { exhaustionErrorType = Just RateLimitError
                        , exhaustionStatusCode = Just 429
                        , exhaustionRetryAfter = Just 60
                        }
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

        it "reports refresh-source failures separately from provider rejection" do
            let refresh _ =
                    pure $ Left (CredentialError "credential store unavailable")
                rejectionReason = ExhaustedByAuthentication
                    { exhaustionErrorType = Nothing
                    , exhaustionStatusCode = Just 401
                    }
            provider <- localProvider ["acc-a", "acc-b"] refresh
            first <- expectCredential =<< getNextToken provider Nothing
            second <- expectCredential =<< getNextToken provider
                (failedWithReason
                    first AccountAuthenticationRejected rejectionReason)

            exhausted <- getNextToken provider
                (failedWithReason
                    second AccountAuthenticationRejected rejectionReason)

            case exhausted of
                Left CredentialsExhausted{exhaustionReasons} ->
                    exhaustionReasons `shouldBe`
                        [ ExhaustedByCredentialRefresh
                            { refreshFailure =
                                RefreshCredentialSourceFailed
                            , exhaustionErrorType = Nothing
                            , exhaustionStatusCode = Nothing
                            }
                        ]
                other -> expectationFailure
                    ("expected CredentialsExhausted, got " <> show other)

        it "shares auth recovery across token providers for one pool" do
            refreshCalls <- newIORef (0 :: Int)
            refreshStarted <- newEmptyMVar
            releaseRefresh <- newEmptyMVar
            state <- freshAuth "acc-a"
            let rotatedToken = freshToken "rotated"
                refresh current = do
                    modifyIORef' refreshCalls (+ 1)
                    putMVar refreshStarted ()
                    takeMVar releaseRefresh
                    pure $ Right current
                        { Auth.accessToken = rotatedToken }
            pool <- newPool [state] refresh
            firstProvider <- poolTokenProvider pool
            secondProvider <- poolTokenProvider pool
            initial <- expectCredential
                =<< getNextToken firstProvider Nothing
            let rejection =
                    failed initial AccountAuthenticationRejected

            withAsync
                (getNextToken firstProvider rejection)
                \firstRecovery -> do
                    takeMVar refreshStarted
                    withAsync
                        (getNextToken secondProvider rejection)
                        \secondRecovery -> do
                            putMVar releaseRefresh ()
                            first <- expectCredential
                                =<< wait firstRecovery
                            second <- expectCredential
                                =<< wait secondRecovery
                            first.accessToken `shouldBe` rotatedToken
                            second.accessToken `shouldBe` rotatedToken

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
            let rejectionReason = ExhaustedByAuthentication
                    { exhaustionErrorType = Nothing
                    , exhaustionStatusCode = Just 401
                    }
            rotated <- expectCredential =<< getNextToken provider
                (failedWithReason
                    initial AccountAuthenticationRejected rejectionReason)

            exhausted <- getNextToken provider
                (failedWithReason
                    rotated AccountAuthenticationRejected rejectionReason)

            case exhausted of
                Left CredentialsExhausted{exhaustionReasons} ->
                    exhaustionReasons `shouldBe` [rejectionReason]
                other -> expectationFailure ("expected CredentialsExhausted, got " <> show other)
            readIORef refreshCalls `shouldReturn` 1

    describe "pooled static bearer credentials" do
        it "report rejection as an authentication error" staticPoolAuthenticationTest
        it "preserves another account's cooldown" staticPoolCooldownTest

    describe "staticBearerProvider" do
        it "returns the same bearer without an account id" do
            let provider = staticBearerProvider "router-key"
            tokenProviderBillingMode provider `shouldBe` ApiBilled
            result <- getNextToken provider Nothing

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

staticPoolAuthenticationTest :: IO ()
staticPoolAuthenticationTest = do
    state <- freshAuth "acc-static"
    pool <- newPool
        [ state
            { accessToken = "static-token"
            , refreshToken = ""
            }
        ]
        unexpectedRefresh
    provider <- poolTokenProvider pool
    initial <- expectCredential =<< getNextToken provider Nothing
    rejected <- getNextToken provider
        (failed initial AccountAuthenticationRejected)
    case rejected of
        Left (ProviderError AuthenticationError _ _) -> pure ()
        other -> expectationFailure
            ("expected AuthenticationError, got " <> show other)
  where
    unexpectedRefresh _ =
        expectationFailure "static token must not refresh"
            >> pure (Left (ConnectionError "unexpected refresh"))

staticPoolCooldownTest :: IO ()
staticPoolCooldownTest = do
    staticState <- freshAuth "acc-static"
    oauthState <- freshAuth "acc-oauth"
    pool <- newPool
        [ staticState { accessToken = "static-token", refreshToken = "" }
        , oauthState
        ]
        (pure . Right)
    Auth.reportRateLimit pool "acc-oauth" (Just 90)
    provider <- poolTokenProvider pool
    initial <- expectCredential =<< getNextToken provider Nothing
    initial.accountId `shouldBe` "acc-static"
    rejected <- getNextToken provider
        (failed initial AccountAuthenticationRejected)
    case rejected of
        Left CredentialsExhausted{} -> pure ()
        other -> expectationFailure
            ("expected CredentialsExhausted, got " <> show other)

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
failed credential failure = Just FailedCredential
    { credential
    , failure
    , failureReason = case failure of
        AccountRateLimited{retryAfterSeconds} ->
            ExhaustedByRateLimit
                { exhaustionErrorType = Just RateLimitError
                , exhaustionStatusCode = Nothing
                , exhaustionRetryAfter = retryAfterSeconds
                }
        AccountAuthenticationRejected ->
            ExhaustedByAuthentication
                { exhaustionErrorType = Just AuthenticationError
                , exhaustionStatusCode = Nothing
                }
    }

failedWithReason
    :: Credential
    -> AccountFailure
    -> CredentialExhaustionReason
    -> Maybe FailedCredential
failedWithReason credential failure failureReason =
    Just FailedCredential { credential, failure, failureReason }

credentialFor :: Text -> Credential
credentialFor accountId = Credential
    { accessToken = "token-" <> accountId
    , accountId
    , leaseId = Just ("lease-" <> accountId)
    , provider = OpenAIProvider
    }

showText :: Show a => a -> Text
showText = Text.pack . show
