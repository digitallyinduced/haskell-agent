-- | OpenAI-specific credential providers backed by ChatGPT OAuth state or a
-- static Responses API bearer token.
module Agent.OpenAI.Credential
    ( poolTokenProvider
    , poolTokenProviderWithBilling
    , staticBearerProvider
    ) where

import qualified Agent.OpenAI.Auth as Auth
import Agent.Error (ApiError(..), ErrorType(..))
import Agent.Provider
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (addUTCTime, getCurrentTime)

poolTokenProvider :: Auth.Pool -> IO TokenProvider
poolTokenProvider = poolTokenProviderWithBilling SubscriptionBilled

poolTokenProviderWithBilling :: BillingMode -> Auth.Pool -> IO TokenProvider
poolTokenProviderWithBilling billing pool =
    pure $ tokenProvider billing \failed -> case failed of
            Nothing -> acquireFromPool pool
            Just FailedCredential { credential, failure } ->
                case failure of
                    AccountRateLimited { retryAfterSeconds } -> do
                        Auth.reportRateLimit pool credential.accountId
                            (max 1 <$> retryAfterSeconds)
                        acquireFromPool pool
                    AccountAuthenticationRejected -> do
                        state <- Auth.readAccountState pool credential.accountId
                        if maybe False (Text.null . (.refreshToken)) state
                            then rejectStaticCredential pool credential.accountId
                            else do
                                Auth.recoverAfterAuthFailure
                                    pool
                                    credential.accountId
                                    credential.accessToken >>= \case
                                        Right refreshed ->
                                            pure $ Right $
                                                credentialFromAuthState refreshed
                                        Left _ -> acquireFromPool pool

rejectStaticCredential :: Auth.Pool -> Text -> IO (Either ApiError Credential)
rejectStaticCredential pool accountId = do
    accountIds <- Auth.allAccountIds pool
    Auth.reportAuthBroken pool accountId
    acquireFromPool pool >>= \case
        Left CredentialsExhausted{}
            | accountIds == [accountId] ->
                pure $ Left $ ProviderError AuthenticationError
                    "static bearer token was rejected"
                    Nothing
        result -> pure result

-- | A single OpenAI-compatible API bearer token with no OAuth refresh or
-- account failover.
staticBearerProvider :: Text -> TokenProvider
staticBearerProvider apiKey = tokenProvider ApiBilled \failed -> case failed of
        Nothing ->
            pure $ Right Credential
                { accessToken = apiKey
                , accountId = ""
                , leaseId = Nothing
                , provider = OpenAIProvider
                }
        Just FailedCredential { failure = AccountRateLimited { retryAfterSeconds } } -> do
            now <- getCurrentTime
            let seconds = max 1
                    (fromMaybe staticBearerRateLimitCooldownSeconds retryAfterSeconds)
            pure $ Left $ CredentialsExhausted
                (addUTCTime (fromIntegral seconds) now)
        Just FailedCredential { failure = AccountAuthenticationRejected } ->
            pure $ Left $ ProviderError AuthenticationError
                "static bearer token was rejected"
                Nothing

staticBearerRateLimitCooldownSeconds :: Int
staticBearerRateLimitCooldownSeconds = 60

acquireFromPool :: Auth.Pool -> IO (Either ApiError Credential)
acquireFromPool pool = fmap credentialFromPair <$> Auth.getAccessToken pool

credentialFromPair :: (Text, Text) -> Credential
credentialFromPair (accessToken, accountId) = Credential
    { accessToken
    , accountId
    , leaseId = Nothing
    , provider = OpenAIProvider
    }

credentialFromAuthState :: Auth.AuthState -> Credential
credentialFromAuthState state = Credential
    { accessToken = state.accessToken
    , accountId = state.accountId
    , leaseId = Nothing
    , provider = OpenAIProvider
    }
