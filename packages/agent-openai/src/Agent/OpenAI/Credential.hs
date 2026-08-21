-- | OpenAI-specific credential providers backed by ChatGPT OAuth state or a
-- static Responses API bearer token.
module Agent.OpenAI.Credential
    ( poolTokenProvider
    , staticBearerProvider
    ) where

import qualified Agent.OpenAI.Auth as Auth
import Agent.Error (ApiError(..), ErrorType(..))
import Agent.Provider
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)

poolTokenProvider :: Auth.Pool -> IO TokenProvider
poolTokenProvider pool = do
    authRecoveryAttempts <- newIORef Map.empty
    pure $ TokenProvider \failed ->
        case failed of
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
                                shouldRefresh <- takeAuthRecoverySlot
                                    authRecoveryAttempts
                                    credential.accountId
                                if shouldRefresh
                                    then Auth.refreshAfterAuthFailure pool credential.accountId >>= \case
                                        Right refreshed ->
                                            pure $ Right $
                                                credentialFromAuthState refreshed
                                        Left _ -> acquireFromPool pool
                                    else do
                                        Auth.reportAuthBroken pool credential.accountId
                                        acquireFromPool pool

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

takeAuthRecoverySlot
    :: IORef (Map.Map Text UTCTime)
    -> Text
    -> IO Bool
takeAuthRecoverySlot attempts accountId = do
    now <- getCurrentTime
    atomicModifyIORef' attempts \current ->
        let recent = case Map.lookup accountId current of
                Just attemptedAt -> diffUTCTime now attemptedAt
                    < fromIntegral Auth.authFailureRetrySeconds
                Nothing -> False
            pruned = Map.filter
                (\attemptedAt -> diffUTCTime now attemptedAt
                    < fromIntegral Auth.authFailureRetrySeconds)
                current
        in if recent
            then (pruned, False)
            else (Map.insert accountId now pruned, True)

-- | A single OpenAI-compatible API bearer token with no OAuth refresh or
-- account failover.
staticBearerProvider :: Text -> TokenProvider
staticBearerProvider apiKey = TokenProvider \failed ->
    case failed of
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
