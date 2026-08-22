-- | Static OpenRouter API-key credentials.
module Agent.OpenRouter.Credential
    ( staticApiKeyProvider
    , credentialFromApiKey
    , credentialFromEnv
    ) where

import Agent.Error (ApiError(..), ErrorType(..))
import Agent.Provider
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (addUTCTime, getCurrentTime)
import System.Environment (lookupEnv)

-- | A single OpenRouter API key with no OAuth refresh or account failover.
staticApiKeyProvider :: Text -> TokenProvider
staticApiKeyProvider apiKey = tokenProvider ApiBilled \failed -> case failed of
        Nothing ->
            pure $ Right (credentialFromApiKey apiKey)
        Just FailedCredential { failure = AccountRateLimited { retryAfterSeconds } } -> do
            now <- getCurrentTime
            let seconds = max 1
                    (fromMaybe staticApiKeyRateLimitCooldownSeconds retryAfterSeconds)
            pure $ Left $ CredentialsExhausted
                (addUTCTime (fromIntegral seconds) now)
        Just FailedCredential { failure = AccountAuthenticationRejected } ->
            pure $ Left $ ProviderError AuthenticationError
                "static OpenRouter API key was rejected"
                Nothing

staticApiKeyRateLimitCooldownSeconds :: Int
staticApiKeyRateLimitCooldownSeconds = 60

credentialFromApiKey :: Text -> Credential
credentialFromApiKey apiKey = Credential
    { accessToken = apiKey
    , accountId = ""
    , leaseId = Nothing
    , provider = OpenRouterProvider
    }

-- | Read @OPENROUTER_API_KEY@. Empty or unset yields 'Nothing'.
credentialFromEnv :: IO (Maybe Credential)
credentialFromEnv = do
    key <- lookupEnv "OPENROUTER_API_KEY"
    pure $ case key of
        Just value | not (null value) -> Just (credentialFromApiKey (Text.pack value))
        _ -> Nothing
