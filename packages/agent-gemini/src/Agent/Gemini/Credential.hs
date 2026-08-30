-- | Static Google AI Studio API-key credentials.
module Agent.Gemini.Credential
    ( staticApiKeyProvider
    , credentialFromApiKey
    , credentialFromEnv
    ) where

import Agent.Error (ApiError(..), ErrorType(..))
import Agent.Provider
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (addUTCTime, getCurrentTime)
import System.Environment (lookupEnv)

-- | A single Gemini API key with no refresh flow.
staticApiKeyProvider :: Text -> TokenProvider
staticApiKeyProvider apiKey = tokenProvider ApiBilled \failed -> case failed of
    Nothing -> pure $ Right (credentialFromApiKey apiKey)
    Just FailedCredential
        { failure = AccountRateLimited { retryAfterSeconds }
        , failureReason
        } -> do
        now <- getCurrentTime
        let seconds = max 1 (fromMaybe 60 retryAfterSeconds)
        pure $ Left $ CredentialsExhausted
            { retryAt = addUTCTime (fromIntegral seconds) now
            , exhaustionReasons = [failureReason]
            }
    Just FailedCredential { failure = AccountAuthenticationRejected } ->
        pure $ Left $ ProviderError AuthenticationError
            "static Gemini API key was rejected"
            Nothing

credentialFromApiKey :: Text -> Credential
credentialFromApiKey apiKey = Credential
    { accessToken = apiKey
    , accountId = "gemini"
    , leaseId = Nothing
    , provider = GeminiProvider
    }

-- | Read the standard Gemini SDK environment variables. When both are set,
-- @GOOGLE_API_KEY@ takes precedence, matching Google's SDKs.
credentialFromEnv :: IO (Maybe Credential)
credentialFromEnv = do
    googleKey <- nonEmpty <$> lookupEnv "GOOGLE_API_KEY"
    geminiKey <- nonEmpty <$> lookupEnv "GEMINI_API_KEY"
    pure $ credentialFromApiKey . Text.pack <$> (googleKey <|> geminiKey)
  where
    nonEmpty = \case
        Just value | not (null value) -> Just value
        _ -> Nothing
