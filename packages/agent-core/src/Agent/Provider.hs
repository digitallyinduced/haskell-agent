-- | Provider-neutral credential acquisition and account failover.
module Agent.Provider
    ( TokenProvider(..)
    , Credential(..)
    , Provider(..)
    , providerSlug
    , parseProvider
    , FailedCredential(..)
    , AccountFailure(..)
    , getNextToken
    , runWithTokenProvider
    , seedTokenProvider
    , accountFailureFromApiError
    ) where

import Agent.Error
    ( ApiError(..)
    , ErrorType(..)
    , apiErrorRetryAfter
    )
import Data.IORef
import Data.Text (Text)

data Provider = OpenAIProvider | XAIProvider | OpenRouterProvider
    deriving (Eq, Show)

providerSlug :: Provider -> Text
providerSlug = \case
    OpenAIProvider -> "openai"
    XAIProvider -> "xai"
    OpenRouterProvider -> "openrouter"

parseProvider :: Text -> Maybe Provider
parseProvider = \case
    "openai" -> Just OpenAIProvider
    "xai" -> Just XAIProvider
    "openrouter" -> Just OpenRouterProvider
    _ -> Nothing

data Credential = Credential
    { accessToken :: !Text
    , accountId :: !Text
    , leaseId :: !(Maybe Text)
    , provider :: !Provider
    }
    deriving (Eq)

instance Show Credential where
    show credential = "Credential { accountId = "
        <> show credential.accountId
        <> ", provider = " <> show credential.provider
        <> ", leaseId = " <> case credential.leaseId of
            Nothing -> "Nothing }"
            Just _ -> "Just <redacted> }"

data AccountFailure
    = AccountRateLimited
        { retryAfterSeconds :: !(Maybe Int)
        }
    | AccountAuthenticationRejected
    deriving (Eq, Show)

data FailedCredential = FailedCredential
    { credential :: !Credential
    , failure :: !AccountFailure
    }
    deriving (Eq, Show)

newtype TokenProvider = TokenProvider
    { runGetNextToken
        :: Maybe FailedCredential
        -> IO (Either ApiError Credential)
    }

getNextToken
    :: TokenProvider
    -> Maybe FailedCredential
    -> IO (Either ApiError Credential)
getNextToken provider = provider.runGetNextToken

seedTokenProvider :: TokenProvider -> Credential -> IO TokenProvider
seedTokenProvider provider credential = do
    seed <- newIORef (Just credential)
    pure $ TokenProvider \failed -> case failed of
        Just reportedFailure -> getNextToken provider (Just reportedFailure)
        Nothing -> atomicModifyIORef' seed (\current -> (Nothing, current)) >>= \case
            Just firstCredential -> pure (Right firstCredential)
            Nothing -> getNextToken provider Nothing

runWithTokenProvider
    :: TokenProvider
    -> (Credential -> IO (Either ApiError a))
    -> IO (Either ApiError a)
runWithTokenProvider provider action =
    go maxProviderFailoverAttempts Nothing
  where
    go attemptsLeft failed
        | attemptsLeft <= 0 = pure $ Left $ ConnectionError
            "token provider failover budget exhausted"
        | otherwise = getNextToken provider failed >>= \case
            Left err -> pure (Left err)
            Right credential -> action credential >>= \case
                Left err
                    | Just failure <- accountFailureFromApiError err ->
                        go (attemptsLeft - 1) $ Just FailedCredential
                            { credential
                            , failure
                            }
                result -> pure result

maxProviderFailoverAttempts :: Int
maxProviderFailoverAttempts = 64

accountFailureFromApiError :: ApiError -> Maybe AccountFailure
accountFailureFromApiError err = case err of
    HttpError 429 _ -> rateLimited
    ProviderError RateLimitError _ _ -> rateLimited
    ProviderError UsageLimitReached _ _ -> rateLimited
    ProviderError UsageBalanceExhausted _ _ -> rateLimited
    HttpError 401 _ -> authenticationRejected
    HttpError 403 _ -> authenticationRejected
    ProviderError AuthenticationError _ _ -> authenticationRejected
    _ -> Nothing
  where
    rateLimited = Just $ AccountRateLimited (apiErrorRetryAfter err)
    authenticationRejected = Just AccountAuthenticationRejected
