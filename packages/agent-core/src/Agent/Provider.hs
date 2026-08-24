-- | Provider-neutral credential acquisition and account failover.
module Agent.Provider
    ( BillingMode(..)
    , TokenProvider
    , tokenProviderBillingMode
    , tokenProvider
    , Credential(..)
    , Provider(..)
    , providerSlug
    , parseProvider
    , FailedCredential(..)
    , AccountFailure(..)
    , getNextToken
    , runWithTokenProvider
    , runWithTokenProviderAfter
    , seedTokenProvider
    , accountFailureFromApiError
    , accountFailureReason
    ) where

import Agent.Error
    ( ApiError(..)
    , CredentialExhaustionReason(..)
    , ErrorType(..)
    , apiErrorRetryAfter
    , credentialExhaustionReasonFromApiError
    )
import qualified Data.Aeson as Aeson
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)

data Provider
    = OpenAIProvider
    | XAIProvider
    | OpenRouterProvider
    | ClaudeCodeProvider
    deriving (Eq, Show)

providerSlug :: Provider -> Text
providerSlug = \case
    OpenAIProvider -> "openai"
    XAIProvider -> "xai"
    OpenRouterProvider -> "openrouter"
    ClaudeCodeProvider -> "claude-code"

parseProvider :: Text -> Maybe Provider
parseProvider = \case
    "openai" -> Just OpenAIProvider
    "xai" -> Just XAIProvider
    "openrouter" -> Just OpenRouterProvider
    "claude-code" -> Just ClaudeCodeProvider
    "claude" -> Just ClaudeCodeProvider
    _ -> Nothing

data Credential = Credential
    { accessToken :: !Text
    , accountId :: !Text
    , leaseId :: !(Maybe Text)
    , provider :: !Provider
    }
    deriving (Eq)

-- | How requests made with a credential source are billed.
--
-- A 'TokenProvider' must only issue credentials with its declared mode. This
-- lets callers enforce spending boundaries without knowing provider-specific
-- authentication details.
data BillingMode
    = SubscriptionBilled
    | ApiBilled
    deriving (Eq, Show)

instance Aeson.ToJSON BillingMode where
    toJSON = Aeson.String . \case
        SubscriptionBilled -> "subscription"
        ApiBilled -> "api_credits"

instance Aeson.FromJSON BillingMode where
    parseJSON = Aeson.withText "BillingMode" \case
        "subscription" -> pure SubscriptionBilled
        "api_credits" -> pure ApiBilled
        other -> fail ("unknown billing mode: " <> show other)

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
    , failureReason :: !CredentialExhaustionReason
    }
    deriving (Eq, Show)

data TokenProvider = TokenProvider
    { providerBillingMode :: !BillingMode
    , runGetNextToken
        :: Maybe FailedCredential
        -> IO (Either ApiError Credential)
    }

tokenProvider
    :: BillingMode
    -> (Maybe FailedCredential -> IO (Either ApiError Credential))
    -> TokenProvider
tokenProvider = TokenProvider

tokenProviderBillingMode :: TokenProvider -> BillingMode
tokenProviderBillingMode TokenProvider{providerBillingMode} =
    providerBillingMode

getNextToken
    :: TokenProvider
    -> Maybe FailedCredential
    -> IO (Either ApiError Credential)
getNextToken provider = provider.runGetNextToken

seedTokenProvider :: TokenProvider -> Credential -> IO TokenProvider
seedTokenProvider provider credential = do
    seed <- newIORef (Just credential)
    pure TokenProvider
        { providerBillingMode = tokenProviderBillingMode provider
        , runGetNextToken = \failed -> case failed of
            Just reportedFailure -> getNextToken provider (Just reportedFailure)
            Nothing -> atomicModifyIORef'
                seed (\current -> (Nothing, current)) >>= \case
                    Just firstCredential -> pure (Right firstCredential)
                    Nothing -> getNextToken provider Nothing
        }

runWithTokenProvider
    :: TokenProvider
    -> (Credential -> IO (Either ApiError a))
    -> IO (Either ApiError a)
runWithTokenProvider provider =
    runWithTokenProviderAfter provider Nothing

-- | Run an action after reporting a credential that already failed outside
-- this invocation. This is used when a long-lived transport (for example a
-- resumed session WebSocket) encounters an in-band account failure: the
-- replacement checkout must cool down that exact account before selecting
-- another credential.
runWithTokenProviderAfter
    :: TokenProvider
    -> Maybe FailedCredential
    -> (Credential -> IO (Either ApiError a))
    -> IO (Either ApiError a)
runWithTokenProviderAfter provider initialFailure action =
    go maxProviderFailoverAttempts initialFailure
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
                            , failureReason =
                                accountFailureReason err failure
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
    CredentialError{} -> authenticationRejected
    _ -> Nothing
  where
    rateLimited = Just $ AccountRateLimited (apiErrorRetryAfter err)
    authenticationRejected = Just AccountAuthenticationRejected

accountFailureReason
    :: ApiError
    -> AccountFailure
    -> CredentialExhaustionReason
accountFailureReason err failure =
    fromMaybe (fallback failure)
        (credentialExhaustionReasonFromApiError err)
  where
    fallback = \case
        AccountRateLimited{retryAfterSeconds} ->
            ExhaustedByRateLimit
                { exhaustionErrorType = Nothing
                , exhaustionStatusCode = Nothing
                , exhaustionRetryAfter = retryAfterSeconds
                }
        AccountAuthenticationRejected ->
            ExhaustedByAuthentication
                { exhaustionErrorType = Nothing
                , exhaustionStatusCode = Nothing
                }
