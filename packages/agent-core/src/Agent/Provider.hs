-- | Provider-neutral credential acquisition and account failover.
module Agent.Provider
    ( BillingMode(..)
    , billingModeDecoder
    , TokenProvider
    , tokenProviderBillingMode
    , tokenProvider
    , tokenProviderWithNextToken
    , withAccountFailureClassifier
    , Credential(..)
    , Provider(..)
    , providerSlug
    , parseProvider
    , FailedCredential(..)
    , AccountFailure(..)
    , getNextToken
    , runWithTokenProvider
    , runWithTokenProviderAfter
    , ReplaySafety(..)
    , ProviderAttemptFailure(..)
    , runWithTokenProviderAttempt
    , runWithTokenProviderStreaming
    , seedTokenProvider
    , accountFailureFromApiError
    , accountFailureReason
    , credentialsExhaustedForRateLimit
    ) where

import Agent.Error
    ( ApiError(..)
    , CredentialExhaustionReason(..)
    , ErrorType(..)
    , apiErrorRetryAfter
    , credentialExhaustionReasonFromApiError
    )
import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Bifunctor (first)
import qualified Agent.Json.Decode as Json
import qualified Data.Aeson as Aeson
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time.Clock (addUTCTime, getCurrentTime)

data Provider
    = OpenAIProvider
    | XAIProvider
    | OpenRouterProvider
    | GeminiProvider
    | ClaudeCodeProvider
    deriving (Eq, Ord, Show)

providerSlug :: Provider -> Text
providerSlug = \case
    OpenAIProvider -> "openai"
    XAIProvider -> "xai"
    OpenRouterProvider -> "openrouter"
    GeminiProvider -> "gemini"
    ClaudeCodeProvider -> "claude-code"

parseProvider :: Text -> Maybe Provider
parseProvider = \case
    "openai" -> Just OpenAIProvider
    "xai" -> Just XAIProvider
    "openrouter" -> Just OpenRouterProvider
    "gemini" -> Just GeminiProvider
    "google" -> Just GeminiProvider
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

billingModeDecoder :: Json.Decoder BillingMode
billingModeDecoder = Json.withText \case
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
    , runClassifyAccountFailure
        :: Credential
        -> ApiError
        -> Maybe AccountFailure
    }

tokenProvider
    :: BillingMode
    -> (Maybe FailedCredential -> IO (Either ApiError Credential))
    -> TokenProvider
tokenProvider providerBillingMode runGetNextToken = TokenProvider
    { providerBillingMode
    , runGetNextToken
    , runClassifyAccountFailure =
        \_credential -> accountFailureFromApiError
    }

-- | Replace credential acquisition while retaining the provider's billing and
-- account-failure policy. Provider decorators should use this instead of
-- constructing a fresh 'TokenProvider'.
tokenProviderWithNextToken
    :: TokenProvider
    -> (Maybe FailedCredential -> IO (Either ApiError Credential))
    -> TokenProvider
tokenProviderWithNextToken provider runGetNextToken =
    provider { runGetNextToken }

-- | Add credential-source-specific failure handling while preserving the
-- provider-neutral defaults. Classifiers added here may cause the protected
-- action to be replayed, so they must only recognize failures that happened
-- before the action produced externally visible effects.
withAccountFailureClassifier
    :: (Credential -> ApiError -> Maybe AccountFailure)
    -> TokenProvider
    -> TokenProvider
withAccountFailureClassifier classifier provider =
    provider
        { runClassifyAccountFailure = \credential err ->
            classifier credential err
                <|> provider.runClassifyAccountFailure credential err
        }

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
    pure $
        tokenProviderWithNextToken provider \failed ->
            case failed of
                Just reportedFailure ->
                    getNextToken provider (Just reportedFailure)
                Nothing -> atomicModifyIORef'
                    seed (\current -> (Nothing, current)) >>= \case
                        Just firstCredential -> pure (Right firstCredential)
                        Nothing -> getNextToken provider Nothing

-- | Compatibility entry point for actions whose account failures are known
-- to be replay-safe. Streaming actions should use
-- 'runWithTokenProviderStreaming'; actions with uncertain effects should
-- return explicit metadata through 'runWithTokenProviderAttempt'.
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
    runWithTokenProviderAttemptAfter provider initialFailure \credential ->
        first (ProviderAttemptFailure ReplaySafe) <$> action credential

-- | Whether repeating a failed action is safe, independently of whether its
-- error suggests that another credential could succeed. Unknown is deliberately
-- distinct from a known pre-effect failure and never permits account failover.
data ReplaySafety = ReplaySafe | ReplayUnsafe | ReplayUnknown
    deriving (Eq, Show)

data ProviderAttemptFailure = ProviderAttemptFailure
    { attemptReplaySafety :: !ReplaySafety
    , attemptError :: !ApiError
    } deriving (Eq, Show)

-- | Account classification cannot override the action's replay boundary.
-- Preserve the original error when replay is unsafe or unknown; do not rewrite
-- its provider type merely to prevent the classifier from recognizing it.
runWithTokenProviderAttempt
    :: TokenProvider
    -> (Credential -> IO (Either ProviderAttemptFailure a))
    -> IO (Either ApiError a)
runWithTokenProviderAttempt provider =
    runWithTokenProviderAttemptAfter provider Nothing

runWithTokenProviderAttemptAfter
    :: TokenProvider
    -> Maybe FailedCredential
    -> (Credential -> IO (Either ProviderAttemptFailure a))
    -> IO (Either ApiError a)
runWithTokenProviderAttemptAfter provider initialFailure action =
    go maxProviderFailoverAttempts initialFailure
  where
    go attemptsLeft failed
        | attemptsLeft <= 0 = pure $ Left $ ConnectionError
            "token provider failover budget exhausted"
        | otherwise = getNextToken provider failed >>= \case
            Left err -> pure (Left err)
            Right credential -> action credential >>= \case
                Left ProviderAttemptFailure{attemptReplaySafety, attemptError = err}
                    | attemptReplaySafety == ReplaySafe
                    , Just failure <-
                        provider.runClassifyAccountFailure credential err ->
                        go (attemptsLeft - 1) $ Just FailedCredential
                            { credential
                            , failure
                            , failureReason =
                                accountFailureReason err failure
                            }
                Left failedAttempt -> pure (Left failedAttempt.attemptError)
                Right result -> pure (Right result)

-- | Credential failover for a streaming action. The transport identifies
-- events that cross its replay boundary (including non-visible output and
-- async tool admission), and must finish all event callbacks before returning.
-- Latch the boundary /before/ invoking the consumer, which may start effects.
--
-- Absence of output is not proof that arbitrary external work is replay-safe:
-- this adapter is for transports whose classified account errors before output
-- represent a rejected request. Other actions must supply explicit failure
-- metadata. Exceptions, including cancellation and consumer failures, propagate
-- unchanged and are never caught or replayed here.
runWithTokenProviderStreaming
    :: TokenProvider
    -> (event -> Bool)
    -> (Credential -> (event -> IO ()) -> IO (Either ApiError a))
    -> (event -> IO ())
    -> IO (Either ApiError a)
runWithTokenProviderStreaming provider outputObserved send onEvent =
    runWithTokenProviderAttempt provider \credential -> do
        emitted <- newIORef False
        result <- send credential \event -> do
            when (outputObserved event) (atomicWriteIORef emitted True)
            onEvent event
        observed <- readIORef emitted
        pure $ first
            (ProviderAttemptFailure
                (if observed then ReplayUnsafe else ReplaySafe))
            result

maxProviderFailoverAttempts :: Int
maxProviderFailoverAttempts = 64

accountFailureFromApiError :: ApiError -> Maybe AccountFailure
accountFailureFromApiError err = case err of
    HttpError 429 _ -> rateLimited
    ProviderError RateLimitError _ _ -> rateLimited
    ProviderError UsageLimitReached _ _ -> rateLimited
    ProviderError UsageBalanceExhausted _ _ -> rateLimited
    HttpError 401 _ -> authenticationRejected
    ProviderError AuthenticationError _ _ -> authenticationRejected
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

-- | Convert a rate-limited credential report into the shared cooldown error.
-- Other failure kinds do not represent a provider-wide exhaustion window.
credentialsExhaustedForRateLimit
    :: FailedCredential
    -> IO (Maybe ApiError)
credentialsExhaustedForRateLimit FailedCredential
    { failure = AccountRateLimited { retryAfterSeconds }
    , failureReason
    } = do
    now <- getCurrentTime
    let seconds = max 1 (fromMaybe 60 retryAfterSeconds)
    pure $ Just $ CredentialsExhausted
        { retryAt = addUTCTime (fromIntegral seconds) now
        , exhaustionReasons = [failureReason]
        }
credentialsExhaustedForRateLimit _ = pure Nothing
