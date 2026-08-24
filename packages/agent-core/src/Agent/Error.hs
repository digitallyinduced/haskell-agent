module Agent.Error
    ( ApiError(..)
    , ErrorType(..)
    , CredentialExhaustionReason(..)
    , RetryDisposition(..)
    , errorTypeFromText
    , errorTypeText
    , retryDisposition
    , isRetryable
    , isInlineRetryableProviderError
    , isInlineRetryableProviderResponseError
    , apiErrorRetryAfter
    , credentialExhaustionReasonFromApiError
    , credentialsExhausted
    , credentialsExhaustedWithReasons
    , credentialsExhaustedRetryAt
    ) where

import Data.Aeson (FromJSON(..), ToJSON(..), Value(String), withText)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

-- | Provider-independent error categories used by transport and credential
-- orchestration. Unknown upstream categories remain lossless.
data ErrorType
    = InvalidRequestError
    | AuthenticationError
    | PermissionError
    | NotFoundError
    | PreviousResponseNotFound
    | ContextWindowExceeded
    | InvalidImageError
    | RateLimitError
    | UsageLimitReached
    | UsageBalanceExhausted
    | QuotaExceeded
    | UsageNotIncluded
    | ApiErrorType
    | OverloadedError
    | ServiceUnavailableError
    | BillingError
    | ClientUpdateRequired
    | PayloadTooLargeError
    | WebSocketConnectionLimitReached
    | CyberPolicyError
    | MisalignmentPolicyViolation
    | UnknownErrorType !Text
    deriving (Eq, Show)

-- | Redacted, structured reason why a credential entered cooldown.
--
-- Provider response bodies and token material are deliberately excluded so
-- this value is safe to retain in session errors and account snapshots.
data CredentialExhaustionReason
    = ExhaustedByRateLimit
        { exhaustionErrorType :: !(Maybe ErrorType)
        , exhaustionStatusCode :: !(Maybe Int)
        , exhaustionRetryAfter :: !(Maybe Int)
        }
    | ExhaustedByAuthentication
        { exhaustionErrorType :: !(Maybe ErrorType)
        , exhaustionStatusCode :: !(Maybe Int)
        }
    deriving (Eq, Show)

-- | Whether retrying can help, and which layer should own the retry.
--
-- A usage-window exhaustion must not be retried inline: the credential
-- orchestrator parks or replaces that account until the provider reset.
data RetryDisposition
    = DoNotRetry
    | RetryTransiently
    | RetryAfterLimitReset
    deriving (Eq, Show)

instance FromJSON ErrorType where
    parseJSON = withText "ErrorType" (pure . errorTypeFromText)

instance ToJSON ErrorType where
    toJSON = String . errorTypeText

errorTypeFromText :: Text -> ErrorType
errorTypeFromText = \case
    "invalid_request_error" -> InvalidRequestError
    "invalid_prompt" -> InvalidRequestError
    "bio_policy" -> InvalidRequestError
    "authentication_error" -> AuthenticationError
    "invalid_api_key" -> AuthenticationError
    "permission_error" -> PermissionError
    "not_found_error" -> NotFoundError
    "previous_response_not_found" -> PreviousResponseNotFound
    "context_length_exceeded" -> ContextWindowExceeded
    "model_context_window_exceeded" -> ContextWindowExceeded
    "invalid_image" -> InvalidImageError
    "rate_limit_error" -> RateLimitError
    "rate_limit_exceeded" -> RateLimitError
    "rate_limited" -> RateLimitError
    "usage_limit_reached" -> UsageLimitReached
    "usage_balance_exhausted" -> UsageBalanceExhausted
    "insufficient_quota" -> QuotaExceeded
    "usage_not_included" -> UsageNotIncluded
    "api_error" -> ApiErrorType
    "server_error" -> ApiErrorType
    "internal_error" -> ApiErrorType
    "overloaded_error" -> OverloadedError
    "server_is_overloaded" -> OverloadedError
    "slow_down" -> OverloadedError
    "service_unavailable_error" -> ServiceUnavailableError
    "billing_error" -> BillingError
    "client_update_required" -> ClientUpdateRequired
    "payload_too_large" -> PayloadTooLargeError
    "websocket_connection_limit_reached" -> WebSocketConnectionLimitReached
    "cyber_policy" -> CyberPolicyError
    "misalignment_policy_violation" -> MisalignmentPolicyViolation
    other -> UnknownErrorType other

errorTypeText :: ErrorType -> Text
errorTypeText = \case
    InvalidRequestError -> "invalid_request_error"
    AuthenticationError -> "authentication_error"
    PermissionError -> "permission_error"
    NotFoundError -> "not_found_error"
    PreviousResponseNotFound -> "previous_response_not_found"
    ContextWindowExceeded -> "context_length_exceeded"
    InvalidImageError -> "invalid_image"
    RateLimitError -> "rate_limit_error"
    UsageLimitReached -> "usage_limit_reached"
    UsageBalanceExhausted -> "usage_balance_exhausted"
    QuotaExceeded -> "insufficient_quota"
    UsageNotIncluded -> "usage_not_included"
    ApiErrorType -> "api_error"
    OverloadedError -> "overloaded_error"
    ServiceUnavailableError -> "service_unavailable_error"
    BillingError -> "billing_error"
    ClientUpdateRequired -> "client_update_required"
    PayloadTooLargeError -> "payload_too_large"
    WebSocketConnectionLimitReached -> "websocket_connection_limit_reached"
    CyberPolicyError -> "cyber_policy"
    MisalignmentPolicyViolation -> "misalignment_policy_violation"
    UnknownErrorType value -> value

data ApiError
    = HttpError { statusCode :: !Int, body :: !Text }
    | JsonDecodeError { decodeError :: !Text, rawBody :: !Text }
    | ProviderError
        { errorType :: !ErrorType
        , message :: !Text
        , retryAfter :: !(Maybe Int)
        }
    | CredentialError { credentialMessage :: !Text }
    | ConnectionError { exception :: !Text }
    | CredentialsExhausted
        { retryAt :: !UTCTime
        , exhaustionReasons :: ![CredentialExhaustionReason]
        }
    deriving (Eq, Show)

retryDisposition :: ApiError -> RetryDisposition
retryDisposition = \case
    ProviderError RateLimitError _ _ -> RetryAfterLimitReset
    ProviderError UsageLimitReached _ _ -> RetryAfterLimitReset
    ProviderError UsageBalanceExhausted _ _ -> RetryAfterLimitReset
    ProviderError OverloadedError _ _ -> RetryTransiently
    ProviderError ServiceUnavailableError _ _ -> RetryTransiently
    ProviderError ApiErrorType _ _ -> RetryTransiently
    ProviderError WebSocketConnectionLimitReached _ _ -> RetryTransiently
    ConnectionError _ -> RetryTransiently
    HttpError 429 _ -> RetryAfterLimitReset
    HttpError status _
        | status `elem` [408, 409, 425] -> RetryTransiently
        | status >= 500 && status < 600 -> RetryTransiently
    _ -> DoNotRetry

isRetryable :: ApiError -> Bool
isRetryable = (/= DoNotRetry) . retryDisposition

-- | Short-lived failures that are safe to retry inside one transport call.
-- Credential quota failures are handled by provider failover instead.
isInlineRetryableProviderError :: ApiError -> Bool
isInlineRetryableProviderError = \case
    ConnectionError _ -> True
    HttpError status _ ->
        status `elem` [408, 409, 425]
            || (status >= 500 && status < 600)
    ProviderError OverloadedError _ _ -> True
    ProviderError ServiceUnavailableError _ _ -> True
    ProviderError ApiErrorType _ _ -> True
    ProviderError WebSocketConnectionLimitReached _ _ -> True
    _ -> False

-- | Failures repeatable on an existing connection. A broken or saturated
-- WebSocket must escape to the connection-replacement layer.
isInlineRetryableProviderResponseError :: ApiError -> Bool
isInlineRetryableProviderResponseError = \case
    ConnectionError _ -> False
    ProviderError WebSocketConnectionLimitReached _ _ -> False
    apiError -> isInlineRetryableProviderError apiError

apiErrorRetryAfter :: ApiError -> Maybe Int
apiErrorRetryAfter (ProviderError _ _ value) = value
apiErrorRetryAfter _ = Nothing

-- | Extract a token-safe account-cooldown reason from an upstream error.
credentialExhaustionReasonFromApiError
    :: ApiError
    -> Maybe CredentialExhaustionReason
credentialExhaustionReasonFromApiError = \case
    HttpError 429 _ ->
        Just ExhaustedByRateLimit
            { exhaustionErrorType = Nothing
            , exhaustionStatusCode = Just 429
            , exhaustionRetryAfter = Nothing
            }
    ProviderError errorType _ retryAfter
        | errorType `elem`
            [ RateLimitError
            , UsageLimitReached
            , UsageBalanceExhausted
            ] ->
                Just ExhaustedByRateLimit
                    { exhaustionErrorType = Just errorType
                    , exhaustionStatusCode = Nothing
                    , exhaustionRetryAfter = retryAfter
                    }
    HttpError status _
        | status == 401 || status == 403 ->
            Just ExhaustedByAuthentication
                { exhaustionErrorType = Nothing
                , exhaustionStatusCode = Just status
                }
    ProviderError AuthenticationError _ _ ->
        Just ExhaustedByAuthentication
            { exhaustionErrorType = Just AuthenticationError
            , exhaustionStatusCode = Nothing
            }
    CredentialError{} ->
        Just ExhaustedByAuthentication
            { exhaustionErrorType = Nothing
            , exhaustionStatusCode = Nothing
            }
    _ -> Nothing

-- | Construct an exhaustion error when no more specific cooldown diagnostics
-- are available.
credentialsExhausted :: UTCTime -> ApiError
credentialsExhausted retryAt =
    credentialsExhaustedWithReasons retryAt []

credentialsExhaustedWithReasons
    :: UTCTime
    -> [CredentialExhaustionReason]
    -> ApiError
credentialsExhaustedWithReasons retryAt exhaustionReasons = CredentialsExhausted
    { retryAt
    , exhaustionReasons
    }

credentialsExhaustedRetryAt :: ApiError -> Maybe UTCTime
credentialsExhaustedRetryAt CredentialsExhausted{retryAt} = Just retryAt
credentialsExhaustedRetryAt _ = Nothing
