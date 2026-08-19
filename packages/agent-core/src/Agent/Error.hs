module Agent.Error
    ( ApiError(..)
    , ErrorType(..)
    , errorTypeFromText
    , errorTypeText
    , isRetryable
    , isInlineRetryableProviderError
    , isInlineRetryableProviderResponseError
    , apiErrorRetryAfter
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
    | RateLimitError
    | UsageLimitReached
    | ApiErrorType
    | OverloadedError
    | ServiceUnavailableError
    | BillingError
    | UnknownErrorType !Text
    deriving (Eq, Show)

instance FromJSON ErrorType where
    parseJSON = withText "ErrorType" (pure . errorTypeFromText)

instance ToJSON ErrorType where
    toJSON = String . errorTypeText

errorTypeFromText :: Text -> ErrorType
errorTypeFromText = \case
    "invalid_request_error" -> InvalidRequestError
    "authentication_error" -> AuthenticationError
    "permission_error" -> PermissionError
    "not_found_error" -> NotFoundError
    "previous_response_not_found" -> PreviousResponseNotFound
    "rate_limit_error" -> RateLimitError
    "usage_limit_reached" -> UsageLimitReached
    "api_error" -> ApiErrorType
    "overloaded_error" -> OverloadedError
    "service_unavailable_error" -> ServiceUnavailableError
    "billing_error" -> BillingError
    other -> UnknownErrorType other

errorTypeText :: ErrorType -> Text
errorTypeText = \case
    InvalidRequestError -> "invalid_request_error"
    AuthenticationError -> "authentication_error"
    PermissionError -> "permission_error"
    NotFoundError -> "not_found_error"
    PreviousResponseNotFound -> "previous_response_not_found"
    RateLimitError -> "rate_limit_error"
    UsageLimitReached -> "usage_limit_reached"
    ApiErrorType -> "api_error"
    OverloadedError -> "overloaded_error"
    ServiceUnavailableError -> "service_unavailable_error"
    BillingError -> "billing_error"
    UnknownErrorType value -> value

data ApiError
    = HttpError { statusCode :: !Int, body :: !Text }
    | JsonDecodeError { decodeError :: !Text, rawBody :: !Text }
    | ProviderError
        { errorType :: !ErrorType
        , message :: !Text
        , retryAfter :: !(Maybe Int)
        }
    | ConnectionError { exception :: !Text }
    | CredentialsExhausted { retryAt :: !UTCTime }
    deriving (Eq, Show)

isRetryable :: ApiError -> Bool
isRetryable (ProviderError RateLimitError _ _) = True
isRetryable (ProviderError UsageLimitReached _ _) = True
isRetryable (ProviderError OverloadedError _ _) = True
isRetryable (ProviderError ServiceUnavailableError _ _) = True
isRetryable (ProviderError ApiErrorType _ _) = True
isRetryable _ = False

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
    _ -> False

-- | Failures repeatable on an existing connection. A broken connection must
-- escape to the connection-replacement layer.
isInlineRetryableProviderResponseError :: ApiError -> Bool
isInlineRetryableProviderResponseError = \case
    ConnectionError _ -> False
    apiError -> isInlineRetryableProviderError apiError

apiErrorRetryAfter :: ApiError -> Maybe Int
apiErrorRetryAfter (ProviderError _ _ value) = value
apiErrorRetryAfter _ = Nothing

credentialsExhaustedRetryAt :: ApiError -> Maybe UTCTime
credentialsExhaustedRetryAt (CredentialsExhausted value) = Just value
credentialsExhaustedRetryAt _ = Nothing
