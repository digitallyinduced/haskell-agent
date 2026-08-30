-- | Normalization of Google API error envelopes into the shared error model.
module Agent.Gemini.Error
    ( classifyFailure
    ) where

import Agent.Error (ApiError(..), ErrorType(..))
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

data GoogleErrorEnvelope = GoogleErrorEnvelope
    { googleErrorMessage :: !Text
    , googleErrorStatus :: !(Maybe Text)
    }

instance FromJSON GoogleErrorEnvelope where
    parseJSON = withObject "Google API error envelope" \object ->
        object .: "error" >>= withObject "Google API error" \err ->
            GoogleErrorEnvelope
                <$> err .:? "message" .!= ""
                <*> err .:? "status"

-- | Classify a non-success HTTP response. Only the provider's short message is
-- retained; error details may contain request material and are deliberately
-- omitted from diagnostics.
classifyFailure :: Int -> Maybe Int -> Text -> ApiError
classifyFailure httpStatus retryAfterHeader body =
    ProviderError errorType message retryAfter
  where
    envelope = decodeEnvelope body
    upstreamStatus = envelope >>= (.googleErrorStatus)
    upstreamMessage = maybe "" (.googleErrorMessage) envelope
    message
        | Text.null (Text.strip upstreamMessage) = preview body
        | otherwise = upstreamMessage
    errorType
        | looksLikeApiKeyFailure message = AuthenticationError
        | otherwise = errorTypeFromGoogle httpStatus upstreamStatus
    retryAfter
        | errorType `elem`
            [ RateLimitError
            , OverloadedError
            , ServiceUnavailableError
            , ApiErrorType
            ] = retryAfterHeader
        | otherwise = Nothing

decodeEnvelope :: Text -> Maybe GoogleErrorEnvelope
decodeEnvelope body =
    decode
        (LBS.fromStrict (TextEncoding.encodeUtf8 body))

errorTypeFromGoogle :: Int -> Maybe Text -> ErrorType
errorTypeFromGoogle httpStatus upstreamStatus =
    case Text.toUpper . Text.strip <$> upstreamStatus of
        Just "UNAUTHENTICATED" -> AuthenticationError
        Just "PERMISSION_DENIED" -> PermissionError
        Just "RESOURCE_EXHAUSTED" -> RateLimitError
        Just "NOT_FOUND" -> NotFoundError
        Just "INVALID_ARGUMENT" -> InvalidRequestError
        Just "FAILED_PRECONDITION" -> InvalidRequestError
        Just "ALREADY_EXISTS" -> InvalidRequestError
        Just "OUT_OF_RANGE" -> InvalidRequestError
        Just "CANCELLED" -> ServiceUnavailableError
        Just "DEADLINE_EXCEEDED" -> ServiceUnavailableError
        Just "ABORTED" -> ServiceUnavailableError
        Just "UNAVAILABLE" -> ServiceUnavailableError
        Just "INTERNAL" -> ApiErrorType
        Just "DATA_LOSS" -> ApiErrorType
        Just "UNKNOWN" -> ApiErrorType
        _ -> errorTypeFromStatus httpStatus

errorTypeFromStatus :: Int -> ErrorType
errorTypeFromStatus = \case
    400 -> InvalidRequestError
    401 -> AuthenticationError
    403 -> PermissionError
    404 -> NotFoundError
    408 -> ServiceUnavailableError
    409 -> ServiceUnavailableError
    413 -> PayloadTooLargeError
    425 -> ServiceUnavailableError
    429 -> RateLimitError
    500 -> ApiErrorType
    502 -> ServiceUnavailableError
    503 -> OverloadedError
    504 -> ServiceUnavailableError
    status
        | status >= 400 && status < 500 -> InvalidRequestError
        | otherwise -> ApiErrorType

looksLikeApiKeyFailure :: Text -> Bool
looksLikeApiKeyFailure message =
    any (`Text.isInfixOf` normalized)
        [ "api key not valid"
        , "invalid api key"
        , "api_key_invalid"
        , "api key was reported as leaked"
        ]
  where
    normalized = Text.toLower message

preview :: Text -> Text
preview body
    | Text.null (Text.strip body) = "Gemini request failed"
    | otherwise = Text.take 500 body
