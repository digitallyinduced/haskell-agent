-- | OpenRouter-specific normalization of HTTP and streaming failures.
module Agent.OpenRouter.Error
    ( classifyFailure
    , classifyStreamError
    ) where

import Agent.Error (ApiError(..), ErrorType(..), errorTypeFromText)
import Agent.OpenAI.Error (classifyHttpFailure, decodeOpenAIError, mkOpenAIError)
import Agent.OpenAI.Responses.Types (ResponseStreamError(..))
import Control.Applicative ((<|>))
import Data.Aeson ((.:), (.:?), (.!=), FromJSON(..), withObject)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

data OpenRouterErrorEnvelope = OpenRouterErrorEnvelope
    { envelopeCode :: !(Maybe Text)
    , envelopeMessage :: !Text
    }

instance FromJSON OpenRouterErrorEnvelope where
    parseJSON = withObject "OpenRouter error envelope" \object -> do
        errObject <- object .: "error"
        OpenRouterErrorEnvelope
            <$> errObject .:? "code"
            <*> (errObject .: "message" <|> errObject .:? "msg" .!= "")

-- | Classify a non-success response from OpenRouter.
classifyFailure :: Int -> Maybe Int -> Text -> ApiError
classifyFailure status retryAfterHeader body =
    case decodeOpenAIError body of
        Right (ProviderError errType message retryAfter) ->
            ProviderError errType message (retryAfter `orElse` retryAfterHeader)
        Right other -> other
        Left _ -> case decodeOpenRouterError body of
            Just envelope ->
                ProviderError
                    (errorTypeFromOpenRouter status envelope.envelopeCode)
                    (nonEmptyMessage envelope.envelopeMessage body)
                    retryAfterHeader
            Nothing -> case status of
                401 -> auth
                403 -> auth
                402 -> ProviderError BillingError (preview body) Nothing
                429 -> ProviderError RateLimitError (preview body) retryAfterHeader
                _ -> case classifyHttpFailure status body of
                    HttpError 429 message ->
                        ProviderError RateLimitError (preview message) retryAfterHeader
                    other -> other
  where
    auth = ProviderError AuthenticationError (preview body) Nothing

-- | Convert a typed Responses streaming error into the shared error channel.
classifyStreamError :: ResponseStreamError -> ApiError
classifyStreamError streamError
    | Just errorType <- streamError.errorType =
        mkOpenAIError
            (errorTypeFromText errorType)
            streamError.message
            streamError.code
            streamError.retryAfter
    | otherwise = ConnectionError
        ("OpenRouter stream error: " <> streamError.message)

decodeOpenRouterError :: Text -> Maybe OpenRouterErrorEnvelope
decodeOpenRouterError body =
    case Aeson.eitherDecodeStrict' (Text.encodeUtf8 body) of
        Right envelope -> Just envelope
        Left _ -> Nothing

errorTypeFromOpenRouter :: Int -> Maybe Text -> ErrorType
errorTypeFromOpenRouter status code = case Text.toLower <$> code of
    Just "invalid_prompt" -> InvalidRequestError
    Just "invalid_request" -> InvalidRequestError
    Just "invalid_request_error" -> InvalidRequestError
    Just "authentication" -> AuthenticationError
    Just "unauthorized" -> AuthenticationError
    Just "forbidden" -> AuthenticationError
    Just "rate_limit" -> RateLimitError
    Just "rate_limit_error" -> RateLimitError
    Just "insufficient_credits" -> BillingError
    Just "payment_required" -> BillingError
    Just "billing_error" -> BillingError
    _ -> case status of
        400 -> InvalidRequestError
        401 -> AuthenticationError
        403 -> AuthenticationError
        402 -> BillingError
        404 -> NotFoundError
        429 -> RateLimitError
        _ -> ApiErrorType

nonEmptyMessage :: Text -> Text -> Text
nonEmptyMessage message body
    | Text.null (Text.strip message) = preview body
    | otherwise = message

preview :: Text -> Text
preview = Text.take 500

Just value `orElse` _ = Just value
Nothing `orElse` fallback = fallback
