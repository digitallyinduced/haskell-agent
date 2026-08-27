-- | OpenRouter-specific normalization of HTTP and streaming failures.
module Agent.OpenRouter.Error
    ( classifyFailure
    , classifyStreamError
    ) where

import Agent.Error (ApiError(..), ErrorType(..), errorTypeFromText)
import qualified Agent.Json.Decode as Json
import Agent.Responses.Error (classifyHttpFailure, decodeOpenAIError, mkOpenAIError)
import Agent.Responses.Types (ResponseStreamError(..))
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text

data OpenRouterErrorEnvelope = OpenRouterErrorEnvelope
    { envelopeCode :: !(Maybe Text)
    , envelopeMessage :: !Text
    }

openRouterErrorEnvelopeDecoder :: Json.Decoder OpenRouterErrorEnvelope
openRouterErrorEnvelopeDecoder = Json.object $
    Json.atKey "error" $ Json.object $
        OpenRouterErrorEnvelope
            <$> Json.optionalKey "code" Json.text
            <*> (Maybe.fromMaybe ""
                <$> firstPresent "message" "msg" Json.text)

-- | Classify a non-success response from OpenRouter.
classifyFailure :: Int -> Maybe Int -> Text -> ApiError
classifyFailure status retryAfterHeader body =
    case openRouterEnvelope of
        Just envelope
            | isOpenRouterSpecificCode envelope.envelopeCode ->
                fromOpenRouterEnvelope envelope
        _ -> case decodeOpenAIError body of
            Right (ProviderError ApiErrorType message retryAfter)
                | isPermanentClientStatus status ->
                    ProviderError
                        (errorTypeFromOpenRouter status
                            (openRouterEnvelope >>= \envelope -> envelope.envelopeCode))
                        message
                        (retryAfter `orElse` retryAfterHeader)
            Right (ProviderError errType message retryAfter) ->
                ProviderError errType message (retryAfter `orElse` retryAfterHeader)
            Right other -> other
            Left _ -> case openRouterEnvelope of
                Just envelope -> fromOpenRouterEnvelope envelope
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
    openRouterEnvelope = decodeOpenRouterError body
    auth = ProviderError AuthenticationError (preview body) Nothing
    fromOpenRouterEnvelope :: OpenRouterErrorEnvelope -> ApiError
    fromOpenRouterEnvelope envelope =
        ProviderError
            (errorTypeFromOpenRouter status envelope.envelopeCode)
            (nonEmptyMessage envelope.envelopeMessage body)
            retryAfterHeader

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
    case Json.decodeText openRouterErrorEnvelopeDecoder body of
        Right envelope -> Just envelope
        Left _ -> Nothing

firstPresent
    :: Text
    -> Text
    -> Json.Decoder value
    -> Json.FieldsDecoder (Maybe value)
firstPresent firstKey secondKey decoder = do
    first <- Json.optionalKey firstKey decoder
    second <- Json.optionalKey secondKey decoder
    pure (first `orElse` second)

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
        _ | status >= 400 && status < 500 -> InvalidRequestError
        _ -> ApiErrorType

isPermanentClientStatus :: Int -> Bool
isPermanentClientStatus status =
    status >= 400
        && status < 500
        && status `notElem` [408, 409, 425, 429]

isOpenRouterSpecificCode :: Maybe Text -> Bool
isOpenRouterSpecificCode code =
    (Text.toLower <$> code) `elem` map Just
        [ "invalid_prompt"
        , "invalid_request"
        , "invalid_request_error"
        , "authentication"
        , "unauthorized"
        , "forbidden"
        , "rate_limit"
        , "insufficient_credits"
        , "payment_required"
        , "billing_error"
        ]

nonEmptyMessage :: Text -> Text -> Text
nonEmptyMessage message body
    | Text.null (Text.strip message) = preview body
    | otherwise = message

preview :: Text -> Text
preview = Text.take 500

Just value `orElse` _ = Just value
Nothing `orElse` fallback = fallback
