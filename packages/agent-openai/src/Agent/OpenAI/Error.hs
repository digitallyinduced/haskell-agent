-- | OpenAI error-envelope decoding and Responses-specific normalization.
module Agent.OpenAI.Error
    ( decodeOpenAIError
    , mkOpenAIError
    , classifyHttpFailure
    , isPreviousResponseIdError
    ) where

import Agent.Error
import Data.Aeson ((.:), (.:?), FromJSON(..), eitherDecodeStrict', withObject)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

newtype OpenAIErrorEnvelope = OpenAIErrorEnvelope ApiError

instance FromJSON OpenAIErrorEnvelope where
    parseJSON = withObject "OpenAI error envelope" \object -> do
        errObject <- object .: "error"
        message <- errObject .: "message"
        code <- errObject .:? "code"
        errorType <- errObject .: "type"
        retryAfter <- errObject .:? "resets_in_seconds"
        pure (OpenAIErrorEnvelope (mkOpenAIError errorType message code retryAfter))

decodeOpenAIError :: Text -> Either String ApiError
decodeOpenAIError body = do
    OpenAIErrorEnvelope apiError <- eitherDecodeStrict' (Text.encodeUtf8 body)
    pure apiError

-- | Build an error from OpenAI's structured error fields, normalizing the
-- several wire shapes used for a missing @previous_response_id@.
mkOpenAIError :: ErrorType -> Text -> Maybe Text -> Maybe Int -> ApiError
mkOpenAIError errorType message code retryAfter =
    ProviderError
        (classifyErrorType errorType message code)
        (appendErrorCode code message)
        retryAfter

classifyErrorType :: ErrorType -> Text -> Maybe Text -> ErrorType
classifyErrorType errorType message code
    | isPreviousResponseNotFound errorType message code = PreviousResponseNotFound
    | normalizedCode == Just "server_is_overloaded" = OverloadedError
    | normalizedCode `elem` map Just ["server_error", "internal_error"] = ApiErrorType
    | normalizedErrorType == Just "overloaded_error" = OverloadedError
    | normalizedErrorType `elem` map Just ["server_error", "internal_error"] = ApiErrorType
    | otherwise = errorType
  where
    normalizedCode = Text.toLower <$> code
    normalizedErrorType = case errorType of
        UnknownErrorType value -> Just (Text.toLower value)
        _ -> Nothing

isPreviousResponseNotFound :: ErrorType -> Text -> Maybe Text -> Bool
isPreviousResponseNotFound PreviousResponseNotFound _ _ = True
isPreviousResponseNotFound errorType message code =
    code == Just "previous_response_not_found"
        || (isPreviousResponseCarrier errorType && mentionsPreviousResponse message)

isPreviousResponseCarrier :: ErrorType -> Bool
isPreviousResponseCarrier InvalidRequestError = True
isPreviousResponseCarrier NotFoundError = True
isPreviousResponseCarrier _ = False

appendErrorCode :: Maybe Text -> Text -> Text
appendErrorCode Nothing message = message
appendErrorCode (Just code) message
    | Text.null message = "code: " <> code
    | otherwise = message <> " (code: " <> code <> ")"

classifyHttpFailure :: Int -> Text -> ApiError
classifyHttpFailure status body =
    case decodeOpenAIError body of
        Right apiError -> apiError
        Left _ -> HttpError status (Text.take 1000 body)

isPreviousResponseIdError :: ApiError -> Bool
isPreviousResponseIdError = \case
    ProviderError PreviousResponseNotFound _ _ -> True
    ProviderError errorType message _ ->
        mentionsPreviousResponse (Text.pack (show errorType))
            || mentionsPreviousResponse message
    ConnectionError message -> mentionsPreviousResponse message
    HttpError _ body -> mentionsPreviousResponse body
    JsonDecodeError message body ->
        mentionsPreviousResponse message || mentionsPreviousResponse body
    CredentialsExhausted{} -> False

mentionsPreviousResponse :: Text -> Bool
mentionsPreviousResponse value =
    let lowered = Text.toLower value
    in "previous_response" `Text.isInfixOf` lowered
        || "previous response" `Text.isInfixOf` lowered
        || "response_not_found" `Text.isInfixOf` lowered
        || "previous_response_not_found" `Text.isInfixOf` lowered
