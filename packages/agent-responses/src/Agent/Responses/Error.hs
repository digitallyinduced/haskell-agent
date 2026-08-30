-- | OpenAI error-envelope decoding and Responses-specific normalization.
module Agent.Responses.Error
    ( decodeOpenAIError
    , mkOpenAIError
    , classifyHttpFailure
    , isPreviousResponseIdError
    , isResponseChainCompatibilityError
    ) where

import Agent.Error
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

data ProviderErrorPayload = ProviderErrorPayload
    { payloadType :: !(Maybe Text)
    , payloadCode :: !(Maybe Text)
    , payloadMessage :: !Text
    , payloadRetryAfter :: !(Maybe Int)
    }

decodeOpenAIError :: Text -> Either String ApiError
decodeOpenAIError body = do
    payload <- decodeProviderErrorPayload body
    pure $ mkOpenAIError
        (maybe ApiErrorType errorTypeFromText payload.payloadType)
        payload.payloadMessage
        payload.payloadCode
        payload.payloadRetryAfter

-- | Build an error from OpenAI's structured error fields, normalizing the
-- several wire shapes used for a missing @previous_response_id@.
mkOpenAIError :: ErrorType -> Text -> Maybe Text -> Maybe Int -> ApiError
mkOpenAIError errorType message code retryAfter =
    ProviderError
        (classifyErrorType errorType message code)
        (appendErrorCode code message)
        (retryAfter `orElse` retryAfterFromMessage message)

classifyErrorType :: ErrorType -> Text -> Maybe Text -> ErrorType
classifyErrorType errorType message code
    | isPreviousResponseNotFound errorType message code = PreviousResponseNotFound
    | normalizedCode `elem` map Just ["context_length_exceeded", "model_context_window_exceeded"] =
        ContextWindowExceeded
    | normalizedCode == Just "invalid_image" = InvalidImageError
    | normalizedCode `elem` map Just ["invalid_api_key", "token_expired"] =
        AuthenticationError
    | normalizedCode `elem` map Just ["permission_denied", "forbidden"] =
        PermissionError
    | normalizedCode `elem` map Just ["model_not_found", "not_found"] =
        NotFoundError
    | normalizedCode `elem` map Just ["rate_limit_exceeded", "rate_limit_error"] = RateLimitError
    | normalizedCode == Just "usage_limit_reached" = UsageLimitReached
    | normalizedCode == Just "usage_balance_exhausted" = UsageBalanceExhausted
    | normalizedCode == Just "insufficient_quota" = QuotaExceeded
    | normalizedCode == Just "usage_not_included" = UsageNotIncluded
    | normalizedCode `elem` map Just ["server_is_overloaded", "slow_down"] = OverloadedError
    | normalizedCode == Just "service_unavailable_error" = ServiceUnavailableError
    | normalizedCode == Just "websocket_connection_limit_reached" =
        WebSocketConnectionLimitReached
    | normalizedCode == Just "cyber_policy" = CyberPolicyError
    | normalizedCode == Just "misalignment_policy_violation" =
        MisalignmentPolicyViolation
    | normalizedCode `elem` map Just ["invalid_prompt", "bio_policy"] = InvalidRequestError
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
    case decodeProviderErrorPayload body of
        Right payload ->
            mkOpenAIError
                (maybe (errorTypeFromStatus status) errorTypeFromText payload.payloadType)
                payload.payloadMessage
                payload.payloadCode
                payload.payloadRetryAfter
        Left _ -> HttpError status (Text.take 1000 body)

decodeProviderErrorPayload :: Text -> Either String ProviderErrorPayload
decodeProviderErrorPayload body = do
    value <- Aeson.eitherDecodeStrict' (Text.encodeUtf8 body)
    maybe (Left "JSON body contains no provider error") Right (payloadFromValue value)

payloadFromValue :: Aeson.Value -> Maybe ProviderErrorPayload
payloadFromValue = \case
    Aeson.Object object ->
        case KeyMap.lookup "error" object of
            Just (Aeson.Object nested) -> payloadFromObjects object nested
            Just (Aeson.String message)
                | not (Text.null (Text.strip message)) ->
                    Just ProviderErrorPayload
                        { payloadType = providerTypeField object
                        , payloadCode = scalarTextField "code" object
                            `orElse` scalarTextField "error_code" object
                        , payloadMessage = message
                        , payloadRetryAfter = retryAfterField object
                            `orElse` retryAfterFromMessage message
                        }
            Just value -> payloadFromValue value
            Nothing -> payloadFromTopLevel object
    Aeson.String message
        | not (Text.null (Text.strip message)) ->
            Just ProviderErrorPayload
                { payloadType = Nothing
                , payloadCode = Nothing
                , payloadMessage = message
                , payloadRetryAfter = retryAfterFromMessage message
                }
    Aeson.Array values -> firstJust (map payloadFromValue (foldr (:) [] values))
    _ -> Nothing

payloadFromObjects :: Aeson.Object -> Aeson.Object -> Maybe ProviderErrorPayload
payloadFromObjects outer nested = do
    message <- firstTextField ["message", "error", "detail", "description"] nested
        `orElse` textField "type" nested
    pure ProviderErrorPayload
        { payloadType = providerTypeField nested `orElse` providerTypeField outer
        , payloadCode = scalarTextField "code" nested
            `orElse` scalarTextField "error_code" nested
            `orElse` scalarTextField "code" outer
        , payloadMessage = message
        , payloadRetryAfter = retryAfterField nested
            `orElse` retryAfterField outer
            `orElse` retryAfterFromMessage message
        }

payloadFromTopLevel :: Aeson.Object -> Maybe ProviderErrorPayload
payloadFromTopLevel object = do
    message <- firstTextField ["message", "detail", "msg", "description"] object
    pure ProviderErrorPayload
        { payloadType = providerTypeField object
        , payloadCode = scalarTextField "code" object
            `orElse` scalarTextField "error_code" object
        , payloadMessage = message
        , payloadRetryAfter = retryAfterField object
            `orElse` retryAfterFromMessage message
        }

errorTypeFromStatus :: Int -> ErrorType
errorTypeFromStatus = \case
    400 -> InvalidRequestError
    401 -> AuthenticationError
    402 -> BillingError
    403 -> PermissionError
    404 -> NotFoundError
    408 -> ApiErrorType
    413 -> PayloadTooLargeError
    422 -> InvalidRequestError
    426 -> ClientUpdateRequired
    429 -> RateLimitError
    500 -> ApiErrorType
    502 -> ServiceUnavailableError
    503 -> ServiceUnavailableError
    504 -> ServiceUnavailableError
    529 -> OverloadedError
    _ -> ApiErrorType

textField :: Text -> Aeson.Object -> Maybe Text
textField name object = case KeyMap.lookup (Key.fromText name) object of
    Just (Aeson.String value) | not (Text.null (Text.strip value)) -> Just value
    _ -> Nothing

providerTypeField :: Aeson.Object -> Maybe Text
providerTypeField object = do
    value <- textField "type" object
    if Text.toLower value `elem` ["error", "errors", "unknown", "unknown_error"]
        then Nothing
        else Just value

scalarTextField :: Text -> Aeson.Object -> Maybe Text
scalarTextField name object = case KeyMap.lookup (Key.fromText name) object of
    Just (Aeson.String value) | not (Text.null (Text.strip value)) -> Just value
    Just (Aeson.Number value) -> Just (Text.pack (show value))
    Just (Aeson.Bool value) -> Just (if value then "true" else "false")
    _ -> Nothing

firstTextField :: [Text] -> Aeson.Object -> Maybe Text
firstTextField names object = firstJust (map (`textField` object) names)

retryAfterField :: Aeson.Object -> Maybe Int
retryAfterField object =
    numberField "resets_in_seconds" object
        `orElse` numberField "retry_after" object

numberField :: Text -> Aeson.Object -> Maybe Int
numberField name object = case KeyMap.lookup (Key.fromText name) object of
    Just (Aeson.Number value) -> max 1 <$> toBoundedInteger value
    Just (Aeson.String value) -> case reads (Text.unpack value) of
        [(seconds, "")] -> Just (max 1 seconds)
        _ -> Nothing
    _ -> Nothing

retryAfterFromMessage :: Text -> Maybe Int
retryAfterFromMessage message = do
    let lowered = Text.toLower message
        (_, suffix) = Text.breakOn "try again in" lowered
    rest <- Text.stripPrefix "try again in" suffix
    let stripped = Text.stripStart rest
        token = Text.takeWhile isNumericCharacter stripped
        unit = Text.toLower
            $ Text.takeWhile isAsciiLetter
            $ Text.stripStart
            $ Text.drop (Text.length token) stripped
    value <- case reads (Text.unpack token) of
        [(seconds, "")] -> Just (seconds :: Double)
        _ -> Nothing
    pure $ max 1 $ ceiling $
        if unit == "ms" then value / 1000 else value
  where
    isNumericCharacter character =
        character == '.' || character >= '0' && character <= '9'
    isAsciiLetter character =
        character >= 'a' && character <= 'z'

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Just value : _) = Just value
firstJust (Nothing : rest) = firstJust rest

isPreviousResponseIdError :: ApiError -> Bool
isPreviousResponseIdError = \case
    ProviderError PreviousResponseNotFound _ _ -> True
    ProviderError errorType message _ ->
        mentionsPreviousResponse (Text.pack (show errorType))
            || mentionsPreviousResponse message
    ConnectionError message -> mentionsPreviousResponse message
    CredentialError{} -> False
    HttpError _ body -> mentionsPreviousResponse body
    JsonDecodeError message body ->
        mentionsPreviousResponse message || mentionsPreviousResponse body
    CredentialsExhausted{} -> False

-- | Errors that can be caused by provider-managed state attached to
-- @previous_response_id@ rather than by the current request body.
--
-- The Codex backend can return stored chains carrying
-- @prompt_cache_retention@ parameter after routing a follow-up to a model that
-- no longer accepts it. Provider-managed continuation state can also lose a
-- pending function call and reject its otherwise valid output. Replaying the
-- local transcript without the continuation id creates a fresh chain and is
-- safe before any model output is observed.
isResponseChainCompatibilityError :: ApiError -> Bool
isResponseChainCompatibilityError error =
    isPreviousResponseIdError error
        || apiErrorTextMatches mentionsUnsupportedPromptCacheRetention error
        || apiErrorTextMatches mentionsMissingFunctionCallOutput error

apiErrorTextMatches :: (Text -> Bool) -> ApiError -> Bool
apiErrorTextMatches predicate = \case
    ProviderError errorType message _ ->
        predicate (Text.pack (show errorType)) || predicate message
    ConnectionError message -> predicate message
    CredentialError message -> predicate message
    HttpError _ body -> predicate body
    JsonDecodeError message body ->
        predicate message || predicate body
    CredentialsExhausted{} -> False

mentionsPreviousResponse :: Text -> Bool
mentionsPreviousResponse value =
    let lowered = Text.toLower value
    in "previous_response" `Text.isInfixOf` lowered
        || "previous response" `Text.isInfixOf` lowered
        || "response_not_found" `Text.isInfixOf` lowered
        || "previous_response_not_found" `Text.isInfixOf` lowered

mentionsUnsupportedPromptCacheRetention :: Text -> Bool
mentionsUnsupportedPromptCacheRetention value =
    let lowered = Text.toLower value
    in "prompt_cache_retention" `Text.isInfixOf` lowered
        && ("not supported" `Text.isInfixOf` lowered
            || "unsupported" `Text.isInfixOf` lowered)

mentionsMissingFunctionCallOutput :: Text -> Bool
mentionsMissingFunctionCallOutput =
    Text.isInfixOf "no tool output found for function call"
        . Text.toLower

orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just value) _ = Just value
orElse Nothing fallback = fallback
