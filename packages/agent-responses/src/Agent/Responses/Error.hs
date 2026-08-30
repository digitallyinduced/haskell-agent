-- | OpenAI error-envelope decoding and Responses-specific normalization.
module Agent.Responses.Error
    ( decodeOpenAIError
    , mkOpenAIError
    , classifyHttpFailure
    , isPreviousResponseIdError
    , isResponseChainCompatibilityError
    ) where

import Agent.Error
import Agent.Json.Decode
    ( Decoder
    , FieldsDecoder
    , ValueType(..)
    , atKeyOptional
    , decodeText
    , getType
    , int
    , jsonErrorMessage
    , nullable
    , object
    , text
    )
import Control.Applicative ((<|>))
import Control.Monad (join)
import Data.Text (Text)
import qualified Data.Text as Text

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
                (maybe (errorTypeFromStatus status) errorTypeFromText
                    (payload.payloadType >>= nonGenericErrorType))
                payload.payloadMessage
                payload.payloadCode
                payload.payloadRetryAfter
        Left _ -> HttpError status (Text.take 1000 body)

nonGenericErrorType :: Text -> Maybe Text
nonGenericErrorType value
    | Text.toLower value == "error" = Nothing
    | otherwise = Just value

decodeProviderErrorPayload :: Text -> Either String ProviderErrorPayload
decodeProviderErrorPayload body =
    either
        (Left . Text.unpack . jsonErrorMessage)
        Right
        (decodeText providerErrorPayloadDecoder body)

providerErrorPayloadDecoder :: Decoder ProviderErrorPayload
providerErrorPayloadDecoder = object do
    nested <- join <$> atKeyOptional "error"
        (nullable errorValueDecoder)
    payloadType <- join <$> atKeyOptional "type" (nullable text)
    code <- join <$> atKeyOptional "code" (nullable text)
    errorCode <- join <$> atKeyOptional "error_code" (nullable text)
    message <- firstPresent
        <$> traverseOptionalText ["message", "detail", "msg", "description"]
    retryAfter <- firstPresent <$> sequence
        [ join <$> atKeyOptional "resets_in_seconds" (nullable int)
        , join <$> atKeyOptional "retry_after" (nullable int)
        ]
    case nested of
        Just payload -> pure payload
            { payloadType = payload.payloadType <|> payloadType
            , payloadCode = payload.payloadCode <|> code <|> errorCode
            , payloadRetryAfter = payload.payloadRetryAfter <|> retryAfter
            }
        Nothing -> case message of
            Just value -> pure ProviderErrorPayload
                { payloadType
                , payloadCode = code <|> errorCode
                , payloadMessage = value
                , payloadRetryAfter =
                    retryAfter <|> retryAfterFromMessage value
                }
            Nothing -> fail "JSON body contains no provider error"

errorValueDecoder :: Decoder ProviderErrorPayload
errorValueDecoder =
    getType >>= \case
        VObject -> providerErrorPayloadDecoder
        VString -> do
            message <- text
            pure ProviderErrorPayload
                { payloadType = Nothing
                , payloadCode = Nothing
                , payloadMessage = message
                , payloadRetryAfter = retryAfterFromMessage message
                }
        _ -> fail "invalid provider error payload"

traverseOptionalText :: [Text] -> FieldsDecoder [Maybe Text]
traverseOptionalText =
    traverse \key -> join <$> atKeyOptional key (nullable text)

firstPresent :: [Maybe value] -> Maybe value
firstPresent = firstJust

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
