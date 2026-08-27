-- | OpenAI error-envelope decoding and Responses-specific normalization.
module Agent.Responses.Error
    ( decodeOpenAIError
    , mkOpenAIError
    , classifyHttpFailure
    , isPreviousResponseIdError
    , isResponseChainCompatibilityError
    ) where

import Agent.Error
import qualified Agent.Json.Decoder as Decoder
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
decodeProviderErrorPayload body =
    case Decoder.decode providerErrorValueDecoder (Text.encodeUtf8 body) of
        Left err -> Left (Text.unpack (Decoder.renderDecodeError err))
        Right Nothing -> Left "JSON body contains no provider error"
        Right (Just payload) -> Right payload

data ErrorField
    = ErrorFieldAbsent
    | ErrorFieldText !Text
    | ErrorFieldPayload !(Maybe ProviderErrorPayload)

data ErrorObjectState = ErrorObjectState
    { stateType :: !(Maybe Text)
    , stateCode :: !(Maybe Text)
    , stateErrorCode :: !(Maybe Text)
    , stateMessage :: !(Maybe Text)
    , stateDetail :: !(Maybe Text)
    , stateMsg :: !(Maybe Text)
    , stateDescription :: !(Maybe Text)
    , stateError :: !ErrorField
    , stateRetryAfter :: !(Maybe Int)
    , stateResetsInSeconds :: !(Maybe Int)
    }

emptyErrorObjectState :: ErrorObjectState
emptyErrorObjectState = ErrorObjectState
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    ErrorFieldAbsent Nothing Nothing

providerErrorValueDecoder
    :: Decoder.Decoder (Maybe ProviderErrorPayload)
providerErrorValueDecoder =
    Decoder.byType \case
        Decoder.JsonString ->
            Decoder.mapDecoder payloadFromMessage Decoder.text
        Decoder.JsonArray ->
            Decoder.mapDecoder firstJust
                (Decoder.array providerErrorValueDecoder)
        Decoder.JsonObject -> providerErrorObjectDecoder
        _ -> Nothing <$ Decoder.skip
  where
    payloadFromMessage message
        | Text.null (Text.strip message) = Nothing
        | otherwise = Just ProviderErrorPayload
            { payloadType = Nothing
            , payloadCode = Nothing
            , payloadMessage = message
            , payloadRetryAfter = retryAfterFromMessage message
            }

providerErrorObjectDecoder
    :: Decoder.Decoder (Maybe ProviderErrorPayload)
providerErrorObjectDecoder =
    providerErrorObjectDecoderWith False

providerErrorObjectDecoderWith
    :: Bool
    -> Decoder.Decoder (Maybe ProviderErrorPayload)
providerErrorObjectDecoderWith allowTypeMessage =
    Decoder.object
        emptyErrorObjectState
        [ textField "type" \value state ->
            state { stateType = Just value }
        , scalarField "code" \value state ->
            state { stateCode = Just value }
        , scalarField "error_code" \value state ->
            state { stateErrorCode = Just value }
        , textField "message" \value state ->
            state { stateMessage = Just value }
        , textField "detail" \value state ->
            state { stateDetail = Just value }
        , textField "msg" \value state ->
            state { stateMsg = Just value }
        , textField "description" \value state ->
            state { stateDescription = Just value }
        , Decoder.field "error" errorFieldDecoder \value state ->
            Right state { stateError = value }
        , retryField "retry_after" \value state ->
            state { stateRetryAfter = Just value }
        , retryField "resets_in_seconds" \value state ->
            state { stateResetsInSeconds = Just value }
        ]
        (Decoder.unknownField Decoder.skip
            \_ () state -> Right state)
        (Right . finish)
  where
    textField name update =
        Decoder.field name tolerantTextDecoder \value state ->
            Right (maybe state (`update` state) value)
    scalarField name update =
        Decoder.field name tolerantScalarTextDecoder \value state ->
            Right (maybe state (`update` state) value)
    retryField name update =
        Decoder.field name tolerantRetryAfterDecoder \value state ->
            Right (maybe state (`update` state) value)

    finish state =
        case state.stateError of
            ErrorFieldPayload (Just nested) ->
                Just nested
                    { payloadType =
                        nested.payloadType
                            `orElse` normalizedProviderType state.stateType
                    , payloadCode =
                        nested.payloadCode
                            `orElse` state.stateCode
                            `orElse` state.stateErrorCode
                    , payloadRetryAfter =
                        nested.payloadRetryAfter
                            `orElse` retry state
                    }
            ErrorFieldText message ->
                payload state message
            _ ->
                firstJust
                    [ state.stateMessage
                    , state.stateDetail
                    , state.stateMsg
                    , state.stateDescription
                    , if allowTypeMessage
                        then state.stateType
                        else Nothing
                    ]
                    >>= payload state

    retry state =
        state.stateResetsInSeconds `orElse` state.stateRetryAfter
    payload state message
        | Text.null (Text.strip message) = Nothing
        | otherwise = Just ProviderErrorPayload
            { payloadType = normalizedProviderType state.stateType
            , payloadCode =
                state.stateCode `orElse` state.stateErrorCode
            , payloadMessage = message
            , payloadRetryAfter =
                retry state `orElse` retryAfterFromMessage message
            }

errorFieldDecoder :: Decoder.Decoder ErrorField
errorFieldDecoder =
    Decoder.byType \case
        Decoder.JsonString ->
            ErrorFieldText <$> Decoder.text
        Decoder.JsonObject ->
            ErrorFieldPayload
                <$> providerErrorObjectDecoderWith True
        Decoder.JsonArray ->
            ErrorFieldPayload
                . firstJust
                <$> Decoder.array providerErrorValueDecoder
        _ -> ErrorFieldAbsent <$ Decoder.skip

tolerantScalarTextDecoder :: Decoder.Decoder (Maybe Text)
tolerantScalarTextDecoder =
    Decoder.byType \case
        Decoder.JsonString ->
            (\value ->
                if Text.null (Text.strip value)
                    then Nothing
                    else Just value)
                <$> Decoder.text
        Decoder.JsonNumber ->
            Just . Text.pack . show <$> Decoder.scientific
        Decoder.JsonBoolean ->
            Just . (\value -> if value then "true" else "false")
                <$> Decoder.bool
        _ -> Nothing <$ Decoder.skip

tolerantTextDecoder :: Decoder.Decoder (Maybe Text)
tolerantTextDecoder =
    Decoder.byType \case
        Decoder.JsonString -> Just <$> Decoder.text
        _ -> Nothing <$ Decoder.skip

tolerantRetryAfterDecoder :: Decoder.Decoder (Maybe Int)
tolerantRetryAfterDecoder =
    Decoder.byType \case
        Decoder.JsonNumber ->
            (\value -> max 1 <$> toBoundedInteger value)
                <$> Decoder.scientific
        Decoder.JsonString ->
            (\value -> case reads (Text.unpack value) of
                [(seconds, "")] -> Just (max 1 seconds)
                _ -> Nothing)
                <$> Decoder.text
        _ -> Nothing <$ Decoder.skip

normalizedProviderType :: Maybe Text -> Maybe Text
normalizedProviderType = \case
    Just value
        | Text.toLower value
            `elem` ["error", "errors", "unknown", "unknown_error"] ->
                Nothing
        | otherwise -> Just value
    Nothing -> Nothing

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
-- no longer accepts it. Replaying the local transcript without the continuation
-- id creates a fresh chain and is safe before any model output is observed.
isResponseChainCompatibilityError :: ApiError -> Bool
isResponseChainCompatibilityError error =
    isPreviousResponseIdError error
        || apiErrorTextMatches mentionsUnsupportedPromptCacheRetention error

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

orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just value) _ = Just value
orElse Nothing fallback = fallback
