-- | User-facing rendering for provider-independent API errors.
--
-- Keep this module free of terminal styling so every CLI surface—turns,
-- startup, slash commands, dashboards, and persisted sessions—can reuse the
-- same wording without exposing Haskell constructors or raw provider bodies.
module Agent.CLI.Error
    ( formatApiError
    , formatApiErrorAt
    , formatApiErrorInline
    , formatApiErrorInlineAt
    , formatApiErrorPersistedAt
    , formatApiErrorRetryCountdownParts
    , formatException
    ) where

import Agent.CLI.Duration (formatDuration)
import Agent.Error
    ( ApiError(..)
    , CredentialExhaustionReason(..)
    , ErrorType(..)
    , errorTypeText
    )
import Control.Exception.Safe (Exception, displayException)
import Data.Char (isControl, isSpace)
import Data.Containers.ListUtils (nubOrd)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime)
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime
    , utcTimeToPOSIXSeconds
    )
import Data.Time.Format (defaultTimeLocale, formatTime)

formatApiError :: ApiError -> Text
formatApiError = formatApiErrorWith Nothing RelativeRetry

formatApiErrorAt :: UTCTime -> ApiError -> Text
formatApiErrorAt now = formatApiErrorWith (Just now) RelativeRetry

formatApiErrorPersistedAt :: UTCTime -> ApiError -> Text
formatApiErrorPersistedAt now =
    formatApiErrorWith Nothing (AbsoluteRetryFrom now)

formatApiErrorInline :: ApiError -> Text
formatApiErrorInline = inline . formatApiError

formatApiErrorInlineAt :: UTCTime -> ApiError -> Text
formatApiErrorInlineAt now = inline . formatApiErrorAt now

formatErrorDetail :: Text -> Text
formatErrorDetail =
    boundText maxExceptionLength
        . cleanText

formatException :: Exception exception => exception -> Text
formatException =
    formatErrorDetail
        . Text.pack
        . displayException

data RetryPresentation
    = RelativeRetry
    | AbsoluteRetryFrom !UTCTime

formatApiErrorWith
    :: Maybe UTCTime
    -> RetryPresentation
    -> ApiError
    -> Text
formatApiErrorWith maybeNow retryPresentation = \case
    HttpError{statusCode} ->
        formatHttpError statusCode
    JsonDecodeError{} ->
        message
            "Provider returned an unreadable response."
            []
            ["Retry the message. If this continues, report the issue."]
    ProviderError{errorType, message = providerMessage, retryAfter} ->
        formatProviderError
            retryPresentation errorType providerMessage retryAfter
    CredentialError{credentialMessage} ->
        message
            "Authentication failed."
            (trustedDetails credentialMessage)
            [ "Run /login or agent-cli login to reconnect "
                <> "the provider account."
            ]
    ConnectionError{exception} ->
        let details = failureDetails exception
            action
                | null details =
                    "Retry. If this continues, check your connection "
                        <> "and provider configuration."
                | otherwise = "Review the details and retry."
        in message
            "The operation could not be completed."
            details
            [action]
    CredentialsExhausted{retryAt, exhaustionReasons} ->
        let (prefix, suffix) =
                credentialsExhaustedRetryParts exhaustionReasons
        in prefix <> credentialsRetryGuidance maybeNow retryAt <> suffix

-- | Static text around the live retry phrase for errors with an absolute
-- credential reset deadline. The TUI inserts @Try again in …@ and updates it.
formatApiErrorRetryCountdownParts :: ApiError -> Maybe (Text, Text)
formatApiErrorRetryCountdownParts = \case
    CredentialsExhausted{exhaustionReasons} ->
        Just (credentialsExhaustedRetryParts exhaustionReasons)
    _ -> Nothing

credentialsExhaustedRetryParts
    :: [CredentialExhaustionReason]
    -> (Text, Text)
credentialsExhaustedRetryParts reasons =
    ( "Provider unavailable.\n\
      \All accounts for this provider are temporarily unavailable.\n"
    , ", or choose another provider with /model."
        <> exhaustionReasonDetails reasons
    )

exhaustionReasonDetails :: [CredentialExhaustionReason] -> Text
exhaustionReasonDetails reasons =
    case nubOrd (map formatExhaustionReason reasons) of
        [] -> ""
        rendered ->
            "\nObserved account failures: "
                <> Text.intercalate "; " rendered
                <> "."

formatExhaustionReason :: CredentialExhaustionReason -> Text
formatExhaustionReason = \case
    ExhaustedByRateLimit
        { exhaustionErrorType
        , exhaustionStatusCode
        , exhaustionRetryAfter
        } ->
            "rate or usage limit"
                <> formatReasonMetadata
                    exhaustionErrorType
                    exhaustionStatusCode
                    exhaustionRetryAfter
    ExhaustedByAuthentication
        { exhaustionErrorType
        , exhaustionStatusCode
        } ->
            "authentication rejected or credential refresh failed"
                <> formatReasonMetadata
                    exhaustionErrorType
                    exhaustionStatusCode
                    Nothing

formatReasonMetadata
    :: Maybe ErrorType
    -> Maybe Int
    -> Maybe Int
    -> Text
formatReasonMetadata maybeErrorType maybeStatus maybeRetryAfter =
    case details of
        [] -> ""
        _ -> " (" <> Text.intercalate ", " details <> ")"
  where
    details =
        maybe [] (\value -> ["type " <> errorTypeText value]) maybeErrorType
            <> maybe
                []
                (\value -> ["HTTP " <> Text.pack (show value)])
                maybeStatus
            <> maybe
                []
                (\value ->
                    [ "provider retry-after "
                        <> formatDuration (fromIntegral value)
                    ])
                maybeRetryAfter

formatProviderError
    :: RetryPresentation
    -> ErrorType
    -> Text
    -> Maybe Int
    -> Text
formatProviderError retryPresentation errorType providerMessage retryAfter =
    case errorType of
        InvalidRequestError ->
            message
                "Provider rejected the request."
                (providerDetails providerMessage)
                ["Review the prompt or attachments and retry."]
        AuthenticationError ->
            message
                "Authentication failed."
                []
                [ "Run /login or agent-cli login to reconnect "
                    <> "the provider account."
                ]
        PermissionError ->
            message
                "This account cannot use the requested model or feature."
                []
                ["Choose another model with /model or reconnect with /login."]
        NotFoundError ->
            message
                "The requested model or provider resource was not found."
                (providerDetails providerMessage)
                ["Check the selected model with /model and retry."]
        PreviousResponseNotFound ->
            message
                "The provider no longer has this conversation state."
                []
                ["Retry the message so the conversation can be resent."]
        ContextWindowExceeded ->
            message
                "This conversation is too long for the selected model."
                []
                ["Run /compact or start a new conversation with /new."]
        InvalidImageError ->
            message
                "The provider could not process an attached image."
                (providerDetails providerMessage)
                ["Remove it or attach a PNG or JPEG and retry."]
        RateLimitError ->
            message
                "Rate limit reached."
                []
                [retryOr retryPresentation retryAfter
                    "Wait a moment and retry, or choose another provider with /model."]
        UsageLimitReached ->
            message
                "Usage limit reached for this account."
                []
                [retryOr retryPresentation retryAfter
                    "Check /usage or choose another provider with /model."]
        UsageBalanceExhausted ->
            message
                "This account's usage balance is exhausted."
                []
                [retryOr retryPresentation retryAfter
                    "Check /usage or choose another provider with /model."]
        QuotaExceeded ->
            message
                "This account has no remaining quota."
                []
                ["Check provider billing or choose another provider with /model."]
        UsageNotIncluded ->
            message
                "This account's plan does not include the requested model or feature."
                []
                ["Choose another model with /model or reconnect with /login."]
        ApiErrorType ->
            message
                "The provider returned an internal error."
                (providerDetails providerMessage)
                [retryOr retryPresentation retryAfter "Retry the message."]
        OverloadedError ->
            message
                "The provider is temporarily overloaded."
                []
                [retryOr retryPresentation retryAfter
                    "Retry shortly or choose another provider with /model."]
        ServiceUnavailableError ->
            message
                "The provider is temporarily unavailable."
                []
                [retryOr retryPresentation retryAfter
                    "Retry shortly or choose another provider with /model."]
        BillingError ->
            message
                "Provider billing or credits are unavailable."
                []
                ["Check provider billing or choose another provider with /model."]
        ClientUpdateRequired ->
            message
                "The provider requires a newer client version."
                []
                ["Update haskell-agent and retry."]
        PayloadTooLargeError ->
            message
                "The request is too large."
                []
                ["Remove large attachments or run /compact, then retry."]
        WebSocketConnectionLimitReached ->
            message
                "This account has too many active provider connections."
                []
                ["Close another agent session or retry in a moment."]
        CyberPolicyError ->
            message
                "The provider blocked this request under its safety policy."
                (providerDetails providerMessage)
                ["Revise the request and try again."]
        MisalignmentPolicyViolation ->
            message
                "The provider blocked this request under its safety policy."
                (providerDetails providerMessage)
                ["Revise the request and try again."]
        UnknownErrorType value ->
            message
                ("Provider error (" <> safeErrorType value <> ").")
                (providerDetails providerMessage)
                [retryOr retryPresentation retryAfter
                    "Retry the message or choose another provider with /model."]

formatHttpError :: Int -> Text
formatHttpError status =
    case status of
        400 ->
            rejected
        401 ->
            auth
        402 ->
            billing
        403 ->
            permission
        404 ->
            notFound
        408 ->
            timedOut
        409 ->
            retryable
        413 ->
            tooLarge
        422 ->
            rejected
        425 ->
            retryable
        426 ->
            updateRequired
        429 ->
            rateLimited
        _
            | status >= 500 ->
                message
                    ("Provider temporarily unavailable (HTTP "
                        <> Text.pack (show status)
                        <> ").")
                    []
                    ["Retry shortly or choose another provider with /model."]
            | otherwise ->
                message
                    ("Request failed (HTTP " <> Text.pack (show status) <> ").")
                    []
                    ["Retry the message or choose another provider with /model."]
  where
    rejected =
        message
            "Provider rejected the request."
            []
            ["Review the prompt or attachments and retry."]
    auth =
        message
            "Authentication failed."
            []
            [ "Run /login or agent-cli login to reconnect "
                <> "the provider account."
            ]
    billing =
        message
            "Provider billing or credits are unavailable."
            []
            ["Check provider billing or choose another provider with /model."]
    permission =
        message
            "This account cannot use the requested model or feature."
            []
            ["Choose another model with /model or reconnect with /login."]
    notFound =
        message
            "The requested model or provider resource was not found."
            []
            ["Check the selected model with /model and retry."]
    timedOut =
        message
            "The provider request timed out."
            []
            ["Retry the message."]
    retryable =
        message
            "The provider could not accept the request yet."
            []
            ["Retry the message."]
    tooLarge =
        message
            "The request is too large."
            []
            ["Remove large attachments or run /compact, then retry."]
    updateRequired =
        message
            "The provider requires a newer client version."
            []
            ["Update haskell-agent and retry."]
    rateLimited =
        message
            "Rate limit reached."
            []
            ["Wait a moment and retry, or choose another provider with /model."]

credentialsRetryGuidance :: Maybe UTCTime -> UTCTime -> Text
credentialsRetryGuidance maybeNow retryAt =
    case maybeNow of
        Just now
            | retryAt <= now -> "Try again now"
            | otherwise ->
                "Try again in " <> formatDuration (diffUTCTime retryAt now)
        Nothing ->
            "Try again after "
                <> formatUtcRetryTime retryAt

retryOr :: RetryPresentation -> Maybe Int -> Text -> Text
retryOr retryPresentation retryAfter fallback =
    case retryAfter of
        Just seconds
            | seconds <= 0 -> "Try again now."
            | otherwise -> case retryPresentation of
                RelativeRetry ->
                    "Try again in "
                        <> formatDuration (fromIntegral seconds)
                        <> "."
                AbsoluteRetryFrom now ->
                    "Try again after "
                        <> formatUtcRetryTime
                            (addUTCTime (fromIntegral seconds) now)
                        <> "."
        Nothing -> fallback

providerDetails :: Text -> [Text]
providerDetails raw =
    case boundedDetail raw of
        Nothing -> []
        Just detail -> ["Provider details: " <> detail]

failureDetails :: Text -> [Text]
failureDetails raw =
    case boundedDetail raw of
        Nothing -> []
        Just detail
            | looksLikeExceptionDump detail -> []
            | otherwise -> ["Details: " <> detail]

trustedDetails :: Text -> [Text]
trustedDetails raw =
    case boundedDetail raw of
        Nothing -> []
        Just detail -> ["Details: " <> detail]

boundedDetail :: Text -> Maybe Text
boundedDetail raw
    | Text.null cleaned = Nothing
    | looksStructured cleaned = Nothing
    | otherwise = Just (boundText maxDetailLength cleaned)
  where
    cleaned = cleanText raw

maxDetailLength :: Int
maxDetailLength = 200

maxExceptionLength :: Int
maxExceptionLength = 240

boundText :: Int -> Text -> Text
boundText limit value
    | Text.length value <= limit = value
    | otherwise = Text.take (limit - 1) value <> "…"

cleanText :: Text -> Text
cleanText =
    Text.unwords
        . Text.words
        . Text.map (\character -> if isControl character then ' ' else character)
        . Text.strip

safeErrorType :: Text -> Text
safeErrorType raw =
    let cleaned = cleanText raw
    in if Text.null cleaned
        then "unknown"
        else boundText 80 cleaned

looksStructured :: Text -> Bool
looksStructured value =
    case Text.uncons (Text.stripStart value) of
        Just ('{', _) -> True
        Just ('<', _) -> True
        Just ('[', rest) ->
            case Text.uncons (Text.dropWhile isSpace rest) of
                Just (next, _) -> next == '{' || next == '"' || next == '['
                Nothing -> True
        _ -> False

looksLikeExceptionDump :: Text -> Bool
looksLikeExceptionDump value =
    any (`Text.isInfixOf` value)
        [ "HttpExceptionRequest"
        , "ConnectionFailure"
        , "InvalidUrlException"
        , "SomeException"
        , "TlsException"
        , "HandshakeFailed"
        , "Network.Socket."
        , "WebSocket error (no type):"
        ]

formatUtcRetryTime :: UTCTime -> Text
formatUtcRetryTime retryAt =
    let wholeSeconds =
            ceiling (utcTimeToPOSIXSeconds retryAt) :: Integer
        rounded = posixSecondsToUTCTime (fromInteger wholeSeconds)
    in Text.pack
        (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC" rounded)

message :: Text -> [Text] -> [Text] -> Text
message title details actions =
    Text.intercalate "\n" (title : details <> actions)

inline :: Text -> Text
inline =
    Text.intercalate " "
        . filter (not . Text.null)
        . map Text.strip
        . Text.lines
