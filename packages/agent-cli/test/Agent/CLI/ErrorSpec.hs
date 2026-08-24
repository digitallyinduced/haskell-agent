module Agent.CLI.ErrorSpec (spec) where

import Agent.CLI.Error
import Agent.Error (ApiError(..), ErrorType(..))
import Control.Monad (forM_)
import qualified Data.Text as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), addUTCTime)
import Test.Hspec

spec :: Spec
spec = do
    describe "formatApiErrorAt" do
        it "renders every API error without exposing Haskell constructors" do
            forM_ sampleErrors \apiError -> do
                let rendered = formatApiErrorAt epoch apiError
                rendered `shouldNotSatisfy` Text.null
                Text.length rendered `shouldSatisfy` (<= 600)
                forM_ internalNames \name ->
                    rendered `shouldNotSatisfy` Text.isInfixOf name

        it "does not expose raw HTTP or decode response bodies" do
            formatApiErrorAt epoch
                (HttpError 502 "TOP_SECRET_HTTP_BODY")
                `shouldNotSatisfy` Text.isInfixOf "TOP_SECRET_HTTP_BODY"
            let decoded =
                    formatApiErrorAt epoch
                        (JsonDecodeError
                            "unexpected token"
                            "TOP_SECRET_RAW_PROVIDER_RESPONSE")
            decoded `shouldNotSatisfy`
                Text.isInfixOf "TOP_SECRET_RAW_PROVIDER_RESPONSE"
            decoded `shouldSatisfy`
                Text.isInfixOf "unreadable response"
            formatApiErrorAt epoch
                (ProviderError AuthenticationError
                    "TOP_SECRET_AUTH_RESPONSE"
                    Nothing)
                `shouldNotSatisfy`
                    Text.isInfixOf "TOP_SECRET_AUTH_RESPONSE"

        it "keeps connection failures neutral and preserves safe details" do
            let rendered =
                    formatApiErrorAt epoch
                        (ConnectionError
                            "automatic compaction failed: credential store is locked")
            rendered `shouldSatisfy`
                Text.isInfixOf "operation could not be completed"
            rendered `shouldSatisfy`
                Text.isInfixOf "credential store is locked"
            rendered `shouldNotSatisfy`
                Text.isInfixOf "internet connection"

        it "suppresses structured and internal connection dumps" do
            formatApiErrorAt epoch
                (ConnectionError "{\"secret\":\"raw body\"}")
                `shouldNotSatisfy` Text.isInfixOf "secret"
            formatApiErrorAt epoch
                (ConnectionError
                    "xAI request failed: HttpExceptionRequest Request {host = \"secret.example\"}")
                `shouldNotSatisfy` Text.isInfixOf "HttpExceptionRequest"
            formatApiErrorAt epoch
                (ConnectionError
                    "WebSocket error (no type): {\"secret\":\"raw event\"}")
                `shouldNotSatisfy` Text.isInfixOf "secret"
            formatApiErrorAt epoch
                (ConnectionError "{\"secret\":\"raw body\"}")
                `shouldSatisfy`
                    Text.isInfixOf "check your connection and provider configuration"

        it "preserves trusted local authentication recovery details" do
            let rendered =
                    formatApiErrorAt epoch
                        (CredentialError
                            "reloaded credential is unchanged; refresh ~/.grok/auth.json or OPENROUTER_API_KEY and retry")
            rendered `shouldSatisfy`
                Text.isInfixOf "~/.grok/auth.json"
            rendered `shouldSatisfy`
                Text.isInfixOf "OPENROUTER_API_KEY"

        it "provides actions for common provider failures" do
            formatApiErrorAt epoch
                (ProviderError AuthenticationError "bad token" Nothing)
                `shouldSatisfy` Text.isInfixOf "/login"
            let context =
                    formatApiErrorAt epoch
                        (ProviderError ContextWindowExceeded "too long" Nothing)
            context `shouldSatisfy` Text.isInfixOf "/compact"
            context `shouldSatisfy` Text.isInfixOf "/new"
            formatApiErrorAt epoch
                (ProviderError InvalidImageError "invalid image" Nothing)
                `shouldSatisfy` Text.isInfixOf "PNG or JPEG"
            formatApiErrorAt epoch
                (ProviderError PayloadTooLargeError "large" Nothing)
                `shouldSatisfy` Text.isInfixOf "/compact"
            formatApiErrorAt epoch
                (ProviderError ClientUpdateRequired "old client" Nothing)
                `shouldSatisfy` Text.isInfixOf "Update haskell-agent"
            formatApiErrorAt epoch
                (ProviderError WebSocketConnectionLimitReached "busy" Nothing)
                `shouldSatisfy` Text.isInfixOf "Close another agent session"

        it "formats provider retry intervals and credential reset times" do
            formatApiErrorAt epoch
                (ProviderError RateLimitError "slow down" (Just 121))
                `shouldSatisfy` Text.isInfixOf "Try again in 2m"
            formatApiErrorAt epoch
                (CredentialsExhausted
                    (addUTCTime (5 * 86400 + 21 * 3600) epoch))
                `shouldSatisfy` Text.isInfixOf "Try again in 5d 21h"
            formatApiError
                (CredentialsExhausted
                    (addUTCTime (5 * 86400 + 21 * 3600) epoch))
                `shouldSatisfy`
                    Text.isInfixOf "2026-08-27 21:00:00 UTC"
            formatApiError
                (CredentialsExhausted (addUTCTime 37.4 epoch))
                `shouldSatisfy`
                    Text.isInfixOf "2026-08-22 00:00:38 UTC"
            formatApiErrorAt epoch
                (CredentialsExhausted epoch)
                `shouldSatisfy` Text.isInfixOf "Try again now"
            formatApiErrorAt epoch
                (ProviderError RateLimitError "slow down" (Just 0))
                `shouldSatisfy` Text.isInfixOf "Try again now"

        it "exposes live countdown framing only for credential exhaustion" do
            formatApiErrorRetryCountdownParts
                (CredentialsExhausted (addUTCTime 60 epoch))
                `shouldBe`
                    Just
                        ( "Provider unavailable.\n\
                          \All accounts for this provider are temporarily unavailable.\n"
                        , ", or choose another provider with /model."
                        )
            formatApiErrorRetryCountdownParts
                (ProviderError RateLimitError "slow down" (Just 60))
                `shouldBe` Nothing

        it "bounds and sanitizes provider detail text" do
            let rendered =
                    formatApiErrorAt epoch
                        (ProviderError InvalidRequestError
                            (Text.replicate 400 "x")
                            Nothing)
            rendered `shouldSatisfy` Text.isInfixOf "…"
            Text.length rendered `shouldSatisfy` (<= 400)
            formatApiErrorAt epoch
                (ProviderError InvalidRequestError
                    "{\"secret\":\"raw body\"}"
                    Nothing)
                `shouldNotSatisfy` Text.isInfixOf "secret"
            formatApiErrorAt epoch
                (ProviderError InvalidRequestError
                    "bad\ESC[31m request"
                    Nothing)
                `shouldNotSatisfy` Text.isInfixOf "\ESC"

    describe "formatApiErrorInlineAt" do
        it "flattens the shared rendering for compact command surfaces" do
            let rendered =
                    formatApiErrorInlineAt epoch
                        (ProviderError AuthenticationError "bad token" Nothing)
            rendered `shouldNotSatisfy` Text.isInfixOf "\n"
            rendered `shouldSatisfy` Text.isInfixOf "/login"

    describe "formatException" do
        it "normalizes and bounds exception details" do
            let rendered =
                    formatException
                        (userError ("failed\n" <> replicate 400 'x'))
            rendered `shouldNotSatisfy` Text.isInfixOf "\n"
            Text.length rendered `shouldSatisfy` (<= 240)
            rendered `shouldSatisfy` Text.isSuffixOf "…"

sampleErrors :: [ApiError]
sampleErrors =
    [ HttpError 502 "raw body"
    , JsonDecodeError "bad JSON" "raw body"
    , CredentialError "credential file is invalid"
    , ConnectionError "socket closed"
    , CredentialsExhausted (addUTCTime 60 epoch)
    ]
        <> [ ProviderError errorType "provider detail" (Just 60)
           | errorType <- allErrorTypes
           ]

allErrorTypes :: [ErrorType]
allErrorTypes =
    [ InvalidRequestError
    , AuthenticationError
    , PermissionError
    , NotFoundError
    , PreviousResponseNotFound
    , ContextWindowExceeded
    , InvalidImageError
    , RateLimitError
    , UsageLimitReached
    , UsageBalanceExhausted
    , QuotaExceeded
    , UsageNotIncluded
    , ApiErrorType
    , OverloadedError
    , ServiceUnavailableError
    , BillingError
    , ClientUpdateRequired
    , PayloadTooLargeError
    , WebSocketConnectionLimitReached
    , CyberPolicyError
    , MisalignmentPolicyViolation
    , UnknownErrorType "future_error"
    ]

internalNames :: [Text.Text]
internalNames =
    [ "HttpError"
    , "JsonDecodeError"
    , "ProviderError"
    , "CredentialError"
    , "ConnectionError"
    , "CredentialsExhausted"
    , "InvalidRequestError"
    , "AuthenticationError"
    , "PermissionError"
    , "NotFoundError"
    , "PreviousResponseNotFound"
    , "ContextWindowExceeded"
    , "InvalidImageError"
    , "RateLimitError"
    , "UsageLimitReached"
    , "UsageBalanceExhausted"
    , "QuotaExceeded"
    , "UsageNotIncluded"
    , "ApiErrorType"
    , "OverloadedError"
    , "ServiceUnavailableError"
    , "BillingError"
    , "ClientUpdateRequired"
    , "PayloadTooLargeError"
    , "WebSocketConnectionLimitReached"
    , "CyberPolicyError"
    , "MisalignmentPolicyViolation"
    , "UnknownErrorType"
    ]

epoch :: UTCTime
epoch = UTCTime (fromGregorian 2026 8 22) 0
