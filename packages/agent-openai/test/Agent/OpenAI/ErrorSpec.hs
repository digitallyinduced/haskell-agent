module Agent.OpenAI.ErrorSpec (spec) where

import Agent.Error
import Agent.OpenAI.Error
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..))
import Test.Hspec

spec :: Spec
spec = do
    describe "ApiError JSON parsing" do
        it "normalizes previous_response_not_found codes to a typed error" do
            decodeOpenAIError "{\"error\":{\"type\":\"invalid_request_error\",\"code\":\"previous_response_not_found\",\"message\":\"Previous response with id 'resp_123' not found.\"}}"
                `shouldBe` Right
                    (ProviderError PreviousResponseNotFound "Previous response with id 'resp_123' not found. (code: previous_response_not_found)" Nothing)

        it "normalizes response_not_found codes on continuation carriers" do
            mkOpenAIError NotFoundError "Previous response was evicted" (Just "response_not_found") Nothing
                `shouldBe` ProviderError PreviousResponseNotFound "Previous response was evicted (code: response_not_found)" Nothing

        it "parses service_unavailable_error as a typed transient failure" do
            classifyHttpFailure 503 "{\"error\":{\"type\":\"service_unavailable_error\",\"message\":\"Service temporarily unavailable\"}}"
                `shouldBe` ProviderError ServiceUnavailableError
                    "Service temporarily unavailable"
                    Nothing

            isRetryable
                (ProviderError ServiceUnavailableError "Service temporarily unavailable" Nothing)
                `shouldBe` True

    describe "isPreviousResponseIdError" do
        it "detects typed previous-response failures" do
            isPreviousResponseIdError
                (ProviderError PreviousResponseNotFound "previous_response_id was not found" Nothing)
                `shouldBe` True

        it "detects explicit previous_response_id failures" do
            isPreviousResponseIdError
                (ProviderError InvalidRequestError "previous_response_id was not found" Nothing)
                `shouldBe` True

        it "detects response_not_found cache misses" do
            isPreviousResponseIdError
                (ProviderError NotFoundError "code: response_not_found" Nothing)
                `shouldBe` True

        it "detects previous response wording in connection errors" do
            isPreviousResponseIdError
                (ConnectionError "previous response not found")
                `shouldBe` True

        it "does not treat unrelated retryable errors as continuation misses" do
            isPreviousResponseIdError
                (ProviderError RateLimitError "usage window exhausted" (Just 60))
                `shouldBe` False
            isPreviousResponseIdError (CredentialsExhausted epoch) `shouldBe` False

    describe "isResponseChainCompatibilityError" do
        it "detects unsupported inherited prompt cache retention" do
            isResponseChainCompatibilityError
                (ProviderError
                    (UnknownErrorType "invalid_parameter")
                    "prompt_cache_retention is not supported on this model \
                    \(code: invalid_parameter)"
                    Nothing)
                `shouldBe` True

        it "does not classify unrelated invalid parameters as chain errors" do
            isResponseChainCompatibilityError
                (ProviderError
                    (UnknownErrorType "invalid_parameter")
                    "temperature is not supported on this model"
                    Nothing)
                `shouldBe` False

    describe "classifyHttpFailure" do
        it "normalizes the real server_is_overloaded response code" do
            classifyHttpFailure 500 "{\"error\":{\"type\":\"server_error\",\"code\":\"server_is_overloaded\",\"message\":\"Our servers are currently overloaded\"}}"
                `shouldBe` ProviderError OverloadedError
                    "Our servers are currently overloaded (code: server_is_overloaded)"
                    Nothing

        it "parses a usage_limit_reached 429 body into a typed error with resets_in_seconds" do
            -- Real prod payload shape (2026-07-10): without this classification
            -- the account pool only applied its ~60s default cooldown to an
            -- account whose quota window resets ~1h later.
            classifyHttpFailure 429 "{\"error\":{\"type\":\"usage_limit_reached\",\"message\":\"The usage limit has been reached\",\"plan_type\":\"pro\",\"resets_at\":1783651184,\"eligible_promo\":null,\"resets_in_seconds\":3505}}"
                `shouldBe` ProviderError UsageLimitReached "The usage limit has been reached" (Just 3505)

        it "parses a rate_limit_error body into a typed error" do
            classifyHttpFailure 429 "{\"error\":{\"type\":\"rate_limit_error\",\"message\":\"slow down\"}}"
                `shouldBe` ProviderError RateLimitError "slow down" Nothing

        it "classifies Codex response error codes exhaustively" do
            let classify code =
                    mkOpenAIError ApiErrorType "failed" (Just code) Nothing
            classify "context_length_exceeded"
                `shouldBe` ProviderError ContextWindowExceeded
                    "failed (code: context_length_exceeded)" Nothing
            classify "invalid_image"
                `shouldBe` ProviderError InvalidImageError
                    "failed (code: invalid_image)" Nothing
            classify "insufficient_quota"
                `shouldBe` ProviderError QuotaExceeded
                    "failed (code: insufficient_quota)" Nothing
            classify "usage_not_included"
                `shouldBe` ProviderError UsageNotIncluded
                    "failed (code: usage_not_included)" Nothing
            classify "cyber_policy"
                `shouldBe` ProviderError CyberPolicyError
                    "failed (code: cyber_policy)" Nothing
            classify "misalignment_policy_violation"
                `shouldBe` ProviderError MisalignmentPolicyViolation
                    "failed (code: misalignment_policy_violation)" Nothing

        it "parses flat provider bodies and prose retry delays" do
            classifyHttpFailure 400
                "{\"code\":\"invalid_image\",\"error\":\"Invalid PNG image.\"}"
                `shouldBe` ProviderError InvalidImageError
                    "Invalid PNG image. (code: invalid_image)"
                    Nothing
            classifyHttpFailure 429
                "{\"error\":{\"code\":\"rate_limit_exceeded\",\"message\":\"Rate limit exceeded. Please try again in 1.898s.\"}}"
                `shouldBe` ProviderError RateLimitError
                    "Rate limit exceeded. Please try again in 1.898s. (code: rate_limit_exceeded)"
                    (Just 2)
            classifyHttpFailure 429 "{\"type\":\"error\",\"error\":\"slow down\"}"
                `shouldBe` ProviderError RateLimitError "slow down" Nothing

        it "falls back to HttpError for non-JSON bodies" do
            classifyHttpFailure 502 "Bad Gateway"
                `shouldBe` HttpError 502 "Bad Gateway"

        it "types JSON detail bodies using the HTTP status" do
            classifyHttpFailure 429 "{\"detail\":\"too many requests\"}"
                `shouldBe` ProviderError RateLimitError "too many requests" Nothing

    describe "isInlineRetryableProviderError" do
        it "retries overload, server, connection, and transient HTTP failures" do
            isInlineRetryableProviderError
                (ProviderError OverloadedError "overloaded" Nothing) `shouldBe` True
            isInlineRetryableProviderError
                (ProviderError ServiceUnavailableError "unavailable" Nothing) `shouldBe` True
            isInlineRetryableProviderError
                (ProviderError ApiErrorType "server error" Nothing) `shouldBe` True
            isInlineRetryableProviderError (ConnectionError "socket closed")
                `shouldBe` True
            map (\status -> isInlineRetryableProviderError (HttpError status "transient"))
                [408, 409, 425]
                `shouldBe` [True, True, True]
            isInlineRetryableProviderError (HttpError 503 "unavailable")
                `shouldBe` True

        it "leaves quota, rate-limit, auth, and request errors to callers" do
            isInlineRetryableProviderError
                (ProviderError UsageLimitReached "quota" (Just 3600)) `shouldBe` False
            isInlineRetryableProviderError
                (ProviderError RateLimitError "slow down" (Just 60)) `shouldBe` False
            isInlineRetryableProviderError (HttpError 429 "slow down")
                `shouldBe` False
            isInlineRetryableProviderError
                (ProviderError AuthenticationError "bad auth" Nothing) `shouldBe` False
            isInlineRetryableProviderError
                (ProviderError InvalidRequestError "bad request" Nothing) `shouldBe` False

        it "does not retry a dead handle as a WebSocket provider response" do
            isInlineRetryableProviderResponseError
                (ProviderError OverloadedError "overloaded" Nothing) `shouldBe` True
            isInlineRetryableProviderResponseError
                (ProviderError ServiceUnavailableError "unavailable" Nothing) `shouldBe` True
            isInlineRetryableProviderResponseError
                (ConnectionError "socket closed") `shouldBe` False
            isInlineRetryableProviderResponseError
                (ProviderError WebSocketConnectionLimitReached
                    "too many websocket connections" Nothing) `shouldBe` False

epoch :: UTCTime
epoch = UTCTime (fromGregorian 1970 1 1) 0
