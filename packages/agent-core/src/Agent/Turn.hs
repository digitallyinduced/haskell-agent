{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, OverloadedRecordDot, BlockArguments, NumericUnderscores, LambdaCase #-}
-- | Execute one model turn. Waiting and retries happen here, before a caller
-- can observe the final response or execute any of its tools. Transports use
-- this at their submission boundary, before any successful response effects.
module Agent.Turn
    ( runTurn
    , runTurnWithPolicy
    , turnRetryPolicy
    , turnErrorIsRetryable
    , transientApiError
    , turnErrorIsOverload
    , requestRetryLimit
    , requestRetryDelayMicros
    ) where

import Prelude
import Agent.Error (ApiError (..), ErrorType (..), apiErrorRetryAfter)
import qualified Agent.Error as Error
import Control.Monad (when)
import Control.Retry
    ( RetryAction (..), RetryPolicyM, RetryStatus (..)
    , capDelay, defaultRetryStatus, exponentialBackoff, getRetryPolicyM
    , limitRetries, retryingDynamic
    )
import Data.Functor.Identity (runIdentity)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.IO (stderr)

runTurn :: IO (Either ApiError result) -> IO (Either ApiError result)
runTurn = runTurnWithPolicy turnRetryPolicy

-- | The shared submission budget. Dynamic provider hints may override its
-- delay, but 'retryingDynamic' always respects the policy's stopping decision.
turnRetryPolicy :: RetryPolicyM IO
turnRetryPolicy = limitRetries requestRetryLimit <> requestRetryBackoff

requestRetryBackoff :: Monad m => RetryPolicyM m
requestRetryBackoff = capDelay 60_000_000 (exponentialBackoff 5_000_000)

-- | Only the model submission is repeated. Successful responses, tool
-- execution, accounting writes and message delivery stay outside this scope.
-- Cancellation interrupts both the submission and its wait normally.
runTurnWithPolicy
    :: RetryPolicyM IO
    -> IO (Either ApiError result)
    -> IO (Either ApiError result)
runTurnWithPolicy policy submit = do
    result <- retryingDynamic policy shouldRetry \status -> do
        when (status.rsIterNumber > 0) $
            Text.hPutStrLn stderr ("ModelTurn: retrying submission (attempt "
                <> Text.pack (show status.rsIterNumber) <> ")" :: Text)
        submit
    case result of
        Left err | turnErrorIsRetryable err ->
            Text.hPutStrLn stderr ("ModelTurn: retry budget exhausted: " <> Text.pack (show err) :: Text)
        _ -> pure ()
    pure result
  where
    shouldRetry _ (Left err) | turnErrorIsRetryable err =
        pure (maybe ConsultPolicy ConsultPolicyOverrideDelay (retryAfterMicros err))
    shouldRetry _ _ = pure DontRetry

transientApiError :: ApiError -> Bool
transientApiError = \case
    ConnectionError _ -> True
    HttpError statusCode _ ->
        statusCode `elem` [408, 409, 425, 429] || (statusCode >= 500 && statusCode < 600)
    ProviderError AuthenticationError _ _ -> False
    ProviderError InvalidRequestError _ _ -> False
    ProviderError (UnknownErrorType errorType) message _ ->
        let diagnostic = Text.toCaseFold (errorType <> " " <> message)
        in Text.toCaseFold errorType
                `elem` (["server_error", "internal_error", "service_unavailable_error"] :: [Text])
            || "server_is_overloaded" `Text.isInfixOf` diagnostic
            || "overloaded" `Text.isInfixOf` diagnostic
    apiError -> Error.isRetryable apiError

-- | Retry a rejected turn while preserving the caller's conversation scope.
-- RateLimitError also represents temporary provider capacity exhaustion
-- (turn_rate_limit_exceeded), so try bounded backoff before account failover.
-- Broken connections and account quotas still reach their dedicated handlers.
turnErrorIsRetryable :: ApiError -> Bool
turnErrorIsRetryable = \case
    ConnectionError _ -> False
    CredentialsExhausted {} -> False
    ProviderError UsageLimitReached _ _ -> False
    ProviderError UsageBalanceExhausted _ _ -> False
    ProviderError WebSocketConnectionLimitReached _ _ -> False
    err -> transientApiError err

requestRetryLimit :: Int
requestRetryLimit = 3

turnErrorIsOverload :: ApiError -> Bool
turnErrorIsOverload = \case
    ProviderError OverloadedError _ _ -> True
    ProviderError (UnknownErrorType errorType) message _ ->
        let diagnostic = Text.toCaseFold (errorType <> " " <> message)
        in "server_is_overloaded" `Text.isInfixOf` diagnostic
            || "overloaded" `Text.isInfixOf` diagnostic
    _ -> False

-- | Honour a positive server hint; otherwise wait 5, 10 and 20 seconds.
-- Bound individual waits at a minute. Longer incidents reach durable retries.
requestRetryDelayMicros :: ApiError -> Int -> Int
requestRetryDelayMicros err attempt =
    fromMaybe backoff (retryAfterMicros err)
  where
    -- At index four the capped policy has reached its maximum delay. Clamp
    -- lookups beyond it so exponentialBackoff cannot overflow on a large index.
    status = defaultRetryStatus { rsIterNumber = min 4 (max 1 attempt - 1) }
    backoff = fromMaybe 0 (runIdentity (getRetryPolicyM requestRetryBackoff status))

retryAfterMicros :: ApiError -> Maybe Int
retryAfterMicros err = case apiErrorRetryAfter err of
    Just seconds | seconds > 0 -> Just (min seconds 60 * 1_000_000)
    _ -> Nothing

