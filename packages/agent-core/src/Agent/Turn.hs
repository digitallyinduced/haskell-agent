{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, OverloadedRecordDot, BlockArguments, NumericUnderscores, LambdaCase #-}
-- | Execute one model turn. Waiting and retries happen here, before a caller
-- can observe the final response or execute any of its tools. Transports use
-- this at their submission boundary, before any successful response effects.
module Agent.Turn
    ( runTurn
    , runTurnWith
    , TurnRuntime (..)
    , defaultTurnRuntime
    , turnErrorIsRetryable
    , transientApiError
    , turnErrorIsOverload
    , requestRetryLimit
    , requestRetryDelayMicros
    ) where

import Prelude
import Agent.Error (ApiError (..), ErrorType (..), apiErrorRetryAfter)
import qualified Agent.Error as Error
import Control.Concurrent (threadDelay)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.IO (stderr)

-- | Injectable effects for deterministic tests. The retry policy itself is
-- shared by all callers, rather than configured by individual agents.
data TurnRuntime = TurnRuntime
    { waitForRetry :: Int -> IO ()
    , logRetry :: Text -> IO ()
    }

defaultTurnRuntime :: TurnRuntime
defaultTurnRuntime = TurnRuntime threadDelay (Text.hPutStrLn stderr)

runTurn :: IO (Either ApiError result) -> IO (Either ApiError result)
runTurn = runTurnWith defaultTurnRuntime

-- | Only the model submission is repeated. Successful responses, tool
-- execution, accounting writes and message delivery stay outside this scope.
-- Cancellation interrupts both the submission and its wait normally.
runTurnWith
    :: TurnRuntime
    -> IO (Either ApiError result)
    -> IO (Either ApiError result)
runTurnWith runtime submit = attempt 0
  where
    attempt retries = do
        result <- submit
        case result of
            Left err | turnErrorIsRetryable err ->
                if retries >= requestRetryLimit
                    then do
                        runtime.logRetry ("ModelTurn: retry budget exhausted: " <> Text.pack (show err) :: Text)
                        pure result
                    else do
                        let next = retries + 1
                            delay = requestRetryDelayMicros err next
                        runtime.logRetry ("ModelTurn: waiting before retry " <> Text.pack (show next)
                            <> ": " <> Text.pack (show err) :: Text)
                        runtime.waitForRetry delay
                        attempt next
            _ -> pure result

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
    case apiErrorRetryAfter err of
        Just seconds | seconds > 0 -> min seconds 60 * 1_000_000
        _ -> min 60 (5 * 2 ^ min 4 (max 0 (attempt - 1))) * 1_000_000

