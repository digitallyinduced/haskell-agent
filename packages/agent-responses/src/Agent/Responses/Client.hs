-- | Shared execution and retry machinery for Responses-compatible clients.
module Agent.Responses.Client
    ( ResponsesClientConfig(..)
    , performResponsesRequest
    , retryStreamingResultWithPolicy
    ) where

import Agent.Error (ApiError)
import Agent.Retry
    ( AttemptObservation(..)
    , AttemptOutcome(..)
    , runObservedAttempt
    )
import qualified Agent.Responses.HttpSSE as HttpSSE
import Agent.Responses.Types
import Control.Retry (RetryPolicyM, retrying)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Network.HTTP.Simple (Request)

data ResponsesClientConfig = ResponsesClientConfig
    { clientExceptionPrefix :: !Text
    , clientBaseUrl :: !String
    , clientTimeoutSeconds :: !Int
    , clientClassifyFailure :: !(Int -> Maybe Int -> Text -> ApiError)
    , clientBuildResponse
        :: !([ResponseStreamEvent] -> Either ApiError Response)
    }

-- | Execute one streaming Responses request with provider-specific request
-- projection, headers, and error classification supplied by the caller.
performResponsesRequest
    :: ResponsesClientConfig
    -> ResponseCreateParams
    -> (Request -> Request)
    -> HttpSSE.StreamEventCallback
    -> IO (Either ApiError Response)
performResponsesRequest config request configureRequest =
    HttpSSE.performResponsesHttpSse
        HttpSSE.HttpSseConfig
            { exceptionPrefix = config.clientExceptionPrefix
            , classifyFailure = config.clientClassifyFailure
            , buildResponse = config.clientBuildResponse
            }
        config.clientBaseUrl
        config.clientTimeoutSeconds
        (Aeson.encode request)
        configureRequest

-- | Retry a streaming operation only while replay remains invisible to its
-- caller. The emitted marker is written before invoking user code, so callback
-- exceptions are never retried.
--
-- A missing callback represents a caller that intentionally discards the
-- stream. Such events remain replay-safe and therefore do not set the marker.
retryStreamingResultWithPolicy
    :: RetryPolicyM IO
    -> (ApiError -> Bool)
    -> ((event -> IO ()) -> IO (Either ApiError value))
    -> Maybe (event -> IO ())
    -> IO (Either ApiError value)
retryStreamingResultWithPolicy policy retryable request onEvent =
    snd <$> retrying policy shouldRetry runAttempt
  where
    runAttempt _status = do
        AttemptOutcome{attemptObservation, attemptResult} <-
            case onEvent of
                Nothing ->
                    AttemptOutcome NoOutputObserved
                        <$> request (const (pure ()))
                Just callback ->
                    runObservedAttempt (const True) callback request
        pure (attemptObservation, attemptResult)

    shouldRetry _status (observation, result) = pure $
        observation == NoOutputObserved
            && case result of
                Left apiError -> retryable apiError
                Right _ -> False
