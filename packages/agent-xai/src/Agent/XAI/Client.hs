-- | HTTP client for the xAI Grok subscription proxy Responses endpoint.
module Agent.XAI.Client
    ( StreamEventCallback
    , createResponse
    , createResponseWith
    , createResponseWithPolicy
    , createResponseWithEvents
    , createResponseWithEventsPolicy
    , retryTransientXaiResultWithPolicy
    ) where

import Agent.Error
    ( ApiError(..)
    , ErrorType(..)
    )
import Agent.Responses.GenericClient
    ( ProviderClientConfig(..)
    , createResponseWithProviderPolicy
    )
import qualified Agent.Responses.HttpSSE as HttpSSE
import Agent.Responses.Types
import Agent.Provider (Credential(..), Provider(..))
import Agent.XAI.Error
    ( capacityRetryAfterSeconds
    , classifyFailure
    , isCapacityBody
    )
import Agent.XAI.Options
import Agent.XAI.Request
    ( buildRequest
    , filterXaiOrLegacyCompactionCheckpoints
    , mapModel
    )
import Agent.XAI.Stream (streamAssemblyConfig)
import Control.Retry
    ( RetryPolicyM
    , constantDelay
    , limitRetries
    , retrying
    )
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text.Encoding as Text
import Network.HTTP.Simple hiding (Response)

type StreamEventCallback = HttpSSE.StreamEventCallback

-- | Send one request using environment-derived client options.
createResponse :: Credential -> ResponseCreateParams -> IO (Either ApiError Response)
createResponse credential request = do
    options <- clientOptionsFromEnv
    createResponseWith options credential request

-- | Send one request using explicit client options.
createResponseWith
    :: ClientOptions
    -> Credential
    -> ResponseCreateParams
    -> IO (Either ApiError Response)
createResponseWith =
    createResponseWithPolicy defaultTransientPolicy

-- | Like 'createResponseWith', with an injectable retry policy for tests.
-- Because this path exposes no stream callbacks, retrying a stream-level
-- capacity failure cannot replay caller-visible output.
createResponseWithPolicy
    :: RetryPolicyM IO
    -> ClientOptions
    -> Credential
    -> ResponseCreateParams
    -> IO (Either ApiError Response)
createResponseWithPolicy policy options credential request =
    createResponseWithMaybeEventsPolicy
        policy
        options
        credential
        request
        Nothing

-- | Send one request and deliver decoded typed Responses events incrementally
-- in wire order before returning the assembled terminal response. Transient
-- capacity / overload failures wait 30s and retry a few times only before the
-- first callback, so callers never observe replayed output.
createResponseWithEvents
    :: ClientOptions
    -> Credential
    -> ResponseCreateParams
    -> StreamEventCallback
    -> IO (Either ApiError Response)
createResponseWithEvents options credential request onEvent =
    createResponseWithEventsPolicy defaultTransientPolicy options credential request onEvent

-- | Same as 'createResponseWithEvents', with an injectable retry policy so
-- tests can use a zero delay without waiting on the production 30s backoff.
createResponseWithEventsPolicy
    :: RetryPolicyM IO
    -> ClientOptions
    -> Credential
    -> ResponseCreateParams
    -> StreamEventCallback
    -> IO (Either ApiError Response)
createResponseWithEventsPolicy policy options credential request onEvent =
    createResponseWithMaybeEventsPolicy
        policy
        options
        credential
        request
        (Just onEvent)

createResponseWithMaybeEventsPolicy
    :: RetryPolicyM IO
    -> ClientOptions
    -> Credential
    -> ResponseCreateParams
    -> Maybe StreamEventCallback
    -> IO (Either ApiError Response)
createResponseWithMaybeEventsPolicy policy options credential request onEvent
    | credential.provider /= XAIProvider = pure $ Left $ ProviderError ApiErrorType
        "agent-xai requires an xAI credential"
        Nothing
    | otherwise =
        createResponseWithProviderPolicy
            policy
            (xaiProviderConfig options credential request)
            request
            onEvent

defaultTransientPolicy :: RetryPolicyM IO
defaultTransientPolicy =
    constantDelay (capacityRetryAfterSeconds * 1_000_000) <> limitRetries 3

xaiProviderConfig
    :: ClientOptions
    -> Credential
    -> ResponseCreateParams
    -> ProviderClientConfig
xaiProviderConfig options credential request = ProviderClientConfig
    { providerExceptionPrefix = "xAI request failed"
    , providerBaseUrl = options.baseUrl
    , providerRequestTimeoutSeconds = options.requestTimeoutSeconds
    , providerBuildRequest = buildRequest options
    , providerConfigureRequest =
        configureCompactionHeaders
            options
            (filterXaiOrLegacyCompactionCheckpoints request)
            . setRequestHeader "Authorization"
            ["Bearer " <> Text.encodeUtf8 credential.accessToken]
            . setRequestHeader "X-XAI-Token-Auth"
                [Text.encodeUtf8 grokTokenAuthValue]
            . setRequestHeader "x-authenticateresponse"
                [Text.encodeUtf8 grokAuthenticateResponseValue]
            . setRequestHeader "x-grok-client-version"
                [Text.encodeUtf8 options.clientVersion]
            . setRequestHeader "x-grok-client-identifier"
                [Text.encodeUtf8 grokClientIdentifier]
            . setRequestHeader "x-grok-client-mode" ["interactive"]
            . setRequestHeader "User-Agent"
                [Text.encodeUtf8 (grokUserAgent options.clientVersion)]
    , providerClassifyFailure = classifyFailure
    , providerAssemblyConfig = streamAssemblyConfig
    , providerRetryableFailure = isCapacityRetryable
    }

-- | Mirror the model metadata headers emitted by Grok Build. The fixed
-- remaining count is sent on every request. The compaction point is omitted
-- once the stateless transcript contains a checkpoint, matching Grok Build's
-- @has_compaction_summary@ gate.
configureCompactionHeaders
    :: ClientOptions
    -> ResponseCreateParams
    -> Request
    -> Request
configureCompactionHeaders options request =
    addCompactionAt . addCompactionsRemaining
  where
    wireModel =
        maybe options.defaultModel (mapModel options) request.model

    addCompactionsRemaining =
        maybe id
            (setRequestHeader "x-compactions-remaining" . pure . renderInt)
            (grokServerCompactionsRemaining wireModel)

    addCompactionAt
        | requestHasCompactionCheckpoint request = id
        | otherwise =
            maybe id
                (setRequestHeader "x-compaction-at" . pure . renderInt)
                serverCompactionAtTokens

    serverCompactionAtTokens = do
        modelDefault <- grokServerCompactionAtTokens wireModel
        pure $ max 1 $ maybe modelDefault id options.autoCompactTokenLimit

    renderInt = BS8.pack . show

requestHasCompactionCheckpoint :: ResponseCreateParams -> Bool
requestHasCompactionCheckpoint request = case request.input of
    Just (ResponseInputItems items) -> any isCompactionCheckpoint items
    _ -> False

isCompactionCheckpoint :: ResponseItem -> Bool
isCompactionCheckpoint = \case
    CompactionItemValue{} -> True
    ContextCompactionItemValue{} -> True
    KnownResponseItem ItemCompaction _ -> True
    KnownResponseItem ItemContextCompaction _ -> True
    MessageItem message
        | message.role == RoleAssistant ->
            responseMessageHasContentItemKind
                localCompactionSummaryContentItemKind
                message
    _ -> False

-- | Retry capacity / overload pressure and short-lived 5xx failures. Generic
-- connection drops and quota errors are left to the caller. Production uses a
-- 30s constant delay so capacity pressure can clear between attempts.
retryTransientXaiResultWithPolicy
    :: RetryPolicyM IO
    -> IO (Either ApiError value)
    -> IO (Either ApiError value)
retryTransientXaiResultWithPolicy policy request =
    retrying policy shouldRetry (const request)
  where
    shouldRetry _retryStatus = \case
        Left apiError | isCapacityRetryable apiError -> pure True
        _ -> pure False

-- | Capacity text is classified as 'OverloadedError' with a 30s interval.
-- Keep a ConnectionError fallback for older wrappers that still prefix the
-- same message, and retry ordinary 5xx / overload shapes the same way.
isCapacityRetryable :: ApiError -> Bool
isCapacityRetryable = \case
    ProviderError OverloadedError _ _ -> True
    ProviderError ServiceUnavailableError _ _ -> True
    ProviderError ApiErrorType _ _ -> True
    HttpError status _
        | status `elem` [408, 409, 425] -> True
        | status >= 500 && status < 600 -> True
        | otherwise -> False
    ConnectionError message -> isCapacityBody message
    _ -> False

