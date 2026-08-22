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

import Agent.Http.Header (parseRetryAfterSeconds)
import Agent.Http.Url (trimTrailingSlash)
import Agent.Error
    ( ApiError(..)
    , ErrorType(..)
    )
import Agent.Responses.Types
import Agent.Provider (Credential(..), Provider(..))
import Agent.XAI.Error
    ( capacityRetryAfterSeconds
    , classifyFailure
    , isCapacityBody
    )
import Agent.XAI.Options
import Agent.XAI.Request (buildRequest)
import Agent.XAI.Stream
    ( buildResponse
    , feedSseDecoder
    , finishSseDecoder
    , newSseDecoder
    )
import Control.Exception.Safe (tryAny)
import Control.Retry
    ( RetryPolicyM
    , constantDelay
    , limitRetries
    , retrying
    )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text (lenientDecode)
import qualified Network.HTTP.Client as HttpClient
import qualified Network.HTTP.Client.TLS as HttpTls
import Network.HTTP.Simple hiding (Response)

type StreamEventCallback = ResponseStreamEvent -> IO ()

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
        retryTransientXaiStreamWithPolicy policy performOnce onEvent
  where
    performOnce emit =
        tryAny (performRequest emit) >>= \case
            Left exception -> pure $ Left $ ConnectionError
                ("xAI request failed: " <> Text.pack (show exception))
            Right result -> pure result

    performRequest emit = do
        httpRequest <- parseRequest ("POST " <> trimTrailingSlash options.baseUrl <> "/responses")
        manager <- HttpTls.getGlobalManager
        HttpClient.withResponse
            ( setRequestBodyLBS (Aeson.encode (buildRequest options request))
            $ setRequestHeader "Authorization"
                ["Bearer " <> Text.encodeUtf8 credential.accessToken]
            $ setRequestHeader "X-XAI-Token-Auth" ["xai-grok-cli"]
            $ setRequestHeader "x-grok-client-version" [Text.encodeUtf8 options.clientVersion]
            $ setRequestHeader "x-grok-client-identifier" ["grok-shell"]
            $ setRequestHeader "x-grok-client-mode" ["interactive"]
            $ setRequestHeader "Content-Type" ["application/json"]
            $ setRequestHeader "Accept" ["text/event-stream"]
            $ setRequestHeader "User-Agent" ["codex-hs"]
            $ withTimeout httpRequest
            )
            manager
            (handleResponse emit)

    withTimeout httpRequest = httpRequest
        { HttpClient.responseTimeout =
            HttpClient.responseTimeoutMicro (options.requestTimeoutSeconds * 1_000_000)
        }

    handleResponse emit response = do
        let status = getResponseStatusCode response
        if status >= 200 && status < 300
            then consumeSse emit (HttpClient.responseBody response)
            else do
                body <- consumeBody (HttpClient.responseBody response)
                let bodyText = Text.decodeUtf8With Text.lenientDecode
                        (LBS.toStrict body)
                pure $ Left $
                    classifyFailure status
                        (parseRetryAfterSeconds
                            (getResponseHeader "Retry-After" response))
                        bodyText

    consumeSse emit body = go newSseDecoder []
      where
        go decoder reversedEvents = do
            chunk <- HttpClient.brRead body
            if BS.null chunk
                then case finishSseDecoder decoder of
                    Left err -> pure (Left err)
                    Right trailing -> do
                        mapM_ emit trailing
                        pure $ buildResponse
                            (reverse reversedEvents <> trailing)
                else case feedSseDecoder decoder chunk of
                    Left err -> pure (Left err)
                    Right (nextDecoder, events) -> do
                        mapM_ emit events
                        let retained = filter retainForResponse events
                        go nextDecoder (reverse retained <> reversedEvents)

    consumeBody body = LBS.fromChunks <$> readChunks []
      where
        readChunks reversedChunks = do
            chunk <- HttpClient.brRead body
            if BS.null chunk
                then pure (reverse reversedChunks)
                else readChunks (chunk : reversedChunks)

    retainForResponse = \case
        ResponseOutputItemDoneEvent {} -> True
        ResponseCompletedEvent {} -> True
        ResponseIncompleteEvent {} -> True
        ResponseErrorEvent {} -> True
        ResponseNestedErrorEvent {} -> True
        ResponseFailedEvent {} -> True
        _ -> False

-- | Retry transient failures while replay is safe. The callback marker is
-- written before user code runs, so callback exceptions are never retried.
retryTransientXaiStreamWithPolicy
    :: RetryPolicyM IO
    -> ((event -> IO ()) -> IO (Either ApiError value))
    -> Maybe (event -> IO ())
    -> IO (Either ApiError value)
retryTransientXaiStreamWithPolicy policy request onEvent =
    snd <$> retrying policy shouldRetry runAttempt
  where
    runAttempt _status = do
        emitted <- newIORef False
        result <- request \event -> case onEvent of
            Nothing -> pure ()
            Just callback -> do
                writeIORef emitted True
                callback event
        didEmit <- readIORef emitted
        pure (didEmit, result)

    shouldRetry _status (emitted, result) = pure $
        not emitted
            && case result of
                Left apiError -> isCapacityRetryable apiError
                Right _ -> False

defaultTransientPolicy :: RetryPolicyM IO
defaultTransientPolicy =
    constantDelay (capacityRetryAfterSeconds * 1_000_000) <> limitRetries 3

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

