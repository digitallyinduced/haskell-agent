-- | HTTP client for the OpenRouter Responses endpoint.
module Agent.OpenRouter.Client
    ( StreamEventCallback
    , createResponse
    , createResponseWith
    , createResponseWithEvents
    , createResponseWithEventsPolicy
    , retryTransientOpenRouterResultWithPolicy
    ) where

import Agent.Http.Header (parseRetryAfterSeconds)
import Agent.Http.Url (trimTrailingSlash)
import Agent.Error
    ( ApiError(..)
    , ErrorType(..)
    , isInlineRetryableProviderError
    )
import Agent.Responses.Types
import Agent.Provider (Credential(..), Provider(..))
import Agent.OpenRouter.Error (classifyFailure)
import Agent.OpenRouter.Options
import Agent.OpenRouter.Request (buildRequest)
import Agent.OpenRouter.Stream
    ( buildResponse
    , feedSseDecoder
    , finishSseDecoder
    , newSseDecoder
    )
import Control.Exception.Safe (tryAny)
import Control.Retry
    ( RetryPolicyM
    , exponentialBackoff
    , limitRetries
    , retrying
    )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text (lenientDecode)
import qualified Network.HTTP.Client as HttpClient
import qualified Network.HTTP.Client.TLS as HttpTls
import Network.HTTP.Simple hiding (Response)
import Network.HTTP.Types (HeaderName)

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
createResponseWith options credential request =
    createResponseWithEvents options credential request (const (pure ()))

-- | Send one request and deliver decoded typed Responses events incrementally
-- in wire order before returning the assembled terminal response.
createResponseWithEvents
    :: ClientOptions
    -> Credential
    -> ResponseCreateParams
    -> StreamEventCallback
    -> IO (Either ApiError Response)
createResponseWithEvents =
    createResponseWithEventsPolicy transientResultPolicy

-- | Like 'createResponseWithEvents', with an injectable retry policy for
-- deterministic tests. Transient failures retry only before the first stream
-- callback, so callers never observe replayed output.
createResponseWithEventsPolicy
    :: RetryPolicyM IO
    -> ClientOptions
    -> Credential
    -> ResponseCreateParams
    -> StreamEventCallback
    -> IO (Either ApiError Response)
createResponseWithEventsPolicy policy options credential request onEvent
    | credential.provider /= OpenRouterProvider = pure $ Left $ ProviderError ApiErrorType
        "agent-openrouter requires an OpenRouter credential"
        Nothing
    | otherwise =
        retryTransientOpenRouterResultWithPolicy policy performOnce onEvent
  where
    performOnce emit =
        tryAny (performRequest emit) >>= \case
            Left exception -> pure $ Left $ ConnectionError
                ("OpenRouter request failed: " <> Text.pack (show exception))
            Right result -> pure result

    performRequest emit = do
        httpRequest <- parseRequest ("POST " <> trimTrailingSlash options.baseUrl <> "/responses")
        manager <- HttpTls.getGlobalManager
        HttpClient.withResponse
            ( setRequestBodyLBS (Aeson.encode (buildRequest options request))
            $ setRequestHeader "Authorization"
                ["Bearer " <> Text.encodeUtf8 credential.accessToken]
            $ setRequestHeader "Content-Type" ["application/json"]
            $ setRequestHeader "Accept" ["text/event-stream"]
            $ setRequestHeader "User-Agent" ["haskell-agent"]
            $ optionalHeader "HTTP-Referer" options.httpReferer
            $ optionalHeader "X-Title" options.appTitle
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
retryTransientOpenRouterResultWithPolicy
    :: RetryPolicyM IO
    -> ((event -> IO ()) -> IO (Either ApiError value))
    -> (event -> IO ())
    -> IO (Either ApiError value)
retryTransientOpenRouterResultWithPolicy policy request onEvent =
    snd <$> retrying policy shouldRetry runAttempt
  where
    runAttempt _status = do
        emitted <- newIORef False
        result <- request \event -> do
            writeIORef emitted True
            onEvent event
        didEmit <- readIORef emitted
        pure (didEmit, result)

    shouldRetry _status (emitted, result) = pure $
        not emitted
            && case result of
                Left apiError -> isInlineRetryableProviderError apiError
                Right _ -> False

transientResultPolicy :: RetryPolicyM IO
transientResultPolicy = exponentialBackoff 1_000_000 <> limitRetries 3

optionalHeader :: HeaderName -> Maybe Text -> Request -> Request
optionalHeader name value request = case nonEmptyText value of
    Just text -> setRequestHeader name [Text.encodeUtf8 text] request
    Nothing -> request

nonEmptyText :: Maybe Text -> Maybe Text
nonEmptyText (Just value) | not (Text.null (Text.strip value)) = Just value
nonEmptyText _ = Nothing

