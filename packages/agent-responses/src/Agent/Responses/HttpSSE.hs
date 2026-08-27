-- | Provider-neutral HTTP transport for streaming Responses endpoints.
module Agent.Responses.HttpSSE
    ( HttpSseConfig(..)
    , StreamEventCallback
    , performResponsesHttpSse
    ) where

import Agent.Error (ApiError(..))
import Agent.Http.Header (parseRetryAfterSeconds)
import Agent.Http.Url (trimTrailingSlash)
import Agent.Responses.SSE
    ( feedSseDecoder
    , finishSseDecoder
    , newSseDecoder
    )
import Agent.Responses.Types
import Control.Exception.Safe (Exception, throwIO, tryAny)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text (lenientDecode)
import qualified Network.HTTP.Client as HttpClient
import qualified Network.HTTP.Client.TLS as HttpTls
import Network.HTTP.Simple hiding (Response)
import qualified System.Timeout as Timeout

type StreamEventCallback = ResponseStreamEvent -> IO ()

-- | Raised when a streaming body read stalls for longer than the configured
-- timeout. http-client's responseTimeout only bounds connection setup and
-- header receipt, so without this the body reader could block forever when the
-- network path dies without a FIN/RST after headers arrived.
newtype StreamStalled = StreamStalled Int

instance Show StreamStalled where
    show (StreamStalled seconds) =
        "streaming response stalled: no data received for "
            <> show seconds <> "s"

instance Exception StreamStalled

-- | Provider-specific hooks around the shared Responses HTTP/SSE mechanics.
data HttpSseConfig = HttpSseConfig
    { exceptionPrefix :: !Text
      -- ^ Prefix used when request setup, transport, or callback code throws.
    , classifyFailure :: !(Int -> Maybe Int -> Text -> ApiError)
      -- ^ Classify a non-success status, optional @Retry-After@, and body.
    , buildResponse :: !([ResponseStreamEvent] -> Either ApiError Response)
      -- ^ Assemble retained terminal events into the provider response.
    }

-- | POST one streaming Responses request and deliver decoded events in wire
-- order. The request modifier supplies provider-specific authentication and
-- metadata headers; this function owns timeout setup, connection management,
-- failure-body collection, incremental SSE decoding, and response assembly.
performResponsesHttpSse
    :: HttpSseConfig
    -> String
    -> Int
    -> LBS.ByteString
    -> (Request -> Request)
    -> StreamEventCallback
    -> IO (Either ApiError Response)
performResponsesHttpSse
    HttpSseConfig{exceptionPrefix, classifyFailure, buildResponse}
    baseUrl
    timeoutSeconds
    requestBody
    configureRequest
    emit =
        tryAny performRequest >>= \case
            Left exception -> pure $ Left $ ConnectionError
                (exceptionPrefix <> ": " <> Text.pack (show exception))
            Right result -> pure result
  where
    performRequest = do
        baseRequest <- parseRequest
            ("POST " <> trimTrailingSlash baseUrl <> "/responses")
        manager <- HttpTls.getGlobalManager
        HttpClient.withResponse
            ( setRequestBodyLBS requestBody
            $ configureRequest
            $ setRequestHeader "Content-Type" ["application/json"]
            $ setRequestHeader "Accept" ["text/event-stream"]
            $ withTimeout baseRequest
            )
            manager
            handleResponse

    withTimeout request = request
        { HttpClient.responseTimeout =
            HttpClient.responseTimeoutMicro (timeoutSeconds * 1_000_000)
        }

    -- Bound each body read so a mid-stream stall cannot hang the turn forever.
    -- responseTimeout above only covers header receipt; this extends the same
    -- budget to the streaming body. A timeout throws StreamStalled, which the
    -- outer tryAny maps to a retryable ConnectionError. A non-positive timeout
    -- means "no timeout", matching responseTimeoutMicro's semantics.
    readChunkWithin body
        | timeoutSeconds <= 0 = HttpClient.brRead body
        | otherwise =
            Timeout.timeout (timeoutSeconds * 1_000_000)
                (HttpClient.brRead body) >>= \case
                    Just chunk -> pure chunk
                    Nothing -> throwIO (StreamStalled timeoutSeconds)

    handleResponse response = do
        let status = getResponseStatusCode response
        if status >= 200 && status < 300
            then consumeSse (HttpClient.responseBody response)
            else do
                body <- consumeBody (HttpClient.responseBody response)
                let bodyText = Text.decodeUtf8With Text.lenientDecode
                        (LBS.toStrict body)
                pure $ Left $
                    classifyFailure status
                        (parseRetryAfterSeconds
                            (getResponseHeader "Retry-After" response))
                        bodyText

    consumeSse body = go newSseDecoder []
      where
        go decoder reversedEvents = do
            chunk <- readChunkWithin body
            if BS.null chunk
                then case finishSseDecoder decoder of
                    Left err -> pure (Left err)
                    Right trailing -> do
                        let delivered = takeThroughTerminal trailing
                        mapM_ emit delivered
                        pure $ buildResponse
                            (reverse reversedEvents
                                <> filter retainForResponse delivered)
                else case feedSseDecoder decoder chunk of
                    Left err -> pure (Left err)
                    Right (nextDecoder, events) -> do
                        let delivered = takeThroughTerminal events
                        mapM_ emit delivered
                        let retained = filter retainForResponse delivered
                            allEvents = reverse retained <> reversedEvents
                        if any isTerminal delivered
                            then pure (buildResponse (reverse allEvents))
                            else go nextDecoder allEvents

    consumeBody body = LBS.fromChunks <$> readChunks []
      where
        readChunks reversedChunks = do
            chunk <- readChunkWithin body
            if BS.null chunk
                then pure (reverse reversedChunks)
                else readChunks (chunk : reversedChunks)

retainForResponse :: ResponseStreamEvent -> Bool
retainForResponse = \case
    ResponseCreatedEvent {} -> True
    ResponseInProgressEvent {} -> True
    ResponseQueuedEvent {} -> True
    ResponseOutputItemAddedEvent {} -> True
    ResponseOutputItemDoneEvent {} -> True
    ResponseCustomToolInputDeltaEvent {} -> True
    ResponseCustomToolInputDoneEvent {} -> True
    ResponseFunctionCallArgumentsDeltaEvent {} -> True
    ResponseFunctionCallArgumentsDoneEvent {} -> True
    ResponseReasoningSummaryPartAddedEvent {} -> True
    ResponseReasoningSummaryTextDoneEvent {} -> True
    event
        | responseStreamEventType event
            == EventReasoningSummaryTextDelta -> True
    ResponseCompletedEvent {} -> True
    ResponseDoneEvent {} -> True
    ResponseIncompleteEvent {} -> True
    ResponseErrorEvent {} -> True
    ResponseNestedErrorEvent {} -> True
    ResponseFailedEvent {} -> True
    _ -> False

isTerminal :: ResponseStreamEvent -> Bool
isTerminal = \case
    ResponseCompletedEvent {} -> True
    ResponseDoneEvent {} -> True
    ResponseIncompleteEvent {} -> True
    ResponseFailedEvent {} -> True
    ResponseErrorEvent {} -> True
    ResponseNestedErrorEvent {} -> True
    _ -> False

takeThroughTerminal :: [ResponseStreamEvent] -> [ResponseStreamEvent]
takeThroughTerminal = \case
    [] -> []
    event : rest
        | isTerminal event -> [event]
        | otherwise -> event : takeThroughTerminal rest
