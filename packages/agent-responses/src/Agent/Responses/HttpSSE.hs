-- | Provider-neutral HTTP transport for streaming Responses endpoints.
module Agent.Responses.HttpSSE
    ( HttpSseConfig(..)
    , StreamEventCallback
    , performResponsesHttpSse
    ) where

import Agent.Error (ApiError(..))
import Agent.Http.Header (parseRetryAfterSeconds)
import Agent.Http.Url (trimTrailingSlash)
import qualified Agent.Json.Decoder.Hermes as Hermes
import qualified Agent.Responses.Hermes as ResponsesHermes
import Agent.Responses.SSE
    ( SseFrame(..)
    , feedSseDecoderWith
    , finishSseDecoderWith
    , newSseDecoder
    )
import Agent.Responses.Types
import Control.Exception.Safe (tryAny)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text (lenientDecode)
import qualified Network.HTTP.Client as HttpClient
import qualified Network.HTTP.Client.TLS as HttpTls
import Network.HTTP.Simple hiding (Response)

type StreamEventCallback = ResponseStreamEvent -> IO ()

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

    consumeSse body =
        -- Hermes environments are mutable parser scratch space. Scope one to
        -- this response stream rather than sharing one across concurrent
        -- requests or paying to allocate one for every SSE event.
        Hermes.withDecoderSession \session ->
            go session newSseDecoder []
      where
        go session decoder reversedEvents = do
            chunk <- HttpClient.brRead body
            if BS.null chunk
                then finishSseDecoderWith
                    (decodeFrame session)
                    decoder >>= \case
                    Left err -> pure (Left err)
                    Right trailing -> do
                        let delivered = takeThroughTerminal trailing
                        mapM_ emit delivered
                        pure $ buildResponse
                            (reverse reversedEvents
                                <> filter retainForResponse delivered)
                else feedSseDecoderWith
                    (decodeFrame session)
                    decoder
                    chunk >>= \case
                    Left err -> pure (Left err)
                    Right (nextDecoder, events) -> do
                        let delivered = takeThroughTerminal events
                        mapM_ emit delivered
                        let retained = filter retainForResponse delivered
                            allEvents = reverse retained <> reversedEvents
                        if any isTerminal delivered
                            then pure (buildResponse (reverse allEvents))
                            else go session nextDecoder allEvents

        decodeFrame session SseFrame{sseFrameEventType, sseFrameData} = do
            decoded <- case sseFrameEventType of
                Just eventType
                    | eventType == "response.output_text.delta" ->
                        fmap (fmap toOutputTextDelta) $
                            Hermes.decodeHermesIO
                                session
                                ResponsesHermes.textDeltaEventDecoder
                                sseFrameData
                    | eventType == "response.reasoning_text.delta" ->
                        fmap (fmap toReasoningTextDelta) $
                            Hermes.decodeHermesIO
                                session
                                ResponsesHermes.textDeltaEventDecoder
                                sseFrameData
                _ -> Hermes.decodeIO
                    session
                    (responseStreamEventDecoderWithType sseFrameEventType)
                    sseFrameData
            -- Malformed JSON and structurally invalid/partial events are
            -- skippable. The framer reports size and UTF-8 failures before
            -- decoding, because those do not have a safe recovery boundary.
            pure (either (const Nothing) Just decoded)

        toOutputTextDelta ResponsesHermes.TextDeltaFields{..} =
                ResponseOutputTextDeltaEvent
                    { delta
                    , streamItemId = itemId
                    , streamOutputIndex = outputIndex
                    , contentIndex
                    , logprobs
                    , sequenceNumber
                    , eventExtraFields = extensions
                    }

        toReasoningTextDelta ResponsesHermes.TextDeltaFields{..} =
                ResponseReasoningTextDeltaEvent
                    { delta
                    , streamItemId = itemId
                    , streamOutputIndex = outputIndex
                    , contentIndex
                    , sequenceNumber
                    , eventExtraFields = extensions
                    }

    consumeBody body = LBS.fromChunks <$> readChunks []
      where
        readChunks reversedChunks = do
            chunk <- HttpClient.brRead body
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
