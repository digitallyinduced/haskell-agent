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
import Agent.Responses.StreamAssembly
    ( StreamAssemblyConfig
    , StreamAssemblyState
    , StreamAssemblyStep(..)
    , emptyStreamAssemblyState
    , finishStreamWithoutTerminal
    , stepStreamResponse
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
    , assemblyConfig :: !StreamAssemblyConfig
      -- ^ Incremental provider-specific stream assembly and classification.
    , responseModelHint :: !(Maybe Text)
      -- ^ Request model used when a provider sends partial lifecycle objects.
    }

data ConsumeResult
    = ConsumeMore !StreamAssemblyState
    | ConsumeDone !(Either ApiError Response)

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
    HttpSseConfig
        { exceptionPrefix
        , classifyFailure
        , assemblyConfig
        , responseModelHint
        }
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
                (body, truncated) <-
                    consumeBodyBounded (HttpClient.responseBody response)
                let decoded = Text.decodeUtf8With Text.lenientDecode body
                    classified =
                        classifyFailure status
                        (parseRetryAfterSeconds
                            (getResponseHeader "Retry-After" response))
                        decoded
                pure $ Left $
                    if truncated
                        then appendBodyTruncatedMessage classified
                        else classified

    consumeSse body = go newSseDecoder emptyStreamAssemblyState
      where
        go decoder state = do
            chunk <- readChunkWithin body
            if BS.null chunk
                then case finishSseDecoder decoder of
                    Left err -> pure (Left err)
                    Right trailing ->
                        consumeEvents state trailing >>= \case
                            ConsumeDone result -> pure result
                            ConsumeMore finalState ->
                                pure (finishStreamWithoutTerminal
                                    assemblyConfig
                                    finalState)
                else case feedSseDecoder decoder chunk of
                    Left err -> pure (Left err)
                    Right (nextDecoder, events) -> do
                        consumeEvents state events >>= \case
                            ConsumeDone result -> pure result
                            ConsumeMore nextState -> go nextDecoder nextState

    consumeEvents state = \case
        [] -> pure (ConsumeMore state)
        event : rest -> do
            emit event
            case stepStreamResponse
                    assemblyConfig
                    responseModelHint
                    state
                    event of
                StreamFinished result -> pure (ConsumeDone result)
                StreamContinue next -> consumeEvents next rest

    consumeBodyBounded body = readChunks [] 0
      where
        readChunks reversedChunks total = do
            chunk <- readChunkWithin body
            if BS.null chunk
                then pure (BS.concat (reverse reversedChunks), False)
                else
                    let remaining = maxErrorBodyBytes - total
                    in if BS.length chunk > remaining
                        then pure
                            ( BS.concat
                                (reverse
                                    (BS.take remaining chunk : reversedChunks))
                            , True
                            )
                        else if BS.length chunk == remaining
                            then do
                                next <- readChunkWithin body
                                pure
                                    ( BS.concat
                                        (reverse (chunk : reversedChunks))
                                    , not (BS.null next)
                                    )
                            else readChunks
                                (chunk : reversedChunks)
                                (total + BS.length chunk)

maxErrorBodyBytes :: Int
maxErrorBodyBytes = 1024 * 1024

appendBodyTruncatedMessage :: ApiError -> ApiError
appendBodyTruncatedMessage apiError =
    let suffix =
            "\n[response body truncated after "
                <> Text.pack (show maxErrorBodyBytes)
                <> " bytes]"
    in case apiError of
        HttpError status message -> HttpError status (message <> suffix)
        JsonDecodeError message body ->
            JsonDecodeError (message <> suffix) body
        ProviderError errorType message retryAfter ->
            ProviderError errorType (message <> suffix) retryAfter
        ConnectionError message -> ConnectionError (message <> suffix)
        CredentialError message -> CredentialError (message <> suffix)
        CredentialsExhausted{} -> apiError
