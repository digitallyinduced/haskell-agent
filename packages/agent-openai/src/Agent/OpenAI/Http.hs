module Agent.OpenAI.Http
    ( postCodexJson
    , decodeCodexHttpBody
    , decodeCodexHttpBodyWithModel
    , decodeCodexHttpBodyBytes
    , decodeCodexHttpBodyBytesWithModel
    , stepCodexStreamResponse
    , finishCodexStreamResponse
    , rejectFailedCodexResponse
    ) where

import Agent.Error (ApiError(..), ErrorType(..), errorTypeFromText)
import qualified Agent.Json.Decode as Json
import Agent.OpenAI.Error (mkOpenAIError)
import Agent.Responses.Codec (decodeResponse)
import Agent.Responses.SSE (parseSseEventsBytes)
import Agent.Responses.LoopBackend (hasRecoverableIncompleteOutput)
import Agent.Responses.StreamAssembly
    ( ResponseFailure(..)
    , StreamAssemblyConfig(..)
    , StreamAssemblyState
    , applyStreamEvent
    , buildStreamResponseWithModel
    , failedResponseMessage
    , failedStreamResponseMessage
    , finishStreamResponse
    , responseFailureFromState
    )
import qualified Agent.Responses.Types as OpenAI
import Control.Applicative ((<|>))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Network.Http.Client
    ( Method(POST)
    , RequestBuilder
    , Response
    , buildRequest1
    , establishConnection
    , http
    , jsonBody
    , receiveResponse
    , sendRequest
    , setContentType
    , setHeader
    , withConnection
    )
import OpenSSL (withOpenSSL)
import qualified Network.URI as URI
import qualified System.IO.Streams as Streams

-- | Execute a JSON POST against a Codex-compatible endpoint.
--
-- Authentication and the optional broker account header are shared by the
-- normal Responses and compaction endpoints. Callers can add endpoint-specific
-- headers with the request modifier and retain control over response decoding
-- through the supplied handler.
postCodexJson
    :: Text
    -> Text
    -> Text
    -> Text
    -> (RequestBuilder () -> RequestBuilder ())
    -> Aeson.Value
    -> (Response -> Streams.InputStream BS.ByteString -> IO (Either ApiError value))
    -> IO (Either ApiError value)
postCodexJson baseUrl endpoint accessToken accountId configureRequest body handler = do
    case codexEndpointUri baseUrl endpoint of
        Nothing -> pure $ Left (JsonDecodeError
            ("Invalid URL: " <> baseUrl)
            "")
        Just uri ->
            let url = Text.pack (URI.uriToString id uri "")
                path = Text.encodeUtf8
                    (Text.pack uri.uriPath <> Text.pack uri.uriQuery)
                request = buildRequest1 $ configureRequest do
                    http POST path
                    setContentType "application/json"
                    setHeader "Accept" "text/event-stream"
                    setHeader
                        "Authorization"
                        ("Bearer " <> Text.encodeUtf8 accessToken)
                    if Text.null (Text.strip accountId)
                        then pure ()
                        else setHeader
                            "chatgpt-account-id"
                            (Text.encodeUtf8 accountId)
            in withOpenSSL $
                withConnection (establishConnection (Text.encodeUtf8 url)) \connection -> do
                    sendRequest connection request (jsonBody body)
                    receiveResponse connection handler

codexEndpointUri :: Text -> Text -> Maybe URI.URI
codexEndpointUri baseUrl endpoint = do
    uri <- URI.parseURI (Text.unpack baseUrl)
    let basePath = Text.dropWhileEnd (== '/') (Text.pack uri.uriPath)
        endpointPath = "/" <> Text.dropWhile (== '/') endpoint
        joinedPath = Text.unpack (basePath <> endpointPath)
    pure uri
        { URI.uriPath = joinedPath
        , URI.uriFragment = ""
        }

-- | Decode a successful Responses HTTP body. Streaming bodies use the same
-- partial-response assembler as the WebSocket and provider SSE transports;
-- compatible non-streaming hosts may still return one canonical JSON object.
decodeCodexHttpBody :: Text -> Either ApiError OpenAI.Response
decodeCodexHttpBody = decodeCodexHttpBodyWithModel Nothing

-- | Decode a successful Responses HTTP body while retaining the originating
-- request model when partial lifecycle events omit the server model field.
decodeCodexHttpBodyWithModel
    :: Maybe Text
    -> Text
    -> Either ApiError OpenAI.Response
decodeCodexHttpBodyWithModel modelHint =
    decodeCodexHttpBodyBytesWithModel modelHint . Text.encodeUtf8

-- | Decode a complete buffered Codex response from its original wire bytes.
decodeCodexHttpBodyBytes :: BS.ByteString -> Either ApiError OpenAI.Response
decodeCodexHttpBodyBytes = decodeCodexHttpBodyBytesWithModel Nothing

-- | Decode a complete buffered Codex response from its original wire bytes
-- while retaining the request model for incomplete response fragments.
decodeCodexHttpBodyBytesWithModel
    :: Maybe Text
    -> BS.ByteString
    -> Either ApiError OpenAI.Response
decodeCodexHttpBodyBytesWithModel modelHint bodyBytes
    | looksLikeSseBytes bodyBytes = do
        events <- parseSseEventsBytes bodyBytes
        response <- buildStreamResponseWithModel streamConfig modelHint events
        rejectFailedCodexResponse response
    | otherwise =
        decodeJsonResponseBodyBytes bodyBytes

decodeJsonResponseBodyBytes
    :: BS.ByteString
    -> Either ApiError OpenAI.Response
decodeJsonResponseBodyBytes bodyBytes =
    case decodeResponse bodyBytes of
        Right response -> rejectFailedCodexResponse response
        Left directError ->
            case Json.decodeEither wrappedResponseDecoder bodyBytes of
                Right response -> rejectFailedCodexResponse response
                Left _ -> Left
                    (JsonDecodeError
                        directError
                        (bodyPreview bodyBytes))

wrappedResponseDecoder :: Json.Decoder OpenAI.Response
wrappedResponseDecoder =
    Json.object (Json.atKey "response" OpenAI.responseDecoder)

-- | The Responses endpoint can return HTTP 200 with a terminal
-- @status: "failed"@ payload. Normalize that wire shape into the same typed
-- error channel as non-2xx responses so the transport retry policy can act on
-- it.
rejectFailedCodexResponse :: OpenAI.Response -> Either ApiError OpenAI.Response
rejectFailedCodexResponse response =
    case response.status of
        OpenAI.ResponseFailed -> Left (terminalResponseError response)
        OpenAI.ResponseIncomplete
            | hasRecoverableIncompleteOutput response -> Right response
            | otherwise -> Left (terminalResponseError response)
        _ -> Right response

-- | Apply one decoded Codex SSE event without retaining the preceding events.
-- A terminal event returns the assembled response (or its typed provider
-- error); non-terminal events return the next bounded assembly state.
stepCodexStreamResponse
    :: Maybe Text
    -> StreamAssemblyState
    -> OpenAI.ResponseStreamEvent
    -> Either ApiError (StreamAssemblyState, Maybe OpenAI.Response)
stepCodexStreamResponse modelHint state event =
    let next = applyStreamEvent state event
        complete = do
            response <- finishStreamResponse modelHint next event
            checked <- rejectFailedCodexResponse response
            pure (next, Just checked)
    in case event of
        OpenAI.ResponseCompletedEvent{} -> complete
        OpenAI.ResponseDoneEvent{} -> complete
        OpenAI.ResponseIncompleteEvent{} -> complete
        OpenAI.ResponseFailedEvent{} ->
            Left (failedStreamResponseError (responseFailureFromState next))
        OpenAI.ResponseErrorEvent { streamError } ->
            Left (streamConfig.classifyStreamError streamError)
        OpenAI.ResponseNestedErrorEvent { streamError } ->
            Left (streamConfig.classifyStreamError streamError)
        _ -> Right (next, Nothing)

-- | Finish a Codex stream that reached EOF before a terminal event. This
-- mirrors 'buildStreamResponseWithModel': a lifecycle failure remains typed,
-- while an otherwise unterminated stream reports a missing completion.
finishCodexStreamResponse
    :: StreamAssemblyState
    -> Either ApiError OpenAI.Response
finishCodexStreamResponse state =
    let failure = responseFailureFromState state
    in case failure.failureStatus of
        Just "failed" -> Left (failedStreamResponseError failure)
        Just "incomplete" -> Left (failedStreamResponseError failure)
        _ -> Left $ JsonDecodeError streamConfig.missingCompletionMessage ""

streamConfig :: StreamAssemblyConfig
streamConfig = StreamAssemblyConfig
    { missingCompletionMessage =
        "No terminal response event found in Codex SSE stream"
    , classifyStreamError = \streamError ->
        mkOpenAIError
            (maybe
                (maybe ApiErrorType errorTypeFromText streamError.code)
                errorTypeFromText
                streamError.errorType)
            streamError.message
            streamError.code
            streamError.retryAfter
    , classifyFailedResponse = failedStreamResponseError
    , incompleteAsFailure = False
    }

failedStreamResponseError :: ResponseFailure -> ApiError
failedStreamResponseError failure =
    mkOpenAIError
        (maybe ApiErrorType errorTypeFromText
            (failure.failureErrorType <|> failure.failureErrorCode))
        (failedStreamResponseMessage failure)
        failure.failureErrorCode
        Nothing

terminalResponseError :: OpenAI.Response -> ApiError
terminalResponseError response =
    case response.error of
        Just responseError
            | not (Text.null (Text.strip responseError.code))
                || not (Text.null (Text.strip responseError.message)) ->
                    mkOpenAIError
                        (if Text.null (Text.strip responseError.code)
                            then ApiErrorType
                            else errorTypeFromText responseError.code)
                        (failedResponseMessage response)
                        (if Text.null (Text.strip responseError.code)
                            then Nothing
                            else Just responseError.code)
                        Nothing
        _ ->
            ProviderError ApiErrorType
                (failedResponseMessage response)
                Nothing

looksLikeSseBytes :: BS.ByteString -> Bool
looksLikeSseBytes bodyBytes =
    any
        (\line ->
            "event:" `BS.isPrefixOf` line
                || "data:" `BS.isPrefixOf` line)
        (BS8.lines bodyBytes)

bodyPreview :: BS.ByteString -> Text
bodyPreview =
    Text.take 2000 . Text.decodeUtf8Lenient
