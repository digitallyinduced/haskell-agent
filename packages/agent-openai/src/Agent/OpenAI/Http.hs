module Agent.OpenAI.Http
    ( postCodexJson
    , decodeCodexHttpBody
    , rejectFailedCodexResponse
    ) where

import Agent.Error (ApiError(..), ErrorType(..), errorTypeFromText)
import Agent.OpenAI.Error (mkOpenAIError)
import Agent.Responses.SSE (parseSseEvents)
import Agent.Responses.StreamAssembly
    ( ResponseFailure(..)
    , StreamAssemblyConfig(..)
    , buildStreamResponse
    , failedStreamResponseMessage
    )
import qualified Agent.Responses.Types as OpenAI
import Control.Applicative ((<|>))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
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
    let url = Text.dropWhileEnd (== '/') baseUrl <> endpoint
    case URI.parseURI (Text.unpack url) of
        Nothing -> pure $ Left (JsonDecodeError ("Invalid URL: " <> url) "")
        Just uri ->
            let path = Text.encodeUtf8 (Text.pack uri.uriPath)
                request = buildRequest1 $ configureRequest do
                    http POST path
                    setContentType "application/json"
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

-- | Decode a successful Responses HTTP body. Streaming bodies use the same
-- partial-response assembler as the WebSocket and provider SSE transports;
-- compatible non-streaming hosts may still return one canonical JSON object.
decodeCodexHttpBody :: Text -> Either ApiError OpenAI.Response
decodeCodexHttpBody bodyText
    | looksLikeSse bodyText = do
        events <- parseSseEvents bodyText
        buildStreamResponse streamConfig events
    | otherwise =
        case decodeJsonResponseBody bodyText of
            Just jsonValue -> decodeResponseValue jsonValue bodyText
            Nothing -> Left (JsonDecodeError
                "Invalid Codex Responses body"
                (Text.take 2000 bodyText))

decodeJsonResponseBody :: Text -> Maybe Aeson.Value
decodeJsonResponseBody bodyText =
    case Aeson.eitherDecodeStrict' (Text.encodeUtf8 (Text.strip bodyText)) of
        Right (Aeson.Object object)
            | Just inner <- KeyMap.lookup "response" object -> Just inner
            | otherwise -> Just (Aeson.Object object)
        _ -> Nothing

decodeResponseValue :: Aeson.Value -> Text -> Either ApiError OpenAI.Response
decodeResponseValue jsonValue bodyText =
    case Aeson.fromJSON jsonValue of
        Aeson.Success response -> rejectFailedCodexResponse response
        Aeson.Error err -> Left (JsonDecodeError (Text.pack err) (Text.take 2000 bodyText))

-- | The Responses endpoint can return HTTP 200 with a terminal
-- @status: "failed"@ payload. Normalize that wire shape into the same typed
-- error channel as non-2xx responses so the transport retry policy can act on
-- it.
rejectFailedCodexResponse :: OpenAI.Response -> Either ApiError OpenAI.Response
rejectFailedCodexResponse response =
    case response.status of
        OpenAI.ResponseFailed -> Left (failedResponseError response.error)
        _ -> Right response

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
    }

failedStreamResponseError :: ResponseFailure -> ApiError
failedStreamResponseError failure =
    mkOpenAIError
        (maybe ApiErrorType errorTypeFromText
            (failure.failureErrorType <|> failure.failureErrorCode))
        (failedStreamResponseMessage failure)
        failure.failureErrorCode
        Nothing

failedResponseError :: Maybe OpenAI.ResponseError -> ApiError
failedResponseError Nothing =
    ProviderError ApiErrorType "Codex response failed without error details" Nothing
failedResponseError (Just responseError) =
    mkOpenAIError
        (errorTypeFromText responseError.code)
        responseError.message
        (Just responseError.code)
        Nothing

looksLikeSse :: Text -> Bool
looksLikeSse bodyText =
    any
        (\line ->
            Text.isPrefixOf "event:" line
                || Text.isPrefixOf "data:" line)
        (Text.lines bodyText)
