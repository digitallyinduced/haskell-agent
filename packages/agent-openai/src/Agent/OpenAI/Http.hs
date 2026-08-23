module Agent.OpenAI.Http
    ( decodeCodexHttpBody
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
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

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
