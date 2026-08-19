module Agent.OpenAI.Http
    ( decodeCodexHttpBody
    , rejectFailedCodexResponse
    ) where

import Agent.Error (ApiError(..), ErrorType(..))
import Agent.OpenAI.Error (mkOpenAIError)
import Agent.OpenAI.ResponseMerge (mergeCompletedResponseOutput)
import qualified Agent.OpenAI.Responses.Types as OpenAI
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

-- | Decode a successful Responses HTTP body.
--
-- ChatGPT Codex and streaming proxies emit SSE with a terminal
-- @response.completed@ event. Compatible non-streaming hosts return the
-- completed response as a single JSON object. Prefer the SSE completed payload
-- when present so incremental @response.output_item.done@ events can still be
-- merged.
decodeCodexHttpBody :: Text -> Either ApiError OpenAI.Response
decodeCodexHttpBody bodyText =
    case extractCompletedResponse bodyText of
        Just jsonValue -> decodeResponseValue jsonValue bodyText
        Nothing -> case decodeJsonResponseBody bodyText of
            Just jsonValue -> decodeResponseValue jsonValue bodyText
            Nothing -> Left (JsonDecodeError
                "No response.completed event found in SSE stream"
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

failedResponseError :: Maybe OpenAI.ResponseError -> ApiError
failedResponseError Nothing =
    ProviderError ApiErrorType "Codex response failed without error details" Nothing
failedResponseError (Just responseError) =
    mkOpenAIError
        ApiErrorType
        responseError.message
        (Just responseError.code)
        Nothing

-- | Parse an SSE stream and build its final response.
extractCompletedResponse :: Text -> Maybe Aeson.Value
extractCompletedResponse sseText =
    let eventBlocks = Text.splitOn "\n\n" sseText
        parsed = map parseSSEBlock eventBlocks
        completedResponse = lastMay [response | (_, Just response, _) <- parsed]
        doneItems = [item | (Just "response.output_item.done", _, Just item) <- parsed]
    in mergeCompletedResponseOutput doneItems <$> completedResponse

-- | Parse a single SSE event block, returning its event type, completed
-- response object, and output item object respectively.
parseSSEBlock :: Text -> (Maybe Text, Maybe Aeson.Value, Maybe Aeson.Value)
parseSSEBlock block =
    let eventType = listToMaybe
            [ Text.strip (Text.drop 6 line)
            | line <- Text.lines block
            , Text.isPrefixOf "event:" line
            ]
        dataLines =
            [ Text.drop 5 line
            | line <- Text.lines block
            , Text.isPrefixOf "data:" line
            ]
        dataText = Text.intercalate "\n" (map Text.strip dataLines)
    in case Aeson.eitherDecodeStrict' (Text.encodeUtf8 dataText) of
        Right (Aeson.Object object) ->
            ( eventType
            , KeyMap.lookup "response" object
            , KeyMap.lookup "item" object
            )
        _ -> (eventType, Nothing, Nothing)

lastMay :: [value] -> Maybe value
lastMay [] = Nothing
lastMay [value] = Just value
lastMay (_ : rest) = lastMay rest

listToMaybe :: [value] -> Maybe value
listToMaybe [] = Nothing
listToMaybe (value : _) = Just value
