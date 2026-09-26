{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Bounded multipart transcription transport. Credential admission and
-- endpoint selection belong to the caller; this module never discovers a
-- provider, retries a request, or follows a redirect.
module Agent.Audio.Transcription
    ( TranscriptionFailure (..)
    , transcriptionRequest
    , performTranscriptionRequest
    , readTranscriptionBody
    , decodeTranscript
    ) where

import Control.Exception.Safe (catch)
import Data.Aeson (eitherDecodeStrict', withObject, (.:))
import Data.Aeson.Types (parseEither)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types.Status (Status)
import System.Timeout (timeout)

data TranscriptionFailure
    = InvalidTranscriptionRequest
    | TranscriptionUnavailable
    | TranscriptionResponseTooLarge
    | InvalidTranscriptionResponse
    deriving (Eq, Show)

-- | The base request must already have an admitted endpoint and credentials.
-- WAV data and the model are encoded once into a bounded strict request body.
transcriptionRequest
    :: Int -> HTTP.Request -> BS.ByteString -> Text -> BS.ByteString
    -> Either TranscriptionFailure HTTP.Request
transcriptionRequest maximumAudioBytes initial boundary model wav
    | BS.null boundary || BS.length boundary > 100
        || not (BS.all validBoundaryByte boundary) = Left InvalidTranscriptionRequest
    | Text.null model || Text.length model > 200
        || Text.any (\character -> character < ' ' || character == '\DEL') model =
            Left InvalidTranscriptionRequest
    | BS.length wav <= 44 || BS.length wav > maximumAudioBytes
        || BS.take 4 wav /= "RIFF" || BS.take 4 (BS.drop 8 wav) /= "WAVE" =
            Left InvalidTranscriptionRequest
    | otherwise = Right initial
        { HTTP.method = "POST"
        , HTTP.redirectCount = 0
        , HTTP.checkResponse = \_ _ -> pure ()
        , HTTP.responseTimeout = HTTP.responseTimeoutMicro requestDuration
        , HTTP.requestHeaders =
            ("Content-Type", "multipart/form-data; boundary=" <> boundary)
                : filter (\(name, _) -> name /= "Content-Type") (HTTP.requestHeaders initial)
        , HTTP.requestBody = HTTP.RequestBodyBS $ BS.concat
            [ "--", boundary, "\r\nContent-Disposition: form-data; name=\"model\"\r\n\r\n"
            , Text.encodeUtf8 model
            , "\r\n--", boundary
            , "\r\nContent-Disposition: form-data; name=\"file\"; filename=\"audio.wav\"\r\nContent-Type: audio/wav\r\n\r\n"
            , wav, "\r\n--", boundary, "--\r\n"
            ]
        }
  where
    validBoundaryByte character =
        character >= 65 && character <= 90
            || character >= 97 && character <= 122
            || character >= 48 && character <= 57
            || character == 45 || character == 95

requestDuration :: Int
requestDuration = 120 * 1000000

-- | Bound the entire operation, not merely each socket read. Exceptions never
-- expose a bearer-bearing request. External asynchronous cancellation escapes.
performTranscriptionRequest
    :: HTTP.Manager -> Int -> HTTP.Request
    -> IO (Either TranscriptionFailure (Status, BS.ByteString))
performTranscriptionRequest manager maximumResponseBytes request =
    catch
        (do
            result <- timeout requestDuration $
                HTTP.withResponse request { HTTP.redirectCount = 0 } manager $ \response -> do
                    body <- readTranscriptionBody maximumResponseBytes (HTTP.responseBody response)
                    pure ((,) (HTTP.responseStatus response) <$> body)
            pure (maybe (Left TranscriptionUnavailable) id result))
        (\(_ :: HTTP.HttpException) -> pure (Left TranscriptionUnavailable))

readTranscriptionBody
    :: Int -> HTTP.BodyReader -> IO (Either TranscriptionFailure BS.ByteString)
readTranscriptionBody maximumBytes reader
    | maximumBytes < 0 = pure (Left TranscriptionResponseTooLarge)
    | otherwise = consume maximumBytes []
  where
    consume remaining chunks = do
        chunk <- HTTP.brRead reader
        if BS.null chunk
            then pure (Right (BS.concat (reverse chunks)))
            else if BS.length chunk > remaining
                then pure (Left TranscriptionResponseTooLarge)
                else consume (remaining - BS.length chunk) (chunk : chunks)

-- | Strict application response policy; callers needing a different policy
-- may decode the bounded response returned by 'performTranscriptionRequest'.
decodeTranscript :: Int -> Int -> BS.ByteString -> Either TranscriptionFailure Text
decodeTranscript maximumBytes maximumCharacters bytes
    | BS.length bytes > maximumBytes = Left TranscriptionResponseTooLarge
    | otherwise = case eitherDecodeStrict' bytes >>= parseEither (withObject "transcription" (.: "text")) of
        Right transcript
            | not (Text.null (Text.strip transcript))
            , Text.length transcript <= maximumCharacters
            , not (Text.any (== '\0') transcript) -> Right transcript
        _ -> Left InvalidTranscriptionResponse
