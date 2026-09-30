-- | Gateway-bound streaming dictation and its bounded HTTP fallback.
module Agent.Runtime.Gateway.Dictation (transcribeGatewayPcmWith) where

import Agent.Accounts.Gateway.Credentials
    ( loadGatewayCredential
    , validateGatewayCredential
    , withGatewayCredentialTurnLease
    )
import Agent.Runtime.Gateway.Http (gatewayMaxResponseBytes)
import Agent.Audio.Transcription qualified as Audio
import Agent.ClientIdentity (gatewayUserAgent)
import Agent.OpenAI.Transcription
    ( ChatGPTDictationStreamFailure(..)
    , encodePcm16Wav
    , openAITranscriptionSampleRate
    , transcribePcmWithChatGPTStreamAt
    )
import Agent.Server.Client.GatewayIdentity (GatewayCredential(..))
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (throwString, tryAny)
import Control.Monad (unless, when)
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL qualified as Base64Url
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types
    ( Status
    , hAccept
    , hAuthorization
    , statusCode
    , statusIsSuccessful
    )
import Network.URI qualified as URI
import System.Entropy (getEntropy)

transcribeGatewayPcmWith
    :: GatewayCredential
    -> ((BS.ByteString -> IO ()) -> IO ())
    -> (Text -> IO ())
    -> IO (Either Text Text)
transcribeGatewayPcmWith admitted produceAudio onTranscript =
    withGatewayCredentialTurnLease do
        loadGatewayCredential >>= \case
            Right (Just current)
                | current == admitted ->
                    case gatewayDictationWebSocketUrl current of
                        Left err -> pure (Left err)
                        Right websocketUrl -> do
                            userAgent <- gatewayUserAgent
                            chunks <- newIORef []
                            capturedBytes <- newIORef 0
                            let captureAndBuffer sendAudio =
                                    produceAudio \chunk ->
                                        unless (BS.null chunk) do
                                            total <-
                                                (+ BS.length chunk)
                                                    <$> readIORef capturedBytes
                                            when
                                                (total > gatewayMaxPcmBytes)
                                                (throwString
                                                    "gateway dictation audio exceeded the client limit")
                                            writeIORef capturedBytes total
                                            modifyIORef' chunks (chunk :)
                                            sendAudio chunk
                            transcribePcmWithChatGPTStreamAt
                                websocketUrl
                                [ ( "Authorization"
                                  , "Bearer "
                                        <> TextEncoding.encodeUtf8
                                            current.gatewayAccessToken
                                  )
                                , ("User-Agent", userAgent)
                                ]
                                captureAndBuffer
                                onTranscript >>= \case
                                    Left ChatGPTDictationCaptureFailed{} ->
                                        pure $
                                            Left
                                                "Gateway dictation audio capture failed."
                                    Left ChatGPTDictationStreamUnavailable{} -> do
                                        pcm <-
                                            BS.concat . reverse
                                                <$> readIORef chunks
                                        wavResult <-
                                            if BS.null pcm
                                                then
                                                    captureGatewayWav
                                                        produceAudio
                                                else
                                                    pure $
                                                        case encodePcm16Wav
                                                            openAITranscriptionSampleRate
                                                            pcm of
                                                                Left _ ->
                                                                    Left
                                                                        "Gateway dictation streaming failed."
                                                                Right wav ->
                                                                    Right wav
                                        case wavResult of
                                            Left err -> pure (Left err)
                                            Right wav ->
                                                loadGatewayCredential >>= \case
                                                    Right (Just latest)
                                                        | latest == current ->
                                                            postGatewayTranscription
                                                                latest
                                                                wav >>= \case
                                                                    Left err ->
                                                                        pure
                                                                            (Left
                                                                                err)
                                                                    Right transcript -> do
                                                                        onTranscript
                                                                            transcript
                                                                        pure
                                                                            (Right
                                                                                transcript)
                                                    _ ->
                                                        pure $
                                                            Left
                                                                "The organization gateway changed during dictation."
                                    Right transcript ->
                                        pure (Right transcript)
            _ ->
                pure $
                    Left
                        "The organization gateway changed before dictation started."

gatewayDictationWebSocketUrl
    :: GatewayCredential
    -> Either Text Text
gatewayDictationWebSocketUrl credential = do
    uri <-
        maybe
            (Left "Gateway WebSocket URL is invalid.")
            Right
            (URI.parseURI (Text.unpack credential.gatewayWebSocketUrl))
    pure $
        Text.pack $
            show
                uri
                    { URI.uriPath = "/v1/audio/transcriptions"
                    , URI.uriQuery = ""
                    , URI.uriFragment = ""
                    }

captureGatewayWav
    :: ((BS.ByteString -> IO ()) -> IO ())
    -> IO (Either Text LBS.ByteString)
captureGatewayWav produceAudio =
    tryAny capture >>= \case
        Left _ ->
            pure (Left "Gateway dictation audio capture failed.")
        Right result -> pure result
  where
    capture = do
        chunks <- newIORef []
        capturedBytes <- newIORef 0
        produceAudio \chunk ->
            unless (BS.null chunk) do
                total <- (+ BS.length chunk) <$> readIORef capturedBytes
                when (total > gatewayMaxPcmBytes) $
                    throwString
                        "gateway dictation audio exceeded the client limit"
                writeIORef capturedBytes total
                modifyIORef' chunks (chunk :)
        pcm <- BS.concat . reverse <$> readIORef chunks
        pure $
            case encodePcm16Wav openAITranscriptionSampleRate pcm of
                Left _ ->
                    Left "Gateway dictation captured invalid audio."
                Right wav -> Right wav

gatewayMaxPcmBytes :: Int
gatewayMaxPcmBytes = 4 * 1024 * 1024 - 4096

postGatewayTranscription
    :: GatewayCredential
    -> LBS.ByteString
    -> IO (Either Text Text)
postGatewayTranscription credential wav =
    case validateGatewayCredential credential of
        Left _ -> pure (Left "Gateway credential is invalid.")
        Right () -> do
            boundary <- gatewayTranscriptionBoundary
            let endpoint =
                    Text.dropWhileEnd (== '/')
                        (Text.strip credential.gatewayBaseUrl)
                        <> "/v1/audio/transcriptions"
            prepared <- tryAny do
                userAgent <- gatewayUserAgent
                manager <- newTlsManager
                initial <- HTTP.parseRequest (Text.unpack endpoint)
                let baseRequest =
                        initial
                            { HTTP.requestHeaders =
                                [ ( hAuthorization
                                  , "Bearer "
                                        <> TextEncoding.encodeUtf8
                                            credential.gatewayAccessToken
                                  )
                                , (hAccept, "application/json")
                                , ("User-Agent", userAgent)
                                ]
                            }
                case Audio.transcriptionRequest
                    (gatewayMaxPcmBytes + 4096)
                    baseRequest boundary "dictation" (LBS.toStrict wav) of
                    Left problem -> pure (Left problem)
                    Right request -> pure (Right (manager, request))
            case prepared of
                Left _ ->
                    pure (Left "Could not reach the organization gateway for dictation.")
                Right (Left _) ->
                    pure (Left "Gateway dictation request is invalid.")
                Right (Right (manager, request)) ->
                    retryGatewayTranscription [1_000_000, 2_000_000, 4_000_000]
                        (Audio.performTranscriptionRequest
                            manager gatewayMaxResponseBytes request)

-- | Repeat only transport failures and temporary HTTP responses. The request
-- contains the same buffered WAV on every attempt; capture is never restarted.
retryGatewayTranscription
    :: [Int]
    -> IO (Either Audio.TranscriptionFailure (Status, BS.ByteString))
    -> IO (Either Text Text)
retryGatewayTranscription delays send = do
    outcome <- send
    case (delays, outcome) of
        (delay : remaining, Left Audio.TranscriptionUnavailable) -> do
            threadDelay delay
            retryGatewayTranscription remaining send
        (delay : remaining, Right (status, _))
            | retryableGatewayStatus status -> do
                threadDelay delay
                retryGatewayTranscription remaining send
        _ -> pure (gatewayTranscriptionResult outcome)

retryableGatewayStatus :: Status -> Bool
retryableGatewayStatus status =
    statusCode status == 408
        || statusCode status == 429
        || statusCode status >= 500 && statusCode status <= 599

gatewayTranscriptionResult
    :: Either Audio.TranscriptionFailure (Status, BS.ByteString)
    -> Either Text Text
gatewayTranscriptionResult = \case
    Left Audio.TranscriptionResponseTooLarge ->
        Left "Gateway dictation returned an oversized response."
    Left _ ->
        Left "Could not reach the organization gateway for dictation."
    Right (status, responseBody)
        | statusIsSuccessful status ->
            decodeGatewayTranscript responseBody
        | statusCode status == 404 ->
            Left "Dictation is not supported by this organization gateway."
        | otherwise ->
            Left (gatewayDictationStatusError status responseBody)

gatewayTranscriptionBoundary :: IO BS.ByteString
gatewayTranscriptionBoundary =
    ("----haskell-agent-gateway-" <>)
        . Base64Url.encodeUnpadded
        <$> getEntropy 18

newtype GatewayTranscript =
    GatewayTranscript { gatewayTranscriptText :: Text }

instance Aeson.FromJSON GatewayTranscript where
    parseJSON =
        Aeson.withObject "GatewayTranscript" \object ->
            GatewayTranscript <$> object .: "text"

newtype GatewayErrorEnvelope =
    GatewayErrorEnvelope { gatewayErrorMessage :: Text }

instance Aeson.FromJSON GatewayErrorEnvelope where
    parseJSON =
        Aeson.withObject "GatewayErrorEnvelope" \root -> do
            err <- root .: "error"
            Aeson.withObject "GatewayError" (\object ->
                GatewayErrorEnvelope <$> object .: "message") err

gatewayDictationStatusError :: Status -> BS.ByteString -> Text
gatewayDictationStatusError status body =
    "Gateway dictation returned HTTP "
        <> Text.pack (show (statusCode status))
        <> case gatewayErrorMessageFromBody body of
            Just message -> ": " <> message
            Nothing -> ""

gatewayErrorMessageFromBody :: BS.ByteString -> Maybe Text
gatewayErrorMessageFromBody body = do
    envelope <-
        either
            (const Nothing)
            Just
            (Aeson.eitherDecodeStrict' body
                :: Either String GatewayErrorEnvelope)
    let trimmed = Text.strip envelope.gatewayErrorMessage
    if Text.null trimmed
        then Nothing
        else Just (Text.take 200 (Text.unwords (Text.words trimmed)))

decodeGatewayTranscript :: BS.ByteString -> Either Text Text
decodeGatewayTranscript body =
    case
        Aeson.eitherDecodeStrict' body
            :: Either String GatewayTranscript
        of
        Left _ ->
            Left "Gateway dictation returned an unreadable response."
        Right response
            | Text.null transcript ->
                Left "Gateway dictation returned an empty transcript."
            | otherwise -> Right transcript
          where
            transcript =
                Text.strip response.gatewayTranscriptText
