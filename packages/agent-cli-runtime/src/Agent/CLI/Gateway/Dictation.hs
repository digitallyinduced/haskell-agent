-- | Gateway-bound streaming dictation and its bounded HTTP fallback.
module Agent.CLI.Gateway.Dictation (transcribeGatewayPcmWith) where

import Agent.CLI.Gateway.Credentials
    ( loadGatewayCredential
    , validateGatewayCredential
    , withGatewayCredentialTurnLease
    )
import Agent.CLI.Gateway.Http (gatewayMaxResponseBytes, readBoundedBody)
import Agent.OpenAI.Transcription
    ( ChatGPTDictationStreamFailure(..)
    , encodePcm16Wav
    , openAITranscriptionSampleRate
    , transcribePcmWithChatGPTStreamAt
    )
import Agent.Server.Client.GatewayIdentity (GatewayCredential(..))
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
import Network.HTTP.Client.MultipartFormData qualified as Multipart
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types
    ( hAccept
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
            outcome <- tryAny do
                manager <- newTlsManager
                initial <- HTTP.parseRequest (Text.unpack endpoint)
                let baseRequest =
                        initial
                            { HTTP.method = "POST"
                            , HTTP.requestHeaders =
                                [ ( hAuthorization
                                  , "Bearer "
                                        <> TextEncoding.encodeUtf8
                                            credential.gatewayAccessToken
                                  )
                                , (hAccept, "application/json")
                                ]
                            , HTTP.checkResponse = \_ _ -> pure ()
                            -- Never forward the gateway bearer to a redirect.
                            , HTTP.redirectCount = 0
                            , HTTP.responseTimeout =
                                HTTP.responseTimeoutMicro
                                    (2 * 60 * 1_000_000)
                            }
                    audioPart =
                        (Multipart.partFileRequestBody
                            "file"
                            "audio.wav"
                            (HTTP.RequestBodyLBS wav))
                            { Multipart.partContentType = Just "audio/wav" }
                request <- Multipart.formDataBodyWithBoundary
                    boundary
                    [ Multipart.partBS "model" "dictation"
                    , audioPart
                    ]
                    baseRequest
                HTTP.withResponse request manager \response -> do
                    responseBody <-
                        readBoundedBody
                            gatewayMaxResponseBytes
                            (HTTP.responseBody response)
                    pure
                        ( HTTP.responseStatus response
                        , responseBody
                        )
            pure case outcome of
                Left _ ->
                    Left "Could not reach the organization gateway for dictation."
                Right (_status, Nothing) ->
                    Left "Gateway dictation returned an oversized response."
                Right (status, Just responseBody)
                    | statusIsSuccessful status ->
                        decodeGatewayTranscript responseBody
                    | statusCode status == 404 ->
                        Left
                            "Dictation is not supported by this organization gateway."
                    | otherwise ->
                        Left $
                            "Gateway dictation returned HTTP "
                                <> Text.pack (show (statusCode status))

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
