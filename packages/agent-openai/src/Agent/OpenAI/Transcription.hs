-- | Microphone transcription through OpenAI and ChatGPT.
module Agent.OpenAI.Transcription
    ( ChatGPTDictationEvent(..)
    , TranscriptEvent(..)
    , chatGPTTranscriptionBaseUrl
    , decodeChatGPTDictationEvent
    , decodeTranscriptEvent
    , decodeTranscriptionResponse
    , encodePcm16Wav
    , openAITranscriptionModel
    , openAITranscriptionSampleRate
    , transcribePcmWithOpenAI
    , transcribePcmWithOpenAIAt
    ) where

import Agent.Error (ApiError(..))
import qualified Agent.Json.Decode as Json
import Agent.Provider
    ( BillingMode(..)
    , Credential(..)
    , Provider(OpenAIProvider)
    , TokenProvider
    , getNextToken
    , runWithTokenProvider
    , seedTokenProvider
    , tokenProviderBillingMode
    )
import Control.Applicative ((<|>))
import Control.Concurrent.Async
    ( cancel
    , waitCatch
    , waitEitherCatch
    , withAsync
    )
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , newEmptyMVar
    , newMVar
    , readMVar
    , takeMVar
    , tryPutMVar
    , tryReadMVar
    )
import Control.Exception.Safe
    ( SomeException
    , displayException
    , finally
    , fromException
    , isSyncException
    , throwIO
    , tryAny
    )
import Control.Monad (join, unless, void)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import Data.Unique (hashUnique, newUnique)
import Data.Word (Word32)
import Network.HTTP.Simple
    ( getResponseBody
    , getResponseStatusCode
    , httpLBS
    , parseRequest
    , setRequestBodyLBS
    , setRequestHeader
    , setRequestMethod
    )
import qualified Network.WebSockets as WS
import Network.URI
    ( URI(..)
    , URIAuth(..)
    , parseURI
    )
import qualified System.Timeout as Timeout
import Text.Read (readMaybe)
import qualified Wuss

-- | ChatGPT backend used by the official desktop app's buffered dictation
-- fallback. Unlike the public OpenAI Realtime endpoint, this route accepts a
-- ChatGPT OAuth bearer and account id.
chatGPTTranscriptionBaseUrl :: Text
chatGPTTranscriptionBaseUrl = "https://chatgpt.com/backend-api"

-- | Codex's transcription model for its Realtime V2 dictation session.
openAITranscriptionModel :: Text
openAITranscriptionModel = "gpt-4o-mini-transcribe"

-- | Realtime PCM input is mono, signed 16-bit little-endian audio at 24 kHz.
openAITranscriptionSampleRate :: Int
openAITranscriptionSampleRate = 24_000

data TranscriptEvent
    = SessionUpdated
    | TranscriptDelta
        { transcriptText :: !Text
        }
    | TranscriptCompleted
        { transcriptText :: !Text
        }
    | TranscriptError
        { transcriptMessage :: !Text
        }
    | TranscriptUnknown
    deriving (Eq, Show)

transcriptEventDecoder :: Json.Decoder TranscriptEvent
transcriptEventDecoder = Json.discriminatedObject "type" \case
    "session.updated" ->
        pure SessionUpdated
    "conversation.item.input_audio_transcription.delta" ->
        Json.object $
            TranscriptDelta
                <$> Json.defaultKey "" "delta" Json.text
    "conversation.item.input_audio_transcription.completed" ->
        Json.object $
            TranscriptCompleted
                <$> Json.defaultKey "" "transcript" Json.text
    "error" ->
        Json.object do
            topLevelMessage <- Json.optionalKey "message" Json.text
            nestedMessage <-
                Json.optionalKey
                    "error"
                    (Json.object $ Json.optionalKey "message" Json.text)
            pure $ TranscriptError
                (fromMaybe
                    "OpenAI Realtime transcription error"
                    (topLevelMessage <|> join nestedMessage))
    _ ->
        pure TranscriptUnknown

decodeTranscriptEvent :: LBS.ByteString -> Either String TranscriptEvent
decodeTranscriptEvent body =
    case Json.decodeEither transcriptEventDecoder (LBS.toStrict body) of
        Left err -> Left (Text.unpack err.jsonErrorMessage)
        Right event -> Right event

-- | Events emitted by ChatGPT's subscription-backed dictation stream.
data ChatGPTDictationEvent
    = ChatGPTSessionStarted
    | ChatGPTSessionUpdated !Text
    | ChatGPTSpeechStarted !Text
    | ChatGPTSpeechStopped !Text
    | ChatGPTTranscriptDelta !Text !Text
    | ChatGPTTranscriptSegment !Text !Text
    | ChatGPTTranscriptFinal !Text !Text
    | ChatGPTTranscriptFailed !Text
    | ChatGPTSessionError !Bool !Text
    | ChatGPTEventUnknown
    deriving (Eq, Show)

chatGPTDictationEventDecoder :: Json.Decoder ChatGPTDictationEvent
chatGPTDictationEventDecoder = Json.discriminatedObject "type" \case
    "session.started" ->
        pure ChatGPTSessionStarted
    "session.updated" ->
        Json.object $
            ChatGPTSessionUpdated
                <$> Json.atKey
                    "session"
                    (Json.object $ Json.atKey "status" Json.text)
    "speech.started" ->
        Json.object $
            ChatGPTSpeechStarted
                <$> Json.atKey "utterance_id" Json.text
    "speech.stopped" ->
        Json.object $
            ChatGPTSpeechStopped
                <$> Json.atKey "utterance_id" Json.text
    "transcript.delta" ->
        Json.object $
            ChatGPTTranscriptDelta
                <$> Json.atKey "utterance_id" Json.text
                <*> Json.atKey "text" Json.text
    "transcript.segment" ->
        Json.object $
            ChatGPTTranscriptSegment
                <$> Json.atKey "utterance_id" Json.text
                <*> Json.atKey "text" Json.text
    "transcript.final" ->
        Json.object $
            ChatGPTTranscriptFinal
                <$> Json.atKey "utterance_id" Json.text
                <*> Json.atKey "text" Json.text
    "transcript.failed" ->
        Json.object $
            ChatGPTTranscriptFailed
                <$> Json.atKey
                    "error"
                    (Json.object $ Json.atKey "message" Json.text)
    "session.error" ->
        Json.object $
            ChatGPTSessionError
                <$> Json.defaultKey False "fatal" Json.bool
                <*> Json.atKey
                    "error"
                    (Json.object $ Json.atKey "message" Json.text)
    _ ->
        pure ChatGPTEventUnknown

decodeChatGPTDictationEvent
    :: LBS.ByteString
    -> Either String ChatGPTDictationEvent
decodeChatGPTDictationEvent body =
    case Json.decodeEither
        chatGPTDictationEventDecoder
        (LBS.toStrict body) of
        Left err -> Left (Text.unpack err.jsonErrorMessage)
        Right event -> Right event

data TranscriptState = TranscriptState
    { partial :: !Text
    , completed :: !(Maybe Text)
    , failure :: !(Maybe Text)
    }

emptyTranscriptState :: TranscriptState
emptyTranscriptState = TranscriptState
    { partial = ""
    , completed = Nothing
    , failure = Nothing
    }

-- | Transcribe live 24 kHz mono signed PCM16 audio. API-billed credentials use
-- the public OpenAI Realtime endpoint. Subscription credentials use Codex
-- desktop's ChatGPT dictation stream with its buffered @/transcribe@ fallback.
transcribePcmWithOpenAI
    :: TokenProvider
    -> ((BS.ByteString -> IO ()) -> IO ())
    -> (Text -> IO ())
    -> IO (Either ApiError Text)
transcribePcmWithOpenAI =
    transcribePcmWithOpenAIAt chatGPTTranscriptionBaseUrl

-- | Variant with an explicit ChatGPT backend base URL. The override is useful
-- for tests and compatible deployments; API-key Realtime traffic still goes
-- directly to @api.openai.com@.
transcribePcmWithOpenAIAt
    :: Text
    -> TokenProvider
    -> ((BS.ByteString -> IO ()) -> IO ())
    -> (Text -> IO ())
    -> IO (Either ApiError Text)
transcribePcmWithOpenAIAt baseUrl provider produceAudio onTranscript =
    case tokenProviderBillingMode provider of
        ApiBilled ->
            transcribeRealtimeWithProvider provider produceAudio onTranscript
        SubscriptionBilled ->
            transcribeWithChatGPT
                baseUrl
                provider
                produceAudio
                onTranscript

transcribeRealtimeWithProvider
    :: TokenProvider
    -> ((BS.ByteString -> IO ()) -> IO ())
    -> (Text -> IO ())
    -> IO (Either ApiError Text)
transcribeRealtimeWithProvider provider produceAudio onTranscript =
    runWithTokenProvider provider \credential ->
        if credential.provider /= OpenAIProvider
            then pure $ Left $ CredentialError
                "OpenAI dictation requires an OpenAI API-key credential"
            else do
                result <- tryAny
                    (transcribe credential produceAudio onTranscript)
                case result of
                    Left err
                        | isSyncException err ->
                            pure (Left (realtimeTranscriptionException err))
                        | otherwise ->
                            throwIO err
                    Right transcript ->
                        pure (Right transcript)

realtimeTranscriptionException :: SomeException -> ApiError
realtimeTranscriptionException err =
    case fromException err of
        Just (WS.RequestRejected _ response) ->
            HttpError
                (WS.responseCode response)
                (if WS.responseCode response `elem` [401, 403]
                    then "OpenAI Realtime transcription authentication was rejected"
                    else "OpenAI Realtime transcription request was rejected")
        _ ->
            ConnectionError
                ("OpenAI Realtime transcription failed: "
                    <> Text.pack (displayException err))

transcribeWithChatGPT
    :: Text
    -> TokenProvider
    -> ((BS.ByteString -> IO ()) -> IO ())
    -> (Text -> IO ())
    -> IO (Either ApiError Text)
transcribeWithChatGPT baseUrl provider produceAudio onTranscript =
    getNextToken provider Nothing >>= \case
        Left err ->
            pure (Left err)
        Right initial
            | initial.provider /= OpenAIProvider ->
                pure $ Left $ CredentialError
                    "ChatGPT dictation requires an OpenAI credential"
            | otherwise -> do
                chunks <- newIORef []
                streamChatGPTDictation
                    baseUrl
                    initial
                    chunks
                    produceAudio
                    onTranscript >>= \case
                        ChatGPTStreamSucceeded transcript ->
                            pure (Right transcript)
                        ChatGPTStreamUnavailable _ ->
                            fallbackToChatGPTBatch
                                baseUrl
                                provider
                                initial
                                chunks
                                onTranscript
                        ChatGPTCaptureFailed message ->
                            pure $ Left $ ConnectionError
                                ("OpenAI dictation audio capture failed: "
                                    <> message)

capturePcmInto
    :: IORef [BS.ByteString]
    -> ((BS.ByteString -> IO ()) -> IO ())
    -> (BS.ByteString -> IO ())
    -> IO ()
capturePcmInto chunks produceAudio forward =
    produceAudio \bytes ->
        unless (BS.null bytes) do
            modifyIORef' chunks (bytes :)
            forward bytes

fallbackToChatGPTBatch
    :: Text
    -> TokenProvider
    -> Credential
    -> IORef [BS.ByteString]
    -> (Text -> IO ())
    -> IO (Either ApiError Text)
fallbackToChatGPTBatch
    baseUrl
    provider
    initial
    chunks
    onTranscript = do
        pcm <- BS.concat . reverse <$> readIORef chunks
        case encodePcm16Wav openAITranscriptionSampleRate pcm of
            Left err ->
                pure (Left err)
            Right wav -> do
                boundary <- transcriptionBoundary
                let body = multipartWavBody boundary wav
                seeded <- seedTokenProvider provider initial
                runWithTokenProvider seeded \credential ->
                    if credential.provider /= OpenAIProvider
                        then pure $ Left $ CredentialError
                            "ChatGPT dictation requires an OpenAI credential"
                        else
                            postChatGPTTranscription
                                baseUrl
                                credential
                                boundary
                                body >>= \case
                                    Left err ->
                                        pure (Left err)
                                    Right transcript -> do
                                        ignoreSynchronousException
                                            (onTranscript transcript)
                                        pure (Right transcript)

data ChatGPTStreamOutcome
    = ChatGPTStreamSucceeded !Text
    | ChatGPTStreamUnavailable !Text
    | ChatGPTCaptureFailed !Text
    deriving (Eq, Show)

data ChatGPTStreamEndpoint = ChatGPTStreamEndpoint
    { streamHost :: !String
    , streamPort :: !Int
    , streamPath :: !String
    , streamSecure :: !Bool
    }

data ChatGPTStreamState = ChatGPTStreamState
    { utteranceOrder :: ![Text]
    , textByUtterance :: !(Map.Map Text Text)
    }

emptyChatGPTStreamState :: ChatGPTStreamState
emptyChatGPTStreamState = ChatGPTStreamState
    { utteranceOrder = []
    , textByUtterance = Map.empty
    }

streamChatGPTDictation
    :: Text
    -> Credential
    -> IORef [BS.ByteString]
    -> ((BS.ByteString -> IO ()) -> IO ())
    -> (Text -> IO ())
    -> IO ChatGPTStreamOutcome
streamChatGPTDictation
    baseUrl
    credential
    chunks
    produceAudio
    onTranscript = do
        audio <- newChan
        withAsync
            (capturePcmInto
                chunks
                produceAudio
                (writeChan audio . Just)
                `finally` writeChan audio Nothing)
            \captureWorker -> do
                withAsync (connectAndStream audio) \streamWorker ->
                    waitEitherCatch
                        captureWorker
                        streamWorker >>= \case
                            Left captureResult ->
                                case captureResult of
                                    Left err
                                        | isSyncException err -> do
                                            cancel streamWorker
                                            pure $ ChatGPTCaptureFailed
                                                (Text.pack
                                                    (displayException err))
                                        | otherwise ->
                                            throwIO err
                                    Right () ->
                                        waitCatch streamWorker
                                            >>= resolveStreamResult
                            Right streamResult -> do
                                outcome <- resolveStreamResult streamResult
                                waitCatch captureWorker >>= \case
                                    Left err
                                        | isSyncException err ->
                                            pure $ ChatGPTCaptureFailed
                                                (Text.pack
                                                    (displayException err))
                                        | otherwise ->
                                            throwIO err
                                    Right () ->
                                        pure outcome
  where
    connectAndStream audio =
        case chatGPTStreamEndpoint baseUrl of
            Left message ->
                pure (ChatGPTStreamUnavailable message)
            Right endpoint ->
                tryAny
                    (runChatGPTWebSocket
                        endpoint
                        credential
                        (chatGPTStreamSession
                            audio
                            onTranscript)) >>= \case
                                Left err
                                    | isSyncException err ->
                                        pure $ ChatGPTStreamUnavailable
                                            (Text.pack
                                                (displayException err))
                                    | otherwise ->
                                        throwIO err
                                Right result ->
                                    pure result
    resolveStreamResult = \case
        Left err
            | isSyncException err ->
                pure $ ChatGPTStreamUnavailable
                    (Text.pack (displayException err))
            | otherwise ->
                throwIO err
        Right outcome ->
            pure outcome

chatGPTStreamSession
    :: Chan (Maybe BS.ByteString)
    -> (Text -> IO ())
    -> WS.Connection
    -> IO ChatGPTStreamOutcome
chatGPTStreamSession
    audio
    onTranscript
    connection = do
        WS.sendTextData connection $
            chatGPTSessionStartMessage openAITranscriptionSampleRate
        Timeout.timeout
            (10 * 1_000_000)
            (awaitChatGPTSessionStarted connection) >>= \case
                Nothing ->
                    pure $ ChatGPTStreamUnavailable
                        "ChatGPT dictation stream timed out during startup"
                Just (Left message) ->
                    pure (ChatGPTStreamUnavailable message)
                Just (Right ()) -> do
                    state <- newMVar emptyChatGPTStreamState
                    finished <- newEmptyMVar
                    withAsync
                        (receiveChatGPTDictation
                            connection
                            state
                            finished
                            onTranscript)
                        \receiver ->
                            runCapture state finished
                                `finally` do
                                    cancel receiver
                                    ignoreSynchronousException $
                                        WS.sendClose
                                            connection
                                            ("done" :: Text)
  where
    runCapture state finished = do
        sendQueuedChatGPTAudio connection finished audio
        sendChatGPTMessage
            finished
            connection
            chatGPTSessionCloseMessage
        Timeout.timeout
            (8 * 1_000_000)
            (takeMVar finished) >>= \case
                Nothing ->
                    pure $ ChatGPTStreamUnavailable
                        "ChatGPT dictation stream timed out while closing"
                Just (Left message) ->
                    pure (ChatGPTStreamUnavailable message)
                Just (Right ()) -> do
                    current <- readMVar state
                    let transcript =
                            Text.strip
                                (renderChatGPTTranscript current)
                    pure $
                        if Text.null transcript
                            then ChatGPTStreamUnavailable
                                "ChatGPT dictation stream produced no text"
                            else ChatGPTStreamSucceeded transcript

sendQueuedChatGPTAudio
    :: WS.Connection
    -> MVar (Either Text ())
    -> Chan (Maybe BS.ByteString)
    -> IO ()
sendQueuedChatGPTAudio connection finished audio =
    readChan audio >>= \case
        Nothing ->
            pure ()
        Just bytes -> do
            tryReadMVar finished >>= \case
                Just _ ->
                    pure ()
                Nothing ->
                    sendChatGPTMessage
                        finished
                        connection
                        (chatGPTAudioAppendMessage bytes)
            sendQueuedChatGPTAudio connection finished audio

sendChatGPTMessage
    :: MVar (Either Text ())
    -> WS.Connection
    -> Text
    -> IO ()
sendChatGPTMessage finished connection message =
    tryAny (WS.sendTextData connection message) >>= \case
        Left err
            | isSyncException err ->
                void $ tryPutMVar finished $
                    Left
                        ("ChatGPT dictation stream send failed: "
                            <> Text.pack (displayException err))
            | otherwise ->
                throwIO err
        Right () ->
            pure ()

awaitChatGPTSessionStarted
    :: WS.Connection
    -> IO (Either Text ())
awaitChatGPTSessionStarted connection = do
    bytes <- WS.receiveData connection
    case decodeChatGPTDictationEvent bytes of
        Left message ->
            pure $ Left
                ("ChatGPT dictation stream returned an invalid event: "
                    <> Text.pack message)
        Right ChatGPTSessionStarted ->
            pure (Right ())
        Right (ChatGPTTranscriptFailed message) ->
            pure (Left message)
        Right (ChatGPTSessionError True message) ->
            pure (Left message)
        Right _ ->
            awaitChatGPTSessionStarted connection

receiveChatGPTDictation
    :: WS.Connection
    -> MVar ChatGPTStreamState
    -> MVar (Either Text ())
    -> (Text -> IO ())
    -> IO ()
receiveChatGPTDictation connection state finished onTranscript =
    tryAny loop >>= \case
        Left err
            | isSyncException err ->
                void $ tryPutMVar finished (receiverFailure err)
            | otherwise ->
                throwIO err
        Right result ->
            void (tryPutMVar finished result)
  where
    loop = do
        bytes <- WS.receiveData connection
        case decodeChatGPTDictationEvent bytes of
            Left message ->
                pure $ Left
                    ("ChatGPT dictation stream returned an invalid event: "
                        <> Text.pack message)
            Right event ->
                case event of
                    ChatGPTSessionUpdated status
                        | status == "closed" ->
                            pure (Right ())
                    ChatGPTTranscriptFailed message ->
                        pure (Left message)
                    ChatGPTSessionError True message ->
                        pure (Left message)
                    ChatGPTTranscriptDelta _ _ ->
                        updateTranscript event >> loop
                    ChatGPTTranscriptSegment _ _ ->
                        updateTranscript event >> loop
                    ChatGPTTranscriptFinal _ _ ->
                        updateTranscript event >> loop
                    _ -> do
                        modifyMVar state \previous ->
                            pure
                                ( applyChatGPTDictationEvent
                                    event
                                    previous
                                , ()
                                )
                        loop
    updateTranscript event = do
        current <- modifyMVar state \previous ->
            let next =
                    applyChatGPTDictationEvent
                        event
                        previous
            in pure (next, next)
        let transcript = renderChatGPTTranscript current
        unless (Text.null transcript) $
            ignoreSynchronousException
                (onTranscript transcript)

receiverFailure :: SomeException -> Either Text ()
receiverFailure err =
    case (fromException err :: Maybe WS.ConnectionException) of
        Just (WS.CloseRequest 1000 _) ->
            Right ()
        _ ->
            Left
                ("ChatGPT dictation stream receive failed: "
                    <> Text.pack (displayException err))

applyChatGPTDictationEvent
    :: ChatGPTDictationEvent
    -> ChatGPTStreamState
    -> ChatGPTStreamState
applyChatGPTDictationEvent event state =
    case event of
        ChatGPTSpeechStarted utteranceId ->
            ensureChatGPTUtterance utteranceId state
        ChatGPTSpeechStopped utteranceId ->
            ensureChatGPTUtterance utteranceId state
        ChatGPTTranscriptDelta utteranceId text ->
            appendChatGPTTranscript utteranceId text state
        ChatGPTTranscriptSegment utteranceId text ->
            appendChatGPTTranscript utteranceId text state
        ChatGPTTranscriptFinal utteranceId text ->
            let withUtterance =
                    ensureChatGPTUtterance utteranceId state
            in withUtterance
                { textByUtterance =
                    Map.insert
                        utteranceId
                        text
                        withUtterance.textByUtterance
                }
        _ ->
            state

appendChatGPTTranscript
    :: Text
    -> Text
    -> ChatGPTStreamState
    -> ChatGPTStreamState
appendChatGPTTranscript utteranceId text state =
    let withUtterance = ensureChatGPTUtterance utteranceId state
    in withUtterance
        { textByUtterance =
            Map.insertWith
                (\new previous -> previous <> new)
                utteranceId
                text
                withUtterance.textByUtterance
        }

ensureChatGPTUtterance
    :: Text
    -> ChatGPTStreamState
    -> ChatGPTStreamState
ensureChatGPTUtterance utteranceId state
    | Map.member utteranceId state.textByUtterance =
        state
    | otherwise =
        state
            { utteranceOrder = state.utteranceOrder <> [utteranceId]
            , textByUtterance =
                Map.insert utteranceId "" state.textByUtterance
            }

renderChatGPTTranscript :: ChatGPTStreamState -> Text
renderChatGPTTranscript state =
    Text.unwords
        [ transcript
        | utteranceId <- state.utteranceOrder
        , Just transcript <- [Map.lookup
            utteranceId
            state.textByUtterance]
        , not (Text.null (Text.strip transcript))
        ]

chatGPTSessionStartMessage :: Int -> Text
chatGPTSessionStartMessage sampleRate =
    encodeText $ Aeson.object
        [ "type" .= ("session.start" :: Text)
        , "config" .= Aeson.object
            [ "input_audio_format" .= ("pcm16" :: Text)
            , "sample_rate_hz" .= sampleRate
            , "num_channels" .= (1 :: Int)
            , "max_buffer_size_bytes" .= (4 * 1024 * 1024 :: Int)
            , "max_utterance_duration_ms" .= (30_000 :: Int)
            , "session_ttl_ms" .= (300_000 :: Int)
            , "provider_mode" .= ("streaming_sse" :: Text)
            , "transcript_delivery_mode" .= ("delta" :: Text)
            , "vad" .= Aeson.object
                [ "type" .= ("server_vad" :: Text)
                , "threshold" .= (0.5 :: Double)
                , "prefix_padding_ms" .= (300 :: Int)
                , "silence_duration_ms" .= (500 :: Int)
                ]
            ]
        ]

chatGPTAudioAppendMessage :: BS.ByteString -> Text
chatGPTAudioAppendMessage bytes =
    encodeText $ Aeson.object
        [ "type" .= ("audio.append" :: Text)
        , "audio" .= Text.decodeUtf8 (Base64.encode bytes)
        ]

chatGPTSessionCloseMessage :: Text
chatGPTSessionCloseMessage =
    encodeText $ Aeson.object
        [ "type" .= ("session.close" :: Text)
        ]

runChatGPTWebSocket
    :: ChatGPTStreamEndpoint
    -> Credential
    -> WS.ClientApp a
    -> IO a
runChatGPTWebSocket endpoint credential client =
    if endpoint.streamSecure
        then
            Wuss.runSecureClientWith
                endpoint.streamHost
                (fromIntegral endpoint.streamPort)
                endpoint.streamPath
                WS.defaultConnectionOptions
                headers
                client
        else
            WS.runClientWith
                endpoint.streamHost
                endpoint.streamPort
                endpoint.streamPath
                WS.defaultConnectionOptions
                headers
                client
  where
    headers =
        [ -- Codex Desktop loads its renderer from @app://-/index.html@.
          -- ChatGPT validates that renderer origin during the upgrade.
          ("Origin", "app://-")
        -- Cloudflare challenges WebSocket upgrades without a browser-shaped
        -- user agent. Keep our own product token for attribution.
        , ( "User-Agent"
          , "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) \
            \AppleWebKit/537.36 (KHTML, like Gecko) Chrome/142.0.0.0 \
            \Safari/537.36 haskell-agent/0.1"
          )
        , ( "Sec-WebSocket-Protocol"
          , BS.intercalate
                ", "
                [ "chatgpt-dictation"
                , "openai-bearer."
                    <> Text.encodeUtf8 credential.accessToken
                , "codex-desktop"
                ]
          )
        ]

chatGPTStreamEndpoint :: Text -> Either Text ChatGPTStreamEndpoint
chatGPTStreamEndpoint baseUrl = do
    uri <- maybe
        (Left "ChatGPT transcription base URL is invalid")
        Right
        (parseURI (Text.unpack baseUrl))
    authority <- maybe
        (Left "ChatGPT transcription base URL has no authority")
        Right
        uri.uriAuthority
    secure <- case uri.uriScheme of
        "https:" -> Right True
        "http:" -> Right False
        _ -> Left "ChatGPT transcription base URL must use HTTP or HTTPS"
    port <- case authority.uriPort of
        "" ->
            Right (if secure then 443 else 80)
        ':' : digits ->
            case readMaybe digits of
                Just value
                    | value > 0 && value <= 65_535 ->
                        Right value
                _ ->
                    Left "ChatGPT transcription base URL has an invalid port"
        _ ->
            Left "ChatGPT transcription base URL has an invalid port"
    if null authority.uriRegName
        then Left "ChatGPT transcription base URL has no host"
        else
            Right ChatGPTStreamEndpoint
                { streamHost = authority.uriRegName
                , streamPort = port
                , streamPath =
                    dropTrailingSlashes uri.uriPath
                        <> "/dictation/stream"
                , streamSecure = secure
                }

dropTrailingSlashes :: String -> String
dropTrailingSlashes =
    reverse . dropWhile (== '/') . reverse

ignoreSynchronousException :: IO () -> IO ()
ignoreSynchronousException action =
    tryAny action >>= \case
        Left err
            | isSyncException err ->
                pure ()
            | otherwise ->
                throwIO err
        Right () ->
            pure ()

-- | Wrap mono signed PCM16 little-endian samples in a standard WAV container.
-- Codex's former open-source TUI used this exact 24 kHz WAV shape for both the
-- ChatGPT backend and public audio-transcriptions endpoint.
encodePcm16Wav :: Int -> BS.ByteString -> Either ApiError LBS.ByteString
encodePcm16Wav sampleRate pcm
    | sampleRate <= 0 =
        Left (ConnectionError "OpenAI dictation has an invalid sample rate")
    | BS.null pcm =
        Left (ConnectionError "OpenAI dictation captured no audio")
    | odd dataLength =
        Left (ConnectionError
            "OpenAI dictation captured a truncated PCM16 sample")
    | dataLength > maxWavDataLength =
        Left (ConnectionError "OpenAI dictation audio is too large for WAV")
    | otherwise =
        Right $ Builder.toLazyByteString $ mconcat
            [ Builder.byteString "RIFF"
            , Builder.word32LE (fromIntegral (36 + dataLength))
            , Builder.byteString "WAVE"
            , Builder.byteString "fmt "
            , Builder.word32LE 16
            , Builder.word16LE 1
            , Builder.word16LE 1
            , Builder.word32LE (fromIntegral sampleRate)
            , Builder.word32LE (fromIntegral (sampleRate * bytesPerSample))
            , Builder.word16LE (fromIntegral bytesPerSample)
            , Builder.word16LE 16
            , Builder.byteString "data"
            , Builder.word32LE (fromIntegral dataLength)
            , Builder.byteString pcm
            ]
  where
    dataLength = BS.length pcm
    bytesPerSample = 2
    maxWavDataLength = fromIntegral (maxBound :: Word32) - 36

postChatGPTTranscription
    :: Text
    -> Credential
    -> BS.ByteString
    -> LBS.ByteString
    -> IO (Either ApiError Text)
postChatGPTTranscription baseUrl credential boundary body = do
    let contentType = "multipart/form-data; boundary=" <> boundary
        endpoint =
            Text.unpack
                (Text.dropWhileEnd (== '/') baseUrl <> "/transcribe")
        accountHeader request
            | Text.null (Text.strip credential.accountId) = request
            | otherwise =
                setRequestHeader
                    "ChatGPT-Account-Id"
                    [Text.encodeUtf8 credential.accountId]
                    request
    tryAny
        (do
            request <- parseRequest endpoint
            httpLBS
                $ setRequestMethod "POST"
                $ setRequestBodyLBS body
                $ setRequestHeader "Content-Type" [contentType]
                $ setRequestHeader "Accept" ["application/json"]
                $ setRequestHeader "Originator" ["haskell-agent"]
                $ setRequestHeader "User-Agent" ["haskell-agent"]
                $ setRequestHeader
                    "Authorization"
                    ["Bearer " <> Text.encodeUtf8 credential.accessToken]
                $ accountHeader request) >>= \case
                    Left err
                        | isSyncException err ->
                            pure $ Left $ ConnectionError
                                ("ChatGPT transcription request failed: "
                                    <> Text.pack (displayException err))
                        | otherwise ->
                            throwIO err
                    Right response -> do
                        let status = getResponseStatusCode response
                            responseBody = getResponseBody response
                        pure $
                            if status >= 200 && status < 300
                                then decodeTranscriptionResponse responseBody
                                else Left $ HttpError status
                                    (responseBodyPreview responseBody)

transcriptionBoundary :: IO BS.ByteString
transcriptionBoundary = do
    unique <- hashUnique <$> newUnique
    pure $ BS8.pack
        ("----haskell-agent-transcribe-"
            <> show (abs (toInteger unique)))

multipartWavBody :: BS.ByteString -> LBS.ByteString -> LBS.ByteString
multipartWavBody boundary wav =
    LBS.fromStrict
        ( "--" <> boundary <> "\r\n"
        <> "Content-Disposition: form-data; name=\"file\"; filename=\"audio.wav\"\r\n"
        <> "Content-Type: audio/wav\r\n\r\n"
        )
        <> wav
        <> LBS.fromStrict
            ("\r\n--" <> boundary <> "--\r\n")

transcriptionResponseDecoder :: Json.Decoder Text
transcriptionResponseDecoder =
    Json.object (Json.atKey "text" Json.text)

decodeTranscriptionResponse
    :: LBS.ByteString
    -> Either ApiError Text
decodeTranscriptionResponse body =
    case Json.decodeEither transcriptionResponseDecoder (LBS.toStrict body) of
        Left err ->
            Left $ JsonDecodeError
                ("Invalid ChatGPT transcription response: "
                    <> err.jsonErrorMessage)
                (responseBodyPreview body)
        Right transcript
            | Text.null (Text.strip transcript) ->
                Left $ ConnectionError
                    "ChatGPT transcription produced no text"
            | otherwise ->
                Right (Text.strip transcript)

responseBodyPreview :: LBS.ByteString -> Text
responseBodyPreview =
    Text.take 2000
        . Text.decodeUtf8With Text.lenientDecode
        . LBS.toStrict

transcribe
    :: Credential
    -> ((BS.ByteString -> IO ()) -> IO ())
    -> (Text -> IO ())
    -> IO Text
transcribe credential produceAudio onTranscript =
    Wuss.runSecureClientWith
        "api.openai.com"
        443
        "/v1/realtime"
        WS.defaultConnectionOptions
        [ ("Authorization"
          , "Bearer " <> Text.encodeUtf8 credential.accessToken)
        ]
        \connection -> Json.withDecoderSession \decoderSession -> do
            WS.sendTextData connection sessionUpdateMessage
            awaitSessionUpdated decoderSession connection
            state <- newMVar emptyTranscriptState
            finished <- newEmptyMVar
            withAsync
                (receiveTranscripts
                    decoderSession
                    connection
                    state
                    finished
                    onTranscript)
                \receiver ->
                    (do
                        produceAudio \bytes ->
                            WS.sendTextData connection (audioAppendMessage bytes)
                        WS.sendTextData connection audioCommitMessage
                        waitForTranscript state finished)
                        `finally` do
                            void (tryAny
                                (WS.sendClose connection ("done" :: Text)))
                            cancel receiver

sessionUpdateMessage :: Text
sessionUpdateMessage =
    encodeText $ Aeson.object
        [ "type" .= ("session.update" :: Text)
        , "session" .= Aeson.object
            [ "type" .= ("transcription" :: Text)
            , "audio" .= Aeson.object
                [ "input" .= Aeson.object
                    [ "format" .= Aeson.object
                        [ "type" .= ("audio/pcm" :: Text)
                        , "rate" .= openAITranscriptionSampleRate
                        ]
                    , "transcription" .= Aeson.object
                        [ "model" .= openAITranscriptionModel
                        ]
                    , "turn_detection" .= Aeson.Null
                    ]
                ]
            ]
        ]

audioAppendMessage :: BS.ByteString -> Text
audioAppendMessage bytes =
    encodeText $ Aeson.object
        [ "type" .= ("input_audio_buffer.append" :: Text)
        , "audio" .= Text.decodeUtf8 (Base64.encode bytes)
        ]

audioCommitMessage :: Text
audioCommitMessage =
    encodeText $ Aeson.object
        [ "type" .= ("input_audio_buffer.commit" :: Text)
        ]

encodeText :: Aeson.Value -> Text
encodeText = Text.decodeUtf8 . LBS.toStrict . Aeson.encode

awaitSessionUpdated :: Json.DecoderSession -> WS.Connection -> IO ()
awaitSessionUpdated decoderSession connection = do
    updated <- Timeout.timeout (10 * 1_000_000) loop
    case updated of
        Nothing ->
            fail "timed out waiting for OpenAI Realtime session.updated"
        Just () ->
            pure ()
  where
    loop = do
        bytes <- WS.receiveData connection
        Json.decodeIO
            decoderSession
            transcriptEventDecoder
            (LBS.toStrict bytes) >>= \case
            Right SessionUpdated -> pure ()
            Right TranscriptError{transcriptMessage} ->
                fail (Text.unpack transcriptMessage)
            _ -> loop

receiveTranscripts
    :: Json.DecoderSession
    -> WS.Connection
    -> MVar TranscriptState
    -> MVar (Either SomeException ())
    -> (Text -> IO ())
    -> IO ()
receiveTranscripts decoderSession connection state finished onTranscript =
    tryAny loop >>= void . tryPutMVar finished
  where
    loop = do
        bytes <- WS.receiveData connection
        Json.decodeIO
            decoderSession
            transcriptEventDecoder
            (LBS.toStrict bytes) >>= \case
            Left _ -> loop
            Right event -> do
                current <- modifyMVar state \previous ->
                    let next = applyTranscriptEvent event previous
                    in pure (next, next)
                case event of
                    TranscriptDelta{} ->
                        notify current
                    TranscriptCompleted{} ->
                        notify current
                    _ ->
                        pure ()
                case event of
                    TranscriptCompleted{} -> pure ()
                    TranscriptError{} -> pure ()
                    _ -> loop
    notify current =
        void (tryAny (onTranscript (renderTranscript current)))

applyTranscriptEvent :: TranscriptEvent -> TranscriptState -> TranscriptState
applyTranscriptEvent event state =
    case event of
        SessionUpdated ->
            state
        TranscriptDelta{transcriptText} ->
            state { partial = state.partial <> transcriptText }
        TranscriptCompleted{transcriptText} ->
            state
                { completed = Just transcriptText
                , partial = ""
                }
        TranscriptError{transcriptMessage} ->
            state { failure = Just transcriptMessage }
        TranscriptUnknown ->
            state

waitForTranscript
    :: MVar TranscriptState
    -> MVar (Either SomeException ())
    -> IO Text
waitForTranscript state finished = do
    completedInTime <- Timeout.timeout (30 * 1_000_000) (takeMVar finished)
    case completedInTime of
        Nothing ->
            fail "timed out waiting for OpenAI Realtime transcription"
        Just (Left err) ->
            throwIO err
        Just (Right ()) -> do
            current <- readMVar state
            case current.failure of
                Just message ->
                    fail (Text.unpack message)
                Nothing ->
                    let transcript = Text.strip (renderTranscript current)
                    in if Text.null transcript
                        then fail "OpenAI transcription produced no text"
                        else pure transcript

renderTranscript :: TranscriptState -> Text
renderTranscript state =
    maybe state.partial id state.completed
