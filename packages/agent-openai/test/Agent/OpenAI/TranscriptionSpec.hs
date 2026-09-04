module Agent.OpenAI.TranscriptionSpec (spec) where

import Agent.Error (ApiError(..))
import Agent.OpenAI.Transcription
import Agent.OpenAI.TestSupport
    ( requireLoopbackListener
    , withLoopbackApplication
    )
import Agent.Provider
    ( BillingMode(..)
    , Credential(..)
    , FailedCredential(..)
    , Provider(OpenAIProvider)
    , tokenProvider
    )
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (wait, withAsync)
import Control.Exception.Safe (bracket, finally, throwString)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import Data.IORef
    ( atomicModifyIORef'
    , modifyIORef'
    , newIORef
    , readIORef
    )
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Network.HTTP.Types as HTTP
import qualified Network.Socket as Socket
import qualified Network.Wai as Wai
import qualified Network.WebSockets as WS
import qualified System.Timeout as Timeout
import Test.Hspec

spec :: Spec
spec = describe "OpenAI transcription" do
    it "decodes incremental and completed transcript events" do
        decodeTranscriptEvent
            "{\"type\":\"conversation.item.input_audio_transcription.delta\",\"delta\":\"hello \"}"
            `shouldBe` Right TranscriptDelta
                { transcriptText = "hello "
                }
        decodeTranscriptEvent
            ("{\"type\":\"conversation.item.input_audio_transcription.completed\","
                <> "\"transcript\":\"hello world\"}")
            `shouldBe` Right TranscriptCompleted
                { transcriptText = "hello world"
                }

    it "decodes session acknowledgement and nested errors" do
        decodeTranscriptEvent
            "{\"type\":\"session.updated\",\"session\":{\"type\":\"transcription\"}}"
            `shouldBe` Right SessionUpdated
        decodeTranscriptEvent
            ("{\"type\":\"error\",\"error\":{\"type\":\"invalid_request_error\","
                <> "\"message\":\"bad audio\"}}")
            `shouldBe` Right TranscriptError
                { transcriptMessage = "bad audio"
                }
        decodeTranscriptEvent
            "{\"type\":\"error\",\"message\":\"top-level error\"}"
            `shouldBe` Right TranscriptError
                { transcriptMessage = "top-level error"
                }

    it "ignores forward-compatible server events" do
        decodeTranscriptEvent
            "{\"type\":\"input_audio_buffer.committed\",\"event_id\":\"evt\"}"
            `shouldBe` Right TranscriptUnknown

    it "decodes ChatGPT dictation stream events" do
        decodeChatGPTDictationEvent
            ("{\"type\":\"session.updated\",\"sequence_no\":4,"
                <> "\"session\":{\"status\":\"closed\"}}")
            `shouldBe` Right (ChatGPTSessionUpdated "closed")
        decodeChatGPTDictationEvent
            ("{\"type\":\"transcript.delta\",\"sequence_no\":2,"
                <> "\"utterance_id\":\"utterance-1\",\"revision\":1,"
                <> "\"text\":\"hello \"}")
            `shouldBe` Right
                (ChatGPTTranscriptDelta "utterance-1" 1 "hello ")
        decodeChatGPTDictationEvent
            ("{\"type\":\"transcript.final\",\"sequence_no\":3,"
                <> "\"utterance_id\":\"utterance-1\",\"revision\":1,"
                <> "\"text\":\"hello world\"}")
            `shouldBe` Right
                (ChatGPTTranscriptFinal "utterance-1" 1 "hello world")
        decodeChatGPTDictationEvent
            ("{\"type\":\"session.error\",\"sequence_no\":2,\"fatal\":true,"
                <> "\"error\":{\"code\":\"failed\",\"message\":\"bad audio\","
                <> "\"retryable\":false}}")
            `shouldBe` Right (ChatGPTSessionError True "bad audio")

    it "decodes the ChatGPT backend response" do
        decodeTranscriptionResponse "{\"text\":\"  hello world  \"}"
            `shouldBe` Right "hello world"
        decodeTranscriptionResponse "{\"unexpected\":true}"
            `shouldSatisfy` \case
                Left JsonDecodeError{} -> True
                _ -> False

    it "encodes 24 kHz mono PCM16 as WAV" do
        case encodePcm16Wav 24_000 "\x01\x00\x02\x00" of
            Left err ->
                expectationFailure (show err)
            Right wav -> do
                LBS.take 4 wav `shouldBe` "RIFF"
                LBS.take 4 (LBS.drop 8 wav) `shouldBe` "WAVE"
                LBS.take 4 (LBS.drop 24 wav)
                    `shouldBe` "\xC0\x5D\x00\x00"
                LBS.take 4 (LBS.drop 36 wav) `shouldBe` "data"
                LBS.take 4 (LBS.drop 40 wav)
                    `shouldBe` "\x04\x00\x00\x00"
                LBS.drop 44 wav `shouldBe` "\x01\x00\x02\x00"

    it "checks subscription credentials before recording" do
        captures <- newIORef (0 :: Int)
        let provider = tokenProvider SubscriptionBilled \_ ->
                pure (Left (CredentialError "sign in required"))
        transcribePcmWithOpenAI
            provider
            (\_ -> modifyIORef' captures (+ 1))
            (const (pure ()))
            `shouldReturn` Left (CredentialError "sign in required")
        readIORef captures `shouldReturn` 0

    it "streams with Codex Desktop's ChatGPT OAuth protocol" do
        captures <- newIORef (0 :: Int)
        callbacks <- newIORef []
        let credential = openAiCredential "stream-token"
            provider = tokenProvider SubscriptionBilled \_ ->
                pure (Right credential)
            server pending = do
                let request = WS.pendingRequest pending
                WS.requestPath request
                    `shouldBe` "/backend-api/dictation/stream"
                lookup
                    (CI.mk "Origin")
                    (WS.requestHeaders request)
                    `shouldBe` Just "app://-"
                lookup
                    (CI.mk "User-Agent")
                    (WS.requestHeaders request)
                    `shouldSatisfy`
                        maybe False
                            ("haskell-agent/0.1" `BS.isSuffixOf`)
                WS.getRequestSubprotocols request
                    `shouldBe`
                        [ "chatgpt-dictation"
                        , "openai-bearer.stream-token"
                        , "codex-desktop"
                        ]
                connection <- WS.acceptRequestWith
                    pending
                    WS.defaultAcceptRequest
                        { WS.acceptSubprotocol =
                            Just "chatgpt-dictation"
                        }
                start <- WS.receiveData connection
                decodeValue start `shouldBe` Aeson.object
                    [ "type" .= ("session.start" :: Text)
                    , "config" .= Aeson.object
                        [ "input_audio_format" .= ("pcm16" :: Text)
                        , "sample_rate_hz" .= (24_000 :: Int)
                        , "num_channels" .= (1 :: Int)
                        , "max_buffer_size_bytes" .=
                            (4 * 1024 * 1024 :: Int)
                        , "max_utterance_duration_ms" .= (30_000 :: Int)
                        , "session_ttl_ms" .= (300_000 :: Int)
                        , "provider_mode" .= ("streaming_sse" :: Text)
                        , "transcript_delivery_mode" .=
                            ("delta" :: Text)
                        , "vad" .= Aeson.object
                            [ "type" .= ("server_vad" :: Text)
                            , "threshold" .= (0.5 :: Double)
                            , "prefix_padding_ms" .= (300 :: Int)
                            , "silence_duration_ms" .= (500 :: Int)
                            ]
                        ]
                    ]
                Timeout.timeout
                    (1 * 1_000_000)
                    (waitUntil ((> 0) <$> readIORef captures))
                    `shouldReturn` Just ()
                sendEvent connection $ Aeson.object
                    [ "type" .= ("session.started" :: Text)
                    , "sequence_no" .= (1 :: Int)
                    , "session" .= sessionValue "active"
                    ]
                audio <- WS.receiveData connection
                decodeValue audio `shouldBe` Aeson.object
                    [ "type" .= ("audio.append" :: Text)
                    , "audio" .= ("AQACAA==" :: Text)
                    ]
                sendEvent connection $ Aeson.object
                    [ "type" .= ("speech.started" :: Text)
                    , "sequence_no" .= (2 :: Int)
                    , "utterance_id" .= ("utterance-1" :: Text)
                    ]
                sendEvent connection $ Aeson.object
                    [ "type" .= ("transcript.delta" :: Text)
                    , "sequence_no" .= (3 :: Int)
                    , "utterance_id" .= ("utterance-1" :: Text)
                    , "revision" .= (1 :: Int)
                    , "text" .= ("hello wor" :: Text)
                    ]
                sendEvent connection $ Aeson.object
                    [ "type" .= ("transcript.delta" :: Text)
                    , "sequence_no" .= (4 :: Int)
                    , "utterance_id" .= ("utterance-1" :: Text)
                    , "revision" .= (2 :: Int)
                    , "text" .= ("hello world" :: Text)
                    ]
                sendEvent connection $ Aeson.object
                    [ "type" .= ("transcript.final" :: Text)
                    , "sequence_no" .= (5 :: Int)
                    , "utterance_id" .= ("utterance-1" :: Text)
                    , "revision" .= (1 :: Int)
                    , "text" .= ("stale final" :: Text)
                    ]
                sendEvent connection $ Aeson.object
                    [ "type" .= ("transcript.delta" :: Text)
                    , "sequence_no" .= (6 :: Int)
                    , "utterance_id" .= ("utterance-1" :: Text)
                    , "revision" .= (3 :: Int)
                    , "text" .= ("" :: Text)
                    ]
                sendEvent connection $ Aeson.object
                    [ "type" .= ("transcript.final" :: Text)
                    , "sequence_no" .= (7 :: Int)
                    , "utterance_id" .= ("utterance-1" :: Text)
                    , "revision" .= (4 :: Int)
                    , "text" .= ("hello from stream" :: Text)
                    ]
                sendEvent connection $ Aeson.object
                    [ "type" .= ("transcript.delta" :: Text)
                    , "sequence_no" .= (8 :: Int)
                    , "utterance_id" .= ("utterance-1" :: Text)
                    , "revision" .= (5 :: Int)
                    , "text" .= ("late interim" :: Text)
                    ]
                close <- WS.receiveData connection
                decodeValue close `shouldBe` Aeson.object
                    [ "type" .= ("session.close" :: Text)
                    ]
                sendEvent connection $ Aeson.object
                    [ "type" .= ("session.updated" :: Text)
                    , "sequence_no" .= (9 :: Int)
                    , "session" .= sessionValue "closed"
                    ]
        withWebSocketServer server \port -> do
            let baseUrl =
                    "http://127.0.0.1:"
                        <> Text.pack (show port)
                        <> "/backend-api"
                produce send = do
                    modifyIORef' captures (+ 1)
                    send "\x01\x00\x02\x00"
            result <- Timeout.timeout (5 * 1_000_000) $
                transcribePcmWithOpenAIAt
                    baseUrl
                    provider
                    produce
                    (\text -> modifyIORef' callbacks (<> [text]))
            result `shouldBe` Just (Right "hello from stream")
        readIORef captures `shouldReturn` 1
        readIORef callbacks
            `shouldReturn`
                [ "hello wor"
                , "hello world"
                , ""
                , "hello from stream"
                ]

    it "falls back to ChatGPT multipart and reuses audio after a 401" do
        recorded <- newIORef []
        streamProtocols <- newIORef Nothing
        attempts <- newIORef (0 :: Int)
        captures <- newIORef (0 :: Int)
        callbacks <- newIORef []
        let stale = openAiCredential "stale-token"
            fresh = openAiCredential "fresh-token"
            provider = tokenProvider SubscriptionBilled \case
                Nothing ->
                    pure (Right stale)
                Just failed -> do
                    failed.credential `shouldBe` stale
                    pure (Right fresh)
            app request respond =
                if Wai.rawPathInfo request
                    == "/backend-api/dictation/stream"
                    then do
                        modifyIORef' streamProtocols $
                            const $
                                lookup
                                    (CI.mk "Sec-WebSocket-Protocol")
                                    (Wai.requestHeaders request)
                        respond $ Wai.responseLBS
                            HTTP.status404
                            []
                            ""
                    else do
                        body <- Wai.strictRequestBody request
                        let recordedRequest = RecordedTranscription
                                { path = Wai.rawPathInfo request
                                , headers = Wai.requestHeaders request
                                , body
                                }
                        attempt <- atomicModifyIORef' attempts \count ->
                            (count + 1, count)
                        modifyIORef' recorded (<> [recordedRequest])
                        respond $
                            if attempt == 0
                                then Wai.responseLBS
                                    HTTP.status401
                                    [("Content-Type", "application/json")]
                                    "{\"error\":\"expired\"}"
                                else Wai.responseLBS
                                    HTTP.status200
                                    [("Content-Type", "application/json")]
                                    (Aeson.encode (Aeson.object
                                        [ "text" .=
                                            ("hello from oauth" :: Text)
                                        ]))
        withLoopbackApplication (pure app) \port -> do
            let baseUrl =
                    "http://127.0.0.1:"
                        <> Text.pack (show port)
                        <> "/backend-api"
                produce send = do
                    modifyIORef' captures (+ 1)
                    send "\x01\x00\x02\x00"
            transcribePcmWithOpenAIAt
                baseUrl
                provider
                produce
                (\text -> modifyIORef' callbacks (<> [text]))
                `shouldReturn` Right "hello from oauth"

        readIORef captures `shouldReturn` 1
        readIORef callbacks `shouldReturn` ["hello from oauth"]
        readIORef streamProtocols
            `shouldReturn`
                Just
                    "chatgpt-dictation, openai-bearer.stale-token, \
                    \codex-desktop"
        requests <- readIORef recorded
        length requests `shouldBe` 2
        map (.path) requests
            `shouldBe` ["/backend-api/transcribe", "/backend-api/transcribe"]
        map (header "Authorization") requests
            `shouldBe` [Just "Bearer stale-token", Just "Bearer fresh-token"]
        map (header "ChatGPT-Account-Id") requests
            `shouldBe` [Just "account-1", Just "account-1"]
        map (header "Originator") requests
            `shouldBe` [Just "haskell-agent", Just "haskell-agent"]
        map (header "Content-Type") requests
            `shouldSatisfy`
                all (maybe False
                    ("multipart/form-data; boundary="
                        `BS.isPrefixOf`))
        map (.body) requests `shouldSatisfy` \case
            [first, second] ->
                first == second
                    && all
                        (`BS.isInfixOf` LBS.toStrict first)
                        [ "name=\"file\""
                        , "filename=\"audio.wav\""
                        , "Content-Type: audio/wav"
                        , "RIFF"
                        ]
            _ -> False

    it "retries a transient ChatGPT server error with the buffered audio" do
        recorded <- newIORef []
        attempts <- newIORef (0 :: Int)
        captures <- newIORef (0 :: Int)
        callbacks <- newIORef []
        let credential = openAiCredential "subscription-token"
            provider = tokenProvider SubscriptionBilled \_ ->
                pure (Right credential)
            app request respond =
                if Wai.rawPathInfo request
                    == "/backend-api/dictation/stream"
                    then
                        respond $ Wai.responseLBS HTTP.status404 [] ""
                    else do
                        body <- Wai.strictRequestBody request
                        modifyIORef' recorded (<> [body])
                        attempt <- atomicModifyIORef' attempts \count ->
                            (count + 1, count)
                        respond $
                            if attempt == 0
                                then Wai.responseLBS
                                    HTTP.status503
                                    [("Content-Type", "application/json")]
                                    "{\"error\":\"temporarily unavailable\"}"
                                else Wai.responseLBS
                                    HTTP.status200
                                    [("Content-Type", "application/json")]
                                    (Aeson.encode (Aeson.object
                                        [ "text" .=
                                            ("recovered speech" :: Text)
                                        ]))
        withLoopbackApplication (pure app) \port -> do
            let baseUrl =
                    "http://127.0.0.1:"
                        <> Text.pack (show port)
                        <> "/backend-api"
                produce send = do
                    modifyIORef' captures (+ 1)
                    send "\x01\x00\x02\x00"
            result <- Timeout.timeout (8 * 1_000_000) $
                transcribePcmWithOpenAIAt
                    baseUrl
                    provider
                    produce
                    (\text -> modifyIORef' callbacks (<> [text]))
            result `shouldBe` Just (Right "recovered speech")

        readIORef captures `shouldReturn` 1
        readIORef callbacks `shouldReturn` ["recovered speech"]
        requests <- readIORef recorded
        requests `shouldSatisfy` \case
            [first, second] ->
                first == second
                    && "RIFF" `BS.isInfixOf` LBS.toStrict first
            _ -> False
    it "distinguishes local capture failures from unavailable gateway streams" do
        let server pending = do
                connection <- WS.acceptRequest pending
                start <- WS.receiveData connection
                decodeValue start `shouldBe` Aeson.object
                    [ "type" .= ("session.start" :: Text)
                    , "config" .= Aeson.object
                        [ "input_audio_format" .= ("pcm16" :: Text)
                        , "sample_rate_hz" .= (24_000 :: Int)
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
                sendEvent connection $
                    Aeson.object ["type" .= ("session.started" :: Text)]
                threadDelay (2 * 1_000_000)
        withWebSocketServer server \port -> do
            let websocketUrl =
                    "ws://127.0.0.1:"
                        <> Text.pack (show port)
                        <> "/v1/audio/transcriptions"
            result <-
                transcribePcmWithChatGPTStreamAt
                    websocketUrl
                    []
                    (\_ -> throwString "microphone failed")
                    (const (pure ()))
            result `shouldSatisfy` \case
                Left (ChatGPTDictationCaptureFailed message) ->
                    "microphone failed" `Text.isInfixOf` message
                _ -> False

        transcribePcmWithChatGPTStreamAt
            "ftp://gateway.example/v1/audio/transcriptions"
            []
            (\_ -> expectationFailure "capture should not start")
            (const (pure ()))
            `shouldReturn`
                Left
                    (ChatGPTDictationStreamUnavailable
                        "Dictation WebSocket URL must use WS, WSS, HTTP, or HTTPS")
withWebSocketServer
    :: WS.ServerApp
    -> (Int -> IO value)
    -> IO value
withWebSocketServer server action =
    requireLoopbackListener >>
    bracket
        (WS.makeListenSocket "127.0.0.1" 0)
        Socket.close
        \listenSocket -> do
            port <- socketAddressPort <$> Socket.getSocketName listenSocket
            withAsync
                (do
                    (socket, _) <- Socket.accept listenSocket
                    (WS.makePendingConnection
                        socket
                        WS.defaultConnectionOptions >>= server)
                        `finally` Socket.close socket)
                \serverThread -> do
                    result <- action port
                    wait serverThread
                    pure result

socketAddressPort :: Socket.SockAddr -> Int
socketAddressPort = \case
    Socket.SockAddrInet port _ ->
        fromIntegral port
    Socket.SockAddrInet6 port _ _ _ ->
        fromIntegral port
    address ->
        error ("unexpected WebSocket test address: " <> show address)

decodeValue :: LBS.ByteString -> Aeson.Value
decodeValue =
    either error id . Aeson.eitherDecode

sendEvent :: WS.Connection -> Aeson.Value -> IO ()
sendEvent connection =
    WS.sendTextData connection
        . TextEncoding.decodeUtf8
        . LBS.toStrict
        . Aeson.encode

sessionValue :: Text -> Aeson.Value
sessionValue status =
    Aeson.object
        [ "session_id" .= ("session-1" :: Text)
        , "status" .= status
        , "config" .= Aeson.object
            [ "provider_mode" .= ("streaming_sse" :: Text)
            , "transcript_delivery_mode" .= ("final_only" :: Text)
            ]
        ]

waitUntil :: IO Bool -> IO ()
waitUntil condition =
    condition >>= \case
        True ->
            pure ()
        False -> do
            threadDelay 1_000
            waitUntil condition

data RecordedTranscription = RecordedTranscription
    { path :: !BS.ByteString
    , headers :: !HTTP.RequestHeaders
    , body :: !LBS.ByteString
    }

openAiCredential :: Text -> Credential
openAiCredential token = Credential
    { accessToken = token
    , accountId = "account-1"
    , leaseId = Nothing
    , provider = OpenAIProvider
    }

header :: BS.ByteString -> RecordedTranscription -> Maybe BS.ByteString
header name request =
    lookup (CI.mk name) request.headers
