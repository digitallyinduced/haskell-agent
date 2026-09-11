module Agent.OpenAI.LiveSpec (spec) where

import Agent.OpenAI.Live
import Agent.OpenAI.Live.Delegation
import Agent.OpenAI.Live.Call
import Agent.OpenAI.Live.Signaling
import Agent.Error (ApiError(..))
import Agent.Provider (BillingMode(..), tokenProvider)
import Agent.OpenAI.TestSupport (requireLoopbackListener)
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (withAsync, wait)
import Control.Exception.Safe (bracket, finally)
import Data.IORef (newIORef, readIORef, modifyIORef', atomicModifyIORef')
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isLeft)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.Socket as Socket
import qualified Network.WebSockets as WS
import qualified System.Timeout as Timeout
import qualified System.IO.Streams as Streams
import Test.Hspec

spec :: Spec
spec = describe "Codex Live protocol" do
    it "interrupts old playback without stopping capture and resumes only the new output turn" do
        receiver <- newEmptyMVar
        captured <- newEmptyMVar
        let connect next receive = do
                putMVar receiver receive
                receive LiveStarted
                next >>= putMVar captured
                _ <- next
                pure (Right ())
            devices call = do
                _ <- awaitLiveStarted call
                receive <- takeMVar receiver
                receive (LiveTranscript LiveAssistant False "old answer")
                receive (LiveAudio (BS.pack [1,0]))
                receive (LiveTranscript LiveUser False "wait")
                awaitLivePlaybackReset call 0 `shouldReturn` Just 1
                readLiveAudioGeneration call 0 `shouldReturn` Nothing
                receive (LiveTranscript LiveUser False "please")
                Timeout.timeout 10000 (awaitLivePlaybackReset call 1) `shouldReturn` Nothing
                receive (LiveTranscript LiveAssistant False "old continuation")
                receive (LiveAudio (BS.pack [2,0]))
                receive (LiveTranscript LiveAssistant True "old answer complete")
                receive (LiveAudio (BS.pack [3,0]))
                Timeout.timeout 10000 (readLiveAudioGeneration call 1) `shouldReturn` Nothing
                submitLiveAudio call (BS.pack [4,0]) `shouldReturn` True
                takeMVar captured `shouldReturn` LiveInputAudio (BS.pack [4,0])
                receive (LiveTranscript LiveUser True "wait please")
                receive (LiveTranscript LiveAssistant False "new answer")
                receive (LiveAudio (BS.pack [5,0]))
                readLiveAudioGeneration call 1 `shouldReturn` Just (BS.pack [5,0])
                stopLiveCall call
                awaitLivePlaybackReset call 1 `shouldReturn` Nothing
        Timeout.timeout 2000000 (runLiveCallWith connect (\_ _ -> pure "") (const (pure ())) devices)
            `shouldReturn` Just (Right ())

    it "discards warmup audio without delaying device initialization" do
        deviceReady <- newEmptyMVar
        connected <- newEmptyMVar
        captured <- newEmptyMVar
        let connect next receive = do
                takeMVar deviceReady
                receive LiveStarted
                putMVar connected ()
                next >>= putMVar captured
                next >>= (\_ -> pure (Right ()))
            devices call = do
                mapM_ (\_ -> submitLiveAudioWhenReady call (BS.pack [1,0]) `shouldReturn` True) [1..1000 :: Int]
                putMVar deviceReady ()
                takeMVar connected
                submitLiveAudioWhenReady call (BS.pack [2,0]) `shouldReturn` True
                takeMVar captured `shouldReturn` LiveInputAudio (BS.pack [2,0])
                stopLiveCall call
                submitLiveAudioWhenReady call (BS.pack [3,0]) `shouldReturn` False
        Timeout.timeout 2_000_000 (runLiveCallWith connect (\_ _ -> pure "") (const (pure ())) devices)
            `shouldReturn` Just (Right ())

    it "connects the gateway while the offer is still being prepared" do
        handshake <- newEmptyMVar
        let sdp = "v=0\r\nm=audio 9\r\n"
            server pending = do
                connection <- WS.acceptRequest pending
                putMVar handshake ()
                _ <- WS.receiveData connection :: IO BS.ByteString
                WS.sendTextData connection (Aeson.encode (Aeson.object
                    ["type" Aeson..= ("voice.answer" :: Text.Text), "sdp" Aeson..= sdp]))
                _ <- WS.receiveData connection :: IO BS.ByteString
                pure ()
        result <- Timeout.timeout 2_000_000 $ withServer server \port ->
            withGatewayLiveCallPreparing ("http://127.0.0.1:" <> Text.pack (show port))
                "test-token" defaultLiveConfig (takeMVar handshake >> pure sdp) \_ sideband ->
                    sideband (pure ()) (pure LiveHangUp) (const (pure ()))
        result `shouldBe` Just (Right ())

    it "joins blocked offer preparation when setup is cancelled" do
        preparing <- newEmptyMVar
        released <- newEmptyMVar
        blocked <- newEmptyMVar
        handshake <- newEmptyMVar
        serverFinished <- newEmptyMVar
        let prepare = (putMVar preparing () >> takeMVar blocked) `finally` putMVar released ()
            server pending = do
                _ <- WS.acceptRequest pending
                putMVar handshake ()
                takeMVar serverFinished
        result <- Timeout.timeout 2_000_000 $ withServer server \port -> do
            withAsync (withGatewayLiveCallPreparing ("http://127.0.0.1:" <> Text.pack (show port))
                "test-token" defaultLiveConfig prepare (\_ _ -> fail "Unexpected call")) \_ ->
                    takeMVar preparing >> takeMVar handshake
            takeMVar released
            putMVar serverFinished ()
        result `shouldBe` Just ()

    it "uses one authenticated gateway socket for creation and hangup" do
        let sdp = "v=0\r\nm=audio 9\r\n"
            server pending = do
                WS.requestPath (WS.pendingRequest pending) `shouldBe` "/v1/voice"
                lookup "Authorization" (WS.requestHeaders (WS.pendingRequest pending))
                    `shouldBe` Just "Bearer test-gateway-token"
                connection <- WS.acceptRequest pending
                request <- WS.receiveData connection :: IO BS.ByteString
                Aeson.decodeStrict' request `shouldBe` either (const Nothing) Just (liveCallRequest defaultLiveConfig sdp)
                WS.sendTextData connection (Aeson.encode (Aeson.object
                    ["type" Aeson..= ("voice.answer" :: Text.Text), "sdp" Aeson..= sdp]))
                closing <- WS.receiveData connection :: IO BS.ByteString
                Aeson.decodeStrict' closing `shouldBe` Just sessionClose
        result <- Timeout.timeout 5_000_000 $ withServer server \port ->
            withGatewayLiveCall ("http://127.0.0.1:" <> Text.pack (show port))
                "test-gateway-token" defaultLiveConfig sdp \answer sideband -> do
                    answer `shouldBe` sdp
                    sideband (pure ()) (pure LiveHangUp) (const (pure ()))
        result `shouldBe` Just (Right ())

    it "defaults to a Codex v3 voice rather than a public Realtime voice" do
        defaultLiveConfig.voice `shouldBe` "juniper"

    it "routes gateway voice only to the configured secure origin" do
        gatewayLiveEndpoint "https://gateway.example/base/" `shouldBe`
            Right (True, "gateway.example", 443, "/base/v1/voice")
        gatewayLiveEndpoint "http://127.0.0.1:8080" `shouldBe`
            Right (False, "127.0.0.1", 8080, "/v1/voice")
        mapM_ (\url -> gatewayLiveEndpoint url `shouldSatisfy` isLeft)
            ["http://gateway.example", "https://user:password@gateway.example", "https://gateway.example?token=x",
             "https://gateway.example/#fragment", "https://gateway.example:65536", "file:///voice"]

    it "validates gateway answers and never exposes gateway error messages" do
        decodeGatewayLiveAnswer "{\"type\":\"voice.answer\",\"sdp\":\"v=0\\r\\nm=audio 9\\r\\n\"}"
            `shouldBe` Right "v=0\r\nm=audio 9\r\n"
        decodeGatewayLiveAnswer "{\"error\":{\"code\":\"insufficient_quota\",\"message\":\"secret\"}}"
            `shouldBe` Left "Gateway voice setup rejected: account usage quota exhausted"
        decodeGatewayLiveAnswer (BS.replicate 131073 32) `shouldSatisfy` isLeft

    it "distinguishes quota, rate and access failures without exposing server messages" do
        liveFailureReason "{\"error\":{\"code\":\"insufficient_quota\",\"message\":\"secret\"}}"
            `shouldBe` "account usage quota exhausted"
        liveFailureReason "{\"error\":{\"type\":\"rate_limit_exceeded\"}}"
            `shouldBe` "request rate limit reached"
        liveFailureReason "{\"code\":\"model_not_found\"}"
            `shouldBe` "model or voice access denied"
        liveFailureReason "{\"error\":{\"code\":\"secret\",\"message\":\"secret\"}}"
            `shouldBe` "server returned an unrecognized error code"
        liveFailureReason "<html>secret</html>"
            `shouldBe` "server returned no structured error code"

    it "rejects API billing before acquiring a credential or creating a call" do
        let provider = tokenProvider ApiBilled (\_ -> fail "Credential must not be acquired")
        result <- withCodexLiveCall provider defaultLiveConfig "v=0\r\nm=audio 9\r\n"
            (\_ _ -> fail "Call must not be created")
        result `shouldBe` Left (CredentialError "Voice requires a local ChatGPT sign-in, not an OpenAI API key.")

    it "builds the backend WebRTC request with a model and client delegation" do
        let sdp = "v=0\r\nm=audio 9 UDP/TLS/RTP/SAVPF 111\r\n"
        liveCallRequest defaultLiveConfig sdp `shouldBe` Right (Aeson.object
            [ "sdp" Aeson..= sdp
            , "session" Aeson..= Aeson.object
                [ "model" Aeson..= liveModel
                , "instructions" Aeson..= defaultLiveConfig.instructions
                , "audio" Aeson..= Aeson.object ["output" Aeson..= Aeson.object ["voice" Aeson..= defaultLiveConfig.voice]]
                , "delegation" Aeson..= Aeson.object ["type" Aeson..= ("client" :: Text.Text)]
                ]
            ])

    it "accepts call identifiers but never follows a supplied Location host" do
        parseLiveCallIdentifier "https://untrusted.invalid/v1/live/rtc_test-1?ignored=true"
            `shouldBe` Right "rtc_test-1"
        parseLiveCallIdentifier "12345678-1234-abcd-1234-123456789abc"
            `shouldBe` Right "12345678-1234-abcd-1234-123456789abc"
        mapM_ (\value -> parseLiveCallIdentifier value `shouldSatisfy` isLeft)
            ["", "rtc_", "..", "rtc_%2fother", "rtc_a\r\nHeader: value", Text.replicate 4097 "x"]

    it "bounds SDP while streaming and rejects malformed input" do
        stream <- Streams.fromList ["v=0\r\n", "m=audio 9 UDP/TLS/RTP/SAVPF 111\r\n"]
        readLiveSdp stream `shouldReturn` Right "v=0\r\nm=audio 9 UDP/TLS/RTP/SAVPF 111\r\n"
        oversized <- Streams.fromList [BS.replicate 32_768 65, BS.replicate 32_769 65]
        readLiveSdp oversized >>= (`shouldSatisfy` isLeft)
        mapM_ (\value -> validateLiveSdp value `shouldSatisfy` isLeft)
            ["", "<html>error</html>", "v=0\r\nm=video 9 RTP/AVP 1\r\n", "v=0\r\nm=audio 9\0", BS.pack [255]]

    it "attaches a sideband without replaying session.update or awaiting session.started" do
        finished <- newEmptyMVar
        input <- newIORef [LiveContext Nothing Commentary "attached", LiveHangUp]
        let next = atomicModifyIORef' input (\case
                value : rest -> (rest, value)
                [] -> ([], LiveHangUp))
            server pending = do
                connection <- WS.acceptRequest pending
                firstMessage <- WS.receiveData connection :: IO BS.ByteString
                firstMessage `shouldBe` LBS.toStrict (Aeson.encode (head (contextAppend Nothing Commentary "attached")))
                closing <- WS.receiveData connection :: IO BS.ByteString
                closing `shouldBe` LBS.toStrict (Aeson.encode sessionClose)
                putMVar finished ()
        outcome <- Timeout.timeout 5_000_000 $ withServer server \port -> do
            WS.runClient "127.0.0.1" port "/" \connection ->
                runLiveSidebandConnection connection next (const (pure ()))
            takeMVar finished
        outcome `shouldBe` Just ()

    it "routes microphone and playback through the scoped call without UI audio copies" do
        blocked <- newEmptyMVar
        events <- newIORef []
        played <- newEmptyMVar
        let transport next receive = do
                receive LiveStarted
                next >>= \case
                    LiveInputAudio bytes -> receive (LiveAudio bytes)
                    _ -> fail "Expected microphone samples"
                takeMVar blocked
            devices call = do
                awaitLiveStarted call `shouldReturn` True
                submitLiveAudio call (BS.pack [1, 0]) `shouldReturn` True
                readLiveAudio call >>= putMVar played
        result <- Timeout.timeout 5_000_000 $
            runLiveCallWith transport (\_ _ -> pure "done")
                (\event -> modifyIORef' events (<> [event])) devices
        result `shouldBe` Just (Right ())
        takeMVar played `shouldReturn` Just (BS.pack [1, 0])
        readIORef events `shouldReturn` [LiveStarted]

    it "bounds microphone bytes and rejects writes through a closed call handle" do
        blocked <- newEmptyMVar
        retained <- newEmptyMVar
        let transport _ receive = receive LiveStarted >> takeMVar blocked
            devices call = do
                awaitLiveStarted call `shouldReturn` True
                submitLiveAudio call (BS.singleton 0) `shouldReturn` False
                submitLiveAudio call (BS.replicate 24_002 0) `shouldReturn` False
                submitLiveAudio call (BS.replicate 24_000 0) `shouldReturn` True
                submitLiveAudio call (BS.replicate 24_000 0) `shouldReturn` True
                submitLiveAudio call (BS.pack [0, 0]) `shouldReturn` False
                putMVar retained call
        result <- Timeout.timeout 5_000_000 $
            runLiveCallWith transport (\_ _ -> pure "done") (const (pure ())) devices
        result `shouldBe` Just (Right ())
        call <- takeMVar retained
        submitLiveAudio call (BS.pack [0, 0]) `shouldReturn` False
        readLiveAudio call `shouldReturn` Nothing

    it "joins device cleanup when the transport fails" do
        deviceStarted <- newEmptyMVar
        deviceStopped <- newEmptyMVar
        blocked <- newEmptyMVar
        let failure = ConnectionError "test disconnect"
            transport _ receive = do
                receive LiveStarted
                takeMVar deviceStarted
                pure (Left failure)
            devices call = do
                awaitLiveStarted call `shouldReturn` True
                (putMVar deviceStarted () >> takeMVar blocked)
                    `finally` putMVar deviceStopped ()
        result <- Timeout.timeout 5_000_000 $ do
            outcome <- runLiveCallWith transport (\_ _ -> pure "done") (const (pure ())) devices
            takeMVar deviceStopped
            pure outcome
        result `shouldBe` Just (Left (ConnectionError "test disconnect"))

    it "can hang up while connection establishment is blocked" do
        connecting <- newEmptyMVar
        closed <- newEmptyMVar
        blocked <- newEmptyMVar
        let transport _ _ = (putMVar connecting () >> takeMVar blocked)
                `finally` putMVar closed ()
            devices call = do
                takeMVar connecting
                submitLiveAudio call (BS.pack [0, 0]) `shouldReturn` False
                stopLiveCall call
        result <- Timeout.timeout 5_000_000 $ do
            outcome <- runLiveCallWith transport (\_ _ -> pure "done") (const (pure ())) devices
            takeMVar closed
            pure outcome
        result `shouldBe` Just (Right ())

    it "fails closed rather than blocking the receiver on playback overrun" do
        blocked <- newEmptyMVar
        let transport _ receive = do
                receive LiveStarted
                receive (LiveAudio (BS.replicate 96_002 0))
                takeMVar blocked
        result <- Timeout.timeout 5_000_000 $
            runLiveCallWith transport (\_ _ -> pure "done") (const (pure ()))
                (\_ -> takeMVar blocked >> pure ())
        result `shouldBe` Just (Left (ConnectionError "Voice call stopped because its transport or audio device failed."))

    it "bounds WebSocket frames and assembled messages before decoding" do
        WS.connectionFramePayloadSizeLimit liveConnectionOptions
            `shouldBe` WS.SizeLimit 1_048_576
        WS.connectionMessageDataSizeLimit liveConnectionOptions
            `shouldBe` WS.SizeLimit 1_048_576

    it "correlates worker progress and results and ignores duplicate delegations" do
        messages <- newEmptyMVar
        calls <- newIORef (0 :: Int)
        let delegate task progress = do
                modifyIORef' calls (+ 1)
                task `shouldBe` "inspect tests"
                progress "Running tests"
                pure "Tests passed"
        result <- Timeout.timeout 5_000_000 $
            withLiveDelegation delegate (putMVar messages) (const (pure ())) \receive -> do
                receive (LiveDelegation "d1" "inspect tests")
                takeMVar messages `shouldReturn` LiveContext (Just "d1") Commentary "Running tests"
                takeMVar messages `shouldReturn` LiveContext (Just "d1") Speakable "Tests passed"
                receive (LiveDelegation "d1" "do not repeat")
        result `shouldBe` Just ()
        readIORef calls `shouldReturn` 1

    it "does not block audio on a coding task and joins the worker on hangup" do
        started <- newEmptyMVar
        stopped <- newEmptyMVar
        blocked <- newEmptyMVar
        events <- newIORef []
        let delegate _ _ = (putMVar started () >> takeMVar blocked)
                `finally` putMVar stopped ()
        result <- Timeout.timeout 5_000_000 $ do
            withLiveDelegation delegate (const (pure ())) (\event -> modifyIORef' events (<> [event])) \receive -> do
                receive (LiveDelegation "d1" "inspect tests")
                takeMVar started
                receive (LiveAudio (BS.pack [0, 0]))
            takeMVar stopped
        result `shouldBe` Just ()
        readIORef events `shouldReturn` [LiveDelegation "d1" "inspect tests", LiveAudio (BS.pack [0, 0])]

    it "sanitizes coding failures instead of leaking exception contents" do
        messages <- newEmptyMVar
        result <- Timeout.timeout 5_000_000 $
            withLiveDelegation (\_ _ -> fail "private exception contents") (putMVar messages) (const (pure ())) \receive -> do
                receive (LiveDelegation "d1" "inspect tests")
                takeMVar messages
        result `shouldBe` Just (LiveContext (Just "d1") Speakable "The coding task failed. Check the application for details.")

    it "distinguishes start and update acknowledgements" do
        decodeLiveEvent "{\"type\":\"session.started\"}" `shouldBe` Right LiveStarted
        decodeLiveEvent "{\"type\":\"session.updated\"}" `shouldBe` Right LiveUpdated

    it "decodes the Codex audio field rather than the public API delta field" do
        decodeLiveEvent "{\"type\":\"output_audio.delta\",\"audio\":\"AAA=\"}"
            `shouldBe` Right (LiveAudio (BS.pack [0, 0]))
        decodeLiveEvent "{\"type\":\"output_audio.delta\",\"delta\":\"AAA=\"}"
            `shouldSatisfy` isLeft

    it "rejects malformed audio and incomplete samples" do
        decodeLiveEvent "{\"type\":\"output_audio.delta\",\"audio\":\"!\"}"
            `shouldSatisfy` isLeft
        decodeLiveEvent "{\"type\":\"output_audio.delta\",\"audio\":\"AA==\"}"
            `shouldSatisfy` isLeft

    it "keeps transcript updates separate from authoritative completed turns" do
        decodeLiveEvent "{\"type\":\"input_transcript.added\",\"item\":{\"text\":\"hello\"}}"
            `shouldBe` Right (LiveTranscript LiveUser False "hello")
        decodeLiveEvent "{\"type\":\"turn.done\",\"turn\":{\"role\":\"assistant\",\"transcript\":\"hi\"}}"
            `shouldBe` Right (LiveTranscript LiveAssistant True "hi")

    it "extracts delegation task text and its correlation identifier" do
        decodeLiveEvent
            ("{\"type\":\"delegation.created\",\"item\":{\"type\":\"delegation\","
            <> "\"target\":\"client\",\"id\":\"d1\",\"content\":["
            <> "{\"type\":\"input_text\",\"text\":\"inspect \"},"
            <> "{\"type\":\"image\"},{\"type\":\"input_text\",\"text\":\"tests\"}]}}")
            `shouldBe` Right (LiveDelegation "d1" "inspect tests")

    it "does not route other delegation targets or ordinary tool calls" do
        decodeLiveEvent "{\"type\":\"delegation.created\",\"item\":{\"type\":\"delegation\",\"target\":\"server\"}}"
            `shouldBe` Right LiveUnknown
        decodeLiveEvent "{\"type\":\"response.function_call_arguments.done\",\"name\":\"shell\"}"
            `shouldBe` Right LiveUnknown

    it "fails closed on a delegation missing its identifier or contents" do
        decodeLiveEvent "{\"type\":\"delegation.created\",\"item\":{\"type\":\"delegation\",\"target\":\"client\"}}"
            `shouldSatisfy` isLeft
        decodeLiveEvent "{\"type\":\"delegation.created\",\"item\":{\"type\":\"delegation\",\"target\":\"client\",\"id\":\"d1\",\"content\":[]}}"
            `shouldSatisfy` isLeft

    it "encodes the Codex session update with client delegation and no tool definitions" do
        Aeson.encode (sessionUpdate "instructions" "marin" []) `shouldBe`
            Aeson.encode (json "{\"type\":\"session.update\",\"session\":{\"instructions\":\"instructions\",\"audio\":{\"output\":{\"voice\":\"marin\"}},\"delegation\":{\"type\":\"client\"}}}")

    it "correlates speakable results with their delegation" do
        contextAppend (Just "d1") Speakable "done" `shouldBe`
            [json "{\"type\":\"delegation.context.append\",\"delegation_item_id\":\"d1\",\"channel\":\"speakable\",\"content\":[{\"type\":\"input_text\",\"text\":\"done\"}]}"]

    it "keeps unrelated commentary at session scope" do
        contextAppend Nothing Commentary "working" `shouldBe`
            [json "{\"type\":\"session.context.append\",\"channel\":\"commentary\",\"content\":[{\"type\":\"input_text\",\"text\":\"working\"}]}"]

    it "chunks at UTF-8 boundaries without loss at every byte boundary" do
        mapM_ (\prefix -> do
            let original = Text.replicate prefix "a" <> Text.replicate 500 "🙂é漢"
                chunks = contextChunks original
            Text.concat chunks `shouldBe` original
            mapM_ (\chunk -> do
                Text.null chunk `shouldBe` False
                BS.length (Text.encodeUtf8 chunk) `shouldSatisfy` (<= 500)) chunks
            ) [0 .. 501]
        contextChunks "" `shouldBe` [""]

    it "streams a delegation result and sends session.close on hangup" do
        received <- newIORef []
        task <- newEmptyMVar
        firstInput <- newIORef True
        let server pending = do
                connection <- WS.acceptRequest pending
                initial <- json <$> WS.receiveData connection
                initial `shouldBe` sessionUpdate defaultLiveConfig.instructions defaultLiveConfig.voice []
                WS.sendTextData connection ("{\"type\":\"session.started\"}" :: Text.Text)
                WS.sendTextData connection
                    ("{\"type\":\"delegation.created\",\"item\":{\"type\":\"delegation\",\"target\":\"client\",\"id\":\"d1\",\"content\":[{\"type\":\"input_text\",\"text\":\"inspect tests\"}]}}" :: Text.Text)
                result <- json <$> WS.receiveData connection
                [result] `shouldBe` contextAppend (Just "d1") Speakable "Tests passed"
                closed <- json <$> WS.receiveData connection
                closed `shouldBe` sessionClose
            next = atomicModifyIORef' firstInput (\first -> (False, first)) >>= \case
                True -> takeMVar task >> pure (LiveContext (Just "d1") Speakable "Tests passed")
                False -> pure LiveHangUp
            onEvent event = do
                modifyIORef' received (<> [event])
                case event of
                    LiveDelegation identifier prompt -> putMVar task (identifier, prompt)
                    _ -> pure ()
        result <- Timeout.timeout 5_000_000 $
            withServer server \port ->
                WS.runClient "127.0.0.1" port "/" \connection ->
                    runLiveConnection connection defaultLiveConfig next onEvent
        result `shouldBe` Just ()
        readIORef received `shouldReturn` [LiveStarted, LiveDelegation "d1" "inspect tests"]

    it "cancels the blocked input worker when the server disconnects" do
        blocked <- newEmptyMVar
        inputStarted <- newEmptyMVar
        inputStopped <- newEmptyMVar
        let server pending = do
                connection <- WS.acceptRequest pending
                _ <- WS.receiveData connection :: IO LBS.ByteString
                WS.sendTextData connection ("{\"type\":\"session.started\"}" :: Text.Text)
                takeMVar inputStarted
                WS.sendClose connection ("bye" :: Text.Text)
            next = (putMVar inputStarted () >> takeMVar blocked)
                `finally` putMVar inputStopped ()
        result <- Timeout.timeout 5_000_000 $
            withServer server \port -> do
                WS.runClient "127.0.0.1" port "/" (\connection ->
                    runLiveConnection connection defaultLiveConfig next (const (pure ())))
                    `shouldThrow` anyException
                takeMVar inputStopped
        result `shouldBe` Just ()

withServer :: WS.ServerApp -> (Int -> IO a) -> IO a
withServer server action = do
    requireLoopbackListener
    bracket (WS.makeListenSocket "127.0.0.1" 0) Socket.close \listener -> do
        address <- Socket.getSocketName listener
        port <- case address of
            Socket.SockAddrInet value _ -> pure (fromIntegral value)
            _ -> fail "Unexpected loopback address"
        withAsync (bracket (fst <$> Socket.accept listener) Socket.close \socket ->
            WS.makePendingConnection socket WS.defaultConnectionOptions >>= server) \worker -> do
                result <- action port
                wait worker
                pure result

json :: LBS.ByteString -> Aeson.Value
json bytes = case Aeson.eitherDecode bytes of
    Left failure -> error failure
    Right value -> value
