module Agent.XAI.TranscriptionSpec (spec) where

import Agent.XAI.Transcription
import Agent.XAI.TestSupport (requireLoopbackListener)
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (cancel, wait, withAsync)
import Control.Exception.Safe (bracket, finally, throwString)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Network.Socket as Socket
import qualified Network.WebSockets as WS
import qualified System.Timeout as Timeout
import System.IO.Error (ioeGetErrorString)
import Test.Hspec

spec :: Spec
spec = describe "xAI transcription events" do
    it "replaces partial hypotheses rather than appending them" do
        callbacks <- newIORef []
        withTranscriptServer
            [ "{\"type\":\"transcript.partial\",\"text\":\"hel\"}"
            , "{\"type\":\"transcript.partial\",\"text\":\"hello\"}"
            , "{\"type\":\"transcript.partial\",\"text\":\"hello world\",\"speech_final\":true}"
            , "{\"type\":\"transcript.done\",\"text\":\"\"}"
            ]
            (\connection -> transcribeOnConnection connection (const (pure ()))
                (\text -> modifyIORef' callbacks (<> [text])))
            `shouldReturn` "hello world"
        readIORef callbacks `shouldReturn` ["hel", "hello", "hello world", "hello world"]

    it "rejects readiness errors before producing audio" do
        produced <- newIORef False
        withRawTranscriptServer
            (\connection -> WS.sendTextData connection
                ("{\"type\":\"error\",\"message\":\"not ready\"}" :: Text))
            (\connection ->
                transcribeOnConnection connection
                    (const (modifyIORef' produced (const True)))
                    (const (pure ()))
                    `shouldThrow` (\err -> ioeGetErrorString err == "not ready"))
        readIORef produced `shouldReturn` False

    it "propagates transport closure before completion" do
        withRawTranscriptServer
            (\connection -> do
                WS.sendTextData connection ("{\"type\":\"transcript.created\"}" :: Text)
                _ <- WS.receiveData connection :: IO Text
                WS.sendClose connection ("early" :: Text)
                (WS.receiveData connection :: IO Text) `shouldThrow` anyException)
            (\connection ->
                transcribeOnConnection connection (const (pure ())) (const (pure ()))
                    `shouldThrow` (\(_ :: WS.ConnectionException) -> True))

    it "closes the connection when the audio producer fails" do
        withRawTranscriptServer
            (\connection -> do
                WS.sendTextData connection ("{\"type\":\"transcript.created\"}" :: Text)
                (WS.receiveData connection :: IO Text) `shouldThrow` anyException)
            (\connection ->
                transcribeOnConnection connection (const (fail "producer failed")) (const (pure ()))
                    `shouldThrow` (\err -> ioeGetErrorString err == "producer failed"))

    it "cancels a receiver blocked in a transcript callback" do
        entered <- newEmptyMVar
        blocked <- newEmptyMVar
        withTranscriptServer
            ["{\"type\":\"transcript.partial\",\"text\":\"interim\"}"]
            (\connection ->
                withAsync
                    (transcribeOnConnection connection (const (pure ())) \_ ->
                        putMVar entered () >> takeMVar blocked)
                    \worker -> do
                        takeMVar entered
                        Timeout.timeout (1 * 1_000_000) (cancel worker)
                            `shouldReturn` Just ())

    it "returns accumulated speech on empty done despite malformed events and callback failures" do
        callbacks <- newIORef []
        withTranscriptServer
            [ "{\"type\":\"transcript.partial\",\"text\":\"hello\",\"speech_final\":true}"
            , "invalid JSON"
            , "{\"type\":\"future.event\"}"
            , "{\"type\":\"transcript.partial\",\"text\":\"world\",\"speech_final\":true}"
            , "{\"type\":\"transcript.done\",\"text\":\"\"}"
            ]
            (\connection -> transcribeOnConnection connection (const (pure ())) \text -> do
                modifyIORef' callbacks (<> [text])
                throwString "callback failed")
            `shouldReturn` "hello world"
        readIORef callbacks `shouldReturn` ["hello", "hello world", "hello world"]

    it "prefers nonempty done text over accumulated partials" do
        withTranscriptServer
            [ "{\"type\":\"transcript.partial\",\"text\":\"interim\"}"
            , "{\"type\":\"transcript.done\",\"text\":\" final \"}"
            ]
            (\connection -> transcribeOnConnection connection (const (pure ())) (const (pure ())))
            `shouldReturn` "final"

    it "delivers provider errors through completion instead of returning partial text" do
        withTranscriptServer
            [ "{\"type\":\"transcript.partial\",\"text\":\"interim\"}"
            , "{\"type\":\"error\",\"message\":\"provider failed\"}"
            ]
            (\connection -> transcribeOnConnection connection (const (pure ())) (const (pure ())))
            `shouldThrow` (\err -> ioeGetErrorString err == "provider failed")

    it "decodes partial and final transcript events" do
        decodeTranscriptEvent
            "{\"type\":\"transcript.partial\",\"text\":\"hello\",\"is_final\":false,\"speech_final\":false}"
            `shouldBe` Right TranscriptPartial
                { transcriptText = "hello"
                , transcriptIsFinal = False
                , transcriptSpeechFinal = False
                }
        decodeTranscriptEvent
            "{\"type\":\"transcript.done\",\"text\":\"hello world\",\"duration\":1.2}"
            `shouldBe` Right TranscriptDone
                { transcriptText = "hello world"
                }

    it "ignores forward-compatible server events" do
        decodeTranscriptEvent "{\"type\":\"usage.updated\",\"tokens\":1}"
            `shouldBe` Right TranscriptUnknown

withTranscriptServer :: [Text] -> (WS.Connection -> IO a) -> IO a
withTranscriptServer events action =
    withRawTranscriptServer
        (\connection -> do
            mapM_ (WS.sendTextData connection)
                [ "invalid JSON"
                , "{\"type\":\"future.event\"}"
                , "{\"type\":\"transcript.created\"}"
                :: Text
                ]
            (WS.receiveData connection :: IO Text)
                `shouldReturn` "{\"type\":\"audio.done\"}"
            mapM_ (WS.sendTextData connection) events
            (WS.receiveData connection :: IO Text) `shouldThrow` anyException)
        action

withRawTranscriptServer :: (WS.Connection -> IO ()) -> (WS.Connection -> IO a) -> IO a
withRawTranscriptServer serve action =
    requireLoopbackListener >>
    bracket (WS.makeListenSocket "127.0.0.1" 0) Socket.close \listener -> do
        Socket.SockAddrInet port _ <- Socket.getSocketName listener
        let server = do
                (socket, _) <- Socket.accept listener
                flip finally (Socket.close socket) do
                    pending <- WS.makePendingConnection socket WS.defaultConnectionOptions
                    connection <- WS.acceptRequest pending
                    serve connection
        withAsync server \worker -> do
            result <- Timeout.timeout (5 * 1_000_000) do
                value <- WS.runClient "127.0.0.1" (fromIntegral port) "/" action
                wait worker
                pure value
            case result of
                Nothing -> expectationFailure "transcription timed out" >> fail "timeout"
                Just value -> pure value
