{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Agent.Audio.Transcription
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (withAsync, cancel, waitCatch)
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.IORef
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Transcription request preparation" $ do
        it "preserves admitted endpoint and credentials while refusing redirects" $ do
            let initial = HTTP.defaultRequest
                    { HTTP.host = "gateway.example"
                    , HTTP.path = "/v1/audio/transcriptions"
                    , HTTP.requestHeaders = [("Authorization", "Bearer test")]
                    }
            case transcriptionRequest 100 initial "test-boundary" "dictation" recording of
                Left failure -> expectationFailure (show failure)
                Right request -> do
                    HTTP.host request `shouldBe` "gateway.example"
                    HTTP.path request `shouldBe` "/v1/audio/transcriptions"
                    HTTP.redirectCount request `shouldBe` 0
                    lookup "Authorization" (HTTP.requestHeaders request) `shouldBe` Just "Bearer test"
                    case HTTP.requestBody request of
                        HTTP.RequestBodyBS body ->
                            BS.isInfixOf "\r\n\r\ndictation\r\n" body `shouldBe` True
                        _ -> expectationFailure "Expected strict bounded multipart body"
        it "rejects multipart injection, malformed WAV, and oversized audio" $ do
            transcriptionRequest 100 HTTP.defaultRequest "bad\r\n" "dictation" recording `shouldSatisfy` isLeft
            transcriptionRequest 100 HTTP.defaultRequest "boundary" "bad\r\n" recording `shouldSatisfy` isLeft
            transcriptionRequest 100 HTTP.defaultRequest "boundary" "dictation" "bad" `shouldSatisfy` isLeft
            transcriptionRequest 45 HTTP.defaultRequest "boundary" "dictation" recording `shouldSatisfy` isLeft
    describe "Bounded transcript responses" $ do
        it "accepts the exact body limit and stops at the first excess chunk" $ do
            reader <- bodyReader ["abc", "de"]
            readTranscriptionBody 5 reader `shouldReturn` Right "abcde"
            excess <- bodyReader ["abc", "def", "must not read"]
            readTranscriptionBody 5 excess `shouldReturn` Left TranscriptionResponseTooLarge
            excess `shouldReturn` "must not read"
        it "rejects invalid, blank, NUL, oversized and overlong transcripts" $ do
            decodeTranscript 100 5 "{\"text\":\"Hallo\"}" `shouldBe` Right "Hallo"
            map (decodeTranscript 100 5)
                ["{}", "{\"text\":\"  \"}", "{\"text\":\"\\u0000\"}", "{\"text\":\"abcdef\"}"]
                `shouldSatisfy` all isLeft
            decodeTranscript 5 5 "{\"text\":\"Hallo\"}" `shouldBe` Left TranscriptionResponseTooLarge
    describe "Transcription HTTP transport" $ do
        it "returns status and bounded body without following a redirect" $
            testWithApplication (pure $ \_ respond ->
                respond $ responseLBS status302 [("Location", "/redirected")] "redirect") $ \port -> do
                    manager <- HTTP.newManager HTTP.defaultManagerSettings
                    request <- HTTP.parseRequest ("http://127.0.0.1:" <> show port)
                    result <- performTranscriptionRequest manager 100 request
                    result `shouldBe` Right (status302, "redirect")
        it "bounds non-success response bodies too" $
            testWithApplication (pure $ \_ respond ->
                respond $ responseLBS status500 [] "too large") $ \port -> do
                    manager <- HTTP.newManager HTTP.defaultManagerSettings
                    request <- HTTP.parseRequest ("http://127.0.0.1:" <> show port)
                    performTranscriptionRequest manager 3 request
                        `shouldReturn` Left TranscriptionResponseTooLarge
        it "does not turn asynchronous cancellation into a transcription failure" $ do
            started <- newEmptyMVar
            release <- newEmptyMVar
            testWithApplication (pure $ \_ respond -> do
                putMVar started ()
                takeMVar release
                respond $ responseLBS status200 [] "{}") $ \port -> do
                    manager <- HTTP.newManager HTTP.defaultManagerSettings
                    request <- HTTP.parseRequest ("http://127.0.0.1:" <> show port)
                    withAsync (performTranscriptionRequest manager 100 request) $ \operation -> do
                        takeMVar started
                        cancel operation
                        waitCatch operation >>= (`shouldSatisfy` isLeft)
                        putMVar release ()

recording :: BS.ByteString
recording = "RIFF" <> BS.replicate 4 0 <> "WAVE" <> BS.replicate 40 0

bodyReader :: [BS.ByteString] -> IO HTTP.BodyReader
bodyReader chunks = do
    remaining <- newIORef chunks
    pure $ atomicModifyIORef' remaining $ \current -> case current of
        [] -> ([], "")
        chunk : rest -> (rest, chunk)
