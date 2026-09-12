module Agent.Transcription.ReceiveSpec (spec) where

import Agent.Transcription.Receive
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (cancel, withAsync)
import Control.Concurrent.MVar
import Control.Exception.Safe (finally)
import Data.IORef
import Data.Text (Text)
import System.IO.Error (ioeGetErrorString)
import qualified System.Timeout as Timeout
import Test.Hspec

spec :: Spec
spec = describe "transcription receiver plumbing" do
    it "ignores undecodable and unrelated events until readiness" do
        next <- events [Nothing, Just "unknown", Just "ready", Just "remaining"]
        awaitReady 1000000 "ready timeout" next ready
        next `shouldReturn` Just "remaining"

    it "reports readiness errors without consuming the next event" do
        next <- events [Just "error", Just "ready"]
        awaitReady 1000000 "ready timeout" next ready
            `shouldThrow` message "provider failed"
        next `shouldReturn` Just "ready"

    it "times out and unwinds a blocked readiness read" do
        blocked <- newEmptyMVar
        released <- newEmptyMVar
        awaitReady 10000 "ready timeout"
            (takeMVar blocked `finally` putMVar released ()) ready
            `shouldThrow` message "ready timeout"
        tryTakeMVar released `shouldReturn` Just ()

    it "bounds the entire readiness loop, not each individual read" do
        let next = threadDelay 1000 >> pure (Just "unknown")
        Timeout.timeout 1000000
            (awaitReady 10000 "ready timeout" next ready
                `shouldThrow` message "ready timeout")
            `shouldReturn` Just ()

    it "publishes only the final state after notifications, ignoring synchronous callback failures" do
        next <- events [Nothing, Just "first", Just "done", Just "unread"]
        finished <- newEmptyMVar
        notifications <- newIORef []
        receiveTranscripts next step [] finished \text -> do
            unpublished <- isEmptyMVar finished
            modifyIORef' notifications (<> [(text, unpublished)])
            fail "callback failed"
        waitForCompletion 1000000 "completion timeout" finished
            `shouldReturn` ["first", "done"]
        readIORef notifications `shouldReturn` [("first", True), ("done", True)]
        next `shouldReturn` Just "unread"

    it "delivers a read exception through completion" do
        finished <- newEmptyMVar
        receiveTranscripts (fail "read failed") step [] finished (const (pure ()))
        waitForCompletion 1000000 "completion timeout" finished
            `shouldThrow` message "read failed"

    it "preserves silent updates and silent completion without notifying" do
        next <- events [Just "update", Just "done", Just "unread"]
        finished <- newEmptyMVar
        notifications <- newIORef ([] :: [Text])
        let silent text previous
                | text == "done" = Complete (previous <> [text]) Nothing
                | otherwise = Continue (previous <> [text]) Nothing
        receiveTranscripts next silent [] finished
            (\text -> modifyIORef' notifications (<> [text]))
        waitForCompletion 1000000 "completion timeout" finished
            `shouldReturn` ["update", "done"]
        readIORef notifications `shouldReturn` []
        next `shouldReturn` Just "unread"

    it "does not overwrite or block on an already published result" do
        finished <- newMVar (Right ["existing"])
        Timeout.timeout 1000000
            (receiveTranscripts (pure (Just "done")) step [] finished (const (pure ())))
            `shouldReturn` Just ()
        waitForCompletion 1000000 "completion timeout" finished
            `shouldReturn` ["existing"]

    it "times out waiting for completion without filling the result cell" do
        finished <- newEmptyMVar
        (waitForCompletion 10000 "completion timeout" finished :: IO [Text])
            `shouldThrow` message "completion timeout"
        isEmptyMVar finished `shouldReturn` True

    it "cancels and joins a blocked receiver without publishing cancellation as a result" do
        entered <- newEmptyMVar
        blocked <- newEmptyMVar
        released <- newEmptyMVar
        finished <- newEmptyMVar
        let next = (putMVar entered () >> takeMVar blocked)
                `finally` putMVar released ()
        withAsync (receiveTranscripts next step [] finished (const (pure ()))) \worker -> do
            takeMVar entered
            Timeout.timeout 1000000 (cancel worker) `shouldReturn` Just ()
        tryTakeMVar released `shouldReturn` Just ()
        isEmptyMVar finished `shouldReturn` True

    it "cancels and joins a blocked callback rather than swallowing cancellation" do
        entered <- newEmptyMVar
        blocked <- newEmptyMVar
        released <- newEmptyMVar
        finished <- newEmptyMVar
        let notify _ = (putMVar entered () >> takeMVar blocked)
                `finally` putMVar released ()
        withAsync (receiveTranscripts (pure (Just "done")) step [] finished notify) \worker -> do
            takeMVar entered
            Timeout.timeout 1000000 (cancel worker) `shouldReturn` Just ()
        tryTakeMVar released `shouldReturn` Just ()
        isEmptyMVar finished `shouldReturn` True

ready :: Text -> Maybe (Either Text ())
ready "ready" = Just (Right ())
ready "error" = Just (Left "provider failed")
ready _ = Nothing

step :: Text -> [Text] -> ReceiveStep [Text]
step text previous
    | text == "done" = Complete (previous <> [text]) (Just text)
    | otherwise = Continue (previous <> [text]) (Just text)

events :: [Maybe Text] -> IO (IO (Maybe Text))
events values = do
    ref <- newIORef values
    pure do
        remaining <- readIORef ref
        case remaining of
            [] -> fail "unexpected extra read"
            value : rest -> writeIORef ref rest >> pure value

message :: String -> IOError -> Bool
message expected err = ioeGetErrorString err == expected
