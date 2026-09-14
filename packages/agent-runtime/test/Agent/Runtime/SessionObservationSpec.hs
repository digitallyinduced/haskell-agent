module Agent.Runtime.SessionObservationSpec (spec) where

import Agent.Runtime.Session.Observation
import Agent.Loop (LoopEvent(..), emptyTurnOutput)
import Control.Concurrent (Chan, newChan, readChan, writeChan)
import Control.Concurrent.Async (withAsync)
import Control.Exception.Safe (bracket, throwIO)
import Control.Monad (replicateM_)
import Data.Bits ((.&.))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory
    ( createDirectory, getTemporaryDirectory, removeDirectoryRecursive, removeFile )
import System.FilePath ((</>))
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.IO (hClose, openTempFile)
import System.Posix.Files (createSymbolicLink, fileMode, getFileStatus)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "CLI session observation" do
    it "redacts inline screenshot output before publishing it" $
        withDirectory \directory ->
            withSessionObservationPublisherAt directory "session" \publisher -> do
                beginObservedTurn publisher 0 0 "Request"
                publishObservedLoopEvent publisher
                    (ToolOutputUpdated "tool" "{\"image\":\"data:image/png;base64,secret")
                withObserver directory "session" \frames -> do
                    initial <- nextFrame frames
                    map (.text) initial.events `shouldBe` ["Screenshot omitted from event"]

    it "drains final text and durable completion before closing an attached publisher" $
        withDirectory \directory ->
            withObserver directory "session" \frames -> do
                withSessionObservationPublisherAt directory "session" \publisher -> do
                    beginObservedTurn publisher 0 0 "Request"
                    _ <- nextFrame frames
                    publishObservedLoopEvent publisher (TextDelta "Final text")
                    completeObservedTurn publisher 0 1 False
                completed <- nextFrame frames
                completed.state `shouldBe` ObservationCompleted
                completed.durableTurnCount `shouldBe` 1
                map (.text) completed.events `shouldBe` ["Final text"]

    it "catches up atomically and follows later text without replaying it" $
        withDirectory \directory ->
            withSessionObservationPublisherAt directory "session" \publisher -> do
                beginObservedTurn publisher 7 12 "Initial request"
                publishObservedLoopEvent publisher (TextDelta "Before attachment")
                withObserver directory "session" \frames -> do
                    initial <- nextFrame frames
                    initial.reset `shouldBe` True
                    initial.durableTurnCount `shouldBe` 12
                    initial.generationStart `shouldBe` 7
                    initial.userText `shouldBe` "Initial request"
                    map (.text) initial.events `shouldBe` ["Before attachment"]
                    publishObservedLoopEvent publisher (ReasoningDelta "Reasoning")
                    publishObservedLoopEvent publisher (TextDelta "After attachment")
                    subsequent <- frameContaining frames "After attachment"
                    subsequent.reset `shouldBe` False
                    subsequent.sequence `shouldSatisfy` (> initial.sequence)
                    map (.text) subsequent.events `shouldNotContain` ["Before attachment"]

    it "publishes durable completion and resets projection for the next turn" $
        withDirectory \directory ->
            withSessionObservationPublisherAt directory "session" \publisher -> do
                beginObservedTurn publisher 0 2 "First"
                withObserver directory "session" \frames -> do
                    initial <- nextFrame frames
                    completeObservedTurn publisher 0 3 False
                    completed <- nextFrame frames
                    completed.state `shouldBe` ObservationCompleted
                    completed.durableTurnCount `shouldBe` 3
                    beginObservedTurn publisher 0 3 "Second"
                    next <- nextFrame frames
                    next.turnID `shouldNotBe` initial.turnID
                    next.ownerID `shouldBe` initial.ownerID
                    next.reset `shouldBe` True
                    next.events `shouldBe` []
                    next.userText `shouldBe` "Second"

    it "reports waiting and interrupted durable turns" $
        withDirectory \directory ->
            withSessionObservationPublisherAt directory "session" \publisher -> do
                beginObservedTurn publisher 0 0 "Request"
                withObserver directory "session" \frames -> do
                    _ <- nextFrame frames
                    setObservedWaiting publisher True
                    waiting <- nextFrame frames
                    waiting.state `shouldBe` ObservationWaiting
                    completeObservedTurn publisher 0 1 True
                    interrupted <- nextFrame frames
                    interrupted.state `shouldBe` ObservationInterrupted
                    interrupted.durableTurnCount `shouldBe` 1

    it "bounds retained events and marks an incomplete catch-up explicitly" $
        withDirectory \directory ->
            withSessionObservationPublisherAt directory "session" \publisher -> do
                beginObservedTurn publisher 0 0 "Request"
                replicateM_ 9000 (publishObservedLoopEvent publisher (TextDelta "x"))
                withObserver directory "session" \frames -> do
                    initial <- nextFrame frames
                    initial.truncated `shouldBe` True
                    length initial.events `shouldSatisfy` (<= 8192)
                    initial.reset `shouldBe` True

    it "bounds giant individual payloads" $
        withDirectory \directory ->
            withSessionObservationPublisherAt directory "session" \publisher -> do
                beginObservedTurn publisher 0 0 "Request"
                publishObservedLoopEvent publisher (TextDelta (Text.replicate 100000 "x"))
                withObserver directory "session" \frames -> do
                    initial <- nextFrame frames
                    initial.truncated `shouldBe` True
                    sum (map (Text.length . (.text)) initial.events) `shouldBe` 65536

    it "uses private socket permissions and refuses a competing publisher" $
        withDirectory \directory ->
            withSessionObservationPublisherAt directory "session" \_ -> do
                status <- getFileStatus (observationSocketPath directory "session")
                fileMode status .&. 0o777 `shouldBe` 0o600
                withSessionObservationPublisherAt directory "session" (const (pure ()))
                    `shouldThrow` anyIOException

    it "rejects symbolic-link directories rather than changing their permissions" $
        withDirectory \directory -> do
            let destination = directory </> "destination"
                symbolic = directory </> "symbolic"
            createDirectory destination
            createSymbolicLink destination symbolic
            withSessionObservationPublisherAt symbolic "session" (const (pure ()))
                `shouldThrow` anyIOException

    it "releases disconnected idle subscribers instead of exhausting the worker limit" $
        withDirectory \directory ->
            withSessionObservationPublisherAt directory "session" \publisher -> do
                beginObservedTurn publisher 0 0 "Request"
                replicateM_ 12 $
                    withObserver directory "session" \frames -> do
                        frame <- nextFrame frames
                        frame.userText `shouldBe` "Request"

    it "reconnects to a new owner without replaying the previous owner's turn" $
        withDirectory \directory ->
            withObserver directory "session" \frames -> do
                previousOwner <-
                    withSessionObservationPublisherAt directory "session" \publisher -> do
                        beginObservedTurn publisher 0 0 "First owner"
                        publishObservedLoopEvent publisher (TextDelta "First response")
                        frame <- nextFrame frames
                        pure frame.ownerID
                withSessionObservationPublisherAt directory "session" \publisher -> do
                    beginObservedTurn publisher 0 1 "Second owner"
                    publishObservedLoopEvent publisher (TextDelta "Second response")
                    frame <- nextFrame frames
                    frame.ownerID `shouldNotBe` previousOwner
                    frame.reset `shouldBe` True
                    frame.durableTurnCount `shouldBe` 1
                    map (.text) frame.events `shouldBe` ["Second response"]

    it "does not mistake provider completion for a durable commit" $
        withDirectory \directory ->
            withSessionObservationPublisherAt directory "session" \publisher -> do
                beginObservedTurn publisher 0 5 "Request"
                publishObservedLoopEvent publisher
                    (TurnFinished (emptyTurnOutput "response" [] (Just "Finished")))
                publishObservedLoopEvent publisher (ActivityUpdated "Persisting")
                withObserver directory "session" \frames -> do
                    frame <- nextFrame frames
                    frame.state `shouldBe` ObservationRunning
                    frame.durableTurnCount `shouldBe` 5

    it "does not retry execution when the observed action throws an IO exception" $
        withDirectory \directory ->
            withObservationDirectory directory do
                count <- newIORef (0 :: Int)
                withOptionalSessionObservationPublisher "session" (\_ -> do
                    modifyIORef' count (+ 1)
                    throwIO (userError "Execution failed") :: IO ())
                    `shouldThrow` anyIOException
                readIORef count `shouldReturn` 1

    it "falls back without observation when the endpoint cannot be opened" $
        withDirectory \directory ->
            withObservationDirectory (directory </> replicate 150 'x') $
                withOptionalSessionObservationPublisher "session" (pure . isNothing)
                    `shouldReturn` True

withObserver :: FilePath -> Text -> (Chan SessionObservationFrame -> IO a) -> IO a
withObserver directory sessionID action = do
    frames <- newChan
    withAsync (observeSessionAt directory sessionID \case
        ObservationFrame frame -> writeChan frames frame
        _ -> pure ()) (const (action frames))

nextFrame :: Chan SessionObservationFrame -> IO SessionObservationFrame
nextFrame frames =
    timeout 3000000 (readChan frames) >>= maybe
        (throwIO (userError "Timed out waiting for session observation.")) pure

frameContaining :: Chan SessionObservationFrame -> Text -> IO SessionObservationFrame
frameContaining frames expected = do
    frame <- nextFrame frames
    if expected `elem` map (.text) frame.events
        then pure frame
        else frameContaining frames expected

withDirectory :: (FilePath -> IO a) -> IO a
withDirectory action = do
    temporary <- getTemporaryDirectory
    bracket (do
        (path, handle) <- openTempFile temporary "session"
        hClose handle
        removeFile path
        createDirectory path
        pure path) removeDirectoryRecursive action

withObservationDirectory :: FilePath -> IO a -> IO a
withObservationDirectory directory action =
    bracket
        (lookupEnv variable <* setEnv variable directory)
        (maybe (unsetEnv variable) (setEnv variable))
        (const action)
  where
    variable = "HASKELL_AGENT_OBSERVATION_DIRECTORY"
