module Agent.Loop.RecoverySpec (spec) where

import Agent.Loop.Recovery
import Control.Concurrent (threadDelay)
import qualified Control.Exception as Exception
import Data.IORef
import Data.Text (Text)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = do
    describe "native replay checkpoints" do
        it "records recovery before sending and retains only committed output" do
            replay <- newReplayState ([] :: [Int])
            input <- prepareReplayAttempt replay False [1] id
            readResumeState replay `shouldReturn` [1]
            commitReplayResponse replay id input [2]
            prepareReplayAttempt replay True [3] id `shouldReturn` [1, 2, 3]
            readResumeState replay `shouldReturn` [1, 2, 3]
            -- The failed attempt added no response to committed history.
            prepareReplayAttempt replay True [3] id `shouldReturn` [1, 2, 3]

        it "treats full input as authoritative and applies checkpoint cuts" do
            replay <- newReplayState ([] :: [Int])
            commitReplayResponse replay (drop 1) [1] [2]
            prepareReplayAttempt replay True [3] id `shouldReturn` [2, 3]
            prepareReplayAttempt replay False [4] id `shouldReturn` [4]

        it "allows hosts without full history to retain their continuation" do
            replay <- newReplayState (Just ("previous" :: Text), [1 :: Int])
            _ <- prepareReplayAttempt replay True [2 :: Int] (const (Just "previous", [2]))
            readResumeState replay `shouldReturn` (Just "previous", [2])

    describe "connection recovery" do
        it "bounds replacement requests including initial connection failures" do
            attempts <- newIORef (0 :: Int)
            replacements <- newIORef []
            let config = configuration
                    { replacementLimit = 2
                    , beforeReplacement = \number _ -> modifyIORef' replacements (<> [number])
                    }
            result <- runRecoveringConnections config
                (\action -> modifyIORef' attempts (+ 1) >> action ())
                (\_ _ -> pure (Right (ReplaceConnection "closed") :: Either Text (ConnectionOutcome Text ())))
            result `shouldBe` Left "closed"
            readIORef attempts `shouldReturn` 3
            readIORef replacements `shouldReturn` [1, 2]

        it "lets the final attempt delegate to its ordinary error handler" do
            result <- runRecoveringConnections (configuration { replacementLimit = 0 })
                (\action -> action ())
                (\available _ -> do
                    permitted <- available
                    pure (Right (ConnectionFinished permitted)))
            result `shouldBe` Right False

        it "returns source errors without retrying or changing them" do
            result <- runRecoveringConnections configuration
                (\_ -> pure (Left "credentials exhausted"))
                (\_ _ -> pure (Right (ConnectionFinished ())))
            result `shouldBe` Left "credentials exhausted"

        it "shares replacement limits across credential-source retries" do
            connections <- newIORef (0 :: Int)
            let source action = do
                    connection <- atomicModifyIORef' connections (\n -> (n + 1, n + 1))
                    outcome <- action connection
                    case outcome of
                        Left "switch account" -> source action
                        _ -> pure outcome
            result <- runRecoveringConnections (configuration { replacementLimit = 1 })
                source
                (\available connection ->
                    if connection == 1
                        then pure (Left "switch account")
                        else do
                            permitted <- available
                            pure (Right (if permitted
                                then ReplaceConnection "closed"
                                else ConnectionFinished connection)))
            result `shouldBe` Right 3
            readIORef connections `shouldReturn` 3

        it "rethrows unclassified synchronous exceptions" do
            runRecoveringConnections configuration
                (\_ -> ioError (userError "unexpected"))
                (\_ _ -> pure (Right (ConnectionFinished ())))
                `shouldThrow` anyIOException

        it "retries only classified transport exceptions" do
            count <- newIORef (0 :: Int)
            let config = configuration
                    { classifyException = \exception ->
                        case Exception.fromException exception :: Maybe IOError of
                            Just _ -> Just "closed"
                            Nothing -> Nothing
                    }
            result <- runRecoveringConnections config
                (\action -> do
                    number <- atomicModifyIORef' count (\n -> (n + 1, n))
                    if number == 0 then ioError (userError "closed") else action ())
                (\_ _ -> pure (Right (ConnectionFinished ())))
            result `shouldBe` Right ()
            readIORef count `shouldReturn` 2

        it "preserves asynchronous timeout exception identity" do
            result <- timeout 10000 $
                runRecoveringConnections configuration
                    (\action -> threadDelay 1000000 >> action ())
                    (\_ _ -> pure (Right (ConnectionFinished ())))
            result `shouldBe` Nothing

configuration :: ConnectionRecovery Text Text
configuration = ConnectionRecovery
    { replacementLimit = 3
    , classifyException = const Nothing
    , exhaustedError = id
    , beforeReplacement = \_ _ -> pure ()
    }
