module Agent.Responses.ClientSpec (spec) where

import Agent.Error (ApiError(..))
import Agent.Responses.Client (retryStreamingResultWithPolicy)
import Control.Retry (constantDelay, limitRetries)
import Data.IORef
import Test.Hspec

spec :: Spec
spec = describe "retryStreamingResultWithPolicy" do
    it "uses the caller-supplied retry predicate" do
        attempts <- newIORef (0 :: Int)
        let retryable = \case
                ConnectionError "retry" -> True
                _ -> False
        result <- retryStreamingResultWithPolicy
            (constantDelay 0 <> limitRetries 2)
            retryable
            (\_ -> do
                attempt <- atomicModifyIORef' attempts (\n -> (n + 1, n))
                pure $
                    if attempt == 0
                        then Left (ConnectionError "retry")
                        else Right ("ok" :: String))
            Nothing
        result `shouldBe` Right "ok"
        readIORef attempts `shouldReturn` 2

    it "treats discarded stream events as replay-safe" do
        attempts <- newIORef (0 :: Int)
        result <- retryStreamingResultWithPolicy
            (constantDelay 0 <> limitRetries 2)
            (const True)
            (\emit -> do
                attempt <- atomicModifyIORef' attempts (\n -> (n + 1, n))
                emit ()
                pure $
                    if attempt == 0
                        then Left (ConnectionError "after hidden output")
                        else Right ("ok" :: String))
            Nothing
        result `shouldBe` Right "ok"
        readIORef attempts `shouldReturn` 2

    it "does not retry after callback-visible output" do
        attempts <- newIORef (0 :: Int)
        delivered <- newIORef (0 :: Int)
        let failure = ConnectionError "after output"
        result <- retryStreamingResultWithPolicy
            (constantDelay 0 <> limitRetries 2)
            (const True)
            (\emit -> do
                modifyIORef' attempts (+ 1)
                emit ()
                pure (Left failure :: Either ApiError String))
            (Just (const (modifyIORef' delivered (+ 1))))
        result `shouldBe` Left failure
        readIORef attempts `shouldReturn` 1
        readIORef delivered `shouldReturn` 1
