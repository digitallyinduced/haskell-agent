module Agent.RetrySpec (spec) where

import Agent.Retry
import Control.Exception.Safe (SomeException, tryAny)
import Control.Retry (constantDelay, limitRetries)
import Data.IORef
import Test.Hspec

spec :: Spec
spec = describe "retryingBeforeCommit" do
    it "normalizes synchronous exceptions into the action error channel" do
        result <- handleSyncExceptions
            (const "normalized")
            (ioError (userError "socket failed"))

        result `shouldBe` (Left "normalized" :: Either String ())

    it "returns the last normalized exception after retries are exhausted" do
        attempts <- newIORef (0 :: Int)
        result <- retryingBeforeCommit
            (constantDelay 0 <> limitRetries 2)
            (const (RetryException "offline"))
            \_markCommitted -> do
                modifyIORef' attempts (+ 1)
                ioError (userError "socket failed")

        result `shouldBe` (Left "offline" :: Either String ())
        readIORef attempts `shouldReturn` 3

    it "does not retry a stopped exception" do
        attempts <- newIORef (0 :: Int)
        result <- retryingBeforeCommit
            (constantDelay 0 <> limitRetries 2)
            (const (StopException "invalid configuration"))
            \_markCommitted -> do
                modifyIORef' attempts (+ 1)
                ioError (userError "invalid")

        result `shouldBe` (Left "invalid configuration" :: Either String ())
        readIORef attempts `shouldReturn` 1

    it "rethrows exceptions raised after the commit boundary" do
        attempts <- newIORef (0 :: Int)
        result <-
            (tryAny $
                retryingBeforeCommit
                    (constantDelay 0 <> limitRetries 2)
                    (const (RetryException "should not be used"))
                    \markCommitted -> do
                        modifyIORef' attempts (+ 1)
                        markCommitted
                        ioError (userError "callback failed"))
                :: IO (Either SomeException (Either String ()))

        result `shouldSatisfy` either (const True) (const False)
        readIORef attempts `shouldReturn` 1
