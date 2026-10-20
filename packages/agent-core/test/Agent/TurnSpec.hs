{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, BlockArguments, NumericUnderscores #-}
module Agent.TurnSpec (spec) where

import Prelude
import Agent.Error (ApiError (..), ErrorType (..))
import Agent.Turn
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as Exception
import Data.Either (isLeft)
import Data.IORef
import Data.Text (Text)
import Test.Hspec

spec :: Spec
spec = describe "runTurn" do
    it "waits and resubmits capacity errors before returning one successful response" do
        attempts <- newIORef (0 :: Int)
        waits <- newIORef []
        let runtime = silentRuntime { waitForRetry = \delay -> modifyIORef' waits (<> [delay]) }
            submit = do
                attempt <- atomicModifyIORef' attempts (\count -> (count + 1, count))
                pure if attempt < 2 then Left capacityError else Right ("complete" :: Text)
        runTurnWith runtime submit `shouldReturn` Right "complete"
        readIORef attempts `shouldReturn` 3
        readIORef waits `shouldReturn` [5_000_000, 10_000_000]

    it "returns the final error after a single shared retry budget" do
        attempts <- newIORef (0 :: Int)
        waits <- newIORef []
        let runtime = silentRuntime { waitForRetry = \delay -> modifyIORef' waits (<> [delay]) }
            submit = modifyIORef' attempts (+ 1) >> pure (Left capacityError :: Either ApiError ())
        runTurnWith runtime submit `shouldReturn` Left capacityError
        readIORef attempts `shouldReturn` 4
        readIORef waits `shouldReturn` [5_000_000, 10_000_000, 20_000_000]

    it "uses the provider retry hint automatically" do
        attempts <- newIORef False
        waits <- newIORef []
        let runtime = silentRuntime { waitForRetry = \delay -> modifyIORef' waits (<> [delay]) }
            submit = do
                repeated <- atomicModifyIORef' attempts (\prior -> (True, prior))
                pure if repeated then Right () else Left (ProviderError OverloadedError "busy" (Just 7))
        runTurnWith runtime submit `shouldReturn` Right ()
        readIORef waits `shouldReturn` [7_000_000]

    it "returns quota and permanent failures without waiting" do
        let runtime = silentRuntime { waitForRetry = \_ -> expectationFailure "must not wait" }
        mapM_ (\err -> runTurnWith runtime (pure (Left err :: Either ApiError ())) `shouldReturn` Left err)
            [ ProviderError UsageLimitReached "quota" Nothing
            , ProviderError AuthenticationError "expired" Nothing
            , ProviderError InvalidRequestError "invalid" Nothing
            , ConnectionError "closed socket"
            ]

    it "can be cancelled during its wait without issuing another submission" do
        waiting <- newEmptyMVar
        release <- newEmptyMVar
        attempts <- newIORef (0 :: Int)
        let runtime = silentRuntime { waitForRetry = \_ -> putMVar waiting () >> takeMVar release }
            submit = modifyIORef' attempts (+ 1) >> pure (Left capacityError :: Either ApiError ())
        Async.withAsync (runTurnWith runtime submit) \worker -> do
            takeMVar waiting
            Async.cancel worker
            result <- Async.waitCatch worker
            result `shouldSatisfy` isLeft
        readIORef attempts `shouldReturn` 1

    it "does not turn application exceptions into model retries" do
        runTurnWith silentRuntime
            (Exception.throwIO (userError "submission interrupted") :: IO (Either ApiError ()))
            `shouldThrow` anyIOException

capacityError :: ApiError
capacityError = ProviderError RateLimitError
    "Codex is at capacity (code: turn_rate_limit_exceeded)" Nothing

silentRuntime :: TurnRuntime
silentRuntime = TurnRuntime (\_ -> pure ()) (\_ -> pure ())
