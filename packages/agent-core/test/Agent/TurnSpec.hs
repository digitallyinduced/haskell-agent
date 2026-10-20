{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, BlockArguments, NumericUnderscores #-}
module Agent.TurnSpec (spec) where

import Prelude
import Agent.Error (ApiError (..), ErrorType (..))
import Agent.Turn
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as Exception
import Control.Retry (RetryPolicyM (..), constantDelay, getRetryPolicyM, limitRetries)
import Data.Either (isLeft)
import Data.IORef
import Data.Text (Text)
import GHC.Clock (getMonotonicTimeNSec)
import Test.Hspec

spec :: Spec
spec = describe "runTurn" do
    it "waits and resubmits capacity errors before returning one successful response" do
        attempts <- newIORef (0 :: Int)
        let submit = do
                attempt <- atomicModifyIORef' attempts (\count -> (count + 1, count))
                pure if attempt < 2 then Left capacityError else Right ("complete" :: Text)
        runTurnWithPolicy immediatePolicy submit `shouldReturn` Right "complete"
        readIORef attempts `shouldReturn` 3

    it "returns the final error after a single shared retry budget" do
        attempts <- newIORef (0 :: Int)
        let submit = modifyIORef' attempts (+ 1) >> pure (Left capacityError :: Either ApiError ())
        runTurnWithPolicy immediatePolicy submit `shouldReturn` Left capacityError
        readIORef attempts `shouldReturn` 4

    it "uses the provider retry hint automatically" do
        attempts <- newIORef False
        submittedAt <- newIORef []
        let submit = do
                now <- getMonotonicTimeNSec
                modifyIORef' submittedAt (<> [now])
                repeated <- atomicModifyIORef' attempts (\prior -> (True, prior))
                pure if repeated then Right () else Left (ProviderError OverloadedError "busy" (Just 1))
        -- A provider hint overrides even a zero-delay policy. Measure the
        -- real wait at the submission boundary, without a timing upper bound.
        runTurnWithPolicy (constantDelay 0 <> limitRetries 1) submit `shouldReturn` Right ()
        times <- readIORef submittedAt
        case times of
            [first, second] -> second - first `shouldSatisfy` (>= 1_000_000_000)
            _ -> expectationFailure "expected two submissions"

    it "cannot override an exhausted policy with a provider retry hint" do
        attempts <- newIORef (0 :: Int)
        let err = ProviderError OverloadedError "busy" (Just 60)
            submit = modifyIORef' attempts (+ 1) >> pure (Left err :: Either ApiError ())
        runTurnWithPolicy (limitRetries 0) submit `shouldReturn` Left err
        readIORef attempts `shouldReturn` 1

    it "returns quota and permanent failures without waiting" do
        let policy = RetryPolicyM (\_ -> expectationFailure "must not retry" >> pure Nothing)
        mapM_ (\err -> runTurnWithPolicy policy (pure (Left err :: Either ApiError ())) `shouldReturn` Left err)
            [ ProviderError UsageLimitReached "quota" Nothing
            , ProviderError AuthenticationError "expired" Nothing
            , ProviderError InvalidRequestError "invalid" Nothing
            , ConnectionError "closed socket"
            ]

    it "can be cancelled during its wait without issuing another submission" do
        waiting <- newEmptyMVar
        attempts <- newIORef (0 :: Int)
        let policy = RetryPolicyM \status -> do
                putMVar waiting ()
                getRetryPolicyM turnRetryPolicy status
            submit = modifyIORef' attempts (+ 1) >> pure (Left capacityError :: Either ApiError ())
        Async.withAsync (runTurnWithPolicy policy submit) \worker -> do
            takeMVar waiting
            Async.cancel worker
            result <- Async.waitCatch worker
            result `shouldSatisfy` isLeft
        readIORef attempts `shouldReturn` 1

    it "does not turn application exceptions into model retries" do
        runTurnWithPolicy immediatePolicy
            (Exception.throwIO (userError "submission interrupted") :: IO (Either ApiError ()))
            `shouldThrow` anyIOException

capacityError :: ApiError
capacityError = ProviderError RateLimitError
    "Codex is at capacity (code: turn_rate_limit_exceeded)" Nothing

-- Preserve the production policy's stopping decisions while skipping delays.
immediatePolicy :: RetryPolicyM IO
immediatePolicy = RetryPolicyM \status ->
    fmap (0 <$) (getRetryPolicyM turnRetryPolicy status)
