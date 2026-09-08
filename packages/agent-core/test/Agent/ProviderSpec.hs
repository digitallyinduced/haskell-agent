module Agent.ProviderSpec (spec) where

import Agent.Error (ApiError(..))
import Agent.Provider
import Control.Concurrent.Async (cancel, waitCatch, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception.Safe (throwIO)
import Control.Monad (forM_)
import Data.Either (isLeft)
import Data.IORef
import Data.Text (Text)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = do
    describe "explicit provider attempt replay safety" do
        forM_ [ReplayUnsafe, ReplayUnknown] \safety ->
            it ("does not let account classification override " <> show safety) do
                (provider, acquisitions) <- countingProvider
                let broadClassifier = withAccountFailureClassifier
                        (\_ _ -> Just AccountAuthenticationRejected) provider
                    err = ConnectionError "effect outcome uncertain"
                result <- runWithTokenProviderAttempt broadClassifier \_ ->
                    pure (Left (ProviderAttemptFailure safety err)
                        :: Either ProviderAttemptFailure ())
                result `shouldBe` Left err
                readIORef acquisitions `shouldReturn` 1

        it "reacquires only for a replay-safe classified failure" do
            (provider, acquisitions) <- countingProvider
            attempts <- newIORef (0 :: Int)
            result <- runWithTokenProviderAttempt provider \_ -> do
                attempt <- atomicModifyIORef' attempts (\n -> (n + 1, n))
                pure $ if attempt == 0
                    then Left (ProviderAttemptFailure ReplaySafe rejected)
                    else Right "done"
            result `shouldBe` Right ("done" :: Text)
            readIORef acquisitions `shouldReturn` 2

        it "does not retry an unclassified error even when replay is safe" do
            (provider, acquisitions) <- countingProvider
            let err = ConnectionError "offline"
            result <- runWithTokenProviderAttempt provider \_ ->
                pure (Left (ProviderAttemptFailure ReplaySafe err)
                    :: Either ProviderAttemptFailure ())
            result `shouldBe` Left err
            readIORef acquisitions `shouldReturn` 1

        it "preserves the bounded account failover budget" do
            (provider, acquisitions) <- countingProvider
            result <- runWithTokenProviderAttempt provider \_ ->
                pure (Left (ProviderAttemptFailure ReplaySafe rejected)
                    :: Either ProviderAttemptFailure ())
            result `shouldBe`
                Left (ConnectionError "token provider failover budget exhausted")
            readIORef acquisitions `shouldReturn` 64

    describe "streaming provider replay boundary" do
        it "allows pre-output rejection and ignores lifecycle-only events" do
            (provider, acquisitions) <- countingProvider
            attempts <- newIORef (0 :: Int)
            events <- newIORef []
            let send _ emit = do
                    attempt <- atomicModifyIORef' attempts (\n -> (n + 1, n))
                    emit False
                    if attempt == 0
                        then pure (Left rejected)
                        else emit True >> pure (Right ())
            result <- runWithTokenProviderStreaming provider id send
                (\event -> modifyIORef' events (<> [event]))
            result `shouldBe` Right ()
            readIORef acquisitions `shouldReturn` 2
            readIORef events `shouldReturn` [False, False, True]

        it "does not replay after output, even if later events are lifecycle-only" do
            (provider, acquisitions) <- countingProvider
            effects <- newIORef (0 :: Int)
            let send _ emit = do
                    emit True
                    emit False
                    pure (Left rejected :: Either ApiError ())
            result <- runWithTokenProviderStreaming provider id send
                (\event -> if event then modifyIORef' effects (+ 1) else pure ())
            result `shouldBe` Left rejected
            readIORef acquisitions `shouldReturn` 1
            readIORef effects `shouldReturn` 1

        it "propagates consumer exceptions without retry" do
            (provider, acquisitions) <- countingProvider
            let send _ emit = emit True >> pure (Right ())
            runWithTokenProviderStreaming provider id send
                (\_ -> throwIO (userError "consumer failed"))
                `shouldThrow` anyIOException
            readIORef acquisitions `shouldReturn` 1

        it "propagates cancellation without credential reacquisition" do
            (provider, acquisitions) <- countingProvider
            entered <- newEmptyMVar
            blocked <- newEmptyMVar
            let send _ emit = emit True >> pure (Right ())
                consume _ = putMVar entered () >> takeMVar blocked
            outcome <- timeout 1000000 $
                withAsync
                    (runWithTokenProviderStreaming provider id send consume)
                    \worker -> do
                        takeMVar entered
                        cancel worker
                        isLeft <$> waitCatch worker
            outcome `shouldBe` Just True
            readIORef acquisitions `shouldReturn` 1

rejected :: ApiError
rejected = HttpError 401 "rejected"

countingProvider :: IO (TokenProvider, IORef Int)
countingProvider = do
    acquisitions <- newIORef 0
    let provider = tokenProvider SubscriptionBilled \_ -> do
            modifyIORef' acquisitions (+ 1)
            pure (Right Credential
                { accessToken = "test"
                , accountId = "test"
                , leaseId = Nothing
                , provider = OpenAIProvider
                })
    pure (provider, acquisitions)
