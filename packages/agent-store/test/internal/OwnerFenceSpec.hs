module Main (main) where

import qualified Agent.Store.Postgres.ServerTurn.OwnerFence as OwnerFence
import Agent.Store.Types (StoreError (..))
import Control.Concurrent.Async (cancel, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception.Safe (throwIO)
import Data.Text (Text)
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Hspec

main :: IO ()
main = hspec $ describe "scoped host action locks" do
    it "preserves input order and releases locks on normal return" $
        withSystemTempDirectory "ha-lock" \directory -> do
            withLocks directory (pure . Right)
                `shouldReturn` Right [instanceOne, instanceTwo]
            assertLocksReleased directory

    it "releases earlier locks if a later acquisition returns an error" $
        withSystemTempDirectory "ha-lock" \directory -> do
            result <- OwnerFence.withAvailableExclusiveActionLocks
                directory
                [instanceOne, "not-a-uuid"]
                (\_ -> expectationFailure "unexpected callback" >> pure (Right ()))
            result `shouldSatisfy` \case
                Left (StoreDataError _) -> True
                _ -> False
            assertLocksReleased directory

    it "releases earlier locks if evaluating a later acquisition throws" $
        withSystemTempDirectory "ha-lock" \directory -> do
            OwnerFence.withAvailableExclusiveActionLocks
                directory
                [instanceOne, error "later acquisition failed"]
                (pure . Right)
                `shouldThrow` errorCall "later acquisition failed"
            assertLocksReleased directory

    it "releases every lock when the callback throws" $
        withSystemTempDirectory "ha-lock" \directory -> do
            withLocks directory (\_ -> throwIO (userError "callback failed"))
                `shouldThrow` anyIOException
            assertLocksReleased directory

    it "releases every lock when the callback is cancelled" $
        withSystemTempDirectory "ha-lock" \directory -> do
            started <- newEmptyMVar
            blocked <- newEmptyMVar
            withAsync
                (withLocks directory \_ -> do
                    putMVar started ()
                    () <- takeMVar blocked
                    pure (Right ()))
                \worker -> do
                    timeout 5000000 (takeMVar started) `shouldReturn` Just ()
                    cancel worker
            assertLocksReleased directory

withLocks
    :: FilePath
    -> ([Text] -> IO (Either StoreError a))
    -> IO (Either StoreError a)
withLocks directory =
    OwnerFence.withAvailableExclusiveActionLocks directory [instanceOne, instanceTwo]

assertLocksReleased :: FilePath -> Expectation
assertLocksReleased directory =
    withLocks directory (pure . Right)
        `shouldReturn` Right [instanceOne, instanceTwo]

instanceOne, instanceTwo :: Text
instanceOne = "01999999-0000-7000-8000-000000000003"
instanceTwo = "01999999-0000-7000-8000-000000000004"
