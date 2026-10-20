{-# LANGUAGE NoImplicitPrelude, BlockArguments, ScopedTypeVariables, OverloadedStrings #-}
module Agent.Transport.SessionResourceSpec (spec) where

import Prelude
import Agent.Transport.SessionResource
import Agent.ResourceScope (ResourceScope, allocateResource, withResourceScope)
import Control.Concurrent (myThreadId)
import Control.Concurrent.MVar
import qualified Control.Exception as Exception
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "session-owned resource" do
    it "acquires lazily, reuses successful requests and closes with the session" do
        events <- newIORef []
        withSessionResource (acquire events) \session -> do
            readIORef events `shouldReturn` []
            runSessionResource session 1 (\_ _ fresh -> pure (Right fresh))
                `shouldReturn` Right True
            runSessionResource session 1 (\_ _ fresh -> pure (Right fresh))
                `shouldReturn` Right False
            readIORef events `shouldReturn` ["open:1"]
        readIORef events `shouldReturn` ["open:1", "close:1"]

    it "closes a failed request before acknowledging it and acquires anew" do
        events <- newIORef []
        withSessionResource (acquire events) \session -> do
            runSessionResource session 1 (\_ _ _ -> pure (Left "failed" :: Either Text ()))
                `shouldReturn` Left "failed"
            readIORef events `shouldReturn` ["open:1", "close:1"]
            currentSessionCredential session `shouldReturn` Nothing
            runSessionResource session 1 (\_ _ fresh -> pure (Right fresh))
                `shouldReturn` Right True

    it "runs callbacks in the caller's thread" do
        caller <- myThreadId
        events <- newIORef []
        withSessionResource (acquire events) \session -> do
            runSessionResource session 1 (\_ _ _ -> Right <$> myThreadId)
                `shouldReturn` Right caller

    it "releases resources acquired before an acquisition failure" do
        events <- newIORef []
        let rejected scope credential = do
                _ <- acquire events scope credential
                pure (Left ("rejected" :: Text))
        withSessionResource rejected \session -> do
            runSessionResource session 1 (\(_ :: ()) _ _ ->
                expectationFailure "must not execute" >> pure (Right ()))
                `shouldReturn` Left "rejected"
            readIORef events `shouldReturn` ["open:1", "close:1"]

    it "attaches retained resources to the caller's scope" do
        events <- newIORef []
        withResourceScope \scope -> do
            session <- newSessionResource scope (acquire events)
            runSessionResource session 1 (\_ _ _ -> pure (Right ())) `shouldReturn` Right ()
            readIORef events `shouldReturn` ["open:1"]
        readIORef events `shouldReturn` ["open:1", "close:1"]

    it "also cancels a later request using the retained resource" do
        events <- newIORef []
        blocker <- newEmptyMVar
        withSessionResource (acquire events) \session -> do
            runSessionResource session 1 (\_ _ _ -> pure (Right ())) `shouldReturn` Right ()
            timeout 100000 (runSessionResource session 1 \_ _ _ ->
                takeMVar blocker >> pure (Right ())) `shouldReturn` Nothing
            readIORef events `shouldReturn` ["open:1", "close:1"]
            runSessionResource session 1 (\_ _ fresh -> pure (Right fresh))
                `shouldReturn` Right True

    it "cancels blocked acquisition and waits for its cleanup" do
        events <- newIORef []
        blocker <- newEmptyMVar
        let blockedAcquire :: ResourceScope -> Int -> IO (Either Text ((), Int))
            blockedAcquire scope credential =
                (takeMVar blocker >> acquire events scope credential)
                    `Exception.finally` modifyIORef' events (<> ["acquisition-stopped"])
        withSessionResource blockedAcquire \session -> do
            timeout 100000 (runSessionResource session 1 \_ _ _ ->
                pure (Right ())) `shouldReturn` Nothing
            readIORef events `shouldReturn` ["acquisition-stopped"]

    it "returns acquisition failures without executing the callback" do
        events <- newIORef []
        let unavailable _ (_ :: Int) = pure (Left ("unavailable" :: Text))
        withSessionResource unavailable \session -> do
            runSessionResource session 1 (\(_ :: ()) _ _ ->
                modifyIORef' events (<> ["unexpected callback"]) >> pure (Right ()))
                `shouldReturn` Left "unavailable"
        readIORef events `shouldReturn` []

    it "releases partial acquisition before a timeout returns" do
        events <- newIORef []
        blocker <- newEmptyMVar
        let blockedAcquire scope credential = do
                acquired <- acquire events scope credential
                takeMVar blocker
                pure acquired
        withSessionResource blockedAcquire \session -> do
            timeout 100000 (runSessionResource session 1 (\_ _ _ -> pure (Right ())))
                `shouldReturn` Nothing
            readIORef events `shouldReturn` ["open:1", "close:1"]
            currentSessionCredential session `shouldReturn` Nothing

    it "resets an idle resource before returning" do
        events <- newIORef []
        withSessionResource (acquire events) \session -> do
            runSessionResource session 1 (\_ _ _ -> pure (Right ())) `shouldReturn` Right ()
            resetSessionResource session
            readIORef events `shouldReturn` ["open:1", "close:1"]
            runSessionResource session 1 (\_ _ fresh -> pure (Right fresh))
                `shouldReturn` Right True

    it "changes credentials only after closing the previous resource" do
        events <- newIORef []
        withSessionResource (acquire events) \session -> do
            runSessionResource session 1 (\_ _ _ -> pure (Right ())) `shouldReturn` Right ()
            runSessionResource session 2 (\_ _ fresh -> pure (Right fresh)) `shouldReturn` Right True
            readIORef events `shouldReturn` ["open:1", "close:1", "open:2"]

    it "accepts a credential refreshed during initial acquisition" do
        events <- newIORef []
        let refreshedAcquire :: ResourceScope -> Int -> IO (Either Text ((), Int))
            refreshedAcquire scope requested =
                fmap (\(resource, _) -> (resource, requested + 1)) <$> acquire events scope requested
        withSessionResource refreshedAcquire \session -> do
            timeout 1000000 (runSessionResource session 1 \_ actual fresh ->
                pure (Right (actual, fresh))) `shouldReturn` Just (Right (2, True))
            currentSessionCredential session `shouldReturn` Just 2
            runSessionResource session 2 (\_ actual fresh -> pure (Right (actual, fresh)))
                `shouldReturn` Right (2, False)
            readIORef events `shouldReturn` ["open:1"]

    it "finishes callback cleanup before a timeout returns" do
        events <- newIORef []
        blocker <- newEmptyMVar
        withSessionResource (acquire events) \session -> do
            result <- timeout 100000 $
                runSessionResource session 1 \_ _ _ ->
                    (takeMVar blocker >> pure (Right ()))
                        `Exception.finally` modifyIORef' events (<> ["callback-stopped"])
            result `shouldBe` Nothing
            readIORef events `shouldReturn`
                ["open:1", "callback-stopped", "close:1"]
            runSessionResource session 1 (\_ _ fresh -> pure (Right fresh))
                `shouldReturn` Right True

    it "preserves callback exception types and closes before rethrowing" do
        events <- newIORef []
        withSessionResource (acquire events) \session -> do
            let request = runSessionResource session 1 \_ _ _ ->
                    Exception.throwIO (userError "callback failure") :: IO (Either Text ())
            request `shouldThrow` (\(_ :: Exception.IOException) -> True)
            readIORef events `shouldReturn` ["open:1", "close:1"]

acquire
    :: IORef [Text]
    -> ResourceScope
    -> Int
    -> IO (Either Text ((), Int))
acquire events scope credential = do
    _ <- allocateResource scope
        (modifyIORef' events (<> ["open:" <> Text.pack (show credential)]))
        (\_ -> modifyIORef' events (<> ["close:" <> Text.pack (show credential)]))
    pure (Right ((), credential))
