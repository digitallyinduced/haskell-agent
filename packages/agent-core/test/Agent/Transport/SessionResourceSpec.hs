{-# LANGUAGE NoImplicitPrelude, BlockArguments, RankNTypes, ScopedTypeVariables, OverloadedStrings #-}
module Agent.Transport.SessionResourceSpec (spec) where

import Prelude
import Agent.Transport.SessionResource
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
        let blockedAcquire :: forall result. Int -> (() -> Int -> IO (Either Text result)) -> IO (Either Text result)
            blockedAcquire credential action =
                (takeMVar blocker >> acquire events credential action)
                    `Exception.finally` modifyIORef' events (<> ["acquisition-stopped"])
        withSessionResource blockedAcquire \session -> do
            timeout 100000 (runSessionResource session 1 \_ _ _ ->
                pure (Right ())) `shouldReturn` Nothing
            readIORef events `shouldReturn` ["acquisition-stopped"]

    it "returns acquisition failures without executing the callback" do
        events <- newIORef []
        let unavailable (_ :: Int) _ = pure (Left ("unavailable" :: Text))
        withSessionResource unavailable \session -> do
            runSessionResource session 1 (\(_ :: ()) _ _ ->
                modifyIORef' events (<> ["unexpected callback"]) >> pure (Right ()))
                `shouldReturn` Left "unavailable"
        readIORef events `shouldReturn` []

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
        let refreshedAcquire :: forall result. Int -> (() -> Int -> IO (Either Text result)) -> IO (Either Text result)
            refreshedAcquire requested action =
                acquire events requested (\resource _ -> action resource (requested + 1))
        withSessionResource refreshedAcquire \session -> do
            timeout 1000000 (runSessionResource session 1 \_ actual fresh ->
                pure (Right (actual, fresh))) `shouldReturn` Just (Right (2, True))
            currentSessionCredential session `shouldReturn` Just 2
            runSessionResource session 2 (\_ actual fresh -> pure (Right (actual, fresh)))
                `shouldReturn` Right (2, False)
            readIORef events `shouldReturn` ["open:1"]

    it "cancels and joins the callback before a timeout returns" do
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
    -> Int
    -> (() -> Int -> IO (Either Text result))
    -> IO (Either Text result)
acquire events credential action = Exception.bracket
    (modifyIORef' events (<> ["open:" <> Text.pack (show credential)]))
    (\_ -> modifyIORef' events (<> ["close:" <> Text.pack (show credential)]))
    (\_ -> action () credential)
