{-# LANGUAGE NumericUnderscores #-}

module Agent.Store.PoolCacheSpec (spec) where

import Agent.Store.PoolCache
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
    ( cancel
    , wait
    , waitCatch
    , withAsync
    )
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import qualified Control.Exception as Exception
import Data.IORef
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "PoolCache" do
    it "single-flights callers for the same key" do
        calls <- newIORef (0 :: Int)
        started <- newEmptyMVar
        release <- newEmptyMVar
        cache <- newPoolCache
            4
            ("closed" :: Text)
            exceptionText
            (\(_ :: Text) -> do
                atomicModifyIORef' calls (\count -> (count + 1, ()))
                putMVar started ()
                takeMVar release
                pure (Right (1 :: Int)))
            (const (pure ()))
        withAsync (acquirePoolCache cache "role") \first -> do
            takeMVar started
            withAsync (acquirePoolCache cache "role") \second -> do
                threadDelay 20_000
                readIORef calls `shouldReturn` 1
                putMVar release ()
                wait first `shouldReturn` Right 1
                wait second `shouldReturn` Right 1

    it "opens different keys concurrently" do
        started <- newChan
        release <- newEmptyMVar
        cache <- newPoolCache
            4
            ("closed" :: Text)
            exceptionText
            (\key -> do
                writeChan started key
                readMVar release
                pure (Right key))
            (const (pure ()))
        withAsync (acquirePoolCache cache ("one" :: Text)) \first ->
            withAsync (acquirePoolCache cache "two") \second -> do
                observed <- timeout 250_000 do
                    left <- readChan started
                    right <- readChan started
                    pure [left, right]
                fmap (all (`elem` ["one", "two"])) observed
                    `shouldBe` Just True
                putMVar release ()
                wait first `shouldReturn` Right "one"
                wait second `shouldReturn` Right "two"

    it "removes a failed entry so a later caller can retry" do
        calls <- newIORef (0 :: Int)
        cache <- newPoolCache
            2
            ("closed" :: Text)
            exceptionText
            (\(_ :: Text) -> do
                attempt <- atomicModifyIORef' calls
                    (\count -> (count + 1, count + 1))
                pure if attempt == 1
                    then Left "failed"
                    else Right (7 :: Int))
            (const (pure ()))
        acquirePoolCache cache "role" `shouldReturn` Left "failed"
        acquirePoolCache cache "role" `shouldReturn` Right 7
        readIORef calls `shouldReturn` 2

    it "wakes same-key waiters when the opening leader is cancelled" do
        started <- newEmptyMVar
        blocked <- newEmptyMVar
        cache <- newPoolCache
            2
            ("closed" :: Text)
            (const "interrupted")
            (\(_ :: Text) -> do
                putMVar started ()
                takeMVar blocked
                pure (Right (1 :: Int)))
            (const (pure ()))
        withAsync (acquirePoolCache cache "role") \leader -> do
            takeMVar started
            withAsync (acquirePoolCache cache "role") \waiter -> do
                threadDelay 20_000
                cancel leader
                _ <- waitCatch leader
                timeout 250_000 (wait waiter)
                    `shouldReturn` Just (Left "interrupted")

    it "closes a resource opened concurrently with shutdown exactly once" do
        started <- newEmptyMVar
        release <- newEmptyMVar
        closes <- newIORef (0 :: Int)
        cache <- newPoolCache
            2
            ("closed" :: Text)
            exceptionText
            (\(_ :: Text) -> do
                putMVar started ()
                takeMVar release
                pure (Right (1 :: Int)))
            (\_ -> atomicModifyIORef' closes (\count -> (count + 1, ())))
        withAsync (acquirePoolCache cache "role") \opening -> do
            takeMVar started
            withAsync (closePoolCache cache) \closing -> do
                threadDelay 20_000
                acquirePoolCache cache "other"
                    `shouldReturn` Left "closed"
                putMVar release ()
                wait opening `shouldReturn` Left "closed"
                wait closing
                readIORef closes `shouldReturn` 1
                closePoolCache cache
                readIORef closes `shouldReturn` 1

    it "closes independent ready resources concurrently and only once" do
        started <- newChan
        release <- newEmptyMVar
        closes <- newIORef ([] :: [Text])
        cache <- newPoolCache
            2
            ("closed" :: Text)
            exceptionText
            (pure . Right)
            (\resource -> do
                writeChan started resource
                readMVar release
                atomicModifyIORef' closes
                    (\values -> (resource : values, ())))
        acquirePoolCache cache "one" `shouldReturn` Right ("one" :: Text)
        acquirePoolCache cache "two" `shouldReturn` Right ("two" :: Text)
        withAsync (closePoolCache cache) \closing -> do
            observed <- timeout 250_000 do
                left <- readChan started
                right <- readChan started
                pure [left, right]
            fmap (all (`elem` ["one", "two"])) observed
                `shouldBe` Just True
            putMVar release ()
            wait closing
        sort <$> readIORef closes `shouldReturn` ["one", "two"]
        closePoolCache cache
        sort <$> readIORef closes `shouldReturn` ["one", "two"]

exceptionText :: Exception.SomeException -> Text
exceptionText = Text.pack . Exception.displayException
