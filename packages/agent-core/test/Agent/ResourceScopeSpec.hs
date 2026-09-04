module Agent.ResourceScopeSpec (spec) where

import Agent.ResourceScope
import Control.Concurrent.Async (race)
import Control.Concurrent.MVar
    ( newEmptyMVar
    , putMVar
    , takeMVar
    )
import Control.Exception.Safe (throwIO, tryAny)
import Data.IORef
import Data.List (sort)
import Test.Hspec

spec :: Spec
spec = describe "Agent.ResourceScope" do
    it "releases resources in reverse acquisition order" do
        released <- newIORef ([] :: [Int])
        scope <- newResourceScope
        _ <- registerResource scope (record released 1)
        _ <- registerResource scope (record released 2)
        closeResourceScope scope
        readIORef released `shouldReturn` [2, 1]

    it "supports explicit idempotent release" do
        releases <- newIORef (0 :: Int)
        scope <- newResourceScope
        key <- registerResource scope $
            atomicModifyIORef' releases \n -> (n + 1, ())
        releaseResource key
        releaseResource key
        closeResourceScope scope
        readIORef releases `shouldReturn` 1

    it "can be closed more than once" do
        scope <- newResourceScope
        closeResourceScope scope
        closeResourceScope scope

    it "closes resources when a lexical scope returns" do
        releases <- newIORef (0 :: Int)
        withResourceScope \scope -> do
            _ <- registerResource scope $
                atomicModifyIORef' releases \n -> (n + 1, ())
            pure ()
        readIORef releases `shouldReturn` 1

    it "acquires independently-owned resources concurrently" do
        releases <- newIORef []
        leftStarted <- newEmptyMVar
        rightStarted <- newEmptyMVar
        withResourceScope \scope -> do
            ((_, left), (_, right)) <-
                allocateResourcesConcurrently
                    scope
                    (putMVar leftStarted () >> takeMVar rightStarted >> pure (1 :: Int))
                    (const (record releases 1))
                    (putMVar rightStarted () >> takeMVar leftStarted >> pure (2 :: Int))
                    (const (record releases 2))
            (left, right) `shouldBe` (1, 2)
        (sort <$> readIORef releases) `shouldReturn` [1, 2]

    it "retains ownership when one concurrent acquisition fails" do
        releases <- newIORef (0 :: Int)
        acquired <- newEmptyMVar
        result <- tryAny $ withResourceScope \scope -> do
            _ <-
                allocateResourcesConcurrently
                    scope
                    (putMVar acquired ())
                    (const (atomicModifyIORef' releases \n -> (n + 1, ())))
                    (takeMVar acquired >> throwIO (userError "boom"))
                    (const (pure ()))
            pure ()
        result `shouldSatisfy` isLeft
        readIORef releases `shouldReturn` 1

    it "retains ownership when concurrent acquisition is interrupted" do
        releases <- newIORef (0 :: Int)
        acquired <- newEmptyMVar
        blocked <- newEmptyMVar
        withResourceScope \scope -> do
            outcome <-
                race
                    (allocateResourcesConcurrently
                        scope
                        (putMVar acquired ())
                        (const (atomicModifyIORef' releases \n -> (n + 1, ())))
                        (takeMVar blocked :: IO ())
                        (const (pure ())))
                    (takeMVar acquired)
            case outcome of
                Left _ ->
                    expectationFailure "blocked acquisition unexpectedly completed"
                Right () -> pure ()
        readIORef releases `shouldReturn` 1

record :: IORef [Int] -> Int -> IO ()
record ref value =
    atomicModifyIORef' ref \values -> (values <> [value], ())

isLeft :: Either a b -> Bool
isLeft = \case
    Left _ -> True
    Right _ -> False
