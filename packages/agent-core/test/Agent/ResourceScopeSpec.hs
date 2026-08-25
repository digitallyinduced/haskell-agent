module Agent.ResourceScopeSpec (spec) where

import Agent.ResourceScope
import Data.Acquire (mkAcquire)
import Data.IORef
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

    it "owns Acquire values until explicit release" do
        releases <- newIORef (0 :: Int)
        scope <- newResourceScope
        (key, value) <-
            allocateAcquireResource scope $
                mkAcquire
                    (pure ("value" :: String))
                    (\_ -> atomicModifyIORef' releases \n -> (n + 1, ()))
        value `shouldBe` "value"
        readIORef releases `shouldReturn` 0
        releaseResource key
        closeResourceScope scope
        readIORef releases `shouldReturn` 1

    it "can be closed more than once" do
        scope <- newResourceScope
        closeResourceScope scope
        closeResourceScope scope

record :: IORef [Int] -> Int -> IO ()
record ref value =
    atomicModifyIORef' ref \values -> (values <> [value], ())
