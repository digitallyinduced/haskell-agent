module Agent.ConcurrentSpec (spec) where

import Agent.Concurrent
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync, waitCatch)
import Control.Concurrent.MVar
import Control.Exception.Safe (finally)
import Data.Either (isLeft)
import Data.IORef
import Test.Hspec

spec :: Spec
spec = describe "bounded concurrency" do
    it "preserves input order while overlapping work" do
        results <-
            mapConcurrentlyBounded 3
                (\value -> do
                    threadDelay ((4 - value) * 10000)
                    pure (value * 2))
                [1, 2, 3]
        results `shouldBe` [2, 4, 6]

    it "never exceeds the requested active worker count" do
        active <- newIORef (0 :: Int)
        peak <- newIORef (0 :: Int)
        let run value =
                bracketActive active peak do
                    threadDelay 20000
                    pure value
        mapConcurrentlyBounded 3 run [1 .. 12 :: Int]
            `shouldReturn` [1 .. 12]
        readIORef peak `shouldReturn` 3

    it "clamps zero and negative limits to one worker" do
        mapM_ assertSingleWorker [0, -3]

    it "cancels and joins sibling workers when one fails" do
        siblingStarted <- newEmptyMVar
        siblingStopped <- newEmptyMVar
        releaseSibling <- newEmptyMVar @()
        let action :: Int -> IO ()
            action = \case
                (0 :: Int) ->
                    (putMVar siblingStarted () >> takeMVar releaseSibling)
                        `finally` putMVar siblingStopped ()
                _ -> do
                    takeMVar siblingStarted
                    fail "worker failed"
        withAsync (mapConcurrentlyBounded 2 action [0, 1]) \running -> do
            waitCatch running `shouldReturnSatisfy` isLeft
            tryTakeMVar siblingStopped `shouldReturn` Just ()

    it "does not invoke the action for empty input" do
        invoked <- newIORef False
        mapConcurrentlyBounded 4
            (\() -> writeIORef invoked True)
            []
            `shouldReturn` []
        readIORef invoked `shouldReturn` False

bracketActive :: IORef Int -> IORef Int -> IO a -> IO a
bracketActive active peak action = do
    current <- atomicModifyIORef' active \count ->
        let next = count + 1
        in (next, next)
    atomicModifyIORef' peak \old -> (max old current, ())
    action `finally`
        atomicModifyIORef' active \count -> (count - 1, ())

shouldReturnSatisfy :: Show a => IO a -> (a -> Bool) -> Expectation
shouldReturnSatisfy action predicate =
    action >>= (`shouldSatisfy` predicate)

assertSingleWorker :: Int -> Expectation
assertSingleWorker limit = do
    active <- newIORef (0 :: Int)
    peak <- newIORef (0 :: Int)
    mapConcurrentlyBounded limit
        (\value -> bracketActive active peak (pure value))
        [1 .. 4 :: Int]
        `shouldReturn` [1 .. 4]
    readIORef peak `shouldReturn` 1
