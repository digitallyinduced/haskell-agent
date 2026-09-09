module Agent.ResourceScopeSpec (spec) where

import Agent.ResourceScope
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.MVar
    ( newEmptyMVar
    , putMVar
    , readMVar
    , takeMVar
    )
import Control.Exception.Safe (throwIO, tryAny)
import qualified Control.Exception as Exception
    ( MaskingState(MaskedInterruptible)
    , getMaskingState
    )
import Control.Monad (replicateM_, void, when)
import Control.Monad.Trans.Resource (ResourceCleanupException(..))
import Data.Acquire (mkAcquire)
import Data.IORef
import Data.List (sort)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "Agent.ResourceScope" do
    it "releases composed acquisitions in reverse dependency order" do
        released <- newIORef []
        withResourceScope \scope -> do
            (_, values) <- allocateAcquire scope do
                first <- mkAcquire (pure (1 :: Int)) (record released)
                second <- mkAcquire (pure (first + 1)) (record released)
                pure (first, second)
            values `shouldBe` (1, 2)
        readIORef released `shouldReturn` [2, 1]

    it "reports checked closure failures after attempting every finalizer" do
        released <- newIORef []
        scope <- newResourceScope
        _ <- registerResource scope (record released 1)
        _ <- registerResource scope do
            record released 2
            throwIO (userError "second release failed")
        _ <- registerResource scope do
            record released 3
            throwIO (userError "third release failed")
        closeResourceScopeChecked scope `shouldThrow`
            (\exception -> length (rceOtherCleanupExceptions exception) == 1)
        readIORef released `shouldReturn` [3, 2, 1]
        closeResourceScopeChecked scope
        closeResourceScope scope
        readIORef released `shouldReturn` [3, 2, 1]

    it "does not make scope finalizers uninterruptible" do
        mapM_ (\close -> do
            maskingState <- newIORef Nothing
            scope <- newResourceScope
            _ <- registerResource scope $
                Exception.getMaskingState >>= writeIORef maskingState . Just
            close scope
            readIORef maskingState `shouldReturn` Just Exception.MaskedInterruptible)
            [closeResourceScope, closeResourceScopeChecked]

    it "does not repeat early release during checked closure" do
        released <- newIORef []
        scope <- newResourceScope
        (key, _) <- allocateAcquire scope $
            mkAcquire (pure (1 :: Int)) (record released)
        releaseResource key
        closeResourceScopeChecked scope
        readIORef released `shouldReturn` [1]

    it "rejects acquisition after checked closure without running its constructor" do
        scope <- newResourceScope
        acquired <- newIORef False
        closeResourceScopeChecked scope
        outcome <- tryAny $ void $ allocateAcquire scope $
            mkAcquire (writeIORef acquired True) (const (pure ()))
        outcome `shouldSatisfy` isLeft
        readIORef acquired `shouldReturn` False

    it "rolls back partial composition before returning an acquisition failure" do
        released <- newIORef []
        withResourceScope \scope -> do
            result <- tryAny $ void $ allocateAcquire scope do
                _ <- mkAcquire (pure (1 :: Int)) (record released)
                mkAcquire
                    (throwIO (userError "dependent acquisition failed") :: IO ())
                    (const (record released 2))
            result `shouldSatisfy` isLeft
            readIORef released `shouldReturn` [1]
        readIORef released `shouldReturn` [1]

    it "releases a composed acquisition once after early release" do
        released <- newIORef []
        withResourceScope \scope -> do
            (key, _) <- allocateAcquire scope do
                _ <- mkAcquire (pure (1 :: Int)) (record released)
                mkAcquire (pure (2 :: Int)) (record released)
            releaseResource key
            releaseResource key
            readIORef released `shouldReturn` [2, 1]
        readIORef released `shouldReturn` [2, 1]

    it "attempts every composed finalizer before reporting an early release exception" do
        released <- newIORef []
        result <- tryAny $ withResourceScope \scope -> do
            (key, _) <- allocateAcquire scope do
                _ <- mkAcquire (pure (1 :: Int)) (record released)
                _ <- mkAcquire (pure (2 :: Int)) \value -> do
                    record released value
                    throwIO (userError "dependent release failed")
                mkAcquire (pure (3 :: Int)) (record released)
            releaseResource key
        result `shouldSatisfy` isLeft
        readIORef released `shouldReturn` [3, 2, 1]

    it "attempts every composed finalizer during best-effort scope closure" do
        released <- newIORef []
        withResourceScope \scope -> do
            _ <- allocateAcquire scope do
                _ <- mkAcquire (pure (1 :: Int)) (record released)
                mkAcquire (pure (2 :: Int)) \value -> do
                    record released value
                    throwIO (userError "dependent release failed")
            pure ()
        readIORef released `shouldReturn` [2, 1]

    it "rolls back completed composition when dependent acquisition is cancelled" do
        released <- newIORef []
        dependentStarted <- newEmptyMVar
        blocked <- newEmptyMVar
        withResourceScope \scope -> do
            outcome <- race
                (allocateAcquire scope do
                    _ <- mkAcquire (pure (1 :: Int)) (record released)
                    mkAcquire
                        (putMVar dependentStarted () >> takeMVar blocked :: IO ())
                        (const (record released 2)))
                (takeMVar dependentStarted)
            case outcome of
                Left _ -> expectationFailure "blocked acquisition unexpectedly completed"
                Right () -> pure ()
            readIORef released `shouldReturn` [1]
        readIORef released `shouldReturn` [1]

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

    it "acquires four heterogeneous resources at the same time" do
        started <- newIORef (0 :: Int)
        allStarted <- newEmptyMVar
        let acquire value = do
                count <- atomicModifyIORef' started \n ->
                    let next = n + 1
                    in (next, next)
                when (count == 4) (putMVar allStarted ())
                readMVar allStarted
                pure value
        outcome <- timeout 1000000 $ withResourceScope \scope -> do
            ((_, first), (_, second), (_, third), (_, fourth)) <-
                allocateFourResourcesConcurrently
                    scope
                    (acquire (1 :: Int))
                    (const (pure ()))
                    (acquire ("two" :: String))
                    (const (pure ()))
                    (acquire True)
                    (const (pure ()))
                    (acquire '4')
                    (const (pure ()))
            pure (first, second, third, fourth)
        outcome `shouldBe` Just (1, "two", True, '4')

    it "keeps four-resource cleanup reverse ordered and idempotent" do
        released <- newIORef ([] :: [Int])
        firstAcquired <- newEmptyMVar
        secondAcquired <- newEmptyMVar
        thirdAcquired <- newEmptyMVar
        withResourceScope \scope -> do
            (_, _, _, (fourthKey, _)) <-
                allocateFourResourcesConcurrently
                    scope
                    (putMVar firstAcquired () >> pure (1 :: Int))
                    (const (record released 1))
                    ( takeMVar firstAcquired
                        >> threadDelay 20000
                        >> putMVar secondAcquired ()
                        >> pure ("two" :: String)
                    )
                    (const (record released 2))
                    (takeMVar secondAcquired >> threadDelay 20000 >> putMVar thirdAcquired () >> pure True)
                    (const (record released 3))
                    (takeMVar thirdAcquired >> threadDelay 20000 >> pure '4')
                    (const (record released 4))
            releaseResource fourthKey
            releaseResource fourthKey
        readIORef released `shouldReturn` [4, 3, 2, 1]

    it "cleans every completed four-way acquisition when one fails" do
        releases <- newIORef ([] :: [Int])
        completed <- newEmptyMVar
        result <- tryAny $ withResourceScope \scope -> do
            _ <-
                allocateFourResourcesConcurrently
                    scope
                    (putMVar completed () >> pure (1 :: Int))
                    (const (record releases 1))
                    (putMVar completed () >> pure ("two" :: String))
                    (const (record releases 2))
                    (putMVar completed () >> pure True)
                    (const (record releases 3))
                    (replicateM_ 3 (takeMVar completed) >> threadDelay 20000 >> throwIO (userError "boom"))
                    (const (record releases 4))
            pure ()
        result `shouldSatisfy` isLeft
        (sort <$> readIORef releases) `shouldReturn` [1, 2, 3]

    it "cancels four-way acquisition and cleans completed resources without hanging" do
        releases <- newIORef ([] :: [Int])
        firstCompleted <- newEmptyMVar
        secondCompleted <- newEmptyMVar
        thirdCompleted <- newEmptyMVar
        blocked <- newEmptyMVar
        let waitForCompleted =
                mapM_ readMVar
                    [firstCompleted, secondCompleted, thirdCompleted]
        outcome <-
            race
                (withResourceScope \scope ->
                    allocateFourResourcesConcurrently
                        scope
                        (putMVar firstCompleted () >> pure (1 :: Int))
                        (const (record releases 1))
                        (putMVar secondCompleted () >> pure ("two" :: String))
                        (const (record releases 2))
                        (putMVar thirdCompleted () >> pure True)
                        (const (record releases 3))
                        (waitForCompleted >> (takeMVar blocked :: IO ()))
                        (const (record releases 4)))
                (waitForCompleted >> threadDelay 20000)
        case outcome of
            Left _ ->
                expectationFailure "blocked acquisition unexpectedly completed"
            Right () -> pure ()
        (sort <$> readIORef releases) `shouldReturn` [1, 2, 3]

record :: IORef [Int] -> Int -> IO ()
record ref value =
    atomicModifyIORef' ref \values -> (values <> [value], ())

isLeft :: Either a b -> Bool
isLeft = \case
    Left _ -> True
    Right _ -> False
