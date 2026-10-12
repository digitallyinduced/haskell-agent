{-# LANGUAGE OverloadedStrings, OverloadedRecordDot, BlockArguments, RankNTypes #-}
module Agent.Subagents.RuntimeSpec (spec) where

import Test.Hspec
import Agent.Subagents.Runtime
import Agent.Subagents.Runtime.Internal
import Agent.Subagents (subagentConfig, SubagentConfig (..), SubagentId (..), SubagentStatus (..),
    SuspensionPersistence (..), setSubagentSuspensionPersistence, setSubagentRunner,
    restoreSubagentAtWithCwdStatus, setSubagentOnSettled)
import Agent.Loop (LoopError (LoopNoResponseId))
import Agent.Subagents.TaskPath (parseTaskPath)
import Control.Exception.Safe (bracket, finally)
import Control.Concurrent.Async (withAsync, wait)
import Control.Concurrent.MVar
import Control.Monad (void)
import Data.IORef
import Data.Maybe (isNothing)
import Data.Time (addUTCTime, getCurrentTime)
import Data.Text (Text)
import System.Timeout (timeout)
import System.OsPath (unsafeEncodeUtf)

config :: SubagentConfig
config = SubagentConfig { maxConcurrent = 3, maxDepth = Just 4, maxSpawnedPerTurn = Just 12 }

newRuntime :: IO SubagentRuntime
newRuntime = newSubagentRuntime config (unsafeEncodeUtf ".")

acquire :: SubagentRuntime -> Text -> RuntimeHooks -> IO (Maybe RuntimeHandle)
acquire runtime key hooks = withSubagentRuntimeHandle runtime key hooks pure

hooks :: RuntimeHooks
hooks = RuntimeHooks
    { withOwnership = fmap Just
    , withOperation = id
    , initialize = const (pure ())
    , pumpRunnable = const (pure True)
    }

spec :: Spec
spec = describe "scoped subagent runtime" do
    it "keeps the original operation guard when later root turns borrow the tree" $
        bracket newRuntime closeSubagentRuntime \runtime -> do
            calls <- newIORef ([] :: [Text])
            let original = hooks { withOperation = \action -> modifyIORef' calls (<> ["original"]) >> action } :: RuntimeHooks
                replacement = hooks { withOperation = \action -> modifyIORef' calls (<> ["replacement"]) >> action } :: RuntimeHooks
                runGuard RuntimeHandle { withOperation = operation } = operation (pure ())
            withSubagentRuntimeHandle runtime "guarded" original runGuard `shouldReturn` Just ()
            withSubagentRuntimeHandle runtime "guarded" replacement runGuard `shouldReturn` Just ()
            readIORef calls `shouldReturn` ["original", "original"]

    it "retires a sleeping tree without persisting cancellation" $
        bracket newRuntime closeSubagentRuntime \runtime -> do
            now <- getCurrentTime
            cancellations <- newIORef (0 :: Int)
            releases <- newIORef (0 :: Int)
            path <- either (fail . show) pure (parseTaskPath "/root/sleeper")
            let callbacks = hooks
                    { withOwnership = \action -> (Just <$> action) `finally` modifyIORef' releases (+ 1)
                    , initialize = \handle -> do
                        restored <- restoreSubagentAtWithCwdStatus handle.registry
                            (unsafeEncodeUtf ".") (SubagentId "sleeper") Nothing path 1 Nothing Nothing
                            (Suspended (addUTCTime 3600 now) "waiting")
                        either (fail . show) (const (pure ())) restored
                        setSubagentOnSettled handle.registry (\_ _ -> modifyIORef' cancellations (+ 1))
                    }
            _ <- acquire runtime "sleeping" callbacks
            tickSubagentRuntimeAt runtime (addUTCTime 61 now) (\_ e -> expectationFailure (show e))
            readIORef releases `shouldReturn` 1
            readIORef cancellations `shouldReturn` 0

    it "retires an idle tree and releases ownership before reacquiring" $
        bracket newRuntime closeSubagentRuntime \runtime -> do
            starts <- newIORef (0 :: Int)
            releases <- newIORef (0 :: Int)
            let callbacks = hooks
                    { withOwnership = \action -> do
                        modifyIORef' starts (+ 1)
                        (Just <$> action) `finally` modifyIORef' releases (+ 1)
                    }
            _ <- acquire runtime "idle" callbacks
            now <- getCurrentTime
            tickSubagentRuntimeAt runtime (addUTCTime 61 now) (\_ e -> expectationFailure (show e))
            readIORef releases `shouldReturn` 1
            _ <- acquire runtime "idle" callbacks
            readIORef starts `shouldReturn` 2

    it "does not retire a borrowed root turn even after the idle deadline" $
        bracket newRuntime closeSubagentRuntime \runtime -> do
            releases <- newIORef (0 :: Int)
            let callbacks = hooks
                    { withOwnership = \action -> (Just <$> action) `finally` modifyIORef' releases (+ 1) }
            void $ withSubagentRuntimeHandle runtime "borrowed" callbacks \_ -> do
                now <- getCurrentTime
                tickSubagentRuntimeAt runtime (addUTCTime 61 now) (\_ e -> expectationFailure (show e))
                readIORef releases `shouldReturn` 0

    it "retires cancelled trees and creates a fresh tree after reactivation" $
        bracket newRuntime closeSubagentRuntime \runtime -> do
            starts <- newIORef (0 :: Int)
            releases <- newIORef (0 :: Int)
            let callbacks = hooks
                    { withOwnership = \action -> (Just <$> action) `finally` modifyIORef' releases (+ 1)
                    , initialize = \_ -> modifyIORef' starts (+ 1)
                    , pumpRunnable = const (pure False)
                    }
            _ <- acquire runtime "stopped" callbacks
            tickSubagentRuntime runtime \_ failure -> expectationFailure (show failure)
            readIORef releases `shouldReturn` 1
            _ <- acquire runtime "stopped" callbacks
            readIORef starts `shouldReturn` 2

    it "isolates heartbeat failures to one tree and reports its key" $
        bracket newRuntime closeSubagentRuntime \runtime -> do
            pumped <- newIORef False
            failures <- newIORef []
            _ <- acquire runtime "a-failing" hooks { pumpRunnable = const (fail "pump failed") }
            _ <- acquire runtime "b-healthy" hooks
                { pumpRunnable = const (writeIORef pumped True >> pure True) }
            tickSubagentRuntime runtime \key _ -> modifyIORef' failures (key :)
            readIORef failures `shouldReturn` ["a-failing"]
            readIORef pumped `shouldReturn` True

    it "keeps one tree across main turns and releases ownership on scoped shutdown" do
        starts <- newIORef (0 :: Int)
        releases <- newIORef (0 :: Int)
        let callbacks = hooks
                { withOwnership = \action -> do
                    modifyIORef' starts (+ 1)
                    (Just <$> action) `finally` modifyIORef' releases (+ 1)
                }
        withSubagentRuntime config (unsafeEncodeUtf ".") (\_ e -> expectationFailure (show e)) \runtime -> do
            first <- acquire runtime "assistant-a" callbacks
            second <- acquire runtime "assistant-a" callbacks
            isNothing first `shouldBe` False
            isNothing second `shouldBe` False
            readIORef starts `shouldReturn` 1
            readIORef releases `shouldReturn` 0
        readIORef releases `shouldReturn` 1

    it "uses the configured shared cap for the entire tree" $
        bracket newRuntime closeSubagentRuntime \runtime -> do
            result <- acquire runtime "assistant-b" hooks
            case result of
                Nothing -> expectationFailure "ownership unexpectedly refused"
                Just handle -> do
                    limits <- subagentConfig handle.registry
                    limits.maxConcurrent `shouldBe` 3
                    limits.maxDepth `shouldBe` Just 4

    it "does not start a registry while another generation holds ownership" $
        bracket newRuntime closeSubagentRuntime \runtime -> do
            initialized <- newIORef False
            result <- acquire runtime "assistant-c" hooks
                { withOwnership = const (pure Nothing)
                , initialize = \_ -> writeIORef initialized True
                }
            isNothing result `shouldBe` True
            readIORef initialized `shouldReturn` False

    it "unblocks an acquiring turn when shutdown cancels initialization" do
        entered <- newEmptyMVar
        blocked <- newEmptyMVar
        bracket newRuntime closeSubagentRuntime \runtime -> do
            let callbacks = hooks { initialize = \_ -> putMVar entered () >> readMVar blocked }
            withAsync (acquire runtime "assistant-d" callbacks) \acquiring -> do
                timeout 1000000 (readMVar entered) `shouldReturn` Just ()
                closeSubagentRuntime runtime
                result <- timeout 1000000 (wait acquiring)
                fmap isNothing result `shouldBe` Just True

    it "pumps trees automatically without an application heartbeat" do
        pumped <- newEmptyMVar
        withSubagentRuntimeInterval 1000 config (unsafeEncodeUtf ".")
            (\_ e -> expectationFailure (show e)) \runtime -> do
                _ <- acquire runtime "automatic" hooks
                    { pumpRunnable = \_ -> void (tryPutMVar pumped ()) >> pure True }
                timeout 1000000 (readMVar pumped) `shouldReturn` Just ()

    it "joins the scheduler before releasing tree ownership on scope failure" do
        entered <- newEmptyMVar
        blocked <- newEmptyMVar
        stopped <- newIORef False
        releasedAfterScheduler <- newIORef False
        let callbacks = hooks
                { withOwnership = \action -> (Just <$> action) `finally`
                    (readIORef stopped >>= writeIORef releasedAfterScheduler)
                , pumpRunnable = \_ ->
                    (putMVar entered () >> readMVar blocked >> pure True)
                        `finally` writeIORef stopped True
                }
        (withSubagentRuntimeInterval 1000 config (unsafeEncodeUtf ".")
            (\_ e -> expectationFailure (show e)) \runtime -> do
                _ <- acquire runtime "failing-scope" callbacks
                timeout 1000000 (readMVar entered) `shouldReturn` Just ()
                fail "root host failed") `shouldThrow` anyIOException
        readIORef releasedAfterScheduler `shouldReturn` True

    it "automatically resumes due sleepers and joins their workers before releasing ownership" do
        started <- newEmptyMVar
        blocked <- newEmptyMVar
        joined <- newIORef False
        releasedAfterChild <- newIORef False
        now <- getCurrentTime
        path <- either (fail . show) pure (parseTaskPath "/root/sleeper")
        let callbacks = hooks
                { withOwnership = \action -> (Just <$> action) `finally`
                    (readIORef joined >>= writeIORef releasedAfterChild)
                , initialize = \handle -> do
                    setSubagentSuspensionPersistence handle.registry SuspensionPersistence
                        { persistSuspension = \_ _ _ -> pure (Right ())
                        , persistSuspensionResume = \_ _ -> pure (Right ())
                        , persistSuspensionCancellation = \_ -> pure (Right ())
                        }
                    setSubagentRunner handle.registry \_ _ _ _ ->
                        (putMVar started () >> readMVar blocked >> pure (Left LoopNoResponseId))
                            `finally` writeIORef joined True
                    restored <- restoreSubagentAtWithCwdStatus handle.registry
                        (unsafeEncodeUtf ".") (SubagentId "sleeper") Nothing path 1 Nothing Nothing
                        (Suspended now "waiting")
                    either (fail . show) (const (pure ())) restored
                }
        withSubagentRuntimeInterval 1000 config (unsafeEncodeUtf ".")
            (\_ e -> expectationFailure (show e)) \runtime -> do
                _ <- acquire runtime "suspended" callbacks
                timeout 1000000 (readMVar started) `shouldReturn` Just ()
        readIORef releasedAfterChild `shouldReturn` True
