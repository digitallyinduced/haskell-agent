module Agent.Tools.ResourceArbiterSpec (spec) where

import Agent.Tools.ResourceArbiter
import Agent.Tools.Scheduling
import Agent.Cancel (requestCancel)
import Agent.Loop
import Agent.Loop.Fixtures
    ( testConfig, registryFromTools, noArgsAppTool, appendStateMarker )
import Agent.ToolDispatch (functionToolCall, withToolCallMode, ToolCallMode(..))
import Agent.Tools.Types
    ( ToolEnv(..), defaultToolEnv, withSharedToolResourceClaims, withAsyncToolCalls )
import Control.Concurrent.Async (cancel, wait, withAsync)
import Control.Concurrent.STM
    ( atomically, check, newEmptyTMVarIO, putTMVar, readTMVar )
import Control.Exception.Safe (try)
import Data.Either (isRight)
import Data.IORef (atomicModifyIORef', newIORef)
import System.OsPath (unsafeEncodeUtf)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "shared tool resource arbiter" do
    mapM_ (\mode ->
        it ("coordinates separate loops, including " <> show mode <> " worker lifetime") do
            env <- defaultToolEnv (unsafeEncodeUtf "/workspace")
            release <- newEmptyTMVarIO
            started <- newEmptyTMVarIO
            first <- configWithClaim env mode $
                atomically (putTMVar started ()) >> atomically (readTMVar release)
            second <- configWithClaim env BlockingToolCall (pure ())
            withAsync (runLoopInputs first Nothing [UserMessage "first"]) \worker -> do
                atomically (readTMVar started)
                withAsync (runLoopInputs second Nothing [UserMessage "second"]) \waiting -> do
                    awaitCounts env.toolResourceArbiter (1, 1)
                    atomically (putTMVar release ())
                    timeout 1000000 (wait waiting) >>= (`shouldSatisfy` maybe False isRight)
                timeout 1000000 (wait worker) >>= (`shouldSatisfy` maybe False isRight)
            atomically (toolResourceArbiterCounts env.toolResourceArbiter)
                `shouldReturn` (0, 0))
        [BlockingToolCall, AsyncToolCall]

    it "cancels a loop waiting for another loop's resources without running its handler" do
        env <- defaultToolEnv (unsafeEncodeUtf "/workspace")
        second <- configWithClaim env BlockingToolCall
            (expectationFailure "cancelled handler executed")
        withHeld env.toolResourceArbiter [writeA] do
            withAsync (runLoopInputs second Nothing [UserMessage "cancel"]) \waiting -> do
                awaitCounts env.toolResourceArbiter (1, 1)
                requestCancel second.loopCancel
                timeout 1000000 (wait waiting) >>= (`shouldSatisfy` maybe False (const True))
                atomically (toolResourceArbiterCounts env.toolResourceArbiter)
                    `shouldReturn` (0, 1)

    it "allows shared reads and unrelated worktrees" do
        arbiter <- newToolResourceArbiter 8
        withHeld arbiter [readA] do
            timeout 1000000 (withToolResources arbiter [readA] (pure ()))
                `shouldReturn` Just ()
            timeout 1000000 (withToolResources arbiter [writeB] (pure ()))
                `shouldReturn` Just ()
        atomically (toolResourceArbiterCounts arbiter) `shouldReturn` (0, 0)

    it "blocks a writer until the reader completes" do
        arbiter <- newToolResourceArbiter 8
        started <- newEmptyTMVarIO
        release <- newEmptyTMVarIO
        withAsync
            (withToolResources arbiter [readA] $
                atomically (putTMVar started ()) >> atomically (readTMVar release))
            \reader -> do
                atomically (readTMVar started)
                withAsync (withToolResources arbiter [writeA] (pure ())) \writer -> do
                    awaitCounts arbiter (1, 1)
                    atomically (putTMVar release ())
                    timeout 1000000 (wait writer) `shouldReturn` Just ()
                wait reader

    it "removes cancelled waiters and keeps all-or-nothing acquisition" do
        arbiter <- newToolResourceArbiter 8
        withHeld arbiter [writeA] do
            withAsync (withToolResources arbiter [writeA, writeB] (pure ())) \worker -> do
                awaitCounts arbiter (1, 1)
                -- B has not been acquired while waiting for A.
                -- A later nonconflicting read of C can also bypass this waiter.
                timeout 1000000 (withToolResources arbiter [writeC] (pure ()))
                    `shouldReturn` Just ()
                cancel worker
            atomically (toolResourceArbiterCounts arbiter) `shouldReturn` (0, 1)
            timeout 1000000 (withToolResources arbiter [writeB] (pure ()))
                `shouldReturn` Just ()

    it "releases leases when a running worker is cancelled" do
        arbiter <- newToolResourceArbiter 8
        started <- newEmptyTMVarIO
        blocked <- newEmptyTMVarIO
        withAsync
            (withToolResources arbiter [writeA] $
                atomically (putTMVar started ()) >> atomically (readTMVar blocked))
            \worker -> do
                atomically (readTMVar started)
                cancel worker
        timeout 1000000 (withToolResources arbiter [writeA] (pure ()))
            `shouldReturn` Just ()
        atomically (toolResourceArbiterCounts arbiter) `shouldReturn` (0, 0)

    it "does not let later readers starve an admitted writer" do
        arbiter <- newToolResourceArbiter 8
        releaseWriter <- newEmptyTMVarIO
        withHeld arbiter [readA] do
            withAsync
                (withToolResources arbiter [writeA] $
                    atomically (readTMVar releaseWriter))
                \writer -> do
                    awaitCounts arbiter (1, 1)
                    withAsync (withToolResources arbiter [readA] (pure ())) \reader -> do
                        awaitCounts arbiter (2, 1)
                        cancel writer
                        timeout 1000000 (wait reader) `shouldReturn` Just ()

    it "releases the lease after a synchronous handler exception or error result" do
        arbiter <- newToolResourceArbiter 8
        withToolResources arbiter [writeA] (ioError (userError "handler failed"))
            `shouldThrow` anyIOException
        withToolResources arbiter [writeA] (pure (Left "handler failed" :: Either String ()))
            `shouldReturn` Left "handler failed"
        timeout 1000000 (withToolResources arbiter [writeA] (pure ()))
            `shouldReturn` Just ()
        atomically (toolResourceArbiterCounts arbiter) `shouldReturn` (0, 0)

    it "rejects full admission without retaining another waiter" do
        arbiter <- newToolResourceArbiter 1
        withHeld arbiter [writeA] do
            try (withToolResources arbiter [writeA] (pure ()))
                `shouldReturn` Left ToolResourceArbiterFull
            atomically (toolResourceArbiterCounts arbiter) `shouldReturn` (0, 1)

    it "close wakes waiters but does not revoke active resources" do
        arbiter <- newToolResourceArbiter 8
        withHeld arbiter [writeA] do
            withAsync
                (try (withToolResources arbiter [writeA] (pure ())))
                \worker -> do
                    awaitCounts arbiter (1, 1)
                    closeToolResourceArbiter arbiter
                    timeout 1000000 (wait worker)
                        `shouldReturn` Just (Left ToolResourceArbiterClosed)
            atomically (toolResourceArbiterCounts arbiter) `shouldReturn` (0, 1)
            try (withToolResources arbiter [] (pure ()))
                `shouldReturn` Left ToolResourceArbiterClosed
        timeout 1000000 (waitToolResourceArbiter arbiter) `shouldReturn` Just ()

    it "does not coordinate independent authorities" do
        first <- newToolResourceArbiter 8
        second <- newToolResourceArbiter 8
        withHeld first [writeA] $
            timeout 1000000 (withToolResources second [writeA] (pure ()))
                `shouldReturn` Just ()

readA, writeA, writeB, writeC :: ToolResourceClaim
readA = ToolResourceClaim ToolRead (ToolPath (unsafeEncodeUtf "/workspace/a/file"))
writeA = ToolResourceClaim ToolWrite (ToolPathTree (unsafeEncodeUtf "/workspace/a"))
writeB = ToolResourceClaim ToolWrite (ToolPathTree (unsafeEncodeUtf "/workspace/b"))
writeC = ToolResourceClaim ToolWrite (ToolPathTree (unsafeEncodeUtf "/workspace/c"))

awaitCounts :: ToolResourceArbiter -> (Int, Int) -> IO ()
awaitCounts arbiter expected =
    timeout 1000000
        (atomically $ toolResourceArbiterCounts arbiter >>= check . (== expected))
        `shouldReturn` Just ()

withHeld :: ToolResourceArbiter -> [ToolResourceClaim] -> IO a -> IO a
withHeld arbiter claims action = do
    started <- newEmptyTMVarIO
    release <- newEmptyTMVarIO
    withAsync
        (withToolResources arbiter claims $
            atomically (putTMVar started ()) >> atomically (readTMVar release))
        \_worker -> atomically (readTMVar started) >> action

configWithClaim :: ToolEnv -> ToolCallMode -> IO () -> IO LoopConfig
configWithClaim env mode action = do
    first <- newIORef True
    let call = withToolCallMode mode (functionToolCall "call" "resource" "{}")
        backend = backendWithCallbacks \snapshot _previous _inputs callbacks -> do
            initial <- atomicModifyIORef' first (\value -> (False, value))
            if initial && mode == AsyncToolCall
                then callbacks.onAsyncToolCall call
                else pure ()
            pure $ Right BackendResult
                { backendOutput = emptyTurnOutput
                    (if initial then "tools" else "done")
                    (if initial then [call] else [])
                    (if initial then Nothing else Just "done")
                , backendState = appendStateMarker snapshot
                }
        tool = withAsyncToolCalls $
            withSharedToolResourceClaims env (\_ -> pure (Right [writeA])) $
                noArgsAppTool "resource" (action >> pure (Right "done"))
    config <- testConfig backend
    pure config { loopTools = registryFromTools [tool] }
