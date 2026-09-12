module Agent.Tools.LoopIntegrationSpec (spec) where

import Agent.Cancel (newCancelFlag, requestCancel)
import Agent.Error (ApiError(..))
import Agent.Loop
import Agent.ToolDispatch
import Agent.Tools.IO
    ( RunningCommand(..)
    , startShellCommandWithCompletion
    , stopShellCommand
    )
import Agent.Tools.Types
import Control.Concurrent.Async (poll, wait, withAsync)
import Control.Concurrent.MVar
import qualified Control.Exception as Exception
import qualified Control.Exception.Safe as Safe
import Control.Monad (void)
import Data.IORef
import qualified Data.Text as Text
import Data.Time.Clock (getCurrentTime)
import System.OsPath (unsafeEncodeUtf)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "loop concrete command integration" do
    it "joins asynchronous command cleanup after provider credentials are exhausted" do
        let workdir = unsafeEncodeUtf "."
        env <- defaultToolEnv workdir
        callbackStarted <- newEmptyMVar
        callbackFinished <- newEmptyMVar
        releaseCallback <- newEmptyMVar
        commandStarted <- newEmptyMVar
        releaseHandler <- newEmptyMVar
        cleanupMask <- newEmptyMVar
        handlerFinished <- newEmptyMVar
        retryAt <- getCurrentTime
        let providerFailure = CredentialsExhausted retryAt []
            call = withToolCallMode AsyncToolCall
                (functionToolCall "async-command" "command" "{}")
            acquire =
                startShellCommandWithCompletion env workdir "true"
                    (\_ ->
                        (putMVar callbackStarted () >> readMVar releaseCallback)
                            `Safe.finally` putMVar callbackFinished ())
                    >>= either (Safe.throwString . Text.unpack) pure
            release command = do
                Exception.getMaskingState >>= putMVar cleanupMask
                stopShellCommand command
                putMVar handlerFinished ()
            tool = withAsyncToolCalls $
                jsonAppToolWithExecution "command" "" [] AlwaysReadOnly ParallelSafe $
                    noArgsTool "command" $
                        Safe.bracket acquire release \command -> do
                            putMVar commandStarted command
                            readMVar releaseHandler
                            pure (Right "unexpected completion")
            backend = backendWithCallbacks \_ _ _ callbacks -> do
                callbacks.onAsyncToolCall call
                void (readMVar commandStarted)
                readMVar callbackStarted
                pure (Left providerFailure)
            unblock = do
                void (tryPutMVar releaseCallback ())
                void (tryPutMVar releaseHandler ())
        cancel <- newCancelFlag
        state <- newIORef emptyBackendSnapshot
        let config = LoopConfig
                { loopBackend = backend
                , loopBackendState = BackendStateStore
                    { readBackendState = readIORef state
                    , commitBackendState = \snapshot -> do
                        writeIORef state snapshot
                        pure snapshot
                    }
                , loopTools = either (error . Text.unpack) id (mkToolRegistry [tool])
                , loopReadTools = Nothing
                , loopDispatch = defaultLoopDispatch
                , loopMaxTurns = defaultLoopMaxTurns
                , loopOnEvent = \_ -> pure ()
                , loopApprove = \_ -> pure (Right True)
                , loopReadSteering = pure []
                , loopCommitSteering = \_ -> pure ()
                , loopInterrupt = pure ()
                , loopCancel = cancel
                }
        withAsync (runLoop config Nothing "go") \running ->
            (do
                timeout 3000000 (readMVar cleanupMask)
                    `shouldReturn` Just Exception.MaskedUninterruptible
                -- Escape can arrive while provider failure is already
                -- unwinding the asynchronous tool-manager scope.
                requestCancel config.loopCancel
                timeout 3000000 (wait running)
                    `shouldReturn` Just (Left (LoopTransport providerFailure))
                tryReadMVar handlerFinished `shouldReturn` Just ()
                tryReadMVar callbackFinished `shouldReturn` Just ()
                command <- readMVar commandStarted
                poll command.runningSupervisor
                    >>= (`shouldSatisfy` maybe False (const True)))
            -- Release gates before withAsync joins a regressed worker, so the
            -- expected timeout failure cannot strand the rest of the suite.
            `Safe.finally` unblock
