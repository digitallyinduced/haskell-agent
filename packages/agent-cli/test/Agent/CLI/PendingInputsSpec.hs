module Agent.CLI.PendingInputsSpec (spec) where

import Agent.CLI.PendingInputs
    ( clearPendingInputs
    , enqueuePendingInput
    , newPendingInputs
    , withPendingInputs
    )
import Agent.Error (ApiError(..))
import Agent.Loop
    ( Backend(..)
    , BackendResult(..)
    , TurnInput(..)
    , emptyBackendSnapshot
    , emptyTurnOutput
    )
import Control.Concurrent
    ( forkIO
    , newEmptyMVar
    , putMVar
    , takeMVar
    )
import Control.Exception.Safe (tryAny)
import Data.Either (isLeft)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Test.Hspec

spec :: Spec
spec = describe "withPendingInputs" do
    it "commits queued inputs after a successful submission" do
        pending <- newPendingInputs
        enqueuePendingInput pending (UserMessage "child result")
        seen <- newIORef []
        let backend = withPendingInputs pending $ Backend
                \state _ inputs _ -> do
                    writeIORef seen inputs
                    pure $ Right BackendResult
                        { backendOutput =
                            emptyTurnOutput "response" [] Nothing
                        , backendState = state
                        }
        _ <- backend.submitTurn emptyBackendSnapshot Nothing
            [UserMessage "parent"] (const (pure ()))
        readIORef seen `shouldReturn`
            [UserMessage "child result", UserMessage "parent"]

    it "requeues inputs when the backend returns an error" do
        let queued = [UserMessage "child result"]
        pending <- newPendingInputs
        mapM_ (enqueuePendingInput pending) queued
        let backend = withPendingInputs pending $ Backend
                \_ _ _ _ -> pure (Left (ConnectionError "offline"))
        _ <- backend.submitTurn emptyBackendSnapshot Nothing
            [UserMessage "parent"] (const (pure ()))
        seen <- newIORef []
        let retry = withPendingInputs pending $ Backend
                \state _ inputs _ -> do
                    writeIORef seen inputs
                    pure $ Right BackendResult
                        { backendOutput = emptyTurnOutput "response" [] Nothing
                        , backendState = state
                        }
        _ <- retry.submitTurn emptyBackendSnapshot Nothing [] (const (pure ()))
        readIORef seen `shouldReturn` queued

    it "requeues inputs when submission is interrupted by an exception" do
        let queued = [UserMessage "child result"]
        pending <- newPendingInputs
        mapM_ (enqueuePendingInput pending) queued
        let backend = withPendingInputs pending $ Backend
                \_ _ _ _ -> ioError (userError "interrupted")
        result <- tryAny $
            backend.submitTurn emptyBackendSnapshot Nothing
                [UserMessage "parent"] (const (pure ()))
        result `shouldSatisfy` isLeft
        seen <- newIORef []
        let retry = withPendingInputs pending $ Backend
                \state _ inputs _ -> do
                    writeIORef seen inputs
                    pure $ Right BackendResult
                        { backendOutput = emptyTurnOutput "response" [] Nothing
                        , backendState = state
                        }
        _ <- retry.submitTurn emptyBackendSnapshot Nothing [] (const (pure ()))
        readIORef seen `shouldReturn` queued

    it "prepends drained inputs ahead of concurrent enqueues when requeuing" do
        pending <- newPendingInputs
        enqueuePendingInput pending (UserMessage "old")
        entered <- newEmptyMVar
        release <- newEmptyMVar
        let backend = withPendingInputs pending $ Backend
                \_ _ _ _ -> do
                    putMVar entered ()
                    takeMVar release
                    pure (Left (ConnectionError "offline"))
        done <- newEmptyMVar
        _ <- forkIO $ backend.submitTurn emptyBackendSnapshot Nothing [] (const (pure ())) >>= putMVar done
        takeMVar entered
        enqueuePendingInput pending (UserMessage "new")
        putMVar release ()
        _ <- takeMVar done
        seen <- newIORef []
        let retry = withPendingInputs pending $ Backend
                \state _ inputs _ -> do
                    writeIORef seen inputs
                    pure $ Right BackendResult
                        { backendOutput = emptyTurnOutput "response" [] Nothing
                        , backendState = state
                        }
        _ <- retry.submitTurn emptyBackendSnapshot Nothing [] (const (pure ()))
        readIORef seen `shouldReturn`
            [UserMessage "old", UserMessage "new"]

    it "clears queued inputs" do
        pending <- newPendingInputs
        enqueuePendingInput pending (UserMessage "stale")
        clearPendingInputs pending
        seen <- newIORef []
        let backend = withPendingInputs pending $ Backend
                \state _ inputs _ -> do
                    writeIORef seen inputs
                    pure $ Right BackendResult
                        { backendOutput = emptyTurnOutput "response" [] Nothing
                        , backendState = state
                        }
        _ <- backend.submitTurn emptyBackendSnapshot Nothing [] (const (pure ()))
        readIORef seen `shouldReturn` []

    it "does not resurrect a batch cleared during an in-flight failure" do
        pending <- newPendingInputs
        enqueuePendingInput pending (UserMessage "stale")
        entered <- newEmptyMVar
        release <- newEmptyMVar
        let backend = withPendingInputs pending $ Backend
                \_ _ _ _ -> do
                    putMVar entered ()
                    takeMVar release
                    pure (Left (ConnectionError "offline"))
        done <- newEmptyMVar
        _ <- forkIO $ backend.submitTurn emptyBackendSnapshot Nothing [] (const (pure ())) >>= putMVar done
        takeMVar entered
        clearPendingInputs pending
        putMVar release ()
        _ <- takeMVar done
        seen <- newIORef []
        let retry = withPendingInputs pending $ Backend
                \state _ inputs _ -> do
                    writeIORef seen inputs
                    pure $ Right BackendResult
                        { backendOutput = emptyTurnOutput "response" [] Nothing
                        , backendState = state
                        }
        _ <- retry.submitTurn emptyBackendSnapshot Nothing [] (const (pure ()))
        readIORef seen `shouldReturn` []

    it "serializes concurrent submission batches" do
        pending <- newPendingInputs
        enqueuePendingInput pending (UserMessage "first")
        entered <- newEmptyMVar
        release <- newEmptyMVar
        seen <- newIORef []
        let backend = withPendingInputs pending $ Backend
                \state _ inputs _ -> do
                    modifyIORef' seen (<> [inputs])
                    putMVar entered ()
                    takeMVar release
                    pure $ Left (ConnectionError "offline")
        done1 <- newEmptyMVar
        done2 <- newEmptyMVar
        _ <- forkIO $ backend.submitTurn emptyBackendSnapshot Nothing [] (const (pure ())) >>= putMVar done1
        takeMVar entered
        enqueuePendingInput pending (UserMessage "second")
        _ <- forkIO $ backend.submitTurn emptyBackendSnapshot Nothing [] (const (pure ())) >>= putMVar done2
        putMVar release ()
        _ <- takeMVar done1
        -- The second submission cannot overtake the first batch.
        takeMVar entered
        putMVar release ()
        _ <- takeMVar done2
        readIORef seen `shouldReturn`
            [ [UserMessage "first"]
            , [UserMessage "first", UserMessage "second"]
            ]
