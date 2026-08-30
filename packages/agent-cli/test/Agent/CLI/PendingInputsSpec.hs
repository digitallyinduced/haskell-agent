module Agent.CLI.PendingInputsSpec (spec) where

import Agent.CLI.PendingInputs
    ( PendingNoticeKind(..)
    , clearPendingInputs
    , enqueuePendingInput
    , enqueuePendingNotice
    , newPendingInputs
    , pendingInputCountLimit
    , withPendingInputs
    )
import Agent.CLI.SteeringInputs
    ( commitSteeringInputs
    , enqueueSteeringInputs
    , newSteeringInputs
    , readSteeringInputs
    , steeringInputCountLimit
    )
import Agent.Error (ApiError(..))
import Agent.Loop
    ( Backend(..)
    , BackendResult(..)
    , TurnInput(..)
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
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = do
  describe "withPendingInputs" do
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
        _ <- backend.submitTurn [] Nothing
            [UserMessage "parent"] (const (pure ()))
        readIORef seen `shouldReturn`
            [UserMessage "child result", UserMessage "parent"]

    it "requeues inputs when the backend returns an error" do
        let queued = [UserMessage "child result"]
        pending <- newPendingInputs
        mapM_ (enqueuePendingInput pending) queued
        let backend = withPendingInputs pending $ Backend
                \_ _ _ _ -> pure (Left (ConnectionError "offline"))
        _ <- backend.submitTurn [] Nothing
            [UserMessage "parent"] (const (pure ()))
        seen <- newIORef []
        let retry = withPendingInputs pending $ Backend
                \state _ inputs _ -> do
                    writeIORef seen inputs
                    pure $ Right BackendResult
                        { backendOutput = emptyTurnOutput "response" [] Nothing
                        , backendState = state
                        }
        _ <- retry.submitTurn [] Nothing [] (const (pure ()))
        readIORef seen `shouldReturn` queued

    it "requeues inputs when submission is interrupted by an exception" do
        let queued = [UserMessage "child result"]
        pending <- newPendingInputs
        mapM_ (enqueuePendingInput pending) queued
        let backend = withPendingInputs pending $ Backend
                \_ _ _ _ -> ioError (userError "interrupted")
        result <- tryAny $
            backend.submitTurn [] Nothing
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
        _ <- retry.submitTurn [] Nothing [] (const (pure ()))
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
        _ <- forkIO $ backend.submitTurn [] Nothing [] (const (pure ())) >>= putMVar done
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
        _ <- retry.submitTurn [] Nothing [] (const (pure ()))
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
        _ <- backend.submitTurn [] Nothing [] (const (pure ()))
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
        _ <- forkIO $ backend.submitTurn [] Nothing [] (const (pure ())) >>= putMVar done
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
        _ <- retry.submitTurn [] Nothing [] (const (pure ()))
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
        _ <- forkIO $ backend.submitTurn [] Nothing [] (const (pure ())) >>= putMVar done1
        takeMVar entered
        enqueuePendingInput pending (UserMessage "second")
        _ <- forkIO $ backend.submitTurn [] Nothing [] (const (pure ())) >>= putMVar done2
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

    it "rejects explicit inputs after the count budget" do
        pending <- newPendingInputs
        accepted <- mapM
            (enqueuePendingInput pending . UserMessage . Text.pack . show)
            [1 .. pendingInputCountLimit]
        accepted `shouldSatisfy` all (== Right ())
        enqueuePendingInput pending (UserMessage "overflow")
            `shouldReturn`
                Left
                    "Root input queue is full; wait for the root agent to consume pending messages."

    it "coalesces queued MCP snapshots to the latest value" do
        pending <- newPendingInputs
        enqueuePendingNotice pending PendingMcpNotice
            (UserMessage "connecting")
            `shouldReturn` Right ()
        enqueuePendingNotice pending PendingMcpNotice
            (UserMessage "settled")
            `shouldReturn` Right ()
        seen <- newIORef []
        let backend = withPendingInputs pending $ Backend
                \state _ inputs _ -> do
                    writeIORef seen inputs
                    pure $ Right BackendResult
                        { backendOutput = emptyTurnOutput "ok" [] Nothing
                        , backendState = state
                        }
        _ <- backend.submitTurn [] Nothing [] (const (pure ()))
        readIORef seen `shouldReturn` [UserMessage "settled"]

    it "emits only one bounded omission summary for rejected notices" do
        pending <- newPendingInputs
        mapM_
            (enqueuePendingInput pending . UserMessage . Text.pack . show)
            [1 .. pendingInputCountLimit]
        enqueuePendingNotice pending PendingSubagentNotice
            (UserMessage "omitted one")
            `shouldReturn`
                Left
                    "Root input queue is full; additional background notices will be summarized."
        enqueuePendingNotice pending PendingSubagentNotice
            (UserMessage "omitted two")
            `shouldReturn` Right ()
        seen <- newIORef []
        let backend = withPendingInputs pending $ Backend
                \state _ inputs _ -> do
                    writeIORef seen inputs
                    pure $ Right BackendResult
                        { backendOutput = emptyTurnOutput "ok" [] Nothing
                        , backendState = state
                        }
        _ <- backend.submitTurn [] Nothing [] (const (pure ()))
        inputs <- readIORef seen
        length inputs `shouldBe` pendingInputCountLimit + 1
        last inputs `shouldBe`
            UserMessage
                "[Some background notices were omitted because the root input queue was full: 2]"

  describe "SteeringInputs" do
    it "bounds, commits, and admits steering inputs in order" do
        steering <- newSteeringInputs
        let queued =
                [ UserMessage (Text.pack (show index))
                | index <- [1 .. steeringInputCountLimit]
                ]
        enqueueSteeringInputs steering queued `shouldReturn` Right ()
        enqueueSteeringInputs steering [UserMessage "overflow"]
            `shouldReturn`
                Left
                    "Steering queue is full; wait for the active turn to consume guidance."
        commitSteeringInputs steering 2
        enqueueSteeringInputs steering
            [UserMessage "new-1", UserMessage "new-2"]
            `shouldReturn` Right ()
        readSteeringInputs steering `shouldReturn`
            drop 2 queued <> [UserMessage "new-1", UserMessage "new-2"]
