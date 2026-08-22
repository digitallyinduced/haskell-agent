module Agent.CLI.ConnectivitySpec (spec) where

import Agent.CLI.Connectivity
    ( reconnectDelayMicros
    , withConnectionRecoveryUsing
    )
import Agent.CLI.PendingInputs (withPendingInputs)
import Agent.Error (ApiError(..))
import Agent.Loop
    ( Backend(..)
    , LoopEvent(..)
    , TurnInput(..)
    , emptyTurnOutput
    )
import Data.IORef
import Test.Hspec

spec :: Spec
spec = describe "withConnectionRecovery" do
    it "retries the same submission until connectivity returns" do
        attempts <- newIORef (0 :: Int)
        waits <- newIORef []
        seen <- newIORef []
        events <- newIORef []
        let backend = withConnectionRecoveryUsing
                (\attempt -> modifyIORef' waits (<> [attempt]))
                (Backend \previous inputs _ -> do
                    modifyIORef' seen (<> [(previous, inputs)])
                    attempt <- atomicModifyIORef' attempts \n -> (n + 1, n + 1)
                    pure $
                        if attempt < 3
                            then Left (ConnectionError "offline")
                            else Right (emptyTurnOutput "response" [] (Just "done")))
            inputs = [UserMessage "continue"]
        result <- backend.submitTurn (Just "previous") inputs
            (\event -> modifyIORef' events (<> [event]))

        result `shouldBe`
            Right (emptyTurnOutput "response" [] (Just "done"))
        readIORef waits `shouldReturn` [1, 2]
        readIORef seen `shouldReturn`
            [ (Just "previous", inputs)
            , (Just "previous", inputs)
            , (Just "previous", inputs)
            ]
        readIORef events `shouldReturn`
            [ ActivityUpdated
                "Connection lost; waiting for internet. Retrying automatically in 1s (Esc or Ctrl-C to cancel)…"
            , ActivityUpdated "Checking internet connection…"
            , ActivityUpdated
                "Connection lost; waiting for internet. Retrying automatically in 2s (Esc or Ctrl-C to cancel)…"
            , ActivityUpdated "Checking internet connection…"
            ]

    it "does not retry non-connection provider errors" do
        waits <- newIORef []
        let expected = HttpError 503 "unavailable"
            backend = withConnectionRecoveryUsing
                (\attempt -> modifyIORef' waits (<> [attempt]))
                (Backend \_ _ _ -> pure (Left expected))
        result <- backend.submitTurn Nothing [] (const (pure ()))
        result `shouldBe` Left expected
        readIORef waits `shouldReturn` []

    it "recovers from a WebSocket frame parse disconnect" do
        attempts <- newIORef (0 :: Int)
        let backend = withConnectionRecoveryUsing
                (const (pure ()))
                (Backend \_ _ _ -> do
                    attempt <- atomicModifyIORef' attempts \n -> (n + 1, n + 1)
                    pure $
                        if attempt == 1
                            then Left $ ConnectionError
                                "WebSocket receive error: ParseException \"not enough bytes\""
                            else Right
                                (emptyTurnOutput "response" [] (Just "done")))
        result <- backend.submitTurn Nothing [] (const (pure ()))
        result `shouldBe`
            Right (emptyTurnOutput "response" [] (Just "done"))
        readIORef attempts `shouldReturn` 2

    it "does not replay a submission after visible output streamed" do
        attempts <- newIORef (0 :: Int)
        waits <- newIORef []
        let backend = withConnectionRecoveryUsing
                (\attempt -> modifyIORef' waits (<> [attempt]))
                (Backend \_ _ onEvent -> do
                    modifyIORef' attempts (+ 1)
                    onEvent (TextDelta "partial")
                    pure (Left (ConnectionError "dropped")))
        result <- backend.submitTurn Nothing [] (const (pure ()))
        result `shouldBe` Left (ConnectionError "dropped")
        readIORef attempts `shouldReturn` 1
        readIORef waits `shouldReturn` []

    it "does not duplicate queued inputs across reconnect attempts" do
        attempts <- newIORef (0 :: Int)
        pending <- newIORef [UserMessage "queued"]
        seen <- newIORef []
        let backend =
                withPendingInputs pending $
                    withConnectionRecoveryUsing
                        (const (pure ()))
                        (Backend \_ inputs _ -> do
                            modifyIORef' seen (<> [inputs])
                            attempt <- atomicModifyIORef' attempts
                                \n -> (n + 1, n + 1)
                            pure $
                                if attempt == 1
                                    then Left (ConnectionError "offline")
                                    else Right
                                        (emptyTurnOutput
                                            "response" [] (Just "done")))
            expected = [UserMessage "queued", UserMessage "current"]
        result <- backend.submitTurn Nothing [UserMessage "current"]
            (const (pure ()))
        result `shouldBe`
            Right (emptyTurnOutput "response" [] (Just "done"))
        readIORef seen `shouldReturn` [expected, expected]
        readIORef pending `shouldReturn` []

    it "backs off to a bounded polling interval" do
        map reconnectDelayMicros [1 .. 7]
            `shouldBe`
                [ 1_000_000
                , 2_000_000
                , 4_000_000
                , 8_000_000
                , 15_000_000
                , 15_000_000
                , 15_000_000
                ]
