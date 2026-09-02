module Agent.CLI.ConnectivitySpec (spec) where

import Agent.CLI.PendingInputs
    ( enqueuePendingInput
    , newPendingInputs
    , withPendingInputs
    )
import Agent.Connectivity (withConnectionRecoveryUsing)
import Agent.Error (ApiError(..))
import Agent.Loop
    ( Backend(..)
    , BackendResult(..)
    , TurnInput(..)
    , emptyBackendSnapshot
    , emptyTurnOutput
    )
import Data.IORef
import Test.Hspec

spec :: Spec
spec =
    describe "connection recovery integration" do
        it "does not duplicate queued inputs across reconnect attempts" do
            attempts <- newIORef (0 :: Int)
            pending <- newPendingInputs
            _ <- enqueuePendingInput pending (UserMessage "queued")
            seen <- newIORef []
            let backend =
                    withPendingInputs pending $
                        withConnectionRecoveryUsing
                            (const (pure ()))
                            (Backend \state _ inputs _ -> do
                                modifyIORef' seen (<> [inputs])
                                attempt <- atomicModifyIORef' attempts
                                    \n -> (n + 1, n + 1)
                                pure $
                                    if attempt == 1
                                        then Left (ConnectionError "offline")
                                        else Right BackendResult
                                            { backendOutput =
                                                emptyTurnOutput
                                                    "response" [] (Just "done")
                                            , backendState = state
                                            })
                expected = [UserMessage "queued", UserMessage "current"]
            result <-
                backend.submitTurn
                    emptyBackendSnapshot
                    Nothing
                    [UserMessage "current"]
                    (const (pure ()))
            result `shouldBe`
                Right BackendResult
                    { backendOutput =
                        emptyTurnOutput "response" [] (Just "done")
                    , backendState = emptyBackendSnapshot
                    }
            readIORef seen `shouldReturn` [expected, expected]
