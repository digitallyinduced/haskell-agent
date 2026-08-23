module Agent.CLI.PendingInputsSpec (spec) where

import Agent.CLI.PendingInputs (withPendingInputs)
import Agent.Error (ApiError(..))
import Agent.Loop
    ( Backend(..)
    , BackendResult(..)
    , TurnInput(..)
    , emptyTurnOutput
    )
import Control.Exception.Safe (tryAny)
import Data.Either (isLeft)
import Data.IORef
import Test.Hspec

spec :: Spec
spec = describe "withPendingInputs" do
    it "commits queued inputs after a successful submission" do
        pending <- newIORef [UserMessage "child result"]
        seen <- newIORef []
        let backend = withPendingInputs pending $ Backend
                \state _ inputs _ -> do
                    writeIORef seen inputs
                    pure $ Right BackendResult
                        { backendOutput =
                            emptyTurnOutput "response" [] Nothing
                        , backendState = state
                        }
        _ <- backend.submitTurn () Nothing
            [UserMessage "parent"] (const (pure ()))
        readIORef seen `shouldReturn`
            [UserMessage "child result", UserMessage "parent"]
        readIORef pending `shouldReturn` []

    it "requeues inputs when the backend returns an error" do
        let queued = [UserMessage "child result"]
        pending <- newIORef queued
        let backend = withPendingInputs pending $ Backend
                \_ _ _ _ -> pure (Left (ConnectionError "offline"))
        _ <- backend.submitTurn () Nothing
            [UserMessage "parent"] (const (pure ()))
        readIORef pending `shouldReturn` queued

    it "requeues inputs when submission is interrupted by an exception" do
        let queued = [UserMessage "child result"]
        pending <- newIORef queued
        let backend = withPendingInputs pending $ Backend
                \_ _ _ _ -> ioError (userError "interrupted")
        result <- tryAny $
            backend.submitTurn () Nothing
                [UserMessage "parent"] (const (pure ()))
        result `shouldSatisfy` isLeft
        readIORef pending `shouldReturn` queued
