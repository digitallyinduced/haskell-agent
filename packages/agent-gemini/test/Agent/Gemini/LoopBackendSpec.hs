module Agent.Gemini.LoopBackendSpec (spec) where

import Agent.Error (ApiError(..))
import Agent.Gemini.LoopBackend (tokenProviderStatelessGeminiBackend)
import Agent.Gemini.Response (GeminiStreamEvent(..))
import Agent.Loop (Backend(..), TurnInput(..), emptyBackendSnapshot)
import Agent.Provider
import Agent.Responses.Types (FunctionCall(..), defaultResponseCreateParams)
import Control.Monad (forM_)
import Data.IORef
import Test.Hspec

spec :: Spec
spec = describe "Gemini account replay boundary" do
    it "retains account failover before output" do
        attempts <- newIORef (0 :: Int)
        let send _ _ _ = do
                attempt <- atomicModifyIORef' attempts (\n -> (n + 1, n))
                pure $ Left $ if attempt == 0
                    then HttpError 401 "rejected"
                    else ConnectionError "stop after failover"
            backend = tokenProviderStatelessGeminiBackend provider send
                (pure defaultResponseCreateParams)
        result <- backend.submitTurn emptyBackendSnapshot Nothing
            [UserMessage "hello"] (const (pure ()))
        result `shouldBe` Left (ConnectionError "stop after failover")
        readIORef attempts `shouldReturn` 2

    forM_
        [ GeminiTextDelta "partial"
        , GeminiReasoningDelta "thinking"
        , GeminiFunctionCallReady FunctionCall
            { itemId = Nothing
            , callId = "call"
            , name = "shell_command"
            , namespace = Nothing
            , provider = Nothing
            , arguments = "{}"
            , encryptedFunctionArgs = Nothing
            , status = Nothing
            , async = Nothing
            }
        ] \event -> it ("does not replay after " <> show event) do
        attempts <- newIORef (0 :: Int)
        events <- newIORef []
        let rejected = HttpError 429 "limited after output"
            send _ _ emit = do
                modifyIORef' attempts (+ 1)
                emit event
                pure (Left rejected)
            backend = tokenProviderStatelessGeminiBackend provider send
                (pure defaultResponseCreateParams)
        result <- backend.submitTurn emptyBackendSnapshot Nothing
            [UserMessage "hello"] (\value -> modifyIORef' events (<> [value]))
        result `shouldBe` Left rejected
        readIORef attempts `shouldReturn` 1
        length <$> readIORef events `shouldReturn` 1

provider :: TokenProvider
provider = tokenProvider SubscriptionBilled $
    const (pure (Right Credential
        { accessToken = "test"
        , accountId = "test"
        , leaseId = Nothing
        , provider = GeminiProvider
        }))
