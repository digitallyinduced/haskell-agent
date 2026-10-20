module Agent.Loop.ConversationSpec (spec) where

import Agent.Loop.Conversation
import Agent.Responses.Types (defaultResponseCreateParams)
import Control.Exception.Safe (Exception, throwIO)
import Data.IORef
import Data.Text (Text)
import Test.Hspec

data HostFailure = HostFailure deriving (Eq, Show)
instance Exception HostFailure

spec :: Spec
spec = describe "host conversation scheduling" do
    it "commits each opaque response once across continuations" do
        effects <- newIORef []
        requests <- newIORef []
        let hooks = baseHooks
                { agentLoopSendRequest = \_ previous -> do
                    modifyIORef' requests (<> [previous])
                    pure (Right (("opaque response", 42) :: (Text, Int)))
                , agentLoopOnResponse = \state _ response -> do
                    modifyIORef' effects (<> [(state.agentLoopIteration, response)])
                    pure if state.agentLoopIteration == 0
                        then AgentLoopContinue [] (Just "previous")
                        else AgentLoopFinish "done"
                }
        runAgenticLoop hooks emptyAgentLoopState `shouldReturn` "done"
        readIORef requests `shouldReturn` [Nothing, Just "previous"]
        readIORef effects `shouldReturn`
            [(0, ("opaque response", 42)), (1, ("opaque response", 42))]

    it "preserves the remaining iteration limit when resumed" do
        iterations <- newIORef []
        limits <- newIORef []
        let hooks = baseHooks
                { agentLoopMaxIterations = 4
                , agentLoopOnResponse = \state _ _ -> do
                    modifyIORef' iterations (<> [state.agentLoopIteration])
                    pure (AgentLoopContinue [] Nothing)
                , agentLoopOnMaxIterations = \state -> do
                    modifyIORef' limits (<> [state.agentLoopIteration])
                    pure "limit"
                }
        runAgenticLoop hooks emptyAgentLoopState { agentLoopIteration = 2 }
            `shouldReturn` "limit"
        readIORef iterations `shouldReturn` [2, 3]
        readIORef limits `shouldReturn` [4]

    it "honours the stop hook before building a request" do
        let hooks = baseHooks
                { agentLoopBeforeIteration = const (pure (Just "stopped"))
                , agentLoopBuildRequest = const (throwIO HostFailure)
                }
        runAgenticLoop hooks emptyAgentLoopState `shouldReturn` "stopped"

    it "propagates the original typed exception from response effects" do
        runAgenticLoop (baseHooks { agentLoopOnResponse = \_ _ _ -> throwIO HostFailure })
            emptyAgentLoopState `shouldThrow` (== HostFailure)

baseHooks :: AgentLoopHooks (Text, Int) Text
baseHooks = AgentLoopHooks
    { agentLoopMaxIterations = 5
    , agentLoopBeforeIteration = const (pure Nothing)
    , agentLoopOnMaxIterations = const (pure "limit")
    , agentLoopBuildRequest = const (pure defaultResponseCreateParams)
    , agentLoopOnRequest = \_ _ -> pure ()
    , agentLoopSendRequest = \_ _ -> pure (Right ("opaque response", 42))
    , agentLoopOnError = \_ _ err -> fail (show err)
    , agentLoopOnResponse = \_ _ _ -> pure (AgentLoopFinish "done")
    }
