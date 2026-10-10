module Agent.Tools.MultiAgentsLoopSpec (spec) where

import Agent.Cancel (newCancelFlag)
import Agent.InterAgentMessage (interAgentMessagePayload)
import Agent.Loop
    ( Backend(..)
    , BackendResult(..)
    , BackendStateStore(..)
    , LoopConfig(..)
    , LoopError(..)
    , LoopResult(..)
    , TurnInput(..)
    , defaultLoopDispatch
    , defaultLoopMaxTurns
    , emptyBackendSnapshot
    , emptyTurnOutput
    , runLoopInputs
    )
import Agent.Subagents
    ( ChildTurn(..)
    , SubagentNotices(..)
    , SubagentTurnEnd(..)
    , closeSubagentRegistry
    , defaultSubagentConfig
    , newRootSubagents
    , newSubagentRegistry
    , runChildTurn
    , setSubagentRunner
    , subagentLoop
    )
import Agent.ToolDispatch (functionToolCall)
import Agent.Tools.MultiAgents (multiAgentTools, rootMultiAgentContext)
import Agent.Tools.Types (AppTool, ToolApproval(..), ToolRegistry, mkToolRegistry)
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (bracket)
import Control.Monad (forever)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import System.OsPath (unsafeEncodeUtf)
import Test.Hspec

spec :: Spec
spec = describe "Agent.Tools.MultiAgents in a root loop" do
    it "runs a spawned child and continues the root with its answer" do
        childPrompts <- newIORef ([] :: [Text])
        rootRequests <- newIORef ([] :: [[TurnInput]])
        bracket
            (newSubagentRegistry
                defaultSubagentConfig
                (unsafeEncodeUtf ".")
                (\_ _ _ _ -> pure (Left LoopNoResponseId))
                (\_ _ -> pure ()))
            closeSubagentRegistry
            \registry -> do
                childTools <- registryOf []
                setSubagentRunner registry \env previous message onEvent -> do
                    transcript <- newIORef emptyBackendSnapshot
                    modifyIORef' childPrompts (interAgentMessagePayload message :)
                    runChildTurn registry env ChildTurn
                        { childBackend = answering "child" "child-answer"
                        , childTools = childTools
                        , childDispatch = defaultLoopDispatch
                        , childApproval = const (pure ToolApprovalGranted)
                        , childMaxTurns = 5
                        , childOnEvent = onEvent
                        , childTranscript = transcript
                        , childBackgroundTasks = Nothing
                        }
                        \config -> runLoopInputs config previous [AgentMessage message]
                root <- newRootSubagents NoticesToLoop registry
                rootTools <- registryOf
                    (multiAgentTools (rootMultiAgentContext root (unsafeEncodeUtf ".")))
                step <- newIORef (0 :: Int)
                let rootBackend = Backend \current _ inputs _ -> do
                        modifyIORef' rootRequests (<> [inputs])
                        n <- atomicModifyIORef' step \k -> (k + 1, k)
                        let output = case n of
                                0 -> emptyTurnOutput "root-spawn"
                                    [ functionToolCall "spawn-1" "spawn_agent"
                                        "{\"task_name\":\"lookup\",\"message\":\"find the invoice\",\"fork_turns\":\"none\"}"
                                    ]
                                    Nothing
                                1 -> emptyTurnOutput "root-wait" [] (Just "waiting")
                                _ | reportsChild inputs ->
                                    emptyTurnOutput "root-done" [] (Just "root-done")
                                _ -> emptyTurnOutput "root-missing" [] (Just "missing")
                        pure $ Right BackendResult
                            { backendOutput = output
                            , backendState = current
                            }
                cancel <- newCancelFlag
                state <- newIORef emptyBackendSnapshot
                let config = LoopConfig
                        { loopBackend = rootBackend
                        , loopBackendState = BackendStateStore
                            { readBackendState = readIORef state
                            , commitBackendState = \snapshot ->
                                snapshot <$ writeIORef state snapshot
                            }
                        , loopTools = rootTools
                        , loopReadTools = Nothing
                        , loopDispatch = defaultLoopDispatch
                        , loopMaxTurns = defaultLoopMaxTurns
                        , loopOnEvent = const (pure ())
                        , loopApprove = const (pure ToolApprovalGranted)
                        , loopReadSteering = pure []
                        , loopWaitSteering = \_ -> forever (threadDelay maxBound)
                        , loopCommitSteering = const (pure ())
                        , loopCloseSteering = pure []
                        , loopInterrupt = pure ()
                        , loopCancel = cancel
                        , loopSubagents =
                            Just (subagentLoop (AwaitChildren 5000) root)
                        }
                result <- runLoopInputs config Nothing [UserMessage "start"]
                fmap (.finalText) result `shouldBe` Right (Just "root-done")
                readIORef childPrompts `shouldReturn` ["find the invoice"]
                length <$> readIORef rootRequests `shouldReturn` 3
  where
    reportsChild = any \case
        UserMessage text -> "child-answer" `Text.isInfixOf` text
        _ -> False

registryOf :: [AppTool] -> IO ToolRegistry
registryOf = either (fail . Text.unpack) pure . mkToolRegistry

answering :: Text -> Text -> Backend
answering responseId answer = Backend \current _ _ _ ->
    pure $ Right BackendResult
        { backendOutput = emptyTurnOutput responseId [] (Just answer)
        , backendState = current
        }
