module Agent.Tools.MultiAgentsLoopSpec (spec) where

import Agent.Cancel (newCancelFlag)
import Agent.InterAgentMessage
    ( InterAgentMessage(..), InterAgentMessageType(..)
    , interAgentMessagePayload, plainInterAgentContent, newInterAgentMessageMetadata
    )
import qualified Agent.Loop.InputQueue as InputQueue
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
    , setRootMessageWaiter
    , subagentLoop
    )
import Agent.ToolDispatch (functionToolCall)
import Agent.Tools.MultiAgents (multiAgentTools, rootMultiAgentContext)
import Agent.Tools.Types (AppTool, ToolApproval(..), ToolRegistry, mkToolRegistry)
import qualified Control.Concurrent as Concurrent
import Control.Exception.Safe (bracket)
import Control.Concurrent.STM (atomically)
import qualified Control.Monad as Monad
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import System.OsPath (unsafeEncodeUtf)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "Agent.Tools.MultiAgents in a root loop" do
    it "wakes wait_agent for already queued mail and acknowledges its identity once" do
        metadata <- newInterAgentMessageMetadata (Just "send-call") (Just "1")
        let message = InterAgentMessage "/root/worker" "/root" QueuedMessage
                (plainInterAgentContent "result ready") (Just metadata)
        requests <- newIORef []
        steering <- InputQueue.newInputQueue
        bracket
            (newSubagentRegistry defaultSubagentConfig (unsafeEncodeUtf ".")
                (\_ _ _ _ -> pure (Left LoopNoResponseId)) (\_ _ -> pure ()))
            closeSubagentRegistry \registry -> do
                root <- newRootSubagents NoticesToLoop registry
                setRootMessageWaiter registry (InputQueue.awaitUserSteeringAfter steering 0)
                tools <- registryOf (multiAgentTools (rootMultiAgentContext root (unsafeEncodeUtf ".")))
                cancel <- newCancelFlag
                state <- newIORef emptyBackendSnapshot
                let backend = Backend \current _ inputs _ -> do
                        index <- atomicModifyIORef' requests \previous ->
                            (previous <> [inputs], length previous)
                        pure $ Right BackendResult
                            { backendOutput = if index == 0
                                then emptyTurnOutput "waiting"
                                    [functionToolCall "wait-call" "wait_agent" "{\"timeout_ms\":10000}"] Nothing
                                else emptyTurnOutput "done" [] (Just "finished")
                            , backendState = current
                            }
                    config = LoopConfig
                        { loopBackend = backend
                        , loopBackendState = BackendStateStore
                            { readBackendState = readIORef state
                            , commitBackendState = \snapshot -> snapshot <$ writeIORef state snapshot
                            }
                        , loopTools = tools
                        , loopReadTools = Nothing
                        , loopDispatch = defaultLoopDispatch
                        , loopMaxTurns = 5
                        , loopOnEvent = const (pure ())
                        , loopApprove = const (pure ToolApprovalGranted)
                        , loopWithToolExecution = \_ action -> do
                            InputQueue.enqueueSteeringInputs steering [AgentMessage message]
                                >>= either (fail . Text.unpack) pure
                            action
                        , loopReadSteering = InputQueue.reserveSteeringInputs steering
                        , loopWaitSteering = atomically . InputQueue.awaitUserSteeringAfter steering
                        , loopCommitSteering = InputQueue.commitSteeringInputs steering
                        , loopCloseSteering = InputQueue.closeSteeringInputs steering
                        , loopInterrupt = pure ()
                        , loopCancel = cancel
                        , loopSubagents = Nothing
                        }
                result <- timeout 2000000 (runLoopInputs config Nothing [UserMessage "wait"])
                fmap (fmap (.finalText)) result `shouldBe` Just (Right (Just "finished"))
                received <- readIORef requests
                [value | inputs <- received, AgentMessage value <- inputs] `shouldBe` [message]
                InputQueue.readSteeringInputs steering `shouldReturn` []
                InputQueue.hasSteeringInputWake steering `shouldReturn` False

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
                        , loopWithToolExecution = \_ action -> action
                        , loopReadSteering = pure []
                        , loopWaitSteering = \_ -> Monad.forever (Concurrent.threadDelay maxBound)
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
