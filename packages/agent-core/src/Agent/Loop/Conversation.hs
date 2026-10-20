-- | Lifecycle hooks for hosts that commit native response effects themselves.
-- The response remains opaque, preserving provider payloads and host metadata.
module Agent.Loop.Conversation
    ( AgentLoopState(..)
    , AgentLoopDecision(..)
    , AgentLoopHooks(..)
    , emptyAgentLoopState
    , runAgenticLoop
    ) where

import qualified Agent.Loop as Agent
import Agent.Cancel (newCancelFlag)
import Agent.Loop.Native (noLiveSteering)
import Agent.Error (ApiError)
import Agent.Responses.Types (ResponseCreateParams, ResponseItem)
import qualified Agent.Tools.Types as Tools
import qualified Control.Exception.Safe as Exception
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text

data AgentLoopState = AgentLoopState
    { agentLoopIteration :: !Int
    , agentLoopInput :: ![ResponseItem]
    , agentLoopPreviousResponseId :: !(Maybe Text)
    }

emptyAgentLoopState :: AgentLoopState
emptyAgentLoopState = AgentLoopState 0 [] Nothing

data AgentLoopDecision result
    = AgentLoopContinue ![ResponseItem] !(Maybe Text)
    | AgentLoopFinish result

data AgentLoopHooks response result = AgentLoopHooks
    { agentLoopMaxIterations :: !Int
    , agentLoopBeforeIteration :: AgentLoopState -> IO (Maybe result)
    , agentLoopOnMaxIterations :: AgentLoopState -> IO result
    , agentLoopBuildRequest :: AgentLoopState -> IO ResponseCreateParams
    , agentLoopOnRequest :: AgentLoopState -> ResponseCreateParams -> IO ()
    , agentLoopSendRequest :: ResponseCreateParams -> Maybe Text -> IO (Either ApiError response)
    , agentLoopOnError :: AgentLoopState -> ResponseCreateParams -> ApiError -> IO (AgentLoopDecision result)
    , agentLoopOnResponse :: AgentLoopState -> ResponseCreateParams -> response -> IO (AgentLoopDecision result)
    }

-- | Schedule model turns with agent-core while the host owns successful
-- response effects. Retrying a submission never re-enters the response hook.
-- Senders own their request retry policy, normally through 'Agent.Turn.runTurn'.
runAgenticLoop :: AgentLoopHooks response result -> AgentLoopState -> IO result
runAgenticLoop hooks initialState
    | initialState.agentLoopIteration >= hooks.agentLoopMaxIterations =
        hooks.agentLoopOnMaxIterations initialState
    | otherwise = do
        stateRef <- newIORef initialState
        resultRef <- newIORef Nothing
        continueRef <- newIORef False
        failureRef <- newIORef Nothing
        checkpointRef <- newIORef Agent.emptyBackendSnapshot
        cancel <- newCancelFlag
        registry <- either (fail . show) pure (Tools.mkToolRegistry [])
        let remember action = Exception.catchAny action \exception -> do
                writeIORef failureRef (Just exception)
                Exception.throwIO exception
            backend = Agent.backendWithCallbacks \snapshot _ _ callbacks -> remember do
                callbacks.onCancellationMode Agent.CancelSubmission
                state <- readIORef stateRef
                decision <- submitTurn hooks state
                case decision of
                    AgentLoopFinish result -> do
                        writeIORef resultRef (Just result)
                        writeIORef continueRef False
                    AgentLoopContinue items previous -> do
                        writeIORef stateRef AgentLoopState
                            { agentLoopIteration = state.agentLoopIteration + 1
                            , agentLoopInput = items
                            , agentLoopPreviousResponseId = previous
                            }
                        writeIORef continueRef True
                pure $ Right Agent.BackendResult
                    { backendOutput = Agent.emptyTurnOutput ("host-turn-" <> Text.pack (show state.agentLoopIteration)) [] Nothing
                    , backendState = snapshot
                    }
            readContinuation = do
                continuing <- readIORef continueRef
                pure [Agent.UserMessage "" | continuing]
            config = Agent.LoopConfig
                { loopBackend = backend
                , loopBackendState = Agent.BackendStateStore
                    { readBackendState = readIORef checkpointRef
                    , commitBackendState = \candidate -> writeIORef checkpointRef candidate >> pure candidate
                    }
                , loopTools = registry
                , loopReadTools = Nothing
                , loopDispatch = Agent.defaultLoopDispatch
                , loopWithToolExecution = \_ action -> action
                , loopMaxTurns = hooks.agentLoopMaxIterations - initialState.agentLoopIteration
                , loopOnEvent = const (pure ())
                , loopApprove = const (pure Agent.ToolApprovalGranted)
                , loopReadSteering = readContinuation
                , loopWaitSteering = const noLiveSteering
                , loopCommitSteering = const (pure ())
                , loopCloseSteering = pure []
                , loopInterrupt = pure ()
                , loopCancel = cancel
                , loopSubagents = Nothing
                }
        execution <- Agent.runLoopInputsDetailed config Nothing []
        -- The core reports synchronous failures as data; preserve the host's
        -- typed exception for its durable scheduling and delivery policies.
        readIORef failureRef >>= mapM_ Exception.throwIO
        readIORef resultRef >>= \case
            Just result -> pure result
            Nothing -> case execution.executionResult of
                Left (Agent.LoopMaxTurns _) -> readIORef stateRef >>= hooks.agentLoopOnMaxIterations
                Left err -> fail ("ConversationLoop: loop failed: " <> show err)
                Right _ -> fail "ConversationLoop: loop ended without a lifecycle result"

submitTurn
    :: AgentLoopHooks response result -> AgentLoopState
    -> IO (AgentLoopDecision result)
submitTurn hooks state = do
    before <- hooks.agentLoopBeforeIteration state
    case before of
        Just result -> pure (AgentLoopFinish result)
        Nothing -> do
            request <- hooks.agentLoopBuildRequest state
            hooks.agentLoopOnRequest state request
            hooks.agentLoopSendRequest request state.agentLoopPreviousResponseId >>= \case
                Right response -> hooks.agentLoopOnResponse state request response
                Left err -> hooks.agentLoopOnError state request err
