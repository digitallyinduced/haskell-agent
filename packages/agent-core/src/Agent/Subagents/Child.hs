-- | One turn of a child agent.
--
-- The host prepares the child's provider backend, tools and approval policy.
-- This module runs the turn around them. The child's backend state outlives
-- the turn, so a follow-up continues the same conversation. A command the
-- child leaves running in the background keeps the turn open, and its
-- completion notice resumes the child. The registry records the response id
-- that a follow-up continues from. A failed turn drops the tool calls it left
-- without output, so the transcript stays valid for the next request.
module Agent.Subagents.Child
    ( ChildTurn(..)
    , ChildBackgroundTasks(..)
    , runChildTurn
    , runChildWithBackgroundTasks
    ) where

import Agent.Cancel (waitCancel)
import Agent.Loop
    ( Backend
    , BackendSnapshot(..)
    , BackendStateStore(..)
    , LoopConfig(..)
    , LoopError(..)
    , LoopEvent(..)
    , LoopResult(..)
    , TurnInput(..)
    , addTokenUsage
    , advanceBackendSnapshot
    , emptyTokenUsage
    , runLoopInputs
    )
import Agent.Loop.SteeringInputs
    ( SteeringInputs
    , awaitSteeringInput
    , awaitSteeringInputReady
    , commitSteeringInputs
    , dismissBackgroundCompletion
    , enqueueBackgroundCompletion
    , newSteeringInputs
    , readSteeringInputs
    )
import Agent.Subagents.History (trimDanglingToolSuffix)
import Agent.Subagents.Registry (SubagentRegistry, setPreviousResponseId)
import Agent.Subagents.Types (SubagentSpawnEnv(..))
import Agent.ToolDispatch (ToolCall, ToolDispatchConfig)
import Agent.Tools.Types
    ( BackgroundTaskHooks(..)
    , BackgroundTaskNotice(..)
    , ToolApproval
    , ToolRegistry
    )
import Control.Concurrent.Async (race)
import Control.Concurrent.STM (STM, atomically, check, orElse)
import Data.Foldable (for_)
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , modifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )

-- | What a host supplies for one turn of a child agent.
data ChildTurn = ChildTurn
    { childBackend :: !Backend
    , childTools :: !ToolRegistry
    , childDispatch :: !ToolDispatchConfig
    , childApproval :: !(ToolCall -> IO ToolApproval)
    , childMaxTurns :: !Int
    , childOnEvent :: !(LoopEvent -> IO ())
      -- | The child's backend state. The host keeps it across turns, so a
      -- follow-up continues the same conversation.
    , childTranscript :: !(IORef BackendSnapshot)
      -- | Commands the child can leave running after its reply.
    , childBackgroundTasks :: !(Maybe ChildBackgroundTasks)
    }

-- | Commands a child starts that can outlive its reply. While one that
-- resumes automatically is still running, the child's turn stays open, and
-- its completion notice resumes the child. @agent-tools@ provides these for a
-- tool environment ('Agent.Tools.Background.toolEnvBackgroundTasks').
data ChildBackgroundTasks = ChildBackgroundTasks
    { installBackgroundTaskHooks :: !(BackgroundTaskHooks -> IO ())
    , awaitingBackgroundResume :: !(STM Bool)
    }

-- | Run one turn of a child agent. The last argument submits the turn's
-- first request, for example 'runLoopInputs' with the delivered message and
-- the child's previous response id.
runChildTurn
    :: SubagentRegistry
    -> SubagentSpawnEnv
    -> ChildTurn
    -> (LoopConfig -> IO (Either LoopError LoopResult))
    -> IO (Either LoopError LoopResult)
runChildTurn registry env child start = do
    steering <- newSteeringInputs
    for_ child.childBackgroundTasks \tasks ->
        tasks.installBackgroundTaskHooks BackgroundTaskHooks
            { backgroundTaskCompleted = \notice ->
                enqueueBackgroundCompletion
                    steering
                    notice.noticeKey
                    (UserMessage notice.noticeBody)
                    >>= \case
                        -- Do not report synchronously from the process
                        -- supervisor: loop event delivery may backpressure.
                        Left _ -> pure False
                        Right inserted -> pure inserted
            , backgroundTaskDismissed =
                dismissBackgroundCompletion steering
            }
    let config = LoopConfig
            { loopBackend = child.childBackend
            , loopBackendState = BackendStateStore
                { readBackendState = readIORef child.childTranscript
                , commitBackendState = \snapshot ->
                    atomicModifyIORef'
                        child.childTranscript
                        \current ->
                            let committed =
                                    advanceBackendSnapshot
                                        current
                                        snapshot.backendItems
                                        snapshot.backendContinuation
                            in (committed, committed)
                }
            , loopTools = child.childTools
            , loopReadTools = Nothing
            , loopDispatch = child.childDispatch
            , loopMaxTurns = child.childMaxTurns
            , loopOnEvent = child.childOnEvent
            , loopApprove = child.childApproval
            , loopReadSteering = readSteeringInputs steering
            , loopCommitSteering = commitSteeringInputs steering
            , loopCloseSteering = pure []
            , loopInterrupt = pure ()
            , loopCancel = env.subCancel
            }
    result <-
        runChildWithBackgroundTasks
            child.childBackgroundTasks steering config start
    case result of
        Right loopResult ->
            setPreviousResponseId
                registry
                env.subId
                loopResult.finalResponseId
        Left _ ->
            modifyIORef' child.childTranscript \snapshot ->
                advanceBackendSnapshot snapshot
                    (trimDanglingToolSuffix snapshot.backendItems)
                    Nothing
    pure result

-- | A final response only suspends a child while it still owns automatically
-- resumed commands. The turn stays open, and the next model request is
-- submitted when a command completes, not on a polling timer.
runChildWithBackgroundTasks
    :: Maybe ChildBackgroundTasks
    -> SteeringInputs
    -> LoopConfig
    -> (LoopConfig -> IO (Either LoopError LoopResult))
    -> IO (Either LoopError LoopResult)
runChildWithBackgroundTasks tasks steering config runFirst = do
    lastOutput <- newIORef Nothing
    let trackedConfig = config
            { loopOnEvent = \event -> do
                case event of
                    TurnFinished output -> writeIORef lastOutput (Just output)
                    _ -> pure ()
                config.loopOnEvent event
            }
        awaitingResume =
            maybe (pure False) (.awaitingBackgroundResume) tasks
        pendingWork =
            (True <$ awaitSteeringInputReady steering)
                `orElse` awaitingResume
        nextCompletion =
            (True <$ awaitSteeringInput steering)
                `orElse` do
                    awaiting <- awaitingResume
                    check (not awaiting)
                    pure False
        continue accumulatedTurns accumulatedUsage result = case result of
            Left err -> pure (Left err)
            Right finished -> do
                let totalTurns = accumulatedTurns + finished.turnsUsed
                    totalUsage = addTokenUsage accumulatedUsage finished.tokenUsage
                    aggregate = finished
                        { turnsUsed = totalTurns, tokenUsage = totalUsage }
                    remainingTurns = config.loopMaxTurns - totalTurns
                pending <- atomically pendingWork
                if not pending
                    then pure (Right aggregate)
                    else if remainingTurns <= 0
                        then readIORef lastOutput >>= \case
                            Just output -> pure (Left (LoopMaxTurns output))
                            Nothing -> pure (Left LoopNoResponseId)
                        else race (waitCancel config.loopCancel)
                                (atomically nextCompletion) >>= \case
                            Left () -> pure (Left (LoopCancelled []))
                            Right False -> pure (Right aggregate)
                            Right True -> do
                                resumed <- runLoopInputs
                                    trackedConfig { loopMaxTurns = remainingTurns }
                                    (Just finished.finalResponseId)
                                    []
                                continue totalTurns totalUsage resumed
    runFirst trackedConfig >>= continue 0 emptyTokenUsage
