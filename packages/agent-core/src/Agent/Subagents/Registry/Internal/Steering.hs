-- | Connect a running child's mailbox to the loop's acknowledged guidance.
module Agent.Subagents.Registry.Internal.Steering
    ( withSubagentMessageSteering
    , enqueueSubagentWorkSTM
    , commitSubagentMessageSteering
    ) where

import Agent.Loop (TurnInput(..))
import Agent.Loop.SteeringInputs
    ( SteeringInputs
    , commitSteeringInputsSTM
    , enqueueSteeringInputsSTM
    , readSteeringInputs
    )
import Agent.Subagents.Registry.Internal.Types
import Agent.Subagents.Types (SubagentSpawnEnv(..))
import Control.Concurrent.STM
import Control.Exception.Safe (bracket)
import Control.Monad (replicateM_, void)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Queue-only mail can steer an active turn without starting an idle one.
-- The final-answer gate rejects late guidance; the supervisor then retains it
-- in the ordinary mailbox for its next turn. Overflow follows the same path.
enqueueSubagentWorkSTM :: SubagentRecord -> SubagentWork -> STM ()
enqueueSubagentWorkSTM record work = do
    active <- readTVar record.recordSteering
    backlog <- not <$> isEmptyTQueue record.recordMailbox
    case if backlog then Nothing else active of
        Nothing -> writeTQueue record.recordMailbox work
        Just steering -> do
            accepted <- enqueueSteeringInputsSTM steering.steeringInputs
                [AgentMessage work.workMessage]
            case accepted of
                Left _ -> writeTQueue record.recordMailbox work
                Right () -> writeTQueue steering.steeringWork work

withSubagentMessageSteering
    :: SubagentRegistry
    -> SubagentSpawnEnv
    -> SteeringInputs
    -> (SubagentSteering -> IO a)
    -> IO a
withSubagentMessageSteering registry env inputs action = do
    work <- newTQueueIO
    let steering = SubagentSteering inputs work
        acquire = atomically do
            agents <- readTVar registry.registryAgents
            record <- maybe (throwSTM (userError "Missing child mailbox")) pure
                (Map.lookup env.subId agents)
            active <- readTVar record.recordSteering
            case active of
                Just _ -> throwSTM (userError "Child mailbox already has an active turn")
                Nothing -> pure ()
            writeTVar record.recordSteering (Just steering)
            queued <- flushTQueue record.recordMailbox
            mapM_ (enqueueSubagentWorkSTM record) queued
            pure record
        release record = atomically do
            writeTVar record.recordSteering Nothing
            pending <- flushTQueue steering.steeringWork
            phase <- readTVar record.recordPhase
            aborted <- readTVar registry.registryAbortedRootTurns
            let retain queued = maybe True (`Set.notMember` aborted) queued.workRootTurnId
            case phase of
                AgentClosed -> pure ()
                _ -> mapM_ (unGetTQueue record.recordMailbox)
                    (reverse (filter retain pending))
    bracket acquire release (const (action steering))

-- | Background notices share the loop queue but have no mailbox work record.
-- Reservation keeps the acknowledged prefix stable while new mail arrives.
commitSubagentMessageSteering :: SubagentSteering -> Int -> IO ()
commitSubagentMessageSteering steering count = do
    inputs <- readSteeringInputs steering.steeringInputs
    let messageCount = length [() | AgentMessage _ <- take (max 0 count) inputs]
    atomically do
        replicateM_ messageCount (void (readTQueue steering.steeringWork))
        commitSteeringInputsSTM steering.steeringInputs count
