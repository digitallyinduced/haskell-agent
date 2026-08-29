module Agent.Subagents.Registry.Internal.Types where

import Agent.Cancel (CancelFlag)
import Agent.InterAgentMessage (InterAgentMessage)
import Agent.Loop (LoopEvent)
import Agent.Subagents.TaskPath (TaskPath)
import Agent.Subagents.Types
    ( RootTurnId
    , RunSubagent
    , SubagentAccessProfile
    , SubagentConfig
    , SubagentId
    , SubagentStatus(..)
    )
import Control.Concurrent.Async (Async)
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.STM (TQueue, TVar)
import Data.Acquire (Acquire, mkAcquire)
import Data.IORef (IORef)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Word (Word64)
import System.OsPath (OsPath)

data SubagentRecord = SubagentRecord
    { recordId :: !SubagentId
    , recordParent :: !(Maybe SubagentId)
    , recordDepth :: !Int
    , recordNickname :: !(Maybe Text)
    , recordPhase :: !(TVar SubagentPhase)
    , recordCancel :: !CancelFlag
    , recordMailbox :: !(TQueue SubagentWork)
    , recordAsync :: !(TVar (Maybe (Async ())))
      -- | Last successful response id for conversation continuity.
    , recordPreviousResponseId :: !(TVar (Maybe Text))
    , recordLastUpdate :: !(TVar (Maybe (Int, SubagentStatus)))
    , recordTaskPath :: !TaskPath
    , recordCwd :: !OsPath
    , recordAccessProfile :: !(TVar SubagentAccessProfile)
    }

data SubagentWork = SubagentWork
    { workRootTurnId :: !(Maybe RootTurnId)
    , workMessage :: !InterAgentMessage
    }

-- | The complete turn lifecycle of an open subagent.
--
-- Pending, running, and interrupting phases each own one active-turn slot.
-- Idle and closed phases never do. Keeping these facts in one value prevents
-- combinations such as an idle agent that still holds capacity.
data SubagentPhase
    = AgentIdle !SubagentStatus !(Maybe RootTurnId)
    | AgentPending !SubagentWork
    | AgentRunning !(Maybe RootTurnId)
    | AgentInterrupting !(Maybe RootTurnId)
    | AgentClosed

phaseStatus :: SubagentPhase -> SubagentStatus
phaseStatus = \case
    AgentIdle status _ -> status
    AgentPending{} -> Pending
    AgentRunning{} -> Running
    AgentInterrupting{} -> Interrupted
    AgentClosed -> Closed

phaseRootTurnId :: SubagentPhase -> Maybe RootTurnId
phaseRootTurnId = \case
    AgentPending work -> work.workRootTurnId
    AgentRunning rootTurnId -> rootTurnId
    AgentInterrupting rootTurnId -> rootTurnId
    AgentIdle _ rootTurnId -> rootTurnId
    AgentClosed -> Nothing

phaseHoldsSlot :: SubagentPhase -> Bool
phaseHoldsSlot = \case
    AgentPending{} -> True
    AgentRunning{} -> True
    AgentInterrupting{} -> True
    AgentIdle{} -> False
    AgentClosed -> False

-- | Resources acquired while preparing an agent and transferred to its
-- supervisor. They remain alive across turns and are released in reverse order
-- when the supervisor exits.
newtype SubagentLease = SubagentLease (Acquire ())

instance Semigroup SubagentLease where
    SubagentLease left <> SubagentLease right = SubagentLease (left *> right)

instance Monoid SubagentLease where
    mempty = SubagentLease (pure ())

-- | Attach an already-acquired resource to the lifetime of a subagent.
subagentLease :: IO () -> SubagentLease
subagentLease cleanup =
    SubagentLease (mkAcquire (pure ()) (const cleanup))

data SubagentRegistry = SubagentRegistry
    { registryAgents :: !(TVar (Map SubagentId SubagentRecord))
    , registryPaths :: !(TVar (Map TaskPath SubagentId))
    , registryLiveCount :: !(TVar Int)
    , registryNextUpdateSeq :: !(TVar Int)
    , registryWaitCursors :: !(TVar (Map (Maybe SubagentId) Int))
    , registryActiveWaits :: !(TVar (Map (Maybe SubagentId) [SubagentId]))
    , registryConfig :: !(TVar SubagentConfig)
    , registryRunRef :: !(IORef RunSubagent)
    , registryOnEvent :: !(SubagentId -> LoopEvent -> IO ())
    , registryOnCompleteRef :: !(IORef (SubagentId -> SubagentStatus -> IO ()))
    , registryOnSettledRef :: !(IORef (SubagentId -> SubagentStatus -> IO ()))
    , registryCwd :: !OsPath
    , registryClosed :: !(TVar Bool)
    , registryNextSubagentId :: !(TVar Int)
    , registryNextRootTurnId :: !(TVar Word64)
    , registryAbortedRootTurns :: !(TVar (Set RootTurnId))
    , registryLifecycle :: !(MVar ())
    }
