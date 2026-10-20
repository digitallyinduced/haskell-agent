-- | Structured failures at subagent admission, startup, and delivery boundaries.
-- Keep diagnostic data typed until a host or tool needs display text.
module Agent.Subagents.Error
    ( CoordinationError(..)
    , AdmissionFailure(..)
    , StartupStage(..)
    , StartupRejection(..)
    , BusyState(..)
    , coordinationErrorText
    ) where

import Agent.Subagents.TaskPath (TaskPath, taskPathText)
import Agent.Subagents.Types (SubagentId(..))
import Data.Text (Text)
import qualified Data.Text as Text

data AdmissionFailure
    = AdmissionRegistryClosed
    | AdmissionTurnAborted
    | AdmissionCapacityExceeded !Int
    deriving (Eq, Show)

data StartupStage = PreparationStage | SupervisorStartupStage
    deriving (Eq, Show)

data StartupRejection
    = StartupRegistryClosed
    | StartupTurnAborted
    | StartupAgentClosed
    | StartupAgentMissing
    | StartupPathOccupied
    | StartupSupervisorPresent
    deriving (Eq, Show)

data BusyState = WorkPending | WorkRunning | WorkInterrupting
    deriving (Eq, Show)

data CoordinationError
    = CoordinationAdmissionRejected !AdmissionFailure
    | CoordinationSuspendedAdmissionRejected !AdmissionFailure
    | CoordinationDepthLimitExceeded !Int
    | CoordinationTurnBudgetExceeded !Int
    | CoordinationInvalidParentContext
    | CoordinationParentUnavailable !SubagentId
    | CoordinationInvalidTaskName !Text
    | CoordinationTaskPathOccupied !TaskPath
    | CoordinationUnknownAgent !SubagentId
    | CoordinationMissingAgent !SubagentId
    | CoordinationAgentClosed !SubagentId
    | CoordinationAgentBusy !SubagentId !BusyState
    | CoordinationStartupRejected !SubagentId !StartupRejection
    | CoordinationStartupFailed !StartupStage !Text
    | CoordinationResumePersistenceFailed !Text
    deriving (Eq, Show)

-- | Preserve the existing model-facing messages. Exception and storage details
-- are diagnostic payloads, never classification keys.
coordinationErrorText :: CoordinationError -> Text
coordinationErrorText = \case
    CoordinationAdmissionRejected failure -> case failure of
        AdmissionRegistryClosed -> "Subagent registry is closed."
        AdmissionTurnAborted -> "Root turn was aborted."
        AdmissionCapacityExceeded limit ->
            "Concurrent subagent limit reached: " <> decimal limit
                <> " agents are already active."
    CoordinationSuspendedAdmissionRejected _ ->
        "Suspended agent remains queued: registry closed, turn aborted, or concurrency capacity exhausted."
    CoordinationDepthLimitExceeded limit ->
        "Agent depth limit reached (maximum depth " <> decimal limit
            <> "). Solve the task yourself."
    CoordinationTurnBudgetExceeded limit ->
        "Subagent budget reached for this turn (maximum " <> decimal limit
            <> "). Solve the remaining task yourself."
    CoordinationInvalidParentContext -> "root spawn has inconsistent parent context"
    CoordinationParentUnavailable _ -> "Parent subagent is closed or missing."
    CoordinationInvalidTaskName detail -> detail
    CoordinationTaskPathOccupied path -> "task path already in use: " <> taskPathText path
    CoordinationUnknownAgent agent -> "unknown agent id: " <> agent.unSubagentId
    CoordinationMissingAgent _ -> "agent not found"
    CoordinationAgentClosed _ -> "agent is closed"
    CoordinationAgentBusy _ state -> case state of
        WorkPending -> "Subagent already has pending work."
        WorkRunning -> "Subagent is already running."
        WorkInterrupting -> "Subagent is still interrupting."
    CoordinationStartupRejected _ _ -> "Subagent closed before its supervisor started."
    CoordinationStartupFailed stage detail -> case stage of
        PreparationStage -> "Failed to prepare subagent: " <> detail
        SupervisorStartupStage -> "Failed to start subagent: " <> detail
    CoordinationResumePersistenceFailed detail -> detail
  where
    decimal = Text.pack . show
