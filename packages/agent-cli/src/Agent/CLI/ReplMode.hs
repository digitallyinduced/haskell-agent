-- | Idle REPL interaction mode: ask (normal), plan, or always-approve.
module Agent.CLI.ReplMode
    ( ReplMode(..)
    , cycleReplMode
    , replModeLabel
    , replModeFromState
    ) where

import Agent.CLI.Options (ApprovalPolicy(..))
import Agent.Tools.PlanMode (PlanModeState(..))
import Data.Text (Text)

-- | The three modes Shift+Tab cycles at the idle prompt.
data ReplMode
    = ReplModeNormal
    -- ^ Prompt for mutating tools (status label "ask").
    | ReplModePlan
    -- ^ Read-only except plan.md.
    | ReplModeAlwaysApprove
    -- ^ Auto-approve mutating tools (status label "yolo").
    deriving (Eq, Show)

-- | Shift+Tab order: normal → plan → always-approve → normal.
cycleReplMode :: ReplMode -> ReplMode
cycleReplMode = \case
    ReplModeNormal -> ReplModePlan
    ReplModePlan -> ReplModeAlwaysApprove
    ReplModeAlwaysApprove -> ReplModeNormal

replModeLabel :: ReplMode -> Text
replModeLabel = \case
    ReplModeNormal -> "ask"
    ReplModePlan -> "plan"
    ReplModeAlwaysApprove -> "yolo"

-- | Plan pending/active wins over yolo so cycling out of plan is well-defined
-- even when auto-approve is also on.
replModeFromState :: PlanModeState -> ApprovalPolicy -> ReplMode
replModeFromState planState policy
    | planState /= PlanInactive = ReplModePlan
    | policy == ApproveAll = ReplModeAlwaysApprove
    | otherwise = ReplModeNormal
