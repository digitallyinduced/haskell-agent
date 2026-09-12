-- | Frontend-neutral approval and reasoning-effort policy.
module Agent.Runtime.Startup.Policy
    ( ApprovalInputs(..)
    , NativeApprovalMode(..)
    , resolveApproval
    , resolveNativeApproval
    , claudeBypassEnabled
    , reasoningEfforts
    , reasoningEffortsForDialect
    , normalizeReasoningEffortForDialect
    ) where

import Agent.Dialect (DialectId(..))
import Agent.ReasoningEffort (ReasoningEffort(..), reasoningEfforts)
import Agent.Runtime.Options (ApprovalPolicy(..))

data ApprovalInputs = ApprovalInputs
    { approvalYolo :: Maybe Bool
    , approvalDenyMutations :: Bool
    , approvalManagedTurn :: Bool
    , approvalOneShot :: Bool
    , approvalInteractive :: Bool
    , approvalProjectAutoApprove :: Bool
    } deriving (Eq, Show)

-- | Explicit approval remains authoritative. A managed turn with an explicit
-- prompt policy can use its host approval channel without borrowing stdin.
resolveApproval :: ApprovalInputs -> ApprovalPolicy
resolveApproval inputs
    | inputs.approvalYolo == Just True = ApproveAll
    | inputs.approvalDenyMutations = DenyMutating
    | inputs.approvalManagedTurn && inputs.approvalYolo == Just False =
        PromptMutating
    | inputs.approvalYolo == Just False && not inputs.approvalInteractive =
        DenyMutating
    | not inputs.approvalInteractive && inputs.approvalOneShot = ApproveAll
    | not inputs.approvalInteractive = DenyMutating
    | inputs.approvalYolo == Just False = PromptMutating
    | inputs.approvalProjectAutoApprove = ApproveAll
    | otherwise = PromptMutating

data NativeApprovalMode
    = NativeApprovalYolo
    | NativeApprovalAsk
    | NativeApprovalPlan
    deriving (Eq, Show)

resolveNativeApproval :: NativeApprovalMode -> ApprovalPolicy
resolveNativeApproval = \case
    NativeApprovalYolo -> ApproveAll
    NativeApprovalAsk -> PromptMutating
    NativeApprovalPlan -> PromptMutating

-- | A live native policy must continue through the host's mutable approval
-- channel, even when its initial mode is Yolo. The tuple flag indicates that
-- the host supports live policy changes.
claudeBypassEnabled
    :: Maybe (NativeApprovalMode, Bool)
    -> Maybe Bool
    -> Bool
    -> Bool
claudeBypassEnabled native yolo projectAutoApprove =
    case native of
        Just (mode, mutable) -> not mutable && mode == NativeApprovalYolo
        Nothing -> yolo /= Just False && (yolo == Just True || projectAutoApprove)

-- | Efforts exposed by the active model-facing protocol. Grok accepts
-- @xhigh@ but rejects the OpenAI-only @max@ value.
reasoningEffortsForDialect :: DialectId -> [ReasoningEffort]
reasoningEffortsForDialect = \case
    GrokBuildDialect -> filter (/= EffortMax) reasoningEfforts
    _ -> reasoningEfforts

-- | Normalize inherited effort when resuming or switching providers.
normalizeReasoningEffortForDialect
    :: DialectId
    -> ReasoningEffort
    -> ReasoningEffort
normalizeReasoningEffortForDialect dialect effort
    | effort `elem` reasoningEffortsForDialect dialect = effort
    | dialect == GrokBuildDialect = EffortHigh
    | otherwise = effort
