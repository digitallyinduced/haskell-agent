-- | Pure planning for parent tool approval.
--
-- Approval facts are acquired in stages so hard denials run before dynamic
-- read-only classifiers, and plan-mode restrictions run before remembered or
-- policy-based approval. 'ApprovalPlan' tells the IO interpreter which fact or
-- effect is needed next.
module Agent.CLI.Approval.Decision
    ( ApprovalAction(..)
    , ApprovalFacts(..)
    , ApprovalNotice(..)
    , ApprovalPlan(..)
    , planApproval
    , resolveApprovalPrompt
    ) where

import Agent.CLI.Options (ApprovalPolicy(..))
import Agent.CLI.Permission (PermissionChoice(..))
import Agent.CLI.Style (glyphOk, glyphWarn)
import Agent.JsonText (jsonTextFieldDefault)
import Agent.OsPath (fromText)
import Agent.ToolDispatch
    ( ToolCall(..)
    , canonicalToolName
    , isComputerToolCallKind
    )
import Agent.Tools.Dangerous (shellCommandBlocked)
import Agent.Tools.PlanMode
    ( isPlanFileEditTarget
    , planModeBlockedEditMessage
    )
import Data.Text (Text)
import qualified Data.Text as Text
import System.OsPath (OsPath)

data ApprovalNotice
    = ApprovalWarning !Text
    | ApprovalSuccess !Text
    deriving (Eq, Show)

-- | Effects requested by a completed approval plan.
--
-- The interpreter runs these from left to right before returning the result.
data ApprovalAction
    = SetApprovalPolicy !ApprovalPolicy
    | PersistProjectAutoApprove
    | RememberToolForSession !Text
    | ReportApprovalNotice !ApprovalNotice
    deriving (Eq, Show)

-- | A staged approval decision.
--
-- Read-only classification and the session allow-list are deliberately
-- requested separately. This preserves the security-sensitive precedence:
--
-- 1. catastrophic shell hard-deny
-- 2. dynamic read-only classification
-- 3. plan-mode restrictions
-- 4. plan-file exception
-- 5. remembered tool approval
-- 6. session policy or user prompt
data ApprovalPlan
    = CompleteApproval !(Either Text Bool) ![ApprovalAction]
    | NeedReadOnlyClassification
    | NeedSessionAllowance
    | NeedPermissionPrompt
    deriving (Eq, Show)

-- | Snapshot inputs and facts discovered so far.
--
-- 'Nothing' means the interpreter has not acquired that fact yet. Calls to
-- 'planApproval' request facts only after all higher-precedence checks pass.
data ApprovalFacts = ApprovalFacts
    { policy :: !ApprovalPolicy
    , planActive :: !Bool
    , planPath :: !OsPath
    , readOnly :: !(Maybe Bool)
    , allowedForSession :: !(Maybe Bool)
    , call :: !ToolCall
    }
    deriving (Eq, Show)

planApproval :: ApprovalFacts -> ApprovalPlan
planApproval facts =
    case shellCommandBlocked toolName facts.call.arguments of
        Just message ->
            CompleteApproval
                (Left message)
                [ReportApprovalNotice
                    (ApprovalWarning (glyphWarn <> message))]
        Nothing -> case facts.readOnly of
            Nothing -> NeedReadOnlyClassification
            Just readOnly
                | planModeBlocksCall
                    facts.planActive facts.planPath readOnly facts.call ->
                        let message =
                                planModeBlockedEditMessage facts.planPath
                        in CompleteApproval
                            (Left message)
                            [ReportApprovalNotice
                                (ApprovalWarning message)]
                | isPlanFileWrite
                    facts.planActive facts.planPath facts.call ->
                        approved
                | isComputerToolCallKind facts.call.callKind ->
                    NeedPermissionPrompt
                | otherwise -> case facts.allowedForSession of
                    Nothing -> NeedSessionAllowance
                    Just True -> approved
                    Just False -> policyPlan facts.policy readOnly
  where
    toolName = canonicalToolName facts.call.name
    approved = CompleteApproval (Right True) []

resolveApprovalPrompt :: ToolCall -> Maybe PermissionChoice -> ApprovalPlan
resolveApprovalPrompt call choice
    | isComputerToolCallKind call.callKind =
        case choice of
            Nothing -> denied
            Just PermissionDeny -> denied
            Just _ -> approved
    | otherwise = resolveOrdinary choice
  where
    resolveOrdinary = \case
        Nothing -> denied
        Just PermissionDeny -> denied
        Just PermissionAllowOnce -> approved
        Just PermissionAllowAll ->
            CompleteApproval
                (Right True)
                [ SetApprovalPolicy ApproveAll
                , PersistProjectAutoApprove
                , ReportApprovalNotice
                    (ApprovalSuccess
                        (glyphOk <> "auto-approve on (saved for project)"))
                ]
        Just PermissionAllowTool ->
            CompleteApproval
                (Right True)
                [ RememberToolForSession (canonicalToolName call.name)
                , ReportApprovalNotice
                    (ApprovalSuccess
                        (glyphOk
                            <> "always allow "
                            <> call.name
                            <> " this session"))
                ]
    approved = CompleteApproval (Right True) []
    denied = CompleteApproval (Right False) []

policyPlan :: ApprovalPolicy -> Bool -> ApprovalPlan
policyPlan policy readOnly = case policy of
    ApproveAll -> CompleteApproval (Right True) []
    DenyMutating -> CompleteApproval (Right readOnly) []
    PromptMutating
        | readOnly -> CompleteApproval (Right True) []
        | otherwise -> NeedPermissionPrompt

planModeBlocksCall :: Bool -> OsPath -> Bool -> ToolCall -> Bool
planModeBlocksCall active planPath readOnly call
    | not active = False
    | name == "apply_patch" = True
    | name == "write_plan" = False
    | name == "exit_plan_mode" = False
    | name == "search_replace" =
        let target = jsonTextFieldDefault "file_path" call.arguments
        in Text.null target
            || not (isPlanFileEditTarget planPath (fromText target))
    | name `elem` ["shell_command", "run_terminal_cmd"] = True
    | name == "write_stdin" = True
    | name `elem`
        [ "spawn_agent", "followup_task", "create_agent_session"
        , "send_agent_session_message"
        ] = True
    | otherwise = not readOnly
  where
    name = canonicalToolName call.name

isPlanFileWrite :: Bool -> OsPath -> ToolCall -> Bool
isPlanFileWrite active planPath call
    | not active = False
    | name == "write_plan" = True
    | name == "search_replace" =
        let target = jsonTextFieldDefault "file_path" call.arguments
        in not (Text.null target)
            && isPlanFileEditTarget planPath (fromText target)
    | otherwise = False
  where
    name = canonicalToolName call.name
