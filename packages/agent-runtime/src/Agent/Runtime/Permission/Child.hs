-- | Approval policy for child agents, which cannot open parent prompts.
module Agent.Runtime.Permission.Child (childApprove, scopedToolPolicy) where

import Agent.Runtime.Options (ApprovalPolicy(..))
import Agent.ToolDispatch (ToolCall(..), isComputerToolCallKind)
import Agent.Tools.Types
    ( ToolRegistry, ToolApproval(..), ApprovalRequirement(..)
    , lookupRegisteredTool, toolAcceptsCall, toolApprovalRequirement
    , toolAllowsWithoutPrompt, toolAutoApproves )

childApprove :: ApprovalPolicy -> ToolRegistry -> ToolCall -> IO ToolApproval
childApprove _ tools call
    | Just tool <- lookupRegisteredTool call.name tools
    , not (toolAcceptsCall tool call) =
        pure $ ToolApprovalDenied
            "Mismatched provider-native tool call kind requires parent review."
childApprove _ _ call
    | isComputerToolCallKind call.callKind =
        pure $ ToolApprovalDenied
            "Computer use must be approved in the interactive parent session."
childApprove policy tools call =
    case lookupRegisteredTool call.name tools of
        Just tool -> toolApprovalRequirement tool call >>= decide
        Nothing -> ordinaryDecision Nothing
  where
    decide FreshApprovalRequired =
        pure $ ToolApprovalDenied
            "This sensitive tool requires an explicit parent approval for every call."
    decide SandboxEscalationApprovalRequired
        | policy /= ApproveAll =
            pure $ ToolApprovalDenied "Sandbox escalation requires parent approval or --yolo."
    decide requirement = case scopedToolPolicy policy tools call of
        ApproveAll -> pure ToolApprovalGranted
        DenyMutating ->
            pure $
                if requirement == ApprovalNotRequired
                    then ToolApprovalGranted
                    else ToolApprovalRejected
        PromptMutating
            | requirement == ApprovalNotRequired -> pure ToolApprovalGranted
            | otherwise -> childCannotPrompt

    ordinaryDecision tool = case scopedToolPolicy policy tools call of
        ApproveAll -> pure ToolApprovalGranted
        DenyMutating -> do
            readOnly <- isReadOnly tool
            pure $
                if readOnly then ToolApprovalGranted else ToolApprovalRejected
        PromptMutating ->
            isReadOnly tool >>= \case
                True -> pure ToolApprovalGranted
                False -> childCannotPrompt

    isReadOnly = \case
        Just tool -> toolAllowsWithoutPrompt tool call
        Nothing -> pure False

    childCannotPrompt = pure $ ToolApprovalDenied
        "Subagent cannot prompt for approval on mutating tools. \
        \Re-run the parent with auto-approve/--yolo, or have the \
        \parent perform this edit."

-- Only the ordinary mutation prompt is waived. Classification, plan mode,
-- dangerous-command checks and computer-use consent still run as before.
-- Do not change the live policy or persist a project-wide YOLO setting.
scopedToolPolicy :: ApprovalPolicy -> ToolRegistry -> ToolCall -> ApprovalPolicy
scopedToolPolicy PromptMutating tools call
    | Just tool <- lookupRegisteredTool call.name tools
    , toolAutoApproves tool = ApproveAll
scopedToolPolicy policy _ _ = policy
