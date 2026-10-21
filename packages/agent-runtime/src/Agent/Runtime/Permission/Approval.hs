-- | Interpret approval policy through frontend-supplied effects.
module Agent.Runtime.Permission.Approval
    ( approveToolDecisionWithReporterAndPersistenceClassifiedWithPrompt ) where

import Agent.Runtime.Permission.Decision
import Agent.Runtime.Permission.Child (scopedToolPolicy)
import Agent.Runtime.Permission.Types (PermissionChoice)
import Agent.Runtime.Options (ApprovalPolicy(..))
import Agent.ToolDispatch (ToolCall(..), canonicalToolName)
import Agent.Tools.PlanMode (PlanModeEnv, isPlanModeActive, planFilePath)
import Agent.Tools.Types
    ( ToolApproval(..), ToolRegistry, ApprovalRequirement(..), ApprovalRule(..)
    , AppTool(..), lookupRegisteredTool, toolAcceptsCall, toolApprovalRequirement )
import Data.IORef
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

approveToolDecisionWithReporterAndPersistenceClassifiedWithPrompt
    :: (ToolCall -> IO (Maybe Bool))
    -> (Bool -> ToolCall -> IO (Maybe PermissionChoice))
    -> (ApprovalNotice -> IO ())
    -> IO ()
    -> IORef ApprovalPolicy
    -> IORef (Set Text)
    -> ToolRegistry
    -> PlanModeEnv
    -> ToolCall
    -> IO ToolApproval
approveToolDecisionWithReporterAndPersistenceClassifiedWithPrompt
        classifyReadOnly requestPermission report persistAlwaysApprove
        policyRef allowedToolsRef tools planMode call =
    case lookupRegisteredTool call.name tools of
        Just tool
            | not (toolAcceptsCall tool call) -> do
                let message =
                        "Rejected: mismatched provider-native tool call kind."
                report (ApprovalWarning message)
                pure (ToolApprovalDenied message)
        _ -> do
            policy <- readIORef policyRef
            planActive <- isPlanModeActive planMode
            planPath <- planFilePath planMode
            let initialFacts = ApprovalFacts
                    { policy = scopedToolPolicy policy tools call
                    , planActive
                    , planPath
                    , readOnly = Nothing
                    , allowedForSession = Nothing
                    , requiresExplicitApproval = False
                    , call
                    }
            interpret initialFacts (planApproval initialFacts)
  where
    interpret facts = \case
        CompleteApproval result actions -> do
            mapM_ runAction actions
            pure result
        NeedReadOnlyClassification -> do
            let registeredTool = lookupRegisteredTool call.name tools
            requirement <- case registeredTool of
                Just tool -> registeredRequirement tool tool.appToolApproval
                Nothing -> classifyReadOnly call >>= \case
                    Just True -> pure ApprovalNotRequired
                    _ -> pure ApprovalPromptRequired
            policy <- readIORef policyRef
            let requiresExplicit = requirement == FreshApprovalRequired
                    || (requirement == SandboxEscalationApprovalRequired
                        && policy /= ApproveAll)
                readOnly = requirement == ApprovalNotRequired
            let nextFacts = facts
                    { readOnly = Just readOnly
                    , requiresExplicitApproval = requiresExplicit
                    }
            interpret nextFacts (planApproval nextFacts)
        NeedSessionAllowance -> do
            allowed <- readIORef allowedToolsRef
            let toolName = canonicalToolName call.name
                nextFacts = facts
                    { allowedForSession =
                        Just (Set.member toolName allowed)
                    }
            interpret nextFacts (planApproval nextFacts)
        NeedPermissionPrompt -> do
            choice <- requestPermission facts.requiresExplicitApproval call
            interpret facts
                (resolveApprovalPromptWith
                    facts.requiresExplicitApproval call choice)

    registeredRequirement tool = \case
        AlwaysConfirm -> pure FreshApprovalRequired
        ClassifyApproval classify -> classify call
        AutoApprove original -> registeredRequirement tool original
        _ -> classifyReadOnly call >>= \case
            Just True -> pure ApprovalNotRequired
            Just False -> pure ApprovalPromptRequired
            Nothing -> toolApprovalRequirement tool call

    runAction = \case
        SetApprovalPolicy next ->
            atomicModifyIORef' policyRef (const (next, ()))
        PersistProjectAutoApprove ->
            persistAlwaysApprove
        RememberToolForSession toolName ->
            modifyIORef' allowedToolsRef (Set.insert toolName)
        ReportApprovalNotice notice ->
            report notice
