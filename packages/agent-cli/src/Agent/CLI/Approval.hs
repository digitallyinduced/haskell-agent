-- | Parent and child tool-approval policy for interactive CLI sessions.
module Agent.CLI.Approval
    ( ApprovalAction(..)
    , ApprovalFacts(..)
    , ApprovalNotice(..)
    , ApprovalPlan(..)
    , approveToolDecision
    , approveToolDecisionClassified
    , approveToolDecisionWith
    , approveToolDecisionWithReporter
    , approveToolDecisionWithReporterAndPersistence
    , approveToolDecisionWithReporterAndPersistenceClassified
    , approveFilesystemRootAccess
    , childApprove
    , planApproval
    , resolveApprovalPrompt
    , setApprovalPolicy
    , toggleAlwaysApprove
    ) where

import Agent.CLI.Approval.Decision
    ( ApprovalAction(..)
    , ApprovalFacts(..)
    , ApprovalNotice(..)
    , ApprovalPlan(..)
    , planApproval
    , resolveApprovalPrompt
    )
import Agent.CLI.Options (ApprovalPolicy(..))
import Agent.CLI.Permission
    ( PermissionChoice
    , promptPermission
    )
import Agent.CLI.Project (saveProjectAutoApprove)
import Agent.CLI.Render (putTextLn)
import Agent.CLI.Style
    ( roleSuccess
    , roleWarn
    )
import Agent.CLI.Terminal (resolveColor)
import Agent.OsPath (toText)
import Agent.ToolDispatch
    ( ToolCall(..)
    , canonicalToolName
    , isComputerToolCallKind
    )
import Agent.Tools.PlanMode
    ( PlanModeEnv
    , isPlanModeActive
    , planFilePath
    )
import Agent.Tools.Types
    ( ToolRegistry
    , lookupRegisteredTool
    , toolAcceptsCall
    , toolAllowsWithoutPrompt
    )
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , modifyIORef'
    , readIORef
    )
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import System.IO (stderr)
import System.OsPath (OsPath)

-- | Auto-approve access to additional filesystem roots while yolo mode is
-- active. Read the live policy so toggling yolo during a session takes effect
-- for subsequent root requests.
approveFilesystemRootAccess :: IORef ApprovalPolicy -> IO Bool -> IO Bool
approveFilesystemRootAccess policyRef requestAccess =
    readIORef policyRef >>= \case
        ApproveAll -> pure True
        _ -> requestAccess

approveToolDecision
    :: IORef ApprovalPolicy
    -> IORef (Set Text)
    -> ToolRegistry
    -> PlanModeEnv
    -> OsPath
    -> OsPath
    -> ToolCall
    -> IO (Either Text Bool)
approveToolDecision
        policyRef allowedToolsRef tools planMode projectRoot cwd =
    approveToolDecisionClassified
        (const (pure Nothing))
        policyRef
        allowedToolsRef
        tools
        planMode
        projectRoot
        cwd

-- | Approval entry point for provider-native tools. Returning @Just True@ or
-- @Just False@ supplies the provider-specific read-only classification;
-- @Nothing@ falls back to the registered host tool.
approveToolDecisionClassified
    :: (ToolCall -> IO (Maybe Bool))
    -> IORef ApprovalPolicy
    -> IORef (Set Text)
    -> ToolRegistry
    -> PlanModeEnv
    -> OsPath
    -> OsPath
    -> ToolCall
    -> IO (Either Text Bool)
approveToolDecisionClassified classifyReadOnly
        policyRef allowedToolsRef tools planMode projectRoot cwd call = do
    approveToolDecisionWithReporterAndPersistenceClassified
        classifyReadOnly
        (\requested -> do
            color <- resolveColor stderr
            promptPermission color (toText cwd) requested)
        (\case
            ApprovalWarning message -> do
                color <- resolveColor stderr
                putTextLn stderr (roleWarn color message)
            ApprovalSuccess message -> do
                color <- resolveColor stderr
                putTextLn stderr (roleSuccess color message))
        (saveProjectAutoApprove projectRoot True)
        policyRef
        allowedToolsRef
        tools
        planMode
        call

approveToolDecisionWith
    :: (ToolCall -> IO (Maybe PermissionChoice))
    -> IORef ApprovalPolicy
    -> IORef (Set Text)
    -> ToolRegistry
    -> PlanModeEnv
    -> ToolCall
    -> IO (Either Text Bool)
approveToolDecisionWith requestPermission =
    approveToolDecisionWithReporterAndPersistence requestPermission (\case
        ApprovalWarning message -> do
            color <- resolveColor stderr
            putTextLn stderr (roleWarn color message)
        ApprovalSuccess message -> do
            color <- resolveColor stderr
            putTextLn stderr (roleSuccess color message))
        (pure ())

approveToolDecisionWithReporter
    :: (ToolCall -> IO (Maybe PermissionChoice))
    -> (ApprovalNotice -> IO ())
    -> IORef ApprovalPolicy
    -> IORef (Set Text)
    -> ToolRegistry
    -> PlanModeEnv
    -> ToolCall
    -> IO (Either Text Bool)
approveToolDecisionWithReporter requestPermission report =
    approveToolDecisionWithReporterAndPersistence requestPermission report (pure ())

approveToolDecisionWithReporterAndPersistence
    :: (ToolCall -> IO (Maybe PermissionChoice))
    -> (ApprovalNotice -> IO ())
    -> IO ()
    -> IORef ApprovalPolicy
    -> IORef (Set Text)
    -> ToolRegistry
    -> PlanModeEnv
    -> ToolCall
    -> IO (Either Text Bool)
approveToolDecisionWithReporterAndPersistence requestPermission report persistAlwaysApprove policyRef allowedToolsRef tools planMode call = do
    approveToolDecisionWithReporterAndPersistenceClassified
        (const (pure Nothing))
        requestPermission
        report
        persistAlwaysApprove
        policyRef
        allowedToolsRef
        tools
        planMode
        call

approveToolDecisionWithReporterAndPersistenceClassified
    :: (ToolCall -> IO (Maybe Bool))
    -> (ToolCall -> IO (Maybe PermissionChoice))
    -> (ApprovalNotice -> IO ())
    -> IO ()
    -> IORef ApprovalPolicy
    -> IORef (Set Text)
    -> ToolRegistry
    -> PlanModeEnv
    -> ToolCall
    -> IO (Either Text Bool)
approveToolDecisionWithReporterAndPersistenceClassified
        classifyReadOnly requestPermission report persistAlwaysApprove
        policyRef allowedToolsRef tools planMode call =
    case lookupRegisteredTool call.name tools of
        Just tool
            | not (toolAcceptsCall tool call) -> do
                let message =
                        "Rejected: mismatched provider-native tool call kind."
                report (ApprovalWarning message)
                pure (Left message)
        _ -> do
            policy <- readIORef policyRef
            planActive <- isPlanModeActive planMode
            planPath <- planFilePath planMode
            let initialFacts = ApprovalFacts
                    { policy
                    , planActive
                    , planPath
                    , readOnly = Nothing
                    , allowedForSession = Nothing
                    , call
                    }
            interpret initialFacts (planApproval initialFacts)
  where
    interpret facts = \case
        CompleteApproval result actions -> do
            mapM_ runAction actions
            pure result
        NeedReadOnlyClassification -> do
            readOnly <- classifyReadOnly call >>= \case
                Just value -> pure value
                Nothing -> case lookupRegisteredTool call.name tools of
                    Nothing -> pure False
                    Just tool -> toolAllowsWithoutPrompt tool call
            let nextFacts = facts { readOnly = Just readOnly }
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
            choice <- requestPermission call
            interpret facts (resolveApprovalPrompt call choice)

    runAction = \case
        SetApprovalPolicy next ->
            atomicModifyIORef' policyRef (const (next, ()))
        PersistProjectAutoApprove ->
            persistAlwaysApprove
        RememberToolForSession toolName ->
            modifyIORef' allowedToolsRef (Set.insert toolName)
        ReportApprovalNotice notice ->
            report notice

toggleAlwaysApprove :: IORef ApprovalPolicy -> OsPath -> IO Text
toggleAlwaysApprove policyRef projectRoot = do
    next <- atomicModifyIORef' policyRef \policy ->
        if policy == ApproveAll
            then (PromptMutating, PromptMutating)
            else (ApproveAll, ApproveAll)
    saveProjectAutoApprove projectRoot (next == ApproveAll)
    pure (case next of
        ApproveAll -> "auto-approve on (saved for project)"
        _ -> "auto-approve off (saved for project)")

-- | Set the interactive approval policy and persist the compatible
-- project-wide auto-approve flag. The on-disk format predates the
-- read-only policy, so only 'ApproveAll' is persisted as enabled.
setApprovalPolicy :: IORef ApprovalPolicy -> OsPath -> ApprovalPolicy -> IO Text
setApprovalPolicy policyRef projectRoot policy = do
    atomicModifyIORef' policyRef (const (policy, ()))
    saveProjectAutoApprove projectRoot (policy == ApproveAll)
    pure $ case policy of
        ApproveAll -> "full access enabled (saved for project)"
        PromptMutating -> "ask before changes (saved for project)"
        DenyMutating -> "read-only enabled for this session"

childApprove :: ApprovalPolicy -> ToolRegistry -> ToolCall -> IO (Either Text Bool)
childApprove _ tools call
    | Just tool <- lookupRegisteredTool call.name tools
    , not (toolAcceptsCall tool call) =
        pure $ Left
            "Mismatched provider-native tool call kind requires parent review."
childApprove _ _ call
    | isComputerToolCallKind call.callKind =
        pure $ Left
            "Computer use must be approved in the interactive parent session."
childApprove policy tools call = case policy of
    ApproveAll -> pure (Right True)
    DenyMutating -> do
        allowed <- isReadOnlyCall tools call
        pure $ if allowed then Right True else Right False
    PromptMutating -> do
        allowed <- isReadOnlyCall tools call
        if allowed
            then pure (Right True)
            else pure $ Left
                "Subagent cannot prompt for approval on mutating tools. \
                \Re-run the parent with auto-approve/--yolo, or have the \
                \parent perform this edit."

isReadOnlyCall :: ToolRegistry -> ToolCall -> IO Bool
isReadOnlyCall tools call = case lookupRegisteredTool call.name tools of
    Just tool -> toolAllowsWithoutPrompt tool call
    Nothing -> pure False
