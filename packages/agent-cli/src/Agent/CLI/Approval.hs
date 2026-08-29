-- | Parent and child tool-approval policy for interactive CLI sessions.
module Agent.CLI.Approval
    ( ApprovalAction(..)
    , ApprovalFacts(..)
    , ApprovalNotice(..)
    , ApprovalPlan(..)
    , approveToolDecision
    , approveToolDecisionWith
    , approveToolDecisionWithReporter
    , approveToolDecisionWithReporterAndPersistence
    , approveFilesystemRootAccess
    , childApprove
    , planApproval
    , resolveApprovalPrompt
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
    )
import Agent.Tools.PlanMode
    ( PlanModeEnv
    , isPlanModeActive
    , planFilePath
    )
import Agent.Tools.Types
    ( ToolRegistry
    , lookupRegisteredTool
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
        policyRef allowedToolsRef tools planMode projectRoot cwd call = do
    approveToolDecisionWithReporterAndPersistence
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
approveToolDecisionWithReporterAndPersistence
        requestPermission report persistAlwaysApprove
        policyRef allowedToolsRef tools planMode call = do
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
            readOnly <- case lookupRegisteredTool call.name tools of
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

childApprove :: ApprovalPolicy -> ToolRegistry -> ToolCall -> IO (Either Text Bool)
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
