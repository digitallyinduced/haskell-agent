-- | Parent and child tool-approval policy for interactive CLI sessions.
module Agent.CLI.Approval
    ( ApprovalNotice(..)
    , approveToolDecision
    , approveToolDecisionWith
    , approveToolDecisionWithReporter
    , approveToolDecisionWithReporterAndPersistence
    , childApprove
    , toggleAlwaysApprove
    ) where

import Agent.CLI.Options (ApprovalPolicy(..))
import Agent.CLI.Permission
    ( PermissionChoice(..)
    , promptPermission
    )
import System.OsPath (OsPath)
import Agent.CLI.Project (saveProjectAutoApprove)
import Agent.CLI.Render (putTextLn)
import Agent.CLI.Style
    ( glyphOk
    , glyphWarn
    , roleSuccess
    , roleWarn
    )
import Agent.CLI.Terminal (resolveColor)
import Agent.OsPath (toText)
import Agent.ToolDispatch
    ( ToolCall(..)
    , canonicalToolName
    )
import Agent.Tools.Dangerous (shellCommandBlocked)
import Agent.Tools.PlanMode
    ( PlanModeEnv
    , PlanModeState(..)
    , isPlanFileEditTarget
    , planFilePath
    , planModeBlockedEditMessage
    , readPlanModeState
    )
import Agent.Tools.Types
    ( AppTool(..)
    , PlanModeCapability(..)
    , ToolRegistry
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

data ApprovalNotice
    = ApprovalWarning !Text
    | ApprovalSuccess !Text
    deriving (Eq, Show)

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
approveToolDecisionWithReporterAndPersistence requestPermission report persistAlwaysApprove policyRef allowedToolsRef tools planMode call = do
    policy <- readIORef policyRef
    planState <- readPlanModeState planMode
    let planRestricted =
            planState `elem` [PlanActive, PlanExitPending]
        planEditable = planState == PlanActive
    planPath <- planFilePath planMode
    let toolName = canonicalToolName call.name
    -- Hard deny for catastrophic shell deletes, even under ApproveAll / yolo.
    case shellCommandBlocked toolName call.arguments of
        Just msg -> do
            report (ApprovalWarning (glyphWarn <> msg))
            pure (Left msg)
        Nothing -> do
            let registered = lookupRegisteredTool call.name tools
            readOnly <- case registered of
                Nothing -> pure False
                Just tool -> toolAllowsWithoutPrompt tool call
            -- Plan-mode authority is independent from generic approval. In
            -- particular, ApproveAll/yolo cannot make an unknown or mutating
            -- tool plan-safe. Plan-file writers must prove their normalized
            -- target is the exact canonical session plan.
            blocked <-
                planModeBlockReason
                    planRestricted
                    planEditable
                    readOnly
                    planPath
                    registered
                    call
            case blocked of
                Just msg -> do
                    report (ApprovalWarning msg)
                    pure (Left msg)
                Nothing -> do
                    fileWrite <-
                        isAuthorizedPlanFileWrite
                            planEditable
                            planPath
                            registered
                            call
                    -- The only mutation admitted by plan-mode capability is
                    -- the exact plan-file write; it is auto-approved.
                    if fileWrite
                        then pure (Right True)
                        else do
                            allowed <- readIORef allowedToolsRef
                            if Set.member toolName allowed
                                then pure (Right True)
                                else case policy of
                                    ApproveAll -> pure (Right True)
                                    DenyMutating -> pure (Right readOnly)
                                    PromptMutating
                                        | readOnly -> pure (Right True)
                                        | otherwise -> do
                                            requestPermission call >>= \case
                                                Nothing -> pure (Right False)
                                                Just PermissionAllowOnce ->
                                                    pure (Right True)
                                                Just PermissionAllowAll -> do
                                                    atomicModifyIORef' policyRef $
                                                        const (ApproveAll, ())
                                                    persistAlwaysApprove
                                                    report $
                                                        ApprovalSuccess
                                                            (glyphOk
                                                                <> "auto-approve on \
                                                                   \(saved for project)")
                                                    pure (Right True)
                                                Just PermissionAllowTool -> do
                                                    modifyIORef' allowedToolsRef
                                                        (Set.insert toolName)
                                                    report $
                                                        ApprovalSuccess
                                                            (glyphOk
                                                                <> "always allow "
                                                                <> call.name
                                                                <> " this session")
                                                    pure (Right True)
                                                Just PermissionDeny ->
                                                    pure (Right False)

planModeBlockReason
    :: Bool
    -> Bool
    -> Bool
    -> OsPath
    -> Maybe AppTool
    -> ToolCall
    -> IO (Maybe Text)
planModeBlockReason restricted editable readOnly planPath registered call
    | not restricted = pure Nothing
    | otherwise = case (.appToolPlanModeCapability) <$> registered of
        Nothing -> pure (Just unknownMessage)
        Just PlanModeUnknown -> pure (Just unknownMessage)
        Just PlanModeBlocked -> pure (Just blockedMessage)
        Just PlanModeReadOnly -> pure Nothing
        Just PlanModeInteraction -> pure Nothing
        Just PlanModeSafeSubagent
            | readOnly -> pure Nothing
            | otherwise -> pure (Just blockedMessage)
        Just PlanModeScopedState -> pure Nothing
        Just (PlanModePlanFileWrite resolveTarget) ->
            if not editable
                then
                    pure
                        (Just
                            "Rejected: the submitted plan snapshot is frozen while its review is pending.")
                else
                    resolveTarget call >>= \case
                        Left err ->
                            pure . Just $
                                planModeBlockedEditMessage planPath
                                    <> " The plan-file target could not be validated: "
                                    <> err
                        Right target
                            | isPlanFileEditTarget planPath target -> pure Nothing
                            | otherwise -> pure (Just blockedMessage)
  where
    unknownMessage = blockedMessage
    blockedMessage = planModeBlockedEditMessage planPath

isAuthorizedPlanFileWrite
    :: Bool
    -> OsPath
    -> Maybe AppTool
    -> ToolCall
    -> IO Bool
isAuthorizedPlanFileWrite active planPath registered call
    | not active = pure False
    | otherwise = case (.appToolPlanModeCapability) <$> registered of
        Just (PlanModePlanFileWrite resolveTarget) ->
            resolveTarget call >>= \case
                Right target ->
                    pure (isPlanFileEditTarget planPath target)
                Left _ -> pure False
        _ -> pure False

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
