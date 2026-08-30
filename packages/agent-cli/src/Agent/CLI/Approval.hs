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
import Agent.JsonText (jsonTextFieldDefault)
import Agent.OsPath (fromText)
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallKind(..)
    , canonicalToolName
    )
import Agent.Tools.Dangerous (shellCommandBlocked)
import Agent.Tools.PlanMode
    ( PlanModeEnv
    , isPlanFileEditTarget
    , isPlanModeActive
    , planFilePath
    , planModeBlockedEditMessage
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
import qualified Data.Text as Text
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
    -> ToolCall
    -> IO (Either Text Bool)
approveToolDecision policyRef allowedToolsRef tools planMode projectRoot call = do
    approveToolDecisionWithReporterAndPersistence
        (\requested -> do
            color <- resolveColor stderr
            promptPermission color requested)
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
    planActive <- isPlanModeActive planMode
    planPath <- planFilePath planMode
    let toolName = canonicalToolName call.name
    -- A provider-native call kind may only reach its dedicated hosted schema.
    -- Reject spoofed function/custom calls before yolo or remembered approval.
    case lookupRegisteredTool call.name tools of
        Just tool
            | not (toolAcceptsCall tool call) -> do
                let msg =
                        "Rejected mismatched provider-native tool call kind for "
                            <> call.name <> "."
                report (ApprovalWarning msg)
                pure (Left msg)
        _ -> approveKnownKind policy planActive planPath toolName
  where
    approveKnownKind policy planActive planPath toolName = do
      -- Hard deny for catastrophic shell deletes, even under ApproveAll / yolo.
      case shellCommandBlocked toolName call.arguments of
        Just msg -> do
            report (ApprovalWarning (glyphWarn <> msg))
            pure (Left msg)
        Nothing -> do
            readOnly <- case lookupRegisteredTool call.name tools of
                Nothing -> pure False
                Just tool -> toolAllowsWithoutPrompt tool call
            -- Plan mode hard-denies writes even under yolo. The dedicated
            -- write_plan tool and Grok's path-locked plan.md edit are the only
            -- mutations allowed. Shell tools are blocked entirely because an
            -- arbitrary shell script cannot be proven read-only.
            if planModeBlocksCall planActive planPath readOnly call
                then do
                    let msg = planModeBlockedEditMessage planPath
                    report (ApprovalWarning msg)
                    pure (Left msg)
                else do
                    -- plan.md edits are auto-approved while plan mode is active.
                    if isPlanFileWrite planActive planPath call
                        then pure (Right True)
                        else do
                            if call.callKind == ComputerCallKind
                                then requestPermission call >>= \case
                                    Nothing -> pure (Right False)
                                    Just PermissionDeny -> pure (Right False)
                                    -- Computer access is deliberately never
                                    -- cached and bypasses yolo/ApproveAll.
                                    -- Every provider call must reach the
                                    -- approval UI, including safety checks.
                                    Just _ -> pure (Right True)
                                else do
                                    allowed <- readIORef allowedToolsRef
                                    if Set.member toolName allowed
                                        then pure (Right True)
                                        else case policy of
                                            ApproveAll -> pure (Right True)
                                            DenyMutating ->
                                                pure (Right readOnly)
                                            PromptMutating
                                                | readOnly ->
                                                    pure (Right True)
                                                | otherwise -> do
                                                    requestPermission call
                                                        >>= \case
                                                            Nothing ->
                                                                pure (Right False)
                                                            Just PermissionAllowOnce ->
                                                                pure (Right True)
                                                            Just PermissionAllowAll -> do
                                                                atomicModifyIORef'
                                                                    policyRef $
                                                                        const
                                                                            ( ApproveAll
                                                                            , ()
                                                                            )
                                                                persistAlwaysApprove
                                                                report $
                                                                    ApprovalSuccess
                                                                        (glyphOk
                                                                            <> "auto-approve on \
                                                                               \(saved for project)")
                                                                pure (Right True)
                                                            Just PermissionAllowTool -> do
                                                                modifyIORef'
                                                                    allowedToolsRef
                                                                    (Set.insert
                                                                        toolName)
                                                                report $
                                                                    ApprovalSuccess
                                                                        (glyphOk
                                                                            <> "always allow "
                                                                            <> call.name
                                                                            <> " this session")
                                                                pure (Right True)
                                                            Just PermissionDeny ->
                                                                pure (Right False)

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
    | name `elem` ["shell_command", "run_terminal_cmd"] =
        True
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
childApprove _ tools call
    | Just tool <- lookupRegisteredTool call.name tools
    , not (toolAcceptsCall tool call) =
        pure $ Left
            "Mismatched provider-native tool call kind requires parent review."
childApprove _ _ call
    | call.callKind == ComputerCallKind =
        pure $ Left
            "Computer use requires an explicit parent approval for every call."
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
