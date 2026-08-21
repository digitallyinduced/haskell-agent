-- | Parent and child tool-approval policy for interactive CLI sessions.
module Agent.CLI.Approval
    ( approveToolDecision
    , approveToolDecisionWith
    , childApprove
    , toggleAlwaysApprove
    ) where

import Agent.CLI.Options (ApprovalPolicy(..))
import Agent.CLI.Permission
    ( PermissionChoice(..)
    , promptPermission
    )
import Agent.CLI.Project (saveProjectAutoApprove)
import Agent.CLI.Render (putTextLn)
import Agent.CLI.Style
    ( glyphOk
    , glyphSession
    , glyphWarn
    , roleMuted
    , roleSuccess
    , roleWarn
    )
import Agent.CLI.Terminal (resolveColor)
import Agent.JsonText (jsonTextFieldDefault)
import Agent.OsPath (OsPath, fromText)
import Agent.ToolDispatch (ToolCall(..))
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

approveToolDecision
    :: IORef ApprovalPolicy
    -> IORef (Set Text)
    -> ToolRegistry
    -> PlanModeEnv
    -> ToolCall
    -> IO (Either Text Bool)
approveToolDecision policyRef allowedToolsRef tools planMode call = do
    approveToolDecisionWith
        (\requested -> do
            color <- resolveColor stderr
            promptPermission color requested)
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
approveToolDecisionWith requestPermission policyRef allowedToolsRef tools planMode call = do
    policy <- readIORef policyRef
    planActive <- isPlanModeActive planMode
    planPath <- planFilePath planMode
    -- Hard deny for catastrophic shell deletes, even under ApproveAll / yolo.
    case shellCommandBlocked call.name call.arguments of
        Just msg -> do
            color <- resolveColor stderr
            putTextLn stderr (roleWarn color (glyphWarn <> msg))
            pure (Left msg)
        Nothing -> do
            -- Plan mode: reject mutating file edits except plan.md (even under yolo).
            -- Grok search_replace also enforces this in-tool; this covers apply_patch
            -- and any other write tool before dispatch.
            if planModeBlocksCall planActive planPath call
                then do
                    let msg = planModeBlockedEditMessage planPath
                    color <- resolveColor stderr
                    putTextLn stderr (roleWarn color msg)
                    pure (Left msg)
                else do
                    readOnly <- case lookupRegisteredTool call.name tools of
                        Nothing -> pure False
                        Just tool -> toolAllowsWithoutPrompt tool call
                    -- plan.md edits are auto-approved while plan mode is active.
                    if isPlanFileWrite planActive planPath call
                        then pure (Right True)
                        else do
                            allowed <- readIORef allowedToolsRef
                            if Set.member call.name allowed
                                then pure (Right True)
                                else case policy of
                                    ApproveAll -> pure (Right True)
                                    DenyMutating -> pure (Right readOnly)
                                    PromptMutating
                                        | readOnly -> pure (Right True)
                                        | otherwise -> do
                                            color <- resolveColor stderr
                                            requestPermission call >>= \case
                                                Nothing -> pure (Right False)
                                                Just PermissionAllowOnce ->
                                                    pure (Right True)
                                                Just PermissionAllowTool -> do
                                                    modifyIORef' allowedToolsRef
                                                        (Set.insert call.name)
                                                    putTextLn stderr
                                                        (roleSuccess color
                                                            (glyphOk
                                                                <> "always allow "
                                                                <> call.name
                                                                <> " this session"))
                                                    pure (Right True)
                                                Just PermissionDeny ->
                                                    pure (Right False)

planModeBlocksCall :: Bool -> OsPath -> ToolCall -> Bool
planModeBlocksCall active planPath call
    | not active = False
    | call.name == "apply_patch" = True
    | call.name == "search_replace" =
        let target = jsonTextFieldDefault "file_path" call.arguments
        in Text.null target
            || not (isPlanFileEditTarget planPath (fromText target))
    | otherwise = False

isPlanFileWrite :: Bool -> OsPath -> ToolCall -> Bool
isPlanFileWrite active planPath call
    | not active = False
    | call.name == "search_replace" =
        let target = jsonTextFieldDefault "file_path" call.arguments
        in not (Text.null target)
            && isPlanFileEditTarget planPath (fromText target)
    | otherwise = False

toggleAlwaysApprove :: IORef ApprovalPolicy -> OsPath -> IO ()
toggleAlwaysApprove policyRef projectRoot = do
    color <- resolveColor stderr
    next <- atomicModifyIORef' policyRef \policy ->
        if policy == ApproveAll
            then (PromptMutating, PromptMutating)
            else (ApproveAll, ApproveAll)
    saveProjectAutoApprove projectRoot (next == ApproveAll)
    putTextLn stderr (case next of
        ApproveAll -> roleSuccess color (glyphOk <> "auto-approve on (saved for project)")
        _ -> roleMuted color (glyphSession <> "auto-approve off (saved for project)"))

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
