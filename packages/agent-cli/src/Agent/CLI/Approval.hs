-- | Parent and child tool-approval policy for interactive CLI sessions.
module Agent.CLI.Approval
    ( approveToolDecision
    , approveToolDecisionWithPromptNotice
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
import Agent.CLI.Tools (lookupAppTool)
import Agent.JsonText (jsonTextFieldDefault)
import Agent.ToolDispatch (ToolCall(..))
import Agent.Tools.Dangerous (shellCommandBlocked)
import Agent.Tools.PlanMode
    ( PlanModeEnv
    , isPlanFileEditTarget
    , isPlanModeActive
    , planFilePath
    , planModeBlockedEditMessage
    )
import Agent.Tools.Types (AppTool, toolAllowsWithoutPrompt)
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
    -> [AppTool]
    -> PlanModeEnv
    -> ToolCall
    -> IO (Either Text Bool)
approveToolDecision policyRef allowedToolsRef tools planMode call = do
    approveToolDecisionWithPromptNotice
        (pure ()) policyRef allowedToolsRef tools planMode call

approveToolDecisionWithPromptNotice
    :: IO ()
    -> IORef ApprovalPolicy
    -> IORef (Set Text)
    -> [AppTool]
    -> PlanModeEnv
    -> ToolCall
    -> IO (Either Text Bool)
approveToolDecisionWithPromptNotice onPrompt
        policyRef allowedToolsRef tools planMode call = do
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
                    readOnly <- case lookupAppTool call.name tools of
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
                                            onPrompt
                                            color <- resolveColor stderr
                                            promptPermission color call >>= \case
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

planModeBlocksCall :: Bool -> FilePath -> ToolCall -> Bool
planModeBlocksCall active planPath call
    | not active = False
    | call.name == "apply_patch" = True
    | call.name == "search_replace" =
        let target = jsonTextFieldDefault "file_path" call.arguments
        in Text.null target
            || not (isPlanFileEditTarget planPath (Text.unpack target))
    | otherwise = False

isPlanFileWrite :: Bool -> FilePath -> ToolCall -> Bool
isPlanFileWrite active planPath call
    | not active = False
    | call.name == "search_replace" =
        let target = jsonTextFieldDefault "file_path" call.arguments
        in not (Text.null target)
            && isPlanFileEditTarget planPath (Text.unpack target)
    | otherwise = False

toggleAlwaysApprove :: IORef ApprovalPolicy -> FilePath -> IO ()
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

childApprove :: ApprovalPolicy -> [AppTool] -> ToolCall -> IO (Either Text Bool)
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

isReadOnlyCall :: [AppTool] -> ToolCall -> IO Bool
isReadOnlyCall tools call = case lookupAppTool call.name tools of
    Just tool -> toolAllowsWithoutPrompt tool call
    Nothing -> pure False
