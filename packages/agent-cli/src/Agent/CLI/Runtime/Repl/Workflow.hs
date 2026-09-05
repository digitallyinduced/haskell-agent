-- | Grok goal and workflow slash commands.
module Agent.CLI.Runtime.Repl.Workflow
    ( handleWorkflowAction
    ) where

import Agent.CLI.Command
    ( WorkflowAction(ReplGoalStatus, ReplGoalPause, ReplGoalResume, ReplGoalClear,
                 ReplGoalSet, ReplWorkflowRuns, ReplWorkflowManage) )
import Agent.CLI.Runtime.Types ( RunResult )
import Agent.CLI.SessionEnv ( SessionEnv(..) )
import Agent.CLI.Style ( roleError, roleMuted )
import Agent.CLI.TUI.App ( emitUiEvent )
import Agent.CLI.Terminal ( resolveColor )
import Agent.GrokBuild.Dialect.Goal
    ( activateGoal, clearGoal, formatGoalSnapshot, pauseGoal, readGoal, resumeGoal )
import Agent.GrokBuild.Dialect.Runtime ( GrokRuntimeControl(..) )
import Agent.GrokBuild.Dialect.Workflow
    ( formatWorkflowRuns, workflowRunSnapshots )
import Agent.TUI.Model ( UiEvent(UiErrorMessage, UiSystemMessage) )
import Data.Text ( Text )
import System.IO ( stderr, stdout )
import qualified Data.Text.IO as Text ( hPutStrLn, putStrLn )

handleWorkflowAction
    :: SessionEnv
    -> (IO RunResult -> Bool -> Text -> Text -> IO RunResult)
    -> Bool
    -> IO RunResult
    -> WorkflowAction
    -> IO RunResult
handleWorkflowAction
        SessionEnv
            { sessionGrokRuntime = grokRuntime
            , sessionFullscreen = fullscreen
            }
        submitExpandedTurn
        color
        continue = \case
    ReplGoalStatus -> do
        color <- resolveColor stdout
        case grokRuntime of
            Nothing ->
                displayError
                    "goal commands are unavailable in this session" $
                    Text.hPutStrLn stderr
                        (roleError color
                            "goal commands are unavailable in this session")
            Just control ->
                readGoal control.grokGoalRuntime >>= \case
                    Nothing ->
                        displayInfo "No goal is active." $
                            Text.putStrLn
                                (roleMuted color
                                    "No goal is active.")
                    Just goal -> do
                        let message =
                                formatGoalSnapshot goal
                        displayInfo message $
                            Text.putStrLn
                                (roleMuted color message)
        continue
    ReplGoalPause -> do
        color <- resolveColor stderr
        case grokRuntime of
            Nothing ->
                displayError
                    "goal commands are unavailable in this session" $
                    Text.hPutStrLn stderr
                        (roleError color
                            "goal commands are unavailable in this session")
            Just control ->
                pauseGoal control.grokGoalRuntime >>= \case
                    Left err ->
                        displayError err $
                            Text.hPutStrLn stderr
                                (roleError color err)
                    Right goal -> do
                        let message =
                                "Goal paused.\n"
                                    <> formatGoalSnapshot goal
                        displayInfo message $
                            Text.hPutStrLn stderr
                                (roleMuted color message)
        continue
    ReplGoalResume -> do
        color <- resolveColor stderr
        case grokRuntime of
            Nothing ->
                displayError
                    "goal commands are unavailable in this session" $
                    Text.hPutStrLn stderr
                        (roleError color
                            "goal commands are unavailable in this session")
            Just control ->
                resumeGoal control.grokGoalRuntime >>= \case
                    Left err ->
                        displayError err $
                            Text.hPutStrLn stderr
                                (roleError color err)
                    Right goal -> do
                        let message =
                                "Goal resumed.\n"
                                    <> formatGoalSnapshot goal
                        displayInfo message $
                            Text.hPutStrLn stderr
                                (roleMuted color message)
        continue
    ReplGoalClear -> do
        color <- resolveColor stderr
        case grokRuntime of
            Nothing ->
                displayError
                    "goal commands are unavailable in this session" $
                    Text.hPutStrLn stderr
                        (roleError color
                            "goal commands are unavailable in this session")
            Just control -> do
                cleared <-
                    clearGoal control.grokGoalRuntime
                let message =
                        if cleared
                            then "Goal cleared."
                            else "No goal was active."
                displayInfo message $
                    Text.hPutStrLn stderr
                        (roleMuted color message)
        continue
    ReplGoalSet original objective budget expanded ->
        case grokRuntime of
            Nothing -> do
                color <- resolveColor stderr
                let err =
                        "goal commands are unavailable in this session"
                displayError err $
                    Text.hPutStrLn stderr
                        (roleError color err)
                continue
            Just control ->
                activateGoal
                    control.grokGoalRuntime
                    objective
                    budget >>= \case
                        Left err -> do
                            color <- resolveColor stderr
                            displayError err $
                                Text.hPutStrLn stderr
                                    (roleError color err)
                            continue
                        Right _ ->
                            submitExpandedTurn
                                continue
                                color
                                original
                                expanded
    ReplWorkflowRuns -> do
        color <- resolveColor stdout
        case grokRuntime >>= (.grokWorkflowRuntime) of
            Nothing ->
                displayError
                    "workflow commands are unavailable in this session" $
                    Text.hPutStrLn stderr
                        (roleError color
                            "workflow commands are unavailable in this session")
            Just runtime -> do
                runs <- workflowRunSnapshots runtime
                let message = formatWorkflowRuns runs
                displayInfo message $
                    Text.putStrLn
                        (roleMuted color message)
        continue
    ReplWorkflowManage operation target -> do
        color <- resolveColor stderr
        let err =
                "workflow_management_unsupported: /workflow "
                    <> operation
                    <> maybe "" (" " <>) target
                    <> " is not supported by this host; use /workflow runs to inspect tracked runs."
        displayError err $
            Text.hPutStrLn stderr
                (roleError color err)
        continue

  where
    displayInfo message minimalAction = case fullscreen of
        Nothing -> minimalAction
        Just runtime -> emitUiEvent runtime (UiSystemMessage message)
    displayError message minimalAction = case fullscreen of
        Nothing -> minimalAction
        Just runtime -> emitUiEvent runtime (UiErrorMessage message)
