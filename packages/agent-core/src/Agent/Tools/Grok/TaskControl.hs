module Agent.Tools.Grok.TaskControl
    ( getTaskOutputTool
    , killTaskTool
    ) where

import Agent.Subagents
    ( SubagentId(..)
    , SubagentStatus(..)
    , closeSubagent
    , getStatus
    , waitSubagents
    )
import Agent.Subagents.Format (isFinalStatus)
import Agent.ToolArgs (objectArgs, optInt, reqText, reqTextList)
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.ToolDispatch (typedTool)
import Agent.Tools.Grok.Common (jsonTool, stripAnsi)
import Agent.Tools.Grok.Shell
    ( GrokSession
    , killTask
    , readTaskOutput
    )
import Agent.Tools.Grok.Task (isSubagentIdText)
import Agent.Tools.MultiAgents (MultiAgentContext(..))
import Agent.Tools.Types
    ( AppTool
    , ToolExecutionPolicy(..)
    )
import Control.Applicative ((<|>))
import Control.Concurrent.Async (mapConcurrently)
import Data.Aeson (FromJSON(..))
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

data TaskOutputArgs = TaskOutputArgs
    { taskIds :: [Text]
    , timeoutMs :: Maybe Int
    }

instance FromJSON TaskOutputArgs where
    parseJSON = objectArgs \object -> do
        taskIds <-
            reqTextList object "task_ids"
                <|> reqTextList object "task_id"
        canonicalTimeout <- optInt object "timeout_ms"
        legacyTimeout <- optInt object "timeout"
        pure TaskOutputArgs
            { taskIds
            , timeoutMs = canonicalTimeout <|> legacyTimeout
            }

getTaskOutputTool :: GrokSession -> Maybe MultiAgentContext -> AppTool
getTaskOutputTool session multi = jsonTool "get_task_output" getTaskOutputDescription
    [ PropertySchema "task_ids" (PropertyArray PropertyString) True $ Just
        "Task IDs to get output from. For a single task use a one-element array."
    , PropertySchema "timeout_ms" PropertyInteger False $ Just
        "Max wait time in milliseconds, up to 600000. A positive value waits for completion; omit or pass 0 for a non-blocking status snapshot."
    ]
    True
    TurnSequential
    (typedTool "get_task_output" (runGetTaskOutput session multi))

getTaskOutputDescription :: Text
getTaskOutputDescription =
    "Get output and status from a background task or subagent.\n\n\
    \Usage notes:\n\
    \- Pass task_ids with one or more ids from background=true commands or run_in_background=true subagents; for a single task use a one-element array. Multiple ids with a positive timeout_ms wait until all complete\n\
    \- Omit timeout_ms or pass 0 for a non-blocking status snapshot; set a positive timeout_ms to wait up to that many milliseconds, capped at 600000 (~10 min)\n\
    \- Returns current output and status."

runGetTaskOutput
    :: GrokSession
    -> Maybe MultiAgentContext
    -> TaskOutputArgs
    -> IO (Either Text Text)
runGetTaskOutput session multi args
    | null resolvedIds =
        pure (Left "Provide a non-empty task_ids list.")
    | length resolvedIds > maxTaskOutputIds =
        pure $ Left $
            "task_ids exceeds maximum of "
                <> Text.pack (show maxTaskOutputIds)
                <> " entries."
    | otherwise = do
        entries <- mapConcurrently
            (runOneTaskOutput session multi effectiveTimeout)
            resolvedIds
        pure $ Right $ case entries of
            [entry] -> entry.output
            _ -> formatMultiTaskOutput waits entries
  where
    resolvedIds =
        nub
            [ stripped
            | taskId <- args.taskIds
            , let stripped = Text.strip taskId
            , not (Text.null stripped)
            ]
    waits = maybe False (> 0) args.timeoutMs
    effectiveTimeout
        | waits = Just (min maxTaskOutputWaitMs (fromMaybe 0 args.timeoutMs))
        | otherwise = Nothing

maxTaskOutputIds :: Int
maxTaskOutputIds = 20

maxTaskOutputWaitMs :: Int
maxTaskOutputWaitMs = 600000

data TaskOutputEntry = TaskOutputEntry
    { taskOutputId :: !Text
    , output :: !Text
    , terminal :: !Bool
    }

runOneTaskOutput
    :: GrokSession
    -> Maybe MultiAgentContext
    -> Maybe Int
    -> Text
    -> IO TaskOutputEntry
runOneTaskOutput session multi timeout taskId = case multi of
    Just ctx | isSubagentIdText taskId -> do
        let agentId = SubagentId taskId
        (status, timedOut) <- case timeout of
            Nothing -> do
                current <- getStatus ctx.multiRegistry agentId
                pure (current, False)
            Just timeoutMs -> do
                (statuses, didTimeOut) <-
                    waitSubagents ctx.multiRegistry [agentId] timeoutMs
                pure (fromMaybe NotFound (Map.lookup agentId statuses), didTimeOut)
        pure TaskOutputEntry
            { taskOutputId = taskId
            , output = formatAgentWait taskId timedOut (Just status)
            , terminal = isFinalStatus status
            }
    _ -> do
        text <- stripAnsi <$> readTaskOutput session taskId timeout
        pure TaskOutputEntry
            { taskOutputId = taskId
            , output = text
            , terminal = terminalCommandOutput text
            }

terminalCommandOutput :: Text -> Bool
terminalCommandOutput text =
    "exit:" `Text.isPrefixOf` text
        || "killed " `Text.isPrefixOf` text

formatMultiTaskOutput :: Bool -> [TaskOutputEntry] -> Text
formatMultiTaskOutput waits entries =
    Text.intercalate "\n\n"
        [ "task_id: " <> entry.taskOutputId <> "\n" <> entry.output
        | entry <- entries
        ]
        <> "\n\n"
        <> Text.pack (show (length (filter (.terminal) entries)))
        <> "/"
        <> Text.pack (show (length entries))
        <> " tasks completed ("
        <> (if waits then "wait_all" else "poll")
        <> ")"

formatAgentWait :: Text -> Bool -> Maybe SubagentStatus -> Text
formatAgentWait taskId timedOut mstatus = case mstatus of
    Just (Completed (Just text)) ->
        "subagent_id: " <> taskId <> "\nstatus: completed\nfinal:\n" <> text
    Just (Completed Nothing) ->
        "subagent_id: " <> taskId <> "\nstatus: completed"
    Just (Errored err) ->
        "subagent_id: " <> taskId <> "\nstatus: errored\nerror: " <> err
    Just Interrupted ->
        "subagent_id: " <> taskId <> "\nstatus: interrupted"
    Just Closed ->
        "subagent_id: " <> taskId <> "\nstatus: shutdown"
    Just Running
        | timedOut -> "subagent_id: " <> taskId <> "\nstatus: running (wait timed out)"
        | otherwise -> "subagent_id: " <> taskId <> "\nstatus: running"
    Just Pending ->
        "subagent_id: " <> taskId <> "\nstatus: pending"
    Just NotFound ->
        "Unknown task_id: " <> taskId
    Nothing ->
        "Unknown task_id: " <> taskId

newtype KillTaskArgs = KillTaskArgs { taskId :: Text }

instance FromJSON KillTaskArgs where
    parseJSON = objectArgs \object -> KillTaskArgs <$> reqText object "task_id"

killTaskTool :: GrokSession -> Maybe MultiAgentContext -> AppTool
killTaskTool session multi = jsonTool "kill_task" killTaskDescription
    [ PropertySchema "task_id" PropertyString True $ Just
        "The task id from a background run_terminal_cmd or the subagent_id from task."
    ]
    False
    TurnSequential
    (typedTool "kill_task" (runKillTask session multi))

killTaskDescription :: Text
killTaskDescription =
    "Kill a background command or close a subagent."

runKillTask
    :: GrokSession
    -> Maybe MultiAgentContext
    -> KillTaskArgs
    -> IO (Either Text Text)
runKillTask session multi args = case multi of
    Just ctx | isSubagentIdText args.taskId -> do
        result <- closeSubagent ctx.multiRegistry (SubagentId args.taskId)
        pure $ case result of
            Left err -> Left err
            Right _ -> Right ("closed subagent " <> args.taskId)
    _ ->
        Right . stripAnsi <$> killTask session args.taskId
