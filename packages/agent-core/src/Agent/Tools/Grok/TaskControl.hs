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
import Agent.ToolArgs (objectArgs, reqText)
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.ToolDispatch (typedTool)
import Agent.Tools.Grok.Common (jsonTool, optionalTimeout, stripAnsi)
import Agent.Tools.Grok.Shell
    ( GrokSession
    , killTask
    , readTaskOutput
    )
import Agent.Tools.Grok.Task (isSubagentIdText)
import Agent.Tools.MultiAgents (MultiAgentContext(..))
import Agent.Tools.Types (AppTool)
import Control.Applicative ((<|>))
import Data.Aeson (FromJSON(..))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)

data TaskOutputArgs = TaskOutputArgs
    { taskId :: Text
    , timeout :: Maybe Int
    }

instance FromJSON TaskOutputArgs where
    parseJSON = objectArgs \object ->
        TaskOutputArgs
            <$> (reqText object "task_id" <|> reqText object "task_ids")
            <*> optionalTimeout object

getTaskOutputTool :: GrokSession -> Maybe MultiAgentContext -> AppTool
getTaskOutputTool session multi = jsonTool "get_task_output" getTaskOutputDescription
    [ PropertySchema "task_id" PropertyString True $ Just
        "The task id from a background run_terminal_cmd or the subagent_id from task."
    , PropertySchema "timeout" PropertyInteger False $ Just
        "Optional wait in milliseconds. If omitted, return a snapshot immediately."
    ]
    True
    (typedTool "get_task_output" (runGetTaskOutput session multi))

getTaskOutputDescription :: Text
getTaskOutputDescription =
    "Read output from a background command or subagent.\n\
    \Use this for a snapshot of current output, or one bounded wait — not a polling loop."

runGetTaskOutput
    :: GrokSession
    -> Maybe MultiAgentContext
    -> TaskOutputArgs
    -> IO (Either Text Text)
runGetTaskOutput session multi args = case multi of
    Just ctx | isSubagentIdText args.taskId -> do
        let agentId = SubagentId args.taskId
            timeoutMs = fromMaybe 0 args.timeout
        if timeoutMs <= 0
            then do
                status <- getStatus ctx.multiRegistry agentId
                pure $ Right $ formatAgentWait args.taskId False (Just status)
            else do
                (statuses, timedOut) <-
                    waitSubagents ctx.multiRegistry [agentId] timeoutMs
                pure $ Right $ formatAgentWait args.taskId timedOut (Map.lookup agentId statuses)
    _ ->
        Right . stripAnsi <$> readTaskOutput session args.taskId args.timeout

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
