module Agent.GrokBuild.Dialect.TaskControl
    ( getTaskOutputTool
    , waitTasksTool
    , killTaskTool
    , validateTaskIds
    ) where

import Agent.Subagents
    ( SubagentId(..)
    , SubagentStatus(..)
    , closeSubagent
    , getStatus
    , waitSubagents
    )
import Agent.Subagents.Format (isFinalStatus)
import qualified Agent.Json.Decode as Json
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.ToolDispatch (typedTool)
import Agent.GrokBuild.Dialect.Common (jsonTool, stripAnsi)
import Agent.GrokBuild.Dialect.Json
    ( optionalInt
    , requiredTextList
    , textList
    )
import Agent.GrokBuild.Dialect.Shell
    ( GrokSession
    , killTask
    , readTaskOutput
    )
import Agent.GrokBuild.Dialect.Task (isSubagentIdText)
import Agent.Tools.MultiAgents (MultiAgentContext(..))
import Agent.Tools.Types
    ( AppTool
    , ToolExecutionPolicy(..)
    )
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently)
import Data.Containers.ListUtils (nubOrd)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (diffUTCTime, getCurrentTime)

data TaskOutputArgs = TaskOutputArgs
    { taskIds :: [Text]
    , timeoutMs :: Maybe Int
    }

taskOutputArgsDecoder :: Json.Decoder TaskOutputArgs
taskOutputArgsDecoder = Json.object do
        canonicalIds <- Json.atKeyOptional "task_ids" textList
        legacyIds <- Json.atKeyOptional "task_id" textList
        taskIds <- maybe (maybe (fail "Missing parameter: task_ids") pure legacyIds) pure canonicalIds
        canonicalTimeout <- optionalInt "timeout_ms"
        legacyTimeout <- optionalInt "timeout"
        pure TaskOutputArgs
            { taskIds
            , timeoutMs = case canonicalTimeout of
                Just timeout -> Just timeout
                Nothing -> legacyTimeout
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
    (typedTool "get_task_output" taskOutputArgsDecoder (runGetTaskOutput session multi))

getTaskOutputDescription :: Text
getTaskOutputDescription =
    "Get output and status from a background task or subagent.\n\n\
    \Usage notes:\n\
    \- Pass task_ids with one or more ids returned by a background command or subagent; for a single task use a one-element array. Multiple ids with a positive timeout_ms wait until all complete\n\
    \- Omit timeout_ms or pass 0 for a non-blocking status snapshot; set a positive timeout_ms to wait up to that many milliseconds, capped at 600000 (~10 min)\n\
    \- Returns current output and status."

runGetTaskOutput
    :: GrokSession
    -> Maybe MultiAgentContext
    -> TaskOutputArgs
    -> IO (Either Text Text)
runGetTaskOutput session multi args
    = case validateTaskIds args.taskIds of
        Left err -> pure (Left err)
        Right resolvedIds -> do
            entries <- mapConcurrently
                (runOneTaskOutput session multi effectiveTimeout)
                resolvedIds
            pure $ Right $ case entries of
                [entry] -> entry.output
                _ -> formatMultiTaskOutput waits entries
  where
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

data WaitMode
    = WaitAny
    | WaitAll

data WaitTasksArgs = WaitTasksArgs
    { waitTaskIds :: ![Text]
    , waitMode :: !WaitMode
    , waitTimeoutMs :: !(Maybe Int)
    }

waitTasksArgsDecoder :: Json.Decoder WaitTasksArgs
waitTasksArgsDecoder = Json.object do
        waitTaskIds <- requiredTextList "task_ids"
        mode <- Json.atKey "mode" Json.text
        waitMode <- case Text.toLower (Text.strip mode) of
            "wait_any" -> pure WaitAny
            "wait_all" -> pure WaitAll
            _ -> fail "mode must be wait_any or wait_all"
        waitTimeoutMs <- optionalInt "timeout_ms"
        pure WaitTasksArgs{..}

waitTasksTool :: GrokSession -> Maybe MultiAgentContext -> AppTool
waitTasksTool session multi =
    jsonTool "wait_tasks" waitTasksDescription
        [ PropertySchema "task_ids" (PropertyArray PropertyString) True $ Just
            "Task IDs to wait for."
        , PropertySchema "mode"
            (PropertyEnum ["wait_any", "wait_all"])
            True
            (Just "Wait mode: wait_any returns when the first task completes; wait_all waits for every task.")
        , PropertySchema "timeout_ms" PropertyInteger False $ Just
            "Maximum wait time in milliseconds, capped at 600000."
        ]
        True
        TurnSequential
        (typedTool "wait_tasks" waitTasksArgsDecoder (runWaitTasks session multi))

waitTasksDescription :: Text
waitTasksDescription =
    "Wait for multiple background commands or subagents to complete.\n\n\
    \Prefer get_command_or_subagent_output with a positive timeout_ms when you also need current output."

runWaitTasks
    :: GrokSession
    -> Maybe MultiAgentContext
    -> WaitTasksArgs
    -> IO (Either Text Text)
runWaitTasks session multi args
    = case validateTaskIds args.waitTaskIds of
        Left err -> pure (Left err)
        Right taskIds -> do
            started <- getCurrentTime
            waitLoop taskIds started
  where
    timeoutMs = min maxTaskOutputWaitMs
        (max 1 (fromMaybe maxTaskOutputWaitMs args.waitTimeoutMs))
    waitLoop taskIds started = do
        entries <- mapConcurrently
            (runOneTaskOutput session multi Nothing)
            taskIds
        let completed = length (filter (.terminal) entries)
            satisfied = case args.waitMode of
                WaitAny -> completed > 0
                WaitAll -> completed == length entries
        now <- getCurrentTime
        let elapsedMs =
                floor (realToFrac (diffUTCTime now started) * (1000 :: Double))
        if satisfied || elapsedMs >= timeoutMs
            then pure $ Right $ formatWaitResult args.waitMode completed entries
            else threadDelay 50000 >> waitLoop taskIds started

validateTaskIds :: [Text] -> Either Text [Text]
validateTaskIds taskIds
    | null resolvedIds = Left "Provide a non-empty task_ids list."
    | length resolvedIds > maxTaskOutputIds =
        Left $
            "task_ids exceeds maximum of "
                <> Text.pack (show maxTaskOutputIds)
                <> " entries."
    | otherwise = Right resolvedIds
  where
    resolvedIds =
        nubOrd
            [ stripped
            | taskId <- taskIds
            , let stripped = Text.strip taskId
            , not (Text.null stripped)
            ]

formatWaitResult :: WaitMode -> Int -> [TaskOutputEntry] -> Text
formatWaitResult mode completed entries =
    Text.intercalate "\n"
        [ entry.taskOutputId
            <> ": "
            <> if entry.terminal then "completed" else "running"
        | entry <- entries
        ]
        <> "\n\n"
        <> Text.pack (show completed)
        <> "/"
        <> Text.pack (show (length entries))
        <> " tasks completed ("
        <> case mode of
            WaitAny -> "wait_any)"
            WaitAll -> "wait_all)"

newtype KillTaskArgs = KillTaskArgs { taskId :: Text }

killTaskArgsDecoder :: Json.Decoder KillTaskArgs
killTaskArgsDecoder = Json.object $
    KillTaskArgs <$> Json.atKey "task_id" Json.text

killTaskTool :: GrokSession -> Maybe MultiAgentContext -> AppTool
killTaskTool session multi = jsonTool "kill_task" killTaskDescription
    [ PropertySchema "task_id" PropertyString True $ Just
        "The id returned by a background command or subagent."
    ]
    False
    TurnSequential
    (typedTool "kill_task" killTaskArgsDecoder (runKillTask session multi))

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
