-- | Session-owned recurring prompt scheduler.
--
-- Scheduled fires are detached Grok subagents. The actor is explicitly owned
-- by 'SchedulerRuntime'; callers must use 'closeSchedulerRuntime' (the dialect
-- runtime registers that cleanup in its resource scope).
module Agent.GrokBuild.Dialect.Scheduler
    ( SchedulerRuntime
    , ScheduledFire(..)
    , ScheduledTaskSnapshot(..)
    , newSchedulerRuntime
    , newSchedulerRuntimeWithFire
    , newSchedulerRuntimeWithFireStatus
    , closeSchedulerRuntime
    , schedulerTools
    , schedulerCreateTool
    , schedulerDeleteTool
    , schedulerListTool
    , listScheduledTasks
    , parseSchedulerInterval
    , intervalToHuman
    ) where

import Agent.GrokBuild.Dialect.Common (jsonTool)
import Agent.Concurrent (mapConcurrentlyBounded)
import Agent.InterAgentMessage
    ( InterAgentMessage(..)
    , InterAgentMessageType(QueuedMessage)
    , plainInterAgentContent
    )
import Agent.GrokBuild.Dialect.Task
    ( GrokSubagentSpec(..)
    , GrokSubagentSpecs
    , runtimeSubagentType
    , spawnManagedGrokSubagent
    )
import Agent.Subagents
    ( SubagentId
    , SubagentStatus(..)
    , getStatus
    )
import qualified Agent.Json.Decode as Json
import Agent.GrokBuild.Dialect.Json (optionalBool, optionalText)
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.ToolDispatch (typedTool)
import Agent.Tools.MultiAgents (MultiAgentContext(..))
import Agent.Tools.Types (AppTool, ToolExecutionPolicy(..))
import Control.Concurrent
    ( Chan
    , newChan
    , readChan
    , threadDelay
    , writeChan
    )
import Control.Concurrent.Async
    ( Async
    , asyncWithUnmask
    , cancel
    , race
    , waitCatch
    )
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , modifyMVar_
    , newMVar
    , readMVar
    )
import Control.Exception.Safe (tryAny)
import Control.Monad (void)
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson.Text as Aeson
import Data.Char (isDigit)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import Data.Time
    ( UTCTime
    , addUTCTime
    , diffUTCTime
    , getCurrentTime
    )
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Word (Word64)
import System.OsPath (OsPath)

maximumScheduledTasks :: Int
maximumScheduledTasks = 50

schedulerConcurrencyLimit :: Int
schedulerConcurrencyLimit = 8

recurringTaskTtlSeconds :: Integer
recurringTaskTtlSeconds = 7 * 24 * 60 * 60

data ScheduledTask = ScheduledTask
    { scheduledId :: !Text
    , scheduledIntervalSeconds :: !Integer
    , scheduledPrompt :: !Text
    , scheduledCreatedAt :: !UTCTime
    , scheduledLastFiredAt :: !(Maybe UTCTime)
    , scheduledExpiresAt :: !UTCTime
    , scheduledActiveAgent :: !(Maybe SubagentId)
    }

data SchedulerState = SchedulerState
    { schedulerNextId :: !Int
    , schedulerTasks :: !(Map Text ScheduledTask)
    }

data SchedulerSignal
    = SchedulerWake
    | SchedulerStop

data ScheduledFire = ScheduledFire
    { scheduledFireTaskId :: !Text
    , scheduledFirePrompt :: !Text
    , scheduledFireIntervalSeconds :: !Integer
    } deriving (Eq, Show)

data ScheduledTaskSnapshot = ScheduledTaskSnapshot
    { scheduledTaskId :: !Text
    , scheduledTaskPrompt :: !Text
    , scheduledTaskIntervalSeconds :: !Integer
    , scheduledTaskNextFireAt :: !UTCTime
    , scheduledTaskCreatedAt :: !UTCTime
    } deriving (Eq, Show)

data SchedulerRuntime = SchedulerRuntime
    { schedulerState :: !(MVar SchedulerState)
    , schedulerSignal :: !(Chan SchedulerSignal)
    , schedulerWorker :: !(Async ())
    , schedulerNow :: !(IO UTCTime)
    }

newSchedulerRuntime
    :: OsPath
    -> MultiAgentContext
    -> GrokSubagentSpecs
    -> IO SchedulerRuntime
newSchedulerRuntime cwd multi specs =
    newSchedulerRuntimeWithFireStatus
        getCurrentTime
        (\fire -> do
            result <- spawnManagedGrokSubagent
                cwd
                multi
                specs
                GrokSubagentSpec
                    { agentType = runtimeSubagentType
                    , modelOverride = Nothing
                    , reasoningEffortOverride = Nothing
                    }
                (scheduledIterationPrompt fire)
                (Just
                    ("scheduled loop "
                        <> fire.scheduledFireTaskId))
            case (result, multi.multiSendToRoot) of
                (Left err, Just sendToRoot) -> do
                    _ <- sendToRoot InterAgentMessage
                        { messageAuthor =
                            "scheduler:" <> fire.scheduledFireTaskId
                        , messageRecipient = "root"
                        , messageType = QueuedMessage
                        , messageContent =
                            plainInterAgentContent
                                ("Scheduled task failed to launch: " <> err)
                        }
                    pure ()
                _ -> pure ()
            pure result)
        (\agentId ->
            getStatus multi.multiRegistry agentId >>= \case
                Pending -> pure True
                Running -> pure True
                _ -> pure False)

-- | Testable constructor with injectable clock and fire callback.
--
-- The returned 'Async' is retained inside the runtime and joined by
-- 'closeSchedulerRuntime'; it is not an unowned background thread.
newSchedulerRuntimeWithFire
    :: IO UTCTime
    -> (ScheduledFire -> IO (Either Text SubagentId))
    -> IO SchedulerRuntime
newSchedulerRuntimeWithFire now fire =
    newSchedulerRuntimeWithFireStatus now fire (const (pure False))

newSchedulerRuntimeWithFireStatus
    :: IO UTCTime
    -> (ScheduledFire -> IO (Either Text SubagentId))
    -> (SubagentId -> IO Bool)
    -> IO SchedulerRuntime
newSchedulerRuntimeWithFireStatus now fire isActive = do
    state <- newMVar SchedulerState
        { schedulerNextId = 0
        , schedulerTasks = Map.empty
        }
    signal <- newChan
    worker <- asyncWithUnmask \restore ->
        restore (schedulerActor now fire isActive state signal)
    pure SchedulerRuntime
        { schedulerState = state
        , schedulerSignal = signal
        , schedulerWorker = worker
        , schedulerNow = now
        }

closeSchedulerRuntime :: SchedulerRuntime -> IO ()
closeSchedulerRuntime runtime = do
    writeChan runtime.schedulerSignal SchedulerStop
    race
        (threadDelay 1000000)
        (waitCatch runtime.schedulerWorker)
        >>= \case
            Right _ -> pure ()
            Left () -> do
                cancel runtime.schedulerWorker
                void (waitCatch runtime.schedulerWorker)

schedulerTools :: SchedulerRuntime -> [AppTool]
schedulerTools runtime =
    [ schedulerCreateTool runtime
    , schedulerDeleteTool runtime
    , schedulerListTool runtime
    ]

data SchedulerCreateArgs = SchedulerCreateArgs
    { taskId :: !(Maybe Text)
    , interval :: !(Maybe Text)
    , prompt :: !(Maybe Text)
    , recurring :: !Bool
    , durable :: !(Maybe Bool)
    , foreground :: !(Maybe Bool)
    , fireImmediately :: !Bool
    }

schedulerCreateArgsDecoder :: Json.Decoder SchedulerCreateArgs
schedulerCreateArgsDecoder = Json.object $
    SchedulerCreateArgs
        <$> optionalText "task_id"
        <*> optionalText "interval"
        <*> optionalText "prompt"
        <*> (fromMaybe True <$> optionalBool "recurring")
        <*> optionalBool "durable"
        <*> optionalBool "foreground"
        <*> (fromMaybe False <$> optionalBool "fire_immediately")

schedulerCreateTool :: SchedulerRuntime -> AppTool
schedulerCreateTool runtime =
    jsonTool
        "scheduler_create"
        schedulerCreateDescription
        [ PropertySchema "task_id" PropertyString False $ Just
            "Id of an existing task to update in place. Provided interval and prompt replace old values; omitted values are unchanged."
        , PropertySchema "interval" PropertyString False $ Just
            "Interval between executions, e.g. \"5m\", \"2h\", \"1d\". Required to create; optional with task_id."
        , PropertySchema "prompt" PropertyString False $ Just
            "The prompt text to execute on each scheduled fire. Required to create; optional with task_id."
        , PropertySchema "durable" PropertyBoolean False $ Just
            "Whether the task persists across sessions. Default: false. This host supports session-only tasks."
        , PropertySchema "foreground" PropertyBoolean False $ Just
            "Run each fire as a main-conversation turn. Default: false. This host supports detached subagent fires only."
        , PropertySchema "fire_immediately" PropertyBoolean False $ Just
            "Whether to fire immediately on creation. Default: false."
        ]
        False
        TurnSequential
        (typedTool "scheduler_create" schedulerCreateArgsDecoder (runSchedulerCreate runtime))

schedulerCreateDescription :: Text
schedulerCreateDescription =
    "Create a scheduled task that runs a prompt on a recurring interval, or update an existing one in place.\n\n\
    \Set fire_immediately=true to also fire once on creation; by default the first run waits for the interval.\n\n\
    \Interval format: 5m, 2h, 1d, or 60s (minimum 60 seconds). Maximum 50 tasks. Tasks auto-expire after 7 days. For one-time delayed work, use a background terminal command instead."

runSchedulerCreate
    :: SchedulerRuntime
    -> SchedulerCreateArgs
    -> IO (Either Text Text)
runSchedulerCreate runtime args
    | args.durable == Just True =
        pure
            (Left
                "scheduler_durability_unsupported: durable=true is not supported; scheduled tasks are session-only.")
    | args.foreground == Just True =
        pure
            (Left
                "scheduler_foreground_unsupported: foreground=true is not supported; fires run as detached background subagents.")
    | Just taskId <- nonBlank args.taskId =
        updateScheduledTask runtime taskId args
    | not args.recurring =
        pure
            (Left
                "one-shot tasks are not supported; run a background terminal command instead (`sleep <secs> && <command>`) or do the work now")
    | otherwise =
        createScheduledTask runtime args

createScheduledTask
    :: SchedulerRuntime
    -> SchedulerCreateArgs
    -> IO (Either Text Text)
createScheduledTask runtime args =
    case nonBlank args.interval of
        Nothing ->
            pure (Left "interval is required when creating a task")
        Just rawInterval ->
            case parseSchedulerInterval rawInterval of
                Left err -> pure (Left err)
                Right intervalSeconds ->
                    case nonBlank args.prompt of
                        Nothing ->
                            pure (Left "prompt is required when creating a task")
                        Just prompt -> do
                            now <- runtime.schedulerNow
                            result <-
                                modifyMVar runtime.schedulerState \state ->
                                    let liveTasks =
                                            activeTasksAt
                                                now
                                                state.schedulerTasks
                                        liveState = state
                                            { schedulerTasks = liveTasks }
                                    in if Map.size liveTasks
                                        >= maximumScheduledTasks
                                        then
                                            pure
                                                ( liveState
                                                , Left
                                                    ("maximum of "
                                                        <> Text.pack
                                                            (show maximumScheduledTasks)
                                                        <> " scheduled tasks reached")
                                                )
                                        else do
                                            let next = state.schedulerNextId + 1
                                                taskId =
                                                    "sched-"
                                                        <> Text.pack (show next)
                                                immediateAnchor =
                                                    if args.fireImmediately
                                                        then
                                                            Just
                                                                (addUTCTime
                                                                    (negate
                                                                        (fromInteger
                                                                            intervalSeconds))
                                                                    now)
                                                        else Nothing
                                                task = ScheduledTask
                                                    { scheduledId = taskId
                                                    , scheduledIntervalSeconds =
                                                        intervalSeconds
                                                    , scheduledPrompt = prompt
                                                    , scheduledCreatedAt =
                                                        now
                                                    , scheduledLastFiredAt =
                                                        immediateAnchor
                                                    , scheduledExpiresAt =
                                                        addUTCTime
                                                            (fromInteger
                                                                recurringTaskTtlSeconds)
                                                            now
                                                    , scheduledActiveAgent =
                                                        Nothing
                                                    }
                                                updated = liveState
                                                    { schedulerNextId = next
                                                    , schedulerTasks =
                                                        Map.insert
                                                            taskId
                                                            task
                                                            liveTasks
                                                    }
                                            pure (updated, Right task)
                            case result of
                                Left err -> pure (Left err)
                                Right task -> do
                                    writeChan
                                        runtime.schedulerSignal
                                        SchedulerWake
                                    pure
                                        (Right
                                            (schedulerCreateOutput False task))

updateScheduledTask
    :: SchedulerRuntime
    -> Text
    -> SchedulerCreateArgs
    -> IO (Either Text Text)
updateScheduledTask runtime taskId args
    | intervalInput == Nothing
    , promptInput == Nothing =
        pure
            (Left
                "nothing to update: provide interval and/or prompt alongside task_id")
    | otherwise =
        case traverse parseSchedulerInterval intervalInput of
            Left err -> pure (Left err)
            Right parsedInterval -> do
                now <- runtime.schedulerNow
                result <-
                    modifyMVar runtime.schedulerState \state ->
                        let liveTasks =
                                activeTasksAt now state.schedulerTasks
                            liveState = state
                                { schedulerTasks = liveTasks }
                        in case Map.lookup taskId liveTasks of
                            Nothing ->
                                pure
                                    ( liveState
                                    , Left
                                        ("no scheduled task with id "
                                            <> taskId
                                            <> "; call scheduler_list to see active task ids")
                                    )
                            Just old -> do
                                let withValues = old
                                        { scheduledPrompt =
                                            fromMaybe
                                                old.scheduledPrompt
                                                promptInput
                                        , scheduledIntervalSeconds =
                                            fromMaybe
                                                old.scheduledIntervalSeconds
                                                parsedInterval
                                        }
                                    reanchored =
                                        if nextFireAt withValues <= now
                                            then withValues
                                                { scheduledLastFiredAt =
                                                    Just now
                                                }
                                            else withValues
                                    updated = liveState
                                        { schedulerTasks =
                                            Map.insert
                                                taskId
                                                reanchored
                                                liveTasks
                                        }
                                pure (updated, Right reanchored)
                case result of
                    Left err -> pure (Left err)
                    Right task -> do
                        writeChan runtime.schedulerSignal SchedulerWake
                        pure (Right (schedulerCreateOutput True task))
  where
    intervalInput = nonBlank args.interval
    promptInput = nonBlank args.prompt

data SchedulerDeleteArgs = SchedulerDeleteArgs
    { deleteId :: !Text
    }

schedulerDeleteArgsDecoder :: Json.Decoder SchedulerDeleteArgs
schedulerDeleteArgsDecoder = Json.object $
    SchedulerDeleteArgs <$> Json.atKey "id" Json.text

schedulerDeleteTool :: SchedulerRuntime -> AppTool
schedulerDeleteTool runtime =
    jsonTool
        "scheduler_delete"
        "Cancel a scheduled task by ID. Returns success=true when the task was found and removed."
        [ PropertySchema "id" PropertyString True $ Just
            "The task ID to cancel (from scheduler_create output)."
        ]
        False
        TurnSequential
        (typedTool "scheduler_delete" schedulerDeleteArgsDecoder (runSchedulerDelete runtime))

runSchedulerDelete
    :: SchedulerRuntime
    -> SchedulerDeleteArgs
    -> IO (Either Text Text)
runSchedulerDelete runtime args = do
    let taskId = Text.strip args.deleteId
    now <- runtime.schedulerNow
    removed <-
        modifyMVar runtime.schedulerState \state ->
            let liveTasks = activeTasksAt now state.schedulerTasks
                existed = Map.member taskId liveTasks
                updated = state
                    { schedulerTasks =
                        Map.delete taskId liveTasks
                    }
            in pure (updated, existed)
    writeChan runtime.schedulerSignal SchedulerWake
    pure $ Right $ jsonText $ object
        [ "success" .= removed
        , "message" .=
            if removed
                then "Scheduled task " <> taskId <> " cancelled."
                else
                    "No scheduled task with ID " <> taskId
                        <> " found. Use scheduler_list to see active tasks."
        ]

data SchedulerListArgs = SchedulerListArgs

schedulerListArgsDecoder :: Json.Decoder SchedulerListArgs
schedulerListArgsDecoder = Json.object (pure SchedulerListArgs)

schedulerListTool :: SchedulerRuntime -> AppTool
schedulerListTool runtime =
    jsonTool
        "scheduler_list"
        "List all active scheduled tasks with their IDs, prompts, intervals, and next fire times."
        []
        True
        TurnSequential
        (typedTool "scheduler_list" schedulerListArgsDecoder \SchedulerListArgs ->
            Right . schedulerListOutput <$> listScheduledTasks runtime)

listScheduledTasks
    :: SchedulerRuntime
    -> IO [ScheduledTaskSnapshot]
listScheduledTasks runtime = do
    now <- runtime.schedulerNow
    state <- readMVar runtime.schedulerState
    pure $
        sortOn (.scheduledTaskCreatedAt)
            [ taskSnapshot task
            | task <- Map.elems (activeTasksAt now state.schedulerTasks)
            ]

schedulerCreateOutput :: Bool -> ScheduledTask -> Text
schedulerCreateOutput updated task =
    jsonText $ object
        [ "id" .= task.scheduledId
        , "humanSchedule" .=
            intervalToHuman task.scheduledIntervalSeconds
        , "updated" .= updated
        ]

schedulerListOutput :: [ScheduledTaskSnapshot] -> Text
schedulerListOutput tasks =
    jsonText $ object
        [ "tasks" .=
            [ object
                [ "id" .= task.scheduledTaskId
                , "prompt" .= truncatePrompt task.scheduledTaskPrompt
                , "intervalHuman" .=
                    intervalToHuman task.scheduledTaskIntervalSeconds
                , "nextFireAt" .=
                    formatTimestamp task.scheduledTaskNextFireAt
                , "createdAt" .=
                    formatTimestamp task.scheduledTaskCreatedAt
                , "recurring" .= True
                ]
            | task <- tasks
            ]
        ]

taskSnapshot :: ScheduledTask -> ScheduledTaskSnapshot
taskSnapshot task = ScheduledTaskSnapshot
    { scheduledTaskId = task.scheduledId
    , scheduledTaskPrompt = task.scheduledPrompt
    , scheduledTaskIntervalSeconds = task.scheduledIntervalSeconds
    , scheduledTaskNextFireAt = nextFireAt task
    , scheduledTaskCreatedAt = task.scheduledCreatedAt
    }

nextFireAt :: ScheduledTask -> UTCTime
nextFireAt task =
    addUTCTime
        (fromInteger task.scheduledIntervalSeconds)
        (fromMaybe task.scheduledCreatedAt task.scheduledLastFiredAt)

schedulerActor
    :: IO UTCTime
    -> (ScheduledFire -> IO (Either Text SubagentId))
    -> (SubagentId -> IO Bool)
    -> MVar SchedulerState
    -> Chan SchedulerSignal
    -> IO ()
schedulerActor now fire isActive state signal = loop
  where
    loop = do
        refreshActiveAgents
        current <- now
        fires <- modifyMVar state \snapshot ->
            let (updated, due) = advanceScheduler current snapshot
            in pure (updated, due)
        fireResults <-
            mapConcurrentlyBounded schedulerConcurrencyLimit runFire fires
        modifyMVar_ state \snapshot ->
            pure snapshot
                { schedulerTasks =
                    foldr publishFire
                        snapshot.schedulerTasks
                        fireResults
                }
        currentAfterFire <- now
        snapshot <- readMVar state
        let delayMicros = nextDelayMicros currentAfterFire snapshot
        race (threadDelay delayMicros) (readChan signal) >>= \case
            Right SchedulerStop -> pure ()
            _ -> loop

    runFire scheduled =
        tryAny (fire scheduled) >>= \case
            Right (Right agentId) ->
                pure (Just (scheduled.scheduledFireTaskId, agentId))
            _ -> pure Nothing

    publishFire Nothing tasks = tasks
    publishFire (Just (taskId, agentId)) tasks =
        Map.adjust
            (\task ->
                task
                    { scheduledActiveAgent =
                        Just agentId
                    })
            taskId
            tasks

    refreshActiveAgents = do
        snapshot <- readMVar state
        let active =
                [ (taskId, agentId)
                | (taskId, task) <- Map.toAscList snapshot.schedulerTasks
                , Just agentId <- [task.scheduledActiveAgent]
                ]
        activity <- Map.fromAscList
            <$> mapConcurrentlyBounded
                schedulerConcurrencyLimit
                (\(taskId, agentId) ->
                    (\activeNow -> (taskId, activeNow))
                        <$> isActive agentId)
                active
        modifyMVar_ state \current ->
            pure current
                { schedulerTasks =
                    Map.mapWithKey
                        (\taskId task ->
                            case task.scheduledActiveAgent of
                                Nothing -> task
                                Just _
                                    | Map.findWithDefault False taskId activity ->
                                        task
                                    | otherwise ->
                                        task { scheduledActiveAgent = Nothing })
                        current.schedulerTasks
                }

advanceScheduler
    :: UTCTime
    -> SchedulerState
    -> (SchedulerState, [ScheduledFire])
advanceScheduler now state =
    let live =
            Map.filter (\task -> task.scheduledExpiresAt > now)
                state.schedulerTasks
        dueTasks =
            [ task
            | task <- Map.elems live
            , nextFireAt task <= now
            , isNothing task.scheduledActiveAgent
            ]
        fired =
            foldr
                (\task ->
                    Map.adjust
                        (\current ->
                            current { scheduledLastFiredAt = Just now })
                        task.scheduledId)
                live
                dueTasks
        fires =
            [ ScheduledFire
                { scheduledFireTaskId = task.scheduledId
                , scheduledFirePrompt = task.scheduledPrompt
                , scheduledFireIntervalSeconds =
                    task.scheduledIntervalSeconds
                }
            | task <- dueTasks
            ]
    in (state { schedulerTasks = fired }, fires)

nextDelayMicros :: UTCTime -> SchedulerState -> Int
nextDelayMicros now state =
    case map nextWake (Map.elems state.schedulerTasks) of
        [] -> maximumActorSleepMicros
        wakes ->
            let seconds =
                    realToFrac
                        (diffUTCTime (minimum wakes) now)
                        :: Double
                micros = floor (max 0 seconds * 1000000)
            in max 1 (min maximumActorSleepMicros micros)
  where
    nextWake task
        | isNothing task.scheduledActiveAgent = nextWakeAt task
        | otherwise =
            min
                task.scheduledExpiresAt
                (addUTCTime 1 now)

nextWakeAt :: ScheduledTask -> UTCTime
nextWakeAt task =
    min task.scheduledExpiresAt (nextFireAt task)

activeTasksAt
    :: UTCTime
    -> Map Text ScheduledTask
    -> Map Text ScheduledTask
activeTasksAt now =
    Map.filter (\task -> task.scheduledExpiresAt > now)

maximumActorSleepMicros :: Int
maximumActorSleepMicros = 60 * 1000 * 1000

scheduledIterationPrompt :: ScheduledFire -> Text
scheduledIterationPrompt fire =
    Text.unlines
        [ "# Scheduled loop iteration"
        , ""
        , "This is one detached background iteration of scheduled task "
            <> fire.scheduledFireTaskId <> "."
        , "You cannot see the conversation that created the schedule."
        , "Perform the prompt once, report a concise result, and stop. Do not poll or sleep-loop inside this run."
        , "The parent session or user owns cancellation; this child cannot modify the schedule."
        , ""
        , "## Prompt"
        , fire.scheduledFirePrompt
        ]

parseSchedulerInterval :: Text -> Either Text Integer
parseSchedulerInterval raw =
    case Text.unsnoc (Text.strip raw) of
        Nothing -> Left "invalid interval: interval cannot be empty"
        Just (digits, suffix)
            | Text.null digits || not (Text.all isDigit digits) ->
                invalidFormat
            | otherwise ->
                case reads (Text.unpack digits) of
                    [(value, "")]
                        | value == (0 :: Integer) ->
                            Left
                                "invalid interval: interval value must be greater than 0"
                        | otherwise -> do
                            unit <- case suffix of
                                's' -> Right 1
                                'm' -> Right 60
                                'h' -> Right 3600
                                'd' -> Right 86400
                                _ ->
                                    Left
                                        ("invalid interval: invalid interval suffix: "
                                            <> Text.singleton suffix
                                            <> " (expected s, m, h, or d)")
                            let maxSeconds =
                                    toInteger (maxBound :: Word64)
                            if value > maxSeconds `div` unit
                                then
                                    Left
                                        ("invalid interval: interval too large: "
                                            <> Text.strip raw)
                                else Right (max 60 (value * unit))
                    _ -> invalidFormat
  where
    invalidFormat =
        Left
            ("invalid interval: invalid interval format: "
                <> Text.strip raw
                <> " (expected e.g. 5m, 2h, 1d)")

intervalToHuman :: Integer -> Text
intervalToHuman seconds
    | seconds `mod` 86400 == 0 =
        plural (seconds `div` 86400) "day"
    | seconds `mod` 3600 == 0 =
        plural (seconds `div` 3600) "hour"
    | seconds `mod` 60 == 0 =
        plural (seconds `div` 60) "minute"
    | otherwise =
        plural seconds "second"
  where
    plural count unit =
        "every " <> Text.pack (show count) <> " " <> unit
            <> if count == 1 then "" else "s"

truncatePrompt :: Text -> Text
truncatePrompt prompt
    | Text.length prompt <= 80 = prompt
    | otherwise = Text.take 80 prompt <> "..."

formatTimestamp :: UTCTime -> Text
formatTimestamp =
    Text.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

nonBlank :: Maybe Text -> Maybe Text
nonBlank = (>>= keep)
  where
    keep value =
        let stripped = Text.strip value
        in if Text.null stripped then Nothing else Just stripped

jsonText :: Value -> Text
jsonText = LazyText.toStrict . Aeson.encodeToLazyText
