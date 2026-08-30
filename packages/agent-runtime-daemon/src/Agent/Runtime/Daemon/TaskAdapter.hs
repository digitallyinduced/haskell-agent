module Agent.Runtime.Daemon.TaskAdapter
    ( TaskRunner(..)
    , processTaskRunner
    , withTaskAdapter
    ) where

import Control.Concurrent.Async
    ( Async
    , asyncWithUnmask
    , cancel
    , concurrently_
    , race
    , waitCatch
    )
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM
import Control.Exception.Safe
    ( finally
    , isAsyncException
    , mask
    , onException
    , throwIO
    , tryAny
    )
import Control.Monad (foldM, forM_, unless, when)
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import Data.Char (isControl)
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time (getCurrentTime)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO (hIsEOF, hSetEncoding, utf8)
import System.Process

import Agent.Runtime.Daemon.Journal
import Agent.Runtime.Daemon.Protocol (EventEnvelope (..))
import Agent.Runtime.Daemon.Supervisor
import Agent.Runtime.Daemon.Task
import Agent.Runtime.Daemon.TaskScheduler

data TaskRunner = TaskRunner
    { runTask :: DurableTask -> (Text -> IO ()) -> IO (Either Text ())
    , resolveApproval :: TaskId -> Text -> Text -> IO (Either Text ())
    }

-- | Execute a submitted turn through the installed @agent-cli@ executable.
-- Set @HASKELL_AGENT_CLI@ to select another executable. The process receives
-- arguments directly (never through a shell), and its bounded persisted log is
-- populated from stdout and stderr.
processTaskRunner :: IO TaskRunner
processTaskRunner = do
    executable <- maybe "agent-cli" id <$> lookupEnv "HASKELL_AGENT_CLI"
    pure
        TaskRunner
            { runTask = runProcessTask executable
            , resolveApproval = \_ _ _ ->
                pure (Left "the process task runner cannot resolve interactive approvals")
            }

data TaskCommand
    = Submit SubmitCommand
    | Cancel TaskId
    | List
    | SetLimit Int
    | Approval TaskId Text Text
    | Retry TaskId

data SubmitCommand = SubmitCommand
    { submittedId :: !TaskId
    , submittedSessionId :: !(Maybe Text)
    , submittedPrompt :: !Text
    , submittedWorkingDirectory :: !FilePath
    , submittedProvider :: !(Maybe Text)
    , submittedModel :: !(Maybe Text)
    , submittedEffort :: !(Maybe Text)
    , submittedWorktree :: !Bool
    }

data AdapterMessage
    = Execute !TaskCommand !(TMVar (Either Text Value))
    | TaskLogged !TaskId !Int !Text
    | TaskFinished !TaskId !Int !(Either Text ())

data RunningTask = RunningTask
    { runningAttempt :: !Int
    , runningWorker :: !(Async ())
    }

data AdapterState = AdapterState
    { adapterLimit :: !Int
    , adapterPending :: !(Seq TaskId)
    , adapterRunning :: !(Map TaskId RunningTask)
    , adapterTasks :: !(Map TaskId DurableTask)
    , adapterInputs :: !(Map TaskId DurableTask)
    }

defaultTaskLimit :: Int
defaultTaskLimit = 4

maximumTaskLimit :: Int
maximumTaskLimit = 32

withTaskAdapter :: Journal -> TaskRunner -> (Supervisor -> IO value) -> IO value
withTaskAdapter journal runner action = do
    saved <- snapshot journal
    commands <- newTBQueueIO 1_024
    let initial =
            AdapterState
                { adapterLimit = defaultTaskLimit
                , adapterPending = Seq.empty
                , adapterRunning = Map.empty
                , adapterTasks = saved.tasks
                , adapterInputs = Map.empty
                }
        supervisor =
            Supervisor
                { handleCommand = \_ raw -> do
                    reply <- newEmptyTMVarIO
                    case parseTaskCommand raw of
                        Left message -> pure (Left message)
                        Right command -> do
                            atomically (writeTBQueue commands (Execute command reply))
                            atomically (takeTMVar reply)
                }
    workers <- newTVarIO Map.empty
    let loop = schedulerLoopWithRegistry journal runner commands workers initial
    ( race loop (action supervisor) >>= \case
        Left () -> fail "task scheduler stopped unexpectedly"
        Right value -> pure value
      ) `finally` shutdownRegistry workers

schedulerLoopWithRegistry
    :: Journal
    -> TaskRunner
    -> TBQueue AdapterMessage
    -> TVar (Map TaskId RunningTask)
    -> AdapterState
    -> IO ()
schedulerLoopWithRegistry journal runner commands registry = go
  where
    go state0 = do
        state <- startRunnable journal runner commands registry state0
        message <- atomically (readTBQueue commands)
        handleMessage state message >>= go

    handleMessage state = \case
        Execute command reply -> do
            (next, result) <-
                executeCommand journal runner registry state command
            atomically (putTMVar reply result)
            pure next
        TaskLogged taskId taskAttempt line ->
            case Map.lookup taskId state.adapterTasks of
                Just task
                    | task.status == TaskRunning
                    , task.attempt == taskAttempt -> do
                        now <- getCurrentTime
                        let updated =
                                task
                                    { updatedAt = now
                                    , logTail = task.logTail <> [line]
                                    }
                        persisted <- persistBounded journal updated
                        pure state
                            { adapterTasks =
                                Map.insert taskId persisted state.adapterTasks
                            }
                _ -> pure state
        TaskFinished taskId taskAttempt result -> do
            case Map.lookup taskId state.adapterRunning of
                Just running | running.runningAttempt == taskAttempt -> do
                    _ <- waitCatch running.runningWorker
                    atomically $ modifyTVar' registry (Map.delete taskId)
                    let withoutWorker =
                            state
                                { adapterRunning =
                                    Map.delete taskId state.adapterRunning
                                }
                    case Map.lookup taskId state.adapterTasks of
                        Just task
                            | task.status == TaskRunning
                            , task.attempt == taskAttempt -> do
                                now <- getCurrentTime
                                let updated =
                                        task
                                            { status = either (const TaskFailed) (const TaskCompleted) result
                                            , updatedAt = now
                                            , logTail =
                                                task.logTail
                                                    <> either (\message -> [message]) (const []) result
                                            }
                                persisted <- persistBounded journal updated
                                pure withoutWorker
                                    { adapterTasks =
                                        Map.insert taskId persisted state.adapterTasks
                                    }
                        _ -> pure withoutWorker
                _ -> pure state

executeCommand
    :: Journal
    -> TaskRunner
    -> TVar (Map TaskId RunningTask)
    -> AdapterState
    -> TaskCommand
    -> IO (AdapterState, Either Text Value)
executeCommand journal runner registry state = \case
    Submit submitted
        | Map.member submitted.submittedId state.adapterTasks ->
            pure (state, Left "task id already exists")
        | otherwise -> do
            now <- getCurrentTime
            let task =
                    DurableTask
                        { taskId = submitted.submittedId
                        , sessionId = submitted.submittedSessionId
                        , status = TaskQueued
                        , description = submitted.submittedPrompt
                        , workingDirectory = submitted.submittedWorkingDirectory
                        , provider = submitted.submittedProvider
                        , model = submitted.submittedModel
                        , effort = submitted.submittedEffort
                        , worktree = submitted.submittedWorktree
                        , attempt = 1
                        , updatedAt = now
                        , logTail = []
                        }
            persisted <- persistBounded journal task
            let next =
                    state
                        { adapterPending = state.adapterPending Seq.|> persisted.taskId
                        , adapterTasks =
                            Map.insert persisted.taskId persisted state.adapterTasks
                        , adapterInputs =
                            Map.insert persisted.taskId task state.adapterInputs
                        }
            pure (next, Right (taskResult "submitted" persisted))
    Cancel taskId ->
        case Map.lookup taskId state.adapterTasks of
            Nothing -> pure (state, Left "task id is unknown")
            Just task
                | not (isActive task) ->
                    pure (state, Left "task is not active")
                | otherwise -> do
                    forM_ (Map.lookup taskId state.adapterRunning) $ \running -> do
                        cancel running.runningWorker
                        _ <- waitCatch running.runningWorker
                        pure ()
                    atomically $ modifyTVar' registry (Map.delete taskId)
                    now <- getCurrentTime
                    let updated = task {status = TaskCancelled, updatedAt = now}
                        next =
                            state
                                { adapterPending =
                                    Seq.filter (/= taskId) state.adapterPending
                                , adapterRunning =
                                    Map.delete taskId state.adapterRunning
                                , adapterTasks =
                                    Map.insert taskId updated state.adapterTasks
                                }
                    persisted <- persistBounded journal updated
                    pure
                        ( next
                            { adapterTasks =
                                Map.insert taskId persisted next.adapterTasks
                            }
                        , Right (taskResult "cancelled" persisted)
                        )
    List ->
        pure
            ( state
            , Right $
                object
                    [ "version" .= (1 :: Int)
                    , "tasks" .= Map.elems state.adapterTasks
                    ]
            )
    SetLimit limit ->
        do
            _ <- appendEvent journal "scheduler_limit_changed" $
                object ["version" .= (1 :: Int), "limit" .= limit]
            pure
                ( state {adapterLimit = limit}
                , Right (object ["version" .= (1 :: Int), "limit" .= limit])
                )
    Approval taskId approvalId decision ->
        case Map.lookup taskId state.adapterTasks of
            Just task | task.status == TaskRunning -> do
                result <- runner.resolveApproval taskId approvalId decision
                case result of
                    Left message -> pure (state, Left message)
                    Right () -> do
                        let response =
                                object
                                    [ "version" .= (1 :: Int)
                                    , "task_id" .= taskId
                                    , "approval_id" .= approvalId
                                    ]
                        _ <- appendEvent journal "approval_resolved" $
                            object
                                [ "version" .= (1 :: Int)
                                , "task_id" .= taskId
                                , "approval_id" .= approvalId
                                , "decision" .= decision
                                ]
                        pure (state, Right response)
            _ -> pure (state, Left "task is not running")
    Retry taskId ->
        case Map.lookup taskId state.adapterTasks of
            Nothing -> pure (state, Left "task id is unknown")
            Just task
                | isActive task ->
                    pure (state, Left "active tasks cannot be retried")
                | task.status == TaskCompleted ->
                    pure (state, Left "completed tasks cannot be retried")
                | otherwise -> do
                    now <- getCurrentTime
                    let updated =
                            task
                                { status = TaskQueued
                                , attempt = task.attempt + 1
                                , updatedAt = now
                                , logTail = []
                                }
                        next =
                            state
                                { adapterPending = state.adapterPending Seq.|> taskId
                                , adapterTasks =
                                    Map.insert taskId updated state.adapterTasks
                                }
                    persisted <- persistBounded journal updated
                    pure
                        ( next
                            { adapterTasks =
                                Map.insert taskId persisted next.adapterTasks
                            }
                        , Right (taskResult "queued" persisted)
                        )

startRunnable
    :: Journal
    -> TaskRunner
    -> TBQueue AdapterMessage
    -> TVar (Map TaskId RunningTask)
    -> AdapterState
    -> IO AdapterState
startRunnable journal runner commands registry state = do
    let activeSessions =
            Set.fromList
                [ sessionId
                | taskId <- Map.keys state.adapterRunning
                , Just task <- [Map.lookup taskId state.adapterTasks]
                , Just sessionId <- [task.sessionId]
                ]
        candidates =
            [ ( TaskIdentity task.taskId.unTaskId task.sessionId
              , taskId
              )
            | taskId <- toList state.adapterPending
            , Just task <- [Map.lookup taskId state.adapterTasks]
            ]
        capacity = state.adapterLimit - Map.size state.adapterRunning
        (selected, remaining) =
            selectRunnableTasks capacity activeSessions candidates
    (tasks, running) <-
        foldM launch (state.adapterTasks, state.adapterRunning) (map snd selected)
    pure
        state
            { adapterPending = Seq.fromList (map snd remaining)
            , adapterRunning = running
            , adapterTasks = tasks
            }
  where
    launch (tasks, running) taskId =
        case Map.lookup taskId tasks of
            Nothing -> pure (tasks, running)
            Just task -> do
                now <- getCurrentTime
                let started = task {status = TaskRunning, updatedAt = now}
                persisted <- persistBounded journal started
                let executionTask =
                        case Map.lookup taskId state.adapterInputs of
                            Nothing -> persisted
                            Just input ->
                                input
                                    { status = TaskRunning
                                    , attempt = persisted.attempt
                                    , updatedAt = persisted.updatedAt
                                    , logTail = persisted.logTail
                                    }
                worker <- launchWorker commands runner executionTask
                let runningTask =
                        RunningTask
                            { runningAttempt = persisted.attempt
                            , runningWorker = worker
                            }
                atomically $ modifyTVar' registry (Map.insert taskId runningTask)
                pure
                    ( Map.insert taskId persisted tasks
                    , Map.insert taskId runningTask running
                    )

launchWorker :: TBQueue AdapterMessage -> TaskRunner -> DurableTask -> IO (Async ())
launchWorker commands runner task =
    mask $ \_ -> do
        gate <- newEmptyMVar
        worker <- asyncWithUnmask $ \unmask -> do
            takeMVar gate
            let finished outcome =
                    atomically $
                        writeTBQueue commands
                            (TaskFinished task.taskId task.attempt outcome)
            result <-
                tryAny
                    ( unmask $
                        runner.runTask task $ \line ->
                            atomically $
                                writeTBQueue commands
                                    (TaskLogged task.taskId task.attempt line)
                    )
            case result of
                Left exception
                    | isAsyncException exception -> throwIO exception
                    | otherwise ->
                        finished (Left (Text.pack (show exception)))
                Right value -> finished value
        putMVar gate ()
        pure worker

shutdownRegistry :: TVar (Map TaskId RunningTask) -> IO ()
shutdownRegistry registry = do
    running <- atomically $ do
        current <- readTVar registry
        writeTVar registry Map.empty
        pure (Map.elems current)
    mapM_ (cancel . (.runningWorker)) running
    mapM_ (waitCatch . (.runningWorker)) running

taskResult :: Text -> DurableTask -> Value
taskResult state task =
    object
        [ "version" .= (1 :: Int)
        , "state" .= state
        , "task" .= task
        ]

persistBounded :: Journal -> DurableTask -> IO DurableTask
persistBounded journal task = do
    event <- persistTask journal task
    case fromJSON event.payload of
        Success persisted -> pure persisted
        Error message -> fail ("journal returned an invalid task event: " <> message)

parseTaskCommand :: Value -> Either Text TaskCommand
parseTaskCommand value =
    case parseEither parser value of
        Left message -> Left (Text.pack message)
        Right command -> Right command
  where
    parser = withObject "daemon task command" $ \objectValue -> do
        version <- objectValue .: "version"
        unless (version == (1 :: Int)) (fail "unsupported task command version")
        commandType <- objectValue .: "type"
        case (commandType :: Text) of
            "submit" -> Submit <$> parseSubmit objectValue
            "cancel" -> Cancel <$> (objectValue .: "task_id" >>= validTaskId)
            "list" -> pure List
            "set_limit" -> do
                limit <- objectValue .: "limit"
                unless (limit >= 1 && limit <= maximumTaskLimit) $
                    fail "limit must be between 1 and 32"
                pure (SetLimit limit)
            "approval" ->
                Approval
                    <$> (objectValue .: "task_id" >>= validTaskId)
                    <*> (objectValue .: "approval_id" >>= boundedIdentifier "approval_id" 256)
                    <*> (objectValue .: "decision" >>= validDecision)
            "retry" -> Retry <$> (objectValue .: "task_id" >>= validTaskId)
            _ -> fail "unknown task command type"

    parseSubmit objectValue = do
        submittedId <- objectValue .: "task_id" >>= validTaskId
        submittedSessionId <-
            objectValue .:? "session_id"
                >>= traverse (boundedIdentifier "session_id" 256)
        submittedPrompt <- objectValue .: "prompt" >>= boundedText "prompt" 8_192
        submittedWorkingDirectory <-
            objectValue .: "cwd" >>= boundedString "cwd" 4_096
        submittedProvider <-
            objectValue .:? "provider"
                >>= traverse (boundedText "provider" 128)
        submittedModel <-
            objectValue .:? "model"
                >>= traverse (boundedText "model" 256)
        submittedEffort <-
            objectValue .:? "effort"
                >>= traverse (boundedText "effort" 64)
        submittedWorktree <- objectValue .:? "worktree" .!= False
        when (submittedWorktree && submittedSessionId /= Nothing) $
            fail "worktree may only be used for a new session"
        when ((submittedProvider == Nothing) /= (submittedModel == Nothing)) $
            fail "provider and model must be supplied together"
        pure SubmitCommand {..}

validTaskId :: Text -> Parser TaskId
validTaskId raw = TaskId <$> boundedIdentifier "task_id" 256 raw

boundedIdentifier :: String -> Int -> Text -> Parser Text
boundedIdentifier label maximumLength raw = do
    value <- boundedText label maximumLength raw
    when (Text.any isControl value) (fail (label <> " contains control characters"))
    pure value

boundedText :: String -> Int -> Text -> Parser Text
boundedText label maximumLength raw = do
    let value = Text.strip raw
    when (Text.null value) (fail (label <> " must not be empty"))
    when (Text.length value > maximumLength) (fail (label <> " is too long"))
    pure value

boundedString :: String -> Int -> String -> Parser String
boundedString label maximumLength raw = do
    when (null raw) (fail (label <> " must not be empty"))
    when (length raw > maximumLength) (fail (label <> " is too long"))
    pure raw

validDecision :: Text -> Parser Text
validDecision raw = do
    decision <- boundedText "decision" 32 raw
    unless (decision `elem` ["approve", "deny", "approve_session"]) $
        fail "decision must be approve, deny, or approve_session"
    pure decision

runProcessTask :: FilePath -> DurableTask -> (Text -> IO ()) -> IO (Either Text ())
runProcessTask executable task logLine = do
    let arguments =
            ["--prompt", Text.unpack task.description, "--save-session"]
                <> maybe [] (\value -> ["--resume", Text.unpack value]) task.sessionId
                <> ["--cwd", task.workingDirectory]
                <> maybe [] (\value -> ["--provider", Text.unpack value]) task.provider
                <> maybe [] (\value -> ["--model", Text.unpack value]) task.model
                <> maybe [] (\value -> ["--effort", Text.unpack value]) task.effort
                <> ["--worktree" | task.worktree]
        configuration =
            (proc executable arguments)
                { std_out = CreatePipe
                , std_err = CreatePipe
                }
    (stdoutHandle, stderrHandle, process) <-
        createProcess configuration >>= \case
            (_, Just stdoutHandle, Just stderrHandle, process) ->
                pure (stdoutHandle, stderrHandle, process)
            _ -> fail "failed to capture agent-cli output"
    let terminate = terminateProcess process >> voidWait process
        consume handle = do
            hSetEncoding handle utf8
            let go = do
                    ended <- hIsEOF handle
                    unless ended (Text.hGetLine handle >>= logLine >> go)
            go
    (concurrently_ (consume stdoutHandle) (consume stderrHandle) >> waitForProcess process)
        `onException` terminate
        >>= \case
            ExitSuccess -> pure (Right ())
            ExitFailure code ->
                pure (Left ("agent-cli exited with status " <> Text.pack (show code)))

voidWait :: ProcessHandle -> IO ()
voidWait process = do
    _ <- waitForProcess process
    pure ()
