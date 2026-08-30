module Agent.Runtime.Daemon.TaskAdapter
    ( TaskRunner(..)
    , processTaskRunner
    , processTaskRunnerFor
    , processTaskRunnerForWithTimeout
    , processTaskArguments
    , withTaskAdapter
    , withTaskAdapterQueueSize
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
    ( IOException
    , catch
    , finally
    , isAsyncException
    , mask
    , onException
    , throwIO
    , tryAny
    )
import Control.Monad (foldM, forM_, unless, when)
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString as BS
import Data.Char (isControl)
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time (getCurrentTime)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO (Handle, hClose)
import System.Posix.Signals (sigKILL, sigTERM, signalProcessGroup)
import System.Process
import System.Timeout (timeout)

import Agent.Runtime.Daemon.Journal
import Agent.Runtime.Daemon.Protocol (EventEnvelope (..))
import Agent.Runtime.Daemon.Supervisor
import Agent.Runtime.Daemon.Task
import Agent.Runtime.Daemon.TaskScheduler

data TaskRunner = TaskRunner
    { runTask :: DurableTask -> (Text -> IO ()) -> IO (Either Text ())
    }

-- | Execute a submitted turn through the installed @agent-cli@ executable.
-- Set @HASKELL_AGENT_CLI@ to select another executable. The process receives
-- arguments directly (never through a shell), and its bounded persisted log is
-- populated from stdout and stderr.
processTaskRunner :: IO TaskRunner
processTaskRunner = do
    executable <- maybe "agent-cli" id <$> lookupEnv "HASKELL_AGENT_CLI"
    pure (processTaskRunnerFor executable)

processTaskRunnerFor :: FilePath -> TaskRunner
processTaskRunnerFor executable =
    processTaskRunnerForWithTimeout defaultTaskRuntimeSeconds executable

processTaskRunnerForWithTimeout :: Int -> FilePath -> TaskRunner
processTaskRunnerForWithTimeout runtimeSeconds executable =
    TaskRunner
        { runTask = runProcessTask runtimeSeconds executable
        }

defaultTaskRuntimeSeconds :: Int
defaultTaskRuntimeSeconds = 6 * 60 * 60

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
    = Execute !TaskCommand !(TMVar (Either Text Value)) !(TVar Bool)
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
withTaskAdapter = withTaskAdapterQueueSize 1_024

withTaskAdapterQueueSize ::
    Int ->
    Journal ->
    TaskRunner ->
    (Supervisor -> IO value) ->
    IO value
withTaskAdapterQueueSize rawQueueSize journal runner action = do
    saved <- snapshot journal
    commands <- newTBQueueIO (fromIntegral (max 1 rawQueueSize))
    completions <- newTQueueIO
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
                            active <- newTVarIO True
                            admitted <-
                                atomically $ do
                                    full <- isFullTBQueue commands
                                    if full
                                        then pure False
                                        else do
                                            writeTBQueue commands (Execute command reply active)
                                            pure True
                            if admitted
                                then
                                    atomically (takeTMVar reply)
                                        `onException` atomically (writeTVar active False)
                                else pure (Left "task scheduler command queue is full")
                }
    workers <- newTVarIO Map.empty
    let loop =
            schedulerLoopWithRegistry
                journal runner commands completions workers initial
    ( race loop (action supervisor) >>= \case
        Left () -> fail "task scheduler stopped unexpectedly"
        Right value -> pure value
      ) `finally` shutdownRegistry workers

schedulerLoopWithRegistry
    :: Journal
    -> TaskRunner
    -> TBQueue AdapterMessage
    -> TQueue AdapterMessage
    -> TVar (Map TaskId RunningTask)
    -> AdapterState
    -> IO ()
schedulerLoopWithRegistry journal runner commands completions registry = go
  where
    go state0 = do
        state <- startRunnable journal runner commands completions registry state0
        message <-
            atomically $
                readTQueue completions `orElse` readTBQueue commands
        handleMessage state message >>= go

    handleMessage state = \case
        Execute command reply active -> do
            stillActive <- readTVarIO active
            if stillActive
                then do
                    (next, result) <-
                        executeCommand journal registry state command
                    atomically (putTMVar reply result)
                    pure next
                else pure state
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
    -> TVar (Map TaskId RunningTask)
    -> AdapterState
    -> TaskCommand
    -> IO (AdapterState, Either Text Value)
executeCommand journal registry state = \case
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
    Approval taskId _ _ ->
        case Map.lookup taskId state.adapterTasks of
            Just task | task.status == TaskRunning ->
                pure
                    ( state
                    , Left
                        ( "interactive approval resolution is unsupported by "
                            <> "the daemon task adapter"
                        )
                    )
            _ -> pure (state, Left "task is not running")
    Retry taskId ->
        case Map.lookup taskId state.adapterTasks of
            Nothing -> pure (state, Left "task id is unknown")
            Just task
                | isActive task ->
                    pure (state, Left "active tasks cannot be retried")
                | task.status == TaskCompleted ->
                    pure (state, Left "completed tasks cannot be retried")
                | Map.notMember taskId state.adapterInputs ->
                    pure
                        ( state
                        , Left
                            ( "task input is unavailable after daemon restart; "
                                <> "submit a new task id"
                            )
                        )
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
    -> TQueue AdapterMessage
    -> TVar (Map TaskId RunningTask)
    -> AdapterState
    -> IO AdapterState
startRunnable journal runner commands completions registry state = do
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
                worker <- launchWorker commands completions runner executionTask
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

launchWorker ::
    TBQueue AdapterMessage ->
    TQueue AdapterMessage ->
    TaskRunner ->
    DurableTask ->
    IO (Async ())
launchWorker commands completions runner task =
    mask $ \_ -> do
        gate <- newEmptyMVar
        worker <- asyncWithUnmask $ \unmask -> do
            takeMVar gate
            let finished outcome =
                    atomically $
                        writeTQueue completions
                            (TaskFinished task.taskId task.attempt outcome)
                logged line =
                    atomically $ do
                        full <- isFullTBQueue commands
                        unless full $
                            writeTBQueue commands
                                (TaskLogged task.taskId task.attempt line)
            result <-
                tryAny
                    ( unmask $
                        runner.runTask task logged
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
    when ('\0' `elem` raw) (fail (label <> " contains a NUL byte"))
    pure raw

validDecision :: Text -> Parser Text
validDecision raw = do
    decision <- boundedText "decision" 32 raw
    unless (decision `elem` ["approve", "deny", "approve_session"]) $
        fail "decision must be approve, deny, or approve_session"
    pure decision

runProcessTask :: Int -> FilePath -> DurableTask -> (Text -> IO ()) -> IO (Either Text ())
runProcessTask runtimeSeconds executable task logLine = do
    let arguments = processTaskArguments task
        configuration =
            (proc executable arguments)
                { std_out = CreatePipe
                , std_err = CreatePipe
                , create_group = True
                , close_fds = True
                }
    (stdoutHandle, stderrHandle, process) <-
        createProcess configuration >>= \case
            (_, Just stdoutHandle, Just stderrHandle, process) ->
                pure (stdoutHandle, stderrHandle, process)
            _ -> fail "failed to capture agent-cli output"
    let terminate = terminateProcessGroup process
        consume = consumeBoundedOutput logLine
        closeHandles = hClose stdoutHandle `finally` hClose stderrHandle
    outcome <-
        timeout
            (max 1 runtimeSeconds * 1_000_000)
            ( ((concurrently_ (consume stdoutHandle) (consume stderrHandle) >> waitForProcess process)
                `onException` terminate)
                `finally` closeHandles
            )
    case outcome of
        Nothing ->
            pure
                (Left ("agent-cli exceeded its " <> Text.pack (show runtimeSeconds) <> " second runtime limit"))
        Just ExitSuccess -> pure (Right ())
        Just (ExitFailure code) ->
            pure (Left ("agent-cli exited with status " <> Text.pack (show code)))

processTaskArguments :: DurableTask -> [String]
processTaskArguments task =
    ["--prompt", Text.unpack task.description, "--save-session", "--no-yolo"]
        <> maybe [] (\value -> ["--resume", Text.unpack value]) task.sessionId
        <> ["--cwd", task.workingDirectory]
        <> maybe [] (\value -> ["--provider", Text.unpack value]) task.provider
        <> maybe [] (\value -> ["--model", Text.unpack value]) task.model
        <> maybe [] (\value -> ["--effort", Text.unpack value]) task.effort
        <> ["--worktree" | task.worktree]

consumeBoundedOutput :: (Text -> IO ()) -> Handle -> IO ()
consumeBoundedOutput logChunk handle = go 0 BS.empty
  where
    go total pending = do
        bytes <- BS.hGetSome handle 4_096
        let nextTotal = total + BS.length bytes
        when (nextTotal > maxTaskOutputBytes) $
            ioError (userError "daemon task output exceeded 64 MiB")
        let buffered = pending <> bytes
        if BS.null bytes
            then unless (BS.null buffered) (emit buffered)
            else drain buffered >>= go nextTotal
    drain buffered =
        let candidate = BS.take 4_096 buffered
         in case BS.elemIndex 10 candidate of
                Just newline -> do
                    emit (BS.take newline buffered)
                    drain (BS.drop (newline + 1) buffered)
                Nothing
                    | BS.length buffered > 4_096 -> do
                        emit candidate
                        drain (BS.drop 4_096 buffered)
                    | otherwise -> pure buffered
    emit = logChunk . TextEncoding.decodeUtf8With lenientDecode

maxTaskOutputBytes :: Int
maxTaskOutputBytes = 64 * 1024 * 1024

terminateProcessGroup :: ProcessHandle -> IO ()
terminateProcessGroup process = do
    signalGroup sigTERM
    timeout 2_000_000 (waitForProcess process) >>= \case
        Just _ -> pure ()
        Nothing -> do
            signalGroup sigKILL
            voidWait process
  where
    signalGroup signal =
        getPid process >>= \case
            Just pid ->
                signalProcessGroup signal (fromIntegral pid)
                    `catch` \(_ :: IOException) -> pure ()
            Nothing ->
                terminateProcess process
                    `catch` \(_ :: IOException) -> pure ()

voidWait :: ProcessHandle -> IO ()
voidWait process = do
    _ <- waitForProcess process
    pure ()
