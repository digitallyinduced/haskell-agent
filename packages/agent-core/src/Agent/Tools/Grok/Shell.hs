-- | Persistent Grok shell session: replay cwd/env, background tasks.
--
-- v1 emulates grok-build's local terminal by wrapping each command so it
-- starts in the last cwd, sources the last @export -p@ dump, then writes
-- those files again. No PTY.
module Agent.Tools.Grok.Shell
    ( GrokSession
    , grokSessionEnv
    , grokSessionBackgroundTaskCount
    , newGrokSession
    , newGrokSessionWithCloseAction
    , closeGrokSession
    , runForegroundStreaming
    , startBackground
    , readTaskOutput
    , killTask
    , hasUnwaitedBackgroundOp
    ) where

import Agent.OsPath (fromText, unsafeToFilePath)
import Agent.ResourceScope
    ( ResourceKey
    , ResourceScope
    , allocateResource
    , closeResourceScope
    , newResourceScope
    , releaseResource
    )
import Agent.Tools.IO
    ( CommandResult(..)
    , RunningCommand(..)
    , resolveUnderCwd
    , runShellCommandStreaming
    , runningLiveOutput
    , startShellCommand
    , stopShellCommand
    )
import Agent.Tools.Types (ToolEnv(..))
import Control.Concurrent
    ( ThreadId
    , forkIOWithUnmask
    , myThreadId
    , threadDelay
    )
import Control.Concurrent.Async (race)
import Control.Concurrent.MVar
import Control.Concurrent.STM
    ( STM
    , TMVar
    , TVar
    , atomically
    , modifyTVar'
    , newEmptyTMVar
    , newTVarIO
    , readTMVar
    , readTVar
    , retry
    , tryPutTMVar
    , writeTVar
    )
import Control.Exception.Safe
    ( SomeAsyncException
    , SomeException
    , catchAsync
    , finally
    , mask
    , onException
    , throwIO
    , toException
    , tryAny
    )
import Control.Monad (void)
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory.OsPath
    ( doesDirectoryExist
    , doesFileExist
    , getTemporaryDirectory
    , removeFile
    )
import System.IO (hClose)
import System.OsPath (OsPath, unsafeEncodeUtf, (<.>), (</>))
import System.Posix.Files (ownerReadMode, ownerWriteMode, setFileMode, unionFileModes)
import System.Posix.Temp (mkstemp)

data PersistentShell = PersistentShell
    { shellCwd :: !OsPath
    , shellEnvFile :: !OsPath
    }

data BackgroundTask = BackgroundTask
    { backgroundId :: !Text
    , backgroundRunning :: !RunningCommand
    , backgroundResource :: !ResourceKey
    }

-- Operations admitted while open retain the shared shell and task resources
-- until they return. Closing rejects new callers and transfers cleanup to a
-- tracked worker so cancellation of whichever caller initiated close cannot
-- strand the session halfway through shutdown.
--
-- Per-thread depths preserve nested ownership. Synchronously closing from
-- inside an admitted operation or streaming callback is rejected instead of
-- deadlocking on itself.
--
-- Task-output readers are observers rather than critical operations. Cleanup
-- first drains commands that can mutate shell/task ownership, then stops
-- background resources so long-polling readers wake, and only then drains
-- those readers and forgets the task map.
data GrokOperationKind
    = GrokCriticalOperation
    | GrokObserverOperation
    | GrokCallbackOperation

data GrokActiveOperations = GrokActiveOperations
    !(Map ThreadId Int)
    !(Map ThreadId Int)
    !(Map ThreadId Int)

data GrokAdmission
    = GrokAdmitted
    | GrokRejected !Text

data GrokSessionState
    = GrokSessionOpen !GrokActiveOperations
    | GrokSessionClosing
        !GrokActiveOperations
        !(TMVar GrokCloseOutcome)
        !(Maybe ThreadId)
    | GrokSessionCleaning
        !ThreadId
        !GrokActiveOperations
        !(TMVar GrokCloseOutcome)
    | GrokSessionClosed !(Either SomeException ())

data GrokCloseOutcome
    = GrokCloseComplete !(Either SomeException ())
    | GrokCloseRetry

data GrokCloseDecision
    = GrokCloseLeader !(TMVar GrokCloseOutcome)
    | GrokCloseFollower !(TMVar GrokCloseOutcome)
    | GrokCloseFinished !(Either SomeException ())
    | GrokCloseFromOperation

data GrokSession = GrokSession
    { grokEnv :: !ToolEnv
    , grokShell :: !(MVar PersistentShell)
    , grokTasks :: !(MVar (Map Text BackgroundTask))
    , grokNextId :: !(IORef Int)
    , grokResources :: !ResourceScope
    , grokState :: !(TVar GrokSessionState)
    , grokCloseAction :: !(IO ())
    }

grokSessionEnv :: GrokSession -> ToolEnv
grokSessionEnv session = session.grokEnv

grokSessionBackgroundTaskCount :: GrokSession -> IO Int
grokSessionBackgroundTaskCount session =
    withMVar session.grokTasks (pure . Map.size)

newGrokSession :: ToolEnv -> IO GrokSession
newGrokSession env =
    newGrokSessionWithCloseAction env (pure ())

-- | Build a session with an additional host-owned shutdown action. The action
-- runs exactly once inside the lifecycle close worker; all close callers share
-- its result, and the session remains terminal if it fails.
newGrokSessionWithCloseAction :: ToolEnv -> IO () -> IO GrokSession
newGrokSessionWithCloseAction env closeAction = do
    resources <- newResourceScope
    flip onException (closeResourceScope resources) do
        (_, envFile) <- allocateResource resources
            (acquirePrivateFile "agent-grok-env")
            cleanupEnvFiles
        shell <- newMVar PersistentShell
            { shellCwd = env.toolCwd
            , shellEnvFile = envFile
            }
        tasks <- newMVar Map.empty
        nextId <- newIORef 0
        state <- newTVarIO $
            GrokSessionOpen
                (GrokActiveOperations Map.empty Map.empty Map.empty)
        pure GrokSession
            { grokEnv = env
            , grokShell = shell
            , grokTasks = tasks
            , grokNextId = nextId
            , grokResources = resources
            , grokState = state
            , grokCloseAction = closeAction
            }

-- | Delete the env/cwd dump and interrupt leftover background tasks.
-- Call this when the CLI/session ends, including after exceptions.
closeGrokSession :: GrokSession -> IO ()
closeGrokSession session =
    mask \restore -> do
        owner <- myThreadId
        let close =
                atomically (beginGrokClose session owner) >>= \case
                    GrokCloseFinished result ->
                        either throwIO pure result
                    GrokCloseFromOperation ->
                        throwIO $ userError
                            "cannot close a Grok session from one of its active operations"
                    GrokCloseFollower done ->
                        awaitClose done
                    GrokCloseLeader done -> do
                        void
                            (startGrokCloseWorker session)
                            `onException`
                                atomically (rollbackGrokClose session)
                        awaitClose done

            awaitClose done =
                restore (atomically (readTMVar done)) >>= \case
                    GrokCloseComplete result ->
                        either throwIO pure result
                    GrokCloseRetry ->
                        close
        close

runForegroundStreaming
    :: GrokSession
    -> String
    -> Int
    -> (Text -> Text -> IO ())
    -> IO CommandResult
runForegroundStreaming session command timeoutMs onSnapshot = do
    result <- withGrokOperation GrokCriticalOperation session $
        runForegroundStreamingOpen session command timeoutMs onSnapshot
    pure (either grokFailedCommandResult id result)

runForegroundStreamingOpen
    :: GrokSession
    -> String
    -> Int
    -> (Text -> Text -> IO ())
    -> IO CommandResult
runForegroundStreamingOpen session command timeoutMs onSnapshot =
    modifyMVar session.grokShell \shell -> do
        let wrapped = bashWrap (wrapScript shell True command)
        result <- runShellCommandStreaming
            session.grokEnv
            session.grokEnv.toolCwd
            wrapped
            timeoutMs
            (\out err -> withGrokCallback session (onSnapshot out err))
        next <- if result.commandTimedOut || result.commandCancelled
            then pure shell
            else refreshCwd session.grokEnv shell
        pure (next, result)

startBackground :: GrokSession -> Text -> IO (Either Text Text)
startBackground session command = do
    result <- withGrokOperation GrokCriticalOperation session do
        -- Capture cwd and environment while excluding foreground updates, then
        -- make the background wrapper source its own immutable file. Merely
        -- copying the shared filename is not enough: startShellCommand returns
        -- before the child is guaranteed to have sourced it, so a foreground
        -- command could otherwise rewrite the file first.
        started <- tryAny $ withMVar session.grokShell \shell ->
            allocateResource session.grokResources
                (acquireBackground shell)
                cleanupBackground
        case started of
            Left exception ->
                pure (Left (Text.pack (show exception)))
            Right (resource, (running, _snapshotFile)) -> do
                taskId <- nextTaskId session
                let task = BackgroundTask
                        { backgroundId = taskId
                        , backgroundRunning = running
                        , backgroundResource = resource
                        }
                modifyMVar_ session.grokTasks
                    (\tasks -> pure (Map.insert taskId task tasks))
                    `onException` releaseResource resource
                pure $ Right $
                    "Command moved to background.\n\
                    \task_id: " <> taskId <> "\n\
                    \Use get_task_output to read output. Do not poll in a loop."
    pure (either Left id result)
  where
    acquireBackground shell =
        mask \restore -> do
            snapshotFile <- restore (acquirePrivateFile "agent-grok-env-bg")
            let cleanupSnapshot = cleanupEnvFiles snapshotFile
            flip onException cleanupSnapshot do
                restore $
                    Text.readFile (unsafeToFilePath shell.shellEnvFile)
                        >>= Text.writeFile (unsafeToFilePath snapshotFile)
                let snapshotShell = shell { shellEnvFile = snapshotFile }
                    wrapped =
                        bashWrap
                            (wrapScript snapshotShell False (Text.unpack command))
                running <- restore
                    (startShellCommand
                        session.grokEnv
                        session.grokEnv.toolCwd
                        wrapped)
                    >>= either (throwIO . userError . Text.unpack) pure
                pure (running, snapshotFile)

    cleanupBackground (running, snapshotFile) =
        stopShellCommand running `finally` cleanupEnvFiles snapshotFile

readTaskOutput :: GrokSession -> Text -> Maybe Int -> IO Text
readTaskOutput session taskId timeoutMs = do
    result <- withGrokOperation GrokObserverOperation session do
        tasks <- readMVar session.grokTasks
        case Map.lookup taskId tasks of
            Nothing -> pure $ "Unknown task_id: " <> taskId
            Just task -> case timeoutMs of
                Nothing -> snapshotTask task
                Just ms -> do
                    raced <- race
                        (threadDelay (max 1 ms * 1000))
                        (readMVar task.backgroundRunning.runningResult)
                    case raced of
                        Left () -> snapshotTask task
                        Right result -> pure (formatExit result)
    pure (either id id result)

snapshotTask :: BackgroundTask -> IO Text
snapshotTask task =
    tryReadMVar task.backgroundRunning.runningResult >>= \case
        Just result -> pure (formatExit result)
        Nothing -> do
            (out, err) <- runningLiveOutput task.backgroundRunning
            let body = combinePipes out err
            pure $ if Text.null body
                then "still running"
                else "still running\n" <> body

killTask :: GrokSession -> Text -> IO Text
killTask session taskId = do
    result <- withGrokOperation GrokCriticalOperation session do
        tasks <- readMVar session.grokTasks
        case Map.lookup taskId tasks of
            Nothing -> pure $ "Unknown task_id: " <> taskId
            Just task -> do
                releaseResource task.backgroundResource
                result <- readMVar task.backgroundRunning.runningResult
                pure $ "killed " <> taskId <> "\n" <> formatExit result
    pure (either id id result)

withGrokOperation
    :: GrokOperationKind
    -> GrokSession
    -> IO a
    -> IO (Either Text a)
withGrokOperation kind session action =
    mask \restore -> do
        owner <- myThreadId
        admission <- atomically (beginGrokOperation kind session owner)
        case admission of
            GrokRejected reason ->
                pure (Left reason)
            GrokAdmitted ->
                Right <$> restore action
                    `finally`
                        atomically (finishGrokOperation kind session owner)

withGrokCallback :: GrokSession -> IO () -> IO ()
withGrokCallback session action =
    mask \restore -> do
        owner <- myThreadId
        admission <- atomically (beginGrokCallback session owner)
        case admission of
            GrokRejected _ ->
                pure ()
            GrokAdmitted ->
                restore action
                    `finally`
                        atomically
                            (finishGrokOperation
                                GrokCallbackOperation
                                session
                                owner)

beginGrokOperation
    :: GrokOperationKind
    -> GrokSession
    -> ThreadId
    -> STM GrokAdmission
beginGrokOperation kind session owner =
    readTVar session.grokState >>= \case
        GrokSessionOpen active ->
            case kind of
                GrokCriticalOperation
                    | isCallbackOwner owner active ->
                        pure (GrokRejected grokCallbackReentryText)
                _ -> do
                    writeTVar session.grokState $
                        GrokSessionOpen (incrementActive kind owner active)
                    pure GrokAdmitted
        GrokSessionClosing{} ->
            pure (GrokRejected grokSessionClosedText)
        GrokSessionCleaning{} ->
            pure (GrokRejected grokSessionClosedText)
        GrokSessionClosed{} ->
            pure (GrokRejected grokSessionClosedText)

beginGrokCallback :: GrokSession -> ThreadId -> STM GrokAdmission
beginGrokCallback session owner =
    readTVar session.grokState >>= \case
        GrokSessionOpen active -> do
            writeTVar session.grokState $
                GrokSessionOpen
                    (incrementActive GrokCallbackOperation owner active)
            pure GrokAdmitted
        GrokSessionClosing active done worker
            | hasCriticalOperations active -> do
                writeTVar session.grokState $
                    GrokSessionClosing
                        (incrementActive GrokCallbackOperation owner active)
                        done
                        worker
                pure GrokAdmitted
            | otherwise ->
                pure (GrokRejected grokSessionClosedText)
        GrokSessionCleaning{} ->
            pure (GrokRejected grokSessionClosedText)
        GrokSessionClosed{} ->
            pure (GrokRejected grokSessionClosedText)

finishGrokOperation
    :: GrokOperationKind
    -> GrokSession
    -> ThreadId
    -> STM ()
finishGrokOperation kind session owner =
    modifyTVar' session.grokState \case
        GrokSessionOpen active ->
            GrokSessionOpen (decrementActive kind owner active)
        GrokSessionClosing active done worker ->
            GrokSessionClosing
                (decrementActive kind owner active)
                done
                worker
        GrokSessionCleaning cleaningOwner active done ->
            GrokSessionCleaning
                cleaningOwner
                (decrementActive kind owner active)
                done
        state ->
            state

beginGrokClose :: GrokSession -> ThreadId -> STM GrokCloseDecision
beginGrokClose session owner =
    readTVar session.grokState >>= \case
        GrokSessionOpen active
            | isActiveOwner owner active ->
                pure GrokCloseFromOperation
            | otherwise -> do
                done <- newEmptyTMVar
                writeTVar session.grokState $
                    GrokSessionClosing active done Nothing
                pure (GrokCloseLeader done)
        GrokSessionClosing active done _
            | isActiveOwner owner active ->
                pure GrokCloseFromOperation
            | otherwise ->
                pure (GrokCloseFollower done)
        GrokSessionCleaning cleaningOwner active done
            | isActiveOwner owner active ->
                pure GrokCloseFromOperation
            | cleaningOwner == owner ->
                pure (GrokCloseFinished (Right ()))
            | otherwise ->
                pure (GrokCloseFollower done)
        GrokSessionClosed result ->
            pure (GrokCloseFinished result)

beginGrokCleaning :: GrokSession -> ThreadId -> STM (TMVar GrokCloseOutcome)
beginGrokCleaning session owner =
    readTVar session.grokState >>= \case
        GrokSessionClosing active done (Just worker)
            | worker /= owner || hasCriticalOperations active ->
                retry
            | otherwise ->
                writeTVar session.grokState
                    (GrokSessionCleaning owner active done)
                    >> pure done
        _ ->
            retry

waitForGrokObservers :: GrokSession -> STM ()
waitForGrokObservers session =
    readTVar session.grokState >>= \case
        GrokSessionCleaning _ active _
            | hasObserverOperations active ->
                retry
            | otherwise ->
                pure ()
        _ ->
            pure ()

startGrokCloseWorker :: GrokSession -> IO ThreadId
startGrokCloseWorker session =
    mask \_restore -> do
        start <- newEmptyMVar
        worker <- forkIOWithUnmask \unmask -> do
            takeMVar start
            unmask (runGrokCleanup session)
        atomically $
            modifyTVar' session.grokState \case
                GrokSessionClosing active done Nothing ->
                    GrokSessionClosing active done (Just worker)
                state ->
                    state
        putMVar start ()
        pure worker

rollbackGrokClose :: GrokSession -> STM ()
rollbackGrokClose session =
    readTVar session.grokState >>= \case
        GrokSessionClosing active done Nothing -> do
            writeTVar session.grokState (GrokSessionOpen active)
            void (tryPutTMVar done GrokCloseRetry)
        _ ->
            pure ()

runGrokCleanup :: GrokSession -> IO ()
runGrokCleanup session =
    mask \restore -> do
        owner <- myThreadId
        done <- atomically (beginGrokCleaning session owner)
        resourceResult <- completeDespiteAsync $
            restore (closeResourceScope session.grokResources)
        closeActionResult <- captureAllExceptions $
            restore session.grokCloseAction
        observerResult <- completeDespiteAsync $
            restore (atomically (waitForGrokObservers session))
        taskResult <- completeDespiteAsync $
            restore (modifyMVar_ session.grokTasks (const (pure Map.empty)))
        let result =
                resourceResult
                    >> closeActionResult
                    >> observerResult
                    >> taskResult
        atomically do
            writeTVar session.grokState (GrokSessionClosed result)
            void (tryPutTMVar done (GrokCloseComplete result))

-- Async interruption is remembered for the shared close result, but the
-- invariant-establishing action is retried to completion before Closed is
-- published. This matters for the observer barrier and task-map clear, and
-- lets ResourceScope rejoin its own detached cleanup worker after cancellation.
completeDespiteAsync :: IO a -> IO (Either SomeException a)
completeDespiteAsync action = go Nothing
  where
    go firstException =
        (tryAny action >>= \case
            Right value ->
                pure $ case firstException of
                    Nothing -> Right value
                    Just exception -> Left exception
            Left exception ->
                pure $ Left $ case firstException of
                    Nothing -> exception
                    Just first -> first)
            `catchAsync` \(exception :: SomeAsyncException) ->
                go $ case firstException of
                    Nothing -> Just (toException exception)
                    Just first -> Just first

captureAllExceptions :: IO a -> IO (Either SomeException a)
captureAllExceptions action =
    tryAny action
        `catchAsync` \(exception :: SomeAsyncException) ->
            pure (Left (toException exception))

incrementOwner :: ThreadId -> Map ThreadId Int -> Map ThreadId Int
incrementOwner owner =
    Map.insertWith (+) owner 1

decrementOwner :: ThreadId -> Map ThreadId Int -> Map ThreadId Int
decrementOwner owner active =
    case Map.lookup owner active of
        Just depth
            | depth > 1 ->
                Map.insert owner (depth - 1) active
        Just _ ->
            Map.delete owner active
        Nothing ->
            active

incrementActive
    :: GrokOperationKind
    -> ThreadId
    -> GrokActiveOperations
    -> GrokActiveOperations
incrementActive
    kind
    owner
    (GrokActiveOperations critical observers callbacks) =
    case kind of
        GrokCriticalOperation ->
            GrokActiveOperations
                (incrementOwner owner critical)
                observers
                callbacks
        GrokObserverOperation ->
            GrokActiveOperations
                critical
                (incrementOwner owner observers)
                callbacks
        GrokCallbackOperation ->
            GrokActiveOperations
                critical
                observers
                (incrementOwner owner callbacks)

decrementActive
    :: GrokOperationKind
    -> ThreadId
    -> GrokActiveOperations
    -> GrokActiveOperations
decrementActive
    kind
    owner
    (GrokActiveOperations critical observers callbacks) =
    case kind of
        GrokCriticalOperation ->
            GrokActiveOperations
                (decrementOwner owner critical)
                observers
                callbacks
        GrokObserverOperation ->
            GrokActiveOperations
                critical
                (decrementOwner owner observers)
                callbacks
        GrokCallbackOperation ->
            GrokActiveOperations
                critical
                observers
                (decrementOwner owner callbacks)

hasCriticalOperations :: GrokActiveOperations -> Bool
hasCriticalOperations (GrokActiveOperations critical _ callbacks) =
    not (Map.null critical && Map.null callbacks)

hasObserverOperations :: GrokActiveOperations -> Bool
hasObserverOperations (GrokActiveOperations _ observers _) =
    not (Map.null observers)

isActiveOwner :: ThreadId -> GrokActiveOperations -> Bool
isActiveOwner
    owner
    (GrokActiveOperations critical observers callbacks) =
        Map.member owner critical
            || Map.member owner observers
            || Map.member owner callbacks

isCallbackOwner :: ThreadId -> GrokActiveOperations -> Bool
isCallbackOwner owner (GrokActiveOperations _ _ callbacks) =
    Map.member owner callbacks

grokSessionClosedText :: Text
grokSessionClosedText = "Grok session is closed."

grokCallbackReentryText :: Text
grokCallbackReentryText =
    "Grok shell operations cannot be started from a streaming callback."

grokFailedCommandResult :: Text -> CommandResult
grokFailedCommandResult message = CommandResult
    { commandExitCode = Just 1
    , commandStdout = ""
    , commandStderr = message
    , commandTimedOut = False
    , commandCancelled = False
    }

nextTaskId :: GrokSession -> IO Text
nextTaskId session = atomicModifyIORef' session.grokNextId \n ->
    (n + 1, "t" <> Text.pack (show (n + 1)))

-- | Run the persist wrapper under bash so `export -p` dumps (`declare -x`)
-- can be sourced on the next call.
bashWrap :: String -> String
bashWrap script = "bash -c " ++ quoteString script

wrapScript :: PersistentShell -> Bool -> String -> String
wrapScript shell persist command =
    unlines $ prefix ++ [command] ++ if persist then persistTail else []
  where
    prefix =
        [ "set +e"
        , "set -a"
        , "[ -s " <> quote shell.shellEnvFile <> " ] && . " <> quote shell.shellEnvFile
        , "set +a"
        , "cd " <> quote shell.shellCwd <> " || exit 1"
        ]
    persistTail =
        [ "STATUS=$?"
        , "pwd > " <> quote (cwdFile shell)
        , "export -p > " <> quote shell.shellEnvFile
        , "exit $STATUS"
        ]

cwdFile :: PersistentShell -> OsPath
cwdFile shell = shell.shellEnvFile <.> unsafeEncodeUtf "cwd"

refreshCwd :: ToolEnv -> PersistentShell -> IO PersistentShell
refreshCwd env shell = do
    contents <- tryAny (Text.readFile (unsafeToFilePath (cwdFile shell)))
    case contents of
        Left _ -> pure shell
        Right raw -> do
            let candidate = fromText (Text.strip raw)
            dirOk <- doesDirectoryExist candidate
            if not dirOk
                then pure shell
                else resolveUnderCwd env candidate >>= \case
                    Left _ -> pure shell
                    Right resolved -> pure shell { shellCwd = resolved }

quote :: OsPath -> String
quote = quoteString . unsafeToFilePath

quoteString :: String -> String
quoteString path = "'" <> concatMap escape path <> "'"
  where
    escape '\'' = "'\\''"
    escape c = [c]

removeIfExists :: OsPath -> IO ()
removeIfExists path = do
    exists <- doesFileExist path
    if exists
        then void $ tryAny (removeFile path)
        else pure ()

acquirePrivateFile :: FilePath -> IO OsPath
acquirePrivateFile template =
    mask \restore -> do
        tmp <- getTemporaryDirectory
        (pathRaw, handle) <- restore $
            mkstemp (unsafeToFilePath (tmp </> unsafeEncodeUtf template))
        let path = unsafeEncodeUtf pathRaw
            rollback = do
                void $ tryAny (hClose handle)
                removeIfExists path
        flip onException rollback do
            hClose handle
            setFileMode pathRaw
                (unionFileModes ownerReadMode ownerWriteMode)
            Text.writeFile pathRaw ""
            pure path

cleanupEnvFiles :: OsPath -> IO ()
cleanupEnvFiles envFile = do
    removeIfExists envFile
    removeIfExists (envFile <.> unsafeEncodeUtf "cwd")

formatExit :: CommandResult -> Text
formatExit result
    | result.commandCancelled = "exit: cancelled\n" <> body
    | result.commandTimedOut = "exit: killed (timeout)\n" <> body
    | otherwise = "exit: " <> Text.pack (show (fromMaybe 1 result.commandExitCode)) <> "\n" <> body
  where
    body = combinePipes result.commandStdout result.commandStderr

combinePipes :: Text -> Text -> Text
combinePipes out err
    | Text.null err = out
    | Text.null out = err
    | otherwise = out <> "\n" <> err

-- | True when a foreground command would background itself with @&@.
hasUnwaitedBackgroundOp :: Text -> Bool
hasUnwaitedBackgroundOp command =
    not (endsWithWait command) && containsBareAmp (stripQuoted command)

endsWithWait :: Text -> Bool
endsWithWait command =
    let trimmed = Text.dropWhileEnd (`elem` (" \t\n;" :: String)) (Text.strip command)
    in trimmed == "wait" || " wait" `Text.isSuffixOf` trimmed
        || ";wait" `Text.isSuffixOf` trimmed || "\nwait" `Text.isSuffixOf` trimmed

stripQuoted :: Text -> Text
stripQuoted = Text.pack . go False False . Text.unpack
  where
    go _ _ [] = []
    go single double (c : cs)
        | c == '\\' && not single = case cs of
            (_ : rest) -> ' ' : go single double rest
            [] -> []
        | c == '\'' && not double = go (not single) double cs
        | c == '"' && not single = go single (not double) cs
        | single || double = ' ' : go single double cs
        | otherwise = c : go single double cs

containsBareAmp :: Text -> Bool
containsBareAmp text = go (' ' : Text.unpack text)
  where
    go [] = False
    go (a : '&' : [])
        | a `notElem` ("&<>|" :: String) = True
        | otherwise = False
    go (a : '&' : b : rest)
        | a `notElem` ("&<>|" :: String) && b `notElem` ("&>" :: String) = True
        | otherwise = go ('&' : b : rest)
    go (_ : rest) = go rest
