-- | Persistent Grok shell session: replay cwd/env, background tasks.
--
-- v1 emulates grok-build's local terminal by wrapping each command so it
-- starts in the last cwd, sources the last @export -p@ dump, then writes
-- those files again. No PTY.
module Agent.GrokBuild.Dialect.Shell
    ( GrokSession(..)
    , PersistentShell(..)
    , newGrokSession
    , resetGrokSessionTemp
    , closeGrokSession
    , runForegroundStreaming
    , startBackground
    , startMonitor
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
import Agent.Tools.Dangerous (blockedShellCommandReasonIn)
import Agent.Tools.Background
    ( CompletionGate
    , consumeCompletion
    , dismissBackgroundTaskNotice
    , newCompletionGate
    , publishBackgroundTaskNotice
    , publishCompletion
    , suppressCompletion
    , systemReminder
    )
import Agent.Tools.IO
    ( CommandResult(..)
    , RunningCommand(..)
    , combineCommandOutput
    , formatCommandResult
    , resolveUnderCwd
    , runShellCommandStreaming
    , runningLiveOutput
    , startShellCommandWithCompletion
    , stopShellCommand
    )
import Agent.Tools.Types
    ( BackgroundTaskNotice(..)
    , ToolEnv(..)
    )
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently_, race)
import Control.Concurrent.MVar
import Control.Exception.Safe (mask, onException, throwIO, tryAny)
import Control.Monad (forM, void)
import Data.IORef
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory.OsPath
    ( doesDirectoryExist
    , doesFileExist
    , removeFile
    )
import System.IO (hClose)
import System.IO.Temp (getCanonicalTemporaryDirectory, openTempFile)
import System.OsPath (OsPath, unsafeEncodeUtf, (<.>))
import System.Posix.Files (ownerReadMode, ownerWriteMode, setFileMode, unionFileModes)

data PersistentShell = PersistentShell
    { shellCwd :: !OsPath
    , shellEnvFile :: !OsPath
    , shellEnvResource :: !ResourceKey
    }

maxRetainedCompletedTasks :: Int
maxRetainedCompletedTasks = 64

maxLiveBackgroundTasks :: Int
maxLiveBackgroundTasks = 64

data BackgroundTask = BackgroundTask
    { backgroundId :: !Text
    , backgroundSequence :: !Int
    , backgroundRunning :: !RunningCommand
    , backgroundResource :: !ResourceKey
    , backgroundCompletion :: !CompletionGate
    }

data BackgroundTaskStore = BackgroundTaskStore
    { backgroundNextId :: !Int
    , backgroundTasks :: !(Map Text BackgroundTask)
    }

data GrokSession = GrokSession
    { grokEnv :: !ToolEnv
    , grokLifecycle :: !(MVar ())
    , grokShell :: !(MVar PersistentShell)
    , grokTasks :: !(MVar BackgroundTaskStore)
    , grokTodos :: !(IORef (Map Text (Text, Text)))
    , grokResources :: !ResourceScope
    }

newGrokSession :: ToolEnv -> IO GrokSession
newGrokSession env = do
    resources <- newResourceScope
    flip onException (closeResourceScope resources) do
        lifecycle <- newMVar ()
        tempDir <- currentSessionTempDir env
        (envResource, envFile) <-
            allocateResource resources
                (acquireEnvFile tempDir)
                cleanupEnvFiles
        shell <- newMVar PersistentShell
            { shellCwd = env.toolCwd
            , shellEnvFile = envFile
            , shellEnvResource = envResource
            }
        tasks <- newMVar BackgroundTaskStore
            { backgroundNextId = 0
            , backgroundTasks = Map.empty
            }
        todos <- newIORef Map.empty
        pure GrokSession
            { grokEnv = env
            , grokLifecycle = lifecycle
            , grokShell = shell
            , grokTasks = tasks
            , grokTodos = todos
            , grokResources = resources
            }

-- | Reset the persistent shell state when the host switches conversations.
-- The previous session directory may already have been removed by the time
-- this runs, so allocate a fresh state file under the new private temp root
-- and reset cwd/environment state rather than retaining dead paths.
resetGrokSessionTemp :: GrokSession -> OsPath -> IO ()
resetGrokSessionTemp session tempDir =
    withMVar session.grokLifecycle \() -> do
        resetGrokBackgroundTasks session
        mask \restore -> do
            (nextResource, nextEnvFile) <- restore $
                allocateResource session.grokResources
                    (acquireEnvFile tempDir)
                    cleanupEnvFiles
            previousResource <-
                (modifyMVar session.grokShell \shell ->
                    pure $
                        replaceShellEnvironment
                            session.grokEnv.toolCwd
                            nextEnvFile
                            nextResource
                            shell)
                    `onException` releaseResource nextResource
            releaseResource previousResource

replaceShellEnvironment
    :: OsPath
    -> OsPath
    -> ResourceKey
    -> PersistentShell
    -> (PersistentShell, ResourceKey)
replaceShellEnvironment nextCwd nextEnvFile nextResource shell =
    ( shell
        { shellCwd = nextCwd
        , shellEnvFile = nextEnvFile
        , shellEnvResource = nextResource
        }
    , shell.shellEnvResource
    )

currentSessionTempDir :: ToolEnv -> IO OsPath
currentSessionTempDir env =
    readIORef env.toolSessionTmp >>= \case
        Just sessionTmp -> pure sessionTmp
        Nothing -> unsafeEncodeUtf <$> getCanonicalTemporaryDirectory

acquireEnvFile :: OsPath -> IO OsPath
acquireEnvFile tempDir =
    mask \restore -> do
        (envFileRaw, handle) <- restore $
            openTempFile (unsafeToFilePath tempDir) "agent-grok-env"
        let envFile = unsafeEncodeUtf envFileRaw
        let rollback = do
                void $ tryAny (hClose handle)
                removeIfExists envFile
        flip onException rollback do
            hClose handle
            setFileMode envFileRaw
                (unionFileModes ownerReadMode ownerWriteMode)
            Text.writeFile envFileRaw ""
            pure envFile

cleanupEnvFiles :: OsPath -> IO ()
cleanupEnvFiles envFile =
    void $ tryAny do
        removeIfExists envFile
        removeIfExists (envFile <.> unsafeEncodeUtf "cwd")

-- | Delete the env/cwd dump and interrupt leftover background tasks.
-- Call this when the CLI/session ends, including after exceptions.
closeGrokSession :: GrokSession -> IO ()
closeGrokSession session =
    withMVar session.grokLifecycle \() -> do
        resetGrokBackgroundTasks session
        closeResourceScope session.grokResources

-- | Stop and forget background commands from the previous conversation.
-- Preserve the id counter so stale task ids cannot alias newly started work.
resetGrokBackgroundTasks :: GrokSession -> IO ()
resetGrokBackgroundTasks session = do
    tasks <- modifyMVar session.grokTasks \store ->
        pure
            ( store { backgroundTasks = Map.empty }
            , Map.elems store.backgroundTasks
            )
    mapConcurrently_
        (\task -> void $ tryAny do
            suppressTaskCompletion session task
            releaseResource task.backgroundResource)
        tasks

runForegroundStreaming
    :: GrokSession
    -> Text
    -> Int
    -> (Text -> Text -> IO ())
    -> IO (Either Text CommandResult)
runForegroundStreaming session command timeoutMs onSnapshot =
    withMVar session.grokLifecycle \() ->
        modifyMVar session.grokShell \shell -> do
            sessionTmp <- readIORef session.grokEnv.toolSessionTmp
            blockedShellCommandReasonIn
                sessionTmp shell.shellCwd command >>= \case
                    Just reason -> pure (shell, Left reason)
                    Nothing -> do
                        let wrapped =
                                bashWrap
                                    (wrapScript shell True
                                        (Text.unpack command))
                        result <- runShellCommandStreaming
                            session.grokEnv
                            session.grokEnv.toolCwd
                            (Text.pack wrapped)
                            timeoutMs
                            onSnapshot
                        next <- if result.commandTimedOut
                                || result.commandCancelled
                            then pure shell
                            else refreshCwd session.grokEnv shell
                        pure (next, Right result)

startBackground :: GrokSession -> Text -> IO (Either Text Text)
startBackground session command =
    startBackgroundCommand session command

startMonitor :: GrokSession -> Text -> Maybe Int -> IO (Either Text Text)
startMonitor session command timeoutMs =
    startBackgroundCommand session (monitorCommand command timeoutMs)

startBackgroundCommand :: GrokSession -> Text -> IO (Either Text Text)
startBackgroundCommand session command =
    withMVar session.grokLifecycle \() ->
        modifyMVar session.grokShell \shell -> do
            sessionTmp <- readIORef session.grokEnv.toolSessionTmp
            blockedShellCommandReasonIn
                sessionTmp shell.shellCwd command >>= \case
                    Just reason -> pure (shell, Left reason)
                    Nothing -> do
                        (stale, reservation) <-
                            reserveTaskId session
                        mapM_
                            (\task ->
                                void $ tryAny $
                                    releaseResource task.backgroundResource)
                            stale
                        case reservation of
                            Left err -> pure (shell, Left err)
                            Right (taskSequence, taskId) -> do
                                completion <- newCompletionGate
                                -- Background wrappers source cwd/env but must
                                -- not write them back; a later foreground
                                -- command owns the persistent session.
                                let wrapped =
                                        bashWrap
                                            (wrapScript shell False
                                                (Text.unpack command))
                                    publish result =
                                        publishCompletion completion $
                                            publishBackgroundTaskNotice
                                                session.grokEnv
                                                (grokCompletionNotice
                                                    taskId command result)
                                    suppress =
                                        suppressCompletion completion $
                                            dismissBackgroundTaskNotice
                                                session.grokEnv
                                                (grokCompletionKey taskId)
                                started <- tryAny $
                                    allocateResource session.grokResources
                                        (startShellCommandWithCompletion
                                            session.grokEnv
                                            session.grokEnv.toolCwd
                                            (Text.pack wrapped)
                                            publish
                                            >>= either
                                                (throwIO . userError . Text.unpack)
                                                pure)
                                        stopShellCommand
                                case started of
                                    Left exception ->
                                        pure
                                            ( shell
                                            , Left (Text.pack (show exception))
                                            )
                                    Right (resource, running) -> do
                                        let task = BackgroundTask
                                                { backgroundId = taskId
                                                , backgroundSequence =
                                                    taskSequence
                                                , backgroundRunning = running
                                                , backgroundResource = resource
                                                , backgroundCompletion =
                                                    completion
                                                }
                                        (insertBackgroundTask session task)
                                            `onException` do
                                                suppress
                                                releaseResource resource
                                        pure
                                            ( shell
                                            , Right $
                                                "Command moved to background.\n\
                                                \task_id: " <> taskId <> "\n\
                                                \Completion will be reported automatically. Do not poll or sleep-wait."
                                            )

reserveTaskId
    :: GrokSession
    -> IO ([BackgroundTask], Either Text (Int, Text))
reserveTaskId session =
    modifyMVar session.grokTasks \store -> do
        classified <-
            forM (Map.elems store.backgroundTasks) \task -> do
                completed <- maybe False (const True)
                    <$> tryReadMVar task.backgroundRunning.runningResult
                pure (completed, task)
        let completed =
                sortOn (.backgroundSequence)
                    [task | (True, task) <- classified]
            evictedCount =
                max 0 (length completed - maxRetainedCompletedTasks)
            evicted = take evictedCount completed
            retainedTasks =
                foldr
                    (Map.delete . (.backgroundId))
                    store.backgroundTasks
                    evicted
            runningCount =
                length [() | (False, _) <- classified]
            compacted = store { backgroundTasks = retainedTasks }
        if runningCount >= maxLiveBackgroundTasks
            then pure
                ( compacted
                , ( evicted
                  , Left
                        "Cannot start background command: terminal task limit reached."
                  )
                )
            else do
                let next = store.backgroundNextId + 1
                    taskId = "t" <> Text.pack (show next)
                pure
                    ( compacted { backgroundNextId = next }
                    , (evicted, Right (next, taskId))
                    )

insertBackgroundTask :: GrokSession -> BackgroundTask -> IO ()
insertBackgroundTask session task =
    modifyMVar session.grokTasks \store ->
        pure
            ( store
                { backgroundTasks =
                    Map.insert
                        task.backgroundId
                        task
                        store.backgroundTasks
                }
            , ()
            )

readTaskOutput :: GrokSession -> Text -> Maybe Int -> IO Text
readTaskOutput session taskId timeoutMs = do
    store <- readMVar session.grokTasks
    case Map.lookup taskId store.backgroundTasks of
        Nothing -> pure $ "Unknown task_id: " <> taskId
        Just task -> do
            case timeoutMs of
              Nothing -> snapshotTask session task
              Just ms -> do
                raced <- race
                    (threadDelay (max 1 ms * 1000))
                    (readMVar task.backgroundRunning.runningResult)
                case raced of
                    Left () -> snapshotTask session task
                    Right result -> do
                        consumeTaskCompletion session task
                        pure (formatCommandResult result)

-- The watchdog is part of the spawned process tree, so it outlives the tool
-- call without requiring an untracked Haskell thread. The outer shell waits
-- for the monitored command, cancels the watchdog on normal completion, and
-- escalates from TERM to KILL after the timeout.
monitorCommand :: Text -> Maybe Int -> Text
monitorCommand command = \case
    Nothing -> command
    Just timeoutMs ->
        Text.unlines
            [ "{"
            , command
            , "} &"
            , "monitored_pid=$!"
            , "("
            , "  sleep " <> timeoutSeconds timeoutMs
            , "  kill -TERM \"$monitored_pid\" 2>/dev/null || exit 0"
            , "  sleep 1"
            , "  kill -KILL \"$monitored_pid\" 2>/dev/null || true"
            , ") &"
            , "watchdog_pid=$!"
            , "wait \"$monitored_pid\""
            , "monitored_status=$?"
            , "kill \"$watchdog_pid\" 2>/dev/null || true"
            , "wait \"$watchdog_pid\" 2>/dev/null || true"
            , "exit \"$monitored_status\""
            ]
  where
    timeoutSeconds ms =
        Text.pack (show (fromIntegral (max 1 ms) / 1000 :: Double))

snapshotTask :: GrokSession -> BackgroundTask -> IO Text
snapshotTask session task =
    tryReadMVar task.backgroundRunning.runningResult >>= \case
        Just result -> do
            consumeTaskCompletion session task
            pure (formatCommandResult result)
        Nothing -> do
            (out, err) <- runningLiveOutput task.backgroundRunning
            let body = combineCommandOutput out err
            pure $ if Text.null body
                then "still running"
                else "still running\n" <> body

killTask :: GrokSession -> Text -> IO Text
killTask session taskId = do
    store <- readMVar session.grokTasks
    case Map.lookup taskId store.backgroundTasks of
        Nothing -> pure $ "Unknown task_id: " <> taskId
        Just task -> do
            suppressTaskCompletion session task
            releaseResource task.backgroundResource
            result <- readMVar task.backgroundRunning.runningResult
            pure $ "killed " <> taskId <> "\n" <> formatCommandResult result

consumeTaskCompletion :: GrokSession -> BackgroundTask -> IO ()
consumeTaskCompletion session task =
    consumeCompletion task.backgroundCompletion $
        dismissBackgroundTaskNotice
            session.grokEnv
            (grokCompletionKey task.backgroundId)

suppressTaskCompletion :: GrokSession -> BackgroundTask -> IO ()
suppressTaskCompletion session task =
    suppressCompletion task.backgroundCompletion $
        dismissBackgroundTaskNotice
            session.grokEnv
            (grokCompletionKey task.backgroundId)

grokCompletionKey :: Text -> Text
grokCompletionKey taskId = "grok-terminal:" <> taskId

grokCompletionNotice :: Text -> Text -> CommandResult -> BackgroundTaskNotice
grokCompletionNotice taskId command result =
    BackgroundTaskNotice
        { noticeKey = grokCompletionKey taskId
        , noticeBody = systemReminder $
            "Background terminal task " <> taskId <> " completed.\n\
            \The result is delivered automatically; do not call \
            \get_task_output merely to poll this task.\n\
            \Command:\n"
                <> boundedCompletionText 2048 command
                <> "\nResult:\n"
                <> boundedCompletionText
                    (32 * 1024)
                    (formatCommandResult result)
        }

boundedCompletionText :: Int -> Text -> Text
boundedCompletionText limit text
    | Text.length text <= limit = text
    | otherwise =
        let marker = "\n[...truncated...]\n"
            side = max 0 ((limit - Text.length marker) `div` 2)
        in Text.take side text <> marker <> Text.takeEnd side text

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
