-- | Persistent Grok shell session: replay cwd/env, background tasks.
--
-- v1 emulates grok-build's local terminal by wrapping each command so it
-- starts in the last cwd, sources the last @export -p@ dump, then writes
-- those files again. No PTY.
module Agent.Tools.Grok.Shell
    ( GrokSession
    , grokSessionEnv
    , grokSessionEnvFile
    , newGrokSession
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
    , combineCommandOutput
    , formatCommandResult
    , resolveUnderCwd
    , runShellCommandStreaming
    , runningLiveOutput
    , startShellCommand
    , stopShellCommand
    )
import Agent.Tools.Types (ToolEnv(..))
import Control.Concurrent (forkIOWithUnmask, threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.MVar
import qualified Control.Exception as Exception
import Control.Exception.Safe
    ( SomeException
    , finally
    , mask
    , onException
    , throwIO
    , tryAny
    , uninterruptibleMask_
    )
import Control.Monad (void)
import qualified Data.ByteString as BS
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
    { backgroundRunning :: !RunningCommand
    , backgroundResource :: !ResourceKey
    , backgroundSnapshot :: !OsPath
    }

type GrokCloseOutcome = Either SomeException ()

data GrokLifecycle
    = GrokOpen
    | GrokClosing !(MVar GrokCloseOutcome)

data GrokSession = GrokSession
    { grokEnv :: !ToolEnv
    , grokEnvFile :: !OsPath
    , grokShell :: !(MVar PersistentShell)
    , grokTasks :: !(MVar (Map Text BackgroundTask))
    , grokNextId :: !(IORef Int)
    , grokResources :: !ResourceScope
    , grokLifecycle :: !(MVar GrokLifecycle)
    }

grokSessionEnv :: GrokSession -> ToolEnv
grokSessionEnv session = session.grokEnv

grokSessionEnvFile :: GrokSession -> OsPath
grokSessionEnvFile session = session.grokEnvFile

newGrokSession :: ToolEnv -> IO GrokSession
newGrokSession env = do
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
        lifecycle <- newMVar GrokOpen
        pure GrokSession
            { grokEnv = env
            , grokEnvFile = envFile
            , grokShell = shell
            , grokTasks = tasks
            , grokNextId = nextId
            , grokResources = resources
            , grokLifecycle = lifecycle
            }

-- | Delete the env/cwd dump and interrupt leftover background tasks.
-- Call this when the CLI/session ends, including after exceptions.
closeGrokSession :: GrokSession -> IO ()
closeGrokSession session =
    mask \restore -> do
        done <- modifyMVar session.grokLifecycle \case
            GrokOpen -> do
                completed <- newEmptyMVar
                _ <- forkIOWithUnmask \unmask -> do
                    result <- Exception.try @SomeException $ unmask do
                        _ <- readMVar session.grokShell
                        modifyMVar_ session.grokTasks
                            (const (pure Map.empty))
                            `finally` closeResourceScope session.grokResources
                    uninterruptibleMask_ (putMVar completed result)
                pure (GrokClosing completed, completed)
            closing@(GrokClosing completed) ->
                pure (closing, completed)
        restore (readMVar done) >>= either throwIO pure

-- The snapshot callback runs while this session's operation gate is held. It
-- must not start another shell operation or close the session.
runForegroundStreaming
    :: GrokSession
    -> String
    -> Int
    -> (Text -> Text -> IO ())
    -> IO CommandResult
runForegroundStreaming session command timeoutMs onSnapshot = do
    result <- withOpenGrokShell session \shell -> do
        let wrapped = bashWrap (wrapScript shell True command)
        commandResult <- runShellCommandStreaming
            session.grokEnv
            session.grokEnv.toolCwd
            wrapped
            timeoutMs
            onSnapshot
        next <- if
                commandResult.commandTimedOut
                    || commandResult.commandCancelled
            then pure shell
            else refreshCwd session.grokEnv shell
        pure (next, commandResult)
    pure (fromMaybe closedCommandResult result)

startBackground :: GrokSession -> Text -> IO (Either Text Text)
startBackground session command =
    mask \_ -> do
        result <- withOpenGrokShell session startOpen
        pure (fromMaybe (Left grokClosedMessage) result)
  where
    startOpen shell = do
        started <- tryAny $
            allocateResource session.grokResources
                (acquireBackground shell)
                cleanupBackground
        case started of
            Left exception ->
                pure (shell, Left (Text.pack (show exception)))
            Right (resource, (running, snapshotFile)) ->
                flip onException (releaseResource resource) do
                    taskId <- nextTaskId session
                    let task = BackgroundTask
                            { backgroundRunning = running
                            , backgroundResource = resource
                            , backgroundSnapshot = snapshotFile
                            }
                    modifyMVar_ session.grokTasks
                        (\tasks -> pure (Map.insert taskId task tasks))
                    let response =
                            "Command moved to background.\n\
                            \task_id: " <> taskId <> "\n\
                            \Use get_task_output to read output. Do not poll in a loop."
                    pure (shell, Right response)

    -- The child may not source its environment until after
    -- startShellCommand returns. Give it an immutable snapshot so a later
    -- foreground command cannot change what this task observes.
    acquireBackground shell =
        mask \restore -> do
            snapshotFile <- acquirePrivateFile "agent-grok-env-bg"
            let cleanupSnapshot = cleanupEnvFiles snapshotFile
            flip onException cleanupSnapshot do
                restore $
                    BS.readFile (unsafeToFilePath shell.shellEnvFile)
                        >>= BS.writeFile (unsafeToFilePath snapshotFile)
                let snapshotShell = shell { shellEnvFile = snapshotFile }
                    wrapped =
                        bashWrap
                            (wrapScript snapshotShell False (Text.unpack command))
                running <-
                    startShellCommand
                        session.grokEnv
                        session.grokEnv.toolCwd
                        wrapped
                    >>= either (throwIO . userError . Text.unpack) pure
                pure (running, snapshotFile)

    cleanupBackground (running, snapshotFile) =
        stopShellCommand running
            `finally` cleanupEnvFiles snapshotFile

readTaskOutput :: GrokSession -> Text -> Maybe Int -> IO Text
readTaskOutput session taskId timeoutMs = do
    background <- Map.lookup taskId <$> readMVar session.grokTasks
    lifecycle <- readMVar session.grokLifecycle
    let task = case lifecycle of
            GrokClosing{} -> Left ()
            GrokOpen -> Right background
    case task of
        Left () -> pure ("exit: 1\n" <> grokClosedMessage)
        Right Nothing -> pure $ "Unknown task_id: " <> taskId
        Right (Just background) -> case timeoutMs of
            Nothing -> snapshotTask background
            Just ms -> do
                raced <- race
                    (threadDelay (max 1 ms * 1000))
                    (readMVar background.backgroundRunning.runningResult)
                case raced of
                    Left () -> snapshotTask background
                    Right result -> pure (formatCommandResult result)

snapshotTask :: BackgroundTask -> IO Text
snapshotTask task =
    tryReadMVar task.backgroundRunning.runningResult >>= \case
        Just result -> pure (formatCommandResult result)
        Nothing -> do
            (out, err) <- runningLiveOutput task.backgroundRunning
            let body = combineCommandOutput out err
            pure $ if Text.null body
                then "still running"
                else "still running\n" <> body

killTask :: GrokSession -> Text -> IO Text
killTask session taskId =
    mask \restore -> do
        result <- withOpenGrokShell session \shell -> do
            tasks <- readMVar session.grokTasks
            response <- case Map.lookup taskId tasks of
                Nothing -> pure $ "Unknown task_id: " <> taskId
                Just task -> do
                    restore (stopShellCommand task.backgroundRunning)
                    cleanupEnvFiles task.backgroundSnapshot
                    releaseResource task.backgroundResource
                    commandResult <-
                        readMVar task.backgroundRunning.runningResult
                    pure $
                        "killed " <> taskId <> "\n"
                            <> formatCommandResult commandResult
            pure (shell, response)
        pure (fromMaybe grokClosedMessage result)

-- 'grokShell' doubles as the operation gate. The lifecycle cell is only held
-- for the short open-to-closing transition, so close can commit while an
-- operation is running and its detached worker uses this as a drain fence.
withOpenGrokShell
    :: GrokSession
    -> (PersistentShell -> IO (PersistentShell, a))
    -> IO (Maybe a)
withOpenGrokShell session action = do
    lifecycle <- readMVar session.grokLifecycle
    case lifecycle of
        GrokClosing{} ->
            pure Nothing
        GrokOpen ->
            modifyMVar session.grokShell \shell -> do
                admitted <- readMVar session.grokLifecycle
                case admitted of
                    GrokOpen -> do
                        (next, result) <- action shell
                        pure (next, Just result)
                    GrokClosing{} ->
                        pure (shell, Nothing)

grokClosedMessage :: Text
grokClosedMessage = "Grok session is closed."

closedCommandResult :: CommandResult
closedCommandResult = CommandResult
    { commandExitCode = Just 1
    , commandStdout = ""
    , commandStderr = grokClosedMessage
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

acquirePrivateFile :: String -> IO OsPath
acquirePrivateFile template =
    mask \_ -> do
        tmp <- getTemporaryDirectory
        (pathRaw, handle) <-
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
