-- | Persistent Grok shell session: replay cwd/env, background tasks.
--
-- v1 emulates grok-build's local terminal by wrapping each command so it
-- starts in the last cwd, sources the last @export -p@ dump, then writes
-- those files again. No PTY.
module Agent.GrokBuild.Dialect.Shell
    ( GrokSession(..)
    , PersistentShell(..)
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
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.MVar
import Control.Exception.Safe (mask, onException, throwIO, tryAny)
import Control.Monad (void)
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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

data GrokSession = GrokSession
    { grokEnv :: !ToolEnv
    , grokShell :: !(MVar PersistentShell)
    , grokTasks :: !(MVar (Map Text BackgroundTask))
    , grokNextId :: !(IORef Int)
    , grokResources :: !ResourceScope
    }

newGrokSession :: ToolEnv -> IO GrokSession
newGrokSession env = do
    resources <- newResourceScope
    flip onException (closeResourceScope resources) do
        (_, envFile) <- allocateResource resources acquireEnvFile cleanupEnvFiles
        shell <- newMVar PersistentShell
            { shellCwd = env.toolCwd
            , shellEnvFile = envFile
            }
        tasks <- newMVar Map.empty
        nextId <- newIORef 0
        pure GrokSession
            { grokEnv = env
            , grokShell = shell
            , grokTasks = tasks
            , grokNextId = nextId
            , grokResources = resources
            }
  where
    acquireEnvFile =
        mask \restore -> do
            tmp <- getTemporaryDirectory
            (envFileRaw, handle) <- restore $
                mkstemp (unsafeToFilePath (tmp </> unsafeEncodeUtf "agent-grok-env"))
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
    cleanupEnvFiles envFile = do
        removeIfExists envFile
        removeIfExists (envFile <.> unsafeEncodeUtf "cwd")

-- | Delete the env/cwd dump and interrupt leftover background tasks.
-- Call this when the CLI/session ends, including after exceptions.
closeGrokSession :: GrokSession -> IO ()
closeGrokSession session = do
    modifyMVar_ session.grokTasks (const (pure Map.empty))
    closeResourceScope session.grokResources

runForegroundStreaming
    :: GrokSession
    -> String
    -> Int
    -> (Text -> Text -> IO ())
    -> IO CommandResult
runForegroundStreaming session command timeoutMs onSnapshot =
    modifyMVar session.grokShell \shell -> do
        let wrapped = bashWrap (wrapScript shell True command)
        result <- runShellCommandStreaming
            session.grokEnv
            session.grokEnv.toolCwd
            wrapped
            timeoutMs
            onSnapshot
        next <- if result.commandTimedOut || result.commandCancelled
            then pure shell
            else refreshCwd session.grokEnv shell
        pure (next, result)

startBackground :: GrokSession -> Text -> IO (Either Text Text)
startBackground session command = do
    shell <- readMVar session.grokShell
    -- Background wrappers source cwd/env but must not write them back;
    -- a later foreground command owns the persistent session.
    let wrapped = bashWrap (wrapScript shell False (Text.unpack command))
    started <- tryAny $
        allocateResource session.grokResources
            (startShellCommand session.grokEnv session.grokEnv.toolCwd wrapped
                >>= either (throwIO . userError . Text.unpack) pure)
            stopShellCommand
    case started of
        Left exception ->
            pure (Left (Text.pack (show exception)))
        Right (resource, running) -> do
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

readTaskOutput :: GrokSession -> Text -> Maybe Int -> IO Text
readTaskOutput session taskId timeoutMs = do
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
killTask session taskId = do
    tasks <- readMVar session.grokTasks
    case Map.lookup taskId tasks of
        Nothing -> pure $ "Unknown task_id: " <> taskId
        Just task -> do
            releaseResource task.backgroundResource
            result <- readMVar task.backgroundRunning.runningResult
            pure $ "killed " <> taskId <> "\n" <> formatCommandResult result

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
