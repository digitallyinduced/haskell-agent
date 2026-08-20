-- | Persistent Grok shell session: replay cwd/env, background tasks.
--
-- v1 emulates grok-build's local terminal by wrapping each command so it
-- starts in the last cwd, sources the last @export -p@ dump, then writes
-- those files again. No PTY.
module Agent.Tools.Grok.Shell
    ( GrokSession(..)
    , PersistentShell(..)
    , newGrokSession
    , closeGrokSession
    , runForeground
    , startBackground
    , readTaskOutput
    , killTask
    , hasUnwaitedBackgroundOp
    ) where

import Agent.Tools.IO
    ( CommandResult(..)
    , RunningCommand(..)
    , resolveUnderCwd
    , runShellCommand
    , runningLiveOutput
    , startShellCommand
    )
import Agent.Tools.Types (ToolEnv(..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.MVar
import Control.Exception (SomeException, try)
import Control.Monad (forM_, void)
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory (doesDirectoryExist, getTemporaryDirectory, removeFile)
import System.IO (hClose)
import System.FilePath ((</>))
import System.Posix.Files (ownerReadMode, ownerWriteMode, setFileMode, unionFileModes)
import System.Posix.Temp (mkstemp)
import System.Process (interruptProcessGroupOf)

data PersistentShell = PersistentShell
    { shellCwd :: !FilePath
    , shellEnvFile :: !FilePath
    }

data BackgroundTask = BackgroundTask
    { backgroundId :: !Text
    , backgroundRunning :: !RunningCommand
    }

data GrokSession = GrokSession
    { grokEnv :: !ToolEnv
    , grokShell :: !(MVar PersistentShell)
    , grokTasks :: !(MVar (Map Text BackgroundTask))
    , grokNextId :: !(IORef Int)
    }

newGrokSession :: ToolEnv -> IO GrokSession
newGrokSession env = do
    tmp <- getTemporaryDirectory
    (envFile, handle) <- mkstemp (tmp </> "agent-grok-env")
    hClose handle
    setFileMode envFile (unionFileModes ownerReadMode ownerWriteMode)
    Text.writeFile envFile ""
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
        }

-- | Delete the env/cwd dump and interrupt leftover background tasks.
-- Call this when the CLI/session ends, including after exceptions.
closeGrokSession :: GrokSession -> IO ()
closeGrokSession session = do
    tasks <- modifyMVar session.grokTasks \current ->
        pure (Map.empty, current)
    forM_ (Map.elems tasks) \task ->
        void $ try @SomeException
            (interruptProcessGroupOf task.backgroundRunning.runningHandle)
    shell <- readMVar session.grokShell
    _ <- try @SomeException (removeFile shell.shellEnvFile)
    _ <- try @SomeException (removeFile (cwdFile shell))
    pure ()

runForeground :: GrokSession -> String -> Int -> IO CommandResult
runForeground session command timeoutMs =
    modifyMVar session.grokShell \shell -> do
        let wrapped = bashWrap (wrapScript shell True command)
        result <- runShellCommand session.grokEnv session.grokEnv.toolCwd wrapped timeoutMs
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
    startShellCommand session.grokEnv session.grokEnv.toolCwd wrapped >>= \case
        Left err -> pure (Left err)
        Right running -> do
            taskId <- nextTaskId session
            modifyMVar_ session.grokTasks \tasks ->
                pure $ Map.insert taskId BackgroundTask
                    { backgroundId = taskId
                    , backgroundRunning = running
                    } tasks
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
                    Right result -> pure (formatExit result)

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
    tasks <- readMVar session.grokTasks
    case Map.lookup taskId tasks of
        Nothing -> pure $ "Unknown task_id: " <> taskId
        Just task -> do
            _ <- try @SomeException
                (interruptProcessGroupOf task.backgroundRunning.runningHandle)
            raced <- race
                (threadDelay 5000000)
                (readMVar task.backgroundRunning.runningResult)
            case raced of
                Left () ->
                    pure $ "kill signal sent to " <> taskId <> ", still running"
                Right result ->
                    pure $ "killed " <> taskId <> "\n" <> formatExit result

nextTaskId :: GrokSession -> IO Text
nextTaskId session = atomicModifyIORef' session.grokNextId \n ->
    (n + 1, "t" <> Text.pack (show (n + 1)))

-- | Run the persist wrapper under bash so `export -p` dumps (`declare -x`)
-- can be sourced on the next call.
bashWrap :: String -> String
bashWrap script = "bash -c " ++ quote script

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

cwdFile :: PersistentShell -> FilePath
cwdFile shell = shell.shellEnvFile <> ".cwd"

refreshCwd :: ToolEnv -> PersistentShell -> IO PersistentShell
refreshCwd env shell = do
    contents <- try @SomeException (Text.readFile (cwdFile shell))
    case contents of
        Left _ -> pure shell
        Right raw -> do
            let candidate = Text.unpack (Text.strip raw)
            dirOk <- doesDirectoryExist candidate
            if not dirOk
                then pure shell
                else resolveUnderCwd env candidate >>= \case
                    Left _ -> pure shell
                    Right resolved -> pure shell { shellCwd = resolved }

quote :: FilePath -> String
quote path = "'" <> concatMap escape path <> "'"
  where
    escape '\'' = "'\\''"
    escape c = [c]

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
