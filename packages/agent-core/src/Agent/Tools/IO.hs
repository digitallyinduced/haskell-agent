-- | Shared filesystem and process helpers for coding tools.
module Agent.Tools.IO
    ( CommandResult(..)
    , RunningCommand(..)
    , resolveUnderCwd
    , readTextFile
    , writeTextFile
    , deleteTextFile
    , renameTextFile
    , listDirectoryEntries
    , runShellCommand
    , startShellCommand
    , runningLiveOutput
    , truncateText
    ) where

import Agent.Tools.Types (ToolEnv(..))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (async, concurrently, race, wait)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar)
import Control.Exception (evaluate)
import Control.Exception.Safe (SomeException, catchIO, throwIO, try)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.ByteString as BS
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import System.Directory
    ( canonicalizePath
    , createDirectoryIfMissing
    , doesDirectoryExist
    , doesPathExist
    , listDirectory
    , removeFile
    , renameFile
    )
import System.Exit (ExitCode(..))
import System.IO.Error (isAlreadyInUseError)
import System.FilePath
    ( addTrailingPathSeparator
    , isAbsolute
    , joinPath
    , splitDirectories
    , takeDirectory
    , takeFileName
    , (</>)
    )
import System.IO (Handle, hClose)
import System.Process
    ( CreateProcess(..)
    , ProcessHandle
    , StdStream(..)
    , createProcess
    , interruptProcessGroupOf
    , shell
    , waitForProcess
    )

-- | Resolve a model-supplied path against the tool cwd and reject anything
-- that canonicalizes outside that tree (including via '..' or symlinks).
resolveUnderCwd :: ToolEnv -> FilePath -> IO (Either Text FilePath)
resolveUnderCwd env requested = do
    absCwd <- canonicalizePath env.toolCwd
    let combined
            | isAbsolute requested = requested
            | otherwise = absCwd </> requested
    exists <- doesPathExist combined
    resolved <- if exists
        then canonicalizePath combined
        else resolveMissing combined
    if isInside absCwd resolved
        then pure (Right resolved)
        else pure $ Left $
            "Path escapes the working directory: " <> Text.pack requested

resolveMissing :: FilePath -> IO FilePath
resolveMissing combined = do
    let parent = takeDirectory combined
    parentExists <- doesDirectoryExist parent
    if parentExists
        then (</> takeFileName combined) <$> canonicalizePath parent
        else pure (collapseDots combined)

-- | Collapse @.@ / @..@ without requiring the path to exist, so a missing
-- @cwd/../outside@ cannot sneak past the prefix check.
collapseDots :: FilePath -> FilePath
collapseDots path = joinPath (go [] (splitDirectories path))
  where
    go acc [] = reverse acc
    go acc ("." : xs) = go acc xs
    go acc (".." : xs) = case acc of
        [] -> go acc xs
        ("/" : _) -> go acc xs
        (_ : rest) -> go rest xs
    go acc (x : xs) = go (x : acc) xs

isInside :: FilePath -> FilePath -> Bool
isInside root path =
    let root' = addTrailingPathSeparator root
    in path == root || root' `isPrefixOf` path

-- | GHC Handle locks throw @isAlreadyInUseError@ when another Handle
-- still has the path open. Retry briefly so overlapping tool calls
-- (e.g. parallel @search_replace@) can wait the lock out.
lockRetryDelaysUs :: [Int]
lockRetryDelaysUs = [1000, 2000, 4000, 8000, 16000]

retryOnBusy :: IO a -> IO a
retryOnBusy action = go lockRetryDelaysUs
  where
    go [] = action
    go (delayUs : rest) =
        catchIO action \err ->
            if isAlreadyInUseError err
                then threadDelay delayUs >> go rest
                else throwIO err

readTextFile :: FilePath -> IO (Either Text Text)
readTextFile path = try @_ @SomeException (retryOnBusy (BS.readFile path)) >>= \case
    Left err -> pure $ Left $ "Failed to read file: " <> Text.pack (show err)
    Right bytes
        | BS.elem 0 (BS.take 8192 bytes) ->
            pure $ Left "Cannot read binary file"
        | otherwise ->
            pure $ Right $ decodeUtf8With lenientDecode bytes

writeTextFile :: FilePath -> Text -> IO (Either Text ())
writeTextFile path content = do
    createDirectoryIfMissing True (takeDirectory path)
    try @_ @SomeException (retryOnBusy (BS.writeFile path (encodeUtf8 content))) >>= \case
        Left err -> pure $ Left $ "Failed to write file: " <> Text.pack (show err)
        Right () -> pure (Right ())

deleteTextFile :: FilePath -> IO (Either Text ())
deleteTextFile path =
    try @_ @SomeException (removeFile path) >>= \case
        Left err -> pure $ Left $ "Failed to delete file: " <> Text.pack (show err)
        Right () -> pure (Right ())

renameTextFile :: FilePath -> FilePath -> IO (Either Text ())
renameTextFile from to = do
    createDirectoryIfMissing True (takeDirectory to)
    try @_ @SomeException (renameFile from to) >>= \case
        Left err -> pure $ Left $ "Failed to move file: " <> Text.pack (show err)
        Right () -> pure (Right ())

listDirectoryEntries :: FilePath -> IO (Either Text [(FilePath, Bool)])
listDirectoryEntries path = try @_ @SomeException (listDirectory path) >>= \case
    Left err -> pure $ Left $ "Failed to list directory: " <> Text.pack (show err)
    Right names -> Right <$> mapM (classify path) names
  where
    classify root name = do
        isDir <- doesDirectoryExist (root </> name)
        pure (name, isDir)

data CommandResult = CommandResult
    { commandExitCode :: !(Maybe Int)
    , commandStdout :: !Text
    , commandStderr :: !Text
    , commandTimedOut :: !Bool
    } deriving (Eq, Show)

data RunningCommand = RunningCommand
    { runningHandle :: !ProcessHandle
    , runningResult :: !(MVar CommandResult)
    , runningStdout :: !(IORef BS.ByteString)
    , runningStderr :: !(IORef BS.ByteString)
    , runningCap :: !Int
    }

-- | Bytes already drained from a still-running command, decoded and capped.
runningLiveOutput :: RunningCommand -> IO (Text, Text)
runningLiveOutput running = do
    outBytes <- readIORef running.runningStdout
    errBytes <- readIORef running.runningStderr
    pure
        ( truncateText running.runningCap (decodeUtf8With lenientDecode outBytes)
        , truncateText running.runningCap (decodeUtf8With lenientDecode errBytes)
        )

-- | Run a shell command in @workdir@, killing the process group on timeout.
runShellCommand
    :: ToolEnv
    -> FilePath
    -> String
    -> Int
    -> IO CommandResult
runShellCommand env workdir command timeoutMs = do
    let spec = (shell command)
            { cwd = Just workdir
            , std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe
            , create_group = True
            }
    try @_ @SomeException (createProcess spec) >>= \case
        Left err -> pure CommandResult
            { commandExitCode = Just 127
            , commandStdout = ""
            , commandStderr = "Failed to start command: " <> Text.pack (show err)
            , commandTimedOut = False
            }
        Right (Just hin, Just hout, Just herr, processHandle) -> do
            hClose hin
            let collect = do
                    -- Drain stdout and stderr concurrently so a child that
                    -- fills one pipe cannot deadlock the other.
                    (outBytes, errBytes) <- concurrently
                        (strictGetContents hout)
                        (strictGetContents herr)
                    code <- waitForProcess processHandle
                    pure (outBytes, errBytes, code)
            raced <- race (threadDelay (max 1 timeoutMs * 1000)) collect
            case raced of
                Left () -> do
                    _ <- try @_ @SomeException (interruptProcessGroupOf processHandle)
                    pure CommandResult
                        { commandExitCode = Nothing
                        , commandStdout = ""
                        , commandStderr = ""
                        , commandTimedOut = True
                        }
                Right (outBytes, errBytes, code) ->
                    pure CommandResult
                        { commandExitCode = Just (exitCodeInt code)
                        , commandStdout = truncateText env.toolStdoutCap
                            (decodeUtf8With lenientDecode outBytes)
                        , commandStderr = truncateText env.toolStdoutCap
                            (decodeUtf8With lenientDecode errBytes)
                        , commandTimedOut = False
                        }
        Right _ ->
            pure CommandResult
                { commandExitCode = Just 127
                , commandStdout = ""
                , commandStderr = "Failed to capture command output"
                , commandTimedOut = False
                }

-- | Start a command without waiting. The result is written to 'runningResult'
-- when the process exits. Use 'interruptProcessGroupOf' on 'runningHandle' to kill it.
startShellCommand
    :: ToolEnv
    -> FilePath
    -> String
    -> IO (Either Text RunningCommand)
startShellCommand env workdir command = do
    let spec = (shell command)
            { cwd = Just workdir
            , std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe
            , create_group = True
            }
    try @_ @SomeException (createProcess spec) >>= \case
        Left err -> pure $ Left $ "Failed to start command: " <> Text.pack (show err)
        Right (Just hin, Just hout, Just herr, processHandle) -> do
            hClose hin
            resultVar <- newEmptyMVar
            stdoutRef <- newIORef BS.empty
            stderrRef <- newIORef BS.empty
            outA <- async (drainHandle hout stdoutRef)
            errA <- async (drainHandle herr stderrRef)
            _ <- forkIO do
                result <- try @_ @SomeException do
                    code <- waitForProcess processHandle
                    outBytes <- wait outA
                    errBytes <- wait errA
                    pure (outBytes, errBytes, code)
                putMVar resultVar $ case result of
                    Left exception -> CommandResult
                        { commandExitCode = Just 127
                        , commandStdout = ""
                        , commandStderr = Text.pack (show exception)
                        , commandTimedOut = False
                        }
                    Right (outBytes, errBytes, code) -> CommandResult
                        { commandExitCode = Just (exitCodeInt code)
                        , commandStdout = truncateText env.toolStdoutCap
                            (decodeUtf8With lenientDecode outBytes)
                        , commandStderr = truncateText env.toolStdoutCap
                            (decodeUtf8With lenientDecode errBytes)
                        , commandTimedOut = False
                        }
            pure $ Right RunningCommand
                { runningHandle = processHandle
                , runningResult = resultVar
                , runningStdout = stdoutRef
                , runningStderr = stderrRef
                , runningCap = env.toolStdoutCap
                }
        Right _ ->
            pure $ Left "Failed to capture command output"

strictGetContents :: Handle -> IO BS.ByteString
strictGetContents handle = do
    bytes <- BS.hGetContents handle
    _ <- evaluate (BS.length bytes)
    pure bytes

-- | Read the handle in chunks so a live snapshot can see output before EOF.
drainHandle :: Handle -> IORef BS.ByteString -> IO BS.ByteString
drainHandle handle ref = go
  where
    go = do
        chunk <- BS.hGetSome handle 8192
        if BS.null chunk
            then readIORef ref
            else do
                atomicModifyIORef' ref \soFar -> (soFar <> chunk, ())
                go

exitCodeInt :: ExitCode -> Int
exitCodeInt = \case
    ExitSuccess -> 0
    ExitFailure n -> n

truncateText :: Int -> Text -> Text
truncateText cap text
    | cap <= 0 = text
    | Text.length text <= cap = text
    | otherwise =
        Text.take cap text
            <> "\n...[truncated "
            <> Text.pack (show (Text.length text - cap))
            <> " chars]"
