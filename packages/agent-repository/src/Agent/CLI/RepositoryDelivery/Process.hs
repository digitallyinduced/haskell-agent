-- | Bounded, non-interactive delivery processes. This policy intentionally
-- remains separate from local review Git execution.
module Agent.CLI.RepositoryDelivery.Process
    ( ProcessResult(..)
    , ProcessFailure(..)
    , runCommand
    , runCommandWithEnvironment
    , trySynchronous
    ) where

import Agent.CLI.ProcessSecurity
    ( resolveExecutableOutside
    , sanitizeSearchPathOutside
    )
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (cancel, wait, withAsync)
import Control.Exception.Safe
    ( SomeException, bracket, finally, isAsyncException, mask
    , onException, throwIO, tryAny
    )
import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Text as Text
import System.Environment (getEnvironment)
import System.Exit (ExitCode)
import System.IO (Handle, hClose)
import System.Posix.Signals (Signal, sigKILL, sigTERM, signalProcessGroup)
import System.Posix.Types (ProcessID)
import System.Process
    ( CreateProcess(..), ProcessHandle, StdStream(CreatePipe)
    , createProcess, getPid, proc, terminateProcess, waitForProcess
    )
import System.Timeout (timeout)

data ProcessResult = ProcessResult
    { processExitCode :: !ExitCode
    , processStdout :: !BS.ByteString
    , processStderr :: !BS.ByteString
    , processOutputTruncated :: !Bool
    }

data ProcessFailure
    = ProcessLaunchFailed
    | ProcessTimedOut
    | ProcessOutputExceeded
    deriving (Eq, Show)

runCommand
    :: FilePath
    -> FilePath
    -> [String]
    -> BS.ByteString
    -> Int
    -> IO (Either ProcessFailure ProcessResult)
runCommand root =
    runCommandWithEnvironment [] root root

runCommandWithEnvironment
    :: [(String, String)]
    -> FilePath
    -> FilePath
    -> FilePath
    -> [String]
    -> BS.ByteString
    -> Int
    -> IO (Either ProcessFailure ProcessResult)
runCommandWithEnvironment
    overrides
    trustRoot
    workingDirectory
    executable
    arguments
    input
    timeoutMicros = do
    launched <- trySynchronous
        (bracket start stop \processData ->
            timeout timeoutMicros (run processData))
    pure case launched of
        Left _ -> Left ProcessLaunchFailed
        Right Nothing -> Left ProcessTimedOut
        Right (Just result) -> result
  where
    start = mask \_ -> do
        resolvedExecutable <-
            resolveExecutableOutside trustRoot executable
                >>= either (fail . Text.unpack) pure
        environment <- applyEnvironmentOverrides overrides
            <$> nonInteractiveEnvironment trustRoot executable
        (maybeInput, maybeOutput, maybeError, process) <-
            createProcess
                (proc resolvedExecutable arguments)
                    { cwd = Just workingDirectory
                    , std_in = CreatePipe
                    , std_out = CreatePipe
                    , std_err = CreatePipe
                    , close_fds = True
                    , create_group = True
                    , env = Just environment
                    }
        case (maybeInput, maybeOutput, maybeError) of
            (Just inputHandle, Just outputHandle, Just errorHandle) -> do
                let closePipes = do
                        closeQuietly inputHandle
                        closeQuietly outputHandle
                        closeQuietly errorHandle
                    cleanupWithoutGroup = do
                        closePipes
                        _ <- tryAny (terminateProcess process)
                        _ <- tryAny (waitForProcess process)
                        pure ()
                processGroup <- getPid process
                    `onException` cleanupWithoutGroup
                completed <- newIORef False
                    `onException` do
                        closePipes
                        terminateProcessGroup sigKILL processGroup process
                        _ <- tryAny (waitForProcess process)
                        pure ()
                pure
                    ( inputHandle
                    , outputHandle
                    , errorHandle
                    , process
                    , processGroup
                    , completed
                    )
            _ -> do
                terminateProcess process
                _ <- waitForProcess process
                fail "could not create command pipes"
    stop
        ( inputHandle
        , outputHandle
        , errorHandle
        , process
        , processGroup
        , completed
        ) = do
        closeQuietly inputHandle
        closeQuietly outputHandle
        closeQuietly errorHandle
        finished <- readIORef completed
        unless finished do
            terminateProcessGroup sigTERM processGroup process
            threadDelay 100_000
            -- A descendant can retain the group and pipes after its leader
            -- exits, so always escalate the captured process group.
            terminateProcessGroup sigKILL processGroup process
        _ <- tryAny (waitForProcess process)
        pure ()
    run
        ( inputHandle
        , outputHandle
        , errorHandle
        , process
        , processGroup
        , completed
        ) =
        withAsync
            (BS.hPut inputHandle input `finally` closeQuietly inputHandle)
            \inputWriter ->
                withAsync (readBounded outputHandle) \outputReader ->
                    withAsync (readBounded errorHandle) \errorReader -> do
                        exitCode <- waitForProcess process
                        inputFinished <- timeout processPipeTeardownMicros
                            (wait inputWriter)
                        case inputFinished of
                            Just () -> pure ()
                            Nothing -> do
                                closeQuietly inputHandle
                                cancel inputWriter
                        let drainReaders =
                                (,) <$> wait outputReader <*> wait errorReader
                        -- Give ordinary buffered output a short chance to
                        -- drain. The captured group is then terminated even
                        -- when EOF already arrived: a descendant may close
                        -- its pipes while continuing to run.
                        naturallyDrained <- timeout
                            processPipeTeardownMicros
                            drainReaders
                        terminateProcessGroup sigTERM processGroup process
                        drainedAfterTerm <- case naturallyDrained of
                            Just values -> pure (Just values)
                            Nothing ->
                                timeout processGroupTermGraceMicros drainReaders
                        -- Always escalate the captured group so a descendant
                        -- cannot outlive a successful leader.
                        terminateProcessGroup sigKILL processGroup process
                        drained <- case drainedAfterTerm of
                            Just values -> pure (Just values)
                            Nothing ->
                                timeout processPipeTeardownMicros drainReaders
                        case drained of
                            Nothing -> do
                                closeQuietly outputHandle
                                closeQuietly errorHandle
                                cancel outputReader
                                cancel errorReader
                                pure (Left ProcessTimedOut)
                            Just
                                ( (output, outputTruncated)
                                , (errors, errorsTruncated)
                                ) -> do
                                    writeIORef completed True
                                    let truncated =
                                            outputTruncated || errorsTruncated
                                    pure
                                        (if truncated
                                            then Left ProcessOutputExceeded
                                            else
                                                Right
                                                    ProcessResult
                                                        { processExitCode =
                                                            exitCode
                                                        , processStdout = output
                                                        , processStderr = errors
                                                        , processOutputTruncated =
                                                            False
                                                        })

applyEnvironmentOverrides
    :: [(String, String)]
    -> [(String, String)]
    -> [(String, String)]
applyEnvironmentOverrides overrides inherited =
    overrides
        <> filter
            (\(name, _) -> name `notElem` map fst overrides)
            inherited

data BoundedReadState = BoundedReadState
    { boundedChunks :: ![BS.ByteString]
    , boundedRetainedBytes :: !Int
    , boundedTruncated :: !Bool
    }

emptyBoundedReadState :: BoundedReadState
emptyBoundedReadState = BoundedReadState
    { boundedChunks = []
    , boundedRetainedBytes = 0
    , boundedTruncated = False
    }

retainBoundedChunk :: BoundedReadState -> BS.ByteString -> BoundedReadState
retainBoundedChunk state chunk =
    BoundedReadState
        { boundedChunks =
            if BS.null kept
                then state.boundedChunks
                else kept : state.boundedChunks
        , boundedRetainedBytes =
            state.boundedRetainedBytes + BS.length kept
        , boundedTruncated =
            state.boundedTruncated || BS.length kept < BS.length chunk
        }
  where
    room = max 0 (maxProcessOutputBytes - state.boundedRetainedBytes)
    kept = BS.take room chunk

readBounded :: Handle -> IO (BS.ByteString, Bool)
readBounded handle = do
    finalState <-
        drain emptyBoundedReadState `finally` closeQuietly handle
    pure
        ( BS.concat (reverse finalState.boundedChunks)
        , finalState.boundedTruncated
        )
  where
    drain state = do
        chunk <- BS.hGetSome handle (64 * 1024)
        if BS.null chunk
            then pure state
            else do
                let nextState = retainBoundedChunk state chunk
                nextState `seq` drain nextState

terminateProcessGroup
    :: Signal
    -> Maybe ProcessID
    -> ProcessHandle
    -> IO ()
terminateProcessGroup signal processGroup process = do
    pid <- maybe (getPid process) (pure . Just) processGroup
    case pid of
        Nothing -> do
            _ <- tryAny (terminateProcess process)
            pure ()
        Just processId -> do
            _ <- tryAny (signalProcessGroup signal processId)
            pure ()

closeQuietly :: Handle -> IO ()
closeQuietly handle = do
    _ <- tryAny (hClose handle)
    pure ()

nonInteractiveEnvironment :: FilePath -> FilePath -> IO [(String, String)]
nonInteractiveEnvironment root executable = do
    inherited <- getEnvironment
    let blocked =
            [ "GIT_TERMINAL_PROMPT"
            , "GCM_INTERACTIVE"
            , "GH_PROMPT_DISABLED"
            , "GH_REPO"
            , "GH_HOST"
            , "SSH_ASKPASS_REQUIRE"
            , "GIT_DIR"
            , "GIT_WORK_TREE"
            , "GIT_INDEX_FILE"
            , "GIT_OBJECT_DIRECTORY"
            , "GIT_ALTERNATE_OBJECT_DIRECTORIES"
            , "GIT_COMMON_DIR"
            , "GIT_CONFIG_COUNT"
            , "GIT_CONFIG_KEY_0"
            , "GIT_CONFIG_VALUE_0"
            , "GIT_CONFIG_PARAMETERS"
            , "GIT_CONFIG_GLOBAL"
            , "GIT_CONFIG_SYSTEM"
            , "GIT_CONFIG_NOSYSTEM"
            , "GIT_ATTR_NOSYSTEM"
            , "GIT_CEILING_DIRECTORIES"
            , "GIT_SSH_COMMAND"
            , "GIT_ASKPASS"
            , "GIT_PROXY_COMMAND"
            , "GIT_EXEC_PATH"
            ]
        sanitized = filter
            (\(name, _) ->
                name `notElem` blocked
                    && not ("GIT_CONFIG_KEY_" `prefixOf` name)
                    && not ("GIT_CONFIG_VALUE_" `prefixOf` name))
            inherited
    safePath <-
        case lookup "PATH" sanitized of
            Nothing -> pure Nothing
            Just value -> sanitizeSearchPathOutside root value
    let
        sanitizedWithSafePath =
            case safePath of
                Nothing -> filter ((/= "PATH") . fst) sanitized
                Just value ->
                    ("PATH", value) : filter ((/= "PATH") . fst) sanitized
        retained
            | executable == "git" =
                filter (\(name, _) -> name `elem` gitEnvironmentAllowlist)
                    sanitizedWithSafePath
            | otherwise = sanitizedWithSafePath
    pure
        ( [ ("GIT_TERMINAL_PROMPT", "0")
          , ("GCM_INTERACTIVE", "never")
          , ("GH_PROMPT_DISABLED", "true")
          , ("SSH_ASKPASS_REQUIRE", "never")
          ]
            <> retained
        )
  where
    prefixOf prefix value = take (length prefix) value == prefix
    gitEnvironmentAllowlist =
        [ "PATH"
        , "HOME"
        , "TMPDIR"
        , "TMP"
        , "TEMP"
        , "LANG"
        , "LC_ALL"
        , "LC_CTYPE"
        , "USER"
        , "LOGNAME"
        , "SSH_AUTH_SOCK"
        , "XDG_CONFIG_HOME"
        , "XDG_CONFIG_DIRS"
        , "SSL_CERT_FILE"
        , "SSL_CERT_DIR"
        , "NIX_SSL_CERT_FILE"
        , "TERM"
        ]

trySynchronous :: IO value -> IO (Either SomeException value)
trySynchronous action =
    tryAny action >>= \case
        Left exception
            | isAsyncException exception -> throwIO exception
            | otherwise -> pure (Left exception)
        Right value -> pure (Right value)

processPipeTeardownMicros :: Int
processPipeTeardownMicros = 1_000_000

processGroupTermGraceMicros :: Int
processGroupTermGraceMicros = 2_000_000

maxProcessOutputBytes :: Int
maxProcessOutputBytes = 1024 * 1024
