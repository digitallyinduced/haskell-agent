-- | Lifecycle-owned child @agent-cli@ processes for non-terminal frontends.
module Agent.CLI.AgentSessions.Process
    ( SessionProcessLifetime(..)
    , SessionProcessManager
    , closeSessionProcessManager
    , launchManagedTurn
    , launchManagedTurnBounded
    , launchSessionTurn
    , newSessionProcessManager
    , newSessionProcessManagerWithLifetime
    , sessionProcessStatus
    , signalManagedSessionReady
    ) where

import Agent.CLI.Error (formatException)
import Agent.CLI.ManagedTurn (ManagedTurnRequest)
import Agent.CLI.Runtime.Options (ApprovalPolicy(..))
import Agent.CLI.Session
    ( SessionHandle(..)
    , SessionMeta(..)
    )
import Agent.CLI.SessionLock
    ( sessionLockIsActive
    , sessionLockPath
    )
import Agent.Concurrent (forConcurrentlyBounded_)
import Agent.OsPath (unsafeToFilePath)
import Agent.Process
    ( terminateProcessGroupWith
    , terminateThenKillPolicy
    )
import Agent.Tools.IO (sessionTempProcessEnv)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , modifyMVar_
    , newEmptyMVar
    , newMVar
    , putMVar
    , readMVar
    )
import Control.Exception.Safe
    ( SomeException
    , finally
    , try
    )
import Control.Monad (void)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.Directory
    ( findExecutable
    , removeFile
    )
import System.Environment (getEnvironment, getExecutablePath, lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath (takeFileName)
import qualified System.FilePath as FilePath
import System.IO (IOMode(AppendMode), hClose, openTempFile, withFile)
import System.OsPath (OsPath, unsafeEncodeUtf, (</>))
import System.Posix.Files (setFileMode)
import System.Process
    ( CreateProcess(..)
    , ProcessHandle
    , StdStream(..)
    , createProcess
    , getPid
    , getProcessExitCode
    , proc
    , terminateProcess
    , waitForProcess
    )
import qualified System.Timeout as Timeout

data ManagedSessionProcess
    = ManagedSessionStarting !Int !(MVar ())
    | ManagedSessionRunning !Int !ProcessHandle

data SessionProcessState = SessionProcessState
    { sessionManagerLifecycle :: !SessionManagerLifecycle
    , sessionManagerProcesses :: !(Map Text ManagedSessionProcess)
    , sessionManagerNextToken :: !Int
    }

data SessionManagerLifecycle
    = SessionManagerOpen
    | SessionManagerClosing !(MVar ())
    | SessionManagerClosed

data SessionProcessManager = SessionProcessManager
    { managedRoot :: !OsPath
    , managedProcesses :: !(MVar SessionProcessState)
    , managedLifetime :: !SessionProcessLifetime
    }

data SessionProcessLifetime
    = DetachedSessionProcesses
    | ScopedSessionProcesses
    deriving (Eq, Show)

newSessionProcessManager :: OsPath -> IO SessionProcessManager
newSessionProcessManager =
    newSessionProcessManagerWithLifetime DetachedSessionProcesses

newSessionProcessManagerWithLifetime
    :: SessionProcessLifetime
    -> OsPath
    -> IO SessionProcessManager
newSessionProcessManagerWithLifetime lifetime root = do
    processes <- newMVar SessionProcessState
        { sessionManagerLifecycle = SessionManagerOpen
        , sessionManagerProcesses = Map.empty
        , sessionManagerNextToken = 0
        }
    pure SessionProcessManager
        { managedRoot = root
        , managedProcesses = processes
        , managedLifetime = lifetime
        }

-- | Start one background turn for a persisted session. A second turn is
-- rejected while the first is still running to keep transcript appends
-- serialized.
launchSessionTurn
    :: SessionProcessManager
    -> Bool
    -> ApprovalPolicy
    -> Bool
    -> Bool
    -> SessionHandle
    -> Text
    -> IO (Either Text Text)
launchSessionTurn manager background policy ghciEnabled bashEnabled handle message =
    launchSessionTurnInput
        manager
        background
        policy
        ghciEnabled
        bashEnabled
        Nothing
        handle
        (ManagedTextInput message)

data ManagedTurnInput
    = ManagedTextInput !Text
    | ManagedRequestInput !ManagedTurnRequest

launchSessionTurnInput
    :: SessionProcessManager
    -> Bool
    -> ApprovalPolicy
    -> Bool
    -> Bool
    -> Maybe Int
    -> SessionHandle
    -> ManagedTurnInput
    -> IO (Either Text Text)
launchSessionTurnInput
        manager background policy ghciEnabled bashEnabled turnTimeout handle input =
    resolveAgentExecutable >>= \case
        Left err -> pure (Left err)
        Right executable -> do
            let sessionId = handle.sessionMeta.metaId
            completion <- newEmptyMVar
            reserved <- modifyMVar manager.managedProcesses \state -> do
                busy <- case Map.lookup sessionId state.sessionManagerProcesses of
                    Nothing -> pure False
                    Just ManagedSessionStarting{} -> pure True
                    Just (ManagedSessionRunning _ managedHandle) ->
                        (== Nothing) <$> getProcessExitCode managedHandle
                if not (sessionManagerIsOpen state) || busy
                    then pure (state, Nothing)
                    else
                        let token = state.sessionManagerNextToken
                        in pure
                            ( state
                                { sessionManagerProcesses =
                                    Map.insert
                                        sessionId
                                        (ManagedSessionStarting token completion)
                                        state.sessionManagerProcesses
                                , sessionManagerNextToken = token + 1
                                }
                            , Just token
                            )
            case reserved of
                Nothing -> pure (Left ("session " <> sessionId
                    <> " is already running or its process manager is closed"))
                Just token -> (`finally` putMVar completion ()) do
                    started <- try @_ @SomeException
                        (startManagedSession executable)
                    case started of
                        Left err -> do
                            forgetSession manager sessionId token
                            pure $ Left
                                ("failed to start agent session: "
                                    <> formatException err)
                        Right (Left err) -> do
                            forgetSession manager sessionId token
                            pure (Left err)
                        Right (Right process) -> do
                            published <-
                                modifyMVar manager.managedProcesses \state ->
                                    if not (sessionManagerIsOpen state)
                                        then pure (state, False)
                                        else pure
                                            ( state
                                                { sessionManagerProcesses =
                                                    Map.insert sessionId
                                                        (ManagedSessionRunning token process)
                                                        state.sessionManagerProcesses
                                                }
                                            , True
                                            )
                            if not published
                                then do
                                    terminateManagedProcess process
                                    pure (Left
                                        "session process manager closed during startup")
                                else if background
                                    then pure (Right ("started session " <> sessionId))
                                    else do
                                        exitResult <- case turnTimeout of
                                            Nothing ->
                                                Right <$> waitForProcess process
                                            Just micros ->
                                                Timeout.timeout micros
                                                    (waitForManagedExit process)
                                                        >>= \case
                                                            Nothing -> do
                                                                terminateManagedProcess
                                                                    process
                                                                pure (Left
                                                                    "agent session timed out")
                                                            Just exitCode ->
                                                                pure (Right exitCode)
                                        forgetSession manager sessionId token
                                        pure case exitResult of
                                            Left err -> Left err
                                            Right ExitSuccess ->
                                                Right
                                                    ("completed session " <> sessionId)
                                            Right (ExitFailure code) ->
                                                Left
                                                    ("session failed with exit code "
                                                        <> Text.pack (show code))
  where
    sessionId = handle.sessionMeta.metaId

    startManagedSession executable = do
        parentEnv <- getEnvironment
        (inputPath, inputHandle) <- openTempFile
            (unsafeToFilePath handle.sessionTempDir) ".agent-turn-"
        case input of
            ManagedTextInput message ->
                TextIO.hPutStr inputHandle message
            ManagedRequestInput request ->
                LBS.hPut inputHandle (encode request)
        hClose inputHandle
        setFileMode inputPath 0o600
        (readyPath, readyHandle) <- openTempFile
            (unsafeToFilePath handle.sessionDir) ".agent-ready-"
        hClose readyHandle
        setFileMode readyPath 0o600
        let childEnv =
                (managedSessionReadyEnv, readyPath)
                    : sessionTempProcessEnv handle.sessionTempDir
                        (filter
                            (\(name, _) ->
                                name /= managedSessionReadyEnv
                                    && name `notElem` gatewayOnlyEnv)
                            parentEnv)
            logPath = unsafeToFilePath handle.sessionDir FilePath.</> "agent.log"
            approvalArgs = case policy of
                ApproveAll -> ["--yolo"]
                DenyMutating -> ["--managed-deny-mutations"]
                PromptMutating -> ["--no-yolo"]
            inputArgs = case input of
                ManagedTextInput _ -> ["--prompt-file", inputPath]
                ManagedRequestInput _ -> ["--managed-turn-file", inputPath]
            agentArgs =
                [ "--resume", Text.unpack sessionId
                ]
                    <> inputArgs
                    <> [ "--save-session"
                       ]
                    <> approvalArgs
                    <> ["--no-ghci" | not ghciEnabled]
                    <> ["--bash" | bashEnabled]
            cleanupScript =
                "prompt=$1; shift; "
                    <> "cleanup() { rm -f \"$prompt\"; }; "
                    <> "trap cleanup EXIT HUP INT TERM; "
                    <> "\"$@\""
            args =
                [ "-c", cleanupScript
                , "agent-session-runner"
                , inputPath
                , executable
                ]
                    <> agentArgs
        started <- try @_ @SomeException do
            withFile logPath AppendMode \logHandle ->
                setFileMode logPath 0o600 >>
                createProcess (proc "/bin/sh" args)
                    { cwd = Just (unsafeToFilePath handle.sessionMeta.metaCwd)
                    , std_in = NoStream
                    , std_out = UseHandle logHandle
                    , std_err = UseHandle logHandle
                    , create_group = True
                    , env = Just childEnv
                    }
        case started of
            Left err -> do
                removePrivateFile inputPath
                removePrivateFile readyPath
                pure $ Left
                    ("failed to start agent session: " <> formatException err)
            Right (_, _, _, process) -> do
                ready <- Timeout.timeout 30_000_000
                    (waitForManagedSessionReady process readyPath) >>= \case
                        Nothing -> do
                            _ <- try @_ @SomeException (terminateProcess process)
                            _ <- try @_ @SomeException (waitForProcess process)
                            pure (Left
                                "agent session did not become ready within 30 seconds")
                        Just result -> pure result
                removePrivateFile readyPath
                case ready of
                    Left err -> do
                        _ <- waitForProcess process
                        pure (Left err)
                    Right () -> pure (Right process)

-- | Launch a structured gateway turn through the private request-file
-- interface, without exposing gateway credentials to the child.
launchManagedTurn
    :: SessionProcessManager
    -> Bool
    -> ApprovalPolicy
    -> Bool
    -> Bool
    -> SessionHandle
    -> ManagedTurnRequest
    -> IO (Either Text Text)
launchManagedTurn manager background policy ghciEnabled bashEnabled handle request =
    launchManagedTurnBounded
        manager
        background
        policy
        ghciEnabled
        bashEnabled
        Nothing
        handle
        request

launchManagedTurnBounded
    :: SessionProcessManager
    -> Bool
    -> ApprovalPolicy
    -> Bool
    -> Bool
    -> Maybe Int
    -> SessionHandle
    -> ManagedTurnRequest
    -> IO (Either Text Text)
launchManagedTurnBounded
        manager background policy ghciEnabled bashEnabled turnTimeout handle request =
    launchSessionTurnInput
        manager
        background
        policy
        ghciEnabled
        bashEnabled
        turnTimeout
        handle
        (ManagedRequestInput request)

forgetSession :: SessionProcessManager -> Text -> Int -> IO ()
forgetSession manager sessionId token =
    modifyMVar_ manager.managedProcesses \state ->
        let matches = case Map.lookup sessionId state.sessionManagerProcesses of
                Just (ManagedSessionStarting current _) -> current == token
                Just (ManagedSessionRunning current _) -> current == token
                Nothing -> False
        in pure if matches
            then state
                { sessionManagerProcesses =
                    Map.delete sessionId state.sessionManagerProcesses
                }
            else state

gatewayOnlyEnv :: [String]
gatewayOnlyEnv =
    [ "TELEGRAM_BOT_TOKEN"
    , "TELEGRAM_ALLOWED_USERS"
    ]

sessionProcessStatus :: SessionProcessManager -> Text -> IO Text
sessionProcessStatus manager sessionId =
    modifyMVar manager.managedProcesses \state ->
        case Map.lookup sessionId state.sessionManagerProcesses of
            Nothing -> do
                locked <- sessionLockIsActive
                    (sessionLockPath
                        (manager.managedRoot
                            </> unsafeEncodeUtf (Text.unpack sessionId)))
                pure (state, if locked then "running" else "idle")
            Just ManagedSessionStarting{} ->
                pure (state, "running")
            Just (ManagedSessionRunning _ managedHandle) ->
                -- Keep the exited process record rather than deleting it on
                -- read: repeated polls must keep reporting the terminal state.
                -- A later launch can replace an exited process record.
                getProcessExitCode managedHandle >>= \case
                    Nothing -> pure (state, "running")
                    Just ExitSuccess -> pure (state, "completed")
                    Just (ExitFailure code) ->
                        pure
                            (state, "failed (" <> Text.pack (show code) <> ")")

closeSessionProcessManager :: SessionProcessManager -> IO ()
closeSessionProcessManager manager = do
    decision <- modifyMVar manager.managedProcesses \state ->
        case state.sessionManagerLifecycle of
            SessionManagerClosed -> pure (state, Left Nothing)
            SessionManagerClosing completion ->
                pure (state, Left (Just completion))
            SessionManagerOpen -> do
                completion <- newEmptyMVar
                pure
                    ( state
                        { sessionManagerLifecycle =
                            SessionManagerClosing completion
                        , sessionManagerProcesses = Map.empty
                        }
                    , Right
                        ( completion
                        , Map.elems state.sessionManagerProcesses
                        )
                    )
    case decision of
        Left Nothing -> pure ()
        Left (Just completion) -> readMVar completion
        Right (completion, processes) ->
            closeProcesses processes `finally` do
                modifyMVar_ manager.managedProcesses \state ->
                    pure state
                        { sessionManagerLifecycle = SessionManagerClosed
                        }
                putMVar completion ()
  where
    closeProcesses processes = do
        let waitStarting = \case
                ManagedSessionStarting _ completion -> readMVar completion
                ManagedSessionRunning _ _ -> pure ()
            closeRunning = \case
                ManagedSessionStarting{} -> pure ()
                ManagedSessionRunning _ managedHandle ->
                    getProcessExitCode managedHandle >>= \case
                        Just _ ->
                            void $ try @_ @SomeException
                                (waitForProcess managedHandle)
                        Nothing ->
                            case manager.managedLifetime of
                                DetachedSessionProcesses -> pure ()
                                ScopedSessionProcesses ->
                                    terminateManagedProcess managedHandle
        forConcurrentlyBounded_ 8 waitStarting processes
        forConcurrentlyBounded_ 8 closeRunning processes

sessionManagerIsOpen :: SessionProcessState -> Bool
sessionManagerIsOpen state =
    case state.sessionManagerLifecycle of
        SessionManagerOpen -> True
        SessionManagerClosing{} -> False
        SessionManagerClosed -> False

waitForManagedExit :: ProcessHandle -> IO ExitCode
waitForManagedExit = waitForProcess

terminateManagedProcess :: ProcessHandle -> IO ()
terminateManagedProcess process = do
    processGroup <- getPid process
    terminateProcessGroupWith terminateThenKillPolicy processGroup process

signalManagedSessionReady :: Either Text () -> IO ()
signalManagedSessionReady result =
    lookupEnv managedSessionReadyEnv >>= \case
        Nothing -> pure ()
        Just path -> TextIO.writeFile path $ case result of
            Right () -> "ready\n"
            Left err -> "error\n" <> err

waitForManagedSessionReady :: ProcessHandle -> FilePath -> IO (Either Text ())
waitForManagedSessionReady process path = go
  where
    go = do
        contents <- try @_ @SomeException (TextIO.readFile path)
        case contents of
            Right "ready\n" -> pure (Right ())
            Right text
                | Just err <- Text.stripPrefix "error\n" text ->
                    pure (Left err)
            _ ->
                getProcessExitCode process >>= \case
                    Nothing -> threadDelay 10000 >> go
                    Just ExitSuccess ->
                        pure (Left "agent session exited before acquiring its lock")
                    Just (ExitFailure code) ->
                        pure $ Left
                            ("agent session exited before acquiring its lock (exit code "
                                <> Text.pack (show code) <> ")")

removePrivateFile :: FilePath -> IO ()
removePrivateFile path = do
    _ <- try @_ @SomeException (removeFile path)
    pure ()

managedSessionReadyEnv :: String
managedSessionReadyEnv = "HASKELL_AGENT_MANAGED_SESSION_READY"

resolveAgentExecutable :: IO (Either Text FilePath)
resolveAgentExecutable = do
    override <- lookupEnv "HASKELL_AGENT_EXECUTABLE"
    case nonEmpty override of
        Just executable -> pure (Right executable)
        Nothing -> do
            current <- getExecutablePath
            if takeFileName current == "agent-cli"
                then pure (Right current)
                else findExecutable "agent-cli" >>= \case
                    Just executable -> pure (Right executable)
                    Nothing -> pure $ Left
                        "could not find agent-cli; set HASKELL_AGENT_EXECUTABLE"
  where
    nonEmpty = \case
        Just value | not (null value) -> Just value
        _ -> Nothing
