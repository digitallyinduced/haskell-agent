-- | Tools for creating, inspecting, and continuing persisted top-level agent
-- sessions. CLI callers can run turns in tracked background threads, while
-- gateways retain the managed @agent-cli@ process runner.
module Agent.CLI.AgentSessions
    ( AgentSessionToolsEnv(..)
    , SessionThreadManager
    , SessionProcessLifetime(..)
    , SessionProcessManager
    , agentSessionTools
    , closeSessionThreadManager
    , closeSessionProcessManager
    , launchSessionThread
    , launchManagedTurn
    , launchManagedTurnBounded
    , launchSessionTurn
    , newSessionThreadManager
    , newSessionProcessManager
    , newSessionProcessManagerWithLifetime
    , signalManagedSessionReady
    , sessionThreadStatus
    , sessionProcessStatus
    ) where

import Agent.CLI.Options (ApprovalPolicy(..))
import Agent.CLI.ManagedTurn (ManagedTurnRequest)
import Agent.CLI.PendingInteraction
    ( ExternalInteractionResponse(..)
    , canonicalizeExternalInteractionResponse
    )
import Agent.CLI.AgentSessions.Render (renderAgentSession)
import Agent.CLI.Error (formatException)
import Agent.Process
    ( terminateProcessGroupWith
    , terminateThenKillPolicy
    )
import Agent.CLI.Session
    ( SessionCreate(..)
    , SessionHandle(..)
    , SessionMeta(..)
    , SessionTurnPage(..)
    , createSession
    , loadSessionActivity
    , loadRecentSessionTurns
    , loadSessionMeta
    , sessionTempDirForId
    , sessionTitleFromPrompt
    )
import Agent.Store.Postgres.Connection (StorePool)
import Agent.Store.Postgres.Interaction
    ( InteractionOrigin(..)
    , InteractionResolution(..)
    , InteractionResolutionRequest(..)
    , InteractionResolveResult(..)
    , SessionInteraction(..)
    , loadSessionInteraction
    , listOpenSessionInteractions
    , resolveSessionInteraction
    )
import Agent.Store.Types (renderStoreError)
import Agent.CLI.SessionLock
    ( sessionLockIsActive
    , sessionLockPath
    )
import Agent.CLI.Models
    ( ModelOption(..)
    , ModelTarget(..)
    , resolveModelOptionDialect
    )
import Agent.Concurrent (forConcurrentlyBounded_)
import Agent.OsPath (fromText, unsafeToFilePath)
import Agent.Dialect
    ( DialectId
    , dialectIdForModel
    )
import Agent.Provider (Provider)
import Agent.Json.Decode (optionalKey)
import qualified Agent.Json.Decode as Hermes
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.ToolDispatch (typedTool)
import Agent.Tools.Types
    ( AppTool
    , PlanModeCapability(..)
    , ToolExecutionPolicy(..)
    , jsonTool
    , withPlanModeCapability
    )
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
    ( Async
    , asyncWithUnmask
    , cancel
    , poll
    , waitCatch
    )
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , modifyMVar_
    , newEmptyMVar
    , newMVar
    , putMVar
    , readMVar
    , takeMVar
    )
import Control.Exception.Safe
    ( SomeException
    , finally
    , mask
    , try
    , tryAny
    )
import Control.Monad (void)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Time.Clock (getCurrentTime)
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

data AgentSessionToolsEnv = AgentSessionToolsEnv
    { toolsPool :: !StorePool
    , toolsRoot :: !OsPath
    , toolsProvider :: !Provider
    , toolsConnection :: !Text
    , toolsModel :: !Text
    , toolsTransportModel :: !Text
    , toolsDialect :: !DialectId
    , toolsCwd :: !OsPath
    , toolsEffort :: !Text
    , toolsCurrentSessionId :: !(IO (Maybe Text))
    , toolsLaunchTurn :: !(SessionHandle -> Text -> IO (Either Text Text))
    , toolsSessionStatus :: !(Text -> IO Text)
    }

data ManagedSessionThread
    = ManagedSessionThreadRunning !(Async ())
    | ManagedSessionThreadCompleted
    | ManagedSessionThreadFailed !Text

data SessionThreadManagerState = SessionThreadManagerState
    { threadManagerClosed :: !Bool
    , managedThreads :: !(Map Text ManagedSessionThread)
    }

data SessionThreadManager = SessionThreadManager
    { threadManagerRoot :: !OsPath
    , threadManagerState :: !(MVar SessionThreadManagerState)
    }

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

newSessionThreadManager :: OsPath -> IO SessionThreadManager
newSessionThreadManager root = do
    state <- newMVar SessionThreadManagerState
        { threadManagerClosed = False
        , managedThreads = Map.empty
        }
    pure SessionThreadManager
        { threadManagerRoot = root
        , threadManagerState = state
        }

-- | Start one in-process background turn. The task is registered before it is
-- allowed to execute, so shutdown can always cancel and join every live turn.
launchSessionThread
    :: SessionThreadManager
    -> Text
    -> IO (Either Text ())
    -> IO (Either Text Text)
launchSessionThread manager sessionId action =
    mask \_ -> do
        launched <- modifyMVar manager.threadManagerState \state ->
            if state.threadManagerClosed
                then pure (state, Left "agent session manager is closed")
                else case Map.lookup sessionId state.managedThreads of
                    Just (ManagedSessionThreadRunning _) ->
                        pure
                            ( state
                            , Left
                                ("session " <> sessionId
                                    <> " is already running")
                            )
                    _ -> do
                        gate <- newEmptyMVar
                        started <- tryAny $
                            asyncWithUnmask \unmask -> do
                                takeMVar gate
                                result <- tryAny (unmask action)
                                let terminal = case result of
                                        Left err ->
                                            ManagedSessionThreadFailed
                                                (formatException err)
                                        Right (Left err) ->
                                            ManagedSessionThreadFailed err
                                        Right (Right ()) ->
                                            ManagedSessionThreadCompleted
                                modifyMVar_ manager.threadManagerState \current ->
                                    pure $
                                        if current.threadManagerClosed
                                            then current
                                            else current
                                                { managedThreads =
                                                    Map.insert
                                                        sessionId
                                                        terminal
                                                        current.managedThreads
                                                }
                        case started of
                            Left err ->
                                pure
                                    ( state
                                    , Left
                                        ("failed to start agent session: "
                                            <> formatException err)
                                    )
                            Right worker -> do
                                let running = state
                                        { managedThreads =
                                            Map.insert
                                                sessionId
                                                (ManagedSessionThreadRunning worker)
                                                state.managedThreads
                                        }
                                putMVar gate ()
                                pure
                                    ( running
                                    , Right ("started session " <> sessionId)
                                    )
        pure launched

sessionThreadStatus :: SessionThreadManager -> Text -> IO Text
sessionThreadStatus manager sessionId =
    modifyMVar manager.threadManagerState \state ->
        case Map.lookup sessionId state.managedThreads of
            Nothing -> do
                locked <- lockIsActive
                pure (state, if locked then "running" else "idle")
            Just (ManagedSessionThreadRunning worker) ->
                poll worker >>= \case
                    Nothing -> pure (state, "running")
                    Just (Right ()) ->
                        settle state ManagedSessionThreadCompleted "completed"
                    Just (Left err) ->
                        let message = "failed (" <> formatException err <> ")"
                        in settle state
                            (ManagedSessionThreadFailed message)
                            message
            Just ManagedSessionThreadCompleted ->
                terminalStatus state "completed"
            Just (ManagedSessionThreadFailed err) ->
                terminalStatus state ("failed (" <> err <> ")")
  where
    lockIsActive =
        sessionLockIsActive
            (sessionLockPath
                (manager.threadManagerRoot
                    </> unsafeEncodeUtf (Text.unpack sessionId)))
    -- Persist the terminal outcome instead of deleting it, so repeated status
    -- polls stay observable. The background worker itself records the same
    -- terminal constructor on exit (launchSessionThread); deleting it here
    -- destroyed that record, making a failed session report "idle" on the
    -- second poll (and never report its failure at all when a poll landed
    -- while the session lock was still held). A still-active lock only masks
    -- the outcome as "running" for this poll; the retained record surfaces the
    -- real status once the lock clears.
    settle state record terminal = do
        locked <- lockIsActive
        pure
            ( state
                { managedThreads =
                    Map.insert sessionId record state.managedThreads
                }
            , if locked then "running" else terminal
            )
    terminalStatus state terminal = do
        locked <- lockIsActive
        pure (state, if locked then "running" else terminal)

closeSessionThreadManager :: SessionThreadManager -> IO ()
closeSessionThreadManager manager = do
    workers <- modifyMVar manager.threadManagerState \state ->
        let running =
                [ worker
                | ManagedSessionThreadRunning worker <-
                    Map.elems state.managedThreads
                ]
        in pure
            ( state
                { threadManagerClosed = True
                , managedThreads = Map.empty
                }
            , running
            )
    mapM_ cancel workers
    mapM_ (void . waitCatch) workers

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
                    : filter
                        (\(name, _) ->
                            name /= managedSessionReadyEnv
                                && name `notElem` gatewayOnlyEnv)
                        parentEnv
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
                -- read: getProcessExitCode returns a stable code once the
                -- process has exited, so retaining the handle lets repeated
                -- polls keep reporting "completed"/"failed" instead of
                -- decaying to "idle" after the first read. Re-launch tolerates
                -- a retained exited record (startSessionProcess treats a
                -- Just exit code as not-running).
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

agentSessionTools :: AgentSessionToolsEnv -> [AppTool]
agentSessionTools env =
    [ createAgentSessionTool env
    , readAgentSessionTool env
    , sendAgentSessionMessageTool env
    , respondAgentSessionInteractionTool env
    ]

data CreateAgentSessionArgs = CreateAgentSessionArgs
    { message :: Text
    , title :: Maybe Text
    , model :: Maybe Text
    , reasoningEffort :: Maybe Text
    }

createAgentSessionArgsDecoder :: Hermes.Decoder CreateAgentSessionArgs
createAgentSessionArgsDecoder = Hermes.object $
    CreateAgentSessionArgs
        <$> Hermes.atKey "message" Hermes.text
        <*> optionalKey "title" Hermes.text
        <*> optionalKey "model" Hermes.text
        <*> optionalKey "reasoning_effort" Hermes.text

createAgentSessionTool :: AgentSessionToolsEnv -> AppTool
createAgentSessionTool env = jsonTool
    "create_agent_session"
    "Create a persisted top-level agent session and start its first turn in the background. Returns the session id and status as readable text."
    [ PropertySchema "message" PropertyString True $ Just
        "Initial task or message for the new agent session."
    , PropertySchema "title" PropertyString False $ Just
        "Optional session title. Defaults to a title derived from the message."
    , PropertySchema "model" PropertyString False $ Just
        "Optional model override. Defaults to the current session model."
    , PropertySchema "reasoning_effort" PropertyString False $ Just
        "Optional reasoning-effort override. Defaults to the current session effort."
    ]
    False
    TurnSequential
    (typedTool "create_agent_session" createAgentSessionArgsDecoder
        (runCreateAgentSession env))

runCreateAgentSession
    :: AgentSessionToolsEnv
    -> CreateAgentSessionArgs
    -> IO (Either Text Text)
runCreateAgentSession env args
    | Text.null (Text.strip args.message) =
        pure (Left "create_agent_session requires a non-empty message")
    | maybe False ((> 100) . Text.length . Text.strip) args.title =
        pure (Left "create_agent_session title must be at most 100 characters")
    | otherwise = do
        let model = fromMaybe env.toolsModel args.model
        target <- case args.model of
            Nothing ->
                pure ModelOption
                    { modelTarget = ModelTarget
                        { targetProvider = env.toolsProvider
                        , targetConnectionId = env.toolsConnection
                        , targetModelId = model
                        , targetWireModelId = env.toolsTransportModel
                        , targetDialect = env.toolsDialect
                        }
                    , modelLabel = Nothing
                    , modelFallbackPriority = Nothing
                    }
            Just _ ->
                resolveModelOptionDialect ModelOption
                    { modelTarget = ModelTarget
                        { targetProvider = env.toolsProvider
                        , targetConnectionId = env.toolsConnection
                        , targetModelId = model
                        , targetWireModelId = model
                        , targetDialect =
                            dialectIdForModel env.toolsProvider model
                        }
                    , modelLabel = Nothing
                    , modelFallbackPriority = Nothing
                    }
        let title = case Text.strip <$> args.title of
                Just value | not (Text.null value) -> value
                _ -> sessionTitleFromPrompt args.message
            spec = SessionCreate
                { createPool = env.toolsPool
                , createRoot = env.toolsRoot
                , createTarget = target.modelTarget
                , createCwd = env.toolsCwd
                , createEffort = fromMaybe env.toolsEffort args.reasoningEffort
                , createTitleHint = Just title
                , createTitleIsManual =
                    maybe False (not . Text.null . Text.strip) args.title
                }
        handle <- createSession spec
        launchToolSessionTurn env handle args.message >>= \case
            Left err -> pure $ Left $
                "created session " <> handle.sessionMeta.metaId
                    <> " but failed to start it: " <> err
            Right result -> pure (Right result)

data ReadAgentSessionArgs = ReadAgentSessionArgs
    { sessionId :: Text
    , limit :: Maybe Int
    }

readAgentSessionArgsDecoder :: Hermes.Decoder ReadAgentSessionArgs
readAgentSessionArgsDecoder = Hermes.object $
    ReadAgentSessionArgs
        <$> Hermes.atKey "session_id" Hermes.text
        <*> optionalKey "limit" Hermes.int

readAgentSessionTool :: AgentSessionToolsEnv -> AppTool
readAgentSessionTool env = jsonTool
    "read_agent_session"
    "Read metadata and recent user/assistant turns from a persisted agent session as readable labeled text."
    [ PropertySchema "session_id" PropertyString True $ Just
        "Persisted session id returned by create_agent_session or shown by /session."
    , PropertySchema "limit" PropertyInteger False $ Just
        "Maximum number of most recent turns to return. Defaults to 20; maximum 100."
    ]
    True
    ParallelSafe
    (typedTool "read_agent_session" readAgentSessionArgsDecoder
        (runReadAgentSession env))

runReadAgentSession
    :: AgentSessionToolsEnv
    -> ReadAgentSessionArgs
    -> IO (Either Text Text)
runReadAgentSession env args =
    loadSessionMeta env.toolsPool env.toolsRoot args.sessionId >>= \case
        Left err -> pure (Left err)
        Right meta -> do
            let limit = min 100 (max 1 (fromMaybe 20 args.limit))
            loadRecentSessionTurns
                env.toolsPool env.toolsRoot args.sessionId limit >>= \case
                    Left err -> pure (Left err)
                    Right page -> do
                        status <- env.toolsSessionStatus args.sessionId
                        pending <-
                            listOpenSessionInteractions
                                env.toolsPool
                                args.sessionId
                        activity <-
                            if status == "running"
                                then loadSessionActivity
                                    env.toolsRoot args.sessionId
                                else pure Nothing
                        pure do
                            interactions <-
                                either
                                    (Left
                                        . ("could not load pending interactions: " <>)
                                        . renderStoreError)
                                    Right
                                    pending
                            let displayStatus
                                    | null interactions = status
                                    | otherwise = "awaiting input"
                            Right $
                                renderAgentSession
                                    meta
                                    displayStatus
                                    activity
                                    (map snd page.pageTurns)
                                    <> renderPendingInteractions interactions

data SendAgentSessionMessageArgs = SendAgentSessionMessageArgs
    { sessionId :: Text
    , message :: Text
    }

sendAgentSessionMessageArgsDecoder
    :: Hermes.Decoder SendAgentSessionMessageArgs
sendAgentSessionMessageArgsDecoder = Hermes.object $
    SendAgentSessionMessageArgs
        <$> Hermes.atKey "session_id" Hermes.text
        <*> Hermes.atKey "message" Hermes.text

sendAgentSessionMessageTool :: AgentSessionToolsEnv -> AppTool
sendAgentSessionMessageTool env = jsonTool
    "send_agent_session_message"
    "Send a message to a persisted agent session by starting a resumed background turn. Returns the session id and status as readable text; fails if that session is already running."
    [ PropertySchema "session_id" PropertyString True $ Just
        "Persisted target session id."
    , PropertySchema "message" PropertyString True $ Just
        "Message or follow-up task for the target session."
    ]
    False
    TurnSequential
    (typedTool "send_agent_session_message" sendAgentSessionMessageArgsDecoder
        (runSendAgentSessionMessage env))

runSendAgentSessionMessage
    :: AgentSessionToolsEnv
    -> SendAgentSessionMessageArgs
    -> IO (Either Text Text)
runSendAgentSessionMessage env args
    | Text.null (Text.strip args.message) =
        pure (Left "send_agent_session_message requires a non-empty message")
    | otherwise = do
        current <- env.toolsCurrentSessionId
        if current == Just args.sessionId
            then pure (Left "cannot message the current agent session")
            else loadSessionMeta
                    env.toolsPool env.toolsRoot args.sessionId >>= \case
                Left err -> pure (Left err)
                Right meta ->
                    launchToolSessionTurn
                        env
                        (sessionHandle env.toolsPool env.toolsRoot meta)
                        args.message

data RespondAgentSessionInteractionArgs =
    RespondAgentSessionInteractionArgs
        { sessionId :: Text
        , interactionId :: Text
        , response :: Text
        }

respondAgentSessionInteractionArgsDecoder
    :: Hermes.Decoder RespondAgentSessionInteractionArgs
respondAgentSessionInteractionArgsDecoder = Hermes.object $
    RespondAgentSessionInteractionArgs
        <$> Hermes.atKey "session_id" Hermes.text
        <*> Hermes.atKey "interaction_id" Hermes.text
        <*> Hermes.atKey "response" Hermes.text

respondAgentSessionInteractionTool :: AgentSessionToolsEnv -> AppTool
respondAgentSessionInteractionTool env =
    withPlanModeCapability PlanModeInteraction $
    jsonTool
    "respond_agent_session_interaction"
    "Resolve a pending approval or question for a persisted agent session. The first valid response wins; concurrent responders observe the winning response."
    [ PropertySchema "session_id" PropertyString True $ Just
        "Persisted target session id."
    , PropertySchema "interaction_id" PropertyString True $ Just
        "Pending interaction id shown by read_agent_session."
    , PropertySchema "response" PropertyString True $ Just
        "Typed response payload requested by the interaction (for example approve, revise feedback, abandon, defer, or question answers)."
    ]
    False
    TurnSequential
    (typedTool
        "respond_agent_session_interaction"
        respondAgentSessionInteractionArgsDecoder
        (runRespondAgentSessionInteraction env))

runRespondAgentSessionInteraction
    :: AgentSessionToolsEnv
    -> RespondAgentSessionInteractionArgs
    -> IO (Either Text Text)
runRespondAgentSessionInteraction env args
    | Text.null (Text.strip args.response) =
        pure
            (Left
                "respond_agent_session_interaction requires a non-empty response")
    | otherwise =
        loadSessionInteraction
            env.toolsPool
            args.sessionId
            args.interactionId >>= \case
                Left err ->
                    pure
                        (Left
                            ("could not load session interaction: "
                                <> renderStoreError err))
                Right Nothing ->
                    pure (Left "session interaction was not found")
                Right (Just interaction) ->
                    case interaction.sessionInteractionResolution of
                        Just winner ->
                            pure . Right $
                                "Another responder answered first.\nWinning response: "
                                    <> winner.interactionResolutionPayload
                        Nothing ->
                            case canonicalizeExternalInteractionResponse
                                    interaction
                                    args.response of
                                Left err ->
                                    pure (Left ("invalid response: " <> err))
                                Right ExternalInteractionDefer ->
                                    pure
                                        (Right
                                            "Response deferred. The interaction remains open.")
                                Right (ExternalInteractionResolve payload) ->
                                    resolveValidated interaction payload
  where
    resolveValidated interaction payload = do
        now <- getCurrentTime
        current <- env.toolsCurrentSessionId
        let responder =
                "agent-session:"
                    <> fromMaybe "external" current
        resolveSessionInteraction
            env.toolsPool
            InteractionResolutionRequest
                { interactionResolutionRequestSessionKey = args.sessionId
                , interactionResolutionRequestInteractionId =
                    interaction.sessionInteractionId
                , interactionResolutionRequestPayloadVersion = 1
                , interactionResolutionRequestPayload = payload
                , interactionResolutionRequestResponder = responder
                , interactionResolutionRequestResolvedAt = now
                }
            >>= \case
                Left err ->
                    pure
                        (Left
                            ("could not resolve session interaction: "
                                <> renderStoreError err))
                Right InteractionResolveNotFound ->
                    pure
                        (Left
                            "session interaction was not found or its session is inactive")
                Right InteractionResolveObserved
                    { interactionResolveWon
                    , interactionResolveValue
                    } ->
                        pure . Right $
                            (if interactionResolveWon
                                then "Response accepted."
                                else "Another responder answered first.")
                                <> "\nWinning response: "
                                <> interactionResolveValue.interactionResolutionPayload

renderPendingInteractions :: [SessionInteraction] -> Text
renderPendingInteractions [] = ""
renderPendingInteractions interactions =
    "\n\nPending interactions:\n"
        <> Text.intercalate
            "\n\n"
            (map renderInteraction interactions)
  where
    renderInteraction interaction =
        Text.unlines $
            [ "Interaction: " <> interaction.sessionInteractionId
            , "Kind: " <> interaction.sessionInteractionKind
            , "Request key: " <> interaction.sessionInteractionRequestKey
            ]
                <> maybe
                    []
                    (\origin ->
                        [ "Origin: "
                            <> origin.interactionOriginToolName
                            <> " ("
                            <> origin.interactionOriginCallId
                            <> ")"
                        ])
                    interaction.sessionInteractionOrigin
                <> [ "Request:"
                   , indentInteractionPayload
                        interaction.sessionInteractionPayload
                   ]

indentInteractionPayload :: Text -> Text
indentInteractionPayload payload =
    Text.unlines
        [ "  " <> line
        | line <- Text.lines (truncateInteractionPayload payload)
        ]

truncateInteractionPayload :: Text -> Text
truncateInteractionPayload payload
    | Text.length payload <= 16000 = payload
    | otherwise =
        Text.take 16000 payload
            <> "\n… (interaction payload truncated)"

launchToolSessionTurn
    :: AgentSessionToolsEnv
    -> SessionHandle
    -> Text
    -> IO (Either Text Text)
launchToolSessionTurn env handle message =
    env.toolsLaunchTurn handle message >>= \case
        Left err -> pure (Left err)
        Right launchResult -> do
            let sessionId = handle.sessionMeta.metaId
            status <- statusAfterLaunch env sessionId launchResult
            pure $ Right $ renderSessionLaunch sessionId status

sessionHandle :: StorePool -> OsPath -> SessionMeta -> SessionHandle
sessionHandle pool root meta =
    let dir = root </> fromText meta.metaId
    in SessionHandle
        { sessionPool = pool
        , sessionDir = dir
        , sessionTempDir =
            either
                (error . Text.unpack)
                id
                (sessionTempDirForId root meta.metaId)
        , sessionMetaPath = dir </> unsafeEncodeUtf "meta.json"
        , sessionTranscriptPath =
            dir </> unsafeEncodeUtf "transcript.jsonl"
        , sessionMeta = meta
        }

statusAfterLaunch :: AgentSessionToolsEnv -> Text -> Text -> IO Text
statusAfterLaunch env sessionId launchResult
    | "completed session " `Text.isPrefixOf` launchResult = pure "completed"
    | otherwise = env.toolsSessionStatus sessionId

renderSessionLaunch :: Text -> Text -> Text
renderSessionLaunch sessionId status =
    "Session: " <> sessionId <> "\nStatus: " <> status
