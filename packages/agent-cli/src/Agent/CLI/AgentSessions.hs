-- | Tools for creating, inspecting, and continuing persisted top-level agent
-- sessions. Turns run in managed background @agent-cli@ processes so they are
-- independent from the caller's model loop while remaining resumable.
module Agent.CLI.AgentSessions
    ( AgentSessionToolsEnv(..)
    , SessionProcessLifetime(..)
    , SessionProcessManager
    , agentSessionTools
    , closeSessionProcessManager
    , launchManagedTurn
    , launchManagedTurnBounded
    , launchSessionTurn
    , newSessionProcessManager
    , newSessionProcessManagerWithLifetime
    , signalManagedSessionReady
    , sessionProcessStatus
    ) where

import Agent.CLI.Options (ApprovalPolicy(..))
import Agent.CLI.ManagedTurn (ManagedTurnRequest)
import Agent.CLI.Error (formatException)
import Agent.Process (terminateProcessGroup)
import Agent.CLI.Session
    ( SessionCreate(..)
    , SessionActivity(..)
    , SessionHandle(..)
    , SessionMeta(..)
    , SessionTurn(..)
    , createSession
    , loadSessionActivity
    , loadSession
    , sessionTempDirForId
    , sessionTitleFromPrompt
    )
import Agent.Store.Postgres.Connection (StorePool)
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
    , dialectSlug
    )
import Agent.Provider (Provider, providerSlug)
import Agent.ToolArgs (objectArgs, optInt, optText, reqText)
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.ToolDispatch (typedTool)
import Agent.Tools.Types
    ( AppTool
    , ToolExecutionPolicy(..)
    , jsonTool
    )
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
import Control.Exception.Safe (SomeException, finally, try)
import Control.Monad (void)
import Data.Aeson (FromJSON(..), encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
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

data ManagedSessionProcess
    = ManagedSessionStarting !(MVar ())
    | ManagedSessionRunning !ProcessHandle

data SessionProcessState = SessionProcessState
    { sessionManagerLifecycle :: !SessionManagerLifecycle
    , sessionManagerProcesses :: !(Map Text ManagedSessionProcess)
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
                    Just (ManagedSessionRunning managedHandle) ->
                        (== Nothing) <$> getProcessExitCode managedHandle
                if not (sessionManagerIsOpen state) || busy
                    then pure (state, False)
                    else pure
                        ( state
                            { sessionManagerProcesses =
                                Map.insert
                                    sessionId
                                    (ManagedSessionStarting completion)
                                    state.sessionManagerProcesses
                            }
                        , True
                        )
            if not reserved
                then pure (Left ("session " <> sessionId
                    <> " is already running or its process manager is closed"))
                else (`finally` putMVar completion ()) do
                    started <- try @_ @SomeException
                        (startManagedSession executable)
                    case started of
                        Left err -> do
                            forgetSession manager sessionId
                            pure $ Left
                                ("failed to start agent session: "
                                    <> formatException err)
                        Right (Left err) -> do
                            forgetSession manager sessionId
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
                                                        (ManagedSessionRunning process)
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
                                        forgetSession manager sessionId
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

forgetSession :: SessionProcessManager -> Text -> IO ()
forgetSession manager sessionId =
    modifyMVar_ manager.managedProcesses
        (\state -> pure state
            { sessionManagerProcesses =
                Map.delete sessionId state.sessionManagerProcesses
            })

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
            Just (ManagedSessionRunning managedHandle) ->
                getProcessExitCode managedHandle >>= \case
                    Nothing -> pure (state, "running")
                    Just ExitSuccess ->
                        pure
                            ( state
                                { sessionManagerProcesses =
                                    Map.delete sessionId
                                        state.sessionManagerProcesses
                                }
                            , "completed"
                            )
                    Just (ExitFailure code) ->
                        pure
                            ( state
                                { sessionManagerProcesses =
                                    Map.delete sessionId
                                        state.sessionManagerProcesses
                                }
                            , "failed (" <> Text.pack (show code) <> ")"
                            )

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
                ManagedSessionStarting completion -> readMVar completion
                ManagedSessionRunning _ -> pure ()
            closeRunning = \case
                ManagedSessionStarting{} -> pure ()
                ManagedSessionRunning managedHandle ->
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
    terminateProcessGroup processGroup process

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
    ]

data CreateAgentSessionArgs = CreateAgentSessionArgs
    { message :: Text
    , title :: Maybe Text
    , model :: Maybe Text
    , reasoningEffort :: Maybe Text
    }

instance FromJSON CreateAgentSessionArgs where
    parseJSON = objectArgs \input -> CreateAgentSessionArgs
        <$> reqText input "message"
        <*> optText input "title"
        <*> optText input "model"
        <*> optText input "reasoning_effort"

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
    (typedTool "create_agent_session" (runCreateAgentSession env))

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

instance FromJSON ReadAgentSessionArgs where
    parseJSON = objectArgs \input -> ReadAgentSessionArgs
        <$> reqText input "session_id"
        <*> optInt input "limit"

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
    (typedTool "read_agent_session" (runReadAgentSession env))

runReadAgentSession
    :: AgentSessionToolsEnv
    -> ReadAgentSessionArgs
    -> IO (Either Text Text)
runReadAgentSession env args =
    loadSession env.toolsPool env.toolsRoot args.sessionId >>= \case
        Left err -> pure (Left err)
        Right (meta, turns) -> do
            status <- env.toolsSessionStatus args.sessionId
            activity <-
                if status == "running"
                    then loadSessionActivity env.toolsRoot args.sessionId
                    else pure Nothing
            let limit = min 100 (max 1 (fromMaybe 20 args.limit))
                recent = drop (max 0 (length turns - limit)) turns
            pure $ Right $ renderAgentSession meta status activity recent

data SendAgentSessionMessageArgs = SendAgentSessionMessageArgs
    { sessionId :: Text
    , message :: Text
    }

instance FromJSON SendAgentSessionMessageArgs where
    parseJSON = objectArgs \input -> SendAgentSessionMessageArgs
        <$> reqText input "session_id"
        <*> reqText input "message"

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
    (typedTool "send_agent_session_message" (runSendAgentSessionMessage env))

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
            else loadSession env.toolsPool env.toolsRoot args.sessionId >>= \case
                Left err -> pure (Left err)
                Right (meta, _) ->
                    launchToolSessionTurn
                        env
                        (sessionHandle env.toolsPool env.toolsRoot meta)
                        args.message

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

renderAgentSession
    :: SessionMeta
    -> Text
    -> Maybe SessionActivity
    -> [SessionTurn]
    -> Text
renderAgentSession meta status activity turns =
    Text.intercalate "\n" $
        [ "Session"
        , "  ID: " <> meta.metaId
        , "  Status: " <> status
        , "  Title: " <> meta.metaTitle
        , "  Provider: " <> providerSlug meta.metaProvider
        , "  Connection: " <> meta.metaConnection
        , "  Model: " <> meta.metaModel
        , "  Dialect: " <> dialectSlug meta.metaDialect
        , "  Reasoning effort: " <> meta.metaEffort
        , "  Working directory: " <> Text.pack (unsafeToFilePath meta.metaCwd)
        , "  Created at: " <> Text.pack (show meta.metaCreatedAt)
        , "  Updated at: " <> Text.pack (show meta.metaUpdatedAt)
        ]
            <> maybe [] renderActivity activity
            <> ["", "Recent turns: " <> Text.pack (show (length turns))]
            <> case turns of
                [] -> ["  (none)"]
                _ -> [""] <> intercalateBlank
                    (zipWith renderSessionTurn [1 :: Int ..] turns)

renderActivity :: SessionActivity -> [Text]
renderActivity activity =
    [ ""
    , "Current activity"
    , "  Kind: " <> activity.activityKind
    , "  Message: " <> activity.activityMessage
    ]
        <> maybe []
            (\retryAt -> ["  Retry at: " <> Text.pack (show retryAt)])
            activity.activityRetryAt
        <> ["  Updated at: " <> Text.pack (show activity.activityUpdatedAt)]

renderSessionTurn :: Int -> SessionTurn -> [Text]
renderSessionTurn index turn =
    [ "Turn " <> Text.pack (show index)
    , "At: " <> Text.pack (show turn.turnAt)
    , "User:"
    , indentText turn.turnUserText
    ]
        <> maybe [] (\text -> ["Assistant:", indentText text])
            turn.turnAssistantText
        <> maybe [] (\text -> ["Error:", indentText text])
            turn.turnError

intercalateBlank :: [[Text]] -> [Text]
intercalateBlank = \case
    [] -> []
    first : rest -> first <> concatMap ("" :) rest

indentText :: Text -> Text
indentText = Text.intercalate "\n" . map ("  " <>) . Text.lines
