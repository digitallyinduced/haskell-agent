-- | Tools for creating, inspecting, and continuing persisted top-level agent
-- sessions. Turns run in managed background @agent-cli@ processes so they are
-- independent from the caller's model loop while remaining resumable.
module Agent.CLI.AgentSessions
    ( AgentSessionToolsEnv(..)
    , SessionProcessManager
    , agentSessionTools
    , closeSessionProcessManager
    , launchSessionTurn
    , newSessionProcessManager
    , signalManagedSessionReady
    , sessionProcessStatus
    ) where

import Agent.CLI.Options (ApprovalPolicy(..))
import Agent.CLI.Error (formatException)
import Agent.CLI.Session
    ( SessionCreate(..)
    , SessionHandle(..)
    , SessionMeta(..)
    , SessionTurn(..)
    , createSession
    , loadSession
    , sessionTempDirForId
    , sessionTitleFromPrompt
    )
import Agent.CLI.SessionLock
    ( sessionLockIsActive
    , sessionLockPath
    )
import Agent.CLI.Models
    ( ModelOption(..)
    , resolveModelOptionDialect
    )
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
    , newMVar
    )
import Control.Exception.Safe (SomeException, try)
import Control.Monad (forM_)
import Data.Aeson (FromJSON(..), Value, object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
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
    , getProcessExitCode
    , proc
    , waitForProcess
    )

data AgentSessionToolsEnv = AgentSessionToolsEnv
    { toolsRoot :: !OsPath
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

data ManagedSessionProcess = ManagedSessionProcess
    { managedHandle :: !ProcessHandle }

data SessionProcessManager = SessionProcessManager
    { managedRoot :: !OsPath
    , managedProcesses :: !(MVar (Map Text ManagedSessionProcess))
    }

newSessionProcessManager :: OsPath -> IO SessionProcessManager
newSessionProcessManager root = do
    processes <- newMVar Map.empty
    pure SessionProcessManager
        { managedRoot = root
        , managedProcesses = processes
        }

-- | Start one background turn for a persisted session. A second turn is
-- rejected while the first is still running to keep transcript appends
-- serialized.
launchSessionTurn
    :: SessionProcessManager
    -> Bool
    -> ApprovalPolicy
    -> SessionHandle
    -> Text
    -> IO (Either Text Text)
launchSessionTurn manager background policy handle message =
    modifyMVar manager.managedProcesses \processes -> do
        let sessionId = handle.sessionMeta.metaId
        busy <- case Map.lookup sessionId processes of
            Nothing -> pure False
            Just process -> (== Nothing) <$> getProcessExitCode process.managedHandle
        if busy
            then pure
                ( processes
                , Left ("session " <> sessionId <> " is already running")
                )
            else resolveAgentExecutable >>= \case
                Left err -> pure (processes, Left err)
                Right executable -> do
                    parentEnv <- getEnvironment
                    (promptPath, promptHandle) <- openTempFile
                        (unsafeToFilePath handle.sessionDir) ".agent-prompt-"
                    TextIO.hPutStr promptHandle message
                    hClose promptHandle
                    setFileMode promptPath 0o600
                    (readyPath, readyHandle) <- openTempFile
                        (unsafeToFilePath handle.sessionDir) ".agent-ready-"
                    hClose readyHandle
                    setFileMode readyPath 0o600
                    let childEnv =
                            (managedSessionReadyEnv, readyPath)
                                : filter
                                    ((/= managedSessionReadyEnv) . fst)
                                    parentEnv
                        logPath = unsafeToFilePath handle.sessionDir FilePath.</> "agent.log"
                        approvalArgs = case policy of
                            ApproveAll -> ["--yolo"]
                            DenyMutating -> ["--no-yolo"]
                            PromptMutating -> ["--no-yolo"]
                        agentArgs =
                            [ "--resume", Text.unpack sessionId
                            , "--prompt-file", promptPath
                            , "--save-session"
                            ]
                                <> approvalArgs
                        cleanupScript =
                            "prompt=$1; shift; "
                                <> "cleanup() { rm -f \"$prompt\"; }; "
                                <> "trap cleanup EXIT HUP INT TERM; "
                                <> "\"$@\""
                        args =
                            [ "-c", cleanupScript
                            , "agent-session-runner"
                            , promptPath
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
                            removePrivateFile promptPath
                            removePrivateFile readyPath
                            pure
                                ( processes
                                , Left
                                    ("failed to start agent session: "
                                        <> formatException err)
                                )
                        Right (_, _, _, process) -> do
                            ready <- waitForManagedSessionReady process readyPath
                            removePrivateFile readyPath
                            case ready of
                                Left err -> do
                                    _ <- waitForProcess process
                                    pure (processes, Left err)
                                Right ()
                                    | background ->
                                        pure
                                            ( Map.insert sessionId ManagedSessionProcess
                                                { managedHandle = process }
                                                processes
                                            , Right ("started session " <> sessionId)
                                            )
                                    | otherwise -> do
                                        exitCode <- waitForProcess process
                                        pure (processes, case exitCode of
                                            ExitSuccess ->
                                                Right ("completed session " <> sessionId)
                                            ExitFailure code ->
                                                Left
                                                    ("session failed with exit code "
                                                        <> Text.pack (show code)))

sessionProcessStatus :: SessionProcessManager -> Text -> IO Text
sessionProcessStatus manager sessionId =
    modifyMVar manager.managedProcesses \processes ->
        case Map.lookup sessionId processes of
            Nothing -> do
                locked <- sessionLockIsActive
                    (sessionLockPath
                        (manager.managedRoot
                            </> unsafeEncodeUtf (Text.unpack sessionId)))
                pure (processes, if locked then "running" else "idle")
            Just process ->
                getProcessExitCode process.managedHandle >>= \case
                    Nothing -> pure (processes, "running")
                    Just ExitSuccess ->
                        pure (Map.delete sessionId processes, "completed")
                    Just (ExitFailure code) ->
                        pure
                            ( Map.delete sessionId processes
                            , "failed (" <> Text.pack (show code) <> ")"
                            )

closeSessionProcessManager :: SessionProcessManager -> IO ()
closeSessionProcessManager manager =
    modifyMVar_ manager.managedProcesses \processes -> do
        -- Running sessions intentionally outlive the caller. The child owns
        -- the advisory session lock; its wrapper owns prompt cleanup.
        forM_ (Map.elems processes) \process -> do
            getProcessExitCode process.managedHandle >>= \case
                Just _ -> do
                    _ <- try @_ @SomeException
                        (waitForProcess process.managedHandle)
                    pure ()
                Nothing -> pure ()
        pure Map.empty

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
    "Create a persisted top-level agent session and start its first turn in the background. Returns the session id immediately."
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
                    { modelConnectionId = env.toolsConnection
                    , modelProvider = env.toolsProvider
                    , modelId = model
                    , modelTransportId = env.toolsTransportModel
                    , modelDialect = env.toolsDialect
                    , modelLabel = Nothing
                    , modelFallbackPriority = Nothing
                    }
            Just _ ->
                resolveModelOptionDialect ModelOption
                    { modelConnectionId = env.toolsConnection
                    , modelProvider = env.toolsProvider
                    , modelId = model
                    , modelTransportId = model
                    , modelDialect =
                        dialectIdForModel env.toolsProvider model
                    , modelLabel = Nothing
                    , modelFallbackPriority = Nothing
                    }
        let title = case Text.strip <$> args.title of
                Just value | not (Text.null value) -> value
                _ -> sessionTitleFromPrompt args.message
            spec = SessionCreate
                { createRoot = env.toolsRoot
                , createProvider = env.toolsProvider
                , createConnection = target.modelConnectionId
                , createModel = model
                , createTransportModel = target.modelTransportId
                , createDialect = target.modelDialect
                , createCwd = env.toolsCwd
                , createEffort = fromMaybe env.toolsEffort args.reasoningEffort
                , createTitleHint = Just title
                , createTitleIsManual =
                    maybe False (not . Text.null . Text.strip) args.title
                }
        handle <- createSession spec
        env.toolsLaunchTurn handle args.message >>= \case
            Left err -> pure $ Left $
                "created session " <> handle.sessionMeta.metaId
                    <> " but failed to start it: " <> err
            Right launchResult -> do
                status <- statusAfterLaunch env handle.sessionMeta.metaId launchResult
                pure $ Right $ encodeJson $ object
                    [ "session_id" .= handle.sessionMeta.metaId
                    , "status" .= status
                    ]

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
    "Read metadata and recent user/assistant turns from a persisted agent session."
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
    loadSession env.toolsRoot args.sessionId >>= \case
        Left err -> pure (Left err)
        Right (meta, turns) -> do
            status <- env.toolsSessionStatus args.sessionId
            let limit = min 100 (max 1 (fromMaybe 20 args.limit))
                recent = drop (max 0 (length turns - limit)) turns
            pure $ Right $ encodeJson $ object
                [ "session" .= sessionJson meta status
                , "turns" .= map turnJson recent
                ]

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
    "Send a message to a persisted agent session by starting a resumed background turn. Fails if that session is already running."
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
            else loadSession env.toolsRoot args.sessionId >>= \case
                Left err -> pure (Left err)
                Right (meta, _) -> do
                    env.toolsLaunchTurn (sessionHandle env.toolsRoot meta) args.message
                        >>= \case
                            Left err -> pure (Left err)
                            Right launchResult -> do
                                status <- statusAfterLaunch env args.sessionId launchResult
                                pure $ Right $ encodeJson $ object
                                    [ "session_id" .= args.sessionId
                                    , "status" .= status
                                    ]

sessionHandle :: OsPath -> SessionMeta -> SessionHandle
sessionHandle root meta =
    let dir = root </> fromText meta.metaId
    in SessionHandle
        { sessionDir = dir
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

sessionJson :: SessionMeta -> Text -> Value
sessionJson meta status = object
    [ "id" .= meta.metaId
    , "status" .= status
    , "title" .= meta.metaTitle
    , "provider" .= providerSlug meta.metaProvider
    , "connection" .= meta.metaConnection
    , "model" .= meta.metaModel
    , "dialect" .= dialectSlug meta.metaDialect
    , "reasoning_effort" .= meta.metaEffort
    , "cwd" .= unsafeToFilePath meta.metaCwd
    , "created_at" .= meta.metaCreatedAt
    , "updated_at" .= meta.metaUpdatedAt
    ]

turnJson :: SessionTurn -> Value
turnJson turn = object
    [ "at" .= turn.turnAt
    , "user" .= turn.turnUserText
    , "assistant" .= turn.turnAssistantText
    , "error" .= turn.turnError
    ]

encodeJson :: Value -> Text
encodeJson = TextEncoding.decodeUtf8 . LBS.toStrict . Aeson.encode
