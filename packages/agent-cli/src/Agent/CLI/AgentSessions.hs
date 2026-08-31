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

import Agent.CLI.AgentSessions.Process
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
    )
import Agent.CLI.AgentSessions.Render (renderAgentSession)
import Agent.CLI.Error (formatException)
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
import Agent.CLI.SessionLock
    ( sessionLockIsActive
    , sessionLockPath
    )
import Agent.CLI.Models
    ( ModelOption(..)
    , ModelTarget(..)
    , resolveModelOptionDialect
    )
import Agent.OsPath (fromText)
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
    , ToolExecutionPolicy(..)
    , jsonTool
    )
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
    , takeMVar
    )
import Control.Exception.Safe
    ( mask
    , tryAny
    )
import Control.Monad (void)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import System.OsPath (OsPath, unsafeEncodeUtf, (</>))

data AgentSessionToolsEnv = AgentSessionToolsEnv
    { toolsPool :: !StorePool
    , toolsRoot :: !OsPath
    , toolsProvider :: !Provider
    , toolsConnection :: !Text
    , toolsModel :: !Text
    , toolsTransportModel :: !Text
    , toolsDialect :: !DialectId
    , toolsAllowedModels :: !(Maybe [Text])
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
    , PropertySchema "model" modelPropertyType False $ Just
        modelPropertyDescription
    , PropertySchema "reasoning_effort" PropertyString False $ Just
        "Optional reasoning-effort override. Defaults to the current session effort."
    ]
    False
    TurnSequential
    (typedTool "create_agent_session" createAgentSessionArgsDecoder
        (runCreateAgentSession env))
  where
    modelPropertyType =
        maybe
            PropertyString
            (PropertyEnum . normalizedAllowedModels)
            env.toolsAllowedModels
    modelPropertyDescription =
        case env.toolsAllowedModels of
            Nothing ->
                "Optional model override. Defaults to the current session model."
            Just _ ->
                "Optional organization-approved model override. Defaults to the current session model."

runCreateAgentSession
    :: AgentSessionToolsEnv
    -> CreateAgentSessionArgs
    -> IO (Either Text Text)
runCreateAgentSession env args
    | Text.null (Text.strip args.message) =
        pure (Left "create_agent_session requires a non-empty message")
    | maybe False ((> 100) . Text.length . Text.strip) args.title =
        pure (Left "create_agent_session title must be at most 100 characters")
    | not (allowedModelOverride env.toolsAllowedModels normalizedOverride) =
        pure
            (Left
                "The requested session model is not allowed by this organization.")
    | otherwise = do
        let model = fromMaybe env.toolsModel normalizedOverride
        target <- case normalizedOverride of
            Nothing ->
                pure ModelOption
                    { modelTarget = ModelTarget
                        { targetProvider = env.toolsProvider
                        , targetConnectionId = env.toolsConnection
                        , targetModelId = model
                        , targetWireModelId = env.toolsTransportModel
                        , targetDialect = env.toolsDialect
                        }
                    , modelContextWindow = Nothing
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
                    , modelContextWindow = Nothing
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
  where
    normalizedOverride = normalizeModelOverride args.model
    allowedModelOverride allowed requested =
        case requested of
            Nothing -> True
            Just requestedModel ->
                maybe
                    True
                    (elem requestedModel . normalizedAllowedModels)
                    allowed

normalizeModelOverride :: Maybe Text -> Maybe Text
normalizeModelOverride requested = do
    stripped <- Text.strip <$> requested
    if Text.null stripped then Nothing else Just stripped

normalizedAllowedModels :: [Text] -> [Text]
normalizedAllowedModels =
    filter (not . Text.null) . map Text.strip

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
                        activity <-
                            if status == "running"
                                then loadSessionActivity
                                    env.toolsRoot args.sessionId
                                else pure Nothing
                        pure $ Right $
                            renderAgentSession
                                meta
                                status
                                activity
                                (map snd page.pageTurns)

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
