{-# LANGUAGE ForeignFunctionInterface #-}

module Agent.CLI.MacOS.Bridge () where

import qualified Agent.CLI.AgentViewport as Viewport
import Agent.CLI.NativeRuntime
    ( NativeProcessRuntime
    , NativeRunHooks(..)
    , closeNativeProcessRuntime
    , newNativeProcessRuntime
    , runNativeAgent
    )
import Agent.CLI.ModelConfig (loadModelCatalogAt)
import Agent.CLI.Models
    ( ModelOption(..)
    , ModelTarget(..)
    , PickerState(..)
    , defaultModelOptionFor
    , initialPickerStateResolved
    , modelCatalog
    , resolveModelOptionDialect
    , selectedOption
    )
import Agent.CLI.Permission (PermissionChoice(..))
import Agent.CLI.Project
    ( ProjectModel(..)
    , ProjectSettings(..)
    , loadProjectSettings
    , resolveProjectRoot
    )
import Agent.CLI.Render
    ( summarizeToolCall )
import Agent.CLI.Options (parseEffort)
import Agent.CLI.Session
    ( SessionMeta(..)
    , deleteSession
    , listArchivedSessionIds
    , listSessions
    , loadSessionMeta
    , renameSession
    , setSessionArchived
    , sessionsRoot
    )
import Agent.CLI.SessionAdmin
    ( accountSummariesJSON
    , loadSessionPageJSON
    , managedPostgresConfigForHome
    , sessionSummaryWithStatusJSON
    )
import Agent.Loop (LoopEvent(..))
import Agent.Dialect (dialectSlug)
import Agent.Provider (Provider(..), providerSlug)
import Agent.Store.Postgres
    ( ManagedPostgresConfig
    , Store
    , closeStore
    , openStore
    , trustedPool
    )
import Agent.Store.Types (renderStoreError)
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallResult(..)
    )
import Control.Concurrent (forkFinally)
import Control.Concurrent.Async
    ( Async
    , cancel
    , waitCatchSTM
    , withAsync
    )
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , newEmptyMVar
    , newMVar
    , putMVar
    , readMVar
    )
import Control.Concurrent.STM
    ( TMVar
    , TQueue
    , TVar
    , atomically
    , modifyTVar'
    , newEmptyTMVarIO
    , newTQueueIO
    , newTVarIO
    , orElse
    , readTQueue
    , readTVar
    , readTVarIO
    , takeTMVar
    , tryPutTMVar
    , writeTQueue
    , writeTVar
    )
import Control.Applicative ((<|>))
import Control.Exception.Safe
    ( SomeException
    , finally
    , tryAny
    )
import Control.Monad
    ( forM_
    , void
    )
import qualified Data.Aeson as Aeson
import Data.Aeson
    ( (.:)
    , (.:?)
    )
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
    ( newIORef
    , readIORef
    , writeIORef
    )
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8)
import Foreign
    ( FunPtr
    , Ptr
    , StablePtr
    , castPtr
    , castPtrToStablePtr
    , castStablePtrToPtr
    , deRefStablePtr
    , freeStablePtr
    , newStablePtr
    , nullFunPtr
    , nullPtr
    )
import Foreign.C.Types (CInt(..), CSize(..))
import System.Directory.OsPath (getHomeDirectory)
import System.IO
    ( IOMode(WriteMode)
    , withFile
    )
import System.OsPath
    ( OsPath
    , takeDirectory
    , unsafeEncodeUtf
    )

type EventCallback = Ptr () -> Ptr Word8 -> CSize -> IO ()

foreign import ccall "dynamic"
    invokeEventCallback :: FunPtr EventCallback -> EventCallback

data BridgeRequest = BridgeRequest
    { requestId :: !Text
    , requestMethod :: !Text
    , requestParams :: !Aeson.Value
    }

instance Aeson.FromJSON BridgeRequest where
    parseJSON = Aeson.withObject "BridgeRequest" \object ->
        BridgeRequest
            <$> object .: "id"
            <*> object .: "method"
            <*> (object .:? "params" Aeson..!= Aeson.object [])

data TurnStart = TurnStart
    { turnStartId :: !Text
    , turnStartPrompt :: !Text
    , turnStartSessionId :: !(Maybe Text)
    , turnStartCwd :: !FilePath
    , turnStartProvider :: !(Maybe Text)
    , turnStartModel :: !(Maybe Text)
    , turnStartEffort :: !(Maybe Text)
    , turnStartWorktree :: !Bool
    }

instance Aeson.FromJSON TurnStart where
    parseJSON = Aeson.withObject "TurnStart" \object -> do
        turnStartId <- object .: "turnId"
        turnStartPrompt <- object .: "prompt"
        turnStartSessionId <- object .:? "sessionId"
        turnStartCwd <- object .: "cwd"
        turnStartProvider <- object .:? "provider"
        turnStartModel <- object .:? "model"
        rawEffort <- object .:? "effort"
        turnStartEffort <- traverse
            (either fail pure . parseEffort)
            rawEffort
        turnStartWorktree <- object .:? "worktree" Aeson..!= False
        let start = TurnStart
                { turnStartId
                , turnStartPrompt
                , turnStartSessionId
                , turnStartCwd
                , turnStartProvider
                , turnStartModel
                , turnStartEffort
                , turnStartWorktree
                }
        case
            ( turnStartSessionId
            , turnStartWorktree
            , turnStartProvider
            , turnStartModel
            )
          of
            (Just _, True, _, _) ->
                fail "a worktree can only be created for a new session"
            (_, _, Nothing, Nothing) -> pure start
            (_, _, Just _, Just _) -> pure start
            _ -> fail "provider and model must be supplied together"

data TurnReference = TurnReference
    { turnReferenceId :: !Text
    }

instance Aeson.FromJSON TurnReference where
    parseJSON = Aeson.withObject "TurnReference" \object ->
        TurnReference <$> object .: "turnId"

data ApprovalResolution = ApprovalResolution
    { approvalResolutionId :: !Text
    , approvalResolutionDecision :: !Text
    }

instance Aeson.FromJSON ApprovalResolution where
    parseJSON = Aeson.withObject "ApprovalResolution" \object ->
        ApprovalResolution
            <$> object .: "approvalId"
            <*> object .: "decision"

data SessionPageRequest = SessionPageRequest
    { sessionPageId :: !Text
    , sessionPageBefore :: !(Maybe Int64)
    , sessionPageLimit :: !(Maybe Int)
    }

instance Aeson.FromJSON SessionPageRequest where
    parseJSON = Aeson.withObject "SessionPageRequest" \object ->
        SessionPageRequest
            <$> object .: "id"
            <*> object .:? "before"
            <*> object .:? "limit"

data SessionReference = SessionReference
    { sessionReferenceId :: !Text
    }

instance Aeson.FromJSON SessionReference where
    parseJSON = Aeson.withObject "SessionReference" \object ->
        SessionReference <$> object .: "id"

data SessionRenameRequest = SessionRenameRequest
    { sessionRenameId :: !Text
    , sessionRenameTitle :: !Text
    }

instance Aeson.FromJSON SessionRenameRequest where
    parseJSON = Aeson.withObject "SessionRenameRequest" \object ->
        SessionRenameRequest
            <$> object .: "id"
            <*> object .: "title"

data SessionArchiveRequest = SessionArchiveRequest
    { sessionArchiveId :: !Text
    , sessionArchiveArchived :: !Bool
    }

instance Aeson.FromJSON SessionArchiveRequest where
    parseJSON = Aeson.withObject "SessionArchiveRequest" \object ->
        SessionArchiveRequest
            <$> object .: "id"
            <*> object .: "archived"

data ModelsListRequest = ModelsListRequest
    { modelsListCwd :: !FilePath
    , modelsListSessionId :: !(Maybe Text)
    }

instance Aeson.FromJSON ModelsListRequest where
    parseJSON = Aeson.withObject "ModelsListRequest" \object ->
        ModelsListRequest
            <$> object .: "cwd"
            <*> object .:? "sessionId"

data EngineCommand
    = EngineRequest !BridgeRequest
    | EngineStop

data Engine = Engine
    { engineCommands :: !(TQueue EngineCommand)
    , engineDone :: !(MVar ())
    }

data TurnControl = TurnControl
    { turnControlId :: !Text
    , turnControlCancel :: !(TVar (IO ()))
    , turnControlApprovals
        :: !(TVar (Map Text (TMVar PermissionChoice)))
    , turnControlApprovalCounter :: !(TVar Int)
    , turnControlAllowedTools :: !(TVar (Set.Set Text))
    , turnControlAgentSnapshot :: !(TVar (IO [Viewport.AgentEntry]))
    }

data TurnOutcome = TurnOutcome
    { turnOutcomeSessionId :: !(Maybe Text)
    , turnOutcomeError :: !(Maybe Text)
    }

data ActiveExit
    = ActiveContinue
    | ActiveStop

foreign export ccall ha_engine_create
    :: FunPtr EventCallback -> Ptr () -> IO (Ptr ())

foreign export ccall ha_engine_send_json
    :: Ptr () -> Ptr Word8 -> CSize -> IO CInt

foreign export ccall ha_engine_destroy
    :: Ptr () -> IO ()

ha_engine_create :: FunPtr EventCallback -> Ptr () -> IO (Ptr ())
ha_engine_create callback context
    | callback == nullFunPtr = pure nullPtr
    | otherwise = do
        created <- tryAny do
            home <- getHomeDirectory
            config <- managedPostgresConfigForHome home
            commands <- newTQueueIO
            done <- newEmptyMVar
            _ <- forkFinally
                (workerLifecycle
                    callback
                    context
                    config
                    (sessionsRoot home)
                    commands)
                (const (putMVar done ()))
            stable <- newStablePtr Engine
                { engineCommands = commands
                , engineDone = done
                }
            pure (castStablePtrToPtr stable)
        case created of
            Left exception -> do
                sendEvent callback context $
                    failureEvent "_engine" (Text.pack (show exception))
                pure nullPtr
            Right pointer -> pure pointer

ha_engine_send_json :: Ptr () -> Ptr Word8 -> CSize -> IO CInt
ha_engine_send_json pointer bytes (CSize length)
    | pointer == nullPtr = pure 1
    | bytes == nullPtr && length > 0 = pure 2
    | otherwise = do
        accepted <- tryAny do
            let stable = castPtrToStablePtr pointer :: StablePtr Engine
            engine <- deRefStablePtr stable
            payload <- BS.packCStringLen
                (castPtr bytes, fromIntegral length)
            case (Aeson.eitherDecodeStrict' payload
                :: Either String BridgeRequest) of
                Left _ -> pure False
                Right request -> do
                    atomically
                        (writeTQueue
                            engine.engineCommands
                            (EngineRequest request))
                    pure True
        pure $ case accepted of
            Left _ -> 3
            Right False -> 4
            Right True -> 0

ha_engine_destroy :: Ptr () -> IO ()
ha_engine_destroy pointer
    | pointer == nullPtr = pure ()
    | otherwise = void $ tryAny do
        let stable = castPtrToStablePtr pointer :: StablePtr Engine
        (do
            engine <- deRefStablePtr stable
            atomically (writeTQueue engine.engineCommands EngineStop)
            readMVar engine.engineDone)
            `finally` freeStablePtr stable

workerLifecycle
    :: FunPtr EventCallback
    -> Ptr ()
    -> ManagedPostgresConfig
    -> OsPath
    -> TQueue EngineCommand
    -> IO ()
workerLifecycle callback context config root commands = do
    store <- newMVar Nothing
    processRuntime <- newNativeProcessRuntime root
    let cleanup =
            closeNativeProcessRuntime processRuntime
                `finally` closeEngineStore store
    idleLoop
        callback
        context
        config
        store
        root
        processRuntime
        commands
        `finally` cleanup

idleLoop
    :: FunPtr EventCallback
    -> Ptr ()
    -> ManagedPostgresConfig
    -> MVar (Maybe Store)
    -> OsPath
    -> NativeProcessRuntime
    -> TQueue EngineCommand
    -> IO ()
idleLoop callback context config store root processRuntime commands =
    atomically (readTQueue commands) >>= \case
        EngineStop -> pure ()
        EngineRequest request
            | request.requestMethod == "turn.start" ->
                case (parseParams request
                    :: Either Text TurnStart) of
                    Left err -> do
                        sendEvent callback context
                            (failureEvent request.requestId err)
                        continue
                    Right start -> do
                        control <- newTurnControl start.turnStartId
                        sendEvent callback context $
                            successEvent request.requestId $
                                Aeson.object
                                    [ "turnId" Aeson..= start.turnStartId
                                    ]
                        sendTurnStatus
                            callback
                            context
                            start.turnStartId
                            (if start.turnStartWorktree
                                then "Creating worktree…"
                                else "Starting…")
                        withAsync
                            (runNativeTurn
                                callback
                                context
                                processRuntime
                                control
                                start)
                            \running ->
                                activeLoop
                                    callback
                                    context
                                    config
                                    store
                                    root
                                    commands
                                    control
                                    running >>= \case
                                        ActiveContinue -> continue
                                        ActiveStop -> pure ()
            | request.requestMethod `elem`
                ["turn.cancel", "approval.resolve"] -> do
                    sendEvent callback context $
                        failureEvent
                            request.requestId
                            "there is no active turn"
                    continue
            | otherwise -> do
                event <- handleRequest config store root request
                sendEvent callback context event
                continue
  where
    continue =
        idleLoop callback context config store root processRuntime commands

activeLoop
    :: FunPtr EventCallback
    -> Ptr ()
    -> ManagedPostgresConfig
    -> MVar (Maybe Store)
    -> OsPath
    -> TQueue EngineCommand
    -> TurnControl
    -> Async TurnOutcome
    -> IO ActiveExit
activeLoop callback context config store root commands control running =
    atomically
        ((Left <$> readTQueue commands)
            `orElse` (Right <$> waitCatchSTM running)) >>= \case
        Right outcome -> do
            finishTurnEvent callback context control.turnControlId outcome
            pure ActiveContinue
        Left EngineStop -> do
            cancelTurn control
            cancel running
            pure ActiveStop
        Left (EngineRequest request) -> do
            if request.requestMethod == "turn.cancel"
              then
                case (parseParams request
                    :: Either Text TurnReference) of
                    Right reference
                        | reference.turnReferenceId == control.turnControlId -> do
                            cancelTurn control
                            cancel running
                            sendEvent callback context $
                                successEvent request.requestId True
                    _ ->
                        sendEvent callback context $
                            failureEvent request.requestId "turn id is not active"
              else if request.requestMethod == "approval.resolve"
              then
                resolveApproval control request >>= sendEvent callback context
              else if request.requestMethod == "turn.agents"
              then
                activeAgentSnapshot control request
                    >>= sendEvent callback context
              else if request.requestMethod == "turn.start"
              then
                sendEvent callback context $
                    failureEvent request.requestId "a turn is already running"
              else
                handleRequest config store root request
                    >>= sendEvent callback context
            activeLoop
                callback
                context
                config
                store
                root
                commands
                control
                running

runNativeTurn
    :: FunPtr EventCallback
    -> Ptr ()
    -> NativeProcessRuntime
    -> TurnControl
    -> TurnStart
    -> IO TurnOutcome
runNativeTurn callback context processRuntime control start = do
    sessionIdRef <- newIORef start.turnStartSessionId
    completedRef <- newIORef False
    let hooks = NativeRunHooks
            { nativeOnLoopEvent = \event -> do
                case event of
                    TurnFinished _ -> writeIORef completedRef True
                    _ -> pure ()
                forM_ (nativeLoopEvent control.turnControlId event)
                    (sendEvent callback context)
            , nativeOnSessionId = \sessionId -> do
                writeIORef sessionIdRef (Just sessionId)
                sendEvent callback context $
                    Aeson.object
                        [ "event" Aeson..= ("turn.session" :: Text)
                        , "turnId" Aeson..= control.turnControlId
                        , "sessionId" Aeson..= sessionId
                        ]
            , nativeRegisterCancel =
                atomically . writeTVar control.turnControlCancel
            , nativeRegisterAgentSnapshot =
                atomically . writeTVar control.turnControlAgentSnapshot
            , nativeRequestApproval =
                requestApproval callback context control
            }
        modelArgs = case
            (start.turnStartProvider, start.turnStartModel) of
                (Just provider, Just model) ->
                    [ "--provider", Text.unpack provider
                    , "--model", Text.unpack model
                    ]
                _ -> []
        args =
            [ "--minimal"
            , "--motion", "off"
            , "--save-session"
            , "--no-yolo"
            ]
                <> maybe
                    []
                    (\sessionId -> ["--resume", Text.unpack sessionId])
                    start.turnStartSessionId
                <> (if start.turnStartWorktree then ["--worktree"] else [])
                <> modelArgs
                <> maybe
                    []
                    (\effort -> ["--effort", Text.unpack effort])
                    start.turnStartEffort
                <> ["--prompt", Text.unpack start.turnStartPrompt]
    result <- tryAny $
        withFile "/dev/null" WriteMode \output ->
            runNativeAgent
                processRuntime
                output
                (unsafeEncodeUtf start.turnStartCwd)
                hooks
                args
    completed <- readIORef completedRef
    sessionId <- readIORef sessionIdRef
    pure TurnOutcome
        { turnOutcomeSessionId = sessionId
        , turnOutcomeError =
            case result of
                Left exception -> Just (Text.pack (show exception))
                Right (Left err) -> Just err
                Right (Right ())
                    | completed -> Nothing
                    | otherwise ->
                        Just
                            "turn ended without a completion event"
        }

newTurnControl :: Text -> IO TurnControl
newTurnControl turnId =
    TurnControl turnId
        <$> newTVarIO (pure ())
        <*> newTVarIO Map.empty
        <*> newTVarIO 0
        <*> newTVarIO Set.empty
        <*> newTVarIO (pure [])

activeAgentSnapshot :: TurnControl -> BridgeRequest -> IO Aeson.Value
activeAgentSnapshot control request = do
    loadSnapshot <- readTVarIO control.turnControlAgentSnapshot
    tryAny loadSnapshot >>= \case
        Left exception -> pure $
            failureEvent request.requestId (Text.pack (show exception))
        Right agents -> pure $
            successEvent request.requestId (map agentEntryJSON agents)

agentEntryJSON :: Viewport.AgentEntry -> Aeson.Value
agentEntryJSON entry =
    Aeson.object
        [ "path" Aeson..= entry.agentPath
        , "status" Aeson..= entry.agentStatus
        , "model" Aeson..= entry.agentModel
        , "steps" Aeson..= map agentStepJSON entry.agentSteps
        ]

agentStepJSON :: Viewport.AgentStep -> Aeson.Value
agentStepJSON step =
    Aeson.object
        [ "state" Aeson..= agentStepStateText step.agentStepState
        , "title" Aeson..= step.agentStepTitle
        , "detail" Aeson..= step.agentStepDetail
        ]

agentStepStateText :: Viewport.AgentStepState -> Text
agentStepStateText = \case
    Viewport.AgentStepRunning -> "running"
    Viewport.AgentStepCompleted -> "completed"
    Viewport.AgentStepFailed -> "failed"
    Viewport.AgentStepInfo -> "info"

cancelTurn :: TurnControl -> IO ()
cancelTurn control = do
    cancelAction <- readTVarIO control.turnControlCancel
    cancelAction
    waiters <- atomically do
        current <- readTVar control.turnControlApprovals
        writeTVar control.turnControlApprovals Map.empty
        pure (Map.elems current)
    atomically $
        forM_ waiters \waiter ->
            void (tryPutTMVar waiter PermissionDeny)

requestApproval
    :: FunPtr EventCallback
    -> Ptr ()
    -> TurnControl
    -> ToolCall
    -> IO (Maybe PermissionChoice)
requestApproval callback context control call = do
    alreadyAllowed <- Set.member call.name
        <$> readTVarIO control.turnControlAllowedTools
    if alreadyAllowed
      then pure (Just PermissionAllowOnce)
      else requestApprovalFromClient callback context control call

requestApprovalFromClient
    :: FunPtr EventCallback
    -> Ptr ()
    -> TurnControl
    -> ToolCall
    -> IO (Maybe PermissionChoice)
requestApprovalFromClient callback context control call = do
    waiter <- newEmptyTMVarIO
    approvalId <- atomically do
        current <- readTVar control.turnControlApprovalCounter
        let next = current + 1
            approvalId =
                control.turnControlId
                    <> "-approval-"
                    <> Text.pack (show next)
        writeTVar control.turnControlApprovalCounter next
        modifyTVar'
            control.turnControlApprovals
            (Map.insert approvalId waiter)
        pure approvalId
    let (arguments, truncated) =
            boundedEventText
                (if call.argumentsEncrypted then "" else call.arguments)
    sendEvent callback context $
        Aeson.object
            [ "event" Aeson..= ("approval.requested" :: Text)
            , "turnId" Aeson..= control.turnControlId
            , "approval" Aeson..= Aeson.object
                [ "id" Aeson..= approvalId
                , "callId" Aeson..= call.callId
                , "name" Aeson..= call.name
                , "summary" Aeson..= summarizeToolCall call
                , "arguments" Aeson..= arguments
                , "argumentsEncrypted" Aeson..= call.argumentsEncrypted
                , "truncated" Aeson..= truncated
                ]
            ]
    choice <- atomically (takeTMVar waiter)
    atomically $
        modifyTVar'
            control.turnControlApprovals
            (Map.delete approvalId)
    case choice of
        PermissionAllowTool ->
            atomically $
                modifyTVar'
                    control.turnControlAllowedTools
                    (Set.insert call.name)
        _ -> pure ()
    pure (Just choice)

resolveApproval :: TurnControl -> BridgeRequest -> IO Aeson.Value
resolveApproval control request =
    case (parseParams request
        :: Either Text ApprovalResolution) of
        Left err -> pure (failureEvent request.requestId err)
        Right resolution ->
            case permissionChoice resolution.approvalResolutionDecision of
                Nothing ->
                    pure $ failureEvent
                        request.requestId
                        "unknown approval decision"
                Just choice -> do
                    accepted <- atomically do
                        current <- readTVar control.turnControlApprovals
                        case Map.lookup
                            resolution.approvalResolutionId
                            current of
                                Nothing -> pure False
                                Just waiter -> do
                                    published <- tryPutTMVar waiter choice
                                    if published
                                        then writeTVar
                                            control.turnControlApprovals
                                            (Map.delete
                                                resolution.approvalResolutionId
                                                current)
                                        else pure ()
                                    pure published
                    pure $
                        if accepted
                            then successEvent request.requestId True
                            else failureEvent
                                request.requestId
                                "approval request is no longer active"

permissionChoice :: Text -> Maybe PermissionChoice
permissionChoice = \case
    "allow_once" -> Just PermissionAllowOnce
    "allow_tool" -> Just PermissionAllowTool
    "deny" -> Just PermissionDeny
    _ -> Nothing

finishTurnEvent
    :: FunPtr EventCallback
    -> Ptr ()
    -> Text
    -> Either SomeException TurnOutcome
    -> IO ()
finishTurnEvent callback context turnId = \case
    Left exception ->
        sendEvent callback context $
            turnFailedEvent turnId (Text.pack (show exception))
    Right outcome ->
        case outcome.turnOutcomeError of
            Just err ->
                sendEvent callback context (turnFailedEvent turnId err)
            Nothing ->
                sendEvent callback context $
                    Aeson.object
                        [ "event" Aeson..= ("turn.completed" :: Text)
                        , "turnId" Aeson..= turnId
                        , "sessionId" Aeson..= outcome.turnOutcomeSessionId
                        ]

nativeLoopEvent :: Text -> LoopEvent -> Maybe Aeson.Value
nativeLoopEvent turnId = \case
    ReasoningDelta text ->
        Just $ Aeson.object
            [ "event" Aeson..= ("turn.reasoning_delta" :: Text)
            , "turnId" Aeson..= turnId
            , "text" Aeson..= text
            ]
    TextDelta text ->
        Just $ Aeson.object
            [ "event" Aeson..= ("turn.text_delta" :: Text)
            , "turnId" Aeson..= turnId
            , "text" Aeson..= text
            ]
    ToolStarted call ->
        let (arguments, truncated) =
                boundedEventText
                    (if call.argumentsEncrypted then "" else call.arguments)
        in Just $ Aeson.object
            [ "event" Aeson..= ("turn.tool" :: Text)
            , "turnId" Aeson..= turnId
            , "tool" Aeson..= Aeson.object
                [ "type" Aeson..= ("tool_started" :: Text)
                , "callId" Aeson..= call.callId
                , "name" Aeson..= call.name
                , "summary" Aeson..= summarizeToolCall call
                , "arguments" Aeson..= arguments
                , "argumentsEncrypted" Aeson..= call.argumentsEncrypted
                , "truncated" Aeson..= truncated
                ]
            ]
    ToolFinished result ->
        let (output, truncated) = boundedEventText result.output
        in Just $ Aeson.object
            [ "event" Aeson..= ("turn.tool" :: Text)
            , "turnId" Aeson..= turnId
            , "tool" Aeson..= Aeson.object
                [ "type" Aeson..= ("tool_finished" :: Text)
                , "callId" Aeson..= result.callId
                , "output" Aeson..= output
                , "truncated" Aeson..= truncated
                ]
            ]
    ActivityUpdated status ->
        Just $ turnStatusEvent turnId status
    WarningRaised warning ->
        Just $ turnStatusEvent turnId warning
    ResponseRestarted message ->
        Just $ turnStatusEvent turnId message
    _ -> Nothing

turnStatusEvent :: Text -> Text -> Aeson.Value
turnStatusEvent turnId status =
    Aeson.object
        [ "event" Aeson..= ("turn.status" :: Text)
        , "turnId" Aeson..= turnId
        , "status" Aeson..= status
        ]

sendTurnStatus
    :: FunPtr EventCallback
    -> Ptr ()
    -> Text
    -> Text
    -> IO ()
sendTurnStatus callback context turnId =
    sendEvent callback context . turnStatusEvent turnId

turnFailedEvent :: Text -> Text -> Aeson.Value
turnFailedEvent turnId message =
    Aeson.object
        [ "event" Aeson..= ("turn.failed" :: Text)
        , "turnId" Aeson..= turnId
        , "error" Aeson..= message
        ]

handleRequest
    :: ManagedPostgresConfig
    -> MVar (Maybe Store)
    -> OsPath
    -> BridgeRequest
    -> IO Aeson.Value
handleRequest config store root request = do
    result <- tryAny (handleMethod request)
    pure $ either
        (failureEvent request.requestId . Text.pack . show)
        id
        result
  where
    handleMethod current =
        case current.requestMethod of
            "ping" ->
                pure $ successEvent current.requestId $
                    Aeson.object
                        [ "runtime" Aeson..= ("haskell" :: Text)
                        , "protocol" Aeson..= (3 :: Int)
                        ]
            "sessions.list" -> do
                activeStore <- acquireStore config store
                let pool = trustedPool activeStore
                sessions <- listSessions pool root
                archivedIds <- listArchivedSessionIds pool
                    >>= either (fail . Text.unpack) pure
                let archived = Set.fromList archivedIds
                summaries <- mapM
                    (\session -> sessionSummaryWithStatusJSON
                        root
                        (Set.member session.metaId archived)
                        session)
                    sessions
                pure $ successEvent current.requestId
                    summaries
            "sessions.show" ->
                case (parseParams current
                    :: Either Text SessionPageRequest) of
                    Left err ->
                        pure (failureEvent current.requestId err)
                    Right page -> do
                        activeStore <- acquireStore config store
                        snapshot <- loadSessionPageJSON
                            (trustedPool activeStore)
                            root
                            page.sessionPageId
                            page.sessionPageBefore
                            (max 1 (min 200 (maybe 50 id page.sessionPageLimit)))
                        pure $ either
                            (failureEvent current.requestId)
                            (successEvent current.requestId)
                            snapshot
            "sessions.rename" ->
                case (parseParams current
                    :: Either Text SessionRenameRequest) of
                    Left err ->
                        pure (failureEvent current.requestId err)
                    Right rename -> do
                        activeStore <- acquireStore config store
                        renamed <- renameSession
                            (trustedPool activeStore)
                            root
                            rename.sessionRenameId
                            rename.sessionRenameTitle
                        pure $ either
                            (failureEvent current.requestId)
                            (const (successEvent current.requestId True))
                            renamed
            "sessions.delete" ->
                case (parseParams current
                    :: Either Text SessionReference) of
                    Left err ->
                        pure (failureEvent current.requestId err)
                    Right reference -> do
                        activeStore <- acquireStore config store
                        deleted <- deleteSession
                            (trustedPool activeStore)
                            root
                            reference.sessionReferenceId
                        pure $ either
                            (failureEvent current.requestId)
                            (const (successEvent current.requestId True))
                            deleted
            "sessions.archive" ->
                case (parseParams current
                    :: Either Text SessionArchiveRequest) of
                    Left err ->
                        pure (failureEvent current.requestId err)
                    Right archive -> do
                        activeStore <- acquireStore config store
                        changed <- setSessionArchived
                            (trustedPool activeStore)
                            root
                            archive.sessionArchiveId
                            archive.sessionArchiveArchived
                        pure $ either
                            (failureEvent current.requestId)
                            (const (successEvent current.requestId True))
                            changed
            "accounts.list" -> do
                accounts <- accountSummariesJSON
                pure $ successEvent current.requestId
                    accounts
            "turn.agents" ->
                pure $ successEvent current.requestId ([] :: [Aeson.Value])
            "models.list" ->
                case (parseParams current
                    :: Either Text ModelsListRequest) of
                    Left err ->
                        pure (failureEvent current.requestId err)
                    Right parameters -> do
                        activeStore <- acquireStore config store
                        catalogResult <- loadNativeModelCatalog
                            activeStore
                            root
                            parameters
                        pure $ either
                            (failureEvent current.requestId)
                            (successEvent current.requestId)
                            catalogResult
            method ->
                pure $ failureEvent current.requestId
                    ("unknown method: " <> method)

loadNativeModelCatalog
    :: Store
    -> OsPath
    -> ModelsListRequest
    -> IO (Either Text Aeson.Value)
loadNativeModelCatalog store root request = do
    let home = takeDirectory (takeDirectory root)
        requestedCwd = unsafeEncodeUtf request.modelsListCwd
    contextResult <- currentModelContext
        store
        root
        requestedCwd
        request.modelsListSessionId
    case contextResult of
        Left err -> pure (Left err)
        Right (cwd, maybeTarget) ->
            loadModelCatalogAt home cwd >>= \case
                Left err -> pure (Left err)
                Right catalog -> do
                    selectedTarget <- case maybeTarget of
                        Just target -> pure (Just target)
                        Nothing ->
                            traverse
                                (fmap (.modelTarget)
                                    . resolveModelOptionDialect)
                                ( defaultModelOptionFor
                                    catalog
                                    OpenAIProvider
                                    <|> listToMaybe (modelCatalog catalog)
                                )
                    case selectedTarget of
                        Nothing -> pure (Left "model catalog is empty")
                        Just target -> do
                            picker <- initialPickerStateResolved
                                catalog
                                target.targetConnectionId
                                target.targetProvider
                                target.targetModelId
                                target.targetDialect
                            pure $ Right $ Aeson.object
                                [ "options" Aeson..=
                                    map modelOptionJSON picker.pickerAll
                                , "current" Aeson..=
                                    fmap modelOptionJSON
                                        (selectedOption picker)
                                ]

currentModelContext
    :: Store
    -> OsPath
    -> OsPath
    -> Maybe Text
    -> IO (Either Text (OsPath, Maybe ModelTarget))
currentModelContext store root cwd = \case
    Just sessionId ->
        fmap (fmap (\meta ->
            (meta.metaCwd, Just (sessionModelTarget meta)))) $
            loadSessionMeta (trustedPool store) root sessionId
    Nothing -> do
        projectRoot <- resolveProjectRoot cwd
        settings <- loadProjectSettings projectRoot
        pure $ Right
            ( cwd
            , (.projectModelTarget) <$> settings.settingsLastModel
            )

sessionModelTarget :: SessionMeta -> ModelTarget
sessionModelTarget meta =
    ModelTarget
        { targetProvider = meta.metaProvider
        , targetConnectionId = meta.metaConnection
        , targetModelId = meta.metaModel
        , targetWireModelId =
            fromMaybe meta.metaModel meta.metaTransportModel
        , targetDialect = meta.metaDialect
        }

modelOptionJSON :: ModelOption -> Aeson.Value
modelOptionJSON option =
    let target = option.modelTarget
    in Aeson.object
        [ "id" Aeson..= target.targetModelId
        , "provider" Aeson..= providerSlug target.targetProvider
        , "connection" Aeson..= target.targetConnectionId
        , "wireModel" Aeson..= target.targetWireModelId
        , "dialect" Aeson..= dialectSlug target.targetDialect
        , "label" Aeson..= option.modelLabel
        ]

parseParams :: Aeson.FromJSON value => BridgeRequest -> Either Text value
parseParams request =
    case Aeson.parseEither Aeson.parseJSON request.requestParams of
        Left err -> Left (Text.pack err)
        Right value -> Right value

acquireStore :: ManagedPostgresConfig -> MVar (Maybe Store) -> IO Store
acquireStore config state =
    modifyMVar state \case
        Just store -> pure (Just store, store)
        Nothing ->
            openStore config >>= \case
                Left err -> fail (Text.unpack (renderStoreError err))
                Right store -> pure (Just store, store)

closeEngineStore :: MVar (Maybe Store) -> IO ()
closeEngineStore state =
    modifyMVar state \case
        Nothing -> pure (Nothing, ())
        Just store -> closeStore store >> pure (Nothing, ())

boundedEventText :: Text -> (Text, Bool)
boundedEventText value =
    let (visible, remainder) = Text.splitAt 8192 value
    in (visible, not (Text.null remainder))

successEvent :: Aeson.ToJSON value => Text -> value -> Aeson.Value
successEvent requestId result =
    Aeson.object
        [ "id" Aeson..= requestId
        , "ok" Aeson..= True
        , "result" Aeson..= result
        ]

failureEvent :: Text -> Text -> Aeson.Value
failureEvent requestId message =
    Aeson.object
        [ "id" Aeson..= requestId
        , "ok" Aeson..= False
        , "error" Aeson..= message
        ]

sendEvent :: FunPtr EventCallback -> Ptr () -> Aeson.Value -> IO ()
sendEvent callback context event =
    void $ tryAny $
        BS.useAsCStringLen (LBS.toStrict (Aeson.encode event)) \(bytes, length) ->
            invokeEventCallback callback
                context
                (castPtr bytes)
                (fromIntegral length)
