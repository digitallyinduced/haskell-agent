-- | Mailbox-driven task admission, worker supervision, and cancellation/join.
-- Gateway leases and terminal callback checks remain within their owning scopes.
module Agent.CLI.MacOS.NativeSupervisor
    ( IntegrationWorkerRegistry
    , launchIntegrationWorkerWith
    , newIntegrationWorkerRegistry
    , shutdownIntegrationWorkers
    , supervisorLoop
    , shutdownRunningTurns
    ) where

import Agent.CLI.MacOS.AgentSnapshot (activeAgentSnapshot)
import Agent.CLI.MacOS.BrowserBridge (BrowserHost, browserToolsWhenEnabled)
import Agent.CLI.MacOS.ComputerBridge
    ( ComputerHost, computerToolSessionWhenEnabled )
import Agent.CLI.MacOS.EngineCallbacks
    ( IntegrationResultCallback
    , invokeIntegrationResultCallback
    , invokeTaskSnapshotCallback
    )
import Agent.CLI.MacOS.EngineEvents
import Agent.CLI.MacOS.EngineMailbox
    ( EngineMailbox, acceptEngineCommand, readEngineCommand )
import Agent.CLI.MacOS.EngineState (EngineCommand(..))
import Agent.CLI.MacOS.InteractionState (InteractionRuntime)
import Agent.CLI.MacOS.Marshalling (withText)
import Agent.CLI.MacOS.McpAdminBridge
    ( mcpAdminTry, emitMcpResult, invokeMcpResultCallback )
import Agent.CLI.MacOS.NativeGatewayBoundary
import Agent.CLI.MacOS.NativeInteraction (resolveApproval)
import Agent.CLI.MacOS.NativeRequest
import Agent.CLI.MacOS.NativeRequestHandler
    ( handleRequest, nativeRequestRequiresGatewayLock )
import Agent.CLI.MacOS.NativeSessionCommands
    ( runSessionMutation, runConversationSearch, sendSearchFailure )
import Agent.CLI.MacOS.TurnEvents
import Agent.CLI.MacOS.TurnExecution (runNativeTurn)
import Agent.CLI.MacOS.TurnState
import qualified Agent.CLI.GatewayBoundary as GatewayBoundary
import Agent.CLI.GatewayClient
    ( withGatewayCredentialLease, withGatewayCredentialTurnLease )
import Agent.CLI.McpAdmin
    ( McpAdminError, McpAdminSnapshot(..), restartMcpAdminServer )
import Agent.CLI.NativeRuntime
    ( NativeProcessRuntime
    , nativeProcessIntegrationSupervisor
    , restartNativeMcpRuntime
    )
import Agent.CLI.IntegrationGateway (gatewayIntegrationAuthority)
import Agent.Integration.API
    ( IntegrationError(..)
    , IntegrationRuntime
    , acquireIntegrationRuntime
    , callIntegrationRuntimeAdmin
    , integrationRuntimeAdminDefinitions
    )
import Agent.Json (RawJson, rawJsonBytes)
import Agent.Loop (ImageAttachment, emptyTokenUsage)
import Agent.Runtime.Daemon.TaskScheduler (TaskIdentity(..), selectRunnableTasks)
import Agent.Store.Postgres (ManagedPostgresConfig, Store)
import Agent.Tools.RenderChart (renderChartTool)
import Control.Concurrent.Async (Async, asyncWithUnmask, cancel, waitCatch)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Control.Concurrent.STM
import Control.Exception.Safe (bracket, finally, mask, tryAny)
import Control.Monad (filterM, foldM, forM_, void, when)
import Data.Aeson ((.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import Data.Foldable (toList)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (partition)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Word (Word8, Word64)
import Foreign.C.Types (CSize)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr)
import System.Directory.OsPath (getHomeDirectory)
import System.OsPath (OsPath)

-- | Integration administration can perform filesystem and network I/O. Keep
-- its workers separate from turns so the mailbox remains available for
-- cancellation, approvals, and shutdown, while still joining every callback
-- owner before its engine is destroyed.
data IntegrationWorkerRegistry = IntegrationWorkerRegistry
    { integrationWorkerNextId :: !(TVar Word64)
    , integrationWorkers :: !(TVar (Map Word64 (Async ())))
    }

newIntegrationWorkerRegistry :: IO IntegrationWorkerRegistry
newIntegrationWorkerRegistry =
    IntegrationWorkerRegistry
        <$> newTVarIO 0
        <*> newTVarIO Map.empty

shutdownIntegrationWorkers :: IntegrationWorkerRegistry -> IO ()
shutdownIntegrationWorkers registry = do
    workers <- atomically do
        current <- readTVar registry.integrationWorkers
        writeTVar registry.integrationWorkers Map.empty
        pure (Map.elems current)
    mapM_ cancel workers
    mapM_ waitCatch workers

supervisorLoop
    :: FunPtr EventCallback
    -> Ptr ()
    -> ManagedPostgresConfig
    -> MVar (Maybe Store)
    -> OsPath
    -> NativeProcessRuntime
    -> EngineMailbox EngineCommand
    -> IntegrationWorkerRegistry
    -> TVar (Map Text [ImageAttachment])
    -> BrowserHost
    -> ComputerHost
    -> TVar Bool
    -> TVar (Map Text NativeTurnOptions)
    -> InteractionRuntime
    -> TVar (Map Text RunningTurn)
    -> TaskSupervisor
    -> IO ()
supervisorLoop
        callback context config store root processRuntime commands
        integrationWorkers stagedImages browser computer chartRenderingEnabled
        stagedTurnOptions interactions workerRegistry =
    go
  where
    go supervisor0 = do
        supervisor <- startRunnableTasks supervisor0
        atomically (readEngineCommand commands) >>= handleCommand supervisor

    handleCommand supervisor = \case
        EngineStop ->
            shutdownSupervisor supervisor
        EngineSearch query limit searchCallback searchContext -> do
            GatewayBoundary.withCurrentGatewayBoundary
                (\boundary ->
                    runConversationSearch
                        config
                        store
                        boundary.gatewayBoundaryIdentity
                        query
                        limit
                        searchCallback
                        searchContext) >>= \case
                Left err ->
                    sendSearchFailure searchCallback searchContext
                        (GatewayBoundary.renderGatewayBoundaryError err)
                Right () -> pure ()
            go supervisor
        EngineSessionMutation mutation resultCallback resultContext -> do
            runSessionMutation
                config store root mutation resultCallback resultContext
            go supervisor
        EngineMcpRestart expected name resultCallback resultContext -> do
            if Map.null supervisor.supervisorRunning
                then do
                    restarted <- tryAny do
                        home <- getHomeDirectory
                        mcpAdminTry
                            (restartMcpAdminServer home expected name
                                (restartNativeMcpRuntime processRuntime))
                    case restarted of
                        Left exception ->
                            withText (Text.pack (show exception)) $
                                invokeMcpResultCallback resultCallback
                                    resultContext (-1) expected
                        Right (Left err) ->
                            emitMcpResult resultCallback resultContext
                                (Left err
                                    :: Either
                                        McpAdminError
                                        (McpAdminSnapshot ()))
                        Right (Right snapshot) ->
                            invokeMcpResultCallback
                                resultCallback
                                resultContext
                                0
                                snapshot.mcpAdminRevision
                                nullPtr
                                0
                else
                    withText "cannot restart MCP while tasks are active" $
                        invokeMcpResultCallback resultCallback resultContext
                            (-1) expected
            go supervisor
        EngineIntegrationAdminList resultCallback resultContext -> do
            launchIntegrationWorker
                integrationWorkers resultCallback resultContext $
                runIntegrationAdmin processRuntime \runtime ->
                    pure (Right (integrationRuntimeAdminDefinitions runtime))
            go supervisor
        EngineIntegrationAdminCall
                name arguments resultCallback resultContext -> do
            launchIntegrationWorker
                integrationWorkers resultCallback resultContext $
                runIntegrationAdmin processRuntime \runtime ->
                    callIntegrationRuntimeAdmin runtime name arguments >>= \case
                        Left err -> pure (Left (renderIntegrationError err))
                        Right result -> pure (Right result)
            go supervisor
        EngineCancelTask taskId -> do
            next <- cancelTaskById supervisor taskId
            go next
        EngineTaskSnapshot snapshotCallback snapshotContext -> do
            sendTaskSnapshot snapshotCallback snapshotContext supervisor
            go supervisor
        EngineSetTaskLimit limit ->
            go supervisor { supervisorLimit = limit }
        EngineTaskSession taskId sessionId -> do
            case Map.lookup taskId supervisor.supervisorRunning of
                Nothing -> pure ()
                Just running -> atomically $
                    writeTVar
                        running.runningTurnControl.turnControlSessionId
                        (Just sessionId)
            go supervisor
        EngineTaskFinished taskId outcome -> do
            case Map.lookup taskId supervisor.supervisorRunning of
                Nothing -> pure ()
                Just running -> do
                    _ <- waitCatch running.runningTurnWorker
                    sessionId <- readTVarIO
                        running.runningTurnControl.turnControlSessionId
                    cancelled <- readTVarIO
                        running.runningTurnControl.turnControlCancelled
                    _ <- emitForNativeGatewayBoundary
                        running.runningTurnControl.turnControlGatewayIdentity $
                            if cancelled
                                then do
                                    sendTaskState
                                        taskId sessionId "cancelled"
                                    sendEvent callback context $
                                        turnFailedEvent
                                            taskId
                                            "turn cancelled"
                                else do
                                    sendTaskState
                                        taskId
                                        (taskResultSessionId
                                            sessionId
                                            outcome)
                                        (taskResultState outcome)
                                    finishTurnEvent
                                        callback context taskId outcome
                    pure ()
            atomically $ modifyTVar' workerRegistry (Map.delete taskId)
            go supervisor
                { supervisorRunning =
                    Map.delete taskId supervisor.supervisorRunning
                }
        EngineRequest request ->
            handleEngineRequest supervisor request >>= go

    handleEngineRequest supervisor request
        | request.requestMethod == "turn.start" =
            enqueueTurn supervisor request
        | request.requestMethod == "turn.cancel" =
            case (parseParams request :: Either Text TurnReference) of
                Left err -> do
                    sendEvent callback context
                        (failureEvent request.requestId err)
                    pure supervisor
                Right reference -> do
                    let active =
                            Map.member
                                reference.turnReferenceId
                                supervisor.supervisorRunning
                        queued = any
                            ((== reference.turnReferenceId)
                                . (.turnStartId)
                                . (.pendingTurnStart))
                            supervisor.supervisorPending
                    next <- cancelTaskById
                        supervisor
                        reference.turnReferenceId
                    sendEvent callback context $
                        if active || queued
                            then successEvent request.requestId True
                            else failureEvent
                                request.requestId
                                "turn id is not active"
                    pure next
        | request.requestMethod == "approval.resolve" =
            case (parseParams request :: Either Text ApprovalResolution) of
                Left err -> do
                    sendEvent callback context
                        (failureEvent request.requestId err)
                    pure supervisor
                Right resolution -> do
                    matching <- filterM
                        (approvalIsActive resolution.approvalResolutionId
                            . (.runningTurnControl))
                        (Map.elems supervisor.supervisorRunning)
                    case matching of
                        [running] ->
                            resolveApproval running.runningTurnControl request
                                >>= sendEvent callback context
                        _ ->
                            sendEvent callback context $
                                failureEvent
                                    request.requestId
                                    "approval request is no longer active"
                    pure supervisor
        | request.requestMethod == "turn.agents" =
            selectRunningTurn request.requestParams supervisor >>= \case
                Left err -> do
                    sendEvent callback context
                        (failureEvent request.requestId err)
                    pure supervisor
                Right Nothing -> do
                    sendEvent callback context $
                        successEvent request.requestId ([] :: [Aeson.Value])
                    pure supervisor
                Right (Just running) ->
                    do
                        _ <- emitForNativeGatewayBoundary
                            running.runningTurnControl.turnControlGatewayIdentity
                            (activeAgentSnapshot
                                running.runningTurnControl
                                request
                                >>= sendEvent callback context)
                        pure supervisor
        | otherwise = do
            let respond = do
                    event <- handleRequest config store root request
                    sendEvent callback context event
            if nativeRequestRequiresGatewayLock request.requestMethod
                then withGatewayCredentialLease respond
                else respond
            pure supervisor

    selectRunningTurn params supervisor =
        case Aeson.parseEither
            (Aeson.withObject "turn reference" (.:? "turnId"))
            params of
            Left err -> pure (Left (Text.pack err))
            Right (Just taskId) ->
                pure $ maybe
                    (Left "turn id is not active")
                    (Right . Just)
                    (Map.lookup taskId supervisor.supervisorRunning)
            Right Nothing ->
                pure $ case Map.elems supervisor.supervisorRunning of
                    [running] -> Right (Just running)
                    [] -> Right Nothing
                    _ -> Left "turnId is required while multiple turns run"

    approvalIsActive approvalId control =
        Map.member approvalId
            <$> readTVarIO control.turnControlApprovals

    enqueueTurn supervisor request =
        case (parseParams request :: Either Text TurnStart) of
            Left err -> do
                atomically $ discardStagedTurn
                    request.requestId
                    request.requestParams
                    stagedImages
                    stagedTurnOptions
                sendEvent callback context
                    (failureEvent request.requestId err)
                pure supervisor
            Right start
                | taskExists start.turnStartId supervisor -> do
                    atomically $ discardStagedTurnById
                        start.turnStartId
                        stagedImages
                        stagedTurnOptions
                    sendEvent callback context $
                        failureEvent request.requestId "turn id already exists"
                    pure supervisor
                | otherwise ->
                    withGatewayCredentialLease $
                        loadNativeGatewayIdentity >>= \case
                            Left err -> do
                                atomically $ discardStagedTurnById
                                    start.turnStartId
                                    stagedImages
                                    stagedTurnOptions
                                sendEvent callback context $
                                    failureEvent request.requestId err
                                pure supervisor
                            Right gatewayIdentity -> do
                                (images, turnOptions) <- atomically $ do
                                    staged <- readTVar stagedImages
                                    writeTVar stagedImages
                                        (Map.delete start.turnStartId staged)
                                    options <- readTVar stagedTurnOptions
                                    writeTVar stagedTurnOptions
                                        (Map.delete start.turnStartId options)
                                    pure
                                        ( Map.findWithDefault
                                            []
                                            start.turnStartId
                                            staged
                                        , Map.findWithDefault
                                            defaultNativeTurnOptions
                                            start.turnStartId
                                            options
                                        )
                                sendEvent callback context $
                                    successEvent request.requestId $
                                        Aeson.object
                                            [ "turnId" Aeson..=
                                                start.turnStartId
                                            , "state" Aeson..=
                                                ("queued" :: Text)
                                            ]
                                sendTaskState
                                    start.turnStartId
                                    start.turnStartSessionId
                                    "queued"
                                pure supervisor
                                    { supervisorPending =
                                        supervisor.supervisorPending
                                            Seq.|> PendingTurn
                                                { pendingTurnStart = start
                                                , pendingTurnGatewayIdentity =
                                                    gatewayIdentity
                                                , pendingTurnImages = images
                                                , pendingTurnOptions =
                                                    turnOptions
                                                }
                                    , supervisorKnownTaskIds =
                                        Set.insert
                                            start.turnStartId
                                            supervisor.supervisorKnownTaskIds
                                    }

    startRunnableTasks supervisor = do
        sessionIds <- activeSessionIds supervisor
        let available =
                supervisor.supervisorLimit
                    - Map.size supervisor.supervisorRunning
            pending = toList supervisor.supervisorPending
            candidates =
                [ ( TaskIdentity
                        pending.pendingTurnStart.turnStartId
                        pending.pendingTurnStart.turnStartSessionId
                  , pending
                  )
                | pending <- pending
                ]
            (selected, remaining) =
                selectRunnableTasks available sessionIds candidates
        running <- foldM
            startTask
            supervisor.supervisorRunning
            (map snd selected)
        pure supervisor
            { supervisorPending = Seq.fromList (map snd remaining)
            , supervisorRunning = running
            }

    startTask
        :: Map Text RunningTurn
        -> PendingTurn
        -> IO (Map Text RunningTurn)
    startTask running pending = do
        let start = pending.pendingTurnStart
        control <- newTurnControl
            start.turnStartId
            pending.pendingTurnGatewayIdentity
            start.turnStartSessionId
            interactions
        nativeBrowserTools <- browserToolsWhenEnabled browser start.turnStartId
        chartsEnabled <- readTVarIO chartRenderingEnabled
        let nativePresentationTools = [renderChartTool | chartsEnabled]
        worker <- launchTrackedWorker start.turnStartId $
            bracket
                (if start.turnStartComputerUse
                    then computerToolSessionWhenEnabled computer
                    else pure (Right Nothing))
                (\case
                    Right (Just (_, _, close)) -> close
                    _ -> pure ())
                (\case
                    Left err -> pure TurnOutcome
                        { turnOutcomeSessionId = start.turnStartSessionId
                        , turnOutcomeError = Just err
                        , turnOutcomeUsage = emptyTokenUsage
                        , turnOutcomeProviderCostUSD = Nothing
                        }
                    Right nativeComputerTool ->
                        withGatewayCredentialTurnLease $
                            ensureNativeGatewayIdentity
                                pending.pendingTurnGatewayIdentity >>= \case
                                    Left err ->
                                        pure
                                            TurnOutcome
                                                { turnOutcomeSessionId =
                                                    start.turnStartSessionId
                                                , turnOutcomeError = Just err
                                                , turnOutcomeUsage =
                                                    emptyTokenUsage
                                                , turnOutcomeProviderCostUSD =
                                                    Nothing
                                                }
                                    Right () ->
                                        do
                                            sendTaskState
                                                start.turnStartId
                                                start.turnStartSessionId
                                                "running"
                                            sendTurnStatus
                                                callback
                                                context
                                                start.turnStartId
                                                (if start.turnStartWorktree
                                                    then "Creating worktree…"
                                                    else "Starting…")
                                            runNativeTurn
                                                callback
                                                context
                                                commands
                                                processRuntime
                                                control
                                                (nativeBrowserTools <> nativePresentationTools)
                                                nativeComputerTool
                                                start
                                                pending.pendingTurnImages
                                                pending.pendingTurnOptions
                                                interactions)
        let runningTurn =
                RunningTurn
                    { runningTurnControl = control
                    , runningTurnWorker = worker
                    }
        atomically $ modifyTVar' workerRegistry $
            Map.insert start.turnStartId runningTurn
        pure $ Map.insert
            start.turnStartId
            runningTurn
            running

    launchTrackedWorker taskId action =
        mask \_ -> do
            gate <- newEmptyMVar
            worker <- asyncWithUnmask \unmask -> do
                takeMVar gate
                outcome <- newIORef (TaskFailure "turn cancelled")
                (tryAny (unmask action) >>= \case
                    Left exception ->
                        writeIORef outcome
                            (TaskFailure (Text.pack (show exception)))
                    Right value ->
                        writeIORef outcome (TaskOutcome value))
                    `finally` do
                        result <- readIORef outcome
                        void $ atomically $ acceptEngineCommand
                            commands
                            (EngineTaskFinished taskId result)
            putMVar gate ()
            pure worker

    activeSessionIds supervisor = do
        sessions <- mapM
            (readTVarIO . (.turnControlSessionId) . (.runningTurnControl))
            (Map.elems supervisor.supervisorRunning)
        pure (Set.fromList [session | Just session <- sessions])

    cancelTaskById supervisor taskId =
        case Map.lookup taskId supervisor.supervisorRunning of
            Just running -> do
                cancelTurn running.runningTurnControl
                cancel running.runningTurnWorker
                pure supervisor
            Nothing -> do
                let (cancelled, retained) = partition
                        ((== taskId)
                            . (.turnStartId)
                            . (.pendingTurnStart))
                        (toList supervisor.supervisorPending)
                forM_ cancelled \pending ->
                    let start = pending.pendingTurnStart
                    in do
                        _ <- emitForNativeGatewayBoundary
                            pending.pendingTurnGatewayIdentity do
                                sendTaskState
                                    start.turnStartId
                                    start.turnStartSessionId
                                    "cancelled"
                                sendEvent callback context $
                                    turnFailedEvent
                                        start.turnStartId
                                        "turn cancelled"
                        pure ()
                pure supervisor
                    { supervisorPending = Seq.fromList retained }

    taskExists taskId supervisor =
        Set.member taskId supervisor.supervisorKnownTaskIds

    shutdownSupervisor _ =
        shutdownRunningTurns workerRegistry
            `finally` shutdownIntegrationWorkers integrationWorkers

    sendTaskState :: Text -> Maybe Text -> Text -> IO ()
    sendTaskState taskId sessionId state =
        sendEvent callback context $
            Aeson.object
                [ "event" Aeson..= ("task.state" :: Text)
                , "taskId" Aeson..= taskId
                , "sessionId" Aeson..= sessionId
                , "state" Aeson..= state
                ]

    sendTaskSnapshot snapshotCallback snapshotContext supervisor =
        withGatewayCredentialLease $
            loadNativeGatewayIdentity >>= \case
                Left err ->
                    withTextBytes err \errorPointer errorLength ->
                        invokeTaskSnapshotCallback
                            snapshotCallback
                            snapshotContext
                            (-1)
                            nullPtr 0 nullPtr 0 0
                            errorPointer errorLength
                Right gatewayIdentity -> do
                    forM_ supervisor.supervisorPending \pending ->
                        when
                            (nativeTurnRouteMatchesBoundary
                                pending.pendingTurnGatewayIdentity
                                gatewayIdentity) $
                                sendSnapshotItem
                                    snapshotCallback
                                    snapshotContext
                                    pending.pendingTurnStart.turnStartId
                                    pending.pendingTurnStart.turnStartSessionId
                                    0
                    forM_
                        (filter
                            (\running ->
                                let control = running.runningTurnControl
                                in nativeTurnRouteMatchesBoundary
                                    control.turnControlGatewayIdentity
                                    gatewayIdentity)
                            (Map.elems supervisor.supervisorRunning))
                        \running -> do
                            sessionId <- readTVarIO
                                running.runningTurnControl.turnControlSessionId
                            sendSnapshotItem
                                snapshotCallback
                                snapshotContext
                                running.runningTurnControl.turnControlId
                                sessionId
                                1
                    invokeTaskSnapshotCallback
                        snapshotCallback snapshotContext
                        1 nullPtr 0 nullPtr 0 0 nullPtr 0

    sendSnapshotItem snapshotCallback snapshotContext taskId sessionId state =
        withTextBytes taskId \taskPointer taskLength ->
        withMaybeTextBytes sessionId \sessionPointer sessionLength ->
            invokeTaskSnapshotCallback snapshotCallback snapshotContext
                0 taskPointer taskLength sessionPointer sessionLength
                state nullPtr 0

shutdownRunningTurns :: TVar (Map Text RunningTurn) -> IO ()
shutdownRunningTurns workerRegistry = do
    running <- atomically do
        current <- readTVar workerRegistry
        writeTVar workerRegistry Map.empty
        pure (Map.elems current)
    forM_ running (cancelTurn . (.runningTurnControl))
    mapM_ (cancel . (.runningTurnWorker)) running
    mapM_ (waitCatch . (.runningTurnWorker)) running

-- | Keep the gateway credential lease while selecting and using the runtime.
-- A connected gateway is authoritative, matching turn startup; a direct
-- engine uses local integrations.
runIntegrationAdmin
    :: NativeProcessRuntime
    -> (IntegrationRuntime -> IO (Either Text RawJson))
    -> IO (Either Text RawJson)
runIntegrationAdmin processRuntime action =
    withNativeGatewayCredentialBoundary \credential _ -> do
        let authority = gatewayIntegrationAuthority credential
        acquireIntegrationRuntime
            (nativeProcessIntegrationSupervisor processRuntime)
            authority >>= \case
                Left err -> pure (Left err)
                Right runtime -> action runtime

-- | A command is accepted only after the worker is registered. The gate keeps
-- cancellation from racing registration. Normal completion and the shutdown
-- finalizer share one claim, so every accepted command attempts one callback.
launchIntegrationWorker
    :: IntegrationWorkerRegistry
    -> FunPtr IntegrationResultCallback
    -> Ptr ()
    -> IO (Either Text RawJson)
    -> IO ()
launchIntegrationWorker registry callback context action =
    launchIntegrationWorkerWith
        registry
        (either
            (sendIntegrationFailure callback context)
            (sendIntegrationResult callback context))
        action

launchIntegrationWorkerWith
    :: IntegrationWorkerRegistry
    -> (Either Text RawJson -> IO ())
    -> IO (Either Text RawJson)
    -> IO ()
launchIntegrationWorkerWith registry complete action =
    mask \_ -> do
        completionClaimed <- newTVarIO False
        let finish outcome = do
                claimed <- atomically do
                    completed <- readTVar completionClaimed
                    if completed
                        then pure False
                        else writeTVar completionClaimed True >> pure True
                when claimed $ void (tryAny (complete outcome))
        workerId <- atomically do
            next <- readTVar registry.integrationWorkerNextId
            writeTVar registry.integrationWorkerNextId (next + 1)
            pure next
        gate <- newEmptyMVar
        worker <- asyncWithUnmask \unmask ->
            (do
                result <- tryAny do
                    takeMVar gate
                    unmask action
                finish $
                    case result of
                        Left _ ->
                            Left "integration admin operation failed"
                        Right outcome -> outcome)
                `finally` do
                    finish
                        (Left
                            "engine stopped before integration operation completed")
                    atomically
                        (modifyTVar'
                            registry.integrationWorkers
                            (Map.delete workerId))
        atomically $
            modifyTVar' registry.integrationWorkers
                (Map.insert workerId worker)
        putMVar gate ()

sendIntegrationResult
    :: FunPtr IntegrationResultCallback
    -> Ptr ()
    -> RawJson
    -> IO ()
sendIntegrationResult callback context result =
    BS.useAsCStringLen (rawJsonBytes result) \(pointer, length) ->
        invokeIntegrationResultCallback
            callback context 0
            pointer (fromIntegral length)
            nullPtr 0

sendIntegrationFailure
    :: FunPtr IntegrationResultCallback
    -> Ptr ()
    -> Text
    -> IO ()
sendIntegrationFailure callback context message =
    withText message \pointer length ->
        invokeIntegrationResultCallback
            callback context (-1)
            nullPtr 0
            pointer length

renderIntegrationError :: IntegrationError -> Text
renderIntegrationError = \case
    IntegrationInvalidInput message -> message
    IntegrationUnavailable message -> message
    IntegrationOperationFailed message -> message

withTextBytes :: Text -> (Ptr Word8 -> CSize -> IO a) -> IO a
withTextBytes value action =
    BS.useAsCStringLen (TextEncoding.encodeUtf8 value) \(pointer, length) ->
        action (castPtr pointer) (fromIntegral length)

withMaybeTextBytes
    :: Maybe Text
    -> (Ptr Word8 -> CSize -> IO a)
    -> IO a
withMaybeTextBytes Nothing action = action nullPtr 0
withMaybeTextBytes (Just value) action = withTextBytes value action
