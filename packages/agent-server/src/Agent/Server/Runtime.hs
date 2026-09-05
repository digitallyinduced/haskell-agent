-- | Production PostgreSQL and in-process native-runtime adapter.
module Agent.Server.Runtime
    ( ServerRuntime
    , openServerRuntime
    , closeServerRuntime
    , serverRuntimeBackend
    ) where

import Agent.CLI.GatewayBoundary
    ( GatewayBoundaryError(..)
    , GatewayBoundarySnapshot(..)
    , loadGatewayBoundarySnapshotAt
    , renderGatewayBoundaryError
    , validateGatewayBoundary
    , withCurrentGatewayBoundaryAt
    , withExpectedGatewayBoundaryAt
    , withGatewayTurnBoundaryAt
    )
import Agent.Tools.Types (appToolsFromGroups)
import Agent.CLI.GatewayModels
    ( loadGatewayModelOptionsWithCredentialAt )
import Agent.CLI.ModelConfig
    ( ModelCatalog
    , organizationGatewayConnectionId
    )
import Agent.CLI.Models
    ( ModelOption(..)
    , ModelTarget(..)
    , defaultModelOptionFor
    , modelCatalog
    )
import Agent.CLI.NativeRuntime
    ( NativeInteractionMode(..)
    , NativeDiscoveryContext(..)
    , NativeWorkspaceDiscovery(..)
    , NativeRunCapabilities(..)
    , NativeProcessRuntime
    , NativeRunHooks(..)
    , NativeSessionTarget(..)
    , NativeShellMode(..)
    , NativeTurnRequest(..)
    , closeNativeProcessRuntime
    , fullNativeRunCapabilities
    , newNativeProcessRuntime
    , runNativeTurn
    )
import Agent.CLI.Options (CliOptions(..))
import Agent.CLI.Project (defaultProjectSettings)
import Agent.CLI.Permission.Types (PermissionChoice(..))
import Agent.CLI.Runtime.Options (defaultEffortFor)
import Agent.CLI.Session
    ( SessionCreate(..)
    , SessionHandle(..)
    , SessionMeta(..)
    , SessionTurnPage(..)
    , createSession
    , deleteSession
    , forkSessionAt
    , forkSessionAtTurn
    , isValidSessionId
    , loadRecentSessionHistoryTurns
    , loadSessionHandle
    , loadSessionHistoryTurnsBefore
    , renameSession
    , sessionsRoot
    , setSessionArchived
    )
import Agent.CLI.Session.Codec (fromStoredMetadata)
import Agent.Dialect (DialectId, dialectSlug)
import Agent.Loop qualified as Loop
import Agent.OsPath (unsafeToFilePath)
import Agent.Provider
    ( Provider(..)
    , providerSlug
    )
import Agent.ReasoningEffort
    ( ReasoningEffort
    , parseReasoningEffort
    , reasoningEffortText
    )
import Agent.Server.Backend (Backend(..))
import Agent.Server.Config
    ( MultiTenantConfig(..)
    , ResolvedServerConfig(..)
    , ResolvedServerMode(..)
    , lookupResolvedTenant
    , resolveTenantWorkspacePath
    )
import Agent.Server.Event
    ( boundedPublicText
    , projectAgentEntries
    , projectLoopEvent
    , projectPublicValue
    )
import Agent.Server.Identifier (newUUIDv7Text)
import Agent.Server.Runtime.TurnStore qualified as TurnStore
import Agent.Server.Sandbox
    ( TenantSandbox
    , closeTenantSandbox
    , openTenantSandbox
    , composeSandboxTools
    )
import Agent.Server.Supervisor
    ( TurnControl(..)
    , TurnPersistence(..)
    )
import Agent.Server.Tenant
    ( ResolvedTenant(..) )
import Agent.Server.Types
import Agent.Store.Postgres
    ( Store
    , closeStore
    , managedPostgresConfigFromEnv
    , openStore
    , trustedPool
    )
import Agent.Store.Postgres.Tenant
    ( TenantStoreManager
    , acquireTenantStore
    , checkTenantStoreManager
    , closeTenantStoreManager
    , openTenantStoreManager
    , tenantDatabase
    )
import Agent.Store.Postgres.Session qualified as StoreSession
import Agent.Store.Types
    ( StoreError(..)
    , renderStoreError
    )
import Agent.ToolDispatch (ToolCall(..))
import Agent.Tools.PlanMode
    ( PlanDecision(..)
    , PlanModeHooks(..)
    )
import Control.Applicative ((<|>))
import Control.Exception.Safe
    ( finally
    , mask
    , onException
    , tryAny
    )
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , newEmptyMVar
    , newMVar
    , putMVar
    , readMVar
    )
import Control.Monad (forM_, void)
import Data.Aeson
    ( Value
    , object
    , toJSON
    , (.=)
    )
import Data.Bifunctor (first)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime)
import Data.Time.Format
    ( defaultTimeLocale
    , formatTime
    , parseTimeM
    )
import System.IO
    ( IOMode(WriteMode)
    , withFile
    )
import System.OsPath (OsPath, unsafeEncodeUtf)

data ServerRuntime = ServerRuntime
    { runtimeBackend :: !Backend
    , runtimeClose :: !(IO ())
    }

serverRuntimeBackend :: ServerRuntime -> Backend
serverRuntimeBackend = (.runtimeBackend)

openServerRuntime
    :: ResolvedServerConfig
    -> IO (Either Text ServerRuntime)
openServerRuntime config = mask \restore -> do
    instanceId <- newUUIDv7Text
    case config.resolvedServerMode of
        MultiTenantMode multi -> do
            postgresConfig <-
                managedPostgresConfigFromEnv config.resolvedStateDirectory
            restore
                (openTenantStoreManager
                    postgresConfig
                    config.resolvedMaxActiveTenants) >>= \case
                    Left err -> pure (Left (renderStoreError err))
                    Right stores -> do
                        manager <-
                            newTenantRuntimeManager
                                config
                                multi
                                stores
                                instanceId
                        pure $
                            Right ServerRuntime
                                { runtimeBackend =
                                    multiTenantBackend instanceId manager
                                , runtimeClose =
                                    closeTenantRuntimeManager manager
                                        `finally`
                                            closeTenantStoreManager stores
                                }
        LocalSingleUserMode -> do
            postgresConfig <-
                managedPostgresConfigFromEnv config.resolvedStateDirectory
            restore (openStore postgresConfig) >>= \case
                Left err -> pure (Left (renderStoreError err))
                Right store ->
                    restore
                        (TurnStore.openTurnStoreOwner store instanceId)
                        >>= \case
                            Left err -> do
                                closeStore store
                                pure (Left err)
                            Right turnStoreOwner -> do
                                let root =
                                        sessionsRoot
                                            (unsafeEncodeUtf config.resolvedHome)
                                    closeOwnedStore =
                                        TurnStore.closeTurnStoreOwner
                                            turnStoreOwner
                                            `finally` closeStore store
                                nativeResult <-
                                    tryAny
                                        (restore
                                            (newNativeProcessRuntime root))
                                        `onException` closeOwnedStore
                                case nativeResult of
                                    Left _ -> do
                                        closeOwnedStore
                                        pure
                                            (Left
                                                "could not initialize the native agent runtime")
                                    Right native -> do
                                        let environment = RuntimeEnvironment
                                                { environmentConfig = config
                                                , environmentStore = store
                                                , environmentTurnStoreOwner =
                                                    turnStoreOwner
                                                , environmentNative = native
                                                , environmentRoot = root
                                                , environmentTenantId =
                                                    localTenantId
                                                , environmentHome =
                                                    config.resolvedHome
                                                , environmentSandbox = Nothing
                                                }
                                            backend =
                                                productionBackend
                                                    instanceId
                                                    environment
                                        pure $
                                            Right ServerRuntime
                                                { runtimeBackend = backend
                                                , runtimeClose =
                                                    closeRuntimeEnvironment
                                                        environment
                                                        `finally`
                                                            closeStore store
                                                }

closeServerRuntime :: ServerRuntime -> IO ()
closeServerRuntime = (.runtimeClose)

data RuntimeEnvironment = RuntimeEnvironment
    { environmentConfig :: !ResolvedServerConfig
    , environmentStore :: !Store
    , environmentTurnStoreOwner :: !TurnStore.TurnStoreOwner
    , environmentNative :: !NativeProcessRuntime
    , environmentRoot :: !OsPath
    , environmentTenantId :: !TenantId
    , environmentHome :: !FilePath
    , environmentSandbox :: !(Maybe TenantSandbox)
    }

data TenantRuntimeSlot
    = TenantRuntimeOpening
        !(MVar (Either ApiError RuntimeEnvironment))
    | TenantRuntimeReady !RuntimeEnvironment

data TenantRuntimeState = TenantRuntimeState
    { tenantRuntimeClosed :: !Bool
    , tenantRuntimeSlots :: !(Map TenantId TenantRuntimeSlot)
    }

data TenantRuntimeManager = TenantRuntimeManager
    { tenantRuntimeConfig :: !ResolvedServerConfig
    , tenantRuntimeMultiConfig :: !MultiTenantConfig
    , tenantRuntimeStores :: !TenantStoreManager
    , tenantRuntimeMaximum :: !Int
    , tenantRuntimeInstanceId :: !Text
    , tenantRuntimeState :: !(MVar TenantRuntimeState)
    }

data TenantRuntimeAcquisition
    = TenantRuntimeRejected !ApiError
    | TenantRuntimeReadyNow !RuntimeEnvironment
    | TenantRuntimeWait
        !(MVar (Either ApiError RuntimeEnvironment))
    | TenantRuntimeOpen
        !ResolvedTenant
        !(MVar (Either ApiError RuntimeEnvironment))

newTenantRuntimeManager
    :: ResolvedServerConfig
    -> MultiTenantConfig
    -> TenantStoreManager
    -> Text
    -> IO TenantRuntimeManager
newTenantRuntimeManager config multi stores instanceId = do
    state <- newMVar TenantRuntimeState
        { tenantRuntimeClosed = False
        , tenantRuntimeSlots = Map.empty
        }
    pure TenantRuntimeManager
        { tenantRuntimeConfig = config
        , tenantRuntimeMultiConfig = multi
        , tenantRuntimeStores = stores
        , tenantRuntimeMaximum = config.resolvedMaxActiveTenants
        , tenantRuntimeInstanceId = instanceId
        , tenantRuntimeState = state
        }

acquireTenantRuntime
    :: TenantRuntimeManager
    -> TenantId
    -> IO (Either ApiError RuntimeEnvironment)
acquireTenantRuntime manager tenantId = mask \restore -> do
    decision <- modifyMVar manager.tenantRuntimeState \state ->
        if state.tenantRuntimeClosed
            then
                pure
                    ( state
                    , TenantRuntimeRejected
                        (tenantRuntimeUnavailable
                            "the tenant runtime manager is closed")
                    )
            else case Map.lookup tenantId state.tenantRuntimeSlots of
                Just (TenantRuntimeReady environment) ->
                    pure (state, TenantRuntimeReadyNow environment)
                Just (TenantRuntimeOpening completion) ->
                    pure (state, TenantRuntimeWait completion)
                Nothing
                    | Map.size state.tenantRuntimeSlots
                        >= manager.tenantRuntimeMaximum ->
                        pure
                            ( state
                            , TenantRuntimeRejected
                                (tenantRuntimeUnavailable
                                    "the active tenant runtime limit has been reached")
                            )
                    | otherwise ->
                        case
                            lookupResolvedTenant
                                manager.tenantRuntimeConfig
                                tenantId
                        of
                            Left err ->
                                pure (state, TenantRuntimeRejected err)
                            Right tenant -> do
                                completion <- newEmptyMVar
                                pure
                                    ( state
                                        { tenantRuntimeSlots =
                                            Map.insert
                                                tenantId
                                                (TenantRuntimeOpening completion)
                                                state.tenantRuntimeSlots
                                        }
                                    , TenantRuntimeOpen tenant completion
                                    )
    case decision of
        TenantRuntimeRejected err -> pure (Left err)
        TenantRuntimeReadyNow environment -> pure (Right environment)
        TenantRuntimeWait completion -> restore (readMVar completion)
        TenantRuntimeOpen tenant completion -> do
            outcome <-
                (tryAny (restore (createTenantRuntime manager tenant))
                    >>= \case
                        Left _ ->
                            pure
                                (Left
                                    (tenantRuntimeUnavailable
                                        "tenant runtime initialization failed"))
                        Right result -> pure result)
                    `onException`
                        void
                            (publishTenantRuntime
                                manager
                                tenant.resolvedTenantId
                                completion
                                (Left
                                    (tenantRuntimeUnavailable
                                        "tenant runtime initialization was cancelled")))
            publishTenantRuntime
                manager
                tenant.resolvedTenantId
                completion
                outcome

publishTenantRuntime
    :: TenantRuntimeManager
    -> TenantId
    -> MVar (Either ApiError RuntimeEnvironment)
    -> Either ApiError RuntimeEnvironment
    -> IO (Either ApiError RuntimeEnvironment)
publishTenantRuntime manager tenantId completion outcome = do
    effective <- modifyMVar manager.tenantRuntimeState \state ->
        if state.tenantRuntimeClosed
            then
                pure
                    ( state
                    , case outcome of
                        Left err -> Left err
                        Right _ ->
                            Left
                                (tenantRuntimeUnavailable
                                    "the tenant runtime manager is closed")
                    )
            else
                pure
                    ( state
                        { tenantRuntimeSlots =
                            case outcome of
                                Left _ ->
                                    Map.delete tenantId state.tenantRuntimeSlots
                                Right environment ->
                                    Map.insert
                                        tenantId
                                        (TenantRuntimeReady environment)
                                        state.tenantRuntimeSlots
                        }
                    , outcome
                    )
    case (outcome, effective) of
        (Right environment, Left _) ->
            void (tryAny (closeRuntimeEnvironment environment))
        _ -> pure ()
    putMVar completion effective
    pure effective

createTenantRuntime
    :: TenantRuntimeManager
    -> ResolvedTenant
    -> IO (Either ApiError RuntimeEnvironment)
createTenantRuntime manager tenant = mask \restore ->
    case
        tenantDatabase
            tenant.resolvedTenantDatabase
            tenant.resolvedTenantRuntimeRole
    of
        Left err -> pure (Left (storeApiError err))
        Right database ->
            openTenantSandbox
                manager.tenantRuntimeMultiConfig.multiTenantSandboxRunner
                tenant >>= \case
                    Left err ->
                        pure (Left (tenantRuntimeUnavailable err))
                    Right sandbox -> do
                        let root =
                                sessionsRoot
                                    (unsafeEncodeUtf tenant.resolvedTenantHome)
                        nativeResult <-
                            tryAny
                                (restore (newNativeProcessRuntime root))
                                `onException` closeTenantSandbox sandbox
                        case nativeResult of
                            Left _ -> do
                                closeTenantSandbox sandbox
                                pure
                                    (Left
                                        (tenantRuntimeUnavailable
                                            "could not initialize the tenant agent runtime"))
                            Right native ->
                                let closeComponents =
                                        closeNativeProcessRuntime native
                                            `finally`
                                                closeTenantSandbox sandbox
                                in restore
                                    (acquireTenantStore
                                        manager.tenantRuntimeStores
                                        database) `onException`
                                            closeComponents >>= \case
                                        Left err -> do
                                            closeComponents
                                            pure (Left (storeApiError err))
                                        Right store ->
                                            restore
                                                (TurnStore.openTurnStoreOwner
                                                    store
                                                    manager.tenantRuntimeInstanceId)
                                                `onException` closeComponents
                                                >>= \case
                                                    Left err -> do
                                                        closeComponents
                                                        pure
                                                            (Left
                                                                (tenantRuntimeUnavailable
                                                                    err))
                                                    Right turnStoreOwner ->
                                                        pure $
                                                            Right
                                                                RuntimeEnvironment
                                                                    { environmentConfig =
                                                                        manager.tenantRuntimeConfig
                                                                    , environmentStore =
                                                                        store
                                                                    , environmentTurnStoreOwner =
                                                                        turnStoreOwner
                                                                    , environmentNative =
                                                                        native
                                                                    , environmentRoot =
                                                                        root
                                                                    , environmentTenantId =
                                                                        tenant.resolvedTenantId
                                                                    , environmentHome =
                                                                        tenant.resolvedTenantHome
                                                                    , environmentSandbox =
                                                                        Just sandbox
                                                                    }

closeTenantRuntimeManager :: TenantRuntimeManager -> IO ()
closeTenantRuntimeManager manager = mask \restore -> do
    (environments, openings) <-
        modifyMVar manager.tenantRuntimeState \state ->
            if state.tenantRuntimeClosed
                then pure (state, ([], []))
                else
                    pure
                        ( state
                            { tenantRuntimeClosed = True
                            , tenantRuntimeSlots = Map.empty
                            }
                        , ( [ environment
                            | TenantRuntimeReady environment <-
                                Map.elems state.tenantRuntimeSlots
                            ]
                          , [ completion
                            | TenantRuntimeOpening completion <-
                                Map.elems state.tenantRuntimeSlots
                            ]
                          )
                        )
    let closeReady =
            forM_ environments \environment ->
                void
                    (tryAny
                        (restore (closeRuntimeEnvironment environment)))
        waitOpening =
            forM_ openings \completion ->
                void (restore (readMVar completion))
    closeReady `finally` waitOpening

closeRuntimeEnvironment :: RuntimeEnvironment -> IO ()
closeRuntimeEnvironment environment =
    closeNativeProcessRuntime environment.environmentNative
        `finally`
            ( mapM_ closeTenantSandbox environment.environmentSandbox
                `finally`
                    TurnStore.closeTurnStoreOwner
                        environment.environmentTurnStoreOwner
            )

multiTenantBackend :: Text -> TenantRuntimeManager -> Backend
multiTenantBackend instanceId manager = Backend
    { backendAdmitBoundary = \principal action ->
        withTenantEnvironment manager principal.principalTenantId \environment ->
            first gatewayApiError
                <$> withCurrentGatewayBoundaryAt
                    (unsafeEncodeUtf environment.environmentHome)
                    (action . accessBoundary principal)
    , backendContinueBoundary = \boundary action ->
        withTenantEnvironment manager boundary.accessTenantId \environment ->
            first gatewayApiError
                <$> withExpectedGatewayBoundaryAt
                    (unsafeEncodeUtf environment.environmentHome)
                    boundary.accessGatewayBoundary
                    action
    , backendTurnBoundaryGuard = \boundary action ->
        acquireTenantRuntime manager boundary.accessTenantId >>= \case
            Left err -> pure (Left err.apiErrorMessage)
            Right environment -> do
                fenced <-
                    TurnStore.withTurnStoreOwnerFence
                        environment.environmentTurnStoreOwner
                        $ first gatewayTurnError
                            <$> withGatewayTurnBoundaryAt
                                (unsafeEncodeUtf environment.environmentHome)
                                boundary.accessGatewayBoundary
                                action
                pure (fenced >>= id)
    , backendCheckReady =
        first storeApiError
            <$> checkTenantStoreManager manager.tenantRuntimeStores
    , backendListModels = \boundary ->
        withBoundaryBackend instanceId manager boundary \backend ->
            backend.backendListModels boundary
    , backendListSessions = \boundary archive cursor limit ->
        withBoundaryBackend instanceId manager boundary \backend ->
            backend.backendListSessions boundary archive cursor limit
    , backendCreateSession = \boundary request ->
        withBoundaryBackend instanceId manager boundary \backend ->
            backend.backendCreateSession boundary request
    , backendGetSession = \boundary sessionId ->
        withBoundaryBackend instanceId manager boundary \backend ->
            backend.backendGetSession boundary sessionId
    , backendPatchSession = \boundary sessionId request ->
        withBoundaryBackend instanceId manager boundary \backend ->
            backend.backendPatchSession boundary sessionId request
    , backendDeleteSession = \boundary sessionId ->
        withBoundaryBackend instanceId manager boundary \backend ->
            backend.backendDeleteSession boundary sessionId
    , backendSessionHistory = \boundary sessionId before limit ->
        withBoundaryBackend instanceId manager boundary \backend ->
            backend.backendSessionHistory boundary sessionId before limit
    , backendForkSession = \boundary sessionId request ->
        withBoundaryBackend instanceId manager boundary \backend ->
            backend.backendForkSession boundary sessionId request
    , backendReserveTurn =
        \boundary sessionId clientRequestId prompt turnId now ->
            withBoundaryBackend instanceId manager boundary \backend ->
                backend.backendReserveTurn
                    boundary
                    sessionId
                    clientRequestId
                    prompt
                    turnId
                    now
    , backendReserveSessionMutation = \boundary sessionId now ->
        withBoundaryBackend instanceId manager boundary \backend ->
            backend.backendReserveSessionMutation boundary sessionId now
    , backendLookupTurn = \boundary turnId ->
        withBoundaryBackend instanceId manager boundary \backend ->
            backend.backendLookupTurn boundary turnId
    , backendListTurns = \boundary sessionId ->
        withBoundaryBackend instanceId manager boundary \backend ->
            backend.backendListTurns boundary sessionId
    , backendLookupTurnResult = \boundary turnId ->
        withBoundaryBackend instanceId manager boundary \backend ->
            backend.backendLookupTurnResult boundary turnId
    , backendRequestTurnCancellation = \boundary turnId requestedAt ->
        withBoundaryBackend instanceId manager boundary \backend ->
            backend.backendRequestTurnCancellation
                boundary
                turnId
                requestedAt
    , backendTurnPersistence =
        multiTenantTurnPersistence instanceId manager
    , backendRunTurn = \control spec ->
        acquireTenantRuntime
            manager
            spec.turnSpecBoundary.accessTenantId >>= \case
                Left err -> pure (Left err.apiErrorMessage)
                Right environment -> runTurn environment control spec
    }

withTenantEnvironment
    :: TenantRuntimeManager
    -> TenantId
    -> (RuntimeEnvironment -> IO (Either ApiError value))
    -> IO (Either ApiError value)
withTenantEnvironment manager tenantId action =
    acquireTenantRuntime manager tenantId >>= \case
        Left err -> pure (Left err)
        Right environment -> action environment

withBoundaryBackend
    :: Text
    -> TenantRuntimeManager
    -> AccessBoundary
    -> (Backend -> IO (Either ApiError value))
    -> IO (Either ApiError value)
withBoundaryBackend instanceId manager boundary action =
    withTenantEnvironment manager boundary.accessTenantId
        (action . productionBackend instanceId)

withBoundaryBackendText
    :: Text
    -> TenantRuntimeManager
    -> AccessBoundary
    -> (Backend -> IO (Either Text value))
    -> IO (Either Text value)
withBoundaryBackendText instanceId manager boundary action =
    acquireTenantRuntime manager boundary.accessTenantId >>= \case
        Left err -> pure (Left err.apiErrorMessage)
        Right environment ->
            action (productionBackend instanceId environment)

tenantRuntimeUnavailable :: Text -> ApiError
tenantRuntimeUnavailable message = ApiError
    { apiErrorStatus = 503
    , apiErrorCode = "tenant_runtime_unavailable"
    , apiErrorMessage = message
    , apiErrorDetails = Nothing
    }

productionBackend :: Text -> RuntimeEnvironment -> Backend
productionBackend _instanceId environment = Backend
    { backendAdmitBoundary = \principal action ->
        first gatewayApiError
            <$> withCurrentGatewayBoundaryAt
                home
                (action . accessBoundary principal)
    , backendContinueBoundary = \boundary action ->
        first gatewayApiError
            <$> withExpectedGatewayBoundaryAt
                home
                boundary.accessGatewayBoundary
                action
    , backendTurnBoundaryGuard = \boundary action -> do
        fenced <-
            TurnStore.withTurnStoreOwnerFence
                environment.environmentTurnStoreOwner
                $ first gatewayTurnError
                    <$> withGatewayTurnBoundaryAt
                        home
                        boundary.accessGatewayBoundary
                        action
        pure (fenced >>= id)
    , backendCheckReady =
        fmap (first storeApiError . fmap (const ())) $
            StoreSession.loadSessionMetadata
                (trustedPool environment.environmentStore)
                "__agent_server_readiness__"
    , backendListModels = listModels environment
    , backendListSessions = listSessions environment
    , backendCreateSession = createSessionForBoundary environment
    , backendGetSession = getSessionForBoundary environment
    , backendPatchSession = patchSessionForBoundary environment
    , backendDeleteSession = deleteSessionForBoundary environment
    , backendSessionHistory = sessionHistoryForBoundary environment
    , backendForkSession = forkSessionForBoundary environment
    , backendReserveSessionMutation =
        turnStore.turnStoreReserveSessionMutation
    , backendReserveTurn = turnStore.turnStoreReserve
    , backendLookupTurn = turnStore.turnStoreLookup
    , backendListTurns = turnStore.turnStoreList
    , backendLookupTurnResult = turnStore.turnStoreLookupResult
    , backendRequestTurnCancellation =
        turnStore.turnStoreRequestCancellation
    , backendTurnPersistence = turnStore.turnStorePersistence
    , backendRunTurn = runTurn environment
    }
  where
    home = unsafeEncodeUtf environment.environmentHome
    turnStore =
        TurnStore.newTurnStoreBackend
            environment.environmentStore
            environment.environmentTenantId
            environment.environmentTurnStoreOwner

multiTenantTurnPersistence
    :: Text
    -> TenantRuntimeManager
    -> TurnPersistence
multiTenantTurnPersistence instanceId manager =
    TurnPersistence
        { turnPersistenceStarted = \record startedAt ->
            withBoundaryBackendText
                instanceId
                manager
                record.turnRecordBoundary
                \backend ->
                    backend.backendTurnPersistence.turnPersistenceStarted
                        record
                        startedAt
        , turnPersistenceTerminal = \record finishedAt outcome ->
            withBoundaryBackendText
                instanceId
                manager
                record.turnRecordBoundary
                \backend ->
                    backend.backendTurnPersistence.turnPersistenceTerminal
                        record
                        finishedAt
                        outcome
        , turnPersistenceShouldCancel = \record ->
            withBoundaryBackendText
                instanceId
                manager
                record.turnRecordBoundary
                \backend ->
                    backend.backendTurnPersistence.turnPersistenceShouldCancel
                        record
        , turnPersistenceCreateHumanRequest = \record request ->
            withBoundaryBackendText
                instanceId
                manager
                record.turnRecordBoundary
                \backend ->
                    backend.backendTurnPersistence.turnPersistenceCreateHumanRequest
                        record
                        request
        , turnPersistenceListHumanRequests = \boundary turnId ->
            withBoundaryBackendText
                instanceId
                manager
                boundary
                \backend ->
                    backend.backendTurnPersistence.turnPersistenceListHumanRequests
                        boundary
                        turnId
        , turnPersistenceResolveHumanRequest =
            \boundary requestId response ->
                withBoundaryBackendText
                    instanceId
                    manager
                    boundary
                    \backend ->
                        backend.backendTurnPersistence.turnPersistenceResolveHumanRequest
                            boundary
                            requestId
                            response
        , turnPersistenceLoadHumanResponse = \record requestId ->
            withBoundaryBackendText
                instanceId
                manager
                record.turnRecordBoundary
                \backend ->
                    backend.backendTurnPersistence.turnPersistenceLoadHumanResponse
                        record
                        requestId
        , turnPersistenceDeleteHumanRequest = \record requestId disposition ->
            withBoundaryBackendText
                instanceId
                manager
                record.turnRecordBoundary
                \backend ->
                    backend.backendTurnPersistence.turnPersistenceDeleteHumanRequest
                        record
                        requestId
                        disposition
        }

gatewayApiError :: GatewayBoundaryError -> ApiError
gatewayApiError err =
    case err of
        GatewayBoundaryCredentialLoadFailed _ ->
            ApiError
                { apiErrorStatus = 503
                , apiErrorCode = "gateway_unavailable"
                , apiErrorMessage =
                    "gateway credentials are unavailable"
                , apiErrorDetails = Nothing
                }
        GatewayBoundaryChanged ->
            ApiError
                { apiErrorStatus = 409
                , apiErrorCode = "gateway_boundary_changed"
                , apiErrorMessage = renderGatewayBoundaryError err
                , apiErrorDetails = Nothing
                }
        GatewayBoundarySessionRejected _ ->
            ApiError
                { apiErrorStatus = 404
                , apiErrorCode = "session_not_found"
                , apiErrorMessage = "session not found"
                , apiErrorDetails = Nothing
                }

gatewayTurnError :: GatewayBoundaryError -> Text
gatewayTurnError = \case
    GatewayBoundaryCredentialLoadFailed _ ->
        "gateway credentials are unavailable"
    GatewayBoundaryChanged ->
        "gateway credentials changed before the turn started"
    GatewayBoundarySessionRejected _ ->
        "session does not belong to the current gateway boundary"

listModels
    :: RuntimeEnvironment
    -> AccessBoundary
    -> IO (Either ApiError Value)
listModels environment boundary =
    loadModelOptions environment boundary
        environment.environmentConfig.resolvedDefaultCwd
        >>= pure . fmap
            (\(_, options) ->
                object
                    [ "data" .= map modelOptionValue options
                    ])

listSessions
    :: RuntimeEnvironment
    -> AccessBoundary
    -> SessionArchiveFilter
    -> Maybe Text
    -> Int
    -> IO (Either ApiError Value)
listSessions environment boundary archiveFilter rawCursor limit =
    case traverse decodeCursor rawCursor of
        Left err -> pure (Left err)
        Right cursor ->
            StoreSession.listSessionMetadataForBoundary
                pool
                organizationGatewayConnectionId
                boundary.accessGatewayBoundary.gatewayBoundaryIdentity
                (storeArchiveFilter archiveFilter)
                cursor
                limit >>= \case
                    Left err -> pure (Left (storeApiError err))
                    Right page ->
                        case traverse
                            (\entry ->
                                (, entry.sessionListEntryArchived)
                                    <$> fromStoredMetadata
                                        entry.sessionListEntryMetadata)
                            page.sessionListPageSessions of
                            Left err ->
                                pure
                                    (Left
                                        (internalApiError
                                            ("could not decode session metadata: "
                                                <> err)))
                            Right sessions ->
                                pure $
                                    Right $
                                        object
                                            [ "data" .=
                                                map
                                                    (\(meta, archived) ->
                                                        sessionValue
                                                            archived
                                                            meta)
                                                    sessions
                                            , "nextCursor"
                                                .= fmap encodeCursor
                                                    page.sessionListPageNextCursor
                                            ]
  where
    pool = trustedPool environment.environmentStore

createSessionForBoundary
    :: RuntimeEnvironment
    -> AccessBoundary
    -> CreateSessionRequest
    -> IO (Either ApiError Value)
createSessionForBoundary environment boundary request =
    resolveTenantWorkspacePath
        environment.environmentConfig
        boundary.accessTenantId
        request.createSessionCwd >>= \case
            Left err -> pure (Left err)
            Right cwd ->
                loadModelOptions environment boundary cwd >>= \case
                    Left err -> pure (Left err)
                    Right (catalog, options) ->
                        case selectModel
                            boundary
                            catalog
                            options
                            request.createSessionModel of
                                Left err -> pure (Left err)
                                Right option ->
                                    case resolveEffort
                                        option.modelTarget.targetProvider
                                        request.createSessionEffort of
                                            Left err -> pure (Left err)
                                            Right effort -> do
                                                created <- tryAny $
                                                    createSession SessionCreate
                                                        { createPool =
                                                            trustedPool
                                                                environment.environmentStore
                                                        , createRoot =
                                                            environment.environmentRoot
                                                        , createTarget =
                                                            option.modelTarget
                                                        , createGatewayIdentity =
                                                            boundary.accessGatewayBoundary.gatewayBoundaryIdentity
                                                        , createCwd =
                                                            unsafeEncodeUtf cwd
                                                        , createEffort =
                                                            reasoningEffortText effort
                                                        , createTitleHint =
                                                            request.createSessionTitle
                                                        , createTitleIsManual =
                                                            maybe
                                                                False
                                                                (const True)
                                                                request.createSessionTitle
                                                        }
                                                pure case created of
                                                    Left _ ->
                                                        Left
                                                            (internalApiError
                                                                "could not create the session")
                                                    Right handle ->
                                                        Right
                                                            (sessionValue
                                                                False
                                                                handle.sessionMeta)

getSessionForBoundary
    :: RuntimeEnvironment
    -> AccessBoundary
    -> Text
    -> IO (Either ApiError Value)
getSessionForBoundary environment boundary sessionId =
    fmap
        (fmap
            (\(meta, archived) ->
                sessionValue archived meta)) $
        loadAuthorizedSession environment boundary sessionId

patchSessionForBoundary
    :: RuntimeEnvironment
    -> AccessBoundary
    -> Text
    -> PatchSessionRequest
    -> IO (Either ApiError Value)
patchSessionForBoundary environment boundary sessionId request =
    loadAuthorizedMeta environment boundary sessionId >>= \case
        Left err -> pure (Left err)
        Right _ -> do
            titleResult <- case request.patchSessionTitle of
                Nothing -> pure (Right ())
                Just title ->
                    fmap (fmap (const ())) $
                        renameSession pool root sessionId title
            case titleResult of
                Left err -> pure (Left (sessionOperationError err))
                Right () -> do
                    archiveResult <- case request.patchSessionArchived of
                        Nothing -> pure (Right ())
                        Just archived ->
                            setSessionArchived
                                pool
                                root
                                sessionId
                                archived
                    case archiveResult of
                        Left err ->
                            pure (Left (sessionOperationError err))
                        Right () ->
                            getSessionForBoundary
                                environment
                                boundary
                                sessionId
  where
    pool = trustedPool environment.environmentStore
    root = environment.environmentRoot

deleteSessionForBoundary
    :: RuntimeEnvironment
    -> AccessBoundary
    -> Text
    -> IO (Either ApiError ())
deleteSessionForBoundary environment boundary sessionId =
    loadAuthorizedMeta environment boundary sessionId >>= \case
        Left err -> pure (Left err)
        Right _ ->
            first sessionOperationError
                <$> deleteSession
                    (trustedPool environment.environmentStore)
                    environment.environmentRoot
                    sessionId

sessionHistoryForBoundary
    :: RuntimeEnvironment
    -> AccessBoundary
    -> Text
    -> Maybe Integer
    -> Int
    -> IO (Either ApiError Value)
sessionHistoryForBoundary environment boundary sessionId before limit =
    loadAuthorizedSession environment boundary sessionId >>= \case
        Left err -> pure (Left err)
        Right (meta, archived) ->
            case traverse integerToInt64 before of
                Left err -> pure (Left err)
                Right Nothing -> do
                    page <-
                        loadRecentSessionHistoryTurns
                            pool root sessionId limit
                    pure $
                        first sessionOperationError
                            (historyValue meta archived <$> page)
                Right (Just cursor) -> do
                    page <-
                        loadSessionHistoryTurnsBefore
                            pool root sessionId cursor limit
                    pure $
                        first sessionOperationError
                            (historyValue meta archived <$> page)
  where
    pool = trustedPool environment.environmentStore
    root = environment.environmentRoot

forkSessionForBoundary
    :: RuntimeEnvironment
    -> AccessBoundary
    -> Text
    -> ForkSessionRequest
    -> IO (Either ApiError Value)
forkSessionForBoundary environment boundary sessionId request =
    loadAuthorizedMeta environment boundary sessionId >>= \case
        Left err -> pure (Left err)
        Right sourceMeta ->
            resolveTenantWorkspacePath
                environment.environmentConfig
                boundary.accessTenantId
                (request.forkSessionCwd
                    <|> Just (unsafeToFilePath sourceMeta.metaCwd)) >>= \case
                    Left err -> pure (Left err)
                    Right cwd ->
                        case request.forkSessionThroughTurn of
                            Just _
                                | request.forkSessionCwd /= Nothing
                                    || request.forkSessionTitle /= Nothing ->
                                    pure $
                                        Left ApiError
                                            { apiErrorStatus = 422
                                            , apiErrorCode =
                                                "unsupported_fork_shape"
                                            , apiErrorMessage =
                                                "title and cwd cannot be changed when forking through a specific turn"
                                            , apiErrorDetails = Nothing
                                            }
                            Just through ->
                                forkThroughTurn through
                            Nothing ->
                                forkAllTurns cwd
  where
    pool = trustedPool environment.environmentStore
    root = environment.environmentRoot

    forkThroughTurn through =
        case integerToInt64 through of
            Left err -> pure (Left err)
            Right index ->
                forkSessionAtTurn pool root sessionId index >>= \case
                    Left err -> pure (Left (sessionOperationError err))
                    Right forkedId ->
                        getSessionForBoundary
                            environment
                            boundary
                            forkedId

    forkAllTurns cwd =
        loadSessionHandle pool root sessionId >>= \case
            Left err -> pure (Left (sessionOperationError err))
            Right (handle, turns) ->
                case validateLoadedMeta boundary handle.sessionMeta of
                    Left err -> pure (Left err)
                    Right () ->
                        forkSessionAt
                            root
                            handle
                            turns
                            request.forkSessionTitle
                            (unsafeEncodeUtf cwd) >>= \case
                                Left err ->
                                    pure (Left (sessionOperationError err))
                                Right forked ->
                                    pure $
                                        Right
                                            (sessionValue
                                                False
                                                forked.sessionMeta)

runTurn
    :: RuntimeEnvironment
    -> TurnControl
    -> TurnSpec
    -> IO (Either Text TurnExecutionOutput)
runTurn environment control spec =
    loadAuthorizedMeta
        environment
        spec.turnSpecBoundary
        spec.turnSpecSessionId >>= \case
            Left err -> pure (Left err.apiErrorMessage)
            Right meta ->
                resolveTenantWorkspacePath
                    environment.environmentConfig
                    spec.turnSpecBoundary.accessTenantId
                    (Just (unsafeToFilePath meta.metaCwd)) >>= \case
                        Left err -> pure (Left err.apiErrorMessage)
                        Right cwd ->
                            case parseReasoningEffort meta.metaEffort of
                                Left err -> pure (Left err)
                                Right effort ->
                                    withFile "/dev/null" WriteMode \output -> do
                                        finalOutput <- newIORef Nothing
                                        let baseHooks =
                                                nativeHooks
                                                    environment
                                                    control
                                                    spec.turnSpecSessionId
                                                    cwd
                                                    meta.metaDialect
                                            hooks =
                                                baseHooks
                                                    { nativeOnLoopEvent = \event -> do
                                                        case event of
                                                            Loop.TurnFinished value ->
                                                                writeIORef
                                                                    finalOutput
                                                                    (Just value)
                                                            _ -> pure ()
                                                        baseHooks.nativeOnLoopEvent event
                                                    }
                                        runNativeTurn
                                            environment.environmentNative
                                            output
                                            hooks
                                            NativeTurnRequest
                                                { nativeTurnPrompt =
                                                    spec.turnSpecPrompt
                                                , nativeTurnSession =
                                                    NativeResumeSession
                                                        spec.turnSpecSessionId
                                                , nativeTurnProvider = Nothing
                                                , nativeTurnModel = Nothing
                                                , nativeTurnCwd =
                                                    unsafeEncodeUtf cwd
                                                , nativeTurnEffort =
                                                    Just effort
                                                , nativeTurnInteractionMode =
                                                    NativeAsk
                                                , nativeTurnShellMode =
                                                    tenantShellMode environment
                                                }
                                            >>= \case
                                                Left err -> pure (Left err)
                                                Right () ->
                                                    readIORef finalOutput
                                                        >>= pure
                                                            . maybe
                                                                ( Left
                                                                    "agent turn completed without a terminal output"
                                                                )
                                                                ( Right
                                                                    . turnExecutionOutput
                                                                )

turnExecutionOutput :: Loop.TurnOutput -> TurnExecutionOutput
turnExecutionOutput output =
    let assistantText = boundedPublicText <$> output.assistantText
     in TurnExecutionOutput
            { turnExecutionResponseId = output.responseId
            , turnExecutionAssistantText = fst <$> assistantText
            , turnExecutionAssistantTextTruncated =
                maybe False snd assistantText
            , turnExecutionCompletion =
                case output.completion of
                    Loop.TurnCompleted -> TurnCompletionComplete
                    Loop.TurnIncomplete reason reasoningTokens ->
                        TurnCompletionIncomplete
                            (fst (boundedPublicText reason))
                            reasoningTokens
            }

nativeHooks
    :: RuntimeEnvironment
    -> TurnControl
    -> Text
    -> FilePath
    -> DialectId
    -> NativeRunHooks
nativeHooks environment control sessionId cwd dialect = NativeRunHooks
    { nativeOnLoopEvent = \event ->
        let (eventType, value) = projectLoopEvent event
        in control.turnControlEmit eventType value
    , nativeOnSessionId = \_ -> pure ()
    , nativeRegisterCancel = control.turnControlRegisterCancel
    , nativeRegisterAgentSnapshot = \snapshot ->
        control.turnControlSetAgents
            (projectAgentEntries <$> snapshot)
    , nativeRequestApproval = requestToolApproval control
    , nativeRequestRootAccess =
        case environment.environmentSandbox of
            Just _ -> const (pure False)
            Nothing ->
                requestRootAccess
                    environment.environmentConfig
                    environment.environmentTenantId
                    control
    , nativeToolGroups = []
    , nativeComposeTools =
        case environment.environmentSandbox of
            Nothing -> appToolsFromGroups
            Just sandbox ->
                composeSandboxTools sandbox sessionId cwd dialect
    , nativePlanHooks = planHooks control
    , nativeInteractionMode = NativeAsk
    , nativeShellMode = tenantShellMode environment
    , nativeHome =
        case environment.environmentSandbox of
            Nothing -> Just (unsafeEncodeUtf environment.environmentHome)
            Just _ -> Nothing
    , nativeDatabaseStore = Just environment.environmentStore
    , nativeDatabaseScopeNamespace =
        renderTenantId environment.environmentTenantId
            <$ environment.environmentSandbox
    , nativeWorkspaceDiscovery =
        case environment.environmentSandbox of
            Nothing -> DiscoverHostWorkspace
            Just _ ->
                UsePreparedWorkspace NativeDiscoveryContext
                    { nativeDiscoveryHome =
                        unsafeEncodeUtf environment.environmentHome
                    , nativeDiscoveryProjectRoot =
                        unsafeEncodeUtf environment.environmentHome
                    , nativeDiscoveryCatalogRoot =
                        unsafeEncodeUtf environment.environmentHome
                    , nativeDiscoveryProjectSettings =
                        defaultProjectSettings
                    , nativeDiscoveryGitBranch = ""
                    , nativeDiscoveryOperatingSystem = "Linux"
                    , nativeDiscoveryShell = "/bin/bash"
                    }
    , nativeCapabilities =
        case environment.environmentSandbox of
            Nothing -> fullNativeRunCapabilities
            Just _ -> NativeRunCapabilities
                { nativeProviderFallback = False
                , nativeProviderHostedTools = False
                , nativeHostExtensions = False
                , nativeCollaboration = False
                , nativeProviderNativeTools = False
                }
    , nativePrepareOptions =
        case environment.environmentSandbox of
            Nothing -> Right
            Just _ -> Right . restrictSandboxOptions cwd
    }

restrictSandboxOptions :: FilePath -> CliOptions -> CliOptions
restrictSandboxOptions cwd options =
    options
        { optCwd = Just (unsafeEncodeUtf cwd)
        , optWorktree = False
        , optYolo = False
        , optNoYolo = True
        , optPromptFile = Nothing
        , optManagedTurnFile = Nothing
        , optAgentsMd = False
        , optSkills = False
        , optComputerUse = False
        , optCodeMode = False
        }

tenantShellMode :: RuntimeEnvironment -> NativeShellMode
tenantShellMode environment =
    case environment.environmentSandbox of
        Nothing -> NativeShellBoth
        Just _ -> NativeShellBash

requestToolApproval
    :: TurnControl
    -> ToolCall
    -> IO (Maybe PermissionChoice)
requestToolApproval control call =
    control.turnControlRequestInput HumanRequestSpec
        { humanRequestSpecKind = ToolApprovalRequest
        , humanRequestSpecPrompt =
            "Allow mutating tool "
                <> fst (boundedPublicText call.name)
                <> " (call "
                <> fst (boundedPublicText call.callId)
                <> ")?\nArguments: "
                <> if call.argumentsEncrypted
                    then "<encrypted>"
                    else fst (boundedPublicText call.arguments)
        , humanRequestSpecOptions =
            [ "allow_once"
            , "allow_tool"
            , "deny"
            ]
        } >>= \case
            Left _ -> pure Nothing
            Right response ->
                pure case response.humanResponseDecision of
                    "allow_once" -> Just PermissionAllowOnce
                    "allow_tool" -> Just PermissionAllowTool
                    "deny" -> Just PermissionDeny
                    _ -> Nothing

requestRootAccess
    :: ResolvedServerConfig
    -> TenantId
    -> TurnControl
    -> OsPath
    -> IO Bool
requestRootAccess config tenantId control root =
    resolveTenantWorkspacePath
        config
        tenantId
        (Just (unsafeToFilePath root)) >>= \case
        Left _ -> pure False
        Right canonical ->
            control.turnControlRequestInput HumanRequestSpec
                { humanRequestSpecKind = RootAccessRequest
                , humanRequestSpecPrompt =
                    "Allow filesystem access to " <> Text.pack canonical
                , humanRequestSpecOptions = ["allow", "deny"]
                } >>= \case
                    Right response ->
                        pure (response.humanResponseDecision == "allow")
                    Left _ -> pure False

planHooks :: TurnControl -> PlanModeHooks
planHooks control = PlanModeHooks
    { planConfirmEnter = \reason ->
        control.turnControlRequestInput HumanRequestSpec
            { humanRequestSpecKind = PlanEnterRequest
            , humanRequestSpecPrompt = reason
            , humanRequestSpecOptions = ["enter", "stay"]
            } >>= \case
                Right response ->
                    pure (response.humanResponseDecision == "enter")
                Left _ -> pure False
    , planDecideExit = \planBody ->
        control.turnControlRequestInput HumanRequestSpec
            { humanRequestSpecKind = PlanExitRequest
            , humanRequestSpecPrompt =
                fst (boundedPublicText planBody)
            , humanRequestSpecOptions =
                ["approve", "request_changes", "cancel"]
            } >>= \case
                Right response ->
                    pure case response.humanResponseDecision of
                        "approve" -> PlanApprove
                        "request_changes" ->
                            PlanRequestChanges
                                (fromMaybe
                                    "(no changes supplied)"
                                    response.humanResponseValue)
                        _ -> PlanCancel
                Left _ -> pure PlanCancel
    , planAskQuestion = \question options ->
        control.turnControlRequestInput HumanRequestSpec
            { humanRequestSpecKind = PlanQuestionRequest
            , humanRequestSpecPrompt = question
            , humanRequestSpecOptions = options <> ["custom"]
            } >>= \case
                Left _ -> pure Nothing
                Right response
                    | response.humanResponseDecision == "custom" ->
                        pure response.humanResponseValue
                    | otherwise ->
                        pure (Just response.humanResponseDecision)
    }

loadAuthorizedMeta
    :: RuntimeEnvironment
    -> AccessBoundary
    -> Text
    -> IO (Either ApiError SessionMeta)
loadAuthorizedMeta environment boundary sessionId =
    fmap (fmap fst) $
        loadAuthorizedSession environment boundary sessionId

loadAuthorizedSession
    :: RuntimeEnvironment
    -> AccessBoundary
    -> Text
    -> IO (Either ApiError (SessionMeta, Bool))
loadAuthorizedSession environment boundary sessionId
    | not (isValidSessionId sessionId) =
        pure (Left notFoundApiError)
    | otherwise =
        StoreSession.loadSessionMetadataForBoundary
            (trustedPool environment.environmentStore)
            organizationGatewayConnectionId
            boundary.accessGatewayBoundary.gatewayBoundaryIdentity
            sessionId >>= \case
                Left err -> pure (Left (storeApiError err))
                Right Nothing -> pure (Left notFoundApiError)
                Right (Just entry) ->
                    pure $
                        (, entry.sessionListEntryArchived)
                            <$> first
                                (internalApiError
                                    . ("could not decode session metadata: " <>))
                                (fromStoredMetadata
                                    entry.sessionListEntryMetadata)

validateLoadedMeta
    :: AccessBoundary
    -> SessionMeta
    -> Either ApiError ()
validateLoadedMeta boundary meta
    | case boundary.accessGatewayBoundary.gatewayBoundaryIdentity of
            Nothing ->
                meta.metaConnection /= organizationGatewayConnectionId
            Just _ ->
                meta.metaConnection == organizationGatewayConnectionId =
            Right ()
    | otherwise = Left notFoundApiError

loadModelOptions
    :: RuntimeEnvironment
    -> AccessBoundary
    -> FilePath
    -> IO (Either ApiError (ModelCatalog, [ModelOption]))
loadModelOptions environment expected cwd = do
    let home = unsafeEncodeUtf environment.environmentHome
        cwdPath = unsafeEncodeUtf cwd
        catalogRoot =
            case environment.environmentSandbox of
                Nothing -> cwdPath
                Just _ -> home
    loadGatewayBoundarySnapshotAt home >>= \case
        Left err -> pure (Left (gatewayApiError err))
        Right snapshot ->
            case
                validateGatewayBoundary
                    expected.accessGatewayBoundary
                    snapshot.gatewayBoundary
            of
                Left err -> pure (Left (gatewayApiError err))
                Right () ->
                    loadGatewayModelOptionsWithCredentialAt
                        home
                        catalogRoot
                        snapshot.gatewayBoundaryCredential >>= \case
                            Left err ->
                                pure
                                    (Left ApiError
                                        { apiErrorStatus = 503
                                        , apiErrorCode =
                                            "model_catalog_unavailable"
                                        , apiErrorMessage = err
                                        , apiErrorDetails = Nothing
                                        })
                            Right (catalog, maybeGatewayOptions) ->
                                let options =
                                        fromMaybe
                                            (modelCatalog catalog)
                                            maybeGatewayOptions
                                in pure $
                                    Right
                                        ( catalog
                                        , case environment.environmentSandbox of
                                            Nothing -> options
                                            Just _ ->
                                                filter
                                                    ((/= ClaudeCodeProvider)
                                                        . (.modelTarget.targetProvider))
                                                    options
                                        )

selectModel
    :: AccessBoundary
    -> ModelCatalog
    -> [ModelOption]
    -> Maybe Text
    -> Either ApiError ModelOption
selectModel boundary catalog options requested =
    case requested of
        Just modelId ->
            maybe
                (Left ApiError
                    { apiErrorStatus = 422
                    , apiErrorCode = "model_not_available"
                    , apiErrorMessage =
                        "the requested model is not available in the current gateway boundary"
                    , apiErrorDetails = Nothing
                    })
                Right
                (find
                    ((== modelId) . (.modelTarget.targetModelId))
                    options)
        Nothing ->
            maybe
                (Left ApiError
                    { apiErrorStatus = 503
                    , apiErrorCode = "model_catalog_empty"
                    , apiErrorMessage =
                        "no model is available in the current gateway boundary"
                    , apiErrorDetails = Nothing
                    })
                Right
                defaultOption
  where
    defaultOption =
        case boundary.accessGatewayBoundary.gatewayBoundaryIdentity of
        Just _ -> listToMaybe options
        Nothing ->
            let preferred = defaultModelOptionFor catalog OpenAIProvider
            in find ((== preferred.modelTarget) . (.modelTarget)) options
                <|> listToMaybe options

resolveEffort
    :: Provider
    -> Maybe Text
    -> Either ApiError ReasoningEffort
resolveEffort provider requested =
    case requested of
        Nothing -> Right (defaultEffortFor provider)
        Just effort ->
            first
                (\message ->
                    ApiError
                        { apiErrorStatus = 422
                        , apiErrorCode = "invalid_effort"
                        , apiErrorMessage = message
                        , apiErrorDetails = Nothing
                        })
                (parseReasoningEffort effort)

sessionValue :: Bool -> SessionMeta -> Value
sessionValue archived meta = object
    [ "id" .= meta.metaId
    , "createdAt" .= meta.metaCreatedAt
    , "updatedAt" .= meta.metaUpdatedAt
    , "provider" .= providerSlug meta.metaProvider
    , "connection" .= meta.metaConnection
    , "model" .= meta.metaModel
    , "transportModel" .= meta.metaTransportModel
    , "dialect" .= dialectSlug meta.metaDialect
    , "cwd" .= unsafeToFilePath meta.metaCwd
    , "effort" .= meta.metaEffort
    , "title" .= meta.metaTitle
    , "titleIsManual" .= meta.metaTitleIsManual
    , "archived" .= archived
    , "usage" .= object
        [ "input" .= meta.metaInputTokens
        , "output" .= meta.metaOutputTokens
        , "cached" .= meta.metaCachedTokens
        ]
    ]

modelOptionValue :: ModelOption -> Value
modelOptionValue option = object
    [ "id" .= option.modelTarget.targetModelId
    , "provider" .= providerSlug option.modelTarget.targetProvider
    , "connection" .= option.modelTarget.targetConnectionId
    , "transportModel" .= option.modelTarget.targetWireModelId
    , "dialect" .= dialectSlug option.modelTarget.targetDialect
    , "label" .= option.modelLabel
    , "contextWindow" .= option.modelContextWindow
    ]

historyValue :: SessionMeta -> Bool -> SessionTurnPage -> Value
historyValue meta archived page = object
    [ "session" .= sessionValue archived meta
    , "data" .=
        [ object
            [ "index" .= index
            , "turn" .= projectPublicValue (toJSON turn)
            ]
        | (index, turn) <- page.pageTurns
        ]
    , "generationStart" .= page.pageGenerationStart
    , "total" .= page.pageTotalTurns
    , "hasOlder" .= page.pageHasOlder
    , "hasNewer" .= page.pageHasNewer
    , "nextCursor" .=
        if page.pageHasOlder
            then fst <$> listToMaybe page.pageTurns
            else Nothing
    ]

storeArchiveFilter
    :: SessionArchiveFilter
    -> StoreSession.SessionArchiveFilter
storeArchiveFilter = \case
    ActiveSessions -> StoreSession.SessionActive
    ArchivedSessions -> StoreSession.SessionArchived
    AllSessions -> StoreSession.SessionAll

encodeCursor :: StoreSession.SessionListCursor -> Text
encodeCursor cursor =
    Text.pack
        (formatTime
            defaultTimeLocale
            cursorTimestampFormat
            cursor.sessionListCursorUpdatedAt)
        <> "|"
        <> cursor.sessionListCursorKey

decodeCursor
    :: Text
    -> Either ApiError StoreSession.SessionListCursor
decodeCursor raw =
    let (timestamp, separatorAndKey) = Text.breakOn "|" raw
        key = Text.drop 1 separatorAndKey
        parsed =
            parseTimeM
                True
                defaultTimeLocale
                cursorTimestampFormat
                (Text.unpack timestamp)
                :: Maybe UTCTime
    in case parsed of
        Just updatedAt
            | not (Text.null separatorAndKey)
            , not (Text.null key) ->
                Right StoreSession.SessionListCursor
                    { sessionListCursorUpdatedAt = updatedAt
                    , sessionListCursorKey = key
                    }
        _ ->
            Left ApiError
                { apiErrorStatus = 400
                , apiErrorCode = "invalid_cursor"
                , apiErrorMessage = "the session cursor is invalid"
                , apiErrorDetails = Nothing
                }

cursorTimestampFormat :: String
cursorTimestampFormat = "%Y-%m-%dT%H:%M:%S%QZ"

integerToInt64 :: Integer -> Either ApiError Int64
integerToInt64 value
    | value < 0
        || value > toInteger (maxBound :: Int64) =
        Left ApiError
            { apiErrorStatus = 400
            , apiErrorCode = "invalid_turn_cursor"
            , apiErrorMessage =
                "the turn cursor must be a non-negative 64-bit integer"
            , apiErrorDetails = Nothing
            }
    | otherwise = Right (fromInteger value)

storeApiError :: StoreError -> ApiError
storeApiError err =
    ApiError
        { apiErrorStatus = 503
        , apiErrorCode = "store_unavailable"
        , apiErrorMessage = renderStoreError err
        , apiErrorDetails = Nothing
        }

sessionOperationError :: Text -> ApiError
sessionOperationError message
    | "not found" `Text.isInfixOf` Text.toLower message =
        notFoundApiError
    | "running session" `Text.isInfixOf` Text.toLower message =
        ApiError
            { apiErrorStatus = 409
            , apiErrorCode = "session_busy"
            , apiErrorMessage = message
            , apiErrorDetails = Nothing
            }
    | otherwise =
        ApiError
            { apiErrorStatus = 422
            , apiErrorCode = "session_operation_failed"
            , apiErrorMessage = message
            , apiErrorDetails = Nothing
            }

notFoundApiError :: ApiError
notFoundApiError = ApiError
    { apiErrorStatus = 404
    , apiErrorCode = "session_not_found"
    , apiErrorMessage = "session not found"
    , apiErrorDetails = Nothing
    }

internalApiError :: Text -> ApiError
internalApiError message = ApiError
    { apiErrorStatus = 500
    , apiErrorCode = "internal_error"
    , apiErrorMessage = message
    , apiErrorDetails = Nothing
    }
