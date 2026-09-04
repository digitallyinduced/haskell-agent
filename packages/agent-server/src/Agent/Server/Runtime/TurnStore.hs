-- | Durable externally submitted turn storage for the production server.
module Agent.Server.Runtime.TurnStore
    ( TurnStoreBackend (..)
    , TurnStoreOwner
    , openTurnStoreOwner
    , closeTurnStoreOwner
    , withTurnStoreOwnerFence
    , newTurnStoreBackend
    )
where

import Agent.Server.Backend (SessionMutationLease(..))
import Agent.Server.Event (boundedPublicText)
import Agent.Server.Supervisor
    ( HumanRequestPersistenceResolution (..)
    , TurnPersistence (..)
    )
import Agent.Server.Types
import Agent.Store.Postgres (Store, trustedPool)
import Agent.Store.Postgres.ServerHumanRequest qualified as HumanRequestStore
import Agent.Store.Postgres.ServerTurn qualified as ServerTurnStore
import Agent.Store.Types (StoreError (..), renderStoreError)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
    ( Async
    , async
    , mapConcurrently_
    , race
    , waitCatch
    , withAsync
    )
import Control.Concurrent.STM
    ( TVar
    , atomically
    , check
    , modifyTVar'
    , newTVarIO
    , readTVar
    , readTVarIO
    , writeTVar
    )
import Control.Exception.Safe (finally, mask, onException, tryAny)
import Control.Monad (void)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Time.Clock
    ( UTCTime
    , getCurrentTime
    )
import System.Timeout (timeout)

data TurnStoreOwner = TurnStoreOwner
    { turnStoreOwnerStore :: !Store
    , turnStoreOwnerInstanceId :: !Text
    , turnStoreOwnerHealthy :: !(TVar Bool)
    , turnStoreOwnerLease :: !ServerTurnStore.ServerTurnOwnerLease
    , turnStoreOwnerStopping :: !(TVar Bool)
    , turnStoreOwnerLifecycle :: !(Async ())
    , turnStoreOwnerPendingMutationReleases ::
        !(TVar (Map MutationReleaseKey ServerTurnStore.ServerSessionMutation))
    }

type MutationReleaseKey = (Text, Maybe Text, Text)

openTurnStoreOwner :: Store -> Text -> IO (Either Text TurnStoreOwner)
openTurnStoreOwner store instanceId = mask \restore -> do
    leaseResult <-
        restore $
            ServerTurnStore.openServerTurnOwnerLease
                (trustedPool store)
                instanceId
    case leaseResult of
        Left err -> pure (Left (renderStoreError err))
        Right lease -> do
            healthy <- newTVarIO True
            stopping <- newTVarIO False
            pendingMutationReleases <- newTVarIO Map.empty
            lifecycle <-
                async
                    ( restore
                        ( ownerLifecycle
                            store
                            lease
                            healthy
                            stopping
                            pendingMutationReleases
                        )
                    )
                    `onException` ServerTurnStore.releaseServerTurnOwner lease
            pure $
                Right
                    TurnStoreOwner
                        { turnStoreOwnerStore = store
                        , turnStoreOwnerInstanceId = instanceId
                        , turnStoreOwnerHealthy = healthy
                        , turnStoreOwnerLease = lease
                        , turnStoreOwnerStopping = stopping
                        , turnStoreOwnerLifecycle = lifecycle
                        , turnStoreOwnerPendingMutationReleases =
                            pendingMutationReleases
                        }

closeTurnStoreOwner :: TurnStoreOwner -> IO ()
closeTurnStoreOwner owner = do
    -- The lifecycle owns heartbeat cancellation and lease retirement. Once
    -- this atomic signal commits, an exception in the caller cannot strand a
    -- live heartbeat between those cleanup steps.
    atomically do
        writeTVar owner.turnStoreOwnerHealthy False
        writeTVar owner.turnStoreOwnerStopping True
    void (waitCatch owner.turnStoreOwnerLifecycle)

ownerLifecycle ::
    Store ->
    ServerTurnStore.ServerTurnOwnerLease ->
    TVar Bool ->
    TVar Bool ->
    TVar (Map MutationReleaseKey ServerTurnStore.ServerSessionMutation) ->
    IO ()
ownerLifecycle store lease healthy stopping pendingMutationReleases =
    ( withAsync
        ( ownerHeartbeatLoop
            store
            lease
            healthy
            pendingMutationReleases
        )
        \heartbeat ->
            void $
                race
                    ( atomically do
                        shouldStop <- readTVar stopping
                        check shouldStop
                    )
                    (void (waitCatch heartbeat))
    )
        `finally` do
            atomically (writeTVar healthy False)
            retireTurnStoreOwnerLease lease

retireTurnStoreOwnerLease ::
    ServerTurnStore.ServerTurnOwnerLease ->
    IO ()
retireTurnStoreOwnerLease lease =
    ( void $
            timeout ownerHeartbeatTimeoutMicroseconds $
                ServerTurnStore.releaseServerTurnOwner
                    lease
        )
        `finally` ServerTurnStore.abandonServerTurnOwnerLease
            lease

withTurnStoreOwnerFence ::
    TurnStoreOwner ->
    IO value ->
    IO (Either Text value)
withTurnStoreOwnerFence owner action =
    first (.apiErrorMessage)
        <$> withTurnStoreOwnerActionFence owner action

ownerHeartbeatLoop ::
    Store ->
    ServerTurnStore.ServerTurnOwnerLease ->
    TVar Bool ->
    TVar (Map MutationReleaseKey ServerTurnStore.ServerSessionMutation) ->
    IO ()
ownerHeartbeatLoop store lease healthy pendingMutationReleases = do
    threadDelay ownerHeartbeatIntervalMicroseconds
    result <-
        tryAny do
            heartbeat <- heartbeatOwnerWithinDeadline lease
            case heartbeat of
                Left err -> pure (Left err)
                Right () -> do
                    retryPendingMutationReleases
                        store
                        pendingMutationReleases
                    pure (Right ())
    case result of
        Right (Right ()) ->
            ownerHeartbeatLoop store lease healthy pendingMutationReleases
        _ -> do
            -- Losing the connection-lifetime fence is irreversible for this
            -- process identity. Never try to revive it after another server
            -- may have recovered its turns.
            atomically (writeTVar healthy False)
            ServerTurnStore.abandonServerTurnOwnerLease lease

heartbeatOwnerWithinDeadline ::
    ServerTurnStore.ServerTurnOwnerLease ->
    IO (Either StoreError ())
heartbeatOwnerWithinDeadline lease =
    timeout
        ownerHeartbeatTimeoutMicroseconds
        (ServerTurnStore.heartbeatServerTurnOwner lease)
        >>= \case
            Nothing ->
                pure
                    ( Left
                        ( StoreConnectionError
                            "server turn owner heartbeat timed out"
                        )
                    )
            Just result -> pure result

ownerHeartbeatIntervalMicroseconds :: Int
ownerHeartbeatIntervalMicroseconds = 5 * 1000 * 1000

ownerHeartbeatTimeoutMicroseconds :: Int
ownerHeartbeatTimeoutMicroseconds = 5 * 1000 * 1000

data TurnStoreBackend = TurnStoreBackend
    { turnStoreReserve ::
        !( AccessBoundary ->
           Text ->
           ClientRequestId ->
           Text ->
           TurnId ->
           UTCTime ->
           IO (Either ApiError TurnReservation)
         )
    , turnStoreLookup ::
        !( AccessBoundary ->
           TurnId ->
           IO (Either ApiError (Maybe TurnRecord))
         )
    , turnStoreList ::
        !( AccessBoundary ->
           Maybe Text ->
           IO (Either ApiError [TurnRecord])
         )
    , turnStoreLookupResult ::
        !( AccessBoundary ->
           TurnId ->
           IO (Either ApiError (Maybe TurnResult))
         )
    , turnStoreReserveSessionMutation ::
        !( AccessBoundary ->
           Text ->
           UTCTime ->
           IO (Either ApiError (Maybe SessionMutationLease))
         )
    , turnStoreRequestCancellation ::
        !( AccessBoundary ->
           TurnId ->
           UTCTime ->
           IO (Either ApiError (Maybe (Bool, TurnRecord)))
         )
    , turnStorePersistence :: !TurnPersistence
    }

newTurnStoreBackend :: Store -> TenantId -> TurnStoreOwner -> TurnStoreBackend
newTurnStoreBackend store tenantId owner =
    TurnStoreBackend
        { turnStoreReserve = \boundary sessionId clientRequestId prompt turnId now ->
            let reserve =
                    reserveTurn
                        store
                        tenantId
                        instanceId
                        boundary
                        sessionId
                        clientRequestId
                        prompt
                        turnId
                        now
             in if boundary.accessTenantId /= tenantId
                    then reserve
                    else
                        readTVarIO owner.turnStoreOwnerHealthy >>= \case
                            True -> reserve
                            False -> pure (Left unhealthyOwnerApiError)
        , turnStoreLookup =
            lookupStoredTurn store tenantId instanceId
        , turnStoreList =
            listStoredTurns store tenantId instanceId
        , turnStoreLookupResult =
            lookupStoredTurnResult store tenantId instanceId
        , turnStoreReserveSessionMutation =
            reserveSessionMutation store tenantId owner
        , turnStoreRequestCancellation =
            requestTurnCancellation store tenantId instanceId
        , turnStorePersistence =
            productionTurnPersistence store owner
        }
  where
    instanceId = owner.turnStoreOwnerInstanceId

productionTurnPersistence :: Store -> TurnStoreOwner -> TurnPersistence
productionTurnPersistence store owner =
    TurnPersistence
        { turnPersistenceStarted = \record startedAt ->
            first renderStoreError
                <$> markStoredTurnRunning
                    store
                    instanceId
                    record
                    startedAt
        , turnPersistenceTerminal = \record finishedAt outcome ->
            first renderStoreError
                <$> finishStoredTurn
                    store
                    instanceId
                    record
                    finishedAt
                    outcome
        , turnPersistenceShouldCancel = \record ->
            readTVarIO owner.turnStoreOwnerHealthy >>= \case
                False -> pure (Right True)
                True ->
                    first renderStoreError
                        <$> ServerTurnStore.shouldCancelServerTurn
                            (trustedPool store)
                            (storeBoundary record.turnRecordBoundary)
                            instanceId
                            record.turnRecordId.unTurnId
        , turnPersistenceCreateHumanRequest = \record request ->
            first renderStoreError
                <$> createStoredHumanRequest
                    store
                    instanceId
                    record
                    request
        , turnPersistenceListHumanRequests = \boundary turnId ->
            first renderStoreError
                <$> listStoredHumanRequests store boundary turnId
        , turnPersistenceResolveHumanRequest =
            resolveStoredHumanRequest store
        , turnPersistenceLoadHumanResponse = \record requestId ->
            first renderStoreError
                <$> loadStoredHumanResponse
                    store
                    instanceId
                    record
                    requestId
        , turnPersistenceDeleteHumanRequest = \record requestId ->
            first renderStoreError
                <$> HumanRequestStore.deleteServerHumanRequest
                    (trustedPool store)
                    (storeBoundary record.turnRecordBoundary)
                    instanceId
                    record.turnRecordId.unTurnId
                    requestId.unRequestId
        }
  where
    instanceId = owner.turnStoreOwnerInstanceId

createStoredHumanRequest ::
    Store ->
    Text ->
    TurnRecord ->
    HumanRequest ->
    IO (Either StoreError ())
createStoredHumanRequest store instanceId record request =
    fmap (>>= requireStoreTransition "human request creation") $
        HumanRequestStore.createServerHumanRequest
            (trustedPool store)
            HumanRequestStore.CreateServerHumanRequest
                { HumanRequestStore.createServerHumanRequestId =
                    request.humanRequestId.unRequestId
                , HumanRequestStore.createServerHumanRequestTurnId =
                    record.turnRecordId.unTurnId
                , HumanRequestStore.createServerHumanRequestBoundary =
                    storeBoundary record.turnRecordBoundary
                , HumanRequestStore.createServerHumanRequestOwnerInstanceId =
                    instanceId
                , HumanRequestStore.createServerHumanRequestKind =
                    humanRequestKindText request.humanRequestKind
                , HumanRequestStore.createServerHumanRequestPrompt =
                    request.humanRequestPrompt
                , HumanRequestStore.createServerHumanRequestOptionsJson =
                    encodeHumanRequestOptions request.humanRequestOptions
                , HumanRequestStore.createServerHumanRequestCreatedAt =
                    request.humanRequestCreatedAt
                }

listStoredHumanRequests ::
    Store ->
    AccessBoundary ->
    Maybe TurnId ->
    IO (Either StoreError [HumanRequest])
listStoredHumanRequests store boundary turnId = do
    result <-
        case turnId of
            Nothing ->
                HumanRequestStore.listServerHumanRequests
                    (trustedPool store)
                    (storeBoundary boundary)
            Just expectedTurnId ->
                HumanRequestStore.listServerHumanRequestsForTurn
                    (trustedPool store)
                    (storeBoundary boundary)
                    expectedTurnId.unTurnId
    pure $
        result
            >>= traverse (storedHumanRequest boundary)

resolveStoredHumanRequest ::
    Store ->
    AccessBoundary ->
    RequestId ->
    HumanResponse ->
    IO (Either Text HumanRequestPersistenceResolution)
resolveStoredHumanRequest store boundary requestId response = do
    now <- getCurrentTime
    result <-
        HumanRequestStore.resolveServerHumanRequest
            (trustedPool store)
            (storeBoundary boundary)
            requestId.unRequestId
            HumanRequestStore.ServerHumanResponse
                { HumanRequestStore.serverHumanResponseDecision =
                    response.humanResponseDecision
                , HumanRequestStore.serverHumanResponseValue =
                    response.humanResponseValue
                }
            now
    pure case result of
        Left err -> Left (renderStoreError err)
        Right HumanRequestStore.ServerHumanRequestNotFound ->
            Right HumanRequestNotFoundDurably
        Right HumanRequestStore.ServerHumanRequestAlreadyResolved ->
            Left "request has already been resolved"
        Right HumanRequestStore.ServerHumanRequestInvalidDecision ->
            Left "decision is not one of the allowed options"
        Right (HumanRequestStore.ServerHumanRequestResolved request) ->
            first renderStoreError $
                HumanRequestResolvedDurably
                    <$> storedHumanRequest boundary request

loadStoredHumanResponse ::
    Store ->
    Text ->
    TurnRecord ->
    RequestId ->
    IO (Either StoreError (Maybe HumanResponse))
loadStoredHumanResponse store instanceId record requestId =
    fmap (fmap (fmap storedHumanResponse)) $
        HumanRequestStore.loadServerHumanResponse
            (trustedPool store)
            (storeBoundary record.turnRecordBoundary)
            instanceId
            record.turnRecordId.unTurnId
            requestId.unRequestId

storedHumanRequest ::
    AccessBoundary ->
    HumanRequestStore.StoredServerHumanRequest ->
    Either StoreError HumanRequest
storedHumanRequest boundary stored = do
    kind <- decodeHumanRequestKind stored.storedServerHumanRequestKind
    options <-
        first
            (StoreDataError . Text.pack)
            ( Aeson.eitherDecodeStrict'
                ( TextEncoding.encodeUtf8
                    stored.storedServerHumanRequestOptionsJson
                )
            )
    pure
        HumanRequest
            { humanRequestId =
                RequestId stored.storedServerHumanRequestId
            , humanRequestTurnId =
                TurnId stored.storedServerHumanRequestTurnId
            , humanRequestSessionId =
                stored.storedServerHumanRequestSessionId
            , humanRequestBoundary = boundary
            , humanRequestKind = kind
            , humanRequestPrompt =
                stored.storedServerHumanRequestPrompt
            , humanRequestOptions = options
            , humanRequestCreatedAt =
                stored.storedServerHumanRequestCreatedAt
            }

storedHumanResponse ::
    HumanRequestStore.ServerHumanResponse ->
    HumanResponse
storedHumanResponse response =
    HumanResponse
        { humanResponseDecision =
            response.serverHumanResponseDecision
        , humanResponseValue =
            response.serverHumanResponseValue
        }

decodeHumanRequestKind :: Text -> Either StoreError HumanRequestKind
decodeHumanRequestKind = \case
    "tool_approval" -> Right ToolApprovalRequest
    "root_access" -> Right RootAccessRequest
    "plan_enter" -> Right PlanEnterRequest
    "plan_exit" -> Right PlanExitRequest
    "plan_question" -> Right PlanQuestionRequest
    other ->
        Left
            ( StoreDataError
                ("invalid durable human request kind: " <> other)
            )

encodeHumanRequestOptions :: [Text] -> Text
encodeHumanRequestOptions =
    TextEncoding.decodeUtf8
        . LazyByteString.toStrict
        . Aeson.encode

reserveSessionMutation ::
    Store ->
    TenantId ->
    TurnStoreOwner ->
    AccessBoundary ->
    Text ->
    UTCTime ->
    IO (Either ApiError (Maybe SessionMutationLease))
reserveSessionMutation store tenantId owner boundary sessionId now
    | boundary.accessTenantId /= tenantId =
        pure (Left notFoundApiError)
    | otherwise = mask \restore -> do
        let mutation =
                sessionMutation
                    boundary
                    sessionId
                    owner.turnStoreOwnerInstanceId
                    now
        fenceResult <-
            restore $
                ServerTurnStore.openServerTurnOwnerActionFence
                    (trustedPool store)
                    owner.turnStoreOwnerInstanceId
        case fenceResult of
            Left err -> pure (Left (storeApiError err))
            Right Nothing -> pure (Left unhealthyOwnerApiError)
            Right (Just fence) -> do
                let closeFence =
                        ServerTurnStore.closeServerTurnOwnerActionFence fence
                    releaseReservation =
                        requestSessionMutationRelease
                            store
                            owner
                            mutation
                            `finally` closeFence
                reservation <-
                    restore
                        ( ServerTurnStore.reserveServerSessionMutation
                            (trustedPool store)
                            mutation
                        )
                        `onException` releaseReservation
                case reservation of
                    Left err -> do
                        releaseReservation
                        pure (Left (storeApiError err))
                    Right ServerTurnStore.ServerSessionMutationReserved ->
                        pure . Right . Just $
                            SessionMutationLease
                                { runSessionMutationLease =
                                    runWhileOwnerAndActionFenceHealthy
                                        owner
                                        fence
                                , releaseSessionMutationLease =
                                    releaseReservation
                                }
                    Right ServerTurnStore.ServerSessionMutationBusy -> do
                        closeFence
                        pure (Right Nothing)
                    Right
                        ServerTurnStore.ServerSessionMutationSessionMissing -> do
                            closeFence
                            pure (Left notFoundApiError)
                    Right
                        ServerTurnStore.ServerSessionMutationOwnerUnavailable -> do
                            closeFence
                            pure (Left unhealthyOwnerApiError)

withTurnStoreOwnerActionFence ::
    TurnStoreOwner ->
    IO value ->
    IO (Either ApiError value)
withTurnStoreOwnerActionFence owner action = mask \restore ->
    ServerTurnStore.openServerTurnOwnerActionFence
        (trustedPool owner.turnStoreOwnerStore)
        owner.turnStoreOwnerInstanceId
        >>= \case
            Left err -> pure (Left (storeApiError err))
            Right Nothing -> pure (Left unhealthyOwnerApiError)
            Right (Just fence) ->
                restore
                    ( runWhileOwnerAndActionFenceHealthy
                        owner
                        fence
                        action
                    )
                    `finally`
                        ServerTurnStore.closeServerTurnOwnerActionFence
                            fence

runWhileOwnerAndActionFenceHealthy ::
    TurnStoreOwner ->
    ServerTurnStore.ServerTurnOwnerActionFence ->
    IO value ->
    IO (Either ApiError value)
runWhileOwnerAndActionFenceHealthy owner fence action = do
    fenceReady <-
        timeout ownerHeartbeatTimeoutMicroseconds $
            ServerTurnStore.checkServerTurnOwnerActionFence fence
    case fenceReady of
        Just (Right ()) ->
            readTVarIO owner.turnStoreOwnerHealthy >>= \case
                False -> pure (Left unhealthyOwnerApiError)
                True ->
                    race waitUntilUnhealthy action >>= \case
                        Left () -> pure (Left unhealthyOwnerApiError)
                        Right value -> pure (Right value)
        _ -> pure (Left unhealthyOwnerApiError)
  where
    waitUntilUnhealthy =
        void $
            race
                ( atomically do
                    healthy <- readTVar owner.turnStoreOwnerHealthy
                    check (not healthy)
                )
                (waitUntilActionFenceFails fence)

waitUntilActionFenceFails ::
    ServerTurnStore.ServerTurnOwnerActionFence ->
    IO ()
waitUntilActionFenceFails fence = do
    threadDelay actionFenceCheckIntervalMicroseconds
    checked <-
        timeout ownerHeartbeatTimeoutMicroseconds $
            ServerTurnStore.checkServerTurnOwnerActionFence fence
    case checked of
        Just (Right ()) -> waitUntilActionFenceFails fence
        _ -> pure ()

actionFenceCheckIntervalMicroseconds :: Int
actionFenceCheckIntervalMicroseconds = 1000 * 1000

requestSessionMutationRelease ::
    Store ->
    TurnStoreOwner ->
    ServerTurnStore.ServerSessionMutation ->
    IO ()
requestSessionMutationRelease store owner mutation =
    mask \restore -> do
        atomically $
            modifyTVar'
                owner.turnStoreOwnerPendingMutationReleases
                (Map.insert (mutationReleaseKey mutation) mutation)
        restore $
            attemptPendingMutationRelease
                store
                owner.turnStoreOwnerPendingMutationReleases
                mutation

retryPendingMutationReleases ::
    Store ->
    TVar (Map MutationReleaseKey ServerTurnStore.ServerSessionMutation) ->
    IO ()
retryPendingMutationReleases store pending = do
    mutations <-
        take maximumMutationReleaseBatch
            . Map.elems
            <$> readTVarIO pending
    mapConcurrently_
        (attemptPendingMutationRelease store pending)
        mutations

attemptPendingMutationRelease ::
    Store ->
    TVar (Map MutationReleaseKey ServerTurnStore.ServerSessionMutation) ->
    ServerTurnStore.ServerSessionMutation ->
    IO ()
attemptPendingMutationRelease store pending mutation = do
    released <-
        tryAny $
            timeout ownerHeartbeatTimeoutMicroseconds $
                ServerTurnStore.releaseServerSessionMutation
                    (trustedPool store)
                    mutation
    case released of
        Right (Just (Right ())) ->
            atomically $
                modifyTVar'
                    pending
                    ( Map.update
                        ( \current ->
                            if current == mutation
                                then Nothing
                                else Just current
                        )
                        (mutationReleaseKey mutation)
                    )
        _ -> pure ()

mutationReleaseKey ::
    ServerTurnStore.ServerSessionMutation ->
    MutationReleaseKey
mutationReleaseKey mutation =
    ( boundary.serverTurnTenantId
    , boundary.serverTurnGatewayIdentity
    , mutation.serverSessionMutationSessionId
    )
  where
    boundary = mutation.serverSessionMutationBoundary

maximumMutationReleaseBatch :: Int
maximumMutationReleaseBatch = 4

requestTurnCancellation ::
    Store ->
    TenantId ->
    Text ->
    AccessBoundary ->
    TurnId ->
    UTCTime ->
    IO (Either ApiError (Maybe (Bool, TurnRecord)))
requestTurnCancellation store tenantId instanceId boundary turnId requestedAt
    | boundary.accessTenantId /= tenantId =
        pure (Right Nothing)
    | otherwise =
        fmap
            ( first storeApiError
                . fmap
                    ( fmap
                        ( \stored ->
                            ( stored.storedServerTurnOwnerInstanceId
                                == instanceId
                            , storedTurnRecord boundary stored
                            )
                        )
                    )
            )
            $ ServerTurnStore.requestServerTurnCancellation
                (trustedPool store)
                (storeBoundary boundary)
                turnId.unTurnId
                requestedAt

sessionMutation ::
    AccessBoundary ->
    Text ->
    Text ->
    UTCTime ->
    ServerTurnStore.ServerSessionMutation
sessionMutation boundary sessionId instanceId now =
    ServerTurnStore.ServerSessionMutation
        { ServerTurnStore.serverSessionMutationBoundary =
            storeBoundary boundary
        , ServerTurnStore.serverSessionMutationSessionId = sessionId
        , ServerTurnStore.serverSessionMutationOwnerInstanceId =
            instanceId
        , ServerTurnStore.serverSessionMutationCreatedAt = now
        }

reserveTurn ::
    Store ->
    TenantId ->
    Text ->
    AccessBoundary ->
    Text ->
    ClientRequestId ->
    Text ->
    TurnId ->
    UTCTime ->
    IO (Either ApiError TurnReservation)
reserveTurn store tenantId instanceId boundary sessionId clientRequestId prompt turnId now
    | boundary.accessTenantId /= tenantId =
        pure (Left notFoundApiError)
    | otherwise = do
        result <-
            ServerTurnStore.reserveServerTurn
                (trustedPool store)
                ServerTurnStore.ReserveServerTurn
                    { ServerTurnStore.reserveServerTurnId = turnId.unTurnId
                    , ServerTurnStore.reserveServerTurnBoundary =
                        storeBoundary boundary
                    , ServerTurnStore.reserveServerTurnSessionId = sessionId
                    , ServerTurnStore.reserveServerTurnClientRequestId =
                        clientRequestId.unClientRequestId
                    , ServerTurnStore.reserveServerTurnInputDigest =
                        turnInputDigest prompt
                    , ServerTurnStore.reserveServerTurnOwnerInstanceId =
                        instanceId
                    , ServerTurnStore.reserveServerTurnCreatedAt = now
                    }
        pure case result of
            Left err -> Left (storeApiError err)
            Right (ServerTurnStore.ServerTurnReserved stored) ->
                Right
                    ( TurnReservationCreated
                        (storedTurnRecord boundary stored)
                    )
            Right (ServerTurnStore.ServerTurnAlreadyReserved stored) ->
                Right
                    ( if stored.storedServerTurnOwnerInstanceId
                        == instanceId
                        then
                            TurnReservationExistingOwned
                                (storedTurnRecord boundary stored)
                        else
                            TurnReservationExisting
                                (storedTurnRecord boundary stored)
                    )
            Right (ServerTurnStore.ServerTurnSessionBusy _) ->
                Left
                    ApiError
                        { apiErrorStatus = 409
                        , apiErrorCode = "session_busy"
                        , apiErrorMessage =
                            "the session already has an active turn"
                        , apiErrorDetails = Nothing
                        }
            Right ServerTurnStore.ServerTurnSessionMutating ->
                Left
                    ApiError
                        { apiErrorStatus = 409
                        , apiErrorCode = "session_busy"
                        , apiErrorMessage =
                            "the session has an active mutation"
                        , apiErrorDetails = Nothing
                        }
            Right ServerTurnStore.ServerTurnOwnerUnavailable ->
                Left unhealthyOwnerApiError
            Right (ServerTurnStore.ServerTurnIdempotencyConflict _) ->
                Left
                    ApiError
                        { apiErrorStatus = 409
                        , apiErrorCode = "idempotency_conflict"
                        , apiErrorMessage =
                            "clientRequestId was already used with different input"
                        , apiErrorDetails = Nothing
                        }

lookupStoredTurn ::
    Store ->
    TenantId ->
    Text ->
    AccessBoundary ->
    TurnId ->
    IO (Either ApiError (Maybe TurnRecord))
lookupStoredTurn store tenantId _instanceId boundary turnId
    | boundary.accessTenantId /= tenantId =
        pure (Right Nothing)
    | otherwise =
        fmap
            ( first storeApiError
                . fmap (fmap (storedTurnRecord boundary))
            )
            $ ServerTurnStore.loadServerTurn
                (trustedPool store)
                (storeBoundary boundary)
                turnId.unTurnId

listStoredTurns ::
    Store ->
    TenantId ->
    Text ->
    AccessBoundary ->
    Maybe Text ->
    IO (Either ApiError [TurnRecord])
listStoredTurns store tenantId _instanceId boundary sessionId
    | boundary.accessTenantId /= tenantId =
        pure (Right [])
    | otherwise =
        fmap
            ( first storeApiError
                . fmap (map (storedTurnRecord boundary))
            )
            $ ServerTurnStore.listServerTurns
                (trustedPool store)
                (storeBoundary boundary)
                sessionId

lookupStoredTurnResult ::
    Store ->
    TenantId ->
    Text ->
    AccessBoundary ->
    TurnId ->
    IO (Either ApiError (Maybe TurnResult))
lookupStoredTurnResult store tenantId _instanceId boundary turnId
    | boundary.accessTenantId /= tenantId =
        pure (Right Nothing)
    | otherwise =
        fmap
            ( first storeApiError
                . fmap (fmap (storedTurnResult boundary))
            )
            $ ServerTurnStore.loadServerTurn
                (trustedPool store)
                (storeBoundary boundary)
                turnId.unTurnId

markStoredTurnRunning ::
    Store ->
    Text ->
    TurnRecord ->
    UTCTime ->
    IO (Either StoreError ())
markStoredTurnRunning store instanceId record startedAt =
    fmap (>>= requireStoreTransition "start") $
        ServerTurnStore.markServerTurnRunning
            (trustedPool store)
            (storeBoundary record.turnRecordBoundary)
            instanceId
            record.turnRecordId.unTurnId
            startedAt

finishStoredTurn ::
    Store ->
    Text ->
    TurnRecord ->
    UTCTime ->
    TurnTerminalOutcome ->
    IO (Either StoreError TurnRecord)
finishStoredTurn store instanceId record finishedAt outcome = do
    result <-
        ServerTurnStore.finishServerTurn
            (trustedPool store)
            (storeBoundary record.turnRecordBoundary)
            instanceId
            record.turnRecordId.unTurnId
            (storedTerminal finishedAt outcome)
    case result of
        Left err -> pure (Left err)
        Right (Just stored) ->
            pure
                (Right
                    (storedTurnRecord
                        record.turnRecordBoundary
                        stored))
        Right Nothing ->
            ServerTurnStore.loadServerTurn
                (trustedPool store)
                (storeBoundary record.turnRecordBoundary)
                record.turnRecordId.unTurnId
                >>= pure . \case
                    Left err -> Left err
                    Right (Just stored)
                        | not
                            ( isStoredTurnActive
                                stored.storedServerTurnStatus
                            ) ->
                            Right
                                (storedTurnRecord
                                    record.turnRecordBoundary
                                    stored)
                    _ ->
                        Left
                            ( StoreDataError
                                "server turn terminal transition was rejected"
                            )

storeBoundary :: AccessBoundary -> ServerTurnStore.ServerTurnBoundary
storeBoundary boundary =
    ServerTurnStore.ServerTurnBoundary
        { ServerTurnStore.serverTurnTenantId =
            renderTenantId boundary.accessTenantId
        , ServerTurnStore.serverTurnGatewayIdentity =
            boundary.accessGatewayBoundary.gatewayBoundaryIdentity
        }

storedTurnRecord ::
    AccessBoundary ->
    ServerTurnStore.StoredServerTurn ->
    TurnRecord
storedTurnRecord boundary stored =
    TurnRecord
        { turnRecordId = TurnId stored.storedServerTurnId
        , turnRecordSessionId = stored.storedServerTurnSessionId
        , turnRecordClientRequestId =
            ClientRequestId stored.storedServerTurnClientRequestId
        , turnRecordBoundary = boundary
        , turnRecordStatus =
            storedTurnStatus stored.storedServerTurnStatus
        , turnRecordCreatedAt = stored.storedServerTurnCreatedAt
        , turnRecordStartedAt = stored.storedServerTurnStartedAt
        , turnRecordFinishedAt = stored.storedServerTurnFinishedAt
        , turnRecordError = stored.storedServerTurnError
        }

storedTurnStatus :: ServerTurnStore.ServerTurnStatus -> TurnStatus
storedTurnStatus = \case
    ServerTurnStore.ServerTurnQueued -> TurnQueued
    ServerTurnStore.ServerTurnRunning -> TurnRunning
    ServerTurnStore.ServerTurnCompleted -> TurnCompleted
    ServerTurnStore.ServerTurnFailed -> TurnFailed
    ServerTurnStore.ServerTurnCancelled -> TurnCancelled

isStoredTurnActive :: ServerTurnStore.ServerTurnStatus -> Bool
isStoredTurnActive = \case
    ServerTurnStore.ServerTurnQueued -> True
    ServerTurnStore.ServerTurnRunning -> True
    ServerTurnStore.ServerTurnCompleted -> False
    ServerTurnStore.ServerTurnFailed -> False
    ServerTurnStore.ServerTurnCancelled -> False

storedTurnResult ::
    AccessBoundary ->
    ServerTurnStore.StoredServerTurn ->
    TurnResult
storedTurnResult boundary stored =
    TurnResult
        { turnResultTurn = storedTurnRecord boundary stored
        , turnResultOutput =
            if stored.storedServerTurnStatus
                == ServerTurnStore.ServerTurnCompleted
                then
                    TurnExecutionOutput
                        <$> stored.storedServerTurnResponseId
                        <*> pure stored.storedServerTurnAssistantText
                        <*> pure
                            stored.storedServerTurnAssistantTextTruncated
                        <*> pure
                            ( case stored.storedServerTurnIncompleteReason of
                                Nothing -> TurnCompletionComplete
                                Just reason ->
                                    TurnCompletionIncomplete
                                        reason
                                        ( fromIntegral
                                            <$> stored.storedServerTurnIncompleteReasoningTokens
                                        )
                            )
                else Nothing
        }

storedTerminal ::
    UTCTime ->
    TurnTerminalOutcome ->
    ServerTurnStore.ServerTurnTerminal
storedTerminal finishedAt = \case
    TurnSucceeded output ->
        let assistantText =
                boundedPublicText <$> output.turnExecutionAssistantText
         in ServerTurnStore.ServerTurnTerminal
                { ServerTurnStore.terminalServerTurnStatus =
                    ServerTurnStore.ServerTurnCompleted
                , ServerTurnStore.terminalServerTurnFinishedAt = finishedAt
                , ServerTurnStore.terminalServerTurnAssistantText =
                    fst <$> assistantText
                , ServerTurnStore.terminalServerTurnAssistantTextTruncated =
                    output.turnExecutionAssistantTextTruncated
                        || maybe False snd assistantText
                , ServerTurnStore.terminalServerTurnResponseId =
                    Just
                        ( fst
                            ( boundedPublicText
                                output.turnExecutionResponseId
                            )
                        )
                , ServerTurnStore.terminalServerTurnIncompleteReason =
                    case output.turnExecutionCompletion of
                        TurnCompletionComplete -> Nothing
                        TurnCompletionIncomplete reason _ ->
                            Just (fst (boundedPublicText reason))
                , ServerTurnStore.terminalServerTurnIncompleteReasoningTokens =
                    case output.turnExecutionCompletion of
                        TurnCompletionComplete -> Nothing
                        TurnCompletionIncomplete _ tokens ->
                            fromIntegral <$> tokens
                , ServerTurnStore.terminalServerTurnError = Nothing
                }
    TurnErrored err ->
        failedTerminal
            ServerTurnStore.ServerTurnFailed
            (Just (fst (boundedPublicText err)))
    TurnWasCancelled ->
        failedTerminal ServerTurnStore.ServerTurnCancelled Nothing
  where
    failedTerminal status err =
        ServerTurnStore.ServerTurnTerminal
            { ServerTurnStore.terminalServerTurnStatus = status
            , ServerTurnStore.terminalServerTurnFinishedAt = finishedAt
            , ServerTurnStore.terminalServerTurnAssistantText = Nothing
            , ServerTurnStore.terminalServerTurnAssistantTextTruncated =
                False
            , ServerTurnStore.terminalServerTurnResponseId = Nothing
            , ServerTurnStore.terminalServerTurnIncompleteReason = Nothing
            , ServerTurnStore.terminalServerTurnIncompleteReasoningTokens =
                Nothing
            , ServerTurnStore.terminalServerTurnError = err
            }

turnInputDigest :: Text -> Text
turnInputDigest prompt =
    Text.pack . show $
        ( hash
            ( TextEncoding.encodeUtf8
                ("agent-server client request v1\NUL" <> prompt)
            ) ::
            Digest SHA256
        )

requireStoreTransition :: Text -> Bool -> Either StoreError ()
requireStoreTransition label changed
    | changed = Right ()
    | otherwise =
        Left (StoreDataError ("server turn " <> label <> " was rejected"))

storeApiError :: StoreError -> ApiError
storeApiError err =
    ApiError
        { apiErrorStatus = 503
        , apiErrorCode = "store_unavailable"
        , apiErrorMessage = renderStoreError err
        , apiErrorDetails = Nothing
        }

unhealthyOwnerApiError :: ApiError
unhealthyOwnerApiError =
    ApiError
        { apiErrorStatus = 503
        , apiErrorCode = "turn_owner_unhealthy"
        , apiErrorMessage =
            "the server cannot confirm its durable turn-owner fence"
        , apiErrorDetails = Nothing
        }

notFoundApiError :: ApiError
notFoundApiError =
    ApiError
        { apiErrorStatus = 404
        , apiErrorCode = "not_found"
        , apiErrorMessage = "resource not found"
        , apiErrorDetails = Nothing
        }
