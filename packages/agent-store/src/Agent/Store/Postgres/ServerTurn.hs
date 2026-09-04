{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Durable admission and terminal results for externally submitted turns.
module Agent.Store.Postgres.ServerTurn
    ( ServerTurnBoundary (..)
    , ServerTurnStatus (..)
    , StoredServerTurn (..)
    , ReserveServerTurn (..)
    , ServerTurnReservation (..)
    , ServerTurnTerminal (..)
    , ServerSessionMutation (..)
    , ServerSessionMutationReservation (..)
    , ServerTurnOwnerLease
    , ServerTurnOwnerActionFence
    , openServerTurnOwnerLease
    , abandonServerTurnOwnerLease
    , openServerTurnOwnerActionFence
    , checkServerTurnOwnerActionFence
    , closeServerTurnOwnerActionFence
    , reserveServerTurn
    , reserveServerSessionMutation
    , releaseServerSessionMutation
    , requestServerTurnCancellation
    , shouldCancelServerTurn
    , markServerTurnRunning
    , finishServerTurn
    , heartbeatServerTurnOwner
    , releaseServerTurnOwner
    , loadServerTurn
    , listServerTurns
    )
where

import Agent.Store.Postgres.Connection (
    StoreConnection,
    StorePool,
    closeStoreConnection,
    openStoreConnection,
    withConnectionSession,
    withSession,
 )
import Agent.Store.Postgres.Hasql (mkStatement)
import Agent.Store.Types (StoreError (..))
import Control.Concurrent.MVar (
    MVar,
    modifyMVar,
    modifyMVar_,
    newMVar,
    withMVar,
 )
import Control.Exception.Safe (mask, onException)
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import qualified Data.Vector as Vector
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Session as Session
import Hasql.Statement (Statement)
import qualified Hasql.Transaction as Transaction
import qualified Hasql.Transaction.Sessions as Transactions

data ServerTurnBoundary = ServerTurnBoundary
    { serverTurnTenantId :: !Text
    , serverTurnGatewayIdentity :: !(Maybe Text)
    }
    deriving (Eq, Show)

data ServerTurnStatus
    = ServerTurnQueued
    | ServerTurnRunning
    | ServerTurnCompleted
    | ServerTurnFailed
    | ServerTurnCancelled
    deriving (Eq, Show)

data StoredServerTurn = StoredServerTurn
    { storedServerTurnId :: !Text
    , storedServerTurnBoundary :: !ServerTurnBoundary
    , storedServerTurnSessionId :: !Text
    , storedServerTurnClientRequestId :: !Text
    , storedServerTurnInputDigest :: !Text
    , storedServerTurnOwnerInstanceId :: !Text
    , storedServerTurnStatus :: !ServerTurnStatus
    , storedServerTurnCreatedAt :: !UTCTime
    , storedServerTurnStartedAt :: !(Maybe UTCTime)
    , storedServerTurnFinishedAt :: !(Maybe UTCTime)
    , storedServerTurnAssistantText :: !(Maybe Text)
    , storedServerTurnAssistantTextTruncated :: !Bool
    , storedServerTurnResponseId :: !(Maybe Text)
    , storedServerTurnIncompleteReason :: !(Maybe Text)
    , storedServerTurnIncompleteReasoningTokens :: !(Maybe Int64)
    , storedServerTurnError :: !(Maybe Text)
    }
    deriving (Eq, Show)

data ReserveServerTurn = ReserveServerTurn
    { reserveServerTurnId :: !Text
    , reserveServerTurnBoundary :: !ServerTurnBoundary
    , reserveServerTurnSessionId :: !Text
    , reserveServerTurnClientRequestId :: !Text
    , reserveServerTurnInputDigest :: !Text
    , reserveServerTurnOwnerInstanceId :: !Text
    , reserveServerTurnCreatedAt :: !UTCTime
    }
    deriving (Eq, Show)

data ServerTurnReservation
    = ServerTurnReserved !StoredServerTurn
    | ServerTurnAlreadyReserved !StoredServerTurn
    | ServerTurnIdempotencyConflict !StoredServerTurn
    | ServerTurnSessionBusy !StoredServerTurn
    | ServerTurnSessionMutating
    | ServerTurnOwnerUnavailable
    deriving (Eq, Show)

data ServerTurnTerminal = ServerTurnTerminal
    { terminalServerTurnStatus :: !ServerTurnStatus
    , terminalServerTurnFinishedAt :: !UTCTime
    , terminalServerTurnAssistantText :: !(Maybe Text)
    , terminalServerTurnAssistantTextTruncated :: !Bool
    , terminalServerTurnResponseId :: !(Maybe Text)
    , terminalServerTurnIncompleteReason :: !(Maybe Text)
    , terminalServerTurnIncompleteReasoningTokens :: !(Maybe Int64)
    , terminalServerTurnError :: !(Maybe Text)
    }
    deriving (Eq, Show)

data ServerSessionMutation = ServerSessionMutation
    { serverSessionMutationBoundary :: !ServerTurnBoundary
    , serverSessionMutationSessionId :: !Text
    , serverSessionMutationOwnerInstanceId :: !Text
    , serverSessionMutationCreatedAt :: !UTCTime
    }
    deriving (Eq, Show)

data ServerSessionMutationReservation
    = ServerSessionMutationReserved
    | ServerSessionMutationBusy
    | ServerSessionMutationSessionMissing
    | ServerSessionMutationOwnerUnavailable
    deriving (Eq, Show)

data ServerTurnOwnerLease = ServerTurnOwnerLease
    { serverTurnOwnerLeaseInstanceId :: !Text
    , serverTurnOwnerLeaseState :: !(MVar ServerTurnOwnerLeaseState)
    }

data ServerTurnOwnerLeaseState
    = ServerTurnOwnerLeaseOpen !StoreConnection
    | ServerTurnOwnerLeaseClosed

newtype ServerTurnOwnerActionFence
    = ServerTurnOwnerActionFence (MVar ServerTurnOwnerActionFenceState)

data ServerTurnOwnerActionFenceState
    = ServerTurnOwnerActionFenceOpen !StoreConnection
    | ServerTurnOwnerActionFenceClosed

{- | Register one server process behind a connection-lifetime PostgreSQL lock.

Unlike a timestamp lease, the lock survives arbitrary scheduler pauses.
PostgreSQL releases it automatically when the process or connection dies,
which lets another live owner recover its durable turns without trusting a
wall-clock timeout.
-}
openServerTurnOwnerLease ::
    StorePool ->
    Text ->
    IO (Either StoreError ServerTurnOwnerLease)
openServerTurnOwnerLease pool instanceId = mask \restore ->
    openStoreConnection pool >>= \case
        Left err -> pure (Left err)
        Right connection -> do
            locked <-
                restore
                    ( withConnectionSession connection $
                        Session.statement
                            instanceId
                            acquireServerTurnOwnerLockStatement
                    )
                    `onException` closeStoreConnection connection
            case locked of
                Left err -> do
                    closeStoreConnection connection
                    pure (Left err)
                Right False -> do
                    closeStoreConnection connection
                    pure . Left . StoreDataError $
                        "server turn owner identity is already active"
                Right True -> do
                    state <- newMVar (ServerTurnOwnerLeaseOpen connection)
                    let lease =
                            ServerTurnOwnerLease
                                { serverTurnOwnerLeaseInstanceId = instanceId
                                , serverTurnOwnerLeaseState = state
                                }
                    registered <-
                        restore (heartbeatServerTurnOwner lease)
                            `onException` abandonServerTurnOwnerLease lease
                    case registered of
                        Left err -> do
                            abandonServerTurnOwnerLease lease
                            pure (Left err)
                        Right () -> pure (Right lease)

{- | Drop the liveness connection without acknowledging local worker teardown.

Production uses this after an irreversible owner-connection failure. It is
also the crash boundary exercised by the store tests.
-}
abandonServerTurnOwnerLease :: ServerTurnOwnerLease -> IO ()
abandonServerTurnOwnerLease lease =
    modifyMVar_ lease.serverTurnOwnerLeaseState \case
        ServerTurnOwnerLeaseClosed ->
            pure ServerTurnOwnerLeaseClosed
        ServerTurnOwnerLeaseOpen connection -> do
            closeStoreConnection connection
            pure ServerTurnOwnerLeaseClosed

{- | Hold a shared action fence while this owner can still produce effects.

The reaper needs the exclusive form of this lock before revoking an owner.
Acquiring the shared lock before validating the durable owner makes action
start and owner revocation linearizable even if the liveness connection is
lost between those operations.
-}
openServerTurnOwnerActionFence ::
    StorePool ->
    Text ->
    IO (Either StoreError (Maybe ServerTurnOwnerActionFence))
openServerTurnOwnerActionFence pool instanceId = mask \restore ->
    openStoreConnection pool >>= \case
        Left err -> pure (Left err)
        Right connection -> do
            locked <-
                restore
                    ( withConnectionSession connection $
                        Session.statement
                            instanceId
                            acquireServerTurnOwnerActionFenceStatement
                    )
                    `onException` closeStoreConnection connection
            case locked of
                Left err -> do
                    closeStoreConnection connection
                    pure (Left err)
                Right False -> do
                    closeStoreConnection connection
                    pure (Right Nothing)
                Right True -> do
                    valid <-
                        restore
                            ( withConnectionSession connection $
                                Transactions.transaction
                                    Transactions.ReadCommitted
                                    Transactions.Read
                                    ( Transaction.statement
                                        instanceId
                                        validateServerTurnOwnerActionFenceStatement
                                    )
                            )
                            `onException` closeStoreConnection connection
                    case valid of
                        Left err -> do
                            closeStoreConnection connection
                            pure (Left err)
                        Right False -> do
                            closeStoreConnection connection
                            pure (Right Nothing)
                        Right True -> do
                            state <-
                                newMVar
                                    (ServerTurnOwnerActionFenceOpen connection)
                            pure . Right . Just $
                                ServerTurnOwnerActionFence state

checkServerTurnOwnerActionFence ::
    ServerTurnOwnerActionFence ->
    IO (Either StoreError ())
checkServerTurnOwnerActionFence
        (ServerTurnOwnerActionFence state) =
    withMVar state \case
        ServerTurnOwnerActionFenceClosed ->
            pure . Left . StoreDataError $
                "server turn owner action fence is closed"
        ServerTurnOwnerActionFenceOpen connection ->
            withConnectionSession connection $
                Session.statement () checkServerTurnOwnerActionFenceStatement

closeServerTurnOwnerActionFence :: ServerTurnOwnerActionFence -> IO ()
closeServerTurnOwnerActionFence
        (ServerTurnOwnerActionFence state) =
    modifyMVar_ state \case
        ServerTurnOwnerActionFenceClosed ->
            pure ServerTurnOwnerActionFenceClosed
        ServerTurnOwnerActionFenceOpen connection -> do
            closeStoreConnection connection
            pure ServerTurnOwnerActionFenceClosed

reserveServerTurn ::
    StorePool ->
    ReserveServerTurn ->
    IO (Either StoreError ServerTurnReservation)
reserveServerTurn pool request = do
    result <- withSession pool $
        Transactions.transaction Transactions.ReadCommitted Transactions.Write do
            Transaction.statement request lockServerTurnSessionStatement
            ownerAvailable <-
                Transaction.statement
                    request
                    lockServerTurnOwnerStatement
            if not ownerAvailable
                then pure (Just ServerTurnOwnerUnavailable)
                else
                    Transaction.statement
                        request
                        loadServerTurnByClientRequestStatement
                        >>= \case
                            Just existing
                                | existing.storedServerTurnInputDigest
                                    /= request.reserveServerTurnInputDigest ->
                                    pure . Just $
                                        ServerTurnIdempotencyConflict existing
                                | otherwise ->
                                    pure . Just $
                                        ServerTurnAlreadyReserved existing
                            Nothing ->
                                Transaction.statement
                                    request
                                    serverSessionMutationExistsForTurnStatement
                                    >>= \case
                                        True ->
                                            pure (Just ServerTurnSessionMutating)
                                        False ->
                                            Transaction.statement
                                                request
                                                insertServerTurnStatement
                                                >>= \case
                                                    Just inserted ->
                                                        pure . Just $
                                                            ServerTurnReserved inserted
                                                    Nothing ->
                                                        fmap ServerTurnSessionBusy
                                                            <$> Transaction.statement
                                                                request
                                                                loadActiveServerTurnStatement
    pure $
        result
            >>= maybe
                ( Left
                    ( StoreDataError
                        "server turn reservation did not find its session"
                    )
                )
                Right

reserveServerSessionMutation ::
    StorePool ->
    ServerSessionMutation ->
    IO (Either StoreError ServerSessionMutationReservation)
reserveServerSessionMutation pool request =
    withSession pool $
        Transactions.transaction Transactions.ReadCommitted Transactions.Write do
            Transaction.statement
                request
                lockServerSessionMutationStatement
            ownerAvailable <-
                Transaction.statement
                    request
                    lockServerSessionMutationOwnerStatement
            if not ownerAvailable
                then pure ServerSessionMutationOwnerUnavailable
                else do
                    exists <-
                        Transaction.statement
                            request
                            serverSessionExistsForMutationStatement
                    if not exists
                        then pure ServerSessionMutationSessionMissing
                        else do
                            active <-
                                Transaction.statement
                                    request
                                    activeServerTurnExistsForMutationStatement
                            if active
                                then pure ServerSessionMutationBusy
                                else do
                                    acquired <-
                                        Transaction.statement
                                            request
                                            insertServerSessionMutationStatement
                                    pure
                                        ( if acquired
                                            then ServerSessionMutationReserved
                                            else ServerSessionMutationBusy
                                        )

releaseServerSessionMutation ::
    StorePool ->
    ServerSessionMutation ->
    IO (Either StoreError ())
releaseServerSessionMutation pool request =
    withSession pool $
        Transactions.transaction Transactions.ReadCommitted Transactions.Write do
            Transaction.statement
                request
                lockServerSessionMutationStatement
            Transaction.statement
                request
                deleteServerSessionMutationStatement

requestServerTurnCancellation ::
    StorePool ->
    ServerTurnBoundary ->
    Text ->
    UTCTime ->
    IO (Either StoreError (Maybe StoredServerTurn))
requestServerTurnCancellation pool boundary turnId requestedAt =
    withSession pool $
        Transactions.transaction Transactions.ReadCommitted Transactions.Write $
            Transaction.statement
                (turnId, boundary, requestedAt)
                requestServerTurnCancellationStatement

shouldCancelServerTurn ::
    StorePool ->
    ServerTurnBoundary ->
    Text ->
    Text ->
    IO (Either StoreError Bool)
shouldCancelServerTurn pool boundary instanceId turnId =
    withSession pool $
        Transactions.transaction Transactions.ReadCommitted Transactions.Read do
            Transaction.statement
                (turnId, boundary, instanceId)
                shouldCancelServerTurnStatement

{- | Refresh one process registration and recover owners whose liveness
connection has disappeared.

A disconnected owner is terminalized only after this transaction acquires
its advisory lock. A merely paused owner still holds that lock, so neither
its turns nor its session mutations expire with wall-clock time.
-}
heartbeatServerTurnOwner ::
    ServerTurnOwnerLease ->
    IO (Either StoreError ())
heartbeatServerTurnOwner lease =
    withMVar lease.serverTurnOwnerLeaseState \case
        ServerTurnOwnerLeaseClosed ->
            pure . Left . StoreDataError $
                "server turn owner liveness connection is closed"
        ServerTurnOwnerLeaseOpen connection -> do
            accepted <-
                withConnectionSession connection $
                    Transactions.transaction
                        Transactions.ReadCommitted
                        Transactions.Write
                        do
                            live <-
                                Transaction.statement
                                    lease.serverTurnOwnerLeaseInstanceId
                                    heartbeatServerTurnOwnerStatement
                            if live
                                then do
                                    Transaction.statement
                                        lease.serverTurnOwnerLeaseInstanceId
                                        interruptDisconnectedServerTurnsStatement
                                    pure True
                                else pure False
            pure (accepted >>= ensureAccepted)
  where
    ensureAccepted True = Right ()
    ensureAccepted False =
        Left
            ( StoreDataError
                "server turn owner lease has been revoked"
            )

-- | Retire an owner during an orderly shutdown.
--
-- The supervisor normally terminalizes all admitted turns first. This final
-- fence covers reservations which were persisted but never admitted locally.
releaseServerTurnOwner ::
    ServerTurnOwnerLease ->
    IO (Either StoreError ())
releaseServerTurnOwner lease =
    modifyMVar lease.serverTurnOwnerLeaseState \case
        ServerTurnOwnerLeaseClosed ->
            pure (ServerTurnOwnerLeaseClosed, Right ())
        ServerTurnOwnerLeaseOpen connection -> do
            released <-
                withConnectionSession connection $
                    Transactions.transaction
                        Transactions.ReadCommitted
                        Transactions.Write
                        do
                            actionFenceAvailable <-
                                Transaction.statement
                                    lease.serverTurnOwnerLeaseInstanceId
                                    acquireServerTurnOwnerReleaseFenceStatement
                            if actionFenceAvailable
                                then do
                                    Transaction.statement
                                        lease.serverTurnOwnerLeaseInstanceId
                                        interruptReleasedServerTurnsStatement
                                    Transaction.statement
                                        lease.serverTurnOwnerLeaseInstanceId
                                        deleteServerTurnOwnerStatement
                                    pure True
                                else pure False
            closeStoreConnection connection
            pure
                ( ServerTurnOwnerLeaseClosed
                , released >>= \case
                    True -> Right ()
                    False ->
                        Left
                            ( StoreDataError
                                "server turn owner still has active actions"
                            )
                )

markServerTurnRunning ::
    StorePool ->
    ServerTurnBoundary ->
    Text ->
    Text ->
    UTCTime ->
    IO (Either StoreError Bool)
markServerTurnRunning pool boundary instanceId turnId startedAt =
    withSession pool $
        Transactions.transaction Transactions.ReadCommitted Transactions.Write $
            Transaction.statement
                (startedAt, turnId, boundary, instanceId)
                markServerTurnRunningStatement

finishServerTurn ::
    StorePool ->
    ServerTurnBoundary ->
    Text ->
    Text ->
    ServerTurnTerminal ->
    IO (Either StoreError (Maybe StoredServerTurn))
finishServerTurn pool boundary instanceId turnId terminal
    | not (isTerminal terminal.terminalServerTurnStatus) =
        pure $
            Left
                ( StoreDataError
                    "server turn terminal transition requires a terminal status"
                )
    | otherwise =
        withSession pool
            $ Transactions.transaction
                Transactions.ReadCommitted
                Transactions.Write
            $ Transaction.statement
                (turnId, boundary, instanceId, terminal)
                finishServerTurnStatement

loadServerTurn ::
    StorePool ->
    ServerTurnBoundary ->
    Text ->
    IO (Either StoreError (Maybe StoredServerTurn))
loadServerTurn pool boundary turnId =
    withSession pool $
        Transactions.transaction Transactions.ReadCommitted Transactions.Read $
            Transaction.statement
                (turnId, boundary)
                loadServerTurnStatement

listServerTurns ::
    StorePool ->
    ServerTurnBoundary ->
    Maybe Text ->
    IO (Either StoreError [StoredServerTurn])
listServerTurns pool boundary sessionId =
    withSession pool $
        Transactions.transaction Transactions.ReadCommitted Transactions.Read do
            Vector.toList
                <$> Transaction.statement
                    (boundary, sessionId)
                    listServerTurnsStatement

data RawServerTurn = RawServerTurn
    { rawTurnId :: !Text
    , rawTenantId :: !Text
    , rawGatewayIdentity :: !(Maybe Text)
    , rawSessionId :: !Text
    , rawClientRequestId :: !Text
    , rawInputDigest :: !Text
    , rawOwnerInstanceId :: !Text
    , rawStatus :: !Text
    , rawCreatedAt :: !UTCTime
    , rawStartedAt :: !(Maybe UTCTime)
    , rawFinishedAt :: !(Maybe UTCTime)
    , rawAssistantText :: !(Maybe Text)
    , rawAssistantTextTruncated :: !Bool
    , rawResponseId :: !(Maybe Text)
    , rawIncompleteReason :: !(Maybe Text)
    , rawIncompleteReasoningTokens :: !(Maybe Int64)
    , rawError :: !(Maybe Text)
    }

decodeServerTurn :: RawServerTurn -> StoredServerTurn
decodeServerTurn raw =
    StoredServerTurn
        { storedServerTurnId = raw.rawTurnId
        , storedServerTurnBoundary =
            ServerTurnBoundary
                { serverTurnTenantId = raw.rawTenantId
                , serverTurnGatewayIdentity = raw.rawGatewayIdentity
                }
        , storedServerTurnSessionId = raw.rawSessionId
        , storedServerTurnClientRequestId = raw.rawClientRequestId
        , storedServerTurnInputDigest = raw.rawInputDigest
        , storedServerTurnOwnerInstanceId = raw.rawOwnerInstanceId
        , storedServerTurnStatus = decodeStatus raw.rawStatus
        , storedServerTurnCreatedAt = raw.rawCreatedAt
        , storedServerTurnStartedAt = raw.rawStartedAt
        , storedServerTurnFinishedAt = raw.rawFinishedAt
        , storedServerTurnAssistantText = raw.rawAssistantText
        , storedServerTurnAssistantTextTruncated =
            raw.rawAssistantTextTruncated
        , storedServerTurnResponseId = raw.rawResponseId
        , storedServerTurnIncompleteReason = raw.rawIncompleteReason
        , storedServerTurnIncompleteReasoningTokens =
            raw.rawIncompleteReasoningTokens
        , storedServerTurnError = raw.rawError
        }

decodeStatus :: Text -> ServerTurnStatus
decodeStatus = \case
    "queued" -> ServerTurnQueued
    "running" -> ServerTurnRunning
    "completed" -> ServerTurnCompleted
    "failed" -> ServerTurnFailed
    "cancelled" -> ServerTurnCancelled
    _ -> ServerTurnFailed

encodeStatus :: ServerTurnStatus -> Text
encodeStatus = \case
    ServerTurnQueued -> "queued"
    ServerTurnRunning -> "running"
    ServerTurnCompleted -> "completed"
    ServerTurnFailed -> "failed"
    ServerTurnCancelled -> "cancelled"

isActive :: ServerTurnStatus -> Bool
isActive = \case
    ServerTurnQueued -> True
    ServerTurnRunning -> True
    ServerTurnCompleted -> False
    ServerTurnFailed -> False
    ServerTurnCancelled -> False

isTerminal :: ServerTurnStatus -> Bool
isTerminal = not . isActive

serverTurnColumns :: Text
serverTurnColumns =
    "turn_id::text, tenant_id, gateway_identity,\
    \ session_key, client_request_id::text, input_digest,\
    \ owner_instance_id::text, status, server_turn.created_at,\
    \ started_at, finished_at, assistant_text, assistant_text_truncated,\
    \ response_id,\
    \ incomplete_reason, incomplete_reasoning_tokens, error_text"

serverTurnDecoder :: Decoders.Row StoredServerTurn
serverTurnDecoder =
    decodeServerTurn
        <$> ( RawServerTurn
                <$> Decoders.column (Decoders.nonNullable Decoders.text)
                <*> Decoders.column (Decoders.nonNullable Decoders.text)
                <*> Decoders.column (Decoders.nullable Decoders.text)
                <*> Decoders.column (Decoders.nonNullable Decoders.text)
                <*> Decoders.column (Decoders.nonNullable Decoders.text)
                <*> Decoders.column (Decoders.nonNullable Decoders.text)
                <*> Decoders.column (Decoders.nonNullable Decoders.text)
                <*> Decoders.column (Decoders.nonNullable Decoders.text)
                <*> Decoders.column (Decoders.nonNullable Decoders.timestamptz)
                <*> Decoders.column (Decoders.nullable Decoders.timestamptz)
                <*> Decoders.column (Decoders.nullable Decoders.timestamptz)
                <*> Decoders.column (Decoders.nullable Decoders.text)
                <*> Decoders.column (Decoders.nonNullable Decoders.bool)
                <*> Decoders.column (Decoders.nullable Decoders.text)
                <*> Decoders.column (Decoders.nullable Decoders.text)
                <*> Decoders.column (Decoders.nullable Decoders.int8)
                <*> Decoders.column (Decoders.nullable Decoders.text)
            )

boundaryEncoder :: (input -> ServerTurnBoundary) -> Encoders.Params input
boundaryEncoder select =
    ( ((.serverTurnTenantId) . select)
        >$< Encoders.param (Encoders.nonNullable Encoders.text)
    )
        <> ( ((.serverTurnGatewayIdentity) . select)
                >$< Encoders.param (Encoders.nullable Encoders.text)
           )

lockServerTurnSessionStatement :: Statement ReserveServerTurn Bool
lockServerTurnSessionStatement =
    mkStatement
        "SELECT pg_advisory_xact_lock(\
        \ hashtextextended(\
        \   jsonb_build_array($1::text, $2::text, $3::text)::text,\
        \   0)) IS NULL"
        ( boundaryEncoder (.reserveServerTurnBoundary)
            <> ( (.reserveServerTurnSessionId)
                    >$< Encoders.param (Encoders.nonNullable Encoders.text)
               )
        )
        (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
        True

lockServerTurnOwnerStatement :: Statement ReserveServerTurn Bool
lockServerTurnOwnerStatement =
    mkStatement
        "SELECT COALESCE((\
        \ SELECT revoked_at IS NULL\
        \ FROM harness.server_turn_owners\
        \ WHERE instance_id = $1::uuid\
        \ FOR SHARE), FALSE)\
        \ AND NOT pg_try_advisory_xact_lock(\
        \   hashtextextended(\
        \     'haskell-agent:server-turn-owner:' || $1::text,\
        \     0))"
        ( (.reserveServerTurnOwnerInstanceId)
            >$< Encoders.param (Encoders.nonNullable Encoders.text)
        )
        (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
        True

serverSessionMutationExistsForTurnStatement ::
    Statement ReserveServerTurn Bool
serverSessionMutationExistsForTurnStatement =
    mkStatement
        "SELECT EXISTS (\
        \ SELECT 1 FROM harness.server_session_mutations\
        \ WHERE tenant_id = $1\
        \ AND gateway_identity IS NOT DISTINCT FROM $2\
        \ AND session_key = $3)"
        ( boundaryEncoder (.reserveServerTurnBoundary)
            <> ( (.reserveServerTurnSessionId)
                    >$< Encoders.param (Encoders.nonNullable Encoders.text)
               )
        )
        (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
        True

serverSessionMutationKeyEncoder ::
    Encoders.Params ServerSessionMutation
serverSessionMutationKeyEncoder =
    boundaryEncoder (.serverSessionMutationBoundary)
        <> ( (.serverSessionMutationSessionId)
                >$< Encoders.param (Encoders.nonNullable Encoders.text)
           )

serverSessionMutationEncoder ::
    Encoders.Params ServerSessionMutation
serverSessionMutationEncoder =
    serverSessionMutationKeyEncoder
        <> ( (.serverSessionMutationOwnerInstanceId)
                >$< Encoders.param (Encoders.nonNullable Encoders.text)
           )
        <> ( (.serverSessionMutationCreatedAt)
                >$< Encoders.param (Encoders.nonNullable Encoders.timestamptz)
           )

lockServerSessionMutationStatement ::
    Statement ServerSessionMutation Bool
lockServerSessionMutationStatement =
    mkStatement
        "SELECT pg_advisory_xact_lock(\
        \ hashtextextended(\
        \   jsonb_build_array($1::text, $2::text, $3::text)::text,\
        \   0)) IS NULL"
        serverSessionMutationKeyEncoder
        (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
        True

acquireServerTurnOwnerActionFenceStatement ::
    Statement Text Bool
acquireServerTurnOwnerActionFenceStatement =
    mkStatement
        "WITH locked AS MATERIALIZED (\
        \ SELECT pg_advisory_lock_shared(\
        \   hashtextextended(\
        \     'haskell-agent:server-turn-owner-action:' || $1::text,\
        \     0)))\
        \ SELECT TRUE FROM locked"
        (Encoders.param (Encoders.nonNullable Encoders.text))
        (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
        True

validateServerTurnOwnerActionFenceStatement ::
    Statement Text Bool
validateServerTurnOwnerActionFenceStatement =
    mkStatement
        "SELECT COALESCE((\
        \ SELECT revoked_at IS NULL\
        \ FROM harness.server_turn_owners\
        \ WHERE instance_id = $1::uuid), FALSE)\
        \ AND NOT pg_try_advisory_xact_lock(\
        \   hashtextextended(\
        \     'haskell-agent:server-turn-owner:' || $1::text,\
        \     0))"
        (Encoders.param (Encoders.nonNullable Encoders.text))
        (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
        True

checkServerTurnOwnerActionFenceStatement :: Statement () ()
checkServerTurnOwnerActionFenceStatement =
    mkStatement
        "SELECT 1"
        Encoders.noParams
        ( const ()
            <$> Decoders.singleRow
                (Decoders.column (Decoders.nonNullable Decoders.int4))
        )
        True

lockServerSessionMutationOwnerStatement ::
    Statement ServerSessionMutation Bool
lockServerSessionMutationOwnerStatement =
    mkStatement
        "SELECT COALESCE((\
        \ SELECT revoked_at IS NULL\
        \ FROM harness.server_turn_owners\
        \ WHERE instance_id = $1::uuid\
        \ FOR SHARE), FALSE)\
        \ AND NOT pg_try_advisory_xact_lock(\
        \   hashtextextended(\
        \     'haskell-agent:server-turn-owner:' || $1::text,\
        \     0))"
        ( (.serverSessionMutationOwnerInstanceId)
            >$< Encoders.param (Encoders.nonNullable Encoders.text)
        )
        (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
        True

serverSessionExistsForMutationStatement ::
    Statement ServerSessionMutation Bool
serverSessionExistsForMutationStatement =
    mkStatement
        "SELECT EXISTS (\
        \ SELECT 1 FROM harness.sessions\
        \ WHERE $1::text IS NOT NULL\
        \ AND gateway_identity IS NOT DISTINCT FROM $2\
        \ AND session_key = $3\
        \ AND deleted_at IS NULL)"
        serverSessionMutationKeyEncoder
        (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
        True

activeServerTurnExistsForMutationStatement ::
    Statement ServerSessionMutation Bool
activeServerTurnExistsForMutationStatement =
    mkStatement
        "SELECT EXISTS (\
        \ SELECT 1 FROM harness.server_turns\
        \ WHERE tenant_id = $1\
        \ AND gateway_identity IS NOT DISTINCT FROM $2\
        \ AND session_key = $3\
        \ AND status IN ('queued', 'running'))"
        serverSessionMutationKeyEncoder
        (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
        True

insertServerSessionMutationStatement ::
    Statement ServerSessionMutation Bool
insertServerSessionMutationStatement =
    mkStatement
        "WITH inserted AS (\
        \ INSERT INTO harness.server_session_mutations\
        \ (tenant_id, gateway_identity, session_key,\
        \  owner_instance_id, created_at)\
        \ VALUES ($1, $2, $3, $4::uuid, $5)\
        \ ON CONFLICT DO NOTHING\
        \ RETURNING 1)\
        \ SELECT EXISTS (SELECT 1 FROM inserted)"
        serverSessionMutationEncoder
        (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
        True

deleteServerSessionMutationStatement ::
    Statement ServerSessionMutation ()
deleteServerSessionMutationStatement =
    mkStatement
        "DELETE FROM harness.server_session_mutations\
        \ WHERE tenant_id = $1\
        \ AND gateway_identity IS NOT DISTINCT FROM $2\
        \ AND session_key = $3\
        \ AND owner_instance_id = $4::uuid\
        \ AND created_at = $5"
        serverSessionMutationEncoder
        Decoders.noResult
        True

insertServerTurnStatement ::
    Statement ReserveServerTurn (Maybe StoredServerTurn)
insertServerTurnStatement =
    mkStatement
        ( "INSERT INTO harness.server_turns AS server_turn\
          \ (turn_id, tenant_id, gateway_identity, session_key,\
          \ client_request_id, input_digest, owner_instance_id, status, created_at)\
          \ SELECT $1::uuid, $2, $3, session.session_key,\
          \ $5::uuid, $6, $7::uuid, 'queued', $8\
          \ FROM harness.sessions session\
          \ WHERE session.session_key = $4 AND session.deleted_at IS NULL\
          \ AND session.gateway_identity IS NOT DISTINCT FROM $3\
          \ ON CONFLICT DO NOTHING\
          \ RETURNING "
            <> serverTurnColumns
        )
        ( ( (.reserveServerTurnId)
                >$< Encoders.param (Encoders.nonNullable Encoders.text)
          )
            <> boundaryEncoder (.reserveServerTurnBoundary)
            <> ( (.reserveServerTurnSessionId)
                    >$< Encoders.param (Encoders.nonNullable Encoders.text)
               )
            <> ( (.reserveServerTurnClientRequestId)
                    >$< Encoders.param (Encoders.nonNullable Encoders.text)
               )
            <> ( (.reserveServerTurnInputDigest)
                    >$< Encoders.param (Encoders.nonNullable Encoders.text)
               )
            <> ( (.reserveServerTurnOwnerInstanceId)
                    >$< Encoders.param (Encoders.nonNullable Encoders.text)
               )
            <> ( (.reserveServerTurnCreatedAt)
                    >$< Encoders.param (Encoders.nonNullable Encoders.timestamptz)
               )
        )
        (Decoders.rowMaybe serverTurnDecoder)
        True

loadServerTurnByClientRequestStatement ::
    Statement ReserveServerTurn (Maybe StoredServerTurn)
loadServerTurnByClientRequestStatement =
    mkStatement
        ( "SELECT "
            <> serverTurnColumns
            <> "\
               \ FROM harness.server_turns server_turn\
               \ WHERE tenant_id = $1\
               \ AND gateway_identity IS NOT DISTINCT FROM $2\
               \ AND session_key = $3\
               \ AND client_request_id = $4::uuid\
               \ FOR UPDATE OF server_turn"
        )
        ( ( ((.serverTurnTenantId) . (.reserveServerTurnBoundary))
                >$< Encoders.param (Encoders.nonNullable Encoders.text)
          )
            <> ( ((.serverTurnGatewayIdentity) . (.reserveServerTurnBoundary))
                    >$< Encoders.param (Encoders.nullable Encoders.text)
               )
            <> ( (.reserveServerTurnSessionId)
                    >$< Encoders.param (Encoders.nonNullable Encoders.text)
               )
            <> ( (.reserveServerTurnClientRequestId)
                    >$< Encoders.param (Encoders.nonNullable Encoders.text)
               )
        )
        (Decoders.rowMaybe serverTurnDecoder)
        True

loadActiveServerTurnStatement ::
    Statement ReserveServerTurn (Maybe StoredServerTurn)
loadActiveServerTurnStatement =
    mkStatement
        ( "SELECT "
            <> serverTurnColumns
            <> "\
               \ FROM harness.server_turns server_turn\
               \ WHERE tenant_id = $1\
               \ AND gateway_identity IS NOT DISTINCT FROM $2\
               \ AND session_key = $3\
               \ AND status IN ('queued', 'running')\
               \ ORDER BY created_at, turn_id\
               \ LIMIT 1\
               \ FOR UPDATE OF server_turn"
        )
        ( ( ((.serverTurnTenantId) . (.reserveServerTurnBoundary))
                >$< Encoders.param (Encoders.nonNullable Encoders.text)
          )
            <> ( ((.serverTurnGatewayIdentity) . (.reserveServerTurnBoundary))
                    >$< Encoders.param (Encoders.nullable Encoders.text)
               )
            <> ( (.reserveServerTurnSessionId)
                    >$< Encoders.param (Encoders.nonNullable Encoders.text)
               )
        )
        (Decoders.rowMaybe serverTurnDecoder)
        True

markServerTurnRunningStatement ::
    Statement (UTCTime, Text, ServerTurnBoundary, Text) Bool
markServerTurnRunningStatement =
    mkStatement
        "WITH changed AS (\
        \ UPDATE harness.server_turns\
        \ SET status = 'running', started_at = COALESCE(started_at, $1)\
        \ WHERE turn_id = $2::uuid AND tenant_id = $3\
        \ AND gateway_identity IS NOT DISTINCT FROM $4\
        \ AND owner_instance_id = $5::uuid AND status IN ('queued', 'running')\
        \ RETURNING 1)\
        \ SELECT EXISTS (SELECT 1 FROM changed)"
        ( ( (\(startedAt, _, _, _) -> startedAt)
                >$< Encoders.param (Encoders.nonNullable Encoders.timestamptz)
          )
            <> ( (\(_, turnId, _, _) -> turnId)
                    >$< Encoders.param (Encoders.nonNullable Encoders.text)
               )
            <> boundaryEncoder (\(_, _, boundary, _) -> boundary)
            <> ( (\(_, _, _, instanceId) -> instanceId)
                    >$< Encoders.param (Encoders.nonNullable Encoders.text)
               )
        )
        (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
        True

finishServerTurnStatement ::
    Statement
        (Text, ServerTurnBoundary, Text, ServerTurnTerminal)
        (Maybe StoredServerTurn)
finishServerTurnStatement =
    mkStatement
        ( "WITH finished_turn AS (\
          \ UPDATE harness.server_turns AS server_turn\
          \ SET status = CASE WHEN status IN ('queued', 'running')\
          \   THEN $5 ELSE status END,\
          \ finished_at = CASE WHEN status IN ('queued', 'running')\
          \   THEN $6 ELSE finished_at END,\
          \ assistant_text = CASE WHEN status IN ('queued', 'running')\
          \   THEN $7 ELSE assistant_text END,\
          \ assistant_text_truncated = CASE\
          \   WHEN status IN ('queued', 'running') THEN $8\
          \   ELSE assistant_text_truncated END,\
          \ response_id = CASE WHEN status IN ('queued', 'running')\
          \   THEN $9 ELSE response_id END,\
          \ incomplete_reason = CASE WHEN status IN ('queued', 'running')\
          \   THEN $10 ELSE incomplete_reason END,\
          \ incomplete_reasoning_tokens = CASE\
          \   WHEN status IN ('queued', 'running') THEN $11\
          \   ELSE incomplete_reasoning_tokens END,\
          \ error_text = CASE WHEN status IN ('queued', 'running')\
          \   THEN $12 ELSE error_text END\
          \ WHERE turn_id = $1::uuid AND tenant_id = $2\
          \ AND gateway_identity IS NOT DISTINCT FROM $3\
          \ AND owner_instance_id = $4::uuid\
          \ RETURNING turn_id AS durable_turn_id, "
            <> serverTurnColumns
            <> "\
               \ ), deleted_request AS (\
               \ DELETE FROM harness.server_human_requests AS request\
               \ USING finished_turn\
               \ WHERE request.turn_id = finished_turn.durable_turn_id)\
               \ SELECT "
            <> serverTurnColumns
            <> " FROM finished_turn AS server_turn"
        )
        ( ( (\(turnId, _, _, _) -> turnId)
                >$< Encoders.param (Encoders.nonNullable Encoders.text)
          )
            <> boundaryEncoder (\(_, boundary, _, _) -> boundary)
            <> ( (\(_, _, instanceId, _) -> instanceId)
                    >$< Encoders.param (Encoders.nonNullable Encoders.text)
               )
            <> ( ( encodeStatus
                    . (.terminalServerTurnStatus)
                    . (\(_, _, _, terminal) -> terminal)
                 )
                    >$< Encoders.param (Encoders.nonNullable Encoders.text)
               )
            <> ( ( (.terminalServerTurnFinishedAt)
                    . (\(_, _, _, terminal) -> terminal)
                 )
                    >$< Encoders.param (Encoders.nonNullable Encoders.timestamptz)
               )
            <> ( ( (.terminalServerTurnAssistantText)
                    . (\(_, _, _, terminal) -> terminal)
                 )
                    >$< Encoders.param (Encoders.nullable Encoders.text)
               )
            <> ( ( (.terminalServerTurnAssistantTextTruncated)
                    . (\(_, _, _, terminal) -> terminal)
                 )
                    >$< Encoders.param (Encoders.nonNullable Encoders.bool)
               )
            <> ( ( (.terminalServerTurnResponseId)
                    . (\(_, _, _, terminal) -> terminal)
                 )
                    >$< Encoders.param (Encoders.nullable Encoders.text)
               )
            <> ( ( (.terminalServerTurnIncompleteReason)
                    . (\(_, _, _, terminal) -> terminal)
                 )
                    >$< Encoders.param (Encoders.nullable Encoders.text)
               )
            <> ( ( (.terminalServerTurnIncompleteReasoningTokens)
                    . (\(_, _, _, terminal) -> terminal)
                 )
                    >$< Encoders.param (Encoders.nullable Encoders.int8)
               )
            <> ( ( (.terminalServerTurnError)
                    . (\(_, _, _, terminal) -> terminal)
                 )
                    >$< Encoders.param (Encoders.nullable Encoders.text)
               )
        )
        (Decoders.rowMaybe serverTurnDecoder)
        True

requestServerTurnCancellationStatement ::
    Statement
        (Text, ServerTurnBoundary, UTCTime)
        (Maybe StoredServerTurn)
requestServerTurnCancellationStatement =
    mkStatement
        ( "UPDATE harness.server_turns AS server_turn\
          \ SET cancellation_requested_at =\
          \   COALESCE(cancellation_requested_at, $4)\
          \ WHERE turn_id = $1::uuid AND tenant_id = $2\
          \ AND gateway_identity IS NOT DISTINCT FROM $3\
          \ AND status IN ('queued', 'running')\
          \ RETURNING "
            <> serverTurnColumns
        )
        ( ( (\(turnId, _, _) -> turnId)
                >$< Encoders.param (Encoders.nonNullable Encoders.text)
          )
            <> boundaryEncoder (\(_, boundary, _) -> boundary)
            <> ( (\(_, _, requestedAt) -> requestedAt)
                    >$< Encoders.param
                        (Encoders.nonNullable Encoders.timestamptz)
               )
        )
        (Decoders.rowMaybe serverTurnDecoder)
        True

shouldCancelServerTurnStatement ::
    Statement (Text, ServerTurnBoundary, Text) Bool
shouldCancelServerTurnStatement =
    mkStatement
        "SELECT COALESCE((\
        \ SELECT cancellation_requested_at IS NOT NULL\
        \ FROM harness.server_turns\
        \ WHERE turn_id = $1::uuid AND tenant_id = $2\
        \ AND gateway_identity IS NOT DISTINCT FROM $3\
        \ AND owner_instance_id = $4::uuid\
        \ AND status IN ('queued', 'running')),\
        \ TRUE)"
        ( ( (\(turnId, _, _) -> turnId)
                >$< Encoders.param (Encoders.nonNullable Encoders.text)
          )
            <> boundaryEncoder (\(_, boundary, _) -> boundary)
            <> ( (\(_, _, instanceId) -> instanceId)
                    >$< Encoders.param (Encoders.nonNullable Encoders.text)
               )
        )
        (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
        True

loadServerTurnStatement ::
    Statement (Text, ServerTurnBoundary) (Maybe StoredServerTurn)
loadServerTurnStatement =
    mkStatement
        ( "SELECT "
            <> serverTurnColumns
            <> "\
               \ FROM harness.server_turns server_turn\
               \ WHERE turn_id = $1::uuid AND tenant_id = $2\
               \ AND gateway_identity IS NOT DISTINCT FROM $3"
        )
        ( (fst >$< Encoders.param (Encoders.nonNullable Encoders.text))
            <> boundaryEncoder snd
        )
        (Decoders.rowMaybe serverTurnDecoder)
        True

heartbeatServerTurnOwnerStatement ::
    Statement Text Bool
heartbeatServerTurnOwnerStatement =
    mkStatement
        "WITH heartbeat AS (\
        \ INSERT INTO harness.server_turn_owners AS owner\
        \ (instance_id, last_heartbeat_at)\
        \ VALUES ($1::uuid, clock_timestamp())\
        \ ON CONFLICT (instance_id) DO UPDATE\
        \ SET last_heartbeat_at = EXCLUDED.last_heartbeat_at\
        \ WHERE owner.revoked_at IS NULL\
        \ RETURNING 1)\
        \ SELECT EXISTS (SELECT 1 FROM heartbeat)"
        (Encoders.param (Encoders.nonNullable Encoders.text))
        (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
        True

acquireServerTurnOwnerLockStatement :: Statement Text Bool
acquireServerTurnOwnerLockStatement =
    mkStatement
        "SELECT pg_try_advisory_lock(\
        \ hashtextextended(\
        \   'haskell-agent:server-turn-owner:' || $1::text,\
        \   0))"
        (Encoders.param (Encoders.nonNullable Encoders.text))
        (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
        True

interruptDisconnectedServerTurnsStatement ::
    Statement Text ()
interruptDisconnectedServerTurnsStatement =
    mkStatement
        "WITH action_unfenced_owner AS MATERIALIZED (\
        \ SELECT owner.instance_id\
        \ FROM harness.server_turn_owners AS owner\
        \ WHERE owner.instance_id <> $1::uuid\
        \ AND owner.revoked_at IS NULL\
        \ AND pg_try_advisory_xact_lock(\
        \   hashtextextended(\
        \     'haskell-agent:server-turn-owner-action:'\
        \       || owner.instance_id::text,\
        \     0))\
        \ FOR UPDATE OF owner SKIP LOCKED\
        \ ), disconnected_owner AS MATERIALIZED (\
        \ SELECT owner.instance_id\
        \ FROM action_unfenced_owner AS candidate\
        \ JOIN harness.server_turn_owners AS owner\
        \   ON owner.instance_id = candidate.instance_id\
        \ AND pg_try_advisory_xact_lock(\
        \   hashtextextended(\
        \     'haskell-agent:server-turn-owner:'\
        \       || owner.instance_id::text,\
        \     0))\
        \ ), revoked_owner AS (\
        \ UPDATE harness.server_turn_owners AS owner\
        \ SET revoked_at = clock_timestamp()\
        \ FROM disconnected_owner AS disconnected\
        \ WHERE owner.instance_id = disconnected.instance_id\
        \ RETURNING owner.instance_id\
        \ ), released_mutation AS (\
        \ DELETE FROM harness.server_session_mutations AS mutation\
        \ USING revoked_owner AS owner\
        \ WHERE mutation.owner_instance_id = owner.instance_id\
        \ ), interrupted_turn AS (\
        \ UPDATE harness.server_turns AS turn\
        \ SET status = 'failed', finished_at = clock_timestamp(),\
        \ error_text =\
        \   'agent server owner disconnected before the turn completed'\
        \ WHERE status IN ('queued', 'running')\
        \ AND owner_instance_id IN (SELECT instance_id FROM revoked_owner)\
        \ RETURNING turn.turn_id)\
        \ DELETE FROM harness.server_human_requests AS request\
        \ USING interrupted_turn AS turn\
        \ WHERE request.turn_id = turn.turn_id"
        (Encoders.param (Encoders.nonNullable Encoders.text))
        Decoders.noResult
        True

interruptReleasedServerTurnsStatement ::
    Statement Text ()
interruptReleasedServerTurnsStatement =
    mkStatement
        "WITH interrupted_turn AS (\
        \ UPDATE harness.server_turns AS turn\
        \ SET status = 'failed', finished_at = clock_timestamp(),\
        \ error_text = CASE WHEN status IN ('queued', 'running')\
        \   THEN 'agent server stopped before the turn completed'\
        \   ELSE error_text END\
        \ WHERE owner_instance_id = $1::uuid\
        \ AND status IN ('queued', 'running')\
        \ RETURNING turn.turn_id)\
        \ DELETE FROM harness.server_human_requests AS request\
        \ USING interrupted_turn AS turn\
        \ WHERE request.turn_id = turn.turn_id"
        (Encoders.param (Encoders.nonNullable Encoders.text))
        Decoders.noResult
        True

acquireServerTurnOwnerReleaseFenceStatement :: Statement Text Bool
acquireServerTurnOwnerReleaseFenceStatement =
    mkStatement
        "SELECT pg_try_advisory_xact_lock(\
        \ hashtextextended(\
        \   'haskell-agent:server-turn-owner-action:' || $1::text,\
        \   0))"
        (Encoders.param (Encoders.nonNullable Encoders.text))
        (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
        True

deleteServerTurnOwnerStatement :: Statement Text ()
deleteServerTurnOwnerStatement =
    mkStatement
        "DELETE FROM harness.server_turn_owners\
        \ WHERE instance_id = $1::uuid"
        (Encoders.param (Encoders.nonNullable Encoders.text))
        Decoders.noResult
        True

listServerTurnsStatement ::
    Statement
        (ServerTurnBoundary, Maybe Text)
        (Vector.Vector StoredServerTurn)
listServerTurnsStatement =
    mkStatement
        ( "SELECT "
            <> serverTurnColumns
            <> "\
               \ FROM harness.server_turns server_turn\
               \ WHERE tenant_id = $1\
               \ AND gateway_identity IS NOT DISTINCT FROM $2\
               \ AND ($3::text IS NULL OR session_key = $3)\
               \ ORDER BY server_turn.created_at DESC, server_turn.turn_id DESC\
               \ LIMIT 200"
        )
        ( boundaryEncoder fst
            <> (snd >$< Encoders.param (Encoders.nullable Encoders.text))
        )
        (Decoders.rowVector serverTurnDecoder)
        True
