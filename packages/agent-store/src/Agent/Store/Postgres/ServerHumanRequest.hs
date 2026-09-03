{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Durable human-input handoff for externally submitted turns.
module Agent.Store.Postgres.ServerHumanRequest
    ( StoredServerHumanRequest (..)
    , CreateServerHumanRequest (..)
    , ServerHumanResponse (..)
    , ServerHumanRequestResolution (..)
    , createServerHumanRequest
    , listServerHumanRequests
    , listServerHumanRequestsForTurn
    , resolveServerHumanRequest
    , loadServerHumanResponse
    , deleteServerHumanRequest
    )
where

import Agent.Store.Postgres.Connection (StorePool, withSession)
import Agent.Store.Postgres.Hasql (mkStatement)
import Agent.Store.Postgres.ServerTurn (ServerTurnBoundary (..))
import Agent.Store.Types (StoreError)
import Data.Functor.Contravariant ((>$<))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import qualified Data.Vector as Vector
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement)
import qualified Hasql.Transaction as Transaction
import qualified Hasql.Transaction.Sessions as Transactions

data StoredServerHumanRequest = StoredServerHumanRequest
    { storedServerHumanRequestId :: !Text
    , storedServerHumanRequestTurnId :: !Text
    , storedServerHumanRequestBoundary :: !ServerTurnBoundary
    , storedServerHumanRequestSessionId :: !Text
    , storedServerHumanRequestKind :: !Text
    , storedServerHumanRequestPrompt :: !Text
    , storedServerHumanRequestOptionsJson :: !Text
    , storedServerHumanRequestCreatedAt :: !UTCTime
    , storedServerHumanRequestResponseDecision :: !(Maybe Text)
    , storedServerHumanRequestResponseValue :: !(Maybe Text)
    , storedServerHumanRequestResolvedAt :: !(Maybe UTCTime)
    }
    deriving (Eq, Show)

data CreateServerHumanRequest = CreateServerHumanRequest
    { createServerHumanRequestId :: !Text
    , createServerHumanRequestTurnId :: !Text
    , createServerHumanRequestBoundary :: !ServerTurnBoundary
    , createServerHumanRequestOwnerInstanceId :: !Text
    , createServerHumanRequestKind :: !Text
    , createServerHumanRequestPrompt :: !Text
    , createServerHumanRequestOptionsJson :: !Text
    , createServerHumanRequestCreatedAt :: !UTCTime
    }
    deriving (Eq, Show)

data ServerHumanResponse = ServerHumanResponse
    { serverHumanResponseDecision :: !Text
    , serverHumanResponseValue :: !(Maybe Text)
    }
    deriving (Eq, Show)

data ServerHumanRequestResolution
    = ServerHumanRequestResolved !StoredServerHumanRequest
    | ServerHumanRequestNotFound
    | ServerHumanRequestAlreadyResolved
    | ServerHumanRequestInvalidDecision
    deriving (Eq, Show)

createServerHumanRequest ::
    StorePool ->
    CreateServerHumanRequest ->
    IO (Either StoreError Bool)
createServerHumanRequest pool request =
    withSession pool $
        Transactions.transaction Transactions.ReadCommitted Transactions.Write $
            Transaction.statement request createServerHumanRequestStatement

listServerHumanRequests ::
    StorePool ->
    ServerTurnBoundary ->
    IO (Either StoreError [StoredServerHumanRequest])
listServerHumanRequests pool boundary =
    listServerHumanRequestsMatching pool boundary Nothing

listServerHumanRequestsForTurn ::
    StorePool ->
    ServerTurnBoundary ->
    Text ->
    IO (Either StoreError [StoredServerHumanRequest])
listServerHumanRequestsForTurn pool boundary turnId =
    listServerHumanRequestsMatching pool boundary (Just turnId)

listServerHumanRequestsMatching ::
    StorePool ->
    ServerTurnBoundary ->
    Maybe Text ->
    IO (Either StoreError [StoredServerHumanRequest])
listServerHumanRequestsMatching pool boundary turnId =
    withSession pool $
        Transactions.transaction Transactions.ReadCommitted Transactions.Read do
            Vector.toList
                <$> Transaction.statement
                    (boundary, turnId)
                    listServerHumanRequestsStatement

resolveServerHumanRequest ::
    StorePool ->
    ServerTurnBoundary ->
    Text ->
    ServerHumanResponse ->
    UTCTime ->
    IO (Either StoreError ServerHumanRequestResolution)
resolveServerHumanRequest pool boundary requestId response resolvedAt =
    withSession pool $
        Transactions.transaction Transactions.ReadCommitted Transactions.Write do
            Transaction.statement
                (boundary, requestId)
                loadServerHumanRequestForUpdateStatement
                >>= \case
                    Nothing ->
                        pure ServerHumanRequestNotFound
                    Just request
                        | request.storedServerHumanRequestResolvedAt
                            /= Nothing ->
                            pure ServerHumanRequestAlreadyResolved
                        | otherwise -> do
                            valid <-
                                Transaction.statement
                                    ( response.serverHumanResponseDecision
                                    , request.storedServerHumanRequestOptionsJson
                                    )
                                    validServerHumanDecisionStatement
                            if not valid
                                then pure ServerHumanRequestInvalidDecision
                                else do
                                    changed <-
                                        Transaction.statement
                                            (requestId, response, resolvedAt)
                                            resolveServerHumanRequestStatement
                                    pure
                                        ( if changed
                                            then
                                                ServerHumanRequestResolved
                                                    ( resolvedRequest
                                                        response
                                                        resolvedAt
                                                        request
                                                    )
                                            else
                                                ServerHumanRequestAlreadyResolved
                                        )

loadServerHumanResponse ::
    StorePool ->
    ServerTurnBoundary ->
    Text ->
    Text ->
    Text ->
    IO (Either StoreError (Maybe ServerHumanResponse))
loadServerHumanResponse pool boundary ownerInstanceId turnId requestId =
    withSession pool $
        Transactions.transaction Transactions.ReadCommitted Transactions.Read $
            Transaction.statement
                (boundary, ownerInstanceId, turnId, requestId)
                loadServerHumanResponseStatement

deleteServerHumanRequest ::
    StorePool ->
    ServerTurnBoundary ->
    Text ->
    Text ->
    Text ->
    IO (Either StoreError ())
deleteServerHumanRequest pool boundary ownerInstanceId turnId requestId =
    withSession pool $
        Transactions.transaction Transactions.ReadCommitted Transactions.Write $
            Transaction.statement
                (boundary, ownerInstanceId, turnId, requestId)
                deleteServerHumanRequestStatement

resolvedRequest ::
    ServerHumanResponse ->
    UTCTime ->
    StoredServerHumanRequest ->
    StoredServerHumanRequest
resolvedRequest response resolvedAt request =
    request
        { storedServerHumanRequestResponseDecision =
            Just response.serverHumanResponseDecision
        , storedServerHumanRequestResponseValue =
            response.serverHumanResponseValue
        , storedServerHumanRequestResolvedAt = Just resolvedAt
        }

data RawServerHumanRequest = RawServerHumanRequest
    { rawRequestId :: !Text
    , rawTurnId :: !Text
    , rawTenantId :: !Text
    , rawGatewayIdentity :: !(Maybe Text)
    , rawSessionId :: !Text
    , rawKind :: !Text
    , rawPrompt :: !Text
    , rawOptionsJson :: !Text
    , rawCreatedAt :: !UTCTime
    , rawResponseDecision :: !(Maybe Text)
    , rawResponseValue :: !(Maybe Text)
    , rawResolvedAt :: !(Maybe UTCTime)
    }

decodeServerHumanRequest ::
    RawServerHumanRequest ->
    StoredServerHumanRequest
decodeServerHumanRequest raw =
    StoredServerHumanRequest
        { storedServerHumanRequestId = raw.rawRequestId
        , storedServerHumanRequestTurnId = raw.rawTurnId
        , storedServerHumanRequestBoundary =
            ServerTurnBoundary
                { serverTurnTenantId = raw.rawTenantId
                , serverTurnGatewayIdentity = raw.rawGatewayIdentity
                }
        , storedServerHumanRequestSessionId = raw.rawSessionId
        , storedServerHumanRequestKind = raw.rawKind
        , storedServerHumanRequestPrompt = raw.rawPrompt
        , storedServerHumanRequestOptionsJson = raw.rawOptionsJson
        , storedServerHumanRequestCreatedAt = raw.rawCreatedAt
        , storedServerHumanRequestResponseDecision = raw.rawResponseDecision
        , storedServerHumanRequestResponseValue = raw.rawResponseValue
        , storedServerHumanRequestResolvedAt = raw.rawResolvedAt
        }

serverHumanRequestColumns :: Text
serverHumanRequestColumns =
    "server_request.request_id::text, server_request.turn_id::text,\
    \ server_turn.tenant_id, server_turn.gateway_identity,\
    \ server_turn.session_key, server_request.kind, server_request.prompt,\
    \ server_request.options_json::text, server_request.created_at,\
    \ server_request.response_decision, server_request.response_value,\
    \ server_request.resolved_at"

serverHumanRequestDecoder :: Decoders.Row StoredServerHumanRequest
serverHumanRequestDecoder =
    decodeServerHumanRequest
        <$> ( RawServerHumanRequest
                <$> Decoders.column (Decoders.nonNullable Decoders.text)
                <*> Decoders.column (Decoders.nonNullable Decoders.text)
                <*> Decoders.column (Decoders.nonNullable Decoders.text)
                <*> Decoders.column (Decoders.nullable Decoders.text)
                <*> Decoders.column (Decoders.nonNullable Decoders.text)
                <*> Decoders.column (Decoders.nonNullable Decoders.text)
                <*> Decoders.column (Decoders.nonNullable Decoders.text)
                <*> Decoders.column (Decoders.nonNullable Decoders.text)
                <*> Decoders.column (Decoders.nonNullable Decoders.timestamptz)
                <*> Decoders.column (Decoders.nullable Decoders.text)
                <*> Decoders.column (Decoders.nullable Decoders.text)
                <*> Decoders.column (Decoders.nullable Decoders.timestamptz)
            )

boundaryEncoder ::
    (input -> ServerTurnBoundary) ->
    Encoders.Params input
boundaryEncoder select =
    ( ((.serverTurnTenantId) . select)
        >$< Encoders.param (Encoders.nonNullable Encoders.text)
    )
        <> ( ((.serverTurnGatewayIdentity) . select)
                >$< Encoders.param (Encoders.nullable Encoders.text)
           )

createServerHumanRequestStatement ::
    Statement CreateServerHumanRequest Bool
createServerHumanRequestStatement =
    mkStatement
        "WITH owned_turn AS (\
        \ SELECT server_turn.turn_id\
        \ FROM harness.server_turns server_turn\
        \ WHERE server_turn.turn_id = $2::uuid\
        \ AND server_turn.tenant_id = $3\
        \ AND server_turn.gateway_identity IS NOT DISTINCT FROM $4\
        \ AND server_turn.owner_instance_id = $5::uuid\
        \ AND server_turn.status IN ('queued', 'running')\
        \ FOR UPDATE\
        \ ), inserted AS (\
        \ INSERT INTO harness.server_human_requests\
        \ (request_id, turn_id, kind, prompt, options_json, created_at)\
        \ SELECT $1::uuid, owned_turn.turn_id, $6, $7, $8::jsonb, $9\
        \ FROM owned_turn\
        \ ON CONFLICT DO NOTHING\
        \ RETURNING 1)\
        \ SELECT EXISTS (SELECT 1 FROM inserted)"
        ( ( (.createServerHumanRequestId)
                >$< Encoders.param (Encoders.nonNullable Encoders.text)
          )
            <> ( (.createServerHumanRequestTurnId)
                    >$< Encoders.param (Encoders.nonNullable Encoders.text)
               )
            <> boundaryEncoder (.createServerHumanRequestBoundary)
            <> ( (.createServerHumanRequestOwnerInstanceId)
                    >$< Encoders.param (Encoders.nonNullable Encoders.text)
               )
            <> ( (.createServerHumanRequestKind)
                    >$< Encoders.param (Encoders.nonNullable Encoders.text)
               )
            <> ( (.createServerHumanRequestPrompt)
                    >$< Encoders.param (Encoders.nonNullable Encoders.text)
               )
            <> ( (.createServerHumanRequestOptionsJson)
                    >$< Encoders.param (Encoders.nonNullable Encoders.text)
               )
            <> ( (.createServerHumanRequestCreatedAt)
                    >$< Encoders.param (Encoders.nonNullable Encoders.timestamptz)
               )
        )
        (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
        True

listServerHumanRequestsStatement ::
    Statement
        (ServerTurnBoundary, Maybe Text)
        (Vector.Vector StoredServerHumanRequest)
listServerHumanRequestsStatement =
    mkStatement
        ( "SELECT "
            <> serverHumanRequestColumns
            <> "\
               \ FROM harness.server_human_requests server_request\
               \ JOIN harness.server_turns server_turn\
               \ ON server_turn.turn_id = server_request.turn_id\
               \ WHERE server_turn.tenant_id = $1\
               \ AND server_turn.gateway_identity IS NOT DISTINCT FROM $2\
               \ AND server_turn.status IN ('queued', 'running')\
               \ AND server_request.resolved_at IS NULL\
               \ AND ($3::uuid IS NULL OR server_request.turn_id = $3::uuid)\
               \ ORDER BY server_request.created_at DESC,\
               \ server_request.request_id DESC\
               \ LIMIT 200"
        )
        ( boundaryEncoder fst
            <> ( snd
                    >$< Encoders.param (Encoders.nullable Encoders.text)
               )
        )
        (Decoders.rowVector serverHumanRequestDecoder)
        True

loadServerHumanRequestForUpdateStatement ::
    Statement
        (ServerTurnBoundary, Text)
        (Maybe StoredServerHumanRequest)
loadServerHumanRequestForUpdateStatement =
    mkStatement
        ( "SELECT "
            <> serverHumanRequestColumns
            <> "\
               \ FROM harness.server_human_requests server_request\
               \ JOIN harness.server_turns server_turn\
               \ ON server_turn.turn_id = server_request.turn_id\
               \ WHERE server_turn.tenant_id = $1\
               \ AND server_turn.gateway_identity IS NOT DISTINCT FROM $2\
               \ AND server_request.request_id = $3::uuid\
               \ AND server_turn.status IN ('queued', 'running')\
               \ FOR UPDATE OF server_request, server_turn"
        )
        ( boundaryEncoder fst
            <> (snd >$< Encoders.param (Encoders.nonNullable Encoders.text))
        )
        (Decoders.rowMaybe serverHumanRequestDecoder)
        True

validServerHumanDecisionStatement ::
    Statement (Text, Text) Bool
validServerHumanDecisionStatement =
    mkStatement
        "SELECT jsonb_array_length($2::jsonb) = 0 OR EXISTS (\
        \ SELECT 1 FROM jsonb_array_elements_text($2::jsonb) allowed(value)\
        \ WHERE allowed.value = $1)"
        ( (fst >$< Encoders.param (Encoders.nonNullable Encoders.text))
            <> (snd >$< Encoders.param (Encoders.nonNullable Encoders.text))
        )
        (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
        True

resolveServerHumanRequestStatement ::
    Statement (Text, ServerHumanResponse, UTCTime) Bool
resolveServerHumanRequestStatement =
    mkStatement
        "WITH changed AS (\
        \ UPDATE harness.server_human_requests\
        \ SET response_decision = $2, response_value = $3,\
        \ resolved_at = $4\
        \ WHERE request_id = $1::uuid AND resolved_at IS NULL\
        \ RETURNING 1)\
        \ SELECT EXISTS (SELECT 1 FROM changed)"
        ( ( (\(requestId, _, _) -> requestId)
                >$< Encoders.param (Encoders.nonNullable Encoders.text)
          )
            <> ( ( (.serverHumanResponseDecision)
                    . (\(_, response, _) -> response)
                 )
                    >$< Encoders.param (Encoders.nonNullable Encoders.text)
               )
            <> ( ( (.serverHumanResponseValue)
                    . (\(_, response, _) -> response)
                 )
                    >$< Encoders.param (Encoders.nullable Encoders.text)
               )
            <> ( (\(_, _, resolvedAt) -> resolvedAt)
                    >$< Encoders.param (Encoders.nonNullable Encoders.timestamptz)
               )
        )
        (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
        True

loadServerHumanResponseStatement ::
    Statement
        (ServerTurnBoundary, Text, Text, Text)
        (Maybe ServerHumanResponse)
loadServerHumanResponseStatement =
    mkStatement
        "SELECT server_request.response_decision,\
        \ server_request.response_value\
        \ FROM harness.server_human_requests server_request\
        \ JOIN harness.server_turns server_turn\
        \ ON server_turn.turn_id = server_request.turn_id\
        \ WHERE server_turn.tenant_id = $1\
        \ AND server_turn.gateway_identity IS NOT DISTINCT FROM $2\
        \ AND server_turn.owner_instance_id = $3::uuid\
        \ AND server_turn.turn_id = $4::uuid\
        \ AND server_request.request_id = $5::uuid\
        \ AND server_turn.status IN ('queued', 'running')\
        \ AND server_request.resolved_at IS NOT NULL"
        ( boundaryEncoder (\(boundary, _, _, _) -> boundary)
            <> ( (\(_, ownerInstanceId, _, _) -> ownerInstanceId)
                    >$< Encoders.param (Encoders.nonNullable Encoders.text)
               )
            <> ( (\(_, _, turnId, _) -> turnId)
                    >$< Encoders.param (Encoders.nonNullable Encoders.text)
               )
            <> ( (\(_, _, _, requestId) -> requestId)
                    >$< Encoders.param (Encoders.nonNullable Encoders.text)
               )
        )
        ( Decoders.rowMaybe
            ( ServerHumanResponse
                <$> Decoders.column (Decoders.nonNullable Decoders.text)
                <*> Decoders.column (Decoders.nullable Decoders.text)
            )
        )
        True

deleteServerHumanRequestStatement ::
    Statement (ServerTurnBoundary, Text, Text, Text) ()
deleteServerHumanRequestStatement =
    mkStatement
        "DELETE FROM harness.server_human_requests server_request\
        \ USING harness.server_turns server_turn\
        \ WHERE server_turn.turn_id = server_request.turn_id\
        \ AND server_turn.tenant_id = $1\
        \ AND server_turn.gateway_identity IS NOT DISTINCT FROM $2\
        \ AND server_turn.owner_instance_id = $3::uuid\
        \ AND server_turn.turn_id = $4::uuid\
        \ AND server_request.request_id = $5::uuid\
        \ AND server_request.resolved_at IS NULL"
        ( boundaryEncoder (\(boundary, _, _, _) -> boundary)
            <> ( (\(_, ownerInstanceId, _, _) -> ownerInstanceId)
                    >$< Encoders.param (Encoders.nonNullable Encoders.text)
               )
            <> ( (\(_, _, turnId, _) -> turnId)
                    >$< Encoders.param (Encoders.nonNullable Encoders.text)
               )
            <> ( (\(_, _, _, requestId) -> requestId)
                    >$< Encoders.param (Encoders.nonNullable Encoders.text)
               )
        )
        Decoders.noResult
        True
