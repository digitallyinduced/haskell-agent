{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Immutable, restart-safe interactions that can be answered by any client.
--
-- Requests, their first accepted response, and the transcript turn that
-- consumed that response are separate facts.  This keeps the request and
-- winning response immutable while allowing another process to answer an
-- interaction that the originating process is no longer presenting.
module Agent.Store.Postgres.Interaction
    ( InteractionOrigin(..)
    , InteractionRequest(..)
    , SessionInteraction(..)
    , InteractionResolution(..)
    , InteractionResolutionRequest(..)
    , InteractionDelivery(..)
    , InteractionDeliveryRequest(..)
    , InteractionDeliveryIntent(..)
    , InteractionDeliveryPreparation(..)
    , InteractionPublishResult(..)
    , InteractionResolveResult(..)
    , InteractionDeliveryResult(..)
    , interactionSchemaStatements
    , interactionRuntimeGrantStatements
    , interactionMigrationStatements
    , publishSessionInteraction
    , loadSessionInteraction
    , loadSessionInteractionByRequestKey
    , listOpenSessionInteractions
    , listUndeliveredSessionInteractions
    , resolveSessionInteraction
    , markSessionInteractionDelivered
    , prepareSessionInteractionDeliveryTransaction
    , commitSessionInteractionDeliveryTransaction
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Functor.Contravariant ((>$<))
import Data.Foldable (traverse_)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Time.Clock (UTCTime)
import qualified Data.UUID.Types as UUID
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Session as HasqlSession
import Hasql.Statement (Statement)
import qualified Hasql.Transaction as Transaction
import qualified Hasql.Transaction.Sessions as Transactions

import Agent.Store.Postgres.Connection
    ( StorePool
    , withSession
    )
import Agent.Store.Postgres.Hasql (mkStatement)
import Agent.Store.Types (StoreError(..))

data InteractionOrigin = InteractionOrigin
    { interactionOriginToolName :: !Text
    , interactionOriginCallId :: !Text
    }
    deriving (Eq, Show)

-- | The delivery fields known before the containing session turn receives its
-- durable index.  'appendSessionTurnIndexedAndDeliver' supplies the session key
-- and allocated turn index inside the same transaction.
data InteractionDeliveryIntent = InteractionDeliveryIntent
    { interactionDeliveryIntentInteractionId :: !Text
    , interactionDeliveryIntentKind :: !Text
    , interactionDeliveryIntentTurnFingerprint :: !(Maybe Text)
    , interactionDeliveryIntentDeliveredAt :: !UTCTime
    }
    deriving (Eq, Show)

-- | Result of locking and checking an interaction before an atomic turn
-- append.  A blocked result must not append a second transcript turn.
data InteractionDeliveryPreparation
    = InteractionDeliveryReady
    | InteractionDeliveryBlocked !InteractionDeliveryResult
    deriving (Eq, Show)

data InteractionRequest = InteractionRequest
    { interactionRequestSessionKey :: !Text
    , interactionRequestKey :: !Text
    , interactionRequestKind :: !Text
    , interactionRequestPayloadVersion :: !Int32
    , interactionRequestPayload :: !Text
    , interactionRequestOrigin :: !(Maybe InteractionOrigin)
    , interactionRequestCreatedAt :: !UTCTime
    }
    deriving (Eq, Show)

data SessionInteraction = SessionInteraction
    { sessionInteractionId :: !Text
    , sessionInteractionSessionKey :: !Text
    , sessionInteractionRequestKey :: !Text
    , sessionInteractionKind :: !Text
    , sessionInteractionPayloadVersion :: !Int32
    , sessionInteractionPayload :: !Text
    , sessionInteractionOrigin :: !(Maybe InteractionOrigin)
    , sessionInteractionCreatedAt :: !UTCTime
    , sessionInteractionResolution :: !(Maybe InteractionResolution)
    , sessionInteractionDelivery :: !(Maybe InteractionDelivery)
    }
    deriving (Eq, Show)

data InteractionResolution = InteractionResolution
    { interactionResolutionInteractionId :: !Text
    , interactionResolutionPayloadVersion :: !Int32
    , interactionResolutionPayload :: !Text
    , interactionResolutionResponder :: !Text
    , interactionResolutionResolvedAt :: !UTCTime
    }
    deriving (Eq, Show)

data InteractionResolutionRequest = InteractionResolutionRequest
    { interactionResolutionRequestSessionKey :: !Text
    , interactionResolutionRequestInteractionId :: !Text
    , interactionResolutionRequestPayloadVersion :: !Int32
    , interactionResolutionRequestPayload :: !Text
    , interactionResolutionRequestResponder :: !Text
    , interactionResolutionRequestResolvedAt :: !UTCTime
    }
    deriving (Eq, Show)

data InteractionDelivery = InteractionDelivery
    { interactionDeliveryInteractionId :: !Text
    , interactionDeliveryKind :: !Text
    , interactionDeliveryTurnIndex :: !Int64
    , interactionDeliveryTurnFingerprint :: !(Maybe Text)
    , interactionDeliveryDeliveredAt :: !UTCTime
    }
    deriving (Eq, Show)

data InteractionDeliveryRequest = InteractionDeliveryRequest
    { interactionDeliveryRequestSessionKey :: !Text
    , interactionDeliveryRequestInteractionId :: !Text
    , interactionDeliveryRequestKind :: !Text
    , interactionDeliveryRequestTurnIndex :: !Int64
    , interactionDeliveryRequestTurnFingerprint :: !(Maybe Text)
    , interactionDeliveryRequestDeliveredAt :: !UTCTime
    }
    deriving (Eq, Show)

data InteractionPublishResult
    = InteractionPublishSessionNotFound
    | InteractionPublishObserved
        { interactionPublishInserted :: !Bool
        , interactionPublishValue :: !SessionInteraction
        }
    deriving (Eq, Show)

data InteractionResolveResult
    = InteractionResolveNotFound
    | InteractionResolveObserved
        { interactionResolveWon :: !Bool
        , interactionResolveValue :: !InteractionResolution
        }
    deriving (Eq, Show)

data InteractionDeliveryResult
    = InteractionDeliveryNotFound
    | InteractionDeliveryUnresolved
    | InteractionDeliveryTurnNotFound
    | InteractionDeliveryObserved
        { interactionDeliveryInserted :: !Bool
        , interactionDeliveryValue :: !InteractionDelivery
        }
    deriving (Eq, Show)

-- | Fresh-schema DDL. The durable-interaction migration applies the same
-- idempotent statements to
-- existing stores.
interactionSchemaStatements :: [ByteString]
interactionSchemaStatements =
    [ "CREATE TABLE IF NOT EXISTS harness.session_interactions (\
      \ interaction_id uuid PRIMARY KEY DEFAULT pg_catalog.uuidv7(),\
      \ session_id uuid NOT NULL\
      \   REFERENCES harness.sessions(session_id) ON DELETE RESTRICT,\
      \ request_key text NOT NULL\
      \   CHECK (length(btrim(request_key)) > 0\
      \     AND length(request_key) <= 512),\
      \ interaction_kind text NOT NULL\
      \   CHECK (interaction_kind ~ '^[a-z][a-z0-9_.-]{0,79}$'),\
      \ payload_version integer NOT NULL CHECK (payload_version > 0),\
      \ request_payload_text text NOT NULL\
      \   CHECK (length(request_payload_text) > 0\
      \     AND octet_length(request_payload_text) <= 4194304),\
      \ origin_tool_name text,\
      \ origin_call_id text,\
      \ created_at timestamptz NOT NULL,\
      \ UNIQUE (session_id, request_key),\
      \ UNIQUE (interaction_id, session_id),\
      \ CHECK (\
      \   (origin_tool_name IS NULL AND origin_call_id IS NULL)\
      \   OR\
      \   (origin_tool_name IS NOT NULL\
      \     AND length(btrim(origin_tool_name)) > 0\
      \     AND length(origin_tool_name) <= 256\
      \     AND origin_call_id IS NOT NULL\
      \     AND length(btrim(origin_call_id)) > 0\
      \     AND length(origin_call_id) <= 1024)\
      \ )\
      \ )"
    , "CREATE INDEX IF NOT EXISTS session_interactions_open_idx\
      \ ON harness.session_interactions\
      \ (session_id, interaction_kind, created_at, interaction_id)"
    , "CREATE TABLE IF NOT EXISTS\
      \ harness.session_interaction_resolutions (\
      \ interaction_id uuid PRIMARY KEY,\
      \ session_id uuid NOT NULL,\
      \ payload_version integer NOT NULL CHECK (payload_version > 0),\
      \ response_payload_text text NOT NULL\
      \   CHECK (length(response_payload_text) > 0\
      \     AND octet_length(response_payload_text) <= 4194304),\
      \ resolved_by text NOT NULL\
      \   CHECK (length(btrim(resolved_by)) > 0\
      \     AND length(resolved_by) <= 256),\
      \ resolved_at timestamptz NOT NULL,\
      \ UNIQUE (interaction_id, session_id),\
      \ FOREIGN KEY (interaction_id, session_id)\
      \   REFERENCES harness.session_interactions(interaction_id, session_id)\
      \   ON DELETE RESTRICT\
      \ )"
    , "CREATE INDEX IF NOT EXISTS session_interaction_resolutions_session_idx\
      \ ON harness.session_interaction_resolutions\
      \ (session_id, resolved_at, interaction_id)"
    , "CREATE TABLE IF NOT EXISTS harness.session_interaction_deliveries (\
      \ interaction_id uuid PRIMARY KEY,\
      \ session_id uuid NOT NULL,\
      \ delivery_kind text NOT NULL\
      \   CHECK (delivery_kind ~ '^[a-z][a-z0-9_.-]{0,79}$'),\
      \ turn_index bigint NOT NULL CHECK (turn_index >= 0),\
      \ turn_fingerprint text\
      \   CHECK (turn_fingerprint IS NULL\
      \     OR (length(turn_fingerprint) > 0\
      \       AND length(turn_fingerprint) <= 128)),\
      \ delivered_at timestamptz NOT NULL,\
      \ FOREIGN KEY (interaction_id, session_id)\
      \   REFERENCES\
      \     harness.session_interaction_resolutions(interaction_id, session_id)\
      \   ON DELETE RESTRICT,\
      \ FOREIGN KEY (session_id, turn_index)\
      \   REFERENCES harness.session_turns(session_id, turn_index)\
      \   ON DELETE RESTRICT\
      \ )"
    , "CREATE INDEX IF NOT EXISTS session_interaction_deliveries_session_idx\
      \ ON harness.session_interaction_deliveries\
      \ (session_id, delivered_at, interaction_id)"
    , "ALTER TABLE harness.session_interaction_deliveries\
      \ ADD COLUMN IF NOT EXISTS turn_fingerprint text"
    , "CREATE OR REPLACE FUNCTION\
      \ harness.reject_session_interaction_fact_mutation()\
      \ RETURNS trigger\
      \ LANGUAGE plpgsql\
      \ AS $$ BEGIN\
      \ RAISE EXCEPTION 'session interaction facts are immutable';\
      \ END $$"
    , "DROP TRIGGER IF EXISTS session_interactions_immutable\
      \ ON harness.session_interactions"
    , "CREATE TRIGGER session_interactions_immutable\
      \ BEFORE UPDATE OR DELETE ON harness.session_interactions\
      \ FOR EACH ROW EXECUTE FUNCTION\
      \ harness.reject_session_interaction_fact_mutation()"
    , "DROP TRIGGER IF EXISTS session_interaction_resolutions_immutable\
      \ ON harness.session_interaction_resolutions"
    , "CREATE TRIGGER session_interaction_resolutions_immutable\
      \ BEFORE UPDATE OR DELETE\
      \ ON harness.session_interaction_resolutions\
      \ FOR EACH ROW EXECUTE FUNCTION\
      \ harness.reject_session_interaction_fact_mutation()"
    , "DROP TRIGGER IF EXISTS session_interaction_deliveries_immutable\
      \ ON harness.session_interaction_deliveries"
    , "CREATE TRIGGER session_interaction_deliveries_immutable\
      \ BEFORE UPDATE OR DELETE ON harness.session_interaction_deliveries\
      \ FOR EACH ROW EXECUTE FUNCTION\
      \ harness.reject_session_interaction_fact_mutation()"
    ]

interactionRuntimeGrantStatements :: [ByteString]
interactionRuntimeGrantStatements =
    [ "GRANT SELECT ON harness.session_interactions TO ha_runtime"
    , "GRANT INSERT\
      \ (session_id, request_key, interaction_kind, payload_version,\
      \ request_payload_text, origin_tool_name, origin_call_id, created_at)\
      \ ON harness.session_interactions TO ha_runtime"
    , "GRANT SELECT\
      \ ON harness.session_interaction_resolutions TO ha_runtime"
    , "GRANT INSERT\
      \ (interaction_id, session_id, payload_version,\
      \ response_payload_text, resolved_by, resolved_at)\
      \ ON harness.session_interaction_resolutions TO ha_runtime"
    , "GRANT SELECT ON harness.session_interaction_deliveries TO ha_runtime"
    , "GRANT INSERT\
      \ (interaction_id, session_id, delivery_kind, turn_index,\
      \ turn_fingerprint, delivered_at)\
      \ ON harness.session_interaction_deliveries TO ha_runtime"
    ]

-- | Catch-up migration statements tolerate the intentionally partial legacy
-- schemas used by older targeted migrations.  A real typed session store has
-- both parent tables; fresh stores receive the unguarded DDL through
-- 'sessionSchemaStatements'.
interactionMigrationStatements :: [ByteString]
interactionMigrationStatements =
    fmap whenTypedSessionSchemaExists
        (interactionSchemaStatements <> interactionRuntimeGrantStatements)

whenTypedSessionSchemaExists :: ByteString -> ByteString
whenTypedSessionSchemaExists statement =
    ByteString.concat
        [ "DO $ha$\
          \ BEGIN\
          \   IF to_regclass('harness.sessions') IS NOT NULL\
          \     AND to_regclass('harness.session_turns') IS NOT NULL THEN\
          \     EXECUTE $interaction$"
        , statement
        , "$interaction$;\
          \   END IF;\
          \ END\
          \ $ha$"
        ]

publishSessionInteraction
    :: StorePool
    -> InteractionRequest
    -> IO (Either StoreError InteractionPublishResult)
publishSessionInteraction pool request =
    case validateInteractionRequest request of
        Left err -> pure (Left (StoreDataError err))
        Right () ->
            runInteractionWrite pool do
                _ <- Transaction.statement
                    (interactionRequestLockKey request)
                    interactionLockStatement
                active <- Transaction.statement
                    request.interactionRequestSessionKey
                    activeSessionExistsStatement
                if not active
                    then pure (Right InteractionPublishSessionNotFound)
                    else do
                        existing <- Transaction.statement
                            ( request.interactionRequestSessionKey
                            , request.interactionRequestKey
                            )
                            loadInteractionByRequestKeyStatement
                        case existing of
                            Just row -> case decodeInteractionRow row of
                                Left err -> pure (Left err)
                                Right interaction
                                    | sameRequest request interaction ->
                                        pure
                                            (Right
                                                InteractionPublishObserved
                                                    { interactionPublishInserted =
                                                        False
                                                    , interactionPublishValue =
                                                        interaction
                                                    })
                                    | otherwise ->
                                        pure
                                            (Left
                                                "interaction request key already exists with different immutable request data")
                            Nothing -> do
                                inserted <- Transaction.statement
                                    request
                                    insertInteractionStatement
                                stored <- Transaction.statement
                                    ( request.interactionRequestSessionKey
                                    , request.interactionRequestKey
                                    )
                                    loadInteractionByRequestKeyStatement
                                case stored of
                                    Nothing ->
                                        pure
                                            (Left
                                                "published interaction request could not be reloaded")
                                    Just row -> case decodeInteractionRow row of
                                        Left err -> pure (Left err)
                                        Right interaction
                                            | sameRequest request interaction ->
                                                pure
                                                    (Right
                                                        InteractionPublishObserved
                                                            { interactionPublishInserted =
                                                                inserted
                                                            , interactionPublishValue =
                                                                interaction
                                                            })
                                            | otherwise ->
                                                pure
                                                    (Left
                                                        "interaction request key was concurrently published with different immutable request data")

loadSessionInteraction
    :: StorePool
    -> Text
    -> Text
    -> IO (Either StoreError (Maybe SessionInteraction))
loadSessionInteraction pool sessionKey interactionId =
    case validateLookup sessionKey interactionId of
        Left err -> pure (Left (StoreDataError err))
        Right () ->
            withSession pool
                (HasqlSession.statement
                    (sessionKey, interactionId)
                    loadInteractionByIdStatement)
                >>= pure . decodeMaybeInteractionResult

loadSessionInteractionByRequestKey
    :: StorePool
    -> Text
    -> Text
    -> IO (Either StoreError (Maybe SessionInteraction))
loadSessionInteractionByRequestKey pool sessionKey requestKey =
    case validateSessionAndRequestKey sessionKey requestKey of
        Left err -> pure (Left (StoreDataError err))
        Right () ->
            withSession pool
                (HasqlSession.statement
                    (sessionKey, requestKey)
                    loadInteractionByRequestKeyStatement)
                >>= pure . decodeMaybeInteractionResult

listOpenSessionInteractions
    :: StorePool
    -> Text
    -> IO (Either StoreError [SessionInteraction])
listOpenSessionInteractions pool sessionKey =
    case validateSessionKey sessionKey of
        Left err -> pure (Left (StoreDataError err))
        Right () ->
            withSession pool
                (HasqlSession.statement
                    sessionKey
                    listOpenInteractionsStatement)
                >>= pure . decodeInteractionListResult

listUndeliveredSessionInteractions
    :: StorePool
    -> Text
    -> IO (Either StoreError [SessionInteraction])
listUndeliveredSessionInteractions pool sessionKey =
    case validateSessionKey sessionKey of
        Left err -> pure (Left (StoreDataError err))
        Right () ->
            withSession pool
                (HasqlSession.statement
                    sessionKey
                    listUndeliveredInteractionsStatement)
                >>= pure . decodeInteractionListResult

resolveSessionInteraction
    :: StorePool
    -> InteractionResolutionRequest
    -> IO (Either StoreError InteractionResolveResult)
resolveSessionInteraction pool request =
    case validateResolutionRequest request of
        Left err -> pure (Left (StoreDataError err))
        Right () ->
            runInteractionWrite pool do
                _ <- Transaction.statement
                    (interactionResolutionLockKey request)
                    interactionLockStatement
                active <- Transaction.statement
                    request.interactionResolutionRequestSessionKey
                    activeSessionExistsStatement
                if not active
                    then pure (Right InteractionResolveNotFound)
                    else do
                        stored <- Transaction.statement
                            ( request.interactionResolutionRequestSessionKey
                            , request.interactionResolutionRequestInteractionId
                            )
                            loadInteractionByIdStatement
                        case stored of
                            Nothing -> pure (Right InteractionResolveNotFound)
                            Just row -> case decodeInteractionRow row of
                                Left err -> pure (Left err)
                                Right interaction ->
                                    case interaction.sessionInteractionResolution of
                                        Just winner ->
                                            pure
                                                (Right
                                                    InteractionResolveObserved
                                                        { interactionResolveWon =
                                                            False
                                                        , interactionResolveValue =
                                                            winner
                                                        })
                                        Nothing -> do
                                            inserted <- Transaction.statement
                                                request
                                                insertResolutionStatement
                                            case inserted of
                                                Nothing -> do
                                                    observed <-
                                                        Transaction.statement
                                                            ( request.interactionResolutionRequestSessionKey
                                                            , request.interactionResolutionRequestInteractionId
                                                            )
                                                            loadInteractionByIdStatement
                                                    case observed of
                                                        Nothing ->
                                                            pure
                                                                (Right
                                                                    InteractionResolveNotFound)
                                                        Just observedRow ->
                                                            case
                                                                decodeInteractionRow
                                                                    observedRow
                                                            of
                                                                Left err ->
                                                                    pure (Left err)
                                                                Right observedInteraction ->
                                                                    case
                                                                        observedInteraction.sessionInteractionResolution
                                                                    of
                                                                        Nothing ->
                                                                            pure
                                                                                (Left
                                                                                    "interaction resolution conflict did not expose a winner")
                                                                        Just winner ->
                                                                            pure
                                                                                (Right
                                                                                    InteractionResolveObserved
                                                                                        { interactionResolveWon =
                                                                                            False
                                                                                        , interactionResolveValue =
                                                                                            winner
                                                                                        })
                                                Just winner ->
                                                    pure
                                                        (Right
                                                            InteractionResolveObserved
                                                                { interactionResolveWon =
                                                                    True
                                                                , interactionResolveValue =
                                                                    winner
                                                                })

markSessionInteractionDelivered
    :: StorePool
    -> InteractionDeliveryRequest
    -> IO (Either StoreError InteractionDeliveryResult)
markSessionInteractionDelivered pool request =
    case validateDeliveryRequest request of
        Left err -> pure (Left (StoreDataError err))
        Right () ->
            runInteractionWrite pool do
                let intent = deliveryIntentFromRequest request
                prepared <-
                    prepareSessionInteractionDeliveryTransaction
                        request.interactionDeliveryRequestSessionKey
                        intent
                case prepared of
                    Left err -> pure (Left err)
                    Right (InteractionDeliveryBlocked result) ->
                        pure (Right result)
                    Right InteractionDeliveryReady -> do
                        turnExists <- Transaction.statement
                            ( request.interactionDeliveryRequestSessionKey
                            , request.interactionDeliveryRequestTurnIndex
                            )
                            sessionTurnExistsStatement
                        if not turnExists
                            then
                                pure
                                    (Right
                                        InteractionDeliveryTurnNotFound)
                            else
                                commitSessionInteractionDeliveryTransaction
                                    request.interactionDeliveryRequestSessionKey
                                    request.interactionDeliveryRequestTurnIndex
                                    intent

-- | Lock and inspect an interaction in the caller's write transaction.
--
-- The lock spans the caller's subsequent turn append and delivery insert.
-- Seeing an existing delivery returns it as a blocked result, which makes a
-- retry after an unknown commit outcome idempotent.
prepareSessionInteractionDeliveryTransaction
    :: Text
    -> InteractionDeliveryIntent
    -> Transaction.Transaction
        (Either Text InteractionDeliveryPreparation)
prepareSessionInteractionDeliveryTransaction sessionKey intent =
    case validateDeliveryIntent sessionKey intent of
        Left err -> pure (Left err)
        Right () -> do
            _ <- Transaction.statement
                (interactionDeliveryIntentLockKey sessionKey intent)
                interactionLockStatement
            active <- Transaction.statement
                sessionKey
                activeSessionExistsStatement
            if not active
                then
                    pure
                        (Right
                            (InteractionDeliveryBlocked
                                InteractionDeliveryNotFound))
                else do
                    stored <- Transaction.statement
                        (sessionKey, intent.interactionDeliveryIntentInteractionId)
                        loadInteractionByIdStatement
                    case stored of
                        Nothing ->
                            pure
                                (Right
                                    (InteractionDeliveryBlocked
                                        InteractionDeliveryNotFound))
                        Just row -> case decodeInteractionRow row of
                            Left err -> pure (Left err)
                            Right interaction ->
                                case
                                    ( interaction.sessionInteractionResolution
                                    , interaction.sessionInteractionDelivery
                                    ) of
                                    (Nothing, _) ->
                                        pure
                                            (Right
                                                (InteractionDeliveryBlocked
                                                    InteractionDeliveryUnresolved))
                                    (_, Just delivery) ->
                                        pure
                                            (Right
                                                (InteractionDeliveryBlocked
                                                    InteractionDeliveryObserved
                                                        { interactionDeliveryInserted =
                                                            False
                                                        , interactionDeliveryValue =
                                                            delivery
                                                        }))
                                    (Just _, Nothing) ->
                                        pure (Right InteractionDeliveryReady)

-- | Insert the immutable delivery fact after the caller has appended the
-- referenced turn in the same transaction and while holding the preparation
-- lock.
commitSessionInteractionDeliveryTransaction
    :: Text
    -> Int64
    -> InteractionDeliveryIntent
    -> Transaction.Transaction (Either Text InteractionDeliveryResult)
commitSessionInteractionDeliveryTransaction sessionKey turnIndex intent = do
    let request = InteractionDeliveryRequest
            { interactionDeliveryRequestSessionKey = sessionKey
            , interactionDeliveryRequestInteractionId =
                intent.interactionDeliveryIntentInteractionId
            , interactionDeliveryRequestKind =
                intent.interactionDeliveryIntentKind
            , interactionDeliveryRequestTurnIndex = turnIndex
            , interactionDeliveryRequestTurnFingerprint =
                intent.interactionDeliveryIntentTurnFingerprint
            , interactionDeliveryRequestDeliveredAt =
                intent.interactionDeliveryIntentDeliveredAt
            }
    case validateDeliveryRequest request of
        Left err -> pure (Left err)
        Right () -> do
            inserted <- Transaction.statement
                request
                insertDeliveryStatement
            case inserted of
                Just delivery ->
                    pure
                        (Right
                            InteractionDeliveryObserved
                                { interactionDeliveryInserted = True
                                , interactionDeliveryValue = delivery
                                })
                Nothing -> do
                    observed <- Transaction.statement
                        ( sessionKey
                        , intent.interactionDeliveryIntentInteractionId
                        )
                        loadInteractionByIdStatement
                    case observed of
                        Nothing ->
                            pure
                                (Right InteractionDeliveryNotFound)
                        Just row -> case decodeInteractionRow row of
                            Left err -> pure (Left err)
                            Right interaction ->
                                case interaction.sessionInteractionDelivery of
                                    Nothing ->
                                        pure
                                            (Left
                                                "interaction delivery insert did not expose a delivery")
                                    Just delivery ->
                                        pure
                                            (Right
                                                InteractionDeliveryObserved
                                                    { interactionDeliveryInserted =
                                                        False
                                                    , interactionDeliveryValue =
                                                        delivery
                                                    })

data InteractionRow = InteractionRow
    { rowInteractionId :: !Text
    , rowSessionKey :: !Text
    , rowRequestKey :: !Text
    , rowKind :: !Text
    , rowPayloadVersion :: !Int32
    , rowPayload :: !Text
    , rowOriginToolName :: !(Maybe Text)
    , rowOriginCallId :: !(Maybe Text)
    , rowCreatedAt :: !UTCTime
    , rowResolutionPayloadVersion :: !(Maybe Int32)
    , rowResolutionPayload :: !(Maybe Text)
    , rowResolvedBy :: !(Maybe Text)
    , rowResolvedAt :: !(Maybe UTCTime)
    , rowDeliveryKind :: !(Maybe Text)
    , rowDeliveryTurnIndex :: !(Maybe Int64)
    , rowDeliveryTurnFingerprint :: !(Maybe Text)
    , rowDeliveredAt :: !(Maybe UTCTime)
    }

data ResolutionRow = ResolutionRow
    { resolutionRowInteractionId :: !Text
    , resolutionRowPayloadVersion :: !Int32
    , resolutionRowPayload :: !Text
    , resolutionRowResponder :: !Text
    , resolutionRowResolvedAt :: !UTCTime
    }

data DeliveryRow = DeliveryRow
    { deliveryRowInteractionId :: !Text
    , deliveryRowKind :: !Text
    , deliveryRowTurnIndex :: !Int64
    , deliveryRowTurnFingerprint :: !(Maybe Text)
    , deliveryRowDeliveredAt :: !UTCTime
    }

interactionSelectColumns :: Text
interactionSelectColumns =
    "SELECT interaction.interaction_id::text,\
    \ session_row.session_key,\
    \ interaction.request_key,\
    \ interaction.interaction_kind,\
    \ interaction.payload_version,\
    \ interaction.request_payload_text,\
    \ interaction.origin_tool_name,\
    \ interaction.origin_call_id,\
    \ interaction.created_at,\
    \ resolution.payload_version,\
    \ resolution.response_payload_text,\
    \ resolution.resolved_by,\
    \ resolution.resolved_at,\
    \ delivery.delivery_kind,\
    \ delivery.turn_index,\
    \ delivery.turn_fingerprint,\
    \ delivery.delivered_at\
    \ FROM harness.session_interactions interaction\
    \ JOIN harness.sessions session_row\
    \   ON session_row.session_id = interaction.session_id\
    \ LEFT JOIN harness.session_interaction_resolutions resolution\
    \   ON resolution.interaction_id = interaction.interaction_id\
    \ LEFT JOIN harness.session_interaction_deliveries delivery\
    \   ON delivery.interaction_id = interaction.interaction_id"

loadInteractionByIdStatement
    :: Statement (Text, Text) (Maybe InteractionRow)
loadInteractionByIdStatement = mkStatement
    (interactionSelectColumns
        <> " WHERE session_row.session_key = $1\
           \ AND interaction.interaction_id = $2::uuid")
    textPairParams
    (Decoders.rowMaybe interactionRowDecoder)
    True

loadInteractionByRequestKeyStatement
    :: Statement (Text, Text) (Maybe InteractionRow)
loadInteractionByRequestKeyStatement = mkStatement
    (interactionSelectColumns
        <> " WHERE session_row.session_key = $1\
           \ AND interaction.request_key = $2")
    textPairParams
    (Decoders.rowMaybe interactionRowDecoder)
    True

listOpenInteractionsStatement :: Statement Text [InteractionRow]
listOpenInteractionsStatement = mkStatement
    (interactionSelectColumns
        <> " WHERE session_row.session_key = $1\
           \ AND resolution.interaction_id IS NULL\
           \ ORDER BY interaction.created_at, interaction.interaction_id")
    textParam
    (Decoders.rowList interactionRowDecoder)
    True

listUndeliveredInteractionsStatement :: Statement Text [InteractionRow]
listUndeliveredInteractionsStatement = mkStatement
    (interactionSelectColumns
        <> " WHERE session_row.session_key = $1\
           \ AND resolution.interaction_id IS NOT NULL\
           \ AND delivery.interaction_id IS NULL\
           \ ORDER BY resolution.resolved_at, interaction.interaction_id")
    textParam
    (Decoders.rowList interactionRowDecoder)
    True

activeSessionExistsStatement :: Statement Text Bool
activeSessionExistsStatement = mkStatement
    "SELECT EXISTS (\
    \ SELECT 1 FROM harness.sessions\
    \ WHERE session_key = $1 AND deleted_at IS NULL\
    \ )"
    textParam
    (Decoders.singleRow
        (Decoders.column (Decoders.nonNullable Decoders.bool)))
    True

sessionTurnExistsStatement :: Statement (Text, Int64) Bool
sessionTurnExistsStatement = mkStatement
    "SELECT EXISTS (\
    \ SELECT 1\
    \ FROM harness.session_turns turn_row\
    \ JOIN harness.sessions session_row\
    \   ON session_row.session_id = turn_row.session_id\
    \ WHERE session_row.session_key = $1\
    \   AND turn_row.turn_index = $2\
    \ )"
    ( ((\(value, _) -> value)
        >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((\(_, value) -> value)
            >$< Encoders.param (Encoders.nonNullable Encoders.int8))
    )
    (Decoders.singleRow
        (Decoders.column (Decoders.nonNullable Decoders.bool)))
    True

insertInteractionStatement :: Statement InteractionRequest Bool
insertInteractionStatement = mkStatement
    "INSERT INTO harness.session_interactions\
    \ (session_id, request_key, interaction_kind, payload_version,\
    \ request_payload_text, origin_tool_name, origin_call_id, created_at)\
    \ SELECT session_id, $2, $3, $4, $5, $6, $7, $8\
    \ FROM harness.sessions\
    \ WHERE session_key = $1 AND deleted_at IS NULL\
    \ ON CONFLICT (session_id, request_key) DO NOTHING"
    interactionRequestParams
    (fmap (> 0) Decoders.rowsAffected)
    True

insertResolutionStatement
    :: Statement InteractionResolutionRequest (Maybe InteractionResolution)
insertResolutionStatement = mkStatement
    "INSERT INTO harness.session_interaction_resolutions\
    \ (interaction_id, session_id, payload_version,\
    \ response_payload_text, resolved_by, resolved_at)\
    \ SELECT interaction.interaction_id, interaction.session_id,\
    \   $3, $4, $5, $6\
    \ FROM harness.session_interactions interaction\
    \ JOIN harness.sessions session_row\
    \   ON session_row.session_id = interaction.session_id\
    \ WHERE session_row.session_key = $1\
    \   AND session_row.deleted_at IS NULL\
    \   AND interaction.interaction_id = $2::uuid\
    \ ON CONFLICT (interaction_id) DO NOTHING\
    \ RETURNING interaction_id::text, payload_version,\
    \   response_payload_text, resolved_by, resolved_at"
    interactionResolutionRequestParams
    (fmap (fmap resolutionFromRow)
        (Decoders.rowMaybe resolutionRowDecoder))
    True

insertDeliveryStatement
    :: Statement InteractionDeliveryRequest (Maybe InteractionDelivery)
insertDeliveryStatement = mkStatement
    "INSERT INTO harness.session_interaction_deliveries\
    \ (interaction_id, session_id, delivery_kind, turn_index,\
    \ turn_fingerprint, delivered_at)\
    \ SELECT resolution.interaction_id, resolution.session_id,\
    \   $3, $4, $5, $6\
    \ FROM harness.session_interaction_resolutions resolution\
    \ JOIN harness.session_interactions interaction\
    \   ON interaction.interaction_id = resolution.interaction_id\
    \ JOIN harness.sessions session_row\
    \   ON session_row.session_id = interaction.session_id\
    \ WHERE session_row.session_key = $1\
    \   AND interaction.interaction_id = $2::uuid\
    \ ON CONFLICT (interaction_id) DO NOTHING\
    \ RETURNING interaction_id::text, delivery_kind,\
    \   turn_index, turn_fingerprint, delivered_at"
    interactionDeliveryRequestParams
    (fmap (fmap deliveryFromRow)
        (Decoders.rowMaybe deliveryRowDecoder))
    True

interactionLockStatement :: Statement Text Bool
interactionLockStatement = mkStatement
    "SELECT true FROM (\
    \ SELECT pg_advisory_xact_lock(hashtextextended($1, 684022779))\
    \ ) AS acquired"
    textParam
    (Decoders.singleRow
        (Decoders.column (Decoders.nonNullable Decoders.bool)))
    True

interactionRowDecoder :: Decoders.Row InteractionRow
interactionRowDecoder =
    InteractionRow
        <$> requiredText
        <*> requiredText
        <*> requiredText
        <*> requiredText
        <*> requiredInt32
        <*> requiredText
        <*> optionalText
        <*> optionalText
        <*> requiredTime
        <*> optionalInt32
        <*> optionalText
        <*> optionalText
        <*> optionalTime
        <*> optionalText
        <*> optionalInt64
        <*> optionalText
        <*> optionalTime

resolutionRowDecoder :: Decoders.Row ResolutionRow
resolutionRowDecoder =
    ResolutionRow
        <$> requiredText
        <*> requiredInt32
        <*> requiredText
        <*> requiredText
        <*> requiredTime

deliveryRowDecoder :: Decoders.Row DeliveryRow
deliveryRowDecoder =
    DeliveryRow
        <$> requiredText
        <*> requiredText
        <*> requiredInt64
        <*> optionalText
        <*> requiredTime

requiredText = Decoders.column (Decoders.nonNullable Decoders.text)
optionalText = Decoders.column (Decoders.nullable Decoders.text)
requiredInt32 = Decoders.column (Decoders.nonNullable Decoders.int4)
optionalInt32 = Decoders.column (Decoders.nullable Decoders.int4)
requiredInt64 = Decoders.column (Decoders.nonNullable Decoders.int8)
optionalInt64 = Decoders.column (Decoders.nullable Decoders.int8)
requiredTime = Decoders.column (Decoders.nonNullable Decoders.timestamptz)
optionalTime = Decoders.column (Decoders.nullable Decoders.timestamptz)

textParam :: Encoders.Params Text
textParam = Encoders.param (Encoders.nonNullable Encoders.text)

textPairParams :: Encoders.Params (Text, Text)
textPairParams =
    ((\(value, _) -> value) >$< textParam)
        <> ((\(_, value) -> value) >$< textParam)

interactionRequestParams :: Encoders.Params InteractionRequest
interactionRequestParams =
    ((.interactionRequestSessionKey) >$< textParam)
        <> ((.interactionRequestKey) >$< textParam)
        <> ((.interactionRequestKind) >$< textParam)
        <> ((.interactionRequestPayloadVersion)
            >$< Encoders.param (Encoders.nonNullable Encoders.int4))
        <> ((.interactionRequestPayload) >$< textParam)
        <> (requestOriginTool
            >$< Encoders.param (Encoders.nullable Encoders.text))
        <> (requestOriginCall
            >$< Encoders.param (Encoders.nullable Encoders.text))
        <> ((.interactionRequestCreatedAt)
            >$< Encoders.param (Encoders.nonNullable Encoders.timestamptz))

interactionResolutionRequestParams
    :: Encoders.Params InteractionResolutionRequest
interactionResolutionRequestParams =
    ((.interactionResolutionRequestSessionKey) >$< textParam)
        <> ((.interactionResolutionRequestInteractionId) >$< textParam)
        <> ((.interactionResolutionRequestPayloadVersion)
            >$< Encoders.param (Encoders.nonNullable Encoders.int4))
        <> ((.interactionResolutionRequestPayload) >$< textParam)
        <> ((.interactionResolutionRequestResponder) >$< textParam)
        <> ((.interactionResolutionRequestResolvedAt)
            >$< Encoders.param (Encoders.nonNullable Encoders.timestamptz))

interactionDeliveryRequestParams
    :: Encoders.Params InteractionDeliveryRequest
interactionDeliveryRequestParams =
    ((.interactionDeliveryRequestSessionKey) >$< textParam)
        <> ((.interactionDeliveryRequestInteractionId) >$< textParam)
        <> ((.interactionDeliveryRequestKind) >$< textParam)
        <> ((.interactionDeliveryRequestTurnIndex)
            >$< Encoders.param (Encoders.nonNullable Encoders.int8))
        <> ((.interactionDeliveryRequestTurnFingerprint)
            >$< Encoders.param (Encoders.nullable Encoders.text))
        <> ((.interactionDeliveryRequestDeliveredAt)
            >$< Encoders.param (Encoders.nonNullable Encoders.timestamptz))

requestOriginTool :: InteractionRequest -> Maybe Text
requestOriginTool request =
    (.interactionOriginToolName) <$> request.interactionRequestOrigin

requestOriginCall :: InteractionRequest -> Maybe Text
requestOriginCall request =
    (.interactionOriginCallId) <$> request.interactionRequestOrigin

deliveryIntentFromRequest
    :: InteractionDeliveryRequest
    -> InteractionDeliveryIntent
deliveryIntentFromRequest request = InteractionDeliveryIntent
    { interactionDeliveryIntentInteractionId =
        request.interactionDeliveryRequestInteractionId
    , interactionDeliveryIntentKind =
        request.interactionDeliveryRequestKind
    , interactionDeliveryIntentTurnFingerprint =
        request.interactionDeliveryRequestTurnFingerprint
    , interactionDeliveryIntentDeliveredAt =
        request.interactionDeliveryRequestDeliveredAt
    }

decodeInteractionRow :: InteractionRow -> Either Text SessionInteraction
decodeInteractionRow row = do
    origin <- decodeOrigin row
    resolution <- decodeResolution row
    delivery <- decodeDelivery row
    case (resolution, delivery) of
        (Nothing, Just _) ->
            Left "interaction delivery exists without a resolution"
        _ ->
            Right SessionInteraction
                { sessionInteractionId = row.rowInteractionId
                , sessionInteractionSessionKey = row.rowSessionKey
                , sessionInteractionRequestKey = row.rowRequestKey
                , sessionInteractionKind = row.rowKind
                , sessionInteractionPayloadVersion =
                    row.rowPayloadVersion
                , sessionInteractionPayload = row.rowPayload
                , sessionInteractionOrigin = origin
                , sessionInteractionCreatedAt = row.rowCreatedAt
                , sessionInteractionResolution = resolution
                , sessionInteractionDelivery = delivery
                }

decodeOrigin :: InteractionRow -> Either Text (Maybe InteractionOrigin)
decodeOrigin row =
    case (row.rowOriginToolName, row.rowOriginCallId) of
        (Nothing, Nothing) -> Right Nothing
        (Just toolName, Just callId) ->
            Right
                (Just InteractionOrigin
                    { interactionOriginToolName = toolName
                    , interactionOriginCallId = callId
                    })
        _ -> Left "interaction origin columns are partially populated"

decodeResolution
    :: InteractionRow
    -> Either Text (Maybe InteractionResolution)
decodeResolution row =
    case
        ( row.rowResolutionPayloadVersion
        , row.rowResolutionPayload
        , row.rowResolvedBy
        , row.rowResolvedAt
        ) of
        (Nothing, Nothing, Nothing, Nothing) -> Right Nothing
        (Just version, Just payload, Just responder, Just resolvedAt) ->
            Right
                (Just InteractionResolution
                    { interactionResolutionInteractionId =
                        row.rowInteractionId
                    , interactionResolutionPayloadVersion = version
                    , interactionResolutionPayload = payload
                    , interactionResolutionResponder = responder
                    , interactionResolutionResolvedAt = resolvedAt
                    })
        _ -> Left "interaction resolution columns are partially populated"

decodeDelivery :: InteractionRow -> Either Text (Maybe InteractionDelivery)
decodeDelivery row =
    case
        ( row.rowDeliveryKind
        , row.rowDeliveryTurnIndex
        , row.rowDeliveryTurnFingerprint
        , row.rowDeliveredAt
        ) of
        (Nothing, Nothing, Nothing, Nothing) -> Right Nothing
        (Just kind, Just turnIndex, fingerprint, Just deliveredAt) ->
            Right
                (Just InteractionDelivery
                    { interactionDeliveryInteractionId =
                        row.rowInteractionId
                    , interactionDeliveryKind = kind
                    , interactionDeliveryTurnIndex = turnIndex
                    , interactionDeliveryTurnFingerprint = fingerprint
                    , interactionDeliveryDeliveredAt = deliveredAt
                    })
        _ -> Left "interaction delivery columns are partially populated"

resolutionFromRow :: ResolutionRow -> InteractionResolution
resolutionFromRow row = InteractionResolution
    { interactionResolutionInteractionId = row.resolutionRowInteractionId
    , interactionResolutionPayloadVersion = row.resolutionRowPayloadVersion
    , interactionResolutionPayload = row.resolutionRowPayload
    , interactionResolutionResponder = row.resolutionRowResponder
    , interactionResolutionResolvedAt = row.resolutionRowResolvedAt
    }

deliveryFromRow :: DeliveryRow -> InteractionDelivery
deliveryFromRow row = InteractionDelivery
    { interactionDeliveryInteractionId = row.deliveryRowInteractionId
    , interactionDeliveryKind = row.deliveryRowKind
    , interactionDeliveryTurnIndex = row.deliveryRowTurnIndex
    , interactionDeliveryTurnFingerprint =
        row.deliveryRowTurnFingerprint
    , interactionDeliveryDeliveredAt = row.deliveryRowDeliveredAt
    }

decodeMaybeInteractionResult
    :: Either StoreError (Maybe InteractionRow)
    -> Either StoreError (Maybe SessionInteraction)
decodeMaybeInteractionResult = \case
    Left err -> Left err
    Right Nothing -> Right Nothing
    Right (Just row) ->
        case decodeInteractionRow row of
            Left err -> Left (StoreDataError err)
            Right interaction -> Right (Just interaction)

decodeInteractionListResult
    :: Either StoreError [InteractionRow]
    -> Either StoreError [SessionInteraction]
decodeInteractionListResult = \case
    Left err -> Left err
    Right rows ->
        case traverse decodeInteractionRow rows of
            Left err -> Left (StoreDataError err)
            Right interactions -> Right interactions

sameRequest :: InteractionRequest -> SessionInteraction -> Bool
sameRequest request interaction =
    ( request.interactionRequestSessionKey
    , request.interactionRequestKey
    , request.interactionRequestKind
    , request.interactionRequestPayloadVersion
    , request.interactionRequestPayload
    , request.interactionRequestOrigin
    )
        == ( interaction.sessionInteractionSessionKey
           , interaction.sessionInteractionRequestKey
           , interaction.sessionInteractionKind
           , interaction.sessionInteractionPayloadVersion
           , interaction.sessionInteractionPayload
           , interaction.sessionInteractionOrigin
           )

runInteractionWrite
    :: StorePool
    -> Transaction.Transaction (Either Text a)
    -> IO (Either StoreError a)
runInteractionWrite pool action =
    withSession pool
        (Transactions.transaction
            Transactions.ReadCommitted
            Transactions.Write
            action)
        >>= pure . flattenDataResult

flattenDataResult
    :: Either StoreError (Either Text a)
    -> Either StoreError a
flattenDataResult = \case
    Left err -> Left err
    Right (Left err) -> Left (StoreDataError err)
    Right (Right value) -> Right value

validateInteractionRequest :: InteractionRequest -> Either Text ()
validateInteractionRequest request = do
    validateSessionKey request.interactionRequestSessionKey
    validateRequestKey request.interactionRequestKey
    validateKind "interaction kind" request.interactionRequestKind
    validateVersion
        "interaction payload version"
        request.interactionRequestPayloadVersion
    validatePayload "interaction request payload" request.interactionRequestPayload
    case request.interactionRequestOrigin of
        Nothing -> Right ()
        Just origin -> do
            validateBoundedText
                "interaction origin tool name"
                256
                origin.interactionOriginToolName
            validateBoundedText
                "interaction origin call id"
                1024
                origin.interactionOriginCallId

validateResolutionRequest
    :: InteractionResolutionRequest
    -> Either Text ()
validateResolutionRequest request = do
    validateLookup
        request.interactionResolutionRequestSessionKey
        request.interactionResolutionRequestInteractionId
    validateVersion
        "interaction response payload version"
        request.interactionResolutionRequestPayloadVersion
    validatePayload
        "interaction response payload"
        request.interactionResolutionRequestPayload
    validateBoundedText
        "interaction responder"
        256
        request.interactionResolutionRequestResponder

validateDeliveryRequest
    :: InteractionDeliveryRequest
    -> Either Text ()
validateDeliveryRequest request = do
    validateLookup
        request.interactionDeliveryRequestSessionKey
        request.interactionDeliveryRequestInteractionId
    validateKind
        "interaction delivery kind"
        request.interactionDeliveryRequestKind
    traverse_
        (validateBoundedText "interaction delivery turn fingerprint" 128)
        request.interactionDeliveryRequestTurnFingerprint
    if request.interactionDeliveryRequestTurnIndex < 0
        then Left "interaction delivery turn index must be non-negative"
        else Right ()

validateDeliveryIntent
    :: Text
    -> InteractionDeliveryIntent
    -> Either Text ()
validateDeliveryIntent sessionKey intent = do
    validateLookup
        sessionKey
        intent.interactionDeliveryIntentInteractionId
    validateKind
        "interaction delivery kind"
        intent.interactionDeliveryIntentKind
    traverse_
        (validateBoundedText "interaction delivery turn fingerprint" 128)
        intent.interactionDeliveryIntentTurnFingerprint

validateLookup :: Text -> Text -> Either Text ()
validateLookup sessionKey interactionId = do
    validateSessionKey sessionKey
    case UUID.fromText interactionId of
        Nothing -> Left "interaction id must be a UUID"
        Just _ -> Right ()

validateSessionAndRequestKey :: Text -> Text -> Either Text ()
validateSessionAndRequestKey sessionKey requestKey = do
    validateSessionKey sessionKey
    validateRequestKey requestKey

validateSessionKey :: Text -> Either Text ()
validateSessionKey =
    validateBoundedText "session key" 512

validateRequestKey :: Text -> Either Text ()
validateRequestKey =
    validateBoundedText "interaction request key" 512

validateKind :: Text -> Text -> Either Text ()
validateKind label value
    | Text.null value =
        Left (label <> " must not be empty")
    | Text.length value > 80 =
        Left (label <> " must be at most 80 characters")
    | not (validKindStart (Text.head value))
        || Text.any (not . validKindRest) (Text.tail value) =
            Left
                (label
                    <> " must start with a lower-case letter and contain only lower-case letters, digits, '.', '_', or '-'")
    | otherwise = Right ()
  where
    validKindStart character =
        character >= 'a' && character <= 'z'
    validKindRest character =
        validKindStart character
            || (character >= '0' && character <= '9')
            || character `elem` (".-_" :: String)

validateVersion :: Text -> Int32 -> Either Text ()
validateVersion label version
    | version <= 0 = Left (label <> " must be positive")
    | otherwise = Right ()

validatePayload :: Text -> Text -> Either Text ()
validatePayload label payload
    | Text.null payload = Left (label <> " must not be empty")
    | Text.any (== '\NUL') payload =
        Left (label <> " must not contain a NUL byte")
    | ByteString.length (TextEncoding.encodeUtf8 payload) > 4194304 =
        Left (label <> " must be at most 4 MiB of UTF-8")
    | otherwise = Right ()

validateBoundedText :: Text -> Int -> Text -> Either Text ()
validateBoundedText label maximum value
    | Text.null (Text.strip value) =
        Left (label <> " must not be empty")
    | Text.any (== '\NUL') value =
        Left (label <> " must not contain a NUL byte")
    | Text.length value > maximum =
        Left
            (label <> " must be at most "
                <> Text.pack (show maximum) <> " characters")
    | otherwise = Right ()

interactionRequestLockKey :: InteractionRequest -> Text
interactionRequestLockKey request =
    "request:"
        <> Text.pack (show (Text.length request.interactionRequestSessionKey))
        <> ":"
        <> request.interactionRequestSessionKey
        <> ":"
        <> request.interactionRequestKey

interactionResolutionLockKey :: InteractionResolutionRequest -> Text
interactionResolutionLockKey request =
    "resolution:"
        <> Text.pack
            (show
                (Text.length request.interactionResolutionRequestSessionKey))
        <> ":"
        <> request.interactionResolutionRequestSessionKey
        <> ":"
        <> request.interactionResolutionRequestInteractionId

interactionDeliveryIntentLockKey
    :: Text
    -> InteractionDeliveryIntent
    -> Text
interactionDeliveryIntentLockKey sessionKey intent =
    interactionDeliveryLockKeyFor
        sessionKey
        intent.interactionDeliveryIntentInteractionId

interactionDeliveryLockKeyFor :: Text -> Text -> Text
interactionDeliveryLockKeyFor sessionKey interactionId =
    "delivery:"
        <> Text.pack
            (show
                (Text.length sessionKey))
        <> ":"
        <> sessionKey
        <> ":"
        <> interactionId
