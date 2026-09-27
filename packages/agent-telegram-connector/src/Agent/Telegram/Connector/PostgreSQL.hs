{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

-- | PostgreSQL persistence primitives for connector adapters.
--
-- Every operation takes a caller-owned connection. A connection must not be
-- shared between concurrent connector workers. Operations documented as
-- transactional compose with application publication on that same connection.
-- These tables are private server state, not an authorization database.
module Agent.Telegram.Connector.PostgreSQL
    ( PostgreSQLStore (..)
    , InboxRecord (..)
    , DeliveryRecord (..)
    , DeliveryDisposition (..)
    , CallbackScope (..)
    , initializePostgreSQLStore
    , postgreSQLInboxStore
    , postgreSQLDeliveryStore
    , readPollingOffset
    , persistInboxBatch
    , processNextInbox
    , withConversationTransaction
    , withConversationLock
    , enqueueDelivery
    , claimDelivery
    , completeDeliveryPart
    , rejectDeliveryPart
    , registerCallback
    , consumeCallback
    , revokeBinding
    ) where

import Control.Exception.Safe (tryAny)
import qualified Agent.Telegram.Connector as Connector
import Control.Monad (forM_, unless, void)
import Data.Aeson (Value, toJSON)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)

data PostgreSQLStore = PostgreSQLStore
    { connection :: !Connection
    , namespace :: !Text
    }

data InboxRecord = InboxRecord
    { updateIdentifier :: !Int64
    , payload :: !Value
    } deriving (Eq, Show)

instance FromRow InboxRecord where
    fromRow = InboxRecord <$> field <*> field

-- | Payloads are prepared and persisted before delivery. Subsequent versions
-- must never re-segment an existing delivery using a new formatting algorithm.
data DeliveryRecord = DeliveryRecord
    { deliveryIdentifier :: !Text
    , bindingIdentifier :: !Text
    , deliveryChatIdentifier :: !Int64
    , partIndex :: !Int
    , partPayload :: !Value
    } deriving (Eq, Show)

instance FromRow DeliveryRecord where
    fromRow = DeliveryRecord <$> field <*> field <*> field <*> field <*> field

data DeliveryDisposition = DeliveryRejected | DeliveryUncertain
    deriving (Eq, Show)

-- | All identifiers originate from the authenticated application binding and
-- current agent request, never from an untrusted callback payload.
data CallbackScope = CallbackScope
    { ownerIdentifier :: !Text
    , chatIdentifier :: !Int64
    , bindingRevision :: !Text
    , sessionIdentifier :: !Text
    , turnIdentifier :: !Text
    , requestIdentifier :: !Text
    } deriving (Eq, Show)

-- | Run explicitly during deployment, using a private server role. No browser
-- policies or public privileges are installed. Existing installations migrate
-- their state explicitly; startup does not reset checkpoints or delivery state.
initializePostgreSQLStore :: Connection -> IO ()
initializePostgreSQLStore connection = withTransaction connection $
    forM_ schemaStatements (void . execute_ connection)

schemaStatements :: [Query]
schemaStatements =
    [ "CREATE TABLE IF NOT EXISTS telegram_connector_offsets (namespace TEXT PRIMARY KEY, next_offset BIGINT NOT NULL DEFAULT 0 CHECK (next_offset >= 0))"
    , "CREATE TABLE IF NOT EXISTS telegram_connector_inbox (namespace TEXT NOT NULL, update_id BIGINT NOT NULL CHECK (update_id >= 0), payload JSONB NOT NULL, processed_at TIMESTAMPTZ, failure_code TEXT, PRIMARY KEY(namespace, update_id))"
    , "CREATE INDEX IF NOT EXISTS telegram_connector_inbox_pending ON telegram_connector_inbox(namespace, update_id) WHERE processed_at IS NULL"
    , "CREATE TABLE IF NOT EXISTS telegram_connector_deliveries (namespace TEXT NOT NULL, id TEXT NOT NULL, binding_revision TEXT NOT NULL, chat_id BIGINT NOT NULL, parts JSONB NOT NULL CHECK(jsonb_typeof(parts) = 'array' AND jsonb_array_length(parts) > 0), next_part INT NOT NULL DEFAULT 0 CHECK(next_part >= 0), status TEXT NOT NULL DEFAULT 'pending' CHECK(status IN ('pending','sending','sent','failed','uncertain')), created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(), PRIMARY KEY(namespace,id))"
    , "CREATE INDEX IF NOT EXISTS telegram_connector_deliveries_pending ON telegram_connector_deliveries(namespace,created_at,id) WHERE status = 'pending'"
    , "CREATE TABLE IF NOT EXISTS telegram_connector_receipts (namespace TEXT NOT NULL, delivery_id TEXT NOT NULL, part_index INT NOT NULL CHECK(part_index >= 0), telegram_message_id BIGINT NOT NULL, PRIMARY KEY(namespace,delivery_id,part_index), FOREIGN KEY(namespace,delivery_id) REFERENCES telegram_connector_deliveries(namespace,id))"
    , "CREATE TABLE IF NOT EXISTS telegram_connector_callbacks (namespace TEXT NOT NULL, token TEXT NOT NULL, owner_id TEXT NOT NULL, chat_id BIGINT NOT NULL, binding_revision TEXT NOT NULL, session_id TEXT NOT NULL, turn_id TEXT NOT NULL, request_id TEXT NOT NULL, response JSONB NOT NULL, expires_at TIMESTAMPTZ NOT NULL, consumed_at TIMESTAMPTZ, PRIMARY KEY(namespace,token))"
    , "ALTER TABLE telegram_connector_offsets ENABLE ROW LEVEL SECURITY"
    , "ALTER TABLE telegram_connector_inbox ENABLE ROW LEVEL SECURITY"
    , "ALTER TABLE telegram_connector_deliveries ENABLE ROW LEVEL SECURITY"
    , "ALTER TABLE telegram_connector_receipts ENABLE ROW LEVEL SECURITY"
    , "ALTER TABLE telegram_connector_callbacks ENABLE ROW LEVEL SECURITY"
    ]

readPollingOffset :: PostgreSQLStore -> IO Int64
readPollingOffset store = do
    rows <- query store.connection
        "SELECT next_offset FROM telegram_connector_offsets WHERE namespace = ?"
        (Only store.namespace)
    pure $ maybe 0 fromOnly (listToMaybe rows)

-- | Use distinct dedicated polling and processing connections: connector loops
-- run concurrently and must never interleave transactions on one connection.
-- Application publication in
-- 'Connector.withNextUpdate' must use the processing connection so successful
-- acknowledgement cannot commit without its associated application records.
postgreSQLInboxStore :: Connection -> Connection -> Text -> Connector.InboxStore Connection
postgreSQLInboxStore pollingConnection processingConnection namespace = Connector.InboxStore
    { Connector.readOffset = toInteger <$> readPollingOffset pollingStore
    , Connector.persistBatch = \records _ -> do
        unless (all (\(identifier, _) -> identifier >= 0
                && identifier < toInteger (maxBound :: Int64)) records) $
            fail "Invalid Telegram update identifier"
        persistInboxBatch pollingStore
            [InboxRecord (fromInteger identifier) value | (identifier, value) <- records]
    , Connector.withNextUpdate = \handler ->
        processNextInbox processingStore (\record -> handler processingConnection record.payload)
    }
    where
        pollingStore = PostgreSQLStore pollingConnection namespace
        processingStore = PostgreSQLStore processingConnection namespace

-- | Adapt durable prepared parts to the connector delivery worker. The decoder
-- must preserve the stored presentation and destination; it must not re-split
-- content using the current rendering algorithm.
--
-- The authorization scope locks and revalidates the current owner, chat and
-- binding revision before executing the supplied action, returning Nothing on
-- revocation. It holds that lock throughout transmission. Claims commit before
-- this scope starts. Receipts commit after it returns; a crash in between leaves
-- the durable sending claim untouched and is never retried.
--
-- Use a dedicated connection, outside any caller transaction.
postgreSQLDeliveryStore
    :: PostgreSQLStore
    -> (forall result. DeliveryRecord -> IO result -> IO (Maybe result))
    -> (DeliveryRecord -> Either Text Connector.DeliveryAttempt)
    -> Connector.DeliveryStore
postgreSQLDeliveryStore store authorize decode = Connector.DeliveryStore
    { Connector.withNextDelivery = \send -> do
        claimed <- claimDelivery store
        case claimed of
            Nothing -> pure False
            Just delivery -> do
                outcome <- case decode delivery of
                    Left _ -> pure Connector.DeliveryRejected
                    Right attempt -> do
                        result <- tryAny (authorize delivery (send attempt))
                        pure $ case result of
                            Left _ -> Connector.DeliveryUncertain
                            Right Nothing -> Connector.DeliveryRejected
                            Right (Just sent) -> sent
                case outcome of
                    Connector.DeliveryAcknowledged identifier
                        | identifier > 0 && identifier <= toInteger (maxBound :: Int64) ->
                            void $ completeDeliveryPart store delivery (fromInteger identifier)
                    Connector.DeliveryAcknowledged _ ->
                        rejectDeliveryPart store delivery DeliveryUncertain
                    Connector.DeliveryRejected ->
                        rejectDeliveryPart store delivery DeliveryRejected
                    Connector.DeliveryUncertain ->
                        rejectDeliveryPart store delivery DeliveryUncertain
                pure True
    }

-- | The batch and acknowledgement checkpoint commit together. Invalid batch
-- identifiers reject the complete batch rather than acknowledging lost work.
persistInboxBatch :: PostgreSQLStore -> [InboxRecord] -> IO ()
persistInboxBatch store records = do
    unless (all (\record -> record.updateIdentifier >= 0 && record.updateIdentifier < maxBound) records) $
        fail "Invalid Telegram update identifier"
    withTransaction store.connection $ do
        void $ execute store.connection
            "INSERT INTO telegram_connector_offsets(namespace) VALUES (?) ON CONFLICT DO NOTHING"
            (Only store.namespace)
        forM_ records $ \record -> do
            void $ execute store.connection
                "INSERT INTO telegram_connector_inbox(namespace,update_id,payload) VALUES (?,?,?) ON CONFLICT DO NOTHING"
                (store.namespace, record.updateIdentifier, record.payload)
            void $ execute store.connection
                "UPDATE telegram_connector_offsets SET next_offset = GREATEST(next_offset, ?) WHERE namespace = ?"
                (record.updateIdentifier + 1, store.namespace)

-- | Database publication must use the store's connection. The handler may
-- prepare media but must not perform non-idempotent external actions. A
-- synchronous failure rolls back publication and records only a sanitized
-- failure code. Cancellation rolls back the entire transaction and propagates.
processNextInbox :: PostgreSQLStore -> (InboxRecord -> IO ()) -> IO Bool
processNextInbox store handler = withTransaction store.connection $ do
    records <- query store.connection
        "SELECT update_id,payload FROM telegram_connector_inbox WHERE namespace = ? AND processed_at IS NULL ORDER BY update_id FOR UPDATE SKIP LOCKED LIMIT 1"
        (Only store.namespace)
    case records of
        [] -> pure False
        record : _ -> do
            void $ execute_ store.connection "SAVEPOINT telegram_connector_inbox_work"
            result <- tryAny (handler record)
            failure <- case result of
                Right () -> pure Nothing
                Left _ -> do
                    void $ execute_ store.connection "ROLLBACK TO SAVEPOINT telegram_connector_inbox_work"
                    pure (Just ("processing_failed" :: Text))
            void $ execute_ store.connection "RELEASE SAVEPOINT telegram_connector_inbox_work"
            void $ execute store.connection
                "UPDATE telegram_connector_inbox SET processed_at = NOW(), failure_code = ? WHERE namespace = ? AND update_id = ?"
                (failure, store.namespace, record.updateIdentifier)
            pure True

-- | All channels submitting to the same conversation must take this lock.
-- Hash collisions only serialize unrelated work; they cannot permit overlap.
withConversationTransaction :: PostgreSQLStore -> Text -> IO result -> IO (Maybe result)
withConversationTransaction store conversation action =
    withTransaction store.connection (withConversationLock store conversation action)

-- | Use within an existing publication transaction, including an inbox
-- handler. Returning Nothing means busy; the caller must retain/requeue work,
-- not silently acknowledge a skipped submission.
withConversationLock :: PostgreSQLStore -> Text -> IO result -> IO (Maybe result)
withConversationLock store conversation action = do
    acquired <- query store.connection
        "SELECT pg_try_advisory_xact_lock(hashtextextended(? || ':' || ?, 82137))"
        (store.namespace, conversation) :: IO [Only Bool]
    if acquired == [Only True] then Just <$> action else pure Nothing

-- | Call inside the application's publication transaction. The identifier is
-- stable across retries. Existing delivery contents and progress are immutable.
enqueueDelivery :: PostgreSQLStore -> Text -> Text -> Int64 -> [Value] -> IO ()
enqueueDelivery store identifier binding chat parts = do
    unless (not (null parts)) $ fail "Empty Telegram delivery"
    void $ execute store.connection
        "INSERT INTO telegram_connector_deliveries(namespace,id,binding_revision,chat_id,parts) VALUES (?,?,?,?,?) ON CONFLICT DO NOTHING"
        (store.namespace, identifier, binding, chat, toJSON parts)

-- | Must run outside a surrounding transaction: the sending claim commits
-- before network IO. A crashed sending claim is never automatically retried.
-- The caller revalidates and locks the application binding before transmission.
claimDelivery :: PostgreSQLStore -> IO (Maybe DeliveryRecord)
claimDelivery store = listToMaybe <$> query store.connection
    "UPDATE telegram_connector_deliveries SET status = 'sending' WHERE (namespace,id) = (SELECT namespace,id FROM telegram_connector_deliveries WHERE namespace = ? AND status = 'pending' AND next_part < jsonb_array_length(parts) ORDER BY created_at,id FOR UPDATE SKIP LOCKED LIMIT 1) RETURNING id,binding_revision,chat_id,next_part,parts->next_part"
    (Only store.namespace)

-- | Compare-and-set progress and insert receipt in one transaction. A duplicate
-- or stale completion cannot advance a later chunk or replace a receipt.
completeDeliveryPart :: PostgreSQLStore -> DeliveryRecord -> Int64 -> IO Bool
completeDeliveryPart store delivery messageIdentifier = withTransaction store.connection $ do
    updated <- execute store.connection
        "UPDATE telegram_connector_deliveries SET next_part = next_part + 1, status = CASE WHEN next_part + 1 < jsonb_array_length(parts) THEN 'pending' ELSE 'sent' END WHERE namespace = ? AND id = ? AND status = 'sending' AND next_part = ?"
        (store.namespace, delivery.deliveryIdentifier, delivery.partIndex)
    if updated /= 1 then pure False else do
        void $ execute store.connection
            "INSERT INTO telegram_connector_receipts(namespace,delivery_id,part_index,telegram_message_id) VALUES (?,?,?,?)"
            (store.namespace, delivery.deliveryIdentifier, delivery.partIndex, messageIdentifier)
        pure True

rejectDeliveryPart :: PostgreSQLStore -> DeliveryRecord -> DeliveryDisposition -> IO ()
rejectDeliveryPart store delivery disposition = void $ execute store.connection
    "UPDATE telegram_connector_deliveries SET status = ? WHERE namespace = ? AND id = ? AND status = 'sending' AND next_part = ?"
    (status, store.namespace, delivery.deliveryIdentifier, delivery.partIndex)
    where
        status = case disposition of
            DeliveryRejected -> "failed" :: Text
            DeliveryUncertain -> "uncertain"

-- | Tokens must be cryptographically random and fit Telegram's callback-data
-- limit. Registration never changes the meaning of an existing token.
registerCallback :: PostgreSQLStore -> Text -> CallbackScope -> Value -> UTCTime -> IO ()
registerCallback store token scope response expiresAt = void $ execute store.connection
    "INSERT INTO telegram_connector_callbacks(namespace,token,owner_id,chat_id,binding_revision,session_id,turn_id,request_id,response,expires_at) VALUES (?,?,?,?,?,?,?,?,?,?) ON CONFLICT DO NOTHING"
    (store.namespace, token, scope.ownerIdentifier, scope.chatIdentifier,
        scope.bindingRevision, scope.sessionIdentifier, scope.turnIdentifier,
        scope.requestIdentifier, response, expiresAt)

-- | Call after locking and revalidating the current binding and pending request,
-- in the same transaction that persists the human response for submission.
-- Consumption alone must never be followed by an unrecorded remote request.
consumeCallback :: PostgreSQLStore -> Text -> CallbackScope -> IO (Maybe Value)
consumeCallback store token scope = fmap fromOnly . listToMaybe <$> query store.connection
    "UPDATE telegram_connector_callbacks SET consumed_at = NOW() WHERE namespace = ? AND token = ? AND owner_id = ? AND chat_id = ? AND binding_revision = ? AND session_id = ? AND turn_id = ? AND request_id = ? AND consumed_at IS NULL AND expires_at > NOW() RETURNING response"
    (store.namespace, token, scope.ownerIdentifier, scope.chatIdentifier,
        scope.bindingRevision, scope.sessionIdentifier, scope.turnIdentifier,
        scope.requestIdentifier)

-- | Compose with application unlink under its owner/binding lock. A sending
-- delivery remains uncertain; revocation must not imply it was never sent.
revokeBinding :: PostgreSQLStore -> Text -> IO ()
revokeBinding store binding = do
    void $ execute store.connection
        "UPDATE telegram_connector_callbacks SET consumed_at = NOW() WHERE namespace = ? AND binding_revision = ? AND consumed_at IS NULL"
        (store.namespace, binding)
    void $ execute store.connection
        "UPDATE telegram_connector_deliveries SET status = 'failed' WHERE namespace = ? AND binding_revision = ? AND status = 'pending'"
        (store.namespace, binding)
