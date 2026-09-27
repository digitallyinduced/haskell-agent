{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module PostgreSQLSpec (main, spec) where

import Agent.Telegram.Connector.PostgreSQL
import qualified Agent.Telegram.Connector as Connector
import Agent.Telegram.Types.State (TelegramChatKey (..))
import Control.Exception (AsyncException (ThreadKilled), throwIO)
import Control.Exception.Safe (bracket, bracket_)
import Control.Monad (void)
import Data.Aeson (Value (..))
import qualified Data.ByteString.Char8 as ByteString
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (addUTCTime, getCurrentTime)
import Database.PostgreSQL.Simple
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess, readProcess)
import Test.Hspec

-- An isolated, socket-only cluster; never obtains application credentials.
main :: IO ()
main = withSystemTempDirectory "tc" $ \directory -> do
    let dataDirectory = directory </> "data"
        options = "-k '" <> directory <> "' -c listen_addresses='' -c unix_socket_permissions=0700"
    void $ readProcess "initdb"
        ["-D", dataDirectory, "-U", "connector_test", "--auth=trust", "--no-locale"] ""
    bracket_
        (callProcess "pg_ctl" ["-D", dataDirectory, "-l", directory </> "postgres.log", "-o", options, "-w", "start"])
        (callProcess "pg_ctl" ["-D", dataDirectory, "-m", "immediate", "-w", "stop"]) $
        let connect = connectPostgreSQL (ByteString.pack $
                "host='" <> directory <> "' user=connector_test dbname=postgres")
        in bracket connect close $ \connection ->
            bracket connect close $ \otherConnection -> do
                initializePostgreSQLStore connection
                hspec (spec connection otherConnection)

spec :: Connection -> Connection -> Spec
spec connection otherConnection = describe "connector PostgreSQL persistence" $ do
    it "stores duplicate batches once and keeps the acknowledgement monotonic" $ do
        let store = PostgreSQLStore connection "inbox"
        persistInboxBatch store [InboxRecord 3 Null, InboxRecord 5 (String "original")]
        persistInboxBatch store [InboxRecord 3 Null, InboxRecord 5 (String "changed")]
        readPollingOffset store `shouldReturn` 6
        records <- query connection
            "SELECT update_id,payload FROM telegram_connector_inbox WHERE namespace = 'inbox' ORDER BY update_id" ()
        records `shouldBe` [InboxRecord 3 Null, InboxRecord 5 (String "original")]
    it "rejects a whole invalid batch without advancing acknowledgement" $ do
        let store = PostgreSQLStore connection "invalid"
        persistInboxBatch store [InboxRecord 3 Null, InboxRecord maxBound Null]
            `shouldThrow` anyIOException
        readPollingOffset store `shouldReturn` 0
    it "rolls back poison publication while allowing the following update" $ do
        let store = PostgreSQLStore connection "poison"
        persistInboxBatch store [InboxRecord 1 Null, InboxRecord 2 Null]
        processNextInbox store (\_ -> do
            enqueueDelivery store "rolled-back" "binding" 1 [Null]
            fail "sensitive failure detail") `shouldReturn` True
        processNextInbox store (\_ -> enqueueDelivery store "retained" "binding" 1 [Null])
            `shouldReturn` True
        identifiers <- query connection
            "SELECT id FROM telegram_connector_deliveries WHERE namespace = 'poison'" () :: IO [Only Text]
        identifiers `shouldBe` [Only "retained"]
        processNextInbox store (\_ -> expectationFailure "no work expected")
            `shouldReturn` False
    it "propagates cancellation and leaves the update unprocessed" $ do
        let store = PostgreSQLStore connection "cancel"
        persistInboxBatch store [InboxRecord 1 Null]
        processNextInbox store (\_ -> throwIO ThreadKilled)
            `shouldThrow` (== ThreadKilled)
        processNextInbox store (\_ -> pure ()) `shouldReturn` True
    it "persists exact chunk progress and never reclaims uncertain sends" $ do
        let store = PostgreSQLStore connection "delivery"
        enqueueDelivery store "response" "binding" 42 [String "first", String "second"]
        first <- requireJust =<< claimDelivery store
        first.partPayload `shouldBe` String "first"
        claimDelivery store `shouldReturn` Nothing
        completeDeliveryPart store first 101 `shouldReturn` True
        completeDeliveryPart store first 101 `shouldReturn` False
        enqueueDelivery store "response" "binding" 42 [String "replacement"]
        second <- requireJust =<< claimDelivery store
        second.partIndex `shouldBe` 1
        second.partPayload `shouldBe` String "second"
        rejectDeliveryPart store second DeliveryUncertain
        claimDelivery store `shouldReturn` Nothing
        receipts <- query connection
            "SELECT part_index,telegram_message_id FROM telegram_connector_receipts WHERE namespace = 'delivery'" () :: IO [(Int,Int64)]
        receipts `shouldBe` [(0,101)]
    it "serializes a conversation without blocking unrelated conversations" $ do
        let first = PostgreSQLStore connection "serialization"
            second = PostgreSQLStore otherConnection "serialization"
        withConversationTransaction first "same" (do
            withConversationTransaction second "same" (expectationFailure "must not acquire")
                `shouldReturn` Nothing
            withConversationTransaction second "different" (pure True)
                `shouldReturn` Just True) `shouldReturn` Just ()
        withConversationTransaction second "same" (pure True)
            `shouldReturn` Just True
    it "rolls callback consumption back with failed response publication" $ do
        let store = PostgreSQLStore connection "callback-rollback"
            scope = CallbackScope "owner" 42 "revision" "session" "turn" "request"
        now <- getCurrentTime
        registerCallback store "token" scope Null (addUTCTime 60 now)
        withTransaction connection (do
            consumeCallback store "token" scope `shouldReturn` Just Null
            fail "response intent publication failed") `shouldThrow` anyIOException
        consumeCallback store "token" scope `shouldReturn` Just Null
    it "adapts the connector inbox without acknowledging failed publication" $ do
        let inbox = postgreSQLInboxStore connection otherConnection "adapter-inbox"
        inbox.persistBatch [(1, String "input")] 999
        inbox.readOffset `shouldReturn` 2
        inbox.withNextUpdate (\_ value -> value `shouldBe` String "input")
            `shouldReturn` True
        inbox.withNextUpdate (\_ _ -> expectationFailure "duplicate") `shouldReturn` False
        failures <- query_ connection
            "SELECT failure_code FROM telegram_connector_inbox WHERE namespace = 'adapter-inbox'"
            :: IO [Only (Maybe Text)]
        failures `shouldBe` [Only Nothing]
    it "durably defers rate-limited parts without advancing or replaying receipts" $ do
        let store = PostgreSQLStore connection "rate-limit"
            restarted = PostgreSQLStore otherConnection "rate-limit"
            attempt = Connector.DeliveryAttempt (TelegramChatKey 42 Nothing) Nothing "part" Nothing
            delivery = postgreSQLDeliveryStore store (\_ action -> Just <$> action) (const (Right attempt))
        enqueueDelivery store "response" "binding" 42 [String "first", String "second"]
        first <- requireJust =<< claimDelivery store
        completeDeliveryPart store first 101 `shouldReturn` True
        delivery.withNextDelivery (\_ -> pure (Connector.DeliveryRetryAfter 37)) `shouldReturn` True
        claimDelivery restarted `shouldReturn` Nothing
        rows <- query connection
            "SELECT next_part,status,retry_at > NOW() + INTERVAL '30 seconds' FROM telegram_connector_deliveries WHERE namespace = 'rate-limit'"
            () :: IO [(Int,Text,Bool)]
        rows `shouldBe` [(1,"pending",True)]
        void $ execute_ connection
            "UPDATE telegram_connector_deliveries SET retry_at = NOW() - INTERVAL '1 second' WHERE namespace = 'rate-limit'"
        retried <- requireJust =<< claimDelivery restarted
        retried.partIndex `shouldBe` 1
        retried.partPayload `shouldBe` String "second"
        completeDeliveryPart restarted retried 102 `shouldReturn` True
        retryDeliveryPart store retried 37
        claimDelivery restarted `shouldReturn` Nothing
        receipts <- query connection
            "SELECT part_index,telegram_message_id FROM telegram_connector_receipts WHERE namespace = 'rate-limit' ORDER BY part_index"
            () :: IO [(Int,Int64)]
        receipts `shouldBe` [(0,101),(1,102)]
    it "adapts delivery claims, receipts and scoped authorization" $ do
        let store = PostgreSQLStore connection "adapter-delivery"
            attempt = Connector.DeliveryAttempt (TelegramChatKey 42 Nothing) Nothing "part" Nothing
            delivery = postgreSQLDeliveryStore store (\_ action -> Just <$> action) (const (Right attempt))
        enqueueDelivery store "response" "binding" 42 [String "prepared"]
        delivery.withNextDelivery (\actual -> do
            actual `shouldBe` attempt
            claimDelivery (PostgreSQLStore otherConnection "adapter-delivery") `shouldReturn` Nothing
            pure (Connector.DeliveryAcknowledged 45)) `shouldReturn` True
        delivery.withNextDelivery (\_ -> fail "duplicate") `shouldReturn` False
        receipts <- query_ connection
            "SELECT telegram_message_id FROM telegram_connector_receipts WHERE namespace = 'adapter-delivery'"
            :: IO [Only Int64]
        receipts `shouldBe` [Only 45]
    it "never transmits a revoked claim or retries an uncertain adapter send" $ do
        let store = PostgreSQLStore connection "adapter-failures"
            attempt = Connector.DeliveryAttempt (TelegramChatKey 42 Nothing) Nothing "part" Nothing
            revoked = postgreSQLDeliveryStore store (\_ _ -> pure Nothing) (const (Right attempt))
            allowed = postgreSQLDeliveryStore store (\_ action -> Just <$> action) (const (Right attempt))
        enqueueDelivery store "revoked" "binding" 42 [Null]
        revoked.withNextDelivery (\_ -> expectationFailure "revoked send" >> pure Connector.DeliveryUncertain)
            `shouldReturn` True
        enqueueDelivery store "uncertain" "binding" 42 [Null]
        allowed.withNextDelivery (\_ -> fail "lost response") `shouldReturn` True
        allowed.withNextDelivery (\_ -> fail "duplicate") `shouldReturn` False
        states <- query_ connection
            "SELECT id,status FROM telegram_connector_deliveries WHERE namespace = 'adapter-failures' ORDER BY id"
            :: IO [(Text, Text)]
        states `shouldBe` [("revoked", "failed"), ("uncertain", "uncertain")]
    it "blocks browser-role reads and writes even with table grants" $ do
        void $ execute_ connection "CREATE ROLE connector_browser NOLOGIN"
        void $ execute_ connection "GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA public TO connector_browser"
        bracket_
            (void $ execute_ connection "SET ROLE connector_browser")
            (void $ execute_ connection "RESET ROLE") $ do
                counts <- query_ connection
                    "SELECT count(*) FROM telegram_connector_offsets UNION ALL SELECT count(*) FROM telegram_connector_inbox UNION ALL SELECT count(*) FROM telegram_connector_deliveries UNION ALL SELECT count(*) FROM telegram_connector_receipts UNION ALL SELECT count(*) FROM telegram_connector_callbacks"
                    :: IO [Only Int64]
                counts `shouldBe` replicate 5 (Only 0)
                execute_ connection "INSERT INTO telegram_connector_offsets(namespace) VALUES ('browser-write')"
                    `shouldThrow` (\exception -> sqlState exception == "42501")
    it "requires every callback scope component and consumes only once" $ do
        let store = PostgreSQLStore connection "callback"
            scope = CallbackScope "owner" 42 "revision" "session" "turn" "request"
        now <- getCurrentTime
        registerCallback store "token" scope (String "approve") (addUTCTime 60 now)
        mapM_ (\wrong -> consumeCallback store "token" wrong `shouldReturn` Nothing)
            [ scope { ownerIdentifier = "other" }
            , scope { chatIdentifier = 43 }
            , scope { bindingRevision = "replacement" }
            , scope { sessionIdentifier = "other" }
            , scope { turnIdentifier = "other" }
            , scope { requestIdentifier = "other" }
            ]
        consumeCallback store "token" scope `shouldReturn` Just (String "approve")
        consumeCallback store "token" scope `shouldReturn` Nothing
    it "rejects expired callbacks and revokes outstanding binding work" $ do
        let store = PostgreSQLStore connection "revoke"
            scope = CallbackScope "owner" 42 "revision" "session" "turn" "request"
        now <- getCurrentTime
        registerCallback store "expired" scope Null (addUTCTime (-60) now)
        consumeCallback store "expired" scope `shouldReturn` Nothing
        registerCallback store "current" scope Null (addUTCTime 60 now)
        enqueueDelivery store "pending" "revision" 42 [Null]
        withTransaction connection $ revokeBinding store "revision"
        consumeCallback store "current" scope `shouldReturn` Nothing
        claimDelivery store `shouldReturn` Nothing
    it "uses namespace separation for checkpoints and callbacks" $ do
        let first = PostgreSQLStore connection "namespace-first"
            second = PostgreSQLStore connection "namespace-second"
            scope = CallbackScope "owner" 42 "revision" "session" "turn" "request"
        now <- getCurrentTime
        persistInboxBatch first [InboxRecord 9 Null]
        readPollingOffset second `shouldReturn` 0
        registerCallback first "same-token" scope Null (addUTCTime 60 now)
        consumeCallback second "same-token" scope `shouldReturn` Nothing
    it "composes connector publication with an application rollback" $ do
        let store = PostgreSQLStore connection "publication"
        withConversationTransaction store "conversation" (do
            enqueueDelivery store "response" "binding" 42 [Null]
            fail "rollback" :: IO ()) `shouldThrow` anyIOException
        claimDelivery store `shouldReturn` Nothing

requireJust :: Maybe value -> IO value
requireJust Nothing = expectationFailure "Expected stored record" >> fail "missing record"
requireJust (Just value) = pure value
