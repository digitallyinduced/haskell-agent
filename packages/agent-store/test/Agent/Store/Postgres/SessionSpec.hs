{-# LANGUAGE OverloadedStrings #-}

module Agent.Store.Postgres.SessionSpec (spec) where

import Control.Exception.Safe (finally)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.Text (Text)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), picosecondsToDiffTime)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Agent.Store.Postgres
    ( closeStore
    , defaultManagedPostgresConfig
    , normalizePostgresTimestamp
    , openStore
    , trustedPool
    )
import Agent.Store.Postgres.Managed (stopManagedPostgres)
import Agent.Store.Postgres.Session
import Agent.Store.SessionItem (StoredResponseItem(..))

spec :: Spec
spec = describe "PostgreSQL session schema" do
    it "normalizes timestamps to PostgreSQL microsecond precision" do
        let timestamp = UTCTime
                (fromGregorian 2026 8 24)
                (picosecondsToDiffTime 467640816000)
        normalizePostgresTimestamp timestamp
            `shouldBe`
                UTCTime
                    (fromGregorian 2026 8 24)
                    (picosecondsToDiffTime 467640000000)

    it "uses typed session, turn, tool-call, and tool-output tables" do
        let ddl = ByteString.intercalate "\n" sessionSchemaStatements
        ddl `shouldContainBytes` "DEFAULT pg_catalog.uuidv7()"
        ddl `shouldNotContainBytes` "CREATE OR REPLACE FUNCTION harness.uuid_v7()"
        ddl `shouldNotContainBytes` "harness.structured_values"
        ddl `shouldContainBytes` "CREATE TABLE IF NOT EXISTS harness.sessions"
        ddl `shouldContainBytes` "CREATE TABLE IF NOT EXISTS harness.session_events"
        ddl `shouldContainBytes` "CREATE TABLE IF NOT EXISTS harness.session_turns"
        ddl `shouldContainBytes`
            "CREATE TABLE IF NOT EXISTS harness.session_response_items"
        ddl `shouldContainBytes`
            "CREATE TABLE IF NOT EXISTS harness.session_function_calls"
        ddl `shouldContainBytes`
            "CREATE TABLE IF NOT EXISTS harness.session_function_call_outputs"
        ddl `shouldContainBytes`
            "CREATE TABLE IF NOT EXISTS harness.session_custom_tool_calls"
        ddl `shouldContainBytes`
            "CREATE TABLE IF NOT EXISTS harness.session_custom_tool_call_outputs"
        ddl `shouldContainBytes` "call_id text NOT NULL"
        ddl `shouldContainBytes` "arguments text NOT NULL"
        ddl `shouldContainBytes` "output_text text NOT NULL"
        ddl `shouldNotContainBytes` "output jsonb NOT NULL"
        ddl `shouldContainBytes` "search_vector tsvector GENERATED ALWAYS"
        ddl `shouldContainBytes` "USING gin (search_vector)"
        ddl `shouldContainBytes` "session_events_immutable"
        ddl `shouldContainBytes` "session_turns_immutable"

    it "tracks restart-safe legacy imports" do
        let ddl = ByteString.intercalate "\n" sessionSchemaStatements
        ddl `shouldContainBytes`
            "CREATE TABLE IF NOT EXISTS harness.legacy_session_imports"
        ddl `shouldContainBytes` "import_id uuid PRIMARY KEY"
        ddl `shouldContainBytes` "UNIQUE (source_path, content_hash)"

    it "round-trips response items, tool calls, and outputs through relational rows" $
        withSystemTempDirectory "hs" \stateDirectory -> do
            let
                config = defaultManagedPostgresConfig stateDirectory ""
                cleanup = do
                    _ <- stopManagedPostgres config
                    pure ()
            (openStore config >>= \case
                Left err -> expectationFailure ("could not open store: " <> show err)
                Right store ->
                    finally
                        (do
                            let
                                pool = trustedPool store
                                now = read "2026-08-23 12:00:00 UTC"
                                metadata = testMetadata now
                                turn = testTurn now
                            createSession pool metadata
                                `shouldReturn` Right True
                            appendSessionTurn pool turn metadata
                                `shouldReturn` Right True
                            loadSession pool "session-1" >>= \case
                                Right (Just stored) -> do
                                    stored.storedMetadata `shouldBe` metadata
                                    case map (.storedTurn) stored.storedTurns of
                                        [loadedTurn] -> do
                                            loadedTurn
                                                { sessionTurnItems = [] }
                                                `shouldBe`
                                                    turn
                                                        { sessionTurnItems = [] }
                                            map itemIdentity
                                                loadedTurn.sessionTurnItems
                                                `shouldBe`
                                                    map itemIdentity
                                                        turn.sessionTurnItems
                                        loaded ->
                                            expectationFailure
                                                ("unexpected turns: " <> show loaded)
                                other ->
                                    expectationFailure
                                        ("unexpected stored session: " <> show other)
                            searchConversationTurns pool "compact" 10 >>= \case
                                Right [match] -> do
                                    match.searchSessionId `shouldBe` "session-1"
                                    match.searchUserText `shouldBe` "/compact"
                                other ->
                                    expectationFailure
                                        ("unexpected conversation search: " <> show other)
                            deleteSession pool "session-1" now
                                `shouldReturn` Right True
                            events <- loadSessionEvents pool "session-1"
                            fmap (map (.storedEventKind)) events
                                `shouldBe`
                                    Right
                                        [ "session.created"
                                        , "turn.appended"
                                        , "session.deleted"
                                        ]
                        )
                        (closeStore store)
                ) `finally` cleanup

testMetadata :: UTCTime -> SessionMetadata
testMetadata now = SessionMetadata
    { sessionMetadataKey = "session-1"
    , sessionMetadataVersion = 1
    , sessionMetadataCreatedAt = now
    , sessionMetadataUpdatedAt = now
    , sessionMetadataProvider = "openai"
    , sessionMetadataConnection = "openai"
    , sessionMetadataModel = "gpt-test"
    , sessionMetadataTransportModel = Just "gpt-test"
    , sessionMetadataDialect = "openai"
    , sessionMetadataLegacyTarget = Nothing
    , sessionMetadataCwd = "/tmp/project"
    , sessionMetadataEffort = "medium"
    , sessionMetadataTitle = "test"
    , sessionMetadataTitleIsManual = False
    , sessionMetadataTitleRefreshIndex = 0
    , sessionMetadataTitleUserTurns = 1
    , sessionMetadataLastResponseId = Just "response-1"
    , sessionMetadataInputTokens = 10
    , sessionMetadataOutputTokens = 5
    , sessionMetadataCachedTokens = 2
    }

testTurn :: UTCTime -> SessionTurn
testTurn now = SessionTurn
    { sessionTurnOccurredAt = now
    , sessionTurnUserText = "/compact"
    , sessionTurnAssistantText = Just "done"
    , sessionTurnError = Nothing
    , sessionTurnResponseId = Just "response-1"
    , sessionTurnItems =
        [ stored "message" "core"
            "{\"type\":\"message\",\"id\":\"item-message\",\"role\":\"developer\",\
            \\"status\":\"in_progress\",\"phase\":\"commentary\",\"content\":[\
            \{\"type\":\"input_text\",\"text\":\"hello\",\
            \\"prompt_cache_breakpoint\":{\"scope\":\"turn\"}},\
            \{\"type\":\"output_text\",\"text\":\"hello back\",\
            \\"annotations\":[{\"type\":\"citation\"}],\
            \\"logprobs\":[{\"token\":\"hello\"}]},\
            \{\"type\":\"provider_content\",\"provider_extension\":true}],\
            \\"provider_extension\":\"message\"}"
        , stored "message" "core"
            "{\"type\":\"message\",\"id\":\"item-text-message\",\
            \\"role\":\"observer\",\"status\":\"paused\",\"content\":\"plain text\"}"
        , stored "function_call" "core"
            "{\"type\":\"function_call\",\"id\":\"item-call\",\"call_id\":\"call-1\",\
            \\"name\":\"shell_command\",\"arguments\":\"{\\\"command\\\":\\\"pwd\\\"}\",\
            \\"status\":\"completed\",\"provider_extension\":\"function-call\"}"
        , stored "function_call_output" "core"
            "{\"type\":\"function_call_output\",\"id\":\"item-output\",\
            \\"call_id\":\"call-1\",\"output\":\"{\\\"stdout\\\":\\\"/tmp/project\\\"}\",\
            \\"status\":\"completed\",\"provider_extension\":\"function-output\"}"
        , stored "custom_tool_call" "core"
            "{\"type\":\"custom_tool_call\",\"id\":\"item-custom-call\",\
            \\"call_id\":\"custom-1\",\"name\":\"apply_patch\",\
            \\"input\":\"*** Begin Patch\",\"status\":\"in_progress\"}"
        , stored "custom_tool_call_output" "core"
            "{\"type\":\"custom_tool_call_output\",\"id\":\"item-custom-output\",\
            \\"call_id\":\"custom-1\",\"name\":\"apply_patch\",\"output\":\"Done\",\
            \\"status\":\"completed\"}"
        , stored "reasoning" "core"
            "{\"type\":\"reasoning\",\"id\":\"item-reasoning\",\
            \\"summary\":[{\"type\":\"summary_text\",\"text\":\"Checked the schema\"}],\
            \\"content\":[{\"type\":\"reasoning_text\",\
            \\"text\":\"private reasoning placeholder\"}],\
            \\"encrypted_content\":\"encrypted\",\"status\":\"completed\"}"
        , stored "item_reference" "core"
            "{\"type\":\"item_reference\",\"id\":\"item-call\"}"
        , stored "compaction_trigger" "known"
            "{\"type\":\"compaction_trigger\",\"provider_extension\":\"known-tagged\"}"
        , stored "provider_extension" "unknown"
            "{\"type\":\"provider_extension\",\"provider_extension\":\"unknown-tagged\"}"
        ]
    , sessionTurnUsage = Just SessionUsage
        { sessionUsageInputTokens = 10
        , sessionUsageOutputTokens = 5
        , sessionUsageCachedTokens = 2
        }
    }

stored :: Text -> Text -> Text -> StoredResponseItem
stored itemType representation payload = StoredResponseItem
    { storedResponseItemType = itemType
    , storedResponseItemRepresentation = representation
    , storedResponseItemPayload = payload
    }

itemIdentity :: StoredResponseItem -> (Text, Text)
itemIdentity item =
    ( item.storedResponseItemType
    , item.storedResponseItemRepresentation
    )

shouldContainBytes :: ByteString.ByteString -> ByteString.ByteString -> Expectation
shouldContainBytes haystack needle =
    ByteString.Char8.unpack haystack
        `shouldContain` ByteString.Char8.unpack needle

shouldNotContainBytes :: ByteString.ByteString -> ByteString.ByteString -> Expectation
shouldNotContainBytes haystack needle =
    ByteString.Char8.unpack haystack
        `shouldNotContain` ByteString.Char8.unpack needle
