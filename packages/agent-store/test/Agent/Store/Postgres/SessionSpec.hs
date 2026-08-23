{-# LANGUAGE OverloadedStrings #-}

module Agent.Store.Postgres.SessionSpec (spec) where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import Control.Exception.Safe (finally)
import qualified Data.Aeson as Aeson
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Agent.Store.Postgres
    ( closeStore
    , defaultManagedPostgresConfig
    , openStore
    , trustedPool
    )
import Agent.Store.Postgres.Managed (stopManagedPostgres)
import Agent.Store.Postgres.Session

spec :: Spec
spec = describe "PostgreSQL session schema" do
    it "defines separate metadata, immutable event, and immutable turn storage" do
        let ddl = ByteString.intercalate "\n" sessionSchemaStatements
        ddl `shouldContainBytes` "DEFAULT pg_catalog.uuidv7()"
        ddl `shouldNotContainBytes` "CREATE OR REPLACE FUNCTION harness.uuid_v7()"
        ddl `shouldContainBytes` "CREATE TABLE IF NOT EXISTS harness.structured_values"
        ddl `shouldContainBytes` "CREATE TABLE IF NOT EXISTS harness.sessions"
        ddl `shouldContainBytes` "CREATE TABLE IF NOT EXISTS harness.session_events"
        ddl `shouldContainBytes` "CREATE TABLE IF NOT EXISTS harness.session_turns"
        ddl `shouldContainBytes` "session_id uuid PRIMARY KEY"
        ddl `shouldContainBytes` "turn_id uuid PRIMARY KEY"
        ddl `shouldContainBytes` "search_vector tsvector GENERATED ALWAYS"
        ddl `shouldContainBytes` "USING gin (search_vector)"
        ddl `shouldNotContainBytes` " jsonb"
        ddl `shouldContainBytes` "session_events_immutable"
        ddl `shouldContainBytes` "session_turns_immutable"

    it "tracks restart-safe legacy imports" do
        let ddl = ByteString.intercalate "\n" sessionSchemaStatements
        ddl `shouldContainBytes`
            "CREATE TABLE IF NOT EXISTS harness.legacy_session_imports"
        ddl `shouldContainBytes` "import_id uuid PRIMARY KEY"
        ddl `shouldContainBytes` "UNIQUE (source_path, content_hash)"

    it "round-trips opaque compaction turns and keeps an immutable event stream" $
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
                                metadata = Aeson.object
                                    [ "id" Aeson..= ("session-1" :: String) ]
                                compactTurn = Aeson.object
                                    [ "userText" Aeson..= ("/compact" :: String)
                                    , "items" Aeson..= ([] :: [String])
                                    ]
                            createSession pool "session-1" now metadata
                                `shouldReturn` Right True
                            appendSessionTurn
                                pool "session-1" now compactTurn metadata
                                `shouldReturn` Right True
                            loadSession pool "session-1" >>= \case
                                Right (Just stored) ->
                                    map (.storedTurnPayload) stored.storedTurns
                                        `shouldBe` [compactTurn]
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

shouldContainBytes :: ByteString.ByteString -> ByteString.ByteString -> Expectation
shouldContainBytes haystack needle =
    ByteString.Char8.unpack haystack
        `shouldContain` ByteString.Char8.unpack needle

shouldNotContainBytes :: ByteString.ByteString -> ByteString.ByteString -> Expectation
shouldNotContainBytes haystack needle =
    ByteString.Char8.unpack haystack
        `shouldNotContain` ByteString.Char8.unpack needle
