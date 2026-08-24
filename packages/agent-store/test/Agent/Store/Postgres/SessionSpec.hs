{-# LANGUAGE OverloadedStrings #-}

module Agent.Store.Postgres.SessionSpec (spec) where

import Control.Exception.Safe (finally)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.Time.Clock (UTCTime)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Agent.Responses.Types
    ( CustomToolCall(..)
    , CustomToolCallOutput(..)
    , FunctionCall(..)
    , FunctionCallOutput(..)
    , ItemReference(..)
    , ItemStatus(..)
    , MessageContent(..)
    , ReasoningItem(..)
    , ReasoningSummaryPart(..)
    , ResponseContentPart(..)
    , ResponseItem(..)
    , ResponseItemType(..)
    , ResponseMessage(..)
    , ResponseRole(..)
    , TaggedObject(..)
    )
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
        ddl `shouldContainBytes` "output jsonb NOT NULL"
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
                                    map (.storedTurn) stored.storedTurns
                                        `shouldBe` [turn]
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
        [ MessageItem ResponseMessage
            { messageId = Just "item-message"
            , content = MessageContentParts
                [ InputTextPart
                    { text = "hello"
                    , promptCacheBreakpoint =
                        Just (Aeson.object ["scope" Aeson..= ("turn" :: String)])
                    , extraFields = testExtras "input-text"
                    }
                , InputImagePart
                    { detail = Just "high"
                    , fileId = Just "image-file"
                    , imageUrl = Just "https://example.invalid/image.png"
                    , promptCacheBreakpoint = Just (Aeson.Bool True)
                    , extraFields = testExtras "input-image"
                    }
                , InputFilePart
                    { detail = Just "document"
                    , fileData = Just "ZmlsZQ=="
                    , fileId = Just "document-file"
                    , fileUrl = Just "https://example.invalid/document.txt"
                    , filename = Just "document.txt"
                    , promptCacheBreakpoint = Just (Aeson.String "break")
                    , extraFields = testExtras "input-file"
                    }
                , InputAudioPart
                    { inputAudio =
                        Aeson.object
                            [ "data" Aeson..= ("YXVkaW8=" :: String)
                            , "format" Aeson..= ("wav" :: String)
                            ]
                    , extraFields = testExtras "input-audio"
                    }
                , OutputTextPart
                    { text = "hello back"
                    , annotations =
                        Just
                            [ Aeson.object
                                [ "type" Aeson..= ("citation" :: String)
                                ]
                            ]
                    , logprobs =
                        Just
                            [ Aeson.object
                                [ "token" Aeson..= ("hello" :: String)
                                ]
                            ]
                    , extraFields = testExtras "output-text"
                    }
                , RefusalPart
                    { refusal = "not available"
                    , extraFields = testExtras "refusal"
                    }
                , UnknownContentPart TaggedObject
                    { tag = "provider_content"
                    , fields = testExtras "unknown-content"
                    }
                ]
            , role = RoleDeveloper
            , status = Just ItemInProgress
            , phase = Just "commentary"
            , extraFields = testExtras "message"
            }
        , MessageItem ResponseMessage
            { messageId = Just "item-text-message"
            , content = MessageContentText "plain text"
            , role = RoleUnknown "observer"
            , status = Just (ItemStatusUnknown "paused")
            , phase = Nothing
            , extraFields = testExtras "text-message"
            }
        , FunctionCallItem FunctionCall
            { itemId = Just "item-call"
            , callId = "call-1"
            , name = "shell_command"
            , arguments = "{\"command\":\"pwd\"}"
            , status = Just ItemCompleted
            , extraFields = testExtras "function-call"
            }
        , FunctionCallOutputItem FunctionCallOutput
            { itemId = Just "item-output"
            , callId = "call-1"
            , output = Aeson.object ["stdout" Aeson..= ("/tmp/project" :: String)]
            , status = Just ItemCompleted
            , extraFields = testExtras "function-output"
            }
        , CustomToolCallItem CustomToolCall
            { itemId = Just "item-custom-call"
            , callId = "custom-1"
            , name = "apply_patch"
            , input = "*** Begin Patch"
            , status = Just ItemInProgress
            , extraFields = testExtras "custom-call"
            }
        , CustomToolCallOutputItem CustomToolCallOutput
            { itemId = Just "item-custom-output"
            , callId = "custom-1"
            , name = Just "apply_patch"
            , output = Aeson.String "Done"
            , status = Just ItemCompleted
            , extraFields = testExtras "custom-output"
            }
        , ReasoningItemValue ReasoningItem
            { itemId = Just "item-reasoning"
            , summary =
                [ ReasoningSummaryPart
                    { partType = "summary_text"
                    , text = Just "Checked the schema"
                    , extraFields = testExtras "reasoning-summary"
                    }
                ]
            , content = Just
                [ ReasoningTextPart
                    { text = "private reasoning placeholder"
                    , extraFields = testExtras "reasoning-text"
                    }
                , SummaryTextPart
                    { text = "summary placeholder"
                    , extraFields = testExtras "summary-text"
                    }
                ]
            , encryptedContent = Just "encrypted"
            , status = Just ItemCompleted
            , extraFields = testExtras "reasoning"
            }
        , ItemReferenceValue ItemReference
            { itemId = "item-call"
            , extraFields = testExtras "reference"
            }
        , KnownResponseItem ItemCompactionTrigger TaggedObject
            { tag = "compaction_trigger"
            , fields = testExtras "known-tagged"
            }
        , UnknownResponseItem TaggedObject
            { tag = "provider_extension"
            , fields = testExtras "unknown-tagged"
            }
        ]
    , sessionTurnUsage = Just SessionUsage
        { sessionUsageInputTokens = 10
        , sessionUsageOutputTokens = 5
        , sessionUsageCachedTokens = 2
        }
    }

testExtras :: String -> Aeson.Object
testExtras label =
    KeyMap.singleton "provider_extension" (Aeson.toJSON label)

shouldContainBytes :: ByteString.ByteString -> ByteString.ByteString -> Expectation
shouldContainBytes haystack needle =
    ByteString.Char8.unpack haystack
        `shouldContain` ByteString.Char8.unpack needle

shouldNotContainBytes :: ByteString.ByteString -> ByteString.ByteString -> Expectation
shouldNotContainBytes haystack needle =
    ByteString.Char8.unpack haystack
        `shouldNotContain` ByteString.Char8.unpack needle
