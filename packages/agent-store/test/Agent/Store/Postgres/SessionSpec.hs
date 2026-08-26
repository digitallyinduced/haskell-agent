{-# LANGUAGE OverloadedStrings #-}

module Agent.Store.Postgres.SessionSpec (spec) where

import Control.Exception.Safe (finally)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as Text
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
import Agent.Store.SessionItem

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
        ddl `shouldContainBytes`
            "output_kind text NOT NULL CHECK (output_kind IN ('text', 'encoded'))"
        ddl `shouldContainBytes` "output_text text NOT NULL"
        ddl `shouldContainBytes` "extra_fields_text text NOT NULL"
        ddl `shouldNotContainBytes` "jsonb"
        ddl `shouldNotContainBytes` "::json"
        ddl `shouldContainBytes` "search_vector tsvector GENERATED ALWAYS"
        ddl `shouldContainBytes` "USING gin (search_vector)"
        ddl `shouldContainBytes` "CREATE EXTENSION IF NOT EXISTS pg_trgm"
        ddl `shouldContainBytes` "USING gin (user_text gin_trgm_ops)"
        ddl `shouldContainBytes` "USING gin (assistant_text gin_trgm_ops)"
        ddl `shouldContainBytes` "session_events_immutable"
        ddl `shouldContainBytes` "session_turns_immutable"

    it "tracks restart-safe legacy imports" do
        let ddl = ByteString.intercalate "\n" sessionSchemaStatements
        ddl `shouldContainBytes`
            "CREATE TABLE IF NOT EXISTS harness.legacy_session_imports"
        ddl `shouldContainBytes` "import_id uuid PRIMARY KEY"
        ddl `shouldContainBytes` "UNIQUE (source_path, content_hash)"

    it "round-trips response items, tool calls, and outputs through relational rows" $
        withSystemTempDirectory "ha" \stateDirectory -> do
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
                                    case map (.storedTurn)
                                        (toList stored.storedTurns) of
                                        [loadedTurn] -> do
                                            loadedTurn `shouldBe` turn
                                        loaded ->
                                            expectationFailure
                                                ("unexpected turns: " <> show loaded)
                                other ->
                                    expectationFailure
                                        ("unexpected stored session: " <> show other)
                            let metadata2 = metadata
                                    { sessionMetadataKey = "session-2"
                                    , sessionMetadataTitle = "second"
                                    }
                                turn2 = turn
                                    { sessionTurnUserText = "batch load"
                                    }
                                turn3 = turn
                                    { sessionTurnUserText = "batch load 2"
                                    }
                            createSession pool metadata2
                                `shouldReturn` Right True
                            appendSessionTurn pool turn2 metadata2
                                `shouldReturn` Right True
                            appendSessionTurn pool turn3 metadata2
                                `shouldReturn` Right True
                            loadSessions pool [] `shouldReturn` []
                            loadSessions pool
                                [ "session-2"
                                , "missing"
                                , "session-1"
                                , "session-2"
                                ] >>= \results ->
                                    fmap
                                        (fmap
                                            (fmap
                                                (\stored ->
                                                    ( stored.storedMetadata.sessionMetadataKey
                                                    , map
                                                        ( (.sessionTurnUserText)
                                                            . (.storedTurn)
                                                        )
                                                        (toList stored.storedTurns)
                                                    ))))
                                        results
                                        `shouldBe`
                                            [ Right
                                                (Just
                                                    ( "session-2"
                                                    , [ "batch load"
                                                      , "batch load 2"
                                                      ]
                                                    ))
                                            , Right Nothing
                                            , Right
                                                (Just
                                                    ( "session-1"
                                                    , ["/compact"]
                                                    ))
                                            , Right
                                                (Just
                                                    ( "session-2"
                                                    , [ "batch load"
                                                      , "batch load 2"
                                                      ]
                                                    ))
                                            ]
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

    it "keeps compaction as a model checkpoint while paging visual history across it" $
        withSystemTempDirectory "ha" \stateDirectory -> do
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
                            let pool = trustedPool store
                                now = read "2026-08-23 12:00:00 UTC"
                                metadata = testMetadata now
                                checkpoint =
                                    (testTurn now)
                                        { sessionTurnUserText = "/compact"
                                        , sessionTurnEffect = TranscriptReplace
                                        }
                                appendTurn index effect =
                                    appendSessionTurn pool
                                        (checkpoint
                                            { sessionTurnUserText =
                                                "/question-" <> Text.pack (show index)
                                            , sessionTurnAssistantText =
                                                Just ("answer-" <> Text.pack (show index))
                                            , sessionTurnEffect = effect
                                            })
                                        metadata
                            createSession pool metadata
                                `shouldReturn` Right True
                            appendTurn (-1 :: Int) TranscriptAppend
                                `shouldReturn` Right True
                            appendTurn (0 :: Int) TranscriptAppend
                                `shouldReturn` Right True
                            appendSessionTurnIndexed pool checkpoint metadata
                                `shouldReturn` Right (Just 2)
                            appendTurn (1 :: Int) TranscriptAppend
                                `shouldReturn` Right True
                            appendTurn (2 :: Int) TranscriptAppend
                                `shouldReturn` Right True
                            appendTurn (3 :: Int) TranscriptAppend
                                `shouldReturn` Right True
                            loadActiveSession pool "session-1" >>= \case
                                Right (Just stored) ->
                                    map
                                        (\storedTurn ->
                                            storedTurn.storedTurn.sessionTurnUserText
                                        )
                                        (toList stored.storedTurns)
                                        `shouldBe`
                                            [ "/compact"
                                            , "/question-1"
                                            , "/question-2"
                                            , "/question-3"
                                            ]
                                other ->
                                    expectationFailure
                                        ("unexpected active session: " <> show other)
                            loadRecentSessionTurns pool "session-1" 2 >>= \case
                                Right (Just page) -> do
                                    map (.storedTurnIndex)
                                        (toList page.sessionPageTurns)
                                        `shouldBe` [4, 5]
                                    page.sessionPageGenerationStart
                                        `shouldBe` 0
                                    page.sessionPageTotal `shouldBe` 6
                                    page.sessionPageHasOlder `shouldBe` True
                                    page.sessionPageHasNewer `shouldBe` False
                                other ->
                                    expectationFailure
                                        ("unexpected recent page: " <> show other)
                            loadSessionTurnsBefore pool "session-1" 4 2 >>= \case
                                Right (Just page) -> do
                                    map (.storedTurnIndex)
                                        (toList page.sessionPageTurns)
                                        `shouldBe` [2, 3]
                                    page.sessionPageHasOlder `shouldBe` True
                                    page.sessionPageHasNewer `shouldBe` True
                                other ->
                                    expectationFailure
                                        ("unexpected before page: " <> show other)
                            loadSessionTurnsAfter pool "session-1" 3 2 >>= \case
                                Right (Just page) -> do
                                    map (.storedTurnIndex)
                                        (toList page.sessionPageTurns)
                                        `shouldBe` [4, 5]
                                    page.sessionPageHasOlder `shouldBe` True
                                    page.sessionPageHasNewer `shouldBe` False
                                other ->
                                    expectationFailure
                                        ("unexpected after page: " <> show other)
                            loadSessionTurnsBefore pool "session-1" 2 2 >>= \case
                                Right (Just page) -> do
                                    map (.storedTurnIndex)
                                        (toList page.sessionPageTurns)
                                        `shouldBe` [0, 1]
                                    page.sessionPageHasOlder `shouldBe` False
                                    page.sessionPageHasNewer `shouldBe` True
                                other ->
                                    expectationFailure
                                        ("unexpected pre-compact before page: "
                                            <> show other)
                            loadSessionTurnsAfter pool "session-1" 5 2 >>= \case
                                Right (Just page) -> do
                                    toList page.sessionPageTurns `shouldBe` []
                                    page.sessionPageHasOlder `shouldBe` True
                                    page.sessionPageHasNewer `shouldBe` False
                                other ->
                                    expectationFailure
                                        ("unexpected empty after page: " <> show other)
                            loadSessionResumeStats pool "session-1" >>= \case
                                Right (Just stats) -> do
                                    stats.sessionResumeTurnCount `shouldBe` 6
                                    stats.sessionResumeMessageCount `shouldBe` 12
                                    stats.sessionResumeToolCount `shouldBe` 12
                                    stats.sessionResumeFirstPrompt
                                        `shouldBe` Just "/question--1"
                                other ->
                                    expectationFailure
                                        ("unexpected resume stats: " <> show other)
                        )
                        (closeStore store)
                ) `finally` cleanup

    it "clips visual history at an explicit reset, not at compaction" $
        withSystemTempDirectory "ha" \stateDirectory -> do
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
                            let pool = trustedPool store
                                now = read "2026-08-23 12:00:00 UTC"
                                metadata = testMetadata now
                                append effect user =
                                    appendSessionTurn pool
                                        ((testTurn now)
                                            { sessionTurnUserText = user
                                            , sessionTurnAssistantText = Just "done"
                                            , sessionTurnEffect = effect
                                            })
                                        metadata
                            createSession pool metadata
                                `shouldReturn` Right True
                            append TranscriptAppend "/before-1"
                                `shouldReturn` Right True
                            append TranscriptReplace "/compact"
                                `shouldReturn` Right True
                            append TranscriptAppend "/after-compact"
                                `shouldReturn` Right True
                            append TranscriptReset "/clear"
                                `shouldReturn` Right True
                            append TranscriptAppend "/after-clear"
                                `shouldReturn` Right True
                            loadActiveSession pool "session-1" >>= \case
                                Right (Just stored) ->
                                    map
                                        (\storedTurn ->
                                            storedTurn.storedTurn.sessionTurnUserText
                                        )
                                        (toList stored.storedTurns)
                                        `shouldBe` ["/clear", "/after-clear"]
                                other ->
                                    expectationFailure
                                        ("unexpected active session: " <> show other)
                            loadRecentSessionTurns pool "session-1" 10 >>= \case
                                Right (Just page) -> do
                                    map
                                        (\storedTurn ->
                                            storedTurn.storedTurn.sessionTurnUserText
                                        )
                                        (toList page.sessionPageTurns)
                                        `shouldBe` ["/clear", "/after-clear"]
                                    page.sessionPageGenerationStart
                                        `shouldBe` 3
                                    page.sessionPageTotal `shouldBe` 2
                                    page.sessionPageHasOlder `shouldBe` False
                                other ->
                                    expectationFailure
                                        ("unexpected recent page: " <> show other)
                            loadSessionTurnsBefore pool "session-1" 3 10 >>= \case
                                Right (Just page) -> do
                                    toList page.sessionPageTurns `shouldBe` []
                                    page.sessionPageHasOlder `shouldBe` False
                                other ->
                                    expectationFailure
                                        ("unexpected before-reset page: "
                                            <> show other)
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
    , sessionMetadataLastRecap = Nothing
    , sessionMetadataLastTurnSummary = Nothing
    , sessionMetadataLastRecapMainTurns = 0
    }

testTurn :: UTCTime -> SessionTurn
testTurn now = SessionTurn
    { sessionTurnOccurredAt = now
    , sessionTurnUserText = "/compact"
    , sessionTurnAssistantText = Just "done"
    , sessionTurnError = Nothing
    , sessionTurnResponseId = Just "response-1"
    , sessionTurnEffect = TranscriptReplace
    , sessionTurnItems =
        [ StoredMessageItem StoredMessage
            { storedMessageProviderItemId = Just "item-message"
            , storedMessageContent = StoredMessageParts
                [ (emptyContentPart "input_text")
                    { storedContentPartText = Just "hello"
                    , storedContentPartPromptCacheBreakpoint =
                        Just (StoredOpaqueValue "{\"scope\":\"turn\"}")
                    }
                , (emptyContentPart "output_text")
                    { storedContentPartText = Just "hello back"
                    , storedContentPartAnnotations =
                        Just
                            (StoredOpaqueValue
                                "[{\"type\":\"citation\"}]")
                    , storedContentPartLogprobs =
                        Just
                            (StoredOpaqueValue
                                "[{\"token\":\"hello\"}]")
                    }
                , (emptyContentPart "provider_content")
                    { storedContentPartExtraFields =
                        StoredOpaqueObject
                            "{\"provider_extension\":true}"
                    }
                , (emptyContentPart "input_image")
                    { storedContentPartImageBinary =
                        Just StoredBinaryData
                            { storedBinaryDataMimeType = "image/png"
                            , storedBinaryDataBytes = "png-bytes"
                            }
                    }
                , (emptyContentPart "input_file")
                    { storedContentPartFilename = Just "notes.txt"
                    , storedContentPartFileBinary =
                        Just StoredBinaryData
                            { storedBinaryDataMimeType = "text/plain"
                            , storedBinaryDataBytes = "file-bytes"
                            }
                    }
                ]
            , storedMessageRole = "developer"
            , storedMessageStatus = Just "in_progress"
            , storedMessagePhase = Just "commentary"
            , storedMessageExtraFields =
                StoredOpaqueObject
                    "{\"provider_extension\":\"message\"}"
            }
        , StoredMessageItem StoredMessage
            { storedMessageProviderItemId = Just "item-text-message"
            , storedMessageContent = StoredMessageText "plain text"
            , storedMessageRole = "observer"
            , storedMessageStatus = Just "paused"
            , storedMessagePhase = Nothing
            , storedMessageExtraFields = emptyObject
            }
        , StoredFunctionCallItem StoredFunctionCall
            { storedFunctionCallProviderItemId = Just "item-call"
            , storedFunctionCallCallId = "call-1"
            , storedFunctionCallName = "shell_command"
            , storedFunctionCallArguments = "{\"command\":\"pwd\"}"
            , storedFunctionCallStatus = Just "completed"
            , storedFunctionCallExtraFields =
                StoredOpaqueObject
                    "{\"provider_extension\":\"function-call\"}"
            }
        , StoredFunctionCallOutputItem StoredFunctionCallOutput
            { storedFunctionCallOutputProviderItemId =
                Just "item-output"
            , storedFunctionCallOutputCallId = "call-1"
            , storedFunctionCallOutputValue = StoredToolOutput
                { storedToolOutputKind = StoredToolOutputEncoded
                , storedToolOutputText =
                    "{\"stdout\":\"/tmp/project\"}"
                }
            , storedFunctionCallOutputStatus = Just "completed"
            , storedFunctionCallOutputExtraFields =
                StoredOpaqueObject
                    "{\"provider_extension\":\"function-output\"}"
            }
        , StoredCustomToolCallItem StoredCustomToolCall
            { storedCustomToolCallProviderItemId =
                Just "item-custom-call"
            , storedCustomToolCallCallId = "custom-1"
            , storedCustomToolCallName = "apply_patch"
            , storedCustomToolCallInput = "*** Begin Patch"
            , storedCustomToolCallStatus = Just "in_progress"
            , storedCustomToolCallExtraFields = emptyObject
            }
        , StoredCustomToolCallOutputItem StoredCustomToolCallOutput
            { storedCustomToolCallOutputProviderItemId =
                Just "item-custom-output"
            , storedCustomToolCallOutputCallId = "custom-1"
            , storedCustomToolCallOutputName = Just "apply_patch"
            , storedCustomToolCallOutputValue = StoredToolOutput
                { storedToolOutputKind = StoredToolOutputText
                , storedToolOutputText = "Done"
                }
            , storedCustomToolCallOutputStatus = Just "completed"
            , storedCustomToolCallOutputExtraFields = emptyObject
            }
        , StoredReasoningItem StoredReasoning
            { storedReasoningProviderItemId = Just "item-reasoning"
            , storedReasoningSummary =
                [ StoredReasoningSummaryPart
                    { storedReasoningSummaryPartType = "summary_text"
                    , storedReasoningSummaryPartText =
                        Just "Checked the schema"
                    , storedReasoningSummaryPartExtraFields = emptyObject
                    }
                ]
            , storedReasoningContent =
                Just
                    [ (emptyContentPart "reasoning_text")
                        { storedContentPartText =
                            Just "private reasoning placeholder"
                        }
                    ]
            , storedReasoningEncryptedContent = Just "encrypted"
            , storedReasoningStatus = Just "completed"
            , storedReasoningExtraFields = emptyObject
            }
        , StoredItemReferenceItem StoredItemReference
            { storedItemReferenceProviderItemId = "item-call"
            , storedItemReferenceExtraFields = emptyObject
            }
        , StoredTaggedResponseItem StoredTaggedItem
            { storedTaggedItemRepresentation = StoredKnownRepresentation
            , storedTaggedItemWireTag = "compaction_trigger"
            , storedTaggedItemFields =
                StoredOpaqueObject
                    "{\"provider_extension\":\"known-tagged\"}"
            }
        , StoredTaggedResponseItem StoredTaggedItem
            { storedTaggedItemRepresentation = StoredUnknownRepresentation
            , storedTaggedItemWireTag = "provider_extension"
            , storedTaggedItemFields =
                StoredOpaqueObject
                    "{\"provider_extension\":\"unknown-tagged\"}"
            }
        ]
    , sessionTurnUsage = Just SessionUsage
        { sessionUsageInputTokens = 10
        , sessionUsageOutputTokens = 5
        , sessionUsageCachedTokens = 2
        }
    }

emptyObject :: StoredOpaqueObject
emptyObject = StoredOpaqueObject "{}"

emptyContentPart :: Text -> StoredContentPart
emptyContentPart partType = StoredContentPart
    { storedContentPartType = partType
    , storedContentPartText = Nothing
    , storedContentPartRefusal = Nothing
    , storedContentPartDetail = Nothing
    , storedContentPartFileData = Nothing
    , storedContentPartFileId = Nothing
    , storedContentPartFileUrl = Nothing
    , storedContentPartFilename = Nothing
    , storedContentPartImageUrl = Nothing
    , storedContentPartFileBinary = Nothing
    , storedContentPartImageBinary = Nothing
    , storedContentPartInputAudio = Nothing
    , storedContentPartPromptCacheBreakpoint = Nothing
    , storedContentPartAnnotations = Nothing
    , storedContentPartLogprobs = Nothing
    , storedContentPartExtraFields = emptyObject
    }

shouldContainBytes :: ByteString.ByteString -> ByteString.ByteString -> Expectation
shouldContainBytes haystack needle =
    ByteString.Char8.unpack haystack
        `shouldContain` ByteString.Char8.unpack needle

shouldNotContainBytes :: ByteString.ByteString -> ByteString.ByteString -> Expectation
shouldNotContainBytes haystack needle =
    ByteString.Char8.unpack haystack
        `shouldNotContain` ByteString.Char8.unpack needle
