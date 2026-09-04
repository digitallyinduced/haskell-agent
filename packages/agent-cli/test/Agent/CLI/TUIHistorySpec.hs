{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Agent.CLI.TUIHistorySpec (spec) where

import Agent.CLI.Session
    ( SessionTurn(..)
    , TranscriptEffect(..)
    )
import Agent.CLI.TUI.History
import Agent.Json (rawJsonFromEncoding)
import Agent.CLI.TUI.Composer (composerScrollbackAvailable)
import Agent.CLI.TUI.SessionHistory (sessionHistoryTurn)
import Agent.Responses.LoopBackend (toolResultToItem)
import Agent.Responses.Types
import Agent.ToolDispatch
    ( ToolCallKind(..)
    , ToolCallResult(..)
    , ToolResultImage(..)
    )
import qualified Data.Aeson as Aeson
import Data.Foldable (toList)
import Agent.TUI.Model
    ( BlockId(..)
    , BlockState(..)
    , BlockKind(..)
    , UiBlock(..)
    , UiEvent(..)
    , UiState(..)
    , initialUiState
    , reduceUi
    )
import qualified Data.Sequence as Seq
import Data.Int (Int64)
import qualified Data.Text as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Test.Hspec

spec :: Spec
spec = describe "bounded fullscreen history window" do
    it "allows keyboard scrollback when only persisted history is loaded" do
        let generation = HistoryGeneration 1
            empty = emptyHistoryWindow generation 10 20 1_000_000
            page =
                HistoryPage
                    { historyPageGeneration = generation
                    , historyPageDirection = HistoryNewer
                    , historyPageTurns = Seq.singleton (turn 1 1)
                    , historyPageGenerationStart = HistoryCursor 0
                    , historyPageTotalTurns = 2
                    , historyPageHasOlder = True
                    , historyPageHasNewer = False
                    }
        window <- expectRight (applyHistoryPage page empty)
        composerScrollbackAvailable initialUiState empty `shouldBe` False
        composerScrollbackAvailable initialUiState window `shouldBe` True

    it "accepts an initial page and requests the adjacent older cursor" do
        let generation = HistoryGeneration 4
            initial = emptyHistoryWindow generation 10 20 1_000_000
            page =
                HistoryPage
                    { historyPageGeneration = generation
                    , historyPageDirection = HistoryNewer
                    , historyPageTurns =
                        Seq.fromList
                            [ turn 10 1
                            , turn 11 1
                            , turn 12 1
                            ]
                    , historyPageGenerationStart = HistoryCursor 0
                    , historyPageTotalTurns = 13
                    , historyPageHasOlder = True
                    , historyPageHasNewer = False
                    }
            loaded = applyHistoryPage page initial
        loaded `shouldSatisfy` isRight
        case loaded of
            Left err -> expectationFailure (show err)
            Right window -> do
                historyWindowCursors window
                    `shouldBe` Seq.fromList (map HistoryCursor [10, 11, 12])
                historyWindowRequest HistoryOlder window
                    `shouldBe`
                        Just HistoryRequest
                            { historyRequestGeneration = generation
                            , historyRequestDirection = HistoryOlder
                            , historyRequestCursor = Just (HistoryCursor 10)
                            }

    it "deduplicates overlapping pages and blocks duplicate requests" do
        let generation = HistoryGeneration 1
            initial = emptyHistoryWindow generation 20 20 1_000_000
            first =
                HistoryPage
                    { historyPageGeneration = generation
                    , historyPageDirection = HistoryNewer
                    , historyPageTurns = Seq.fromList [turn 10 1, turn 11 1]
                    , historyPageGenerationStart = HistoryCursor 0
                    , historyPageTotalTurns = 12
                    , historyPageHasOlder = True
                    , historyPageHasNewer = False
                    }
            older =
                HistoryPage
                    { historyPageGeneration = generation
                    , historyPageDirection = HistoryOlder
                    , historyPageTurns = Seq.fromList [turn 8 1, turn 9 1, turn 10 4]
                    , historyPageGenerationStart = HistoryCursor 0
                    , historyPageTotalTurns = 12
                    , historyPageHasOlder = True
                    , historyPageHasNewer = True
                    }
        loaded <- expectRight (applyHistoryPage first initial)
        let requested = markHistoryRequest HistoryOlder loaded
        merged <- expectRight (applyHistoryPage older requested)
        historyWindowCursors merged
            `shouldBe` Seq.fromList (map HistoryCursor [8, 9, 10, 11])
        historyWindowLoadedBlocks merged `shouldBe` 7
        historyWindowRequest HistoryOlder requested
            `shouldBe` Nothing

    it "keeps older persisted turns requestable after appending a compaction summary" do
        let generation = HistoryGeneration 3
            initial = emptyHistoryWindow generation 200 400 1_000_000
            recent =
                HistoryPage
                    { historyPageGeneration = generation
                    , historyPageDirection = HistoryNewer
                    , historyPageTurns =
                        Seq.fromList [turn 10 1, turn 11 1, turn 12 1]
                    , historyPageGenerationStart = HistoryCursor 0
                    , historyPageTotalTurns = 13
                    , historyPageHasOlder = True
                    , historyPageHasNewer = False
                    }
        window <- expectRight (applyHistoryPage recent initial)
        let compacted = appendHistoryTurn (turn 13 1) window
        historyWindowCursors compacted
            `shouldBe` Seq.fromList (map HistoryCursor [10, 11, 12, 13])
        compacted.historyWindowTotalTurns `shouldBe` 14
        historyWindowOlderAvailable compacted `shouldBe` True
        historyWindowRequest HistoryOlder compacted
            `shouldBe`
                Just HistoryRequest
                    { historyRequestGeneration = generation
                    , historyRequestDirection = HistoryOlder
                    , historyRequestCursor = Just (HistoryCursor 10)
                    }

    it "resets a non-tail window before archiving a new latest turn" do
        let generation = HistoryGeneration 7
            initial = emptyHistoryWindow generation 200 400 1_000_000
            middlePage =
                HistoryPage
                    { historyPageGeneration = generation
                    , historyPageDirection = HistoryNewer
                    , historyPageTurns =
                        Seq.fromList [turn 100 1, turn 101 1]
                    , historyPageGenerationStart = HistoryCursor 0
                    , historyPageTotalTurns = 500
                    , historyPageHasOlder = True
                    , historyPageHasNewer = True
                    }
        middle <- expectRight (applyHistoryPage middlePage initial)
        let latest = appendHistoryTurn (turn 500 1) middle
        historyWindowCursors latest
            `shouldBe` Seq.singleton (HistoryCursor 500)
        latest.historyWindowTotalTurns `shouldBe` 501
        historyWindowOlderAvailable latest `shouldBe` True
        historyWindowNewerAvailable latest `shouldBe` False

    it "does not let a stale failure clear a current page request" do
        let currentGeneration = HistoryGeneration 8
            initial =
                (emptyHistoryWindow currentGeneration 10 20 1_000_000)
                    { historyWindowHasOlder = True
                    }
            currentRequest =
                HistoryRequest
                    { historyRequestGeneration = currentGeneration
                    , historyRequestDirection = HistoryOlder
                    , historyRequestCursor = Nothing
                    }
            staleRequest =
                currentRequest
                    { historyRequestGeneration = HistoryGeneration 7
                    }
            pending = markHistoryRequest HistoryOlder initial
        historyWindowRequest HistoryOlder
            (clearHistoryRequest staleRequest pending)
            `shouldBe` Nothing
        historyWindowRequest HistoryOlder
            (clearHistoryRequest currentRequest pending)
            `shouldBe` Just currentRequest

    it "rejects a page from an old provider or session generation" do
        let current =
                emptyHistoryWindow (HistoryGeneration 9) 10 20 1_000_000
            stale =
                HistoryPage
                    { historyPageGeneration = HistoryGeneration 8
                    , historyPageDirection = HistoryNewer
                    , historyPageTurns = Seq.singleton (turn 1 1)
                    , historyPageGenerationStart = HistoryCursor 0
                    , historyPageTotalTurns = 2
                    , historyPageHasOlder = False
                    , historyPageHasNewer = False
                    }
        applyHistoryPage stale current
            `shouldBe` Left (HistoryPageStale (HistoryGeneration 8))

    it "evicts oldest completed turns to stay within budget" do
        let generation = HistoryGeneration 2
            initial = emptyHistoryWindow generation 2 100 1_000_000
            page =
                HistoryPage
                    { historyPageGeneration = generation
                    , historyPageDirection = HistoryNewer
                    , historyPageTurns =
                        Seq.fromList [turn 1 1, turn 2 1, turn 3 1]
                    , historyPageGenerationStart = HistoryCursor 0
                    , historyPageTotalTurns = 4
                    , historyPageHasOlder = False
                    , historyPageHasNewer = False
                    }
        window <- expectRight (applyHistoryPage page initial)
        historyWindowCursors window
            `shouldBe` Seq.fromList (map HistoryCursor [2, 3])
        historyWindowOlderAvailable window `shouldBe` True

    it "does not evict the visible or selected anchor" do
        let generation = HistoryGeneration 3
            initial = emptyHistoryWindow generation 1 100 1_000_000
            firstPage =
                HistoryPage
                    { historyPageGeneration = generation
                    , historyPageDirection = HistoryNewer
                    , historyPageTurns = Seq.singleton (turn 1 1)
                    , historyPageGenerationStart = HistoryCursor 0
                    , historyPageTotalTurns = 3
                    , historyPageHasOlder = False
                    , historyPageHasNewer = False
                    }
            nextPage =
                HistoryPage
                    { historyPageGeneration = generation
                    , historyPageDirection = HistoryNewer
                    , historyPageTurns = Seq.singleton (turn 2 1)
                    , historyPageGenerationStart = HistoryCursor 0
                    , historyPageTotalTurns = 3
                    , historyPageHasOlder = True
                    , historyPageHasNewer = False
                    }
        window <- expectRight (applyHistoryPage firstPage initial)
        let pinned =
                historyWindowSetAnchors
                    (Just (HistoryCursor 1))
                    Nothing
                    window
        trimmed <- expectRight (applyHistoryPage nextPage pinned)
        historyWindowCursors trimmed
            `shouldBe` Seq.singleton (HistoryCursor 1)

    it "drops anchors that no longer belong to the loaded window" do
        let generation = HistoryGeneration 5
            initial = emptyHistoryWindow generation 10 10 1_000_000
            page =
                HistoryPage
                    { historyPageGeneration = generation
                    , historyPageDirection = HistoryNewer
                    , historyPageTurns = Seq.singleton (turn 4 1)
                    , historyPageGenerationStart = HistoryCursor 0
                    , historyPageTotalTurns = 5
                    , historyPageHasOlder = False
                    , historyPageHasNewer = False
                    }
        window <- expectRight (applyHistoryPage page initial)
        let anchored =
                historyWindowSetAnchors
                    (Just (HistoryCursor 99))
                    (Just (HistoryCursor 4))
                    window
        historyWindowVisible anchored `shouldBe` Nothing
        historyWindowSelected anchored
            `shouldBe` Just (HistoryCursor 4)

    it "evicts completed turns when the estimated byte budget is exceeded" do
        let generation = HistoryGeneration 6
            initial = emptyHistoryWindow generation 100 100 180
            page =
                HistoryPage
                    { historyPageGeneration = generation
                    , historyPageDirection = HistoryNewer
                    , historyPageTurns =
                        Seq.fromList [turn 1 1, turn 2 1]
                    , historyPageGenerationStart = HistoryCursor 0
                    , historyPageTotalTurns = 3
                    , historyPageHasOlder = False
                    , historyPageHasNewer = False
                    }
        window <- expectRight (applyHistoryPage page initial)
        historyWindowCursors window
            `shouldBe` Seq.singleton (HistoryCursor 2)
        historyWindowLoadedBytes window `shouldSatisfy` (<= 180)

    it "keeps an oversized completed turn visible after archiving its live blocks" do
        let generation = HistoryGeneration 6
            initial = emptyHistoryWindow generation 100 100 180
            page =
                HistoryPage
                    { historyPageGeneration = generation
                    , historyPageDirection = HistoryNewer
                    , historyPageTurns = Seq.singleton (turn 1 1)
                    , historyPageGenerationStart = HistoryCursor 0
                    , historyPageTotalTurns = 2
                    , historyPageHasOlder = True
                    , historyPageHasNewer = False
                    }
        loaded <- expectRight (applyHistoryPage page initial)
        let completed = appendHistoryTurn (turn 2 2) loaded
        historyWindowCursors completed
            `shouldBe` Seq.singleton (HistoryCursor 2)
        historyWindowLoadedBytes completed `shouldSatisfy` (> 180)
        historyWindowOlderAvailable completed `shouldBe` True

    it "omits persisted reasoning while projecting tool and assistant history" do
        let projected =
                sessionHistoryTurn
                    (12 :: Int)
                    (sessionTurn
                        TranscriptAppend
                        "inspect the repository"
                        [ userMessage "inspect the repository"
                        , ReasoningItemValue ReasoningItem
                            { itemId = Nothing
                            , summary =
                                [ ReasoningSummaryPart
                                    { partType = "summary_text"
                                    , text =
                                        Just
                                            "Don't mention skills. Brief summary for the user."
                                    }
                                ]
                            , content = Nothing
                            , encryptedContent = Nothing
                            , status = Nothing
                            }
                        , FunctionCallItem FunctionCall
                            { itemId = Nothing
                            , callId = "call-1"
                            , name = "shell_command"
                            , namespace = Nothing
                            , provider = Nothing
                            , arguments = "{\"command\":\"pwd\"}"
                            , encryptedFunctionArgs = Nothing
                            , status = Nothing
                            }
                        , FunctionCallOutputItem FunctionCallOutput
                            { itemId = Nothing
                            , callId = "call-1"
                            , name = Nothing
                            , namespace = Nothing
                            , provider = Nothing
                            , output = rawJsonFromEncoding
                                (Aeson.toEncoding ("/tmp/project" :: Text.Text))
                            , status = Nothing
                            }
                        , assistantMessage "Done"
                        ])
            blocks = toList projected.historyTurnBlocks
        map (.blockKind) blocks
            `shouldBe`
                [ BlockUser
                , BlockShell
                , BlockAssistant
                ]
        map (.blockBody) blocks
            `shouldSatisfy`
                any (Text.isInfixOf "/tmp/project")
        map (.blockBody) blocks
            `shouldSatisfy`
                all (not . Text.isInfixOf "Don't mention skills")

    it "does not render persisted image payloads as tool output" do
        let summary = "Viewed image file: example.png"
            payloadMarker = "VERY_SECRET_IMAGE_BYTES"
            imageOutput =
                toolResultToItem
                    (ToolCallResultWithImages
                        "image-call"
                        summary
                        FunctionCallKind
                        [ ToolResultImage
                            ("data:image/png;base64," <> payloadMarker)
                            (Just "high")
                        ])
            rendered = projectedToolResult imageOutput
        rendered `shouldSatisfy` Text.isInfixOf summary
        rendered `shouldSatisfy` (not . Text.isInfixOf payloadMarker)
        rendered `shouldSatisfy` (not . Text.isInfixOf "base64")
        rendered `shouldSatisfy` (not . Text.isInfixOf "\"input_image\"")

    it "fails closed for schema-drifted persisted media output" do
        let summary = "Viewed schema-drifted image"
            payloadMarker = "SCHEMA_DRIFT_IMAGE_BYTES"
            rendered =
                projectedToolResult $
                    toolOutputItem
                        [ Aeson.object
                            [ "type" Aeson..= ("input_image" :: Text.Text)
                            , "image_url" Aeson..= (7 :: Int)
                            , "payload" Aeson..=
                                ("data:image/png;base64," <> payloadMarker)
                            ]
                        , Aeson.object
                            [ "type" Aeson..= ("input_text" :: Text.Text)
                            , "text" Aeson..= summary
                            ]
                        ]
        rendered `shouldSatisfy` Text.isInfixOf summary
        rendered `shouldSatisfy` (not . Text.isInfixOf payloadMarker)
        rendered `shouldSatisfy` (not . Text.isInfixOf "base64")

    it "fails closed for future persisted media content parts" do
        let summary = "Viewed future image"
            payloadMarker = "FUTURE_IMAGE_BYTES"
            rendered =
                projectedToolResult $
                    toolOutputItem
                        [ Aeson.object
                            [ "type" Aeson..= ("future_media" :: Text.Text)
                            , "payload" Aeson..=
                                (" DATA:image/png;BASE64," <> payloadMarker)
                            ]
                        , Aeson.object
                            [ "type" Aeson..= ("input_text" :: Text.Text)
                            , "text" Aeson..= summary
                            ]
                        ]
        rendered `shouldSatisfy` Text.isInfixOf summary
        rendered `shouldSatisfy` (not . Text.isInfixOf payloadMarker)
        rendered `shouldSatisfy` (not . Text.isInfixOf "BASE64")

    it "does not trust text fields inside persisted media arrays" do
        let payloadMarker = "TEXT_FIELD_IMAGE_BYTES"
            rendered =
                projectedToolResult $
                    toolOutputItem
                        [ Aeson.object
                            [ "type" Aeson..= ("input_image" :: Text.Text)
                            , "image_url" Aeson..=
                                ("https://example.test/image.png" :: Text.Text)
                            ]
                        , Aeson.object
                            [ "type" Aeson..= ("input_text" :: Text.Text)
                            , "text" Aeson..=
                                ("data:image/png;base64," <> payloadMarker)
                            ]
                        ]
        rendered `shouldSatisfy` Text.isInfixOf "[image]"
        rendered `shouldSatisfy` (not . Text.isInfixOf payloadMarker)
        rendered `shouldSatisfy` (not . Text.isInfixOf "base64")

    it "preserves partial assistant output in a cancelled durable turn" do
        let turnValue =
                (sessionTurn TranscriptAppend "stop here"
                    [userMessage "stop here"])
                    { turnAssistantText = Just "already visible"
                    , turnError = Just "cancelled"
                    }
            blocks = toList $
                (sessionHistoryTurn (19 :: Int) turnValue).historyTurnBlocks
        map (\block -> (block.blockKind, block.blockBody, block.blockState)) blocks
            `shouldBe`
                [ (BlockUser, "stop here", BlockComplete)
                , (BlockAssistant, "already visible", BlockComplete)
                , (BlockError, "cancelled", BlockFailed)
                ]

    it "restores failed-attempt tools from display-only durable items" do
        let call callId name arguments =
                FunctionCallItem FunctionCall
                    { itemId = Nothing
                    , callId
                    , name
                    , namespace = Nothing
                    , provider = Nothing
                    , arguments
                    , encryptedFunctionArgs = Nothing
                    , status = Just ItemInProgress
                    }
            output callId body status =
                FunctionCallOutputItem FunctionCallOutput
                    { itemId = Nothing
                    , callId
                    , name = Nothing
                    , namespace = Nothing
                    , provider = Nothing
                    , output =
                        rawJsonFromEncoding
                            (Aeson.toEncoding (body :: Text.Text))
                    , status = Just status
                    }
            turnValue =
                (sessionTurn TranscriptAppend "fix it" [userMessage "fix it"])
                    { turnAssistantText = Just "Work completed before timeout."
                    , turnDisplayItems =
                        [ assistantMessage "Work completed before timeout."
                        , call "done" "shell_command"
                            "{\"command\":\"git status\"}"
                        , output "done" "clean" ItemCompleted
                        , call "waiting" "TaskOutput"
                            "{\"task_id\":\"ci\"}"
                        , output "waiting" "still queued" ItemIncomplete
                        ]
                    , turnError = Just "Claude Code timed out."
                    }
            blocks = toList $
                (sessionHistoryTurn (22 :: Int) turnValue).historyTurnBlocks
            assistantBlocks =
                filter ((== BlockAssistant) . (.blockKind)) blocks
            toolBlocks =
                [ (block.blockCallId, block.blockState, block.blockBody)
                | block <- blocks
                , block.blockCallId /= Nothing
                ]
        map (\block -> (block.blockBody, block.blockState)) assistantBlocks
            `shouldBe`
                [("Work completed before timeout.", BlockFailed)]
        map (\(callId, state, _) -> (callId, state)) toolBlocks
            `shouldBe`
                [ (Just "done", BlockComplete)
                , (Just "waiting", BlockFailed)
                ]
        toolBlocks
            `shouldSatisfy`
                any (\(_, _, body) -> "still queued" `Text.isInfixOf` body)

    it "keeps retry attempts separate when a provider reuses a tool id" do
        let call arguments =
                FunctionCallItem FunctionCall
                    { itemId = Nothing
                    , callId = "same"
                    , name = "shell_command"
                    , namespace = Nothing
                    , provider = Nothing
                    , arguments
                    , encryptedFunctionArgs = Nothing
                    , status = Just ItemInProgress
                    }
            output body status =
                FunctionCallOutputItem FunctionCallOutput
                    { itemId = Nothing
                    , callId = "same"
                    , name = Nothing
                    , namespace = Nothing
                    , provider = Nothing
                    , output =
                        rawJsonFromEncoding
                            (Aeson.toEncoding (body :: Text.Text))
                    , status = Just status
                    }
            boundary =
                UnknownResponseItem
                    (TaggedObject "haskell_agent_display_attempt_boundary")
            turnValue =
                (sessionTurn TranscriptAppend "retry" [userMessage "retry"])
                    { turnDisplayItems =
                        [ assistantMessage "first attempt"
                        , call "{\"command\":\"first\"}"
                        , output "first output" ItemCompleted
                        , boundary
                        , assistantMessage "second attempt"
                        , call "{\"command\":\"second\"}"
                        , output "second output" ItemIncomplete
                        ]
                    , turnError = Just "provider failed"
                    }
            blocks = toList $
                (sessionHistoryTurn (23 :: Int) turnValue).historyTurnBlocks
            assistantBlocks =
                [ (block.blockBody, block.blockState)
                | block <- blocks
                , block.blockKind == BlockAssistant
                ]
            toolBlocks =
                [ (block.blockBody, block.blockState)
                | block <- blocks
                , block.blockCallId == Just "same"
                ]
        assistantBlocks
            `shouldBe`
                [ ("first attempt", BlockComplete)
                , ("second attempt", BlockFailed)
                ]
        map snd toolBlocks
            `shouldBe` [BlockComplete, BlockFailed]
        map fst toolBlocks
            `shouldSatisfy`
                \bodies ->
                    any (Text.isInfixOf "first output") bodies
                        && any (Text.isInfixOf "second output") bodies

    it "shows the uncommitted partial text after the retained steps of a cancelled turn" do
        let turnValue =
                (sessionTurn TranscriptAppend "stop here"
                    [ userMessage "stop here"
                    , assistantMessage "checking first"
                    , FunctionCallItem FunctionCall
                        { itemId = Nothing
                        , callId = "call-1"
                        , name = "shell_command"
                        , namespace = Nothing
                        , provider = Nothing
                        , arguments = "{\"command\":\"pwd\"}"
                        , encryptedFunctionArgs = Nothing
                        , status = Nothing
                        }
                    , FunctionCallOutputItem FunctionCallOutput
                        { itemId = Nothing
                        , callId = "call-1"
                        , name = Nothing
                        , namespace = Nothing
                        , provider = Nothing
                        , output = rawJsonFromEncoding
                            (Aeson.toEncoding ("/tmp/project" :: Text.Text))
                        , status = Nothing
                        }
                    ])
                    { turnAssistantText = Just "already visible"
                    , turnError = Just "cancelled"
                    }
            blocks = toList $
                (sessionHistoryTurn (20 :: Int) turnValue).historyTurnBlocks
            summarize block =
                ( block.blockKind
                , if block.blockKind == BlockShell then "" else block.blockBody
                , block.blockState
                )
        map summarize blocks
            `shouldBe`
                [ (BlockUser, "stop here", BlockComplete)
                , (BlockAssistant, "checking first", BlockComplete)
                , (BlockShell, "", BlockComplete)
                , (BlockAssistant, "already visible", BlockComplete)
                , (BlockError, "cancelled", BlockFailed)
                ]

    it "does not repeat the committed text of an incomplete response" do
        let turnValue =
                (sessionTurn TranscriptAppend "explain"
                    [userMessage "explain", assistantMessage "partial answer"])
                    { turnAssistantText = Just "partial answer"
                    , turnError = Just "Response incomplete: max_output_tokens."
                    }
            blocks = toList $
                (sessionHistoryTurn (21 :: Int) turnValue).historyTurnBlocks
        map (\block -> (block.blockKind, block.blockBody, block.blockState)) blocks
            `shouldBe`
                [ (BlockUser, "explain", BlockComplete)
                , (BlockAssistant, "partial answer", BlockComplete)
                , ( BlockError
                  , "Response incomplete: max_output_tokens."
                  , BlockFailed
                  )
                ]

    it "does not rematerialise the compacted prefix of replacement turns" do
        let projected =
                sessionHistoryTurn
                    (20 :: Int)
                    (sessionTurn
                        TranscriptReplace
                        "current prompt"
                        [ assistantMessage "old compacted answer"
                        , userMessage "current prompt"
                        , assistantMessage "current answer"
                        ])
            blocks = toList projected.historyTurnBlocks
        map (.blockKind) blocks `shouldBe` [BlockUser, BlockAssistant]
        map (.blockBody) blocks
            `shouldBe` ["current prompt", "current answer"]

    it "keeps mid-turn steering in durable history" do
        let projected =
                sessionHistoryTurn
                    (21 :: Int)
                    (sessionTurn
                        TranscriptAppend
                        "inspect the repository"
                        [ userMessage "generated skill context"
                        , userMessage "inspect the repository"
                        , assistantMessage "I will inspect it."
                        , userMessage "# Skill instructions: parser\nhidden"
                        , userMessage "focus on the parser"
                        , assistantMessage "Done"
                        ])
            blocks = toList projected.historyTurnBlocks
        map (.blockKind) blocks
            `shouldBe`
                [ BlockUser
                , BlockAssistant
                , BlockUser
                , BlockAssistant
                ]
        map (.blockBody) blocks
            `shouldBe`
                [ "inspect the repository"
                , "I will inspect it."
                , "focus on the parser"
                , "Done"
                ]

    it "drops a live compact summary that was rendered without a user turn" do
        let asked =
                reduceUi (UiAssistantHistory "answer") $
                    reduceUi (UiUserSubmitted "question") initialUiState
            live =
                reduceUi
                    (UiSystemMessage "Earlier conversation summary")
                    asked
            durable =
                sessionHistoryTurn
                    (30 :: Int)
                    ((sessionTurn TranscriptReplace "/compact" [])
                        { turnAssistantText =
                            Just "Earlier conversation summary"
                        })
        unarchivedLiveStart live.uiBlocks durable.historyTurnBlocks
            `shouldBe` Seq.length asked.uiBlocks

    it "keeps live blocks when the durable turn is not already on screen" do
        let live =
                reduceUi (UiUserSubmitted "question") initialUiState
            durable =
                sessionHistoryTurn
                    (1 :: Int)
                    (sessionTurn TranscriptAppend "other" [])
        unarchivedLiveStart live.uiBlocks durable.historyTurnBlocks
            `shouldBe` Seq.length live.uiBlocks

    it "renders manual compaction as a single checkpoint summary" do
        let turnValue =
                (sessionTurn TranscriptReplace "/compact" [])
                    { turnAssistantText = Just "Earlier conversation summary"
                    }
            projected = sessionHistoryTurn (30 :: Int) turnValue
            blocks = toList projected.historyTurnBlocks
        map (.blockKind) blocks `shouldBe` [BlockSystem]
        map (.blockBody) blocks `shouldBe` ["Earlier conversation summary"]

turn :: Int64 -> Int -> HistoryTurn
turn cursor blocks =
    HistoryTurn
        { historyTurnCursor = HistoryCursor cursor
        , historyTurnBlocks =
            Seq.fromList
                [ block (fromIntegral cursor * 10 + index)
                | index <- [0 .. blocks - 1]
                ]
        }

block :: Int -> UiBlock
block identifier =
    UiBlock
        { blockId = BlockId identifier
        , blockKind = BlockAssistant
        , blockTitle = "Assistant"
        , blockBody = "body"
        , blockTimestamp = ""
        , blockDetail = ""
        , blockState = BlockComplete
        , blockExpanded = False
        , blockCallId = Nothing
        , blockInspectionGroupable = False
        }

isRight :: Either a b -> Bool
isRight value =
    case value of
        Left _ -> False
        Right _ -> True

expectRight :: (Show a) => Either a b -> IO b
expectRight value =
    case value of
        Left err -> expectationFailure (show err) >> fail "unexpected history page rejection"
        Right result -> pure result

sessionTurn
    :: TranscriptEffect
    -> Text.Text
    -> [ResponseItem]
    -> SessionTurn
sessionTurn effect userText items =
    SessionTurn
        { turnAt = fixedTime
        , turnUserText = userText
        , turnAssistantText = Nothing
        , turnError = Nothing
        , turnResponseId = Nothing
        , turnEffect = effect
        , turnItems = items
        , turnDisplayItems = []
        , turnUsage = Nothing
        , turnProviderTelemetry = []
        }

userMessage :: Text.Text -> ResponseItem
userMessage text =
    MessageItem ResponseMessage
        { messageId = Nothing
        , content = MessageContentText text
        , role = RoleUser
        , status = Nothing
        , phase = Nothing
        , passthrough = Nothing
        }

assistantMessage :: Text.Text -> ResponseItem
assistantMessage text =
    MessageItem ResponseMessage
        { messageId = Nothing
        , content = MessageContentText text
        , role = RoleAssistant
        , status = Nothing
        , phase = Nothing
        , passthrough = Nothing
        }

toolOutputItem :: [Aeson.Value] -> ResponseItem
toolOutputItem parts =
    FunctionCallOutputItem FunctionCallOutput
        { itemId = Nothing
        , callId = "image-call"
        , name = Nothing
        , namespace = Nothing
        , provider = Nothing
        , output = rawJsonFromEncoding (Aeson.toEncoding parts)
        , status = Nothing
        }

projectedToolResult :: ResponseItem -> Text.Text
projectedToolResult outputItem =
    Text.intercalate "\n" $
        map (.blockBody) (toList projected.historyTurnBlocks)
  where
    projected =
        sessionHistoryTurn
            (13 :: Int)
            (sessionTurn
                TranscriptAppend
                "inspect the image"
                [ userMessage "inspect the image"
                , FunctionCallItem FunctionCall
                    { itemId = Nothing
                    , callId = "image-call"
                    , name = "view_image"
                    , namespace = Nothing
                    , provider = Nothing
                    , arguments = "{\"path\":\"example.png\"}"
                    , encryptedFunctionArgs = Nothing
                    , status = Nothing
                    }
                , outputItem
                ])

fixedTime :: UTCTime
fixedTime =
    UTCTime
        (fromGregorian 2026 8 25)
        (secondsToDiffTime 0)
