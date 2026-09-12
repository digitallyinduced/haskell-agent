module Main (main) where

import Agent.Loop.Output
import Agent.Loop.DisplayJournal
import Agent.Loop.VisibleState
import Agent.ToolDispatch
    ( functionToolCall, setToolCallArguments, ToolCallResult(..)
    , ToolCallKind(..), ToolCallMode(..)
    )
import Test.Hspec

main :: IO ()
main = hspec $ describe "VisibleLoopState event sequences" do
    it "starts empty and ignores live-only events" do
        let state = events [ReasoningDelta "private", ResponseAttemptFailed]
        visibleAssistantText state `shouldBe` Nothing
        visibleDisplayEvents state `shouldBe` []
        state.providerAttemptActive `shouldBe` False

    it "preserves chunk and attempt order across restarts" do
        let state = events
                [ TurnStarted, TextDelta "a", TextDelta "b"
                , ResponseRestarted "retry", TextDelta "c", TextDelta "d"
                , ResponseRestarted "again", TextDelta "e"
                ]
        visibleAssistantText state `shouldBe` Just "ab\n\ncd\n\ne"
        visibleDisplayEvents state `shouldBe`
            [ TextDelta "ab", ResponseRestarted "retry"
            , TextDelta "cd", ResponseRestarted "again", TextDelta "e"
            ]
        state.providerAttemptActive `shouldBe` True

    it "discards only the current attempt and accepts recovery output" do
        let state = events
                [ TurnStarted, TextDelta "prior", ResponseRestarted "retry"
                , TextDelta "discard", ToolStarted call
                , ResponseAttemptDiscarded, ResponseAttemptDiscarded
                , TextDelta "recovered", ResponseAttemptFailed
                ]
        visibleAssistantText state `shouldBe` Just "prior\n\nrecovered"
        visibleDisplayEvents state `shouldBe`
            [TextDelta "prior", ResponseRestarted "retry", TextDelta "recovered"]

    it "does not create empty text attempts or separators" do
        let state = events
                [ TurnStarted, ResponseRestarted "empty"
                , TextDelta "", ResponseRestarted "empty chunk"
                , TextDelta "kept", ResponseRestarted "retry"
                , TextDelta "", ResponseAttemptDiscarded
                ]
        visibleAssistantText state `shouldBe` Just "kept"
        state.currentTextChunks `shouldBe` []
        state.finishedTextAttempts `shouldBe` [["kept"], [""]]

    it "gates tool display on provider activity but not text accumulation" do
        let state = events
                [ ToolStarted call, TextDelta "before"
                , TurnStarted, TextDelta "during", ToolStarted call
                , TurnFinished (emptyTurnOutput "response" [] Nothing)
                , ToolOutputUpdated "c1" "late", TextDelta "after"
                ]
        visibleAssistantText state `shouldBe` Just "beforeduringafter"
        visibleDisplayEvents state `shouldBe` []
        state.providerAttemptActive `shouldBe` False

    it "starts a fresh journal without resetting uncommitted text" do
        let state = events
                [ TurnStarted, TextDelta "old", ToolStarted call
                , TurnStarted, TextDelta "new"
                ]
        visibleAssistantText state `shouldBe` Just "oldnew"
        visibleDisplayEvents state `shouldBe` [TextDelta "new"]

    it "keeps restart and discard boundaries even outside an active provider" do
        let state = events
                [ TextDelta "prior", ResponseRestarted "retry"
                , TextDelta "discard", ResponseAttemptDiscarded
                ]
        visibleAssistantText state `shouldBe` Just "prior"
        visibleDisplayEvents state `shouldBe` [ResponseRestarted "retry"]

    it "scopes tool retraction to the current attempt when IDs repeat" do
        let state = events
                [ TurnStarted, ToolStarted call, TextDelta "prior"
                , ResponseRestarted "retry", ToolStarted call
                , ToolOutputUpdated "c1" "removed", ToolRetracted "c1"
                , TextDelta "current"
                ]
        visibleDisplayEvents state `shouldBe`
            [ToolStarted call, TextDelta "prior", ResponseRestarted "retry", TextDelta "current"]
        visibleAssistantText state `shouldBe` Just "prior\n\ncurrent"

    it "keeps only the latest alternating argument snapshot" do
        let first = setToolCallArguments "partial" call
            latest = setToolCallArguments "complete" call
        visibleDisplayEvents (events
            [TurnStarted, ToolStarted call, ToolUpdated first,
             ToolArgumentsUpdated latest, ToolUpdated latest]) `shouldBe`
            [ToolStarted call, ToolUpdated latest]

    it "does not collapse text boundaries while superseding snapshots" do
        visibleDisplayEvents (events
            [ TurnStarted, TextDelta "before", ToolOutputUpdated "c1" "old"
            , TextDelta "between", ToolOutputUpdated "c1" "new"
            , ToolOutputUpdated "c1" "latest", TextDelta "after"
            ]) `shouldBe`
            [ TextDelta "before", TextDelta "between"
            , ToolOutputUpdated "c1" "latest", TextDelta "after"
            ]

    it "retains other calls and earlier attempts when snapshots repeat" do
        visibleDisplayEvents (events
            [ TurnStarted, ToolOutputUpdated "c1" "prior"
            , ResponseRestarted "retry", ToolOutputUpdated "c1" "first"
            , ToolOutputUpdated "c2" "other", ToolOutputUpdated "c1" "second"
            , ToolOutputUpdated "c1" "latest", ToolRetracted "c1"
            ]) `shouldBe`
            [ ToolOutputUpdated "c1" "prior", ResponseRestarted "retry"
            , ToolOutputUpdated "c2" "other"
            ]

    it "preserves repeated finishes and output arriving after the final finish" do
        let finished output = ToolCallResult
                { callId = "c1", output, callKind = FunctionCallKind
                , toolResultMode = BlockingToolCall, toolResultImages = []
                , toolResultOutcome = Nothing
                }
            first = finished "first"
            second = finished "second"
        visibleDisplayEvents (events
            [ TurnStarted, ToolOutputUpdated "c1" "old", ToolFinished first
            , ToolOutputUpdated "c1" "between", ToolFinished second
            , ToolOutputUpdated "c1" "late", ToolOutputUpdated "c1" "latest"
            ]) `shouldBe`
            [ToolFinished first, ToolFinished second, ToolOutputUpdated "c1" "latest"]

    it "uses an empty snapshot at commit before completion is painted" do
        -- The IO commit checkpoint installs this value directly, not via a
        -- TurnFinished event (which deliberately does not clear text).
        let committed = emptyVisibleLoopState
            lateTool = recordVisibleLoopEvent (ToolOutputUpdated "c1" "late") committed
            next = foldl' (flip recordVisibleLoopEvent) lateTool
                [TurnStarted, TextDelta "next"]
        visibleAssistantText lateTool `shouldBe` Nothing
        visibleDisplayEvents lateTool `shouldBe` []
        lateTool.providerAttemptActive `shouldBe` False
        visibleAssistantText next `shouldBe` Just "next"
        visibleDisplayEvents next `shouldBe` [TextDelta "next"]

    it "discards snapshots without forcing superseded output" do
        let old = error "discarded output was evaluated"
            state = events
                [ TurnStarted, ToolOutputUpdated "c1" "prior"
                , ResponseRestarted "retry", ToolUpdated call
                , ToolOutputUpdated "c1" old, ResponseAttemptDiscarded
                ]
        visibleDisplayEvents state `shouldBe`
            [ToolOutputUpdated "c1" "prior", ResponseRestarted "retry"]

    it "projects the latest output without evaluating its obsolete predecessor" do
        let journal = recordDisplayEvent
                (ToolOutputUpdated "c1" (error "superseded output was evaluated"))
                emptyDisplayJournal
        journal `seq` pure ()
        displayEventsFromJournal
            (recordDisplayEvent (ToolOutputUpdated "c1" "latest") journal)
            `shouldBe` [ToolOutputUpdated "c1" "latest"]

    it "projects a finish without evaluating the output snapshot it suppresses" do
        let journal = recordDisplayEvent
                (ToolOutputUpdated "c1" (error "suppressed output was evaluated"))
                emptyDisplayJournal
            finished = ToolCallResult
                { callId = "c1", output = "finished", callKind = FunctionCallKind
                , toolResultMode = BlockingToolCall, toolResultImages = []
                , toolResultOutcome = Nothing
                }
        journal `seq` pure ()
        displayEventsFromJournal (recordDisplayEvent (ToolFinished finished) journal)
            `shouldBe` [ToolFinished finished]

    it "keeps surviving output payloads lazy across repeated unrelated retractions" do
        let journal = recordDisplayEvent
                (ToolOutputUpdated "c1" (error "surviving output was evaluated"))
                emptyDisplayJournal
            retracted = recordDisplayEvent (ToolRetracted "other") journal
            retractedAgain = recordDisplayEvent (ToolRetracted "another") retracted
        journal `seq` pure ()
        retracted `seq` pure ()
        retractedAgain `seq` pure ()
        displayEventsFromJournal
            (recordDisplayEvent (ToolOutputUpdated "c1" "latest") retractedAgain)
            `shouldBe` [ToolOutputUpdated "c1" "latest"]

    it "releases an output head without evaluating the filtered tail" do
        let journal = foldl' (flip recordDisplayEvent) emptyDisplayJournal
                [ ToolOutputUpdated (error "tail ID was evaluated") "tail"
                , TextDelta "boundary", ToolOutputUpdated "c1" "head"
                ]
            retracted = recordDisplayEvent (ToolRetracted "c1") journal
        retracted `seq` (pure () :: IO ())
  where
    events = foldl' (flip recordVisibleLoopEvent) emptyVisibleLoopState
    call = functionToolCall "c1" "tool" "{}"
