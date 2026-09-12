module Main (main) where

import Agent.Loop.Output
import Agent.Loop.VisibleState
import Agent.ToolDispatch (functionToolCall)
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
  where
    events = foldl' (flip recordVisibleLoopEvent) emptyVisibleLoopState
    call = functionToolCall "c1" "tool" "{}"
