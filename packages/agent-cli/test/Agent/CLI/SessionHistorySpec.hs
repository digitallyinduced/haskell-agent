module Agent.CLI.SessionHistorySpec (spec) where

import Agent.CLI.Session
    ( SessionTurn(..)
    , TranscriptEffect(..)
    )
import Agent.CLI.Session.ConversationStore (newConversationStore)
import Agent.CLI.Session.History
    ( foldSessionItems
    , hydrateUiHistory
    , readLiveAttachments
    , readLivePreviousResponseId
    , readLiveTranscript
    , resetLiveConversationState
    , resetLiveConversationWith
    )
import Agent.Loop (ImageAttachment(..), TurnInput(..))
import Agent.Responses.LoopBackend (turnInputsToItems)
import Agent.Tools.PlanMode (newPlanModeEnv)
import Agent.TUI.Model (BlockKind(..), UiBlock(..), UiState(..))
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Foldable as Foldable
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Time (UTCTime(..), fromGregorian)
import System.OsPath (unsafeEncodeUtf)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)

spec :: Spec
spec = do
    describe "LiveConversation" do
      it "resets all coordinated fields together" do
        state <- newConversationStore
            (Just "resp-1")
            (turnInputsToItems [UserMessage "hello"])
            [ImageAttachment "image/png" (ByteString.pack "bytes")]
        stateRef <- newIORef state
        resetLiveConversationState state
        readLivePreviousResponseId stateRef `shouldReturn` Nothing
        readLiveTranscript stateRef `shouldReturn` []
        readLiveAttachments stateRef `shouldReturn` []

      it "is idempotent" do
        state <- newConversationStore (Just "resp-1") [] []
        stateRef <- newIORef state
        resetLiveConversationState state
        resetLiveConversationState state
        readLivePreviousResponseId stateRef `shouldReturn` Nothing
        readLiveTranscript stateRef `shouldReturn` []
        readLiveAttachments stateRef `shouldReturn` []

      it "clears provider-owned transcript state at the same reset boundary" do
        let transcript = turnInputsToItems [UserMessage "old context"]
        conversation <-
            newConversationStore (Just "resp-1") transcript []
        conversationRef <- newIORef conversation
        providerTranscriptRef <- newIORef transcript
        planMode <- newPlanModeEnv (unsafeEncodeUtf "/tmp/session-reset") Nothing
        resetLiveConversationWith
            (writeIORef providerTranscriptRef [])
            conversationRef
            planMode
        readIORef providerTranscriptRef `shouldReturn` []
        readLivePreviousResponseId conversationRef `shouldReturn` Nothing
        readLiveTranscript conversationRef `shouldReturn` []
        readLiveAttachments conversationRef `shouldReturn` []

    describe "hydrateUiHistory" do
      it "keeps pre-compaction blocks scrollable while appending the summary" do
        let before = sessionTurn "question" (Just "answer") TranscriptAppend
            compacted =
                sessionTurn
                    "/compact"
                    (Just "Context compacted remotely.")
                    TranscriptReplace
            blocks =
                Foldable.toList
                    (hydrateUiHistory [before, compacted]).uiBlocks
        map (\block -> (block.blockKind, block.blockBody)) blocks
            `shouldBe`
                [ (BlockUser, "question")
                , (BlockAssistant, "answer")
                , (BlockSystem, "Context compacted remotely.")
                ]

      it "keeps visual scrollback across an automatic checkpoint boundary" do
        let before = sessionTurn "question" (Just "answer") TranscriptAppend
            automatic = sessionTurn "" Nothing TranscriptReplace
            after = sessionTurn "follow up" (Just "done") TranscriptAppend
            blocks =
                Foldable.toList
                    (hydrateUiHistory [before, automatic, after]).uiBlocks
        map (\block -> (block.blockKind, block.blockBody)) blocks
            `shouldBe`
                [ (BlockUser, "question")
                , (BlockAssistant, "answer")
                , (BlockUser, "follow up")
                , (BlockAssistant, "done")
                ]

      it "still clears displayed history for an explicit clear" do
        let before = sessionTurn "question" (Just "answer") TranscriptAppend
            cleared = sessionTurn "/clear" Nothing TranscriptReset
        Foldable.toList (hydrateUiHistory [before, cleared]).uiBlocks
            `shouldBe` []

    describe "foldSessionItems" do
      it "resumes from a committed checkpoint after continuation failure" do
        let old = turnInputsToItems [UserMessage "superseded"]
            checkpoint =
                turnInputsToItems [UserMessage "compacted checkpoint"]
            pending = turnInputsToItems [UserMessage "failed request"]
            oldTurn =
                (sessionTurn "old" (Just "old answer") TranscriptAppend)
                    { turnItems = old }
            compactTurn =
                (sessionTurn "" Nothing TranscriptReplace)
                    { turnItems = checkpoint <> pending }
            failedTurn =
                (sessionTurn "failed request" Nothing TranscriptAppend)
                    { turnItems = []
                    , turnError = Just "cancelled"
                    }
        foldSessionItems [oldTurn, compactTurn, failedTurn]
            `shouldBe` checkpoint <> pending

      it "appends only the successful post-checkpoint suffix" do
        let checkpoint =
                turnInputsToItems [UserMessage "compacted checkpoint"]
            suffix =
                turnInputsToItems
                    [ UserMessage "current request"
                    , UserMessage "current response"
                    ]
            compactTurn =
                (sessionTurn "" Nothing TranscriptReplace)
                    { turnItems = checkpoint }
            completedTurn =
                (sessionTurn "current request" (Just "current response")
                    TranscriptAppend)
                    { turnItems = suffix }
        foldSessionItems [compactTurn, completedTurn]
            `shouldBe` checkpoint <> suffix

      it "leaves non-compacted append history unchanged" do
        let first = turnInputsToItems [UserMessage "first"]
            second = turnInputsToItems [UserMessage "second"]
        foldSessionItems
            [ (sessionTurn "first" Nothing TranscriptAppend)
                { turnItems = first }
            , (sessionTurn "second" Nothing TranscriptAppend)
                { turnItems = second }
            ]
            `shouldBe` first <> second

sessionTurn :: Text -> Maybe Text -> TranscriptEffect -> SessionTurn
sessionTurn user assistant effect = SessionTurn
    { turnAt = UTCTime (fromGregorian 2026 8 25) 0
    , turnUserText = user
    , turnAssistantText = assistant
    , turnError = Nothing
    , turnResponseId = Nothing
    , turnEffect = effect
    , turnItems = []
    , turnUsage = Nothing
    , turnProviderTelemetry = []
    }
