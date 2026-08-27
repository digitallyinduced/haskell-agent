module Agent.CLI.SessionHistorySpec (spec) where

import Agent.CLI.Session.History
    ( LiveConversation(..)
    , hydrateUiHistory
    , resetLiveConversationState
    , resetLiveConversationWith
    )
import Agent.CLI.Session
    ( SessionTurn(..)
    , TranscriptEffect(..)
    )
import Agent.Loop (ImageAttachment(..))
import Agent.Responses.LoopBackend (turnInputsToItems)
import Agent.Loop (TurnInput(..))
import Agent.TUI.Model (BlockKind(..), UiBlock(..), UiState(..))
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Foldable as Foldable
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Time (UTCTime(..), fromGregorian)
import Agent.Tools.PlanMode (newPlanModeEnv)
import System.OsPath (unsafeEncodeUtf)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)

spec :: Spec
spec = do
    describe "LiveConversation" do
      it "resets all coordinated fields together" do
        let state = LiveConversation
                { livePreviousResponseId = Just "resp-1"
                , liveTranscript = turnInputsToItems [UserMessage "hello"]
                , liveAttachments =
                    [ImageAttachment "image/png" (ByteString.pack "bytes")]
                }
        resetLiveConversationState state `shouldBe`
            LiveConversation
                { livePreviousResponseId = Nothing
                , liveTranscript = []
                , liveAttachments = []
                }

      it "is idempotent" do
        let state = LiveConversation
                { livePreviousResponseId = Just "resp-1"
                , liveTranscript = []
                , liveAttachments = []
                }
            reset = resetLiveConversationState state
        resetLiveConversationState reset `shouldBe` reset

      it "clears provider-owned transcript state at the same reset boundary" do
        let transcript = turnInputsToItems [UserMessage "old context"]
        conversationRef <- newIORef LiveConversation
            { livePreviousResponseId = Just "resp-1"
            , liveTranscript = transcript
            , liveAttachments = []
            }
        providerTranscriptRef <- newIORef transcript
        planMode <- newPlanModeEnv (unsafeEncodeUtf "/tmp/session-reset") Nothing
        resetLiveConversationWith
            (writeIORef providerTranscriptRef [])
            conversationRef
            planMode
        readIORef providerTranscriptRef `shouldReturn` []
        readIORef conversationRef `shouldReturn`
            LiveConversation
                { livePreviousResponseId = Nothing
                , liveTranscript = []
                , liveAttachments = []
                }

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

      it "still clears displayed history for an explicit clear" do
        let before = sessionTurn "question" (Just "answer") TranscriptAppend
            cleared = sessionTurn "/clear" Nothing TranscriptReset
        Foldable.toList (hydrateUiHistory [before, cleared]).uiBlocks
            `shouldBe` []

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
    }
