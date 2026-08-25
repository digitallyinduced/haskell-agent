module Agent.CLI.SessionHistorySpec (spec) where

import Agent.CLI.Session.History
    ( LiveConversation(..)
    , resetLiveConversationState
    )
import Agent.Loop (ImageAttachment(..))
import Agent.Responses.LoopBackend (turnInputsToItems)
import Agent.Loop (TurnInput(..))
import qualified Data.ByteString.Char8 as ByteString
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "LiveConversation" do
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
