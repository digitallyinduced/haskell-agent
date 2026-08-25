module Agent.CLI.TUIScrollSpec (spec) where

import Agent.CLI.TUI.Scroll
import Agent.TUI.Model (BlockId(..))
import Test.Hspec

spec :: Spec
spec = describe "fullscreen conversation scrolling" do
    describe "conversationScrollGesture" do
        it "ignores scrolling when an empty session has no viewport" do
            conversationScrollGesture 3 Nothing
                `shouldBe` IgnoreConversationScroll

        it "does not pause following when scrolling up from the top" do
            conversationScrollGesture (-3) (Just (0, 20, 10))
                `shouldBe` IgnoreConversationScroll

        it "pauses only when the viewport can move away from the tail" do
            conversationScrollGesture (-3) (Just (12, 20, 60))
                `shouldBe` PauseAndScrollConversation
            conversationScrollGesture 3 (Just (12, 20, 60))
                `shouldBe` PauseAndScrollConversation

        it "resumes following when a downward scroll reaches the tail" do
            conversationScrollGesture 10 (Just (31, 20, 60))
                `shouldBe` ResumeConversationFollow

    it "reserves the rest of the viewport below a submitted prompt" do
        let anchor = startConversationAnchor (BlockId 7) "question" 40
            (next, action) =
                reflowConversationAnchor True 35 20 45 anchor
        next.anchorReserveRows `shouldBe` 15
        next.anchorViewportTop `shouldBe` 40
        next.anchorPhase `shouldBe` ConversationFillingPage
        conversationAnchorSticky next `shouldBe` False
        action `shouldBe` ScrollConversationToEnd

    it "shrinks the reserve as response content fills the page" do
        let initial = startConversationAnchor (BlockId 7) "question" 40
            (pinned, _) =
                reflowConversationAnchor True 40 20 45 initial
            (grown, _) =
                reflowConversationAnchor True 40 20 55 pinned
        grown.anchorReserveRows `shouldBe` 5
        grown.anchorViewportTop `shouldBe` 40
        grown.anchorPhase `shouldBe` ConversationFillingPage

    it "recomputes the reserve when the terminal height changes" do
        let initial = startConversationAnchor (BlockId 7) "question" 40
            (pinned, _) =
                reflowConversationAnchor True 40 20 45 initial
            (taller, tallerAction) =
                reflowConversationAnchor True 40 30 45 pinned
            (shorter, shorterAction) =
                reflowConversationAnchor True 40 10 45 taller
        taller.anchorReserveRows `shouldBe` 25
        taller.anchorViewportTop `shouldBe` 40
        tallerAction `shouldBe` ScrollConversationToEnd
        shorter.anchorReserveRows `shouldBe` 5
        shorter.anchorViewportTop `shouldBe` 40
        shorterAction `shouldBe` ScrollConversationToEnd

    it "switches permanently to tail following after the page overflows" do
        let initial = startConversationAnchor (BlockId 7) "question" 40
            (pinned, _) =
                reflowConversationAnchor True 40 20 45 initial
            (following, action) =
                reflowConversationAnchor True 40 20 65 pinned
            (shrunk, _) =
                reflowConversationAnchor True 45 20 55 following
        following.anchorReserveRows `shouldBe` 0
        following.anchorPhase `shouldBe` ConversationFollowingTail
        following.anchorViewportTop `shouldBe` 45
        conversationAnchorSticky following `shouldBe` True
        action `shouldBe` ScrollConversationToEnd
        shrunk.anchorPhase `shouldBe` ConversationFollowingTail
        shrunk.anchorReserveRows `shouldBe` 0

    it "does not move the viewport while the user is reading history" do
        let initial = startConversationAnchor (BlockId 7) "question" 40
            (next, action) =
                reflowConversationAnchor False 12 20 70 initial
        next.anchorViewportTop `shouldBe` 12
        next.anchorPhase `shouldBe` ConversationFillingPage
        action `shouldBe` KeepConversationPosition

    it "releases a short page flip on an explicit bottom gesture" do
        let initial = startConversationAnchor (BlockId 7) "question" 40
            (pinned, _) =
                reflowConversationAnchor True 40 20 45 initial
            released = followConversationTail pinned
        released.anchorReserveRows `shouldBe` 0
        released.anchorPhase `shouldBe` ConversationFollowingTail
