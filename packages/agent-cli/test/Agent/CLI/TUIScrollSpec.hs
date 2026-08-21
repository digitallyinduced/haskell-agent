module Agent.CLI.TUIScrollSpec (spec) where

import Agent.CLI.TUI.Scroll
import Agent.TUI.Model (BlockId(..))
import Test.Hspec

spec :: Spec
spec = describe "fullscreen conversation scrolling" do
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
