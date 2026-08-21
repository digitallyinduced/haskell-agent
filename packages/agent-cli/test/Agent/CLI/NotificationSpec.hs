module Agent.CLI.NotificationSpec (spec) where

import Agent.CLI.Notification
import Test.Hspec

spec :: Spec
spec =
    describe "attentionNotificationSequence" do
        it "requests input with a Ghostty OSC 9 notification" do
            attentionNotificationSequence InputRequested
                `shouldBe` "\ESC]9;Haskell Agent: input requested\ESC\\"

        it "distinguishes permission prompts" do
            attentionNotificationSequence PermissionRequested
                `shouldBe` "\ESC]9;Haskell Agent: permission required\ESC\\"
