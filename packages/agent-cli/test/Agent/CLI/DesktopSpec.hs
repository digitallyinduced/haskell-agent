module Agent.CLI.DesktopSpec (spec) where

import Agent.CLI.Desktop
import Test.Hspec

spec :: Spec
spec =
    describe "desktop conversation deep links" do
        it "targets the installed native app by bundle identifier" do
            desktopOpenArguments "2026-08-30-1234abcd"
                `shouldBe`
                    [ "-b"
                    , "dev.haskell-agent.macos"
                    , "haskell-agent://session/2026-08-30-1234abcd"
                    ]

        it "percent-encodes reserved and non-ASCII session-id bytes" do
            desktopConversationUrl "legacy café #%?"
                `shouldBe`
                    "haskell-agent://session/legacy%20caf%C3%A9%20%23%25%3F"
