module Agent.CLI.TerminalSpec (spec) where

import Agent.CLI.Terminal
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = do
    describe "terminal protocol encoders" do
        it "reports an escaped working directory" do
            osc7WorkingDirectory "/tmp/a b"
                `shouldBe` "\ESC]7;file:///tmp/a%20b\ESC\\"

        it "sanitizes desktop notifications" do
            osc9Notification "done\nnow\ESC]2;bad"
                `shouldBe` "\ESC]9;done now]2;bad\ESC\\"

        it "encodes OSC 52 clipboard data as base64" do
            osc52Clipboard "hello"
                `shouldBe` "\ESC]52;c;aGVsbG8=\ESC\\"

        it "includes semantic command exit status" do
            osc133CommandFinished (Just 1)
                `shouldBe` "\ESC]133;D;1\ESC\\"

        it "wraps escape sequences for tmux" do
            wrapTerminalPassthrough True "\ESC]9;done\ESC\\"
                `shouldBe` "\ESCPtmux;\ESC\ESC]9;done\ESC\ESC\\\ESC\\"

        it "leaves non-tmux output alone" do
            wrapTerminalPassthrough False synchronizedOutputBegin
                `shouldBe` synchronizedOutputBegin

        it "exposes Kitty keyboard push/pop sequences" do
            Text.null kittyKeyboardPush `shouldBe` False
            Text.null kittyKeyboardPop `shouldBe` False
