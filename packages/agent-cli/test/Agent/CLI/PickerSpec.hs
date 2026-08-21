module Agent.CLI.PickerSpec (spec) where

import Agent.CLI.Picker
import Test.Hspec

spec :: Spec
spec = do
    describe "decodePickerKey" do
        it "decodes Kitty keyboard protocol keys" do
            decodePickerKey "\ESC[13u" `shouldBe` Just PickerKeyConfirm
            decodePickerKey "\ESC[27u" `shouldBe` Just PickerKeyCancel
            decodePickerKey "\ESC[97;2u" `shouldBe` Just (PickerKeyChar 'a')

        it "decodes modified CSI arrows" do
            decodePickerKey "\ESC[1;2A" `shouldBe` Just PickerKeyUp
            decodePickerKey "\ESC[1;5B" `shouldBe` Just PickerKeyDown

        it "ignores Kitty key releases instead of moving twice" do
            decodePickerKey "\ESC[1;1:1A" `shouldBe` Just PickerKeyUp
            decodePickerKey "\ESC[1;1:2B" `shouldBe` Just PickerKeyDown
            decodePickerKey "\ESC[1;1:3A" `shouldBe` Nothing
            decodePickerKey "\ESC[106;1:3u" `shouldBe` Nothing

    describe "decodeMouseEvent" do
        it "decodes SGR clicks, releases, and wheel events" do
            decodeMouseEvent "\ESC[<0;12;7M"
                `shouldBe` Just (MouseLeftPress 12 7)
            decodeMouseEvent "\ESC[<0;12;7m"
                `shouldBe` Just (MouseLeftRelease 12 7)
            decodeMouseEvent "\ESC[<64;4;9M"
                `shouldBe` Just (MouseWheelUp 4 9)
            decodeMouseEvent "\ESC[<65;4;9M"
                `shouldBe` Just (MouseWheelDown 4 9)

        it "ignores unsupported buttons and malformed reports" do
            decodeMouseEvent "\ESC[<2;12;7M" `shouldBe` Nothing
            decodeMouseEvent "\ESC[<0;x;7M" `shouldBe` Nothing

    describe "mouseKeysForFrame" do
        let frame = "title\n› first\n  second\n  third\nfooter"

        it "moves to and confirms a clicked row" do
            mouseKeysForFrame (Just 10) frame (MouseLeftPress 8 14)
                `shouldBe` [PickerKeyDown, PickerKeyDown, PickerKeyConfirm]

        it "does not activate headers or footers" do
            mouseKeysForFrame (Just 10) frame (MouseLeftPress 8 11)
                `shouldBe` []
            mouseKeysForFrame (Just 10) frame (MouseLeftPress 8 15)
                `shouldBe` []

        it "maps the scroll wheel to picker movement" do
            mouseKeysForFrame Nothing frame (MouseWheelUp 1 1)
                `shouldBe` [PickerKeyUp]
            mouseKeysForFrame Nothing frame (MouseWheelDown 1 1)
                `shouldBe` [PickerKeyDown]

        it "counts selectable rows rather than multi-line item details" do
            let detailed =
                    "title\n› first\n    first detail\n  second\n    second detail\nfooter"
            mouseKeysForFrame (Just 10) detailed (MouseLeftPress 8 14)
                `shouldBe` [PickerKeyDown, PickerKeyConfirm]
