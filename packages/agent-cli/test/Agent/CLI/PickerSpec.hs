module Agent.CLI.PickerSpec (spec) where

import Agent.CLI.Picker
import Test.Hspec

spec :: Spec
spec = do
    describe "decodePickerKey" do
        it "decodes Kitty keyboard protocol keys" do
            decodePickerKey "\ESC[13u" `shouldBe` Just PickerKeyConfirm
            decodePickerKey "\ESC[27u" `shouldBe` Just PickerKeyCancel
            decodePickerKey "\t" `shouldBe` Just PickerKeyTab
            decodePickerKey "\ESC[Z" `shouldBe` Just PickerKeyBackTab
            decodePickerKey "\ESC[97;2u" `shouldBe` Just (PickerKeyChar 'a')

        it "decodes modified CSI arrows" do
            decodePickerKey "\ESC[1;2A" `shouldBe` Just PickerKeyUp
            decodePickerKey "\ESC[1;5B" `shouldBe` Just PickerKeyDown
            decodePickerKey "\ESC[1;2C" `shouldBe` Just PickerKeyRight
            decodePickerKey "\ESC[1;5D" `shouldBe` Just PickerKeyLeft

        it "decodes normal and SS3 horizontal arrows" do
            decodePickerKey "\ESC[C" `shouldBe` Just PickerKeyRight
            decodePickerKey "\ESC[D" `shouldBe` Just PickerKeyLeft
            decodePickerKey "\ESCOC" `shouldBe` Just PickerKeyRight
            decodePickerKey "\ESCOD" `shouldBe` Just PickerKeyLeft

        it "ignores Kitty key releases instead of moving twice" do
            decodePickerKey "\ESC[1;1:1A" `shouldBe` Just PickerKeyUp
            decodePickerKey "\ESC[1;1:2B" `shouldBe` Just PickerKeyDown
            decodePickerKey "\ESC[1;1:3A" `shouldBe` Nothing
            decodePickerKey "\ESC[106;1:3u" `shouldBe` Nothing

        it "ignores empty and truncated CSI sequences" do
            map decodePickerKey ["", "\ESC[", "\ESC[1;", "\ESC[97;"]
                `shouldBe` replicate 4 Nothing

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

        it "requires the exact SGR prefix and a complete row and terminator" do
            map decodeMouseEvent
                [ ""
                , "[<0;12;7M"
                , " \ESC[<0;12;7M"
                , "\ESC[<0;12;"
                , "\ESC[<0;12;M"
                , "\ESC[<0;12;7"
                , "\ESC[<0;12;7Mx"
                ]
                `shouldBe` replicate 7 Nothing

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
