module Agent.CLI.PickerSpec (spec) where

import Agent.CLI.Picker
import Test.Hspec

spec :: Spec
spec =
    describe "decodePickerKey" do
        it "decodes Kitty keyboard protocol keys" do
            decodePickerKey "\ESC[13u" `shouldBe` Just PickerKeyConfirm
            decodePickerKey "\ESC[27u" `shouldBe` Just PickerKeyCancel
            decodePickerKey "\ESC[97;2u" `shouldBe` Just (PickerKeyChar 'a')

        it "decodes modified CSI arrows" do
            decodePickerKey "\ESC[1;2A" `shouldBe` Just PickerKeyUp
            decodePickerKey "\ESC[1;5B" `shouldBe` Just PickerKeyDown
