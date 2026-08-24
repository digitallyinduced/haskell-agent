module Agent.CLI.PermissionSpec (spec) where

import Agent.CLI.Permission
import Agent.CLI.Picker (PickerKey(..))
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = do
    describe "applyPermissionKey" do
        let state = initialPermissionState "search_replace src/A.hs"

        it "confirms the selected row" do
            applyPermissionKey PickerKeyConfirm state
                `shouldBe` Left PermissionAllowOnce

        it "moves down to always-this-tool" do
            case applyPermissionKey PickerKeyDown state of
                Right down ->
                    applyPermissionKey PickerKeyConfirm down
                        `shouldBe` Left PermissionAllowTool
                Left choice ->
                    expectationFailure ("expected navigation, got " <> show choice)

        it "maps y / a / n shortcuts" do
            applyPermissionKey (PickerKeyChar 'y') state
                `shouldBe` Left PermissionAllowOnce
            applyPermissionKey (PickerKeyChar 'A') state
                `shouldBe` Left PermissionAllowTool
            applyPermissionKey (PickerKeyChar 'n') state
                `shouldBe` Left PermissionDeny

        it "cancels with esc" do
            applyPermissionKey PickerKeyCancel state
                `shouldBe` Left PermissionDeny

    describe "renderPermissionFrame" do
        it "names the tool and the three choices" do
            let frame =
                    renderPermissionFrame False
                        (initialPermissionState "Allow search_replace src/A.hs?")
            frame `shouldSatisfy`
                Text.isInfixOf "Allow search_replace src/A.hs?"
            frame `shouldSatisfy` Text.isInfixOf "Allow once"
            frame `shouldSatisfy` Text.isInfixOf "Always allow this tool this session"
            frame `shouldSatisfy` Text.isInfixOf "Deny"
