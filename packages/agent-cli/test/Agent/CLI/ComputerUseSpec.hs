module Agent.CLI.ComputerUseSpec (spec) where

import Agent.CLI.ComputerUse
    ( computerApprovalPrompt
    , keyCombinationScript
    , parseDisplaySize
    , parseSessionLocked
    , pointerScript
    , summarizeComputerCall
    , validateComputerCall
    )
import Agent.CLI.SessionAdmin (sessionToolEvent)
import Agent.Responses.Types
import Agent.ToolDispatch (ToolCall(..), ToolCallKind(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Test.Hspec

spec :: Spec
spec = do
    describe "computer action validation" do
        it "preserves supported mouse buttons and modifiers" do
            pointerScript (ClickAction 12 34 "back" ["shift"])
                `shouldSatisfy` either
                    (const False)
                    (\script ->
                        "click(12,34,3," `Text.isInfixOf` script
                            && "kCGEventFlagMaskShift"
                                `Text.isInfixOf` script)

        it "rejects unknown mouse buttons instead of changing them to left" do
            pointerScript (ClickAction 12 34 "sideways" [])
                `shouldBe` Left
                    "Unsupported computer mouse button: sideways"

        it "rejects unknown modifiers instead of dropping them" do
            keyCombinationScript ["hyper", "a"]
                `shouldBe` Left "Unsupported computer modifier: hyper"

        it "uses CGEvents rather than System Events automation" do
            keyCombinationScript ["cmd", "a"]
                `shouldSatisfy` either
                    (const False)
                    (\script ->
                        "CGEventCreateKeyboardEvent" `Text.isInfixOf` script
                            && not ("System Events" `Text.isInfixOf` script))

        it "validates logical main-display dimensions" do
            parseDisplaySize "2056,1329\n" `shouldBe` Just (2056, 1329)
            parseDisplaySize "4112,-1" `shouldBe` Nothing
            parseDisplaySize "screen" `shouldBe` Nothing

        it "preserves printable key case and Unicode" do
            keyCombinationScript ["A"] `shouldSatisfy`
                either (const False) ("typeText(\"A\")" `Text.isInfixOf`)
            keyCombinationScript ["ß"] `shouldSatisfy`
                either (const False) ("typeText(\"ß\")" `Text.isInfixOf`)
            keyCombinationScript ["CMD", "A"] `shouldSatisfy`
                either (const False)
                    (\script ->
                        "kCGEventFlagMaskCommand" `Text.isInfixOf` script
                            && "key(0," `Text.isInfixOf` script)

        it "fails closed on malformed session lock output" do
            parseSessionLocked "false\n" `shouldBe` Right False
            parseSessionLocked "true" `shouldBe` Right True
            parseSessionLocked "" `shouldBe`
                Left "macOS returned an invalid GUI session lock state."
            parseSessionLocked "unlocked" `shouldBe`
                Left "macOS returned an invalid GUI session lock state."

        it "caps keys, drag paths, actions, and safety checks" do
            validateComputerCall
                exampleCall
                    { computerActions =
                        [KeypressAction (replicate 16 "shift" <> ["a"])]
                    }
                `shouldBe` Left "Computer action exceeds the 16-key limit."
            validateComputerCall
                exampleCall
                    { computerActions =
                        [ DragAction
                            (replicate 1025 (ComputerPoint 0 0))
                            []
                        ]
                    }
                `shouldBe` Left
                    "Computer drag path exceeds 1024 points."
            validateComputerCall
                exampleCall
                    { computerActions = replicate 129 ScreenshotAction }
                `shouldBe` Left
                    "Computer call exceeds the 128-action limit."
            validateComputerCall
                exampleCall
                    { pendingSafetyChecks =
                        replicate 65
                            (SafetyCheck "id" Nothing Nothing KeyMap.empty)
                    }
                `shouldBe` Left
                    "Computer call exceeds the 64-safety-check limit."

        it "accepts boundary-sized key and drag arrays" do
            validateComputerCall
                exampleCall
                    { computerActions =
                        [ KeypressAction (replicate 15 "shift" <> ["a"])
                        , DragAction
                            (replicate 1024 (ComputerPoint 0 0))
                            []
                        ]
                    }
                `shouldBe` Right ()

    describe "computer approval summaries" do
        it "redacts typed text while surfacing actions and safety checks" do
            let call = ComputerCall
                    { computerCallItemId = Nothing
                    , computerCallId = "call-1"
                    , computerActions =
                        [ ClickAction 20 30 "left" []
                        , TypeAction "top secret"
                        ]
                    , pendingSafetyChecks =
                        [ SafetyCheck
                            { safetyCheckId = "check-1"
                            , safetyCheckCode = Just "sensitive"
                            , safetyCheckMessage =
                                Just "Confirm sensitive action"
                            , safetyCheckExtra = KeyMap.empty
                            }
                        ]
                    , computerCallStatus = Nothing
                    , computerCallExtra = KeyMap.empty
                    }
                summary = summarizeComputerCall call
                prompt = computerApprovalPrompt (toolCall call)
            summary `shouldSatisfy`
                ("\"left\" click at 20,30" `Text.isInfixOf`)
            summary `shouldSatisfy`
                ("type 10 characters" `Text.isInfixOf`)
            summary `shouldSatisfy`
                ("Confirm sensitive action" `Text.isInfixOf`)
            summary `shouldSatisfy`
                (not . ("top secret" `Text.isInfixOf`))
            prompt `shouldSatisfy`
                maybe False ("Allow this computer action?"
                    `Text.isPrefixOf`)

        it "escapes control characters in untrusted summary fields" do
            let call =
                    exampleCall
                        { computerActions =
                            [ClickAction 1 2 "left\nDENY" ["shift\rALLOW"]]
                        , pendingSafetyChecks =
                            [ SafetyCheck
                                "id"
                                Nothing
                                (Just "confirm\nALLOW")
                                KeyMap.empty
                            ]
                        }
                summary = summarizeComputerCall call
            summary `shouldSatisfy` (not . Text.any (`elem` ['\n', '\r']))
            summary `shouldSatisfy`
                ("\"confirm ALLOW\"" `Text.isInfixOf`)

        it "rehydrates typed computer calls and outputs as native tool cards" do
            let call = exampleCall
                output = ComputerCallOutput
                    { computerOutputItemId = Nothing
                    , computerOutputCallId = "call-1"
                    , screenshotDataUrl =
                        "data:image/png;base64,large-private-payload"
                    , acknowledgedChecks = []
                    , computerOutputStatus = Just ItemCompleted
                    , computerOutputExtra = KeyMap.empty
                    }
                encoded =
                    TextEncoding.decodeUtf8 . LBS.toStrict . Aeson.encode $
                        [ sessionToolEvent (ComputerCallItem call)
                        , sessionToolEvent (ComputerCallOutputItem output)
                        ]
            encoded `shouldSatisfy`
                ("\"name\":\"computer\"" `Text.isInfixOf`)
            encoded `shouldSatisfy`
                ("\"output\":\"Screenshot captured\"" `Text.isInfixOf`)
            encoded `shouldSatisfy`
                (not . ("large-private-payload" `Text.isInfixOf`))

toolCall :: ComputerCall -> ToolCall
toolCall call = ToolCall
    { callId = call.computerCallId
    , name = "computer"
    , arguments =
        TextEncoding.decodeUtf8 (LBS.toStrict (Aeson.encode call))
    , callKind = ComputerCallKind
    , argumentsEncrypted = True
    }

exampleCall :: ComputerCall
exampleCall = ComputerCall
    { computerCallItemId = Nothing
    , computerCallId = "call-1"
    , computerActions = [ClickAction 20 30 "left" []]
    , pendingSafetyChecks = []
    , computerCallStatus = Nothing
    , computerCallExtra = KeyMap.empty
    }
