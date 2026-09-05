module Agent.CLI.ComputerUseSpec (spec) where

import Agent.CLI.ComputerUse
    ( ComputerObservation(..)
    , ComputerUseBackend(..)
    , ScreenshotEncoding(..)
    , computerApprovalPrompt
    , executeComputerCallWithBackend
    , keyCombinationScript
    , parseDisplaySize
    , parseSessionLocked
    , pointerScript
    , summarizeComputerCall
    , validateComputerCall
    , validateComputerCallForDisplay
    )
import Agent.CLI.SessionAdmin (sessionToolEvent)
import Agent.Json (rawJsonFromEncoding)
import Agent.Loop (ImageAttachment(..))
import Agent.Responses.Types
import Agent.ToolDispatch (ToolCall(..), ToolCallKind(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.IORef (modifyIORef', newIORef, readIORef)
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

        it "preflights every drag point before pressing the mouse button" do
            pointerScript
                (DragAction [ComputerPoint 10 20, ComputerPoint 30 40] [])
                `shouldSatisfy` either
                    (const False)
                    ("check(10,20);check(30,40);down(10,20"
                        `Text.isInfixOf`)

        it "maps browser scroll signs to CoreGraphics and caps deltas" do
            pointerScript (ScrollAction 12 34 50 (-60) [])
                `shouldSatisfy` either
                    (const False)
                    ("2,-dy,-dx" `Text.isInfixOf`)
            validateComputerCall
                exampleCall
                    { computerActions =
                        [ScrollAction 0 0 100001 0 []]
                    }
                `shouldBe` Left
                    "Computer scroll delta exceeds 100000 pixels."

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

        it "fails closed when macOS GUI session state is unavailable" do
            pointerScript (ClickAction 12 34 "left" [])
                `shouldSatisfy` either
                    (const False)
                    (\script ->
                        "if(!d||typeof d.CGSSessionScreenIsLocked!=='boolean')"
                            `Text.isInfixOf` script)
            keyCombinationScript ["enter"]
                `shouldSatisfy` either
                    (const False)
                    (\script ->
                        "if(!d||typeof d.CGSSessionScreenIsLocked!=='boolean')"
                            `Text.isInfixOf` script)

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
            keyCombinationScript ["🙂"] `shouldSatisfy`
                either (const False)
                    (\script ->
                        "value.slice(i,end)" `Text.isInfixOf` script
                            && "charCodeAt(end-1)" `Text.isInfixOf` script)

        it "normalizes provider special-key names to macOS virtual keys" do
            keyCombinationScript ["ARROWLEFT"] `shouldSatisfy`
                either (const False) ("key(123,0)" `Text.isInfixOf`)
            keyCombinationScript ["PAGEUP"] `shouldSatisfy`
                either (const False) ("key(116,0)" `Text.isInfixOf`)
            keyCombinationScript ["DELETE"] `shouldSatisfy`
                either (const False) ("key(117,0)" `Text.isInfixOf`)
            keyCombinationScript ["BACKSPACE"] `shouldSatisfy`
                either (const False) ("key(51,0)" `Text.isInfixOf`)

        it "fails closed on malformed session lock output" do
            parseSessionLocked "false\n" `shouldBe` Right False
            parseSessionLocked "true" `shouldBe` Right True
            parseSessionLocked "" `shouldBe`
                Left "macOS returned an invalid GUI session lock state."
            parseSessionLocked "unlocked" `shouldBe`
                Left "macOS returned an invalid GUI session lock state."

        it "caps keys, drag paths, actions, and safety checks" do
            validateComputerCall
                exampleCall { computerActions = [] }
                `shouldBe` Left
                    "Computer call requires at least one action."
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
                    { computerActions = replicate 11 WaitAction }
                `shouldBe` Left
                    "Computer call exceeds the 10-action limit."
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

    describe "computer backend transaction" do
        it "strips the terminal screenshot and returns one fresh observation" do
            transactions <- newIORef
                ([] :: [(ScreenshotEncoding, (Int, Int), [ComputerAction])])
            let backend = ComputerUseBackend
                    { computerRunTransaction =
                        \encoding actions validateDisplay ->
                            case validateDisplay (1440, 900) of
                                Left err -> pure (Left err)
                                Right () -> do
                                    modifyIORef' transactions
                                        (<> [( encoding
                                             , (1440, 900)
                                             , actions
                                             )])
                                    pure (Right
                                        (ComputerObservation
                                            (ImageAttachment
                                                "image/jpeg"
                                                "fresh-image")
                                            Nothing))
                    }
                actions =
                    [ ClickAction 20 30 "left" []
                    , ScreenshotAction
                    ]
            result <- executeComputerCallWithBackend
                backend
                ScreenshotJpeg
                exampleCall { computerActions = actions }
            readIORef transactions `shouldReturn`
                [ ( ScreenshotJpeg
                  , (1440, 900)
                  , [ClickAction 20 30 "left" []]
                  )
                ]
            result `shouldSatisfy` either
                (const False)
                ("data:image/jpeg;base64,ZnJlc2gtaW1hZ2U="
                    `Text.isInfixOf`)

        it "captures a screenshot-only call without forwarding a fake action" do
            observedActions <- newIORef Nothing
            let backend = ComputerUseBackend
                    { computerRunTransaction =
                        \_ actions validateDisplay ->
                            case validateDisplay (100, 100) of
                                Left err -> pure (Left err)
                                Right () -> do
                                    modifyIORef'
                                        observedActions
                                        (const (Just actions))
                                    pure (Right
                                        (ComputerObservation
                                            (ImageAttachment
                                                "image/png"
                                                "observation")
                                            Nothing))
                    }
            result <- executeComputerCallWithBackend
                backend
                ScreenshotPng
                exampleCall { computerActions = [ScreenshotAction] }
            result `shouldSatisfy` either (const False) (const True)
            readIORef observedActions `shouldReturn` Just []

        it "does not invoke a backend for an invalid batch" do
            invocations <- newIORef (0 :: Int)
            let backend = ComputerUseBackend
                    { computerRunTransaction = \_ _ _ -> do
                        modifyIORef' invocations (+ 1)
                        pure (Right
                            (ComputerObservation
                                (ImageAttachment "image/png" "observation")
                                Nothing))
                    }
            result <- executeComputerCallWithBackend
                backend
                ScreenshotPng
                exampleCall
                    { computerActions =
                        [ScreenshotAction, ClickAction 1 2 "left" []]
                    }
            result `shouldBe` Left
                "Computer screenshot must be the final action in a batch."
            readIORef invocations `shouldReturn` 0

        it "allows only a terminal screenshot marker" do
            validateComputerCall
                exampleCall
                    { computerActions =
                        [ScreenshotAction, ClickAction 1 2 "left" []]
                    }
                `shouldBe` Left
                    "Computer screenshot must be the final action in a batch."
            validateComputerCall
                exampleCall
                    { computerActions =
                        [ClickAction 1 2 "left" [], ScreenshotAction]
                    }
                `shouldBe` Right ()
            validateComputerCall
                exampleCall { computerActions = [ScreenshotAction] }
                `shouldBe` Right ()

        it "accepts exactly ten actions" do
            validateComputerCall
                exampleCall { computerActions = replicate 10 WaitAction }
                `shouldBe` Right ()
            validateComputerCall
                exampleCall
                    { computerActions =
                        replicate 10 WaitAction <> [ScreenshotAction]
                    }
                `shouldBe` Right ()

        it "prevalidates every action before a batch can change the desktop" do
            validateComputerCall
                exampleCall
                    { computerActions =
                        [ ScreenshotAction
                        , TypeAction (Text.replicate 8193 "x")
                        ]
                    }
                `shouldBe` Left
                    "Computer text input exceeds the 8192-character limit."
            validateComputerCall
                exampleCall
                    { computerActions =
                        [ ScreenshotAction
                        , DragAction [ComputerPoint 1 2] []
                        ]
                    }
                `shouldBe` Left
                    "Computer drag path needs at least two points."
            validateComputerCall
                exampleCall
                    { computerActions =
                        [ ScreenshotAction
                        , KeypressAction ["hyper", "a"]
                        ]
                    }
                `shouldBe` Left
                    "Unsupported computer modifier: hyper"
            validateComputerCall
                exampleCall
                    { computerActions =
                        [ ScreenshotAction
                        , UnknownComputerAction
                            (TaggedObject "future_action")
                        ]
                    }
                `shouldBe` Left
                    "Unsupported computer action: \"future_action\""

        it "prevalidates every coordinate against the main display" do
            validateComputerCallForDisplay
                (1440, 900)
                exampleCall
                    { computerActions =
                        [ ClickAction 20 30 "left" []
                        , DragAction
                            [ ComputerPoint 100 100
                            , ComputerPoint 1440 899
                            ]
                            []
                        ]
                    }
                `shouldBe` Left
                    "Computer point is outside the main display."
            validateComputerCallForDisplay
                (1440, 900)
                exampleCall
                    { computerActions =
                        [ ScrollAction 1439 899 0 100 []
                        , MoveAction 0 0 []
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
                maybe False ("Allow this computer-use request?"
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

        it "redacts ordinary computer function arguments and screenshot output" do
            let call = FunctionCall
                    { itemId = Nothing
                    , callId = "call-function"
                    , name = computerFunctionName
                    , namespace = Just "functions"
                    , arguments =
                        "{\"actions\":[{\"type\":\"type\",\
                        \\"text\":\"top secret\"}]}"
                    , encryptedFunctionArgs = Nothing
                    , provider = Nothing
                    , status = Nothing
                    , async = Just True
                    }
                output = FunctionCallOutput
                    { localOutcome = Nothing
                    , itemId = Nothing
                    , callId = "call-function"
                    , name = Nothing
                    , namespace = Nothing
                    , output = rawJsonFromEncoding . Aeson.toEncoding $
                        [ Aeson.object
                            [ "type" Aeson..= ("input_image" :: Text.Text)
                            , "image_url" Aeson..=
                                ("data:image/png;base64,large-private-payload"
                                    :: Text.Text)
                            ]
                        ]
                    , provider = Nothing
                    , status = Nothing
                    , async = Just True
                    }
                encoded =
                    TextEncoding.decodeUtf8 . LBS.toStrict . Aeson.encode $
                        [ sessionToolEvent (FunctionCallItem call)
                        , sessionToolEvent (FunctionCallOutputItem output)
                        ]
            encoded `shouldSatisfy`
                ("\"name\":\"computer\"" `Text.isInfixOf`)
            encoded `shouldSatisfy`
                ("type 10 characters" `Text.isInfixOf`)
            encoded `shouldSatisfy`
                ("\"output\":\"Screenshot captured\"" `Text.isInfixOf`)
            encoded `shouldSatisfy`
                ("\"async\":true" `Text.isInfixOf`)
            encoded `shouldSatisfy`
                (not . ("top secret" `Text.isInfixOf`))
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
