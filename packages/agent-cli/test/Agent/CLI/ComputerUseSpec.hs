module Agent.CLI.ComputerUseSpec (spec) where

import Agent.CLI.ComputerUse
    ( ComputerObservation(..)
    , ComputerUseBackend(..)
    , ScreenshotEncoding(..)
    , computerApprovalPrompt
    , executeComputerCallWithBackend
    , executeComputerCallWithDesktopBackend
    , keyCombinationScript
    , newLeasedDesktopComputerUseBackend
    , parseDisplaySize
    , parseSessionLocked
    , pointerScript
    , summarizeComputerCall
    , validateComputerCall
    , validateComputerCallForDisplay
    )
import Agent.CLI.ComputerUse.Backend
    ( CapturedDisplay(..)
    , ComputerBackend(..)
    , ComputerDisplay(..)
    )
import Agent.CLI.ComputerUse.Linux
    ( LinuxSessionType(..)
    , detectLinuxSessionType
    )
import Agent.CLI.ComputerUse.Linux.Logind
    ( LogindSessionTarget(..)
    , processSessionRequest
    , validateLogindSession
    , validateLogindState
    )
import Agent.CLI.ComputerUse.Input (MouseButton(..))
import Agent.CLI.ComputerUse.Linux.Portal
    ( PortalStream(..)
    , parsePortalStartResults
    , portalDisplayForStream
    , portalKeysym
    , portalMouseButtonCode
    , portalRequestPathForSender
    , requestResponseRule
    , sessionClosedRule
    , withPortalCaptureReadiness
    , withPortalInputReadiness
    )
import Agent.CLI.ComputerUse.Linux.X11
    ( XdotoolInvocation(..)
    , parseXrandrDisplay
    , pinX11DisplayEnvironment
    , runX11TypeInvocationsWith
    , withX11InputCleanup
    , withX11Readiness
    , x11KeyInvocation
    , x11PointerPosition
    , x11ScrollInvocations
    , x11TypeInvocations
    )
import Agent.CLI.SessionAdmin (sessionToolEvent)
import Agent.Json (rawJsonFromEncoding)
import Agent.Loop (ImageAttachment(..))
import Agent.Responses.Types
import Agent.ToolDispatch (ToolCall(..), ToolCallKind(..))
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (finally, throwString, tryAny)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int32)
import Data.IORef
    ( atomicModifyIORef'
    , modifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Word (Word32, Word64)
import DBus (Variant, busName_, fromVariant, objectPath_, toVariant)
import DBus.Client (MatchRule(..))
import Test.Hspec

spec :: Spec
spec = do
    describe "computer backend executor" do
        it "rejects a structurally invalid batch before touching the backend" do
            backendCalls <- newIORef ([] :: [Text.Text])
            let record name result = do
                    modifyIORef' backendCalls (<> [name])
                    pure result
                backend = ComputerBackend
                    { computerBackendEnsureReady =
                        record "ready" (Right ())
                    , computerBackendInspectDisplay =
                        record "inspect" (Right x11Display)
                    , computerBackendExecuteAction = \_ _ ->
                        record "execute" (Right ())
                    , computerBackendCaptureDisplay = \_ ->
                        record "capture" (Left "unexpected capture")
                    , computerBackendClose =
                        modifyIORef' backendCalls (<> ["close"])
                    }
                call =
                    exampleCall
                        { computerActions =
                            [ ScreenshotAction
                            , UnknownComputerAction
                                (TaggedObject "future_action")
                            ]
                        }
            executeComputerCallWithDesktopBackend backend ScreenshotPng call
                `shouldReturn` Left
                    "Unsupported computer action: \"future_action\""
            readIORef backendCalls `shouldReturn` []

        it "prevalidates the entire batch before executing an action" do
            actionCount <- newIORef (0 :: Int)
            let backend =
                    (testBackend x11Display)
                        { computerBackendExecuteAction = \_ _ -> do
                            modifyIORef' actionCount (+ 1)
                            pure (Right ())
                        }
                call =
                    exampleCall
                        { computerActions =
                            [ ClickAction 25 30 "left" []
                            , MoveAction 1440 0 []
                            ]
                        }
            executeComputerCallWithDesktopBackend backend ScreenshotPng call
                `shouldReturn` Left
                    "Computer point is outside the selected display."
            readIORef actionCount `shouldReturn` 0

        it "rechecks the complete display identity before every action" do
            inspections <- newIORef
                [ x11Display
                , x11Display
                , x11Display
                    { computerDisplayOriginX =
                        x11Display.computerDisplayOriginX + 1
                    }
                ]
            actionCount <- newIORef (0 :: Int)
            let inspect = atomicModifyIORef' inspections \case
                    [] -> ([], Left "unexpected display inspection")
                    display : rest -> (rest, Right display)
                backend =
                    (testBackend x11Display)
                        { computerBackendInspectDisplay = inspect
                        , computerBackendExecuteAction = \_ _ -> do
                            modifyIORef' actionCount (+ 1)
                            pure (Right ())
                        }
                call =
                    exampleCall
                        { computerActions =
                            [ ClickAction 25 30 "left" []
                            , MoveAction 30 40 []
                            ]
                        }
            executeComputerCallWithDesktopBackend backend ScreenshotPng call
                `shouldReturn` Left
                    "The selected display changed during computer use; take a fresh screenshot before continuing."
            readIORef actionCount `shouldReturn` 1

        it "rejects a changed display identity after the batch" do
            let changed =
                    x11Display
                        { computerDisplayFrameWidth =
                            x11Display.computerDisplayFrameWidth + 1
                        }
                backend =
                    (testBackend x11Display)
                        { computerBackendCaptureDisplay =
                            \_ -> pure (Right CapturedDisplay
                                { capturedComputerDisplay = changed
                                , capturedComputerImage = emptyImage
                                })
                        }
            executeComputerCallWithDesktopBackend
                backend
                ScreenshotPng
                exampleCall
                `shouldReturn` Left
                    "The selected display changed during computer use; take a fresh screenshot before continuing."

        it "requires an observation before a leased desktop action" do
            backendCalls <- newIORef ([] :: [Text.Text])
            let record name result = do
                    modifyIORef' backendCalls (<> [name])
                    pure result
                backend = ComputerBackend
                    { computerBackendEnsureReady =
                        record "ready" (Right ())
                    , computerBackendInspectDisplay =
                        record "inspect" (Right x11Display)
                    , computerBackendExecuteAction = \_ _ ->
                        record "execute" (Right ())
                    , computerBackendCaptureDisplay = \_ ->
                        record "capture" (Right CapturedDisplay
                            { capturedComputerDisplay = x11Display
                            , capturedComputerImage = emptyImage
                            })
                    , computerBackendClose = pure ()
                    }
            leasedBackend <- newLeasedDesktopComputerUseBackend backend
            executeComputerCallWithBackend
                leasedBackend
                ScreenshotPng
                exampleCall
                `shouldReturn` Left
                    "Take a fresh computer screenshot before sending input actions."
            readIORef backendCalls `shouldReturn` []

        it "pins actions to the latest successful display observation" do
            currentDisplay <- newIORef x11Display
            captureFails <- newIORef False
            executedDisplays <- newIORef ([] :: [ComputerDisplay])
            let changedDisplay =
                    x11Display
                        { computerDisplayId =
                            "x11:HDMI-1:(0,0,1440,900)"
                        , computerDisplayOriginX = 0
                        , computerDisplayOriginY = 0
                        }
                backend = ComputerBackend
                    { computerBackendEnsureReady = pure (Right ())
                    , computerBackendInspectDisplay =
                        Right <$> readIORef currentDisplay
                    , computerBackendExecuteAction = \display _ -> do
                        modifyIORef' executedDisplays (<> [display])
                        pure (Right ())
                    , computerBackendCaptureDisplay = \_ -> do
                        fails <- readIORef captureFails
                        display <- readIORef currentDisplay
                        pure $ if fails
                            then Left "capture failed"
                            else Right CapturedDisplay
                                { capturedComputerDisplay = display
                                , capturedComputerImage = emptyImage
                                }
                    , computerBackendClose = pure ()
                    }
                screenshotCall =
                    exampleCall { computerActions = [ScreenshotAction] }
                succeeds = either (const False) (const True)
            leasedBackend <- newLeasedDesktopComputerUseBackend backend

            firstObservation <- executeComputerCallWithBackend
                leasedBackend
                ScreenshotPng
                screenshotCall
            firstObservation `shouldSatisfy` succeeds

            writeIORef currentDisplay changedDisplay
            writeIORef captureFails True
            executeComputerCallWithBackend
                leasedBackend
                ScreenshotPng
                screenshotCall
                `shouldReturn` Left "capture failed"
            writeIORef captureFails False

            executeComputerCallWithBackend
                leasedBackend
                ScreenshotPng
                exampleCall
                `shouldReturn` Left
                    "The selected display changed during computer use; take a fresh screenshot before continuing."
            readIORef executedDisplays `shouldReturn` []

            refreshedObservation <- executeComputerCallWithBackend
                leasedBackend
                ScreenshotPng
                screenshotCall
            refreshedObservation `shouldSatisfy` succeeds
            actionResult <- executeComputerCallWithBackend
                leasedBackend
                ScreenshotPng
                exampleCall
            actionResult `shouldSatisfy` succeeds
            readIORef executedDisplays `shouldReturn` [changedDisplay]

        it "invalidates a lease after a mutating transaction fails" do
            captureFails <- newIORef False
            actionCount <- newIORef (0 :: Int)
            let backend =
                    (testBackend x11Display)
                        { computerBackendExecuteAction = \_ _ -> do
                            modifyIORef' actionCount (+ 1)
                            pure (Right ())
                        , computerBackendCaptureDisplay = \_ -> do
                            fails <- readIORef captureFails
                            pure $ if fails
                                then Left "capture failed after input"
                                else Right CapturedDisplay
                                    { capturedComputerDisplay = x11Display
                                    , capturedComputerImage = emptyImage
                                    }
                        }
                screenshotCall =
                    exampleCall { computerActions = [ScreenshotAction] }
                succeeds = either (const False) (const True)
            leasedBackend <- newLeasedDesktopComputerUseBackend backend

            initialObservation <- executeComputerCallWithBackend
                leasedBackend
                ScreenshotPng
                screenshotCall
            initialObservation `shouldSatisfy` succeeds

            writeIORef captureFails True
            executeComputerCallWithBackend
                leasedBackend
                ScreenshotPng
                exampleCall
                `shouldReturn` Left "capture failed after input"
            writeIORef captureFails False

            executeComputerCallWithBackend
                leasedBackend
                ScreenshotPng
                exampleCall
                `shouldReturn` Left
                    "Take a fresh computer screenshot before sending input actions."
            readIORef actionCount `shouldReturn` 1

            refreshedObservation <- executeComputerCallWithBackend
                leasedBackend
                ScreenshotPng
                screenshotCall
            refreshedObservation `shouldSatisfy` succeeds
            finalAction <- executeComputerCallWithBackend
                leasedBackend
                ScreenshotPng
                exampleCall
            finalAction `shouldSatisfy` succeeds
            readIORef actionCount `shouldReturn` 2

        it "keeps a mutating lease invalid after a backend exception" do
            actionThrows <- newIORef True
            let backend =
                    (testBackend x11Display)
                        { computerBackendExecuteAction = \_ _ -> do
                            throws <- readIORef actionThrows
                            if throws
                                then throwString "input backend failed"
                                else pure (Right ())
                        }
                screenshotCall =
                    exampleCall { computerActions = [ScreenshotAction] }
                succeeds = either (const False) (const True)
            leasedBackend <- newLeasedDesktopComputerUseBackend backend

            initialObservation <- executeComputerCallWithBackend
                leasedBackend
                ScreenshotPng
                screenshotCall
            initialObservation `shouldSatisfy` succeeds
            failedAction <- tryAny $
                executeComputerCallWithBackend
                    leasedBackend
                    ScreenshotPng
                    exampleCall
            failedAction `shouldSatisfy` \case
                Left _ -> True
                Right _ -> False

            writeIORef actionThrows False
            executeComputerCallWithBackend
                leasedBackend
                ScreenshotPng
                exampleCall
                `shouldReturn` Left
                    "Take a fresh computer screenshot before sending input actions."
            refreshedObservation <- executeComputerCallWithBackend
                leasedBackend
                ScreenshotPng
                screenshotCall
            refreshedObservation `shouldSatisfy` succeeds

    describe "Linux X11 computer use" do
        it "prefers native Wayland over the XWayland DISPLAY" do
            detectLinuxSessionType
                (Just "wayland")
                (Just "wayland-0")
                (Just ":0")
                `shouldBe` Right LinuxWayland
            detectLinuxSessionType
                (Just "x11")
                Nothing
                (Just ":1")
                `shouldBe` Right LinuxX11

        it "fails closed for inactive or locked logind state" do
            validateLogindState True False `shouldBe` Right ()
            validateLogindState True True
                `shouldBe` Left
                    "Computer use is unavailable while the Linux session is locked."
            validateLogindState False False
                `shouldBe` Left
                    "Computer use is unavailable while the Linux session is inactive."

        it "binds logind lookup to the current process" do
            let (member, body) = processSessionRequest 4242
            member `shouldBe` "GetSessionByPID"
            case body of
                [pid] ->
                    (fromVariant pid :: Maybe Word32)
                        `shouldBe` Just 4242
                _ -> expectationFailure "expected one process id"

        it "binds the X11 server to the graphical logind session" do
            validateLogindSession
                (LogindX11Session ":1.0")
                True
                False
                "x11"
                (Just "unix/:1")
                `shouldBe` Right ()
            validateLogindSession
                (LogindX11Session ":1")
                True
                False
                "x11"
                (Just ":0")
                `shouldBe` Left
                    "Computer use cannot associate DISPLAY with the current systemd-logind X11 session."
            validateLogindSession
                (LogindX11Session ":1")
                True
                False
                "tty"
                (Just ":1")
                `shouldBe` Left
                    "Computer use cannot verify that the current systemd-logind session is X11."
            validateLogindSession
                (LogindX11Session "host.example:1")
                True
                False
                "x11"
                (Just "host.example:1")
                `shouldBe` Left
                    "Computer use cannot associate DISPLAY with the current systemd-logind X11 session."
            validateLogindSession
                LogindWaylandSession
                True
                False
                "wayland"
                Nothing
                `shouldBe` Right ()

        it "pins every X11 subprocess to one DISPLAY environment" do
            pinX11DisplayEnvironment
                ":7"
                [ ("PATH", "/run/current-system/sw/bin")
                , ("DISPLAY", ":0")
                , ("XAUTHORITY", "/run/user/1000/xauth")
                , ("DISPLAY", ":2")
                ]
                `shouldBe`
                    [ ("DISPLAY", ":7")
                    , ("PATH", "/run/current-system/sw/bin")
                    , ("XAUTHORITY", "/run/user/1000/xauth")
                    ]

        it "selects the primary monitor and preserves its signed origin" do
            parseXrandrDisplay
                "Monitors: 2\n\
                \ 0: +DP-1 1920/520x1080/290+0+0  DP-1\n\
                \ 1: +*eDP-1 1440/310x900/190-1440+80  eDP-1\n"
                `shouldBe` Right x11Display

        it "uses the sole monitor when xrandr has no primary" do
            parseXrandrDisplay
                "Monitors: 1\n\
                \ 0: +Virtual-1 1280/300x720/170+0+0  Virtual-1\n"
                `shouldSatisfy` either
                    (const False)
                    (\display ->
                        display.computerDisplayWidth == 1280
                            && display.computerDisplayHeight == 720)

        it "fails closed on an ambiguous monitor layout" do
            parseXrandrDisplay
                "Monitors: 2\n\
                \ 0: +DP-1 1920/520x1080/290+0+0  DP-1\n\
                \ 1: +HDMI-1 1920/520x1080/290+1920+0  HDMI-1\n"
                `shouldBe` Left
                    "xrandr reported multiple active monitors but no primary monitor."

        it "translates local coordinates without exposing typed text in argv" do
            x11PointerPosition x11Display 25 30 `shouldBe` (-1415, 110)
            x11KeyInvocation ["🙂"] `shouldBe`
                Right
                    (XdotoolInvocation
                        [ "type", "--clearmodifiers", "--delay", "1"
                        , "--file", "-"
                        ]
                        "🙂")

        it "maps browser scroll signs to bounded X wheel clicks" do
            x11ScrollInvocations 50 (-250) `shouldBe`
                [ XdotoolInvocation
                    ["click", "--repeat", "1", "7"]
                    ""
                , XdotoolInvocation
                    ["click", "--repeat", "3", "4"]
                    ""
                ]

        it "reports failures while releasing held input" do
            calls <- newIORef ([] :: [Text.Text])
            let record name result = do
                    modifyIORef' calls (<> [name])
                    pure result
            withX11InputCleanup
                (record "action" (Right ()))
                (record "cleanup" (Left "release failed"))
                `shouldReturn` Left "release failed"
            readIORef calls `shouldReturn` ["action", "cleanup"]

        it "chunks typing and stops before the next chunk on lock" do
            let value = Text.replicate 129 "x"
                chunks = x11TypeInvocations value
            map (.xdotoolStdin) chunks
                `shouldBe`
                    [ Text.replicate 64 "x"
                    , Text.replicate 64 "x"
                    , "x"
                    ]
            map (.xdotoolArguments) chunks
                `shouldSatisfy` all (notElem (replicate 64 'x'))

            locked <- newIORef False
            calls <- newIORef []
            let readiness =
                    readIORef locked >>= \case
                        False -> pure (Right ())
                        True -> pure (Left "session locked")
                run invocation = do
                    modifyIORef' calls (<> [invocation.xdotoolStdin])
                    writeIORef locked True
                    pure (Right ())
            runX11TypeInvocationsWith readiness run chunks
                `shouldReturn` Left "session locked"
            readIORef calls
                `shouldReturn` [Text.replicate 64 "x"]

        it "cancels an in-flight X11 action on lock" do
            readinessResults <- newIORef
                [Right (), Left "session locked"]
            cancelled <- newIORef False
            completed <- newIORef False
            let readiness =
                    atomicModifyIORef' readinessResults \case
                        [] -> ([], Left "session locked")
                        result : rest -> (rest, result)
                blockedAction =
                    ( do
                        threadDelay 1000000
                        writeIORef completed True
                        pure (Right ())
                    )
                        `finally` writeIORef cancelled True
            withX11Readiness readiness blockedAction
                `shouldReturn` Left "session locked"
            readIORef cancelled `shouldReturn` True
            readIORef completed `shouldReturn` False

    describe "Linux Wayland portal computer use" do
        it "derives race-free request paths from the unique bus name" do
            portalRequestPathForSender ":1.42" "request_ab12"
                `shouldBe`
                    "/org/freedesktop/portal/desktop/request/1_42/request_ab12"

        it "pins portal signals to the resolved unique owner" do
            let owner = busName_ ":1.24"
                requestPath =
                    objectPath_
                        "/org/freedesktop/portal/desktop/request/1_42/request_ab12"
                sessionPath =
                    objectPath_
                        "/org/freedesktop/portal/desktop/session/1_42/session_ab12"
            matchSender (requestResponseRule owner requestPath)
                `shouldBe` Just owner
            matchSender (sessionClosedRule owner sessionPath)
                `shouldBe` Just owner

        it "requires one monitor stream and both input grants" do
            parsePortalStartResults portalStartResults
                `shouldBe` Right portalStream
            parsePortalStartResults
                (Map.insert
                    "streams"
                    (toVariant
                        [ (77 :: Word32, portalStreamProperties)
                        , (78 :: Word32, portalStreamProperties)
                        ])
                    portalStartResults)
                `shouldBe` Left
                    "The desktop portal returned more than one monitor stream."
            parsePortalStartResults
                (Map.insert "devices" (toVariant (1 :: Word32))
                    portalStartResults)
                `shouldBe` Left
                    "The desktop portal did not grant both keyboard and pointer access."

        it "maps Unicode keysyms and Linux evdev mouse buttons" do
            portalKeysym 'A' `shouldBe` 0x41
            portalKeysym 'ß' `shouldBe` 0xdf
            portalKeysym '🙂' `shouldBe` 0x101f642
            portalKeysym '\n' `shouldBe` 0xff0d
            portalMouseButtonCode MouseLeft `shouldBe` 0x110
            portalMouseButtonCode MouseForward `shouldBe` 0x114

        it "rechecks session readiness after capture" do
            calls <- newIORef ([] :: [Text.Text])
            let record name result = do
                    modifyIORef' calls (<> [name])
                    pure result
            withPortalCaptureReadiness
                (record "ready" (Left "session locked"))
                (record "capture" (Right ()))
                `shouldReturn` Left "session locked"
            readIORef calls `shouldReturn` ["capture", "ready"]

        it "cancels portal input on lock and still runs cleanup" do
            readinessResults <- newIORef
                [Right (), Left "session locked"]
            cleanedUp <- newIORef False
            completed <- newIORef False
            let readiness =
                    atomicModifyIORef' readinessResults \case
                        [] -> ([], Left "session locked")
                        result : rest -> (rest, result)
                blockedInput =
                    ( do
                        threadDelay 1000000
                        writeIORef completed True
                        pure (Right ())
                    )
                        `finally` writeIORef cleanedUp True
            withPortalInputReadiness readiness blockedInput
                `shouldReturn` Left "session locked"
            readIORef cleanedUp `shouldReturn` True
            readIORef completed `shouldReturn` False

        it "uses negotiated stream metadata for display inspection" do
            let display =
                    portalDisplayForStream
                        (objectPath_
                            "/org/freedesktop/portal/desktop/session/1_42/session_ab12")
                        portalStream
            display.computerDisplayWidth
                `shouldBe` portalStream.portalStreamWidth
            display.computerDisplayHeight
                `shouldBe` portalStream.portalStreamHeight
            display.computerDisplayFrameWidth
                `shouldBe` portalStream.portalStreamWidth
            display.computerDisplayFrameHeight
                `shouldBe` portalStream.portalStreamHeight

    describe "computer action validation" do
        it "preserves supported mouse buttons and modifiers" do
            pointerScript (ClickAction 12 34 "back" ["shift"])
                `shouldSatisfy` either
                    (const False)
                    (\script ->
                        "click(12,34,3," `Text.isInfixOf` script
                            && "kCGEventFlagMaskShift"
                                `Text.isInfixOf` script)
            pointerScript (ClickAction 12 34 "left" ["fn"])
                `shouldSatisfy` either
                    (const False)
                    ("kCGEventFlagMaskSecondaryFn" `Text.isInfixOf`)
            x11KeyInvocation ["function", "a"]
                `shouldBe` Right (XdotoolInvocation
                    ["key", "--clearmodifiers", "XF86Fn+a"] "")

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

        it "prevalidates every coordinate against the selected display" do
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
                    "Computer point is outside the selected display."
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
                    }
                output = FunctionCallOutput
                    { itemId = Nothing
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

x11Display :: ComputerDisplay
x11Display = ComputerDisplay
    { computerDisplayId = "x11:eDP-1:(-1440,80,1440,900)"
    , computerDisplayOriginX = -1440
    , computerDisplayOriginY = 80
    , computerDisplayWidth = 1440
    , computerDisplayHeight = 900
    , computerDisplayFrameWidth = 1440
    , computerDisplayFrameHeight = 900
    }

testBackend :: ComputerDisplay -> ComputerBackend
testBackend display = ComputerBackend
    { computerBackendEnsureReady = pure (Right ())
    , computerBackendInspectDisplay = pure (Right display)
    , computerBackendExecuteAction = \_ _ -> pure (Right ())
    , computerBackendCaptureDisplay = \_ -> pure (Right CapturedDisplay
        { capturedComputerDisplay = display
        , capturedComputerImage = emptyImage
        })
    , computerBackendClose = pure ()
    }

emptyImage :: ImageAttachment
emptyImage = ImageAttachment
    { imageMime = "image/png"
    , imageBytes = BS.empty
    }

portalStartResults :: Map.Map Text.Text Variant
portalStartResults = Map.fromList
    [ ("devices", toVariant (3 :: Word32))
    , ("streams",
        toVariant [(77 :: Word32, portalStreamProperties)])
    ]

portalStreamProperties :: Map.Map Text.Text Variant
portalStreamProperties = Map.fromList
    [ ("id", toVariant ("monitor-1" :: Text.Text))
    , ("position", toVariant ((-1920 :: Int32), (0 :: Int32)))
    , ("size", toVariant ((1920 :: Int32), (1080 :: Int32)))
    , ("source_type", toVariant (1 :: Word32))
    , ("mapping_id", toVariant ("mapping-1" :: Text.Text))
    , ("pipewire-serial", toVariant (1234 :: Word64))
    ]

portalStream :: PortalStream
portalStream = PortalStream
    { portalStreamNodeId = 77
    , portalStreamId = Just "monitor-1"
    , portalStreamPositionX = -1920
    , portalStreamPositionY = 0
    , portalStreamWidth = 1920
    , portalStreamHeight = 1080
    , portalStreamSourceType = Just 1
    , portalStreamMappingId = Just "mapping-1"
    , portalStreamPipeWireSerial = Just 1234
    }
