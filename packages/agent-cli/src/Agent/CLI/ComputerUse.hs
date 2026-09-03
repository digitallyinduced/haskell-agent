-- | Function-based computer use backed by macOS screen capture and input.
module Agent.CLI.ComputerUse
    ( computerUseTool
    , computerFunctionParameters
    , executeComputerCall
    , screenshotMacOS
    , summarizeComputerCall
    , summarizeComputerToolCall
    , computerApprovalPrompt
    , pointerScript
    , keyCombinationScript
    , parseDisplaySize
    , parseSessionLocked
    , validateComputerCall
    , validateComputerCallForDisplay
    ) where

import qualified Agent.Json.Decode as Json
import Agent.Loop (ImageAttachment(..))
import Agent.Responses.Types
    ( ComputerAction(..)
    , ComputerCall(..)
    , ComputerCallOutput(..)
    , ComputerPoint(..)
    , SafetyCheck(..)
    , TaggedObject(..)
    )
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallKind(..)
    , isComputerToolCallKind
    , noArgsTool
    , typedToolWithCall
    )
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRule(..)
    , ToolExecutionPolicy(..)
    , ToolPlacement(..)
    , ToolSchema(..)
    )
import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (finally, tryAny)
import Control.Monad (foldM)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isControl, isDigit)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (ExitCode(..))
import System.Info (os)
import System.IO (hClose, openBinaryTempFile)
import System.Process (readProcessWithExitCode)
import Text.Read (readMaybe)

-- | A dedicated internal schema marker, rather than a provider-native wire
-- type, keeps this privileged handler separate from caller-defined functions.
computerUseTool :: AppTool
computerUseTool = AppTool
    { appToolName = "computer"
    , appToolDescription =
        "Run one or more approved actions on the main macOS display and receive a fresh screenshot. Start with screenshot when the UI state is unknown."
    , appToolSchema = HostedComputerSchema
    , appToolHandler = handler
    , appToolApproval = AlwaysPrompt
    , appToolExecution = TurnSequential
    , appToolResourceClaims = Nothing
    , appToolPlacement = UnclassifiedTool
    }
  where
    handler
        | os == "darwin" =
            typedToolWithCall "computer" computerToolInputDecoder \call input ->
                executeComputerCallWith
                    (case call.callKind of
                        ComputerFunctionCallKind -> ScreenshotJpeg
                        _ -> ScreenshotPng)
                    (computerCallFromInput call input)
        | otherwise = noArgsTool "computer"
            (pure (Left "Local computer use is currently supported only on macOS."))

data ComputerToolInput = ComputerToolInput
    { toolComputerActions :: ![ComputerAction]
    , toolPendingSafetyChecks :: ![SafetyCheck]
    }

instance Aeson.FromJSON ComputerToolInput where
    parseJSON = Aeson.withObject "ComputerToolInput" \object ->
        ComputerToolInput
            <$> object Aeson..:? "actions" Aeson..!= []
            <*> object Aeson..:? "pending_safety_checks" Aeson..!= []

computerToolInputDecoder :: Json.Decoder ComputerToolInput
computerToolInputDecoder =
    Json.withOwnedRawJson \bytes ->
        case Aeson.eitherDecodeStrict' bytes of
            Left err -> fail err
            Right input -> pure input

-- | Strict parameters for the ordinary @computer_use@ function. Actions stay
-- typed internally even though the provider sees a normal function call.
computerFunctionParameters :: Aeson.Value
computerFunctionParameters = Aeson.object
    [ "type" Aeson..= ("object" :: Text)
    , "properties" Aeson..= Aeson.object
        [ "actions" Aeson..= Aeson.object
            [ "type" Aeson..= ("array" :: Text)
            , "description" Aeson..=
                ("Ordered desktop actions. Coordinates are logical pixels on the main display." :: Text)
            , "items" Aeson..= Aeson.object
                [ "anyOf" Aeson..= computerActionSchemas ]
            , "minItems" Aeson..= (1 :: Int)
            , "maxItems" Aeson..= (128 :: Int)
            ]
        ]
    , "required" Aeson..= ["actions" :: Text]
    , "additionalProperties" Aeson..= False
    ]

computerActionSchemas :: [Aeson.Value]
computerActionSchemas =
    [ actionSchema "screenshot" []
    , actionSchema "click"
        [ ("x", integerProperty)
        , ("y", integerProperty)
        , ("button", enumProperty
            ["left", "right", "middle", "back", "forward"])
        , ("keys", keysProperty)
        ]
    , actionSchema "double_click"
        [ ("x", integerProperty)
        , ("y", integerProperty)
        , ("keys", keysProperty)
        ]
    , actionSchema "scroll"
        [ ("x", integerProperty)
        , ("y", integerProperty)
        , ("scroll_x", integerProperty)
        , ("scroll_y", integerProperty)
        , ("keys", keysProperty)
        ]
    , actionSchema "move"
        [ ("x", integerProperty)
        , ("y", integerProperty)
        , ("keys", keysProperty)
        ]
    , actionSchema "drag"
        [ ("path", Aeson.object
            [ "type" Aeson..= ("array" :: Text)
            , "items" Aeson..= Aeson.object
                [ "type" Aeson..= ("object" :: Text)
                , "properties" Aeson..= Aeson.object
                    [ "x" Aeson..= integerProperty
                    , "y" Aeson..= integerProperty
                    ]
                , "required" Aeson..= ["x" :: Text, "y"]
                , "additionalProperties" Aeson..= False
                ]
            , "minItems" Aeson..= (2 :: Int)
            , "maxItems" Aeson..= (1024 :: Int)
            ])
        , ("keys", keysProperty)
        ]
    , actionSchema "type"
        [ ("text", Aeson.object
            [ "type" Aeson..= ("string" :: Text)
            , "maxLength" Aeson..= (8192 :: Int)
            ])
        ]
    , actionSchema "keypress" [("keys", keysProperty)]
    , actionSchema "wait" []
    ]
  where
    integerProperty :: Aeson.Value
    integerProperty = Aeson.object ["type" Aeson..= ("integer" :: Text)]
    keysProperty :: Aeson.Value
    keysProperty = Aeson.object
        [ "type" Aeson..= ("array" :: Text)
        , "items" Aeson..= Aeson.object
            [ "type" Aeson..= ("string" :: Text)
            , "maxLength" Aeson..= (64 :: Int)
            ]
        , "maxItems" Aeson..= (16 :: Int)
        ]
    enumProperty :: [Text] -> Aeson.Value
    enumProperty values = Aeson.object
        [ "type" Aeson..= ("string" :: Text)
        , "enum" Aeson..= values
        ]

actionSchema :: Text -> [(Text, Aeson.Value)] -> Aeson.Value
actionSchema actionType properties = Aeson.object
    [ "type" Aeson..= ("object" :: Text)
    , "properties" Aeson..= Aeson.Object
        (KeyMap.fromList
            ((Key.fromText "type", enumProperty [actionType])
                : [(Key.fromText name, value) | (name, value) <- properties]))
    , "required" Aeson..= ("type" : map fst properties)
    , "additionalProperties" Aeson..= False
    ]
  where
    enumProperty :: [Text] -> Aeson.Value
    enumProperty values = Aeson.object
        [ "type" Aeson..= ("string" :: Text)
        , "enum" Aeson..= values
        ]

computerCallFromInput :: ToolCall -> ComputerToolInput -> ComputerCall
computerCallFromInput call input = ComputerCall
    { computerCallItemId = Nothing
    , computerCallId = call.callId
    , computerActions = input.toolComputerActions
    , pendingSafetyChecks = input.toolPendingSafetyChecks
    , computerCallStatus = Nothing
    , computerCallExtra = KeyMap.empty
    }

executeComputerCall :: ComputerCall -> IO (Either Text Text)
executeComputerCall = executeComputerCallWith ScreenshotPng

data ScreenshotEncoding
    = ScreenshotPng
    | ScreenshotJpeg

executeComputerCallWith
    :: ScreenshotEncoding
    -> ComputerCall
    -> IO (Either Text Text)
executeComputerCallWith screenshotEncoding call
    | Left err <- validateComputerCall call = pure (Left err)
    | otherwise = do
        unlocked <- ensureUnlockedSession
        case unlocked of
            Left err -> pure (Left err)
            Right () -> do
                dimensions <- mainDisplayLogicalSize
                case dimensions of
                    Left err -> pure (Left err)
                    Right display
                        | Left err <-
                            validateComputerCallForDisplay display call ->
                            pure (Left err)
                        | otherwise -> do
                            actionResult <-
                                foldM run (Right ()) call.computerActions
                            case actionResult of
                                Left err -> pure (Left err)
                                Right () -> do
                                    currentDisplay <- mainDisplayLogicalSize
                                    case currentDisplay of
                                        Left err -> pure (Left err)
                                        Right value
                                            | value /= display ->
                                                pure (Left
                                                    "The main display changed during computer use; take a fresh screenshot before continuing.")
                                            | otherwise ->
                                                screenshotMainDisplayWith
                                                    screenshotEncoding
                                                    display
                                                    >>= \case
                                                        Left err ->
                                                            pure (Left err)
                                                        Right image ->
                                                            pure (Right
                                                                (encodeComputerOutput
                                                                    call image))
  where
    run (Left err) _ = pure (Left err)
    run (Right ()) action = executeAction action

encodeComputerOutput :: ComputerCall -> ImageAttachment -> Text
encodeComputerOutput call ImageAttachment{imageMime, imageBytes} =
    TextEncoding.decodeUtf8 . LBS.toStrict . Aeson.encode $
        ComputerCallOutput
            { computerOutputItemId = Nothing
            , computerOutputCallId = call.computerCallId
            , screenshotDataUrl = dataUrl imageMime imageBytes
            -- Reaching the handler means this exact call was approved.
            , acknowledgedChecks = call.pendingSafetyChecks
            , computerOutputStatus = Nothing
            , computerOutputExtra = KeyMap.empty
            }

validateComputerCall :: ComputerCall -> Either Text ()
validateComputerCall call
    | null call.computerActions =
        Left "Computer call requires at least one action."
    | exceedsList 128 call.computerActions =
        Left "Computer call exceeds the 128-action limit."
    | exceedsList 64 call.pendingSafetyChecks =
        Left "Computer call exceeds the 64-safety-check limit."
    | Just err <- firstJust
        (map validateSafetyCheck call.pendingSafetyChecks
            <> map validateAction call.computerActions) =
        Left err
    | otherwise = Right ()

-- | Validate all model-space coordinates before the first action in a batch.
-- The per-action JXA checks remain as a second line of defense if the display
-- configuration changes while the batch is executing.
validateComputerCallForDisplay :: (Int, Int) -> ComputerCall -> Either Text ()
validateComputerCallForDisplay (width, height) call = do
    validateComputerCall call
    case firstJust (map validatePoint (computerPoints call.computerActions)) of
        Just err -> Left err
        Nothing -> Right ()
  where
    validatePoint ComputerPoint{pointX, pointY}
        | pointX < 0 || pointY < 0 || pointX >= width || pointY >= height =
            Just "Computer point is outside the main display."
        | otherwise = Nothing

computerPoints :: [ComputerAction] -> [ComputerPoint]
computerPoints = concatMap \case
    ClickAction{clickX, clickY} -> [ComputerPoint clickX clickY]
    DoubleClickAction{doubleClickX, doubleClickY} ->
        [ComputerPoint doubleClickX doubleClickY]
    ScrollAction{scrollX, scrollY} -> [ComputerPoint scrollX scrollY]
    MoveAction{moveX, moveY} -> [ComputerPoint moveX moveY]
    DragAction{dragPath} -> dragPath
    _ -> []

validateSafetyCheck :: SafetyCheck -> Maybe Text
validateSafetyCheck check
    | exceedsText 256 check.safetyCheckId =
        Just "Computer safety-check id exceeds 256 characters."
    | maybe False (exceedsText 128) check.safetyCheckCode =
        Just "Computer safety-check code exceeds 128 characters."
    | maybe False (exceedsText 1024) check.safetyCheckMessage =
        Just "Computer safety-check message exceeds 1024 characters."
    | otherwise = Nothing

validateAction :: ComputerAction -> Maybe Text
validateAction = \case
    ClickAction { clickButton, clickKeys }
        | exceedsText 32 clickButton ->
            Just "Computer mouse button exceeds 32 characters."
        | Just err <- validateKeys clickKeys -> Just err
        | Left err <- buttonNumber clickButton -> Just err
        | Left err <- modifierFlags clickKeys -> Just err
        | otherwise -> Nothing
    DoubleClickAction { doubleClickKeys }
        | Just err <- validateKeys doubleClickKeys -> Just err
        | Left err <- modifierFlags doubleClickKeys -> Just err
        | otherwise -> Nothing
    ScrollAction { scrollDx, scrollDy, scrollKeys }
        | any (\delta -> abs (toInteger delta) > 100000)
            [scrollDx, scrollDy] ->
            Just "Computer scroll delta exceeds 100000 pixels."
        | Just err <- validateKeys scrollKeys -> Just err
        | Left err <- modifierFlags scrollKeys -> Just err
        | otherwise -> Nothing
    MoveAction { moveKeys }
        | Just err <- validateKeys moveKeys -> Just err
        | Left err <- modifierFlags moveKeys -> Just err
        | otherwise -> Nothing
    DragAction { dragPath, dragKeys }
        | exceedsList 1024 dragPath ->
            Just "Computer drag path exceeds 1024 points."
        | null dragPath -> Just "Computer drag path is empty."
        | null (drop 1 dragPath) ->
            Just "Computer drag path needs at least two points."
        | Just err <- validateKeys dragKeys -> Just err
        | Left err <- modifierFlags dragKeys -> Just err
        | otherwise -> Nothing
    TypeAction value
        | exceedsText 8192 value ->
            Just "Computer text input exceeds the 8192-character limit."
        | otherwise -> Nothing
    KeypressAction keys ->
        either Just (const Nothing) (keyCombinationScript keys)
    UnknownComputerAction value
        | exceedsText 128 value.tag ->
            Just "Computer action type exceeds 128 characters."
        | otherwise ->
            Just ("Unsupported computer action: " <> safeQuoted 128 value.tag)
    _ -> Nothing

validateKeys :: [Text] -> Maybe Text
validateKeys keys
    | exceedsList 16 keys = Just "Computer action exceeds the 16-key limit."
    | any (exceedsText 64) keys =
        Just "Computer key name exceeds 64 characters."
    | otherwise = Nothing

firstJust :: [Maybe value] -> Maybe value
firstJust = foldr (<|>) Nothing

exceedsList :: Int -> [value] -> Bool
exceedsList limit = not . null . drop limit

exceedsText :: Int -> Text -> Bool
exceedsText limit = not . Text.null . Text.drop limit

executeAction :: ComputerAction -> IO (Either Text ())
executeAction = \case
    ScreenshotAction -> pure (Right ())
    WaitAction -> do
        threadDelay 2000000
        pure (Right ())
    action@ClickAction{} -> runPointerAction action
    action@DoubleClickAction{} -> runPointerAction action
    action@ScrollAction{} -> runPointerAction action
    action@MoveAction{} -> runPointerAction action
    action@DragAction{} -> runPointerAction action
    TypeAction value ->
        runJxa (keyboardPrelude <> typeTextCommand value)
    KeypressAction keys ->
        either (pure . Left) runJxa (keyCombinationScript keys)
    UnknownComputerAction value ->
        pure (Left ("Unsupported computer action: " <> safeQuoted 128 value.tag))

runPointerAction :: ComputerAction -> IO (Either Text ())
runPointerAction =
    either (pure . Left) runJxa . pointerScript

-- | Build a script whose coordinates exactly match the logical-point image
-- returned by 'screenshotMacOS'. Only the main display is exposed; this avoids
-- ambiguous mixed-scale/mixed-origin mappings across multiple displays.
pointerScript :: ComputerAction -> Either Text Text
pointerScript action = do
    maybe (Right ()) Left (validateAction action)
    command <- case action of
        ClickAction { clickX, clickY, clickButton, clickKeys } -> do
            button <- buttonNumber clickButton
            flags <- modifierFlags clickKeys
            pure $
                "click(" <> ints [clickX, clickY] <> ","
                    <> button <> "," <> flags <> ",1);"
        DoubleClickAction
            { doubleClickX, doubleClickY, doubleClickKeys } -> do
            flags <- modifierFlags doubleClickKeys
            pure $
                "click(" <> ints [doubleClickX, doubleClickY]
                    <> ",0," <> flags <> ",2);"
        MoveAction { moveX, moveY, moveKeys } -> do
            flags <- modifierFlags moveKeys
            pure $
                "move(" <> ints [moveX, moveY] <> "," <> flags <> ");"
        ScrollAction
            { scrollX, scrollY, scrollDx, scrollDy, scrollKeys } -> do
            flags <- modifierFlags scrollKeys
            pure $
                "move(" <> ints [scrollX, scrollY] <> "," <> flags
                    <> "); scroll(" <> ints [scrollDx, scrollDy]
                    <> "," <> flags <> ");"
        DragAction { dragPath, dragKeys } -> do
            flags <- modifierFlags dragKeys
            case dragPath of
                [] -> Left "Computer drag path is empty."
                [_] -> Left "Computer drag path needs at least two points."
                first@ComputerPoint { pointX, pointY } : rest ->
                    pure $
                        Text.concat
                            [ "check(" <> ints [px, py] <> ");"
                            | ComputerPoint
                                { pointX = px, pointY = py } <- first : rest
                            ]
                            <> "down(" <> ints [pointX, pointY] <> "," <> flags
                            <> ");"
                            <> Text.concat
                                [ "delay(0.02); drag(" <> ints [px, py]
                                    <> "," <> flags <> ");"
                                | ComputerPoint
                                    { pointX = px, pointY = py } <- rest
                                ]
                            <> "up(" <> flags <> ");"
        _ -> Left "Not a pointer action."
    pure (pointerPrelude <> command)

pointerPrelude :: Text
pointerPrelude = Text.unlines
    [ "ObjC.import('CoreGraphics');"
    , "ObjC.import('Foundation');"
    , "ObjC.import('ApplicationServices');"
    , "const tap=$.kCGHIDEventTap, left=0;"
    , "function assertReady(){const d=ObjC.deepUnwrap($.CGSessionCopyCurrentDictionary()); if(!d) throw new Error('macOS GUI session state is unavailable'); if(Boolean(d.CGSSessionScreenIsLocked)) throw new Error('macOS session is locked'); if(!Boolean($.AXIsProcessTrusted())) throw new Error('Accessibility permission is required');}"
    , "assertReady();"
    , "const bounds=$.CGDisplayBounds($.CGMainDisplayID());"
    , "let last=$.CGPointMake(0,0);"
    , "function check(x,y){if(x<0||y<0||x>=Number(bounds.size.width)||y>=Number(bounds.size.height)) throw new Error('point outside main display');}"
    , "function kinds(b){return b===0?[$.kCGEventLeftMouseDown,$.kCGEventLeftMouseUp]:b===1?[$.kCGEventRightMouseDown,$.kCGEventRightMouseUp]:[$.kCGEventOtherMouseDown,$.kCGEventOtherMouseUp];}"
    , "function post(t,x,y,b,f,c){check(x,y); last=$.CGPointMake(x,y); const e=$.CGEventCreateMouseEvent(null,t,last,b); $.CGEventSetFlags(e,f); if(c) $.CGEventSetIntegerValueField(e,$.kCGMouseEventClickState,c); $.CGEventPost(tap,e);}"
    , "function move(x,y,f){post($.kCGEventMouseMoved,x,y,left,f,0);}"
    , "function click(x,y,b,f,count){const k=kinds(b); post(k[0],x,y,b,f,1); post(k[1],x,y,b,f,1); if(count===2){delay(0.08); post(k[0],x,y,b,f,2); post(k[1],x,y,b,f,2);}}"
    -- The Responses API follows browser-wheel signs (positive means down/right);
    -- CoreGraphics uses the opposite convention for both pixel-scroll axes.
    , "function scroll(dx,dy,f){const e=$.CGEventCreateScrollWheelEvent(null,$.kCGScrollEventUnitPixel,2,-dy,-dx); $.CGEventSetFlags(e,f); $.CGEventPost(tap,e);}"
    , "function down(x,y,f){post($.kCGEventLeftMouseDown,x,y,left,f,1);}"
    , "function drag(x,y,f){post($.kCGEventLeftMouseDragged,x,y,left,f,1);}"
    , "function up(f){post($.kCGEventLeftMouseUp,Number(last.x),Number(last.y),left,f,1);}"
    ]

keyboardPrelude :: Text
keyboardPrelude = Text.unlines
    [ "ObjC.import('CoreGraphics');"
    , "ObjC.import('Foundation');"
    , "ObjC.import('ApplicationServices');"
    , "const tap=$.kCGHIDEventTap;"
    , "function assertReady(){const d=ObjC.deepUnwrap($.CGSessionCopyCurrentDictionary()); if(!d) throw new Error('macOS GUI session state is unavailable'); if(Boolean(d.CGSSessionScreenIsLocked)) throw new Error('macOS session is locked'); if(!Boolean($.AXIsProcessTrusted())) throw new Error('Accessibility permission is required');}"
    , "assertReady();"
    , "function key(code,flags){const d=$.CGEventCreateKeyboardEvent(null,code,true),u=$.CGEventCreateKeyboardEvent(null,code,false); $.CGEventSetFlags(d,flags); $.CGEventSetFlags(u,flags); $.CGEventPost(tap,d); $.CGEventPost(tap,u);}"
    -- Keep each Unicode payload small enough for Quartz and avoid splitting a
    -- UTF-16 surrogate pair across events.
    , "function typeText(raw){const value=String(raw); for(let i=0;i<value.length;){let end=Math.min(i+32,value.length); if(end<value.length&&end>i){const c=value.charCodeAt(end-1); if(c>=0xD800&&c<=0xDBFF) end--;} const chunk=value.slice(i,end),n=chunk.length,v=$(chunk); const d=$.CGEventCreateKeyboardEvent(null,0,true),u=$.CGEventCreateKeyboardEvent(null,0,false); $.CGEventKeyboardSetUnicodeString(d,n,v); $.CGEventKeyboardSetUnicodeString(u,n,v); $.CGEventPost(tap,d); $.CGEventPost(tap,u); i=end;}}"
    ]

keyCombinationScript :: [Text] -> Either Text Text
keyCombinationScript [] = Left "Computer key combination is empty."
keyCombinationScript rawKeys
    | Just err <- validateKeys rawKeys = Left err
    | otherwise = do
        flags <- modifierFlags (init rawKeys)
        command <- keyCommand flags (Text.strip (last rawKeys))
        pure (keyboardPrelude <> command)
  where
    keyCommand flags key =
        case keyCode (normalize key) of
            Just code ->
                Right ("key(" <> Text.pack (show code) <> "," <> flags <> ");")
            Nothing
                | Text.length key == 1
                , flags == "0" ->
                    Right (typeTextCommand key)
                | Text.length key == 1
                , Just code <- characterKeyCode (normalize key) ->
                    Right
                        ("key(" <> Text.pack (show code) <> ","
                            <> flags <> ");")
                | otherwise ->
                    Left ("Unsupported computer key: " <> key)

keyCode :: Text -> Maybe Int
keyCode = \case
    "enter" -> Just 36
    "return" -> Just 36
    "tab" -> Just 48
    "space" -> Just 49
    "backspace" -> Just 51
    "escape" -> Just 53
    "esc" -> Just 53
    "command" -> Just 55
    "cmd" -> Just 55
    "meta" -> Just 55
    "shift" -> Just 56
    "capslock" -> Just 57
    "caps_lock" -> Just 57
    "option" -> Just 58
    "alt" -> Just 58
    "control" -> Just 59
    "ctrl" -> Just 59
    "home" -> Just 115
    "pageup" -> Just 116
    "page_up" -> Just 116
    "delete" -> Just 117
    "del" -> Just 117
    "end" -> Just 119
    "pagedown" -> Just 121
    "page_down" -> Just 121
    "left" -> Just 123
    "arrowleft" -> Just 123
    "arrow_left" -> Just 123
    "right" -> Just 124
    "arrowright" -> Just 124
    "arrow_right" -> Just 124
    "down" -> Just 125
    "arrowdown" -> Just 125
    "arrow_down" -> Just 125
    "up" -> Just 126
    "arrowup" -> Just 126
    "arrow_up" -> Just 126
    _ -> Nothing

-- ANSI virtual key codes are needed only for modified printable shortcuts.
-- Unmodified text goes through Unicode CGEvents and therefore follows no
-- keyboard-layout assumptions.
characterKeyCode :: Text -> Maybe Int
characterKeyCode key = lookup key
    [ ("a", 0), ("s", 1), ("d", 2), ("f", 3), ("h", 4), ("g", 5)
    , ("z", 6), ("x", 7), ("c", 8), ("v", 9), ("b", 11)
    , ("q", 12), ("w", 13), ("e", 14), ("r", 15), ("y", 16), ("t", 17)
    , ("1", 18), ("2", 19), ("3", 20), ("4", 21), ("6", 22), ("5", 23)
    , ("=", 24), ("9", 25), ("7", 26), ("-", 27), ("8", 28), ("0", 29)
    , ("]", 30), ("o", 31), ("u", 32), ("[", 33), ("i", 34), ("p", 35)
    , ("l", 37), ("j", 38), ("'", 39), ("k", 40), (";", 41), ("\\", 42)
    , (",", 43), ("/", 44), ("n", 45), ("m", 46), (".", 47), ("`", 50)
    ]

modifierFlags :: [Text] -> Either Text Text
modifierFlags rawKeys =
    fmap render $ traverse flag (map normalize rawKeys)
  where
    flag = \case
        "cmd" -> Right "$.kCGEventFlagMaskCommand"
        "command" -> Right "$.kCGEventFlagMaskCommand"
        "meta" -> Right "$.kCGEventFlagMaskCommand"
        "ctrl" -> Right "$.kCGEventFlagMaskControl"
        "control" -> Right "$.kCGEventFlagMaskControl"
        "alt" -> Right "$.kCGEventFlagMaskAlternate"
        "option" -> Right "$.kCGEventFlagMaskAlternate"
        "shift" -> Right "$.kCGEventFlagMaskShift"
        unsupported ->
            Left ("Unsupported computer modifier: " <> unsupported)
    render [] = "0"
    render values =
        Text.intercalate "|" ["Number(" <> value <> ")" | value <- values]

buttonNumber :: Text -> Either Text Text
buttonNumber raw = case normalize raw of
    "left" -> Right "0"
    "right" -> Right "1"
    "wheel" -> Right "2"
    "middle" -> Right "2"
    "back" -> Right "3"
    "forward" -> Right "4"
    unsupported -> Left ("Unsupported computer mouse button: " <> unsupported)

typeTextCommand :: Text -> Text
typeTextCommand value =
    "typeText(" <> javascriptString value <> ");"

javascriptString :: Text -> Text
javascriptString =
    TextEncoding.decodeUtf8 . LBS.toStrict . Aeson.encode

screenshotMacOS :: IO (Either Text ImageAttachment)
screenshotMacOS =
    ensureUnlockedSession >>= \case
        Left err -> pure (Left err)
        Right () -> screenshotUnlockedMacOS

screenshotUnlockedMacOS :: IO (Either Text ImageAttachment)
screenshotUnlockedMacOS = do
    dimensions <- mainDisplayLogicalSize
    case dimensions of
        Left err -> pure (Left err)
        Right display -> screenshotMainDisplay display

screenshotMainDisplay :: (Int, Int) -> IO (Either Text ImageAttachment)
screenshotMainDisplay = screenshotMainDisplayWith ScreenshotPng

screenshotMainDisplayWith
    :: ScreenshotEncoding
    -> (Int, Int)
    -> IO (Either Text ImageAttachment)
screenshotMainDisplayWith encoding (width, height) = do
    temporaryDirectory <- getTemporaryDirectory
    attempted <- tryAny do
        let (suffix, format, mime, formatOptions) = case encoding of
                ScreenshotPng ->
                    (".png", "png", "image/png", [])
                -- Responses Lite accounts inline image bytes against its
                -- context. Preserve logical display dimensions while making
                -- iterative screenshots substantially smaller.
                ScreenshotJpeg ->
                    (".jpg", "jpg", "image/jpeg",
                        ["-s", "formatOptions", "80"])
        (path, handle) <- openBinaryTempFile temporaryDirectory
            ("agent-computer-use-" <> suffix)
        hClose handle
        let cleanup = removeFile path
        flip finally cleanup do
            capture <- readProcessWithExitCode
                "/usr/sbin/screencapture"
                ["-x", "-m", "-C", "-t", format, path]
                ""
            case capture of
                (ExitFailure _, _, stderr) ->
                    pure (Left (commandError "screencapture" stderr))
                (ExitSuccess, _, _) -> do
                    resized <- readProcessWithExitCode
                        "/usr/bin/sips"
                        ([ "-z", show height, show width ]
                            <> formatOptions
                            <> [path])
                        ""
                    case resized of
                        (ExitFailure _, _, stderr) ->
                            pure (Left (commandError "screenshot resize" stderr))
                        (ExitSuccess, _, _) -> do
                            bytes <- BS.readFile path
                            pure $ if BS.null bytes
                                then Left
                                    "Screen capture returned an empty image. Grant Screen Recording permission to the terminal or agent app."
                                else Right (ImageAttachment mime bytes)
    pure $ either (Left . Text.pack . show) id attempted

mainDisplayLogicalSize :: IO (Either Text (Int, Int))
mainDisplayLogicalSize = do
    let script = Text.unlines
            [ "ObjC.import('CoreGraphics');"
            , "const b=$.CGDisplayBounds($.CGMainDisplayID());"
            , "String(Math.round(Number(b.size.width)))+','+String(Math.round(Number(b.size.height)));"
            ]
    attempted <- tryAny $ readProcessWithExitCode
        "/usr/bin/osascript" ["-l", "JavaScript"] (Text.unpack script)
    pure $ case attempted of
        Left exception -> Left (Text.pack (show exception))
        Right (ExitFailure _, _, stderr) ->
            Left (commandError "main display query" stderr)
        Right (ExitSuccess, stdout, _) ->
            maybe
                (Left "macOS returned an invalid main-display size.")
                Right
                (parseDisplaySize (Text.pack stdout))

parseDisplaySize :: Text -> Maybe (Int, Int)
parseDisplaySize value =
    case Text.splitOn "," (Text.strip value) of
        [widthText, heightText]
            | Text.all isDigit widthText
            , Text.all isDigit heightText
            , Just width <- readMaybe (Text.unpack widthText)
            , Just height <- readMaybe (Text.unpack heightText)
            , width > 0
            , height > 0 ->
                Just (width, height)
        _ -> Nothing

ensureUnlockedSession :: IO (Either Text ())
ensureUnlockedSession = do
    let script = Text.unlines
            [ "ObjC.import('CoreGraphics');"
            , "const d=ObjC.deepUnwrap($.CGSessionCopyCurrentDictionary());"
            , "if(!d) throw new Error('macOS GUI session state is unavailable');"
            , "String(Boolean(d && d.CGSSessionScreenIsLocked));"
            ]
    attempted <- tryAny $ readProcessWithExitCode
        "/usr/bin/osascript" ["-l", "JavaScript"] (Text.unpack script)
    pure $ case attempted of
        Left exception -> Left (Text.pack (show exception))
        Right (ExitFailure _, _, stderr) ->
            Left (commandError "GUI session query" stderr)
        Right (ExitSuccess, stdout, _) ->
            case parseSessionLocked (Text.pack stdout) of
                Right False -> Right ()
                Right True ->
                    Left
                        "Computer use is unavailable while the macOS session is locked."
                Left err -> Left err

parseSessionLocked :: Text -> Either Text Bool
parseSessionLocked value =
    case Text.toLower (Text.strip value) of
        "true" -> Right True
        "false" -> Right False
        _ -> Left "macOS returned an invalid GUI session lock state."

runJxa :: Text -> IO (Either Text ())
runJxa = runScript ["-l", "JavaScript"]

-- The script is sent over stdin, not argv, so typed text is absent from process
-- listings and exception command lines.
runScript :: [String] -> Text -> IO (Either Text ())
runScript arguments script = do
    attempted <- tryAny $ readProcessWithExitCode
        "/usr/bin/osascript" arguments (Text.unpack script)
    pure $ case attempted of
        Left exception -> Left (Text.pack (show exception))
        Right (ExitSuccess, _, _) -> Right ()
        Right (ExitFailure _, _, stderr) ->
            Left (commandError "macOS input automation" stderr)

-- | A nonsecret summary suitable for approval UI and persisted tool cards.
-- Typed content is represented only by its character count.
summarizeComputerCall :: ComputerCall -> Text
summarizeComputerCall call =
    Text.intercalate "; " $
        map summary (take 128 call.computerActions)
            <> map safety (take 64 call.pendingSafetyChecks)
  where
    summary = \case
        ScreenshotAction -> "capture main-display screenshot"
        ClickAction { clickX, clickY, clickButton, clickKeys } ->
            withKeys clickKeys $
                safeQuoted 32 clickButton <> " click at " <> ints [clickX, clickY]
        DoubleClickAction
            { doubleClickX, doubleClickY, doubleClickKeys } ->
            withKeys doubleClickKeys $
                "double-click at " <> ints [doubleClickX, doubleClickY]
        TypeAction value ->
            let prefix = Text.take 8193 value
                count = Text.length prefix
            in if count > 8192
                then "type more than 8192 characters"
                else "type " <> Text.pack (show count) <> " characters"
        KeypressAction keys ->
            "press " <> Text.intercalate "+" (map (safeQuoted 64) (take 16 keys))
        ScrollAction { scrollDx, scrollDy, scrollKeys } ->
            withKeys scrollKeys $ "scroll by " <> ints [scrollDx, scrollDy]
        MoveAction { moveX, moveY, moveKeys } ->
            withKeys moveKeys $ "move pointer to " <> ints [moveX, moveY]
        WaitAction -> "wait 2s"
        DragAction { dragPath, dragKeys } ->
            withKeys dragKeys $
                "drag through "
                    <> if exceedsList 1024 dragPath
                        then "more than 1024 points"
                        else
                            Text.pack (show (length (take 1024 dragPath)))
                                <> " points"
        UnknownComputerAction value -> "unsupported " <> safeQuoted 128 value.tag
    safety check =
        "safety check "
            <> safeQuoted 1024
                (maybe check.safetyCheckId id check.safetyCheckMessage)
    withKeys [] description = description
    withKeys keys description =
        description
            <> " with "
            <> Text.intercalate "+" (map (safeQuoted 64) (take 16 keys))

safeQuoted :: Int -> Text -> Text
safeQuoted limit value =
    TextEncoding.decodeUtf8 $ LBS.toStrict $ Aeson.encode $
        Text.take limit (Text.map replaceControl value)
  where
    replaceControl character
        | isControl character = ' '
        | otherwise = character

summarizeComputerToolCall :: ToolCall -> Maybe Text
summarizeComputerToolCall call
    | call.name /= "computer"
        || not (isComputerToolCallKind call.callKind) = Nothing
    | otherwise =
        case Json.decodeText computerToolInputDecoder call.arguments of
            Left _ -> Just "Computer action"
            Right input ->
                let computerCall = computerCallFromInput call input
                    detail = summarizeComputerCall computerCall
                in Just $
                    if Text.null detail
                        then "Computer action"
                        else "Computer: " <> detail

computerApprovalPrompt :: ToolCall -> Maybe Text
computerApprovalPrompt call =
    fmap ("Allow this computer action?\n\n" <>) $
        summarizeComputerToolCall call

dataUrl :: Text -> BS.ByteString -> Text
dataUrl mime bytes =
    "data:" <> mime <> ";base64,"
        <> TextEncoding.decodeUtf8 (Base64.encode bytes)

ints :: [Int] -> Text
ints = Text.intercalate "," . map (Text.pack . show)

normalize :: Text -> Text
normalize = Text.toLower . Text.strip

commandError :: Text -> String -> Text
commandError command stderr =
    let detail = Text.strip (Text.pack stderr)
    in if Text.null detail
        then command
            <> " failed. Check Screen Recording and Accessibility permissions."
        else command <> " failed: " <> detail
