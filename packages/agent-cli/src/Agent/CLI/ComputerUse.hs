-- | Provider-native computer use backed by macOS screen capture and input.
module Agent.CLI.ComputerUse
    ( computerUseTool
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
    ) where

import Agent.Loop (ImageAttachment(..))
import Agent.Responses.Types
    ( ComputerAction(..)
    , ComputerCall(..)
    , ComputerCallOutput(..)
    , ComputerPoint(..)
    , SafetyCheck(..)
    , TaggedObject(..)
    )
import Agent.ToolDispatch (ToolCall(..), noArgsTool, typedTool)
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRule(..)
    , ToolExecutionPolicy(..)
    , ToolSchema(..)
    )
import Control.Concurrent (threadDelay)
import Control.Applicative ((<|>))
import Control.Exception.Safe (finally, tryAny)
import Control.Monad (foldM)
import qualified Data.Aeson as Aeson
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

-- | A dedicated schema constructor, rather than the name @computer@, marks
-- this as the provider-hosted computer tool. That prevents an MCP/function
-- tool with the same name from accidentally acquiring desktop control.
computerUseTool :: AppTool
computerUseTool = AppTool
    { appToolName = "computer"
    , appToolDescription =
        "Control the main macOS display using screenshots, pointer, keyboard, and scrolling."
    , appToolSchema = HostedComputerSchema
    , appToolHandler = handler
    , appToolApproval = AlwaysPrompt
    , appToolExecution = TurnSequential
    , appToolResourceClaims = Nothing
    }
  where
    handler
        | os == "darwin" = typedTool "computer" executeComputerCall
        | otherwise = noArgsTool "computer"
            (pure (Left "Local computer use is currently supported only on macOS."))

executeComputerCall :: ComputerCall -> IO (Either Text Text)
executeComputerCall call
    | Left err <- validateComputerCall call = pure (Left err)
    | otherwise = do
        unlocked <- ensureUnlockedSession
        case unlocked of
            Left err -> pure (Left err)
            Right () -> do
                actionResult <- foldM run (Right ()) call.computerActions
                case actionResult of
                    Left err -> pure (Left err)
                    Right () -> screenshotMacOS >>= \case
                        Left err -> pure (Left err)
                        Right ImageAttachment{imageMime, imageBytes} ->
                            pure $ Right $
                                TextEncoding.decodeUtf8 $ LBS.toStrict $
                                    Aeson.encode ComputerCallOutput
                                        { computerOutputItemId = Nothing
                                        , computerOutputCallId =
                                            call.computerCallId
                                        , screenshotDataUrl =
                                            dataUrl imageMime imageBytes
                                        -- Reaching the handler means this
                                        -- exact call was explicitly approved.
                                        , acknowledgedChecks =
                                            call.pendingSafetyChecks
                                        , computerOutputStatus = Nothing
                                        , computerOutputExtra = KeyMap.empty
                                        }
  where
    run (Left err) _ = pure (Left err)
    run (Right ()) action = executeAction action

validateComputerCall :: ComputerCall -> Either Text ()
validateComputerCall call
    | exceedsList 128 call.computerActions =
        Left "Computer call exceeds the 128-action limit."
    | exceedsList 64 call.pendingSafetyChecks =
        Left "Computer call exceeds the 64-safety-check limit."
    | Just err <- firstJust
        (map validateSafetyCheck call.pendingSafetyChecks
            <> map validateAction call.computerActions) =
        Left err
    | otherwise = Right ()

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
        | otherwise -> validateKeys clickKeys
    DoubleClickAction { doubleClickKeys } -> validateKeys doubleClickKeys
    ScrollAction { scrollKeys } -> validateKeys scrollKeys
    MoveAction { moveKeys } -> validateKeys moveKeys
    DragAction { dragPath, dragKeys }
        | exceedsList 1024 dragPath ->
            Just "Computer drag path exceeds 1024 points."
        | otherwise -> validateKeys dragKeys
    KeypressAction keys -> validateKeys keys
    UnknownComputerAction value
        | exceedsText 128 value.tag ->
            Just "Computer action type exceeds 128 characters."
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
    WaitAction milliseconds
        | milliseconds < 0 || milliseconds > 30000 ->
            pure (Left "Computer wait must be between 0 and 30000 milliseconds.")
        | otherwise -> do
            threadDelay (milliseconds * 1000)
            pure (Right ())
    action@ClickAction{} -> runPointerAction action
    action@DoubleClickAction{} -> runPointerAction action
    action@ScrollAction{} -> runPointerAction action
    action@MoveAction{} -> runPointerAction action
    action@DragAction{} -> runPointerAction action
    TypeAction value
        | exceedsText 8192 value ->
            pure (Left "Computer text input exceeds the 8192-character limit.")
        | otherwise ->
            runJxa (keyboardPrelude <> typeTextCommand value)
    KeypressAction keys ->
        either (pure . Left) runJxa (keyCombinationScript keys)
    UnknownComputerAction value ->
        pure (Left ("Unsupported computer action: " <> value.tag))

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
                ComputerPoint { pointX, pointY } : rest ->
                    pure $
                        "down(" <> ints [pointX, pointY] <> "," <> flags
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
    , "function assertReady(){const d=ObjC.deepUnwrap($.CGSessionCopyCurrentDictionary()); if(d&&Boolean(d.CGSSessionScreenIsLocked)) throw new Error('macOS session is locked'); if(!Boolean($.AXIsProcessTrusted())) throw new Error('Accessibility permission is required');}"
    , "assertReady();"
    , "const bounds=$.CGDisplayBounds($.CGMainDisplayID());"
    , "let last=$.CGPointMake(0,0);"
    , "function check(x,y){if(x<0||y<0||x>=Number(bounds.size.width)||y>=Number(bounds.size.height)) throw new Error('point outside main display');}"
    , "function kinds(b){return b===0?[$.kCGEventLeftMouseDown,$.kCGEventLeftMouseUp]:b===1?[$.kCGEventRightMouseDown,$.kCGEventRightMouseUp]:[$.kCGEventOtherMouseDown,$.kCGEventOtherMouseUp];}"
    , "function post(t,x,y,b,f,c){check(x,y); last=$.CGPointMake(x,y); const e=$.CGEventCreateMouseEvent(null,t,last,b); $.CGEventSetFlags(e,f); if(c) $.CGEventSetIntegerValueField(e,$.kCGMouseEventClickState,c); $.CGEventPost(tap,e);}"
    , "function move(x,y,f){post($.kCGEventMouseMoved,x,y,left,f,0);}"
    , "function click(x,y,b,f,count){const k=kinds(b); post(k[0],x,y,b,f,1); post(k[1],x,y,b,f,1); if(count===2){delay(0.08); post(k[0],x,y,b,f,2); post(k[1],x,y,b,f,2);}}"
    , "function scroll(dx,dy,f){const e=$.CGEventCreateScrollWheelEvent(null,$.kCGScrollEventUnitPixel,2,dy,dx); $.CGEventSetFlags(e,f); $.CGEventPost(tap,e);}"
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
    , "function assertReady(){const d=ObjC.deepUnwrap($.CGSessionCopyCurrentDictionary()); if(d&&Boolean(d.CGSSessionScreenIsLocked)) throw new Error('macOS session is locked'); if(!Boolean($.AXIsProcessTrusted())) throw new Error('Accessibility permission is required');}"
    , "assertReady();"
    , "function key(code,flags){const d=$.CGEventCreateKeyboardEvent(null,code,true),u=$.CGEventCreateKeyboardEvent(null,code,false); $.CGEventSetFlags(d,flags); $.CGEventSetFlags(u,flags); $.CGEventPost(tap,d); $.CGEventPost(tap,u);}"
    , "function typeText(raw){const value=$(raw),n=Number(value.length); const d=$.CGEventCreateKeyboardEvent(null,0,true),u=$.CGEventCreateKeyboardEvent(null,0,false); $.CGEventKeyboardSetUnicodeString(d,n,value); $.CGEventKeyboardSetUnicodeString(u,n,value); $.CGEventPost(tap,d); $.CGEventPost(tap,u);}"
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
    "delete" -> Just 51
    "backspace" -> Just 51
    "escape" -> Just 53
    "esc" -> Just 53
    "left" -> Just 123
    "right" -> Just 124
    "down" -> Just 125
    "up" -> Just 126
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
screenshotMacOS = do
    dimensions <- mainDisplayLogicalSize
    case dimensions of
        Left err -> pure (Left err)
        Right (width, height) -> do
            temporaryDirectory <- getTemporaryDirectory
            attempted <- tryAny do
                (path, handle) <- openBinaryTempFile temporaryDirectory
                    "agent-computer-use-.png"
                hClose handle
                let cleanup = removeFile path
                flip finally cleanup do
                    capture <- readProcessWithExitCode
                        "/usr/sbin/screencapture"
                        ["-x", "-m", "-C", "-t", "png", path]
                        ""
                    case capture of
                        (ExitFailure _, _, stderr) ->
                            pure (Left (commandError "screencapture" stderr))
                        (ExitSuccess, _, _) -> do
                            resized <- readProcessWithExitCode
                                "/usr/bin/sips"
                                [ "-z", show height, show width, path ]
                                ""
                            case resized of
                                (ExitFailure _, _, stderr) ->
                                    pure (Left (commandError "screenshot resize" stderr))
                                (ExitSuccess, _, _) -> do
                                    bytes <- BS.readFile path
                                    pure $ if BS.null bytes
                                        then Left
                                            "Screen capture returned an empty image. Grant Screen Recording permission to the terminal or agent app."
                                        else Right
                                            (ImageAttachment "image/png" bytes)
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
        WaitAction ms -> "wait " <> Text.pack (show ms) <> "ms"
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
    | call.name /= "computer" = Nothing
    | otherwise =
        case Aeson.eitherDecodeStrict'
            (TextEncoding.encodeUtf8 call.arguments) of
            Left _ -> Just "Computer action"
            Right computerCall ->
                let detail = summarizeComputerCall computerCall
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
