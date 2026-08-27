-- | Provider-native computer use backed by macOS screen capture and input.
module Agent.CLI.ComputerUse
    ( computerUseTool
    , executeComputerCall
    , screenshotMacOS
    , summarizeComputerCall
    ) where

import Agent.Loop (ImageAttachment(..))
import Agent.Responses.Types
    ( ComputerAction(..)
    , ComputerCall(..)
    , ComputerCallOutput(..)
    , ComputerPoint(..)
    , TaggedObject(..)
    )
import Agent.ToolDispatch (noArgsTool, typedTool)
import Agent.Tools.Types
    ( AppTool
    , ApprovalRule(..)
    , ToolExecutionPolicy(..)
    , jsonAppToolWithExecution
    )
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (finally, tryAny)
import Control.Monad (foldM)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (ExitCode(..))
import System.Info (os)
import System.IO (hClose, openBinaryTempFile)
import System.Process (readProcessWithExitCode)

-- | This schema is never sent as a function schema: 'Agent.CLI.Tools' projects
-- this reserved app tool to the Responses built-in @{type:"computer"}@ tool.
computerUseTool :: AppTool
computerUseTool = jsonAppToolWithExecution
    "computer"
    "Control the local macOS desktop using screenshots, pointer, keyboard, and scrolling."
    []
    AlwaysPrompt
    TurnSequential
    handler
  where
    handler
        | os == "darwin" = typedTool "computer" executeComputerCall
        | otherwise = noArgsTool "computer"
            (pure (Left "Local computer use is currently supported only on macOS."))

executeComputerCall :: ComputerCall -> IO (Either Text Text)
executeComputerCall call = do
    actionResult <- foldM run (Right ()) call.computerActions
    case actionResult of
        Left err -> pure (Left err)
        Right () -> screenshotMacOS >>= \case
            Left err -> pure (Left err)
            Right ImageAttachment{imageMime, imageBytes} ->
                pure $ Right $ TextEncoding.decodeUtf8 $ LBS.toStrict $ Aeson.encode
                    ComputerCallOutput
                        { computerOutputItemId = Nothing
                        , computerOutputCallId = call.computerCallId
                        , screenshotDataUrl = dataUrl imageMime imageBytes
                        , acknowledgedChecks = call.pendingSafetyChecks
                        , computerOutputStatus = Nothing
                        , computerOutputExtra = KeyMap.empty
                        }
  where
    run (Left err) _ = pure (Left err)
    run (Right ()) action = executeAction action

executeAction :: ComputerAction -> IO (Either Text ())
executeAction = \case
    ScreenshotAction -> pure (Right ())
    WaitAction milliseconds -> do
        threadDelay (max 0 (min 30000 milliseconds) * 1000)
        pure (Right ())
    action@ClickAction{} -> runJxa (pointerScript action)
    action@DoubleClickAction{} -> runJxa (pointerScript action)
    action@ScrollAction{} -> runJxa (pointerScript action)
    action@MoveAction{} -> runJxa (pointerScript action)
    action@DragAction{} -> runJxa (pointerScript action)
    TypeAction value -> runAppleScript
        ("tell application \"System Events\" to keystroke " <> appleString value)
    KeypressAction keys ->
        maybe
            (pure (Left "Unsupported computer key combination."))
            runAppleScript
            (keyCombinationScript keys)
    UnknownComputerAction value ->
        pure (Left ("Unsupported computer action: " <> value.tag))

-- Screen coordinates returned to the model are the same global points used by
-- CoreGraphics. A future window-scoped executor can replace this boundary.
pointerScript :: ComputerAction -> Text
pointerScript action = jxaPrelude <> case action of
    ClickAction { clickX, clickY, clickButton } ->
        "click(" <> ints [clickX, clickY] <> ","
            <> jxaButton clickButton <> ");"
    DoubleClickAction { doubleClickX, doubleClickY } ->
        "click(" <> ints [doubleClickX, doubleClickY]
            <> ",0); delay(0.08); click("
            <> ints [doubleClickX, doubleClickY] <> ",0);"
    MoveAction { moveX, moveY } ->
        "move(" <> ints [moveX, moveY] <> ");"
    ScrollAction { scrollX, scrollY, scrollDx, scrollDy } ->
        "move(" <> ints [scrollX, scrollY] <> "); scroll("
            <> ints [scrollDx, scrollDy] <> ");"
    DragAction points -> case points of
        [] -> "throw new Error('empty drag path');"
        ComputerPoint { pointX, pointY } : rest ->
            "down(" <> ints [pointX, pointY] <> ");"
                <> Text.concat
                    [ "delay(0.02); drag(" <> ints [px, py] <> ");"
                    | ComputerPoint { pointX = px, pointY = py } <- rest
                    ]
                <> "up();"
    _ -> "throw new Error('not a pointer action');"

jxaPrelude :: Text
jxaPrelude = Text.unlines
    [ "ObjC.import('CoreGraphics');"
    , "const tap=$.kCGHIDEventTap, left=$.kCGMouseButtonLeft;"
    , "let last=$.CGPointMake(0,0);"
    , "function post(t,x,y,b){last=$.CGPointMake(x,y); const e=$.CGEventCreateMouseEvent(null,t,last,b); $.CGEventPost(tap,e); }"
    , "function move(x,y){post($.kCGEventMouseMoved,x,y,left);}"
    , "function click(x,y,b){const d=b===1?$.kCGEventRightMouseDown:b===2?$.kCGEventOtherMouseDown:$.kCGEventLeftMouseDown; const u=b===1?$.kCGEventRightMouseUp:b===2?$.kCGEventOtherMouseUp:$.kCGEventLeftMouseUp; post(d,x,y,b); post(u,x,y,b);}"
    , "function scroll(dx,dy){const e=$.CGEventCreateScrollWheelEvent(null,$.kCGScrollEventUnitPixel,2,dy,dx); $.CGEventPost(tap,e);}"
    , "function down(x,y){post($.kCGEventLeftMouseDown,x,y,left);}"
    , "function drag(x,y){post($.kCGEventLeftMouseDragged,x,y,left);}"
    , "function up(){post($.kCGEventLeftMouseUp,last.x,last.y,left);}"
    ]

keyCombinationScript :: [Text] -> Maybe Text
keyCombinationScript [] = Nothing
keyCombinationScript rawKeys = do
    let keys = map (Text.toLower . Text.strip) rawKeys
    modifiers <- traverse modifier (init keys)
    let suffix = case modifiers of
            [] -> ""
            values -> " using {" <> Text.intercalate ", " values <> "}"
    command <- keyCommand (last keys)
    pure ("tell application \"System Events\" to " <> command <> suffix)
  where
    modifier = \case
        "cmd" -> Just "command down"
        "command" -> Just "command down"
        "ctrl" -> Just "control down"
        "control" -> Just "control down"
        "alt" -> Just "option down"
        "option" -> Just "option down"
        "shift" -> Just "shift down"
        _ -> Nothing
    keyCommand = \case
        "enter" -> Just "key code 36"
        "return" -> Just "key code 36"
        "tab" -> Just "key code 48"
        "space" -> Just "key code 49"
        "delete" -> Just "key code 51"
        "backspace" -> Just "key code 51"
        "escape" -> Just "key code 53"
        "left" -> Just "key code 123"
        "right" -> Just "key code 124"
        "down" -> Just "key code 125"
        "up" -> Just "key code 126"
        value | Text.length value == 1 ->
            Just ("keystroke " <> appleString value)
        _ -> Nothing

screenshotMacOS :: IO (Either Text ImageAttachment)
screenshotMacOS = do
    temporaryDirectory <- getTemporaryDirectory
    attempted <- tryAny do
        (path, handle) <- openBinaryTempFile temporaryDirectory
            "agent-computer-use-.png"
        hClose handle
        let cleanup = removeFile path
        flip finally cleanup do
            (exitCode, _, stderr) <- readProcessWithExitCode
                "/usr/sbin/screencapture" ["-x", "-t", "png", path] ""
            case exitCode of
                ExitFailure _ -> pure (Left (commandError "screencapture" stderr))
                ExitSuccess -> do
                    bytes <- BS.readFile path
                    pure $ if BS.null bytes
                        then Left "Screen capture returned an empty image. Grant Screen Recording permission to the terminal or agent app."
                        else Right (ImageAttachment "image/png" bytes)
    pure $ either (Left . Text.pack . show) id attempted

runJxa :: Text -> IO (Either Text ())
runJxa = runScript ["-l", "JavaScript"]

runAppleScript :: Text -> IO (Either Text ())
runAppleScript = runScript []

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

summarizeComputerCall :: ComputerCall -> Text
summarizeComputerCall call = Text.intercalate ", " (map summary call.computerActions)
  where
    summary = \case
        ScreenshotAction -> "capture screenshot"
        ClickAction { clickX, clickY, clickButton } ->
            clickButton <> " click at " <> ints [clickX, clickY]
        DoubleClickAction { doubleClickX, doubleClickY } ->
            "double-click at " <> ints [doubleClickX, doubleClickY]
        TypeAction value -> "type " <> Text.pack (show (Text.length value)) <> " characters"
        KeypressAction keys -> "press " <> Text.intercalate "+" keys
        ScrollAction { scrollDx, scrollDy } ->
            "scroll by " <> ints [scrollDx, scrollDy]
        MoveAction { moveX, moveY } ->
            "move pointer to " <> ints [moveX, moveY]
        WaitAction ms -> "wait " <> Text.pack (show ms) <> "ms"
        DragAction path -> "drag through " <> Text.pack (show (length path)) <> " points"
        UnknownComputerAction value -> "unsupported " <> value.tag

dataUrl :: Text -> BS.ByteString -> Text
dataUrl mime bytes =
    "data:" <> mime <> ";base64,"
        <> TextEncoding.decodeUtf8 (Base64.encode bytes)

ints :: [Int] -> Text
ints = Text.intercalate "," . map (Text.pack . show)

jxaButton :: Text -> Text
jxaButton button
    | Text.toLower button == "right" = "1"
    | Text.toLower button == "middle" = "2"
    | otherwise = "0"

appleString :: Text -> Text
appleString value = "\"" <> Text.concatMap escape value <> "\""
  where
    escape '\"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape character = Text.singleton character

commandError :: Text -> String -> Text
commandError command stderr =
    let detail = Text.strip (Text.pack stderr)
    in if Text.null detail
        then command <> " failed. Check Screen Recording and Accessibility permissions."
        else command <> " failed: " <> detail
