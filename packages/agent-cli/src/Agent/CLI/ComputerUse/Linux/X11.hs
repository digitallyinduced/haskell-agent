module Agent.CLI.ComputerUse.Linux.X11
    ( XdotoolInvocation(..)
    , newX11Backend
    , parseXrandrDisplay
    , pinX11DisplayEnvironment
    , runX11TypeInvocationsWith
    , withX11InputCleanup
    , withX11Readiness
    , x11KeyInvocation
    , x11PointerPosition
    , x11ScrollInvocations
    , x11TypeInvocations
    ) where

import Agent.CLI.ComputerUse.Backend
    ( CapturedDisplay(..)
    , ComputerBackend(..)
    , ComputerDisplay(..)
    , ScreenshotEncoding(..)
    )
import Agent.CLI.ComputerUse.Input
    ( ComputerKey(..)
    , Modifier(..)
    , MouseButton(..)
    , NamedKey(..)
    , parseComputerKeyCombination
    , parseModifiers
    , parseMouseButton
    )
import Agent.CLI.ComputerUse.Linux.Logind (withLogindReadiness)
import Agent.Loop (ImageAttachment(..))
import Agent.Responses.Types
    ( ComputerAction(..)
    , ComputerPoint(..)
    , TaggedObject(..)
    )
import Codec.Picture
    ( DynamicImage
    , convertRGB8
    , decodeImage
    , dynamicMap
    , encodeJpegAtQuality
    , imageHeight
    , imageWidth
    )
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (finally, generalBracket, tryAny)
import Control.Monad (foldM)
import Codec.Picture.Types (convertImage)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isDigit)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.IO (hClose, openBinaryTempFile)
import System.Process
    ( CreateProcess(..)
    , proc
    , readCreateProcessWithExitCode
    )
import Text.Read (readMaybe)

data XdotoolInvocation = XdotoolInvocation
    { xdotoolArguments :: ![String]
    , xdotoolStdin :: !Text
    } deriving (Eq, Show)

newX11Backend :: Text -> IO (Either Text ()) -> IO ComputerBackend
newX11Backend display readiness = do
    processEnvironment <-
        pinX11DisplayEnvironment display <$> getEnvironment
    pure ComputerBackend
        { computerBackendEnsureReady =
            readiness >>= \case
                Left err -> pure (Left err)
                Right () -> ensureX11Ready processEnvironment
        , computerBackendInspectDisplay =
            inspectX11Display processEnvironment
        , computerBackendExecuteAction =
            executeX11Action processEnvironment readiness
        , computerBackendCaptureDisplay =
            captureX11Display processEnvironment readiness
        , computerBackendClose = pure ()
        }

pinX11DisplayEnvironment
    :: Text
    -> [(String, String)]
    -> [(String, String)]
pinX11DisplayEnvironment display environment =
    ("DISPLAY", Text.unpack display)
        : filter ((/= "DISPLAY") . fst) environment

ensureX11Ready :: [(String, String)] -> IO (Either Text ())
ensureX11Ready processEnvironment =
    fmap (fmap (const ())) (inspectX11Display processEnvironment)

inspectX11Display
    :: [(String, String)]
    -> IO (Either Text ComputerDisplay)
inspectX11Display processEnvironment = do
    attempted <- tryAny $
        readCreateProcessWithExitCode
            (processWithEnvironment
                processEnvironment
                "xrandr"
                ["--listactivemonitors"])
            ""
    pure case attempted of
        Left exception ->
            Left ("Unable to query the X11 display with xrandr: "
                <> Text.pack (show exception))
        Right (ExitFailure _, _, stderr) ->
            Left (processError "xrandr --listactivemonitors" stderr)
        Right (ExitSuccess, stdout, _) ->
            parseXrandrDisplay (Text.pack stdout)

parseXrandrDisplay :: Text -> Either Text ComputerDisplay
parseXrandrDisplay output = do
    let monitorLines =
            filter (Text.isInfixOf ":") (drop 1 (Text.lines output))
        monitors = mapMaybe parseMonitor monitorLines
    if length monitors /= length monitorLines
        then Left "xrandr returned an invalid active-monitor description."
        else selectMonitor monitors
  where
    selectMonitor [] =
        Left "xrandr reported no active monitors."
    selectMonitor [(_, display)] =
        Right display
    selectMonitor monitors =
        case [display | (True, display) <- monitors] of
            [display] -> Right display
            [] ->
                Left
                    "xrandr reported multiple active monitors but no primary monitor."
            _ ->
                Left "xrandr reported multiple primary monitors."

parseMonitor :: Text -> Maybe (Bool, ComputerDisplay)
parseMonitor line = do
    (_index, flagsAndName, geometry) <- case Text.words line of
        index : flags : bounds : _ -> Just (index, flags, bounds)
        _ -> Nothing
    let name = Text.dropWhile (`elem` ['+', '*']) flagsAndName
        primary = "*" `Text.isInfixOf` flagsAndName
    if Text.null name
        then Nothing
        else do
            (width, height, originX, originY) <- parseGeometry geometry
            pure
                ( primary
                , ComputerDisplay
                    { computerDisplayId =
                        "x11:" <> name <> ":("
                            <> commaInts [originX, originY, width, height]
                            <> ")"
                    , computerDisplayOriginX = originX
                    , computerDisplayOriginY = originY
                    , computerDisplayWidth = width
                    , computerDisplayHeight = height
                    , computerDisplayFrameWidth = width
                    , computerDisplayFrameHeight = height
                    }
                )

parseGeometry :: Text -> Maybe (Int, Int, Int, Int)
parseGeometry value = do
    let (widthText, afterWidthSlash) = Text.breakOn "/" value
    afterWidth <- Text.stripPrefix "/" afterWidthSlash
    let (_physicalWidth, afterXMarker) = Text.breakOn "x" afterWidth
    afterX <- Text.stripPrefix "x" afterXMarker
    let (heightText, afterHeightSlash) = Text.breakOn "/" afterX
    afterHeight <- Text.stripPrefix "/" afterHeightSlash
    let originText = Text.dropWhile isDigit afterHeight
    width <- positiveInt widthText
    height <- positiveInt heightText
    (originX, rest) <- signedInt originText
    (originY, trailing) <- signedInt rest
    if Text.null trailing
        then Just (width, height, originX, originY)
        else Nothing

positiveInt :: Text -> Maybe Int
positiveInt value
    | Text.null value || not (Text.all isDigit value) = Nothing
    | otherwise = do
        number <- readMaybe (Text.unpack value)
        if number > 0 then Just number else Nothing

signedInt :: Text -> Maybe (Int, Text)
signedInt value = do
    (sign, unsigned) <- case Text.uncons value of
        Just ('+', rest) -> Just (1, rest)
        Just ('-', rest) -> Just (-1, rest)
        _ -> Nothing
    let (digits, trailing) = Text.span isDigit unsigned
    if Text.null digits
        then Nothing
        else do
            number <- readMaybe (Text.unpack digits)
            pure (sign * number, trailing)

x11PointerPosition :: ComputerDisplay -> Int -> Int -> (Int, Int)
x11PointerPosition display x y =
    ( display.computerDisplayOriginX + x
    , display.computerDisplayOriginY + y
    )

x11KeyInvocation :: [Text] -> Either Text XdotoolInvocation
x11KeyInvocation rawKeys = do
    (modifiers, key) <- parseComputerKeyCombination rawKeys
    case (modifiers, key) of
        ([], ComputerTextKey value) ->
            Right (typeInvocation value)
        _ ->
            Right XdotoolInvocation
                { xdotoolArguments =
                    [ "key"
                    , "--clearmodifiers"
                    , Text.unpack (Text.intercalate "+"
                        (map modifierName modifiers <> [computerKeyName key]))
                    ]
                , xdotoolStdin = ""
                }

x11ScrollInvocations :: Int -> Int -> [XdotoolInvocation]
x11ScrollInvocations dx dy =
    wheel dx "7" "6" <> wheel dy "5" "4"
  where
    wheel 0 _ _ = []
    wheel delta positiveButton negativeButton =
        [ XdotoolInvocation
            { xdotoolArguments =
                [ "click"
                , "--repeat"
                , show (scrollClicks delta)
                , if delta > 0 then positiveButton else negativeButton
                ]
            , xdotoolStdin = ""
            }
        ]

scrollClicks :: Int -> Int
scrollClicks delta =
    min 100 ((abs (toInteger delta) + 99) `div` 100)
        & fromInteger
  where
    (&) = flip ($)

executeX11Action
    :: [(String, String)]
    -> IO (Either Text ())
    -> ComputerDisplay
    -> ComputerAction
    -> IO (Either Text ())
executeX11Action processEnvironment readiness expected action =
    inspectX11Display processEnvironment >>= \case
        Left err -> pure (Left err)
        Right current
            | current /= expected ->
                pure (Left
                    "The selected display changed during computer use; take a fresh screenshot before continuing.")
            | otherwise ->
                withX11Readiness readiness
                    (executeX11ActionUnchecked
                        processEnvironment
                        readiness
                        expected
                        action)

executeX11ActionUnchecked
    :: [(String, String)]
    -> IO (Either Text ())
    -> ComputerDisplay
    -> ComputerAction
    -> IO (Either Text ())
executeX11ActionUnchecked processEnvironment readiness display = \case
    ScreenshotAction -> pure (Right ())
    WaitAction -> threadDelay 2000000 >> pure (Right ())
    TypeAction value ->
        runX11TypeInvocationsWith
            readiness
            (runXdotool processEnvironment)
            (x11TypeInvocations value)
    KeypressAction keys ->
        either
            (pure . Left)
            (runXdotool processEnvironment)
            (x11KeyInvocation keys)
    ClickAction{clickX, clickY, clickButton, clickKeys} ->
        case (parseMouseButton clickButton, parseModifiers clickKeys) of
            (Left err, _) -> pure (Left err)
            (_, Left err) -> pure (Left err)
            (Right button, Right modifiers) ->
                withX11Modifiers processEnvironment modifiers do
                    moved <-
                        movePointer
                            processEnvironment
                            display
                            clickX
                            clickY
                    continue moved
                        (runXdotool
                            processEnvironment
                            (arguments ["click", mouseButton button]))
    DoubleClickAction{doubleClickX, doubleClickY, doubleClickKeys} ->
        case parseModifiers doubleClickKeys of
            Left err -> pure (Left err)
            Right modifiers ->
                withX11Modifiers processEnvironment modifiers do
                    moved <-
                        movePointer
                            processEnvironment
                            display
                            doubleClickX
                            doubleClickY
                    continue moved
                        (runXdotool
                            processEnvironment
                            (arguments
                                ["click", "--repeat", "2", "--delay", "80", "1"]))
    MoveAction{moveX, moveY, moveKeys} ->
        case parseModifiers moveKeys of
            Left err -> pure (Left err)
            Right modifiers ->
                withX11Modifiers
                    processEnvironment
                    modifiers
                    (movePointer processEnvironment display moveX moveY)
    ScrollAction{scrollX, scrollY, scrollDx, scrollDy, scrollKeys} ->
        case parseModifiers scrollKeys of
            Left err -> pure (Left err)
            Right modifiers ->
                withX11Modifiers processEnvironment modifiers do
                    moved <-
                        movePointer
                            processEnvironment
                            display
                            scrollX
                            scrollY
                    case moved of
                        Left err -> pure (Left err)
                        Right () ->
                            runXdotoolInvocations processEnvironment
                                (x11ScrollInvocations scrollDx scrollDy)
    DragAction{dragPath, dragKeys} ->
        case parseModifiers dragKeys of
            Left err -> pure (Left err)
            Right modifiers ->
                withX11Modifiers
                    processEnvironment
                    modifiers
                    (dragPointer processEnvironment display dragPath)
    UnknownComputerAction value ->
        pure (Left ("Unsupported computer action: " <> value.tag))

dragPointer
    :: [(String, String)]
    -> ComputerDisplay
    -> [ComputerPoint]
    -> IO (Either Text ())
dragPointer _ _ [] =
    pure (Left "Computer drag path is empty.")
dragPointer _ _ [_] =
    pure (Left "Computer drag path needs at least two points.")
dragPointer processEnvironment display (first : rest) = do
    moved <- movePoint first
    case moved of
        Left err -> pure (Left err)
        Right () ->
            withX11InputCleanup
                (do
                    pressed <-
                        runXdotool
                            processEnvironment
                            (arguments ["mousedown", "1"])
                    case pressed of
                        Left err -> pure (Left err)
                        Right () -> foldM moveResult (Right ()) rest)
                (runXdotool
                    processEnvironment
                    (arguments ["mouseup", "1"]))
  where
    movePoint ComputerPoint{pointX, pointY} = do
        threadDelay 20000
        movePointer processEnvironment display pointX pointY
    moveResult (Left err) _ = pure (Left err)
    moveResult (Right ()) point = movePoint point

withX11Modifiers
    :: [(String, String)]
    -> [Modifier]
    -> IO (Either Text ())
    -> IO (Either Text ())
withX11Modifiers _ [] action = action
withX11Modifiers processEnvironment modifiers action =
    withX11InputCleanup
        (do
            pressed <- runXdotoolInvocations processEnvironment
                [ arguments ["keydown", Text.unpack (modifierName modifier)]
                | modifier <- modifiers
                ]
            case pressed of
                Left err -> pure (Left err)
                Right () -> action)
        cleanup
  where
    cleanup =
        runXdotoolCleanupInvocations processEnvironment
            [ arguments ["keyup", Text.unpack (modifierName modifier)]
            | modifier <- reverse modifiers
            ]

withX11InputCleanup
    :: IO (Either Text ())
    -> IO (Either Text ())
    -> IO (Either Text ())
withX11InputCleanup action cleanup = do
    (actionResult, cleanupResult) <-
        generalBracket
            (pure ())
            (\() _ -> cleanup)
            (\() -> action)
    pure case (actionResult, cleanupResult) of
        (Right (), result) -> result
        (Left err, Right ()) -> Left err
        (Left err, Left cleanupErr) ->
            Left
                ( err
                    <> " Input cleanup also failed: "
                    <> cleanupErr
                )

withX11Readiness
    :: IO (Either Text ())
    -> IO (Either Text ())
    -> IO (Either Text ())
withX11Readiness = withLogindReadiness

movePointer
    :: [(String, String)]
    -> ComputerDisplay
    -> Int
    -> Int
    -> IO (Either Text ())
movePointer processEnvironment display x y =
    let (rootX, rootY) = x11PointerPosition display x y
    in runXdotool processEnvironment
        (arguments ["mousemove", "--sync", show rootX, show rootY])

runXdotoolInvocations
    :: [(String, String)]
    -> [XdotoolInvocation]
    -> IO (Either Text ())
runXdotoolInvocations processEnvironment =
    foldM step (Right ())
  where
    step (Left err) _ = pure (Left err)
    step (Right ()) invocation =
        runXdotool processEnvironment invocation

runX11TypeInvocationsWith
    :: IO (Either Text ())
    -> (XdotoolInvocation -> IO (Either Text ()))
    -> [XdotoolInvocation]
    -> IO (Either Text ())
runX11TypeInvocationsWith readiness run =
    foldM step (Right ())
  where
    step (Left err) _ = pure (Left err)
    step (Right ()) invocation =
        readiness >>= \case
            Left err -> pure (Left err)
            Right () -> run invocation

runXdotoolCleanupInvocations
    :: [(String, String)]
    -> [XdotoolInvocation]
    -> IO (Either Text ())
runXdotoolCleanupInvocations processEnvironment =
    foldM step (Right ())
  where
    step previous invocation = do
        current <- runXdotool processEnvironment invocation
        pure case (previous, current) of
            (Right (), result) -> result
            (result, Right ()) -> result
            (Left first, Left next) ->
                Left
                    ( first
                        <> " Additional input cleanup failure: "
                        <> next
                    )

runXdotool
    :: [(String, String)]
    -> XdotoolInvocation
    -> IO (Either Text ())
runXdotool processEnvironment invocation = do
    attempted <- tryAny $
        readCreateProcessWithExitCode
            (processWithEnvironment
                processEnvironment
                "xdotool"
                invocation.xdotoolArguments)
            (Text.unpack invocation.xdotoolStdin)
    pure case attempted of
        Left exception ->
            Left ("Unable to run xdotool: " <> Text.pack (show exception))
        Right (ExitSuccess, _, _) -> Right ()
        Right (ExitFailure _, _, stderr) ->
            Left (processError "xdotool" stderr)

captureX11Display
    :: [(String, String)]
    -> IO (Either Text ())
    -> ScreenshotEncoding
    -> IO (Either Text CapturedDisplay)
captureX11Display processEnvironment readiness encoding =
    inspectX11Display processEnvironment >>= \case
        Left err -> pure (Left err)
        Right display ->
            readiness >>= \case
                Left err -> pure (Left err)
                Right () -> capture display
  where
    capture display = do
        temporaryDirectory <- getTemporaryDirectory
        attempted <- tryAny do
            (path, handle) <- openBinaryTempFile temporaryDirectory
                "agent-computer-use-x11.png"
            hClose handle
            flip finally (removeFile path) do
                let geometry = x11CaptureGeometry display
                readCreateProcessWithExitCode
                    (processWithEnvironment
                        processEnvironment
                        "maim"
                        ["--geometry", geometry, path])
                    ""
                    >>= \case
                        (ExitFailure _, _, stderr) ->
                            pure (Left (processError "maim" stderr))
                        (ExitSuccess, _, _) -> do
                            bytes <- BS.readFile path
                            case encodeCaptured display encoding bytes of
                                Left err -> pure (Left err)
                                Right captured ->
                                    verifyCapturedDisplay display captured
        pure (either (Left . Text.pack . show) id attempted)

    verifyCapturedDisplay display captured =
        inspectX11Display processEnvironment >>= \case
            Left err -> pure (Left err)
            Right current
                | current /= display ->
                    pure (Left
                        "The selected display changed during screen capture; take a fresh screenshot before continuing.")
                | otherwise ->
                    readiness >>= \case
                        Left err -> pure (Left err)
                        Right () -> pure (Right captured)

encodeCaptured
    :: ComputerDisplay
    -> ScreenshotEncoding
    -> BS.ByteString
    -> Either Text CapturedDisplay
encodeCaptured display encoding bytes
    | BS.null bytes =
        Left "X11 screen capture returned an empty image."
    | otherwise =
        case decodeImage bytes of
            Left err ->
                Left ("Unable to decode the X11 screenshot: " <> Text.pack err)
            Right dynamicImage ->
                let (frameWidth, frameHeight) = dynamicImageSize dynamicImage
                    currentDisplay = display
                        { computerDisplayFrameWidth = frameWidth
                        , computerDisplayFrameHeight = frameHeight
                        }
                    image = case encoding of
                        ScreenshotPng ->
                            ImageAttachment "image/png" bytes
                        ScreenshotJpeg ->
                            ImageAttachment "image/jpeg"
                                (LBS.toStrict
                                    (encodeJpegAtQuality
                                        (80 :: Word8)
                                        (convertImage
                                            (convertRGB8 dynamicImage))))
                in Right CapturedDisplay
                    { capturedComputerDisplay = currentDisplay
                    , capturedComputerImage = image
                    }

dynamicImageSize :: DynamicImage -> (Int, Int)
dynamicImageSize =
    dynamicMap \image -> (imageWidth image, imageHeight image)

x11CaptureGeometry :: ComputerDisplay -> String
x11CaptureGeometry display =
    show display.computerDisplayWidth
        <> "x" <> show display.computerDisplayHeight
        <> signed display.computerDisplayOriginX
        <> signed display.computerDisplayOriginY
  where
    signed number
        | number >= 0 = '+' : show number
        | otherwise = show number

typeInvocation :: Text -> XdotoolInvocation
typeInvocation value = XdotoolInvocation
    { xdotoolArguments =
        ["type", "--clearmodifiers", "--delay", "1", "--file", "-"]
    , xdotoolStdin = value
    }

x11TypeInvocations :: Text -> [XdotoolInvocation]
x11TypeInvocations =
    map typeInvocation . Text.chunksOf typeChunkSize

arguments :: [String] -> XdotoolInvocation
arguments values = XdotoolInvocation
    { xdotoolArguments = values
    , xdotoolStdin = ""
    }

mouseButton :: MouseButton -> String
mouseButton = \case
    MouseLeft -> "1"
    MouseMiddle -> "2"
    MouseRight -> "3"
    MouseBack -> "8"
    MouseForward -> "9"

modifierName :: Modifier -> Text
modifierName = \case
    ModifierMeta -> "Super_L"
    ModifierControl -> "ctrl"
    ModifierAlt -> "alt"
    ModifierShift -> "shift"
    ModifierFunction -> "XF86Fn"

computerKeyName :: ComputerKey -> Text
computerKeyName = \case
    ComputerNamedKey named -> namedKeyName named
    ComputerTextKey value -> value
    ComputerShortcutKey value -> value

namedKeyName :: NamedKey -> Text
namedKeyName = \case
    KeyEnter -> "Return"
    KeyTab -> "Tab"
    KeySpace -> "space"
    KeyBackspace -> "BackSpace"
    KeyEscape -> "Escape"
    KeyMeta -> "Super_L"
    KeyShift -> "Shift_L"
    KeyCapsLock -> "Caps_Lock"
    KeyAlt -> "Alt_L"
    KeyControl -> "Control_L"
    KeyHome -> "Home"
    KeyPageUp -> "Page_Up"
    KeyDelete -> "Delete"
    KeyEnd -> "End"
    KeyPageDown -> "Page_Down"
    KeyLeft -> "Left"
    KeyRight -> "Right"
    KeyDown -> "Down"
    KeyUp -> "Up"

continue :: Either Text () -> IO (Either Text ()) -> IO (Either Text ())
continue (Left err) _ = pure (Left err)
continue (Right ()) action = action

processError :: Text -> String -> Text
processError command stderr =
    let detail = Text.strip (Text.pack stderr)
    in if Text.null detail
        then command <> " failed."
        else command <> " failed: " <> detail

processWithEnvironment
    :: [(String, String)]
    -> FilePath
    -> [String]
    -> CreateProcess
processWithEnvironment processEnvironment command commandArguments =
    (proc command commandArguments)
        { env = Just processEnvironment
        }

commaInts :: [Int] -> Text
commaInts = Text.intercalate "," . map (Text.pack . show)

typeChunkSize :: Int
typeChunkSize = 64
