-- | Shared raw-TTY overlay loop used by the model picker, permission card,
-- and session resume list. Overlays accept keyboard navigation, mouse-wheel
-- movement, and left-click selection through xterm SGR mouse reporting.
module Agent.CLI.Picker
    ( PickerKey(..)
    , MouseEvent(..)
    , decodePickerKey
    , decodeMouseEvent
    , mouseKeysForFrame
    , runOverlay
    , runOverlayWithUpdates
    , withRawTty
    ) where

import Agent.CLI.Terminal
    ( TerminalCapabilities(..)
    , detectTerminalCapabilities
    , emitTerminalSequence
    , kittyKeyboardPop
    , kittyKeyboardPush
    , withSynchronizedOutput
    )
import Control.Concurrent.Async (race)
import Control.Exception.Safe (bracket, throwIO, tryIO)
import Control.Monad (when)
import Data.Char (isDigit)
import Data.List (elemIndex, findIndex)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Console.ANSI (hGetCursorPosition, hHideCursor, hShowCursor)
import System.Console.ANSI.Codes
    ( clearLineCode
    , cursorUpCode
    )
import System.IO
    ( BufferMode(..)
    , Handle
    , hFlush
    , hGetBuffering
    , hGetChar
    , hPutStr
    , hSetBuffering
    , hWaitForInput
    , stderr
    , stdin
    )
import System.IO.Error (isEOFError)
import System.Posix.IO (stdInput)
import System.Posix.Terminal
    ( TerminalAttributes
    , TerminalMode(..)
    , TerminalState(..)
    , getTerminalAttributes
    , setTerminalAttributes
    , withMinInput
    , withMode
    , withTime
    , withoutMode
    )

data PickerKey
    = PickerKeyUp
    | PickerKeyDown
    | PickerKeyConfirm
    | PickerKeyCancel
    | PickerKeyBackspace
    | PickerKeyChar Char
    deriving (Eq, Show)

data MouseEvent
    = MouseLeftPress Int Int
    | MouseLeftRelease Int Int
    | MouseWheelUp Int Int
    | MouseWheelDown Int Int
    deriving (Eq, Show)

decodePickerKey :: String -> Maybe PickerKey
decodePickerKey = \case
    sequence_
        | kittyEvent sequence_ == Just kittyRelease -> Nothing
    "\n" -> Just PickerKeyConfirm
    "\r" -> Just PickerKeyConfirm
    "\ESC" -> Just PickerKeyCancel
    "q" -> Just PickerKeyCancel
    "Q" -> Just PickerKeyCancel
    "k" -> Just PickerKeyUp
    "K" -> Just PickerKeyUp
    "j" -> Just PickerKeyDown
    "J" -> Just PickerKeyDown
    "\ESC[A" -> Just PickerKeyUp
    "\ESC[B" -> Just PickerKeyDown
    "\ESC[OA" -> Just PickerKeyUp
    "\ESC[OB" -> Just PickerKeyDown
    "\DEL" -> Just PickerKeyBackspace
    "\b" -> Just PickerKeyBackspace
    sequence_
        | Just codepoint <- kittyCodepoint sequence_ ->
            codepointKey codepoint
    sequence_
        | "\ESC[" `Text.isPrefixOf` Text.pack sequence_
        , Just final <- safeLast sequence_ ->
            case final of
                'A' -> Just PickerKeyUp
                'B' -> Just PickerKeyDown
                _ -> Nothing
    [c] -> Just (PickerKeyChar c)
    _ -> Nothing

-- | Decode xterm's SGR extended mouse protocol.
decodeMouseEvent :: String -> Maybe MouseEvent
decodeMouseEvent raw = do
    payload <- stripPrefix "\ESC[<" raw
    (buttonText, rest1) <- splitOnce ';' payload
    (columnText, rowAndFinal) <- splitOnce ';' rest1
    (rowText, final) <- unsnoc rowAndFinal
    button <- readDecimal buttonText
    column <- readDecimal columnText
    row <- readDecimal rowText
    case (button, final) of
        (0, 'M') -> Just (MouseLeftPress column row)
        (0, 'm') -> Just (MouseLeftRelease column row)
        (64, 'M') -> Just (MouseWheelUp column row)
        (65, 'M') -> Just (MouseWheelDown column row)
        _ -> Nothing

mouseKeysForFrame :: Maybe Int -> Text -> MouseEvent -> [PickerKey]
mouseKeysForFrame frameTop frame = \case
    MouseWheelUp _ _ -> [PickerKeyUp]
    MouseWheelDown _ _ -> [PickerKeyDown]
    MouseLeftRelease _ _ -> []
    MouseLeftPress _ row -> case frameTop of
        Nothing -> []
        Just top -> case selectableRows frame of
            Nothing -> []
            Just (selected, rows)
                | Just selectedIndex <- elemIndex selected rows
                , Just clickedIndex <- elemIndex clicked rows ->
                    let delta = clickedIndex - selectedIndex
                    in
                    replicate (abs delta)
                        (if delta < 0 then PickerKeyUp else PickerKeyDown)
                        <> [PickerKeyConfirm]
                | otherwise -> []
              where
                clicked = row - 1 - top

kittyCodepoint :: String -> Maybe Int
kittyCodepoint sequence_ = do
    body <- Text.stripPrefix "\ESC[" (Text.pack sequence_)
    raw <- Text.stripSuffix "u" body
    let code = Text.takeWhile (/= ';') raw
    case reads (Text.unpack code) of
        [(n, "")] -> Just n
        _ -> Nothing

-- | Kitty's keyboard protocol appends @:event@ to the modifier field when
-- event reporting is enabled. Releases must not be interpreted as a second
-- press; repeats remain navigable.
kittyEvent :: String -> Maybe Int
kittyEvent sequence_ = do
    body <- stripPrefix "\ESC[" sequence_
    (parameters, _) <- unsnoc body
    modifierField <- listAt 1 (splitOnceOrSingleton ';' parameters)
    eventField <- listAt 1 (splitOnceOrSingleton ':' modifierField)
    readDecimal eventField

kittyRelease :: Int
kittyRelease = 3

codepointKey :: Int -> Maybe PickerKey
codepointKey = \case
    13 -> Just PickerKeyConfirm
    27 -> Just PickerKeyCancel
    8 -> Just PickerKeyBackspace
    127 -> Just PickerKeyBackspace
    n
        | n >= 0
        , n <= fromEnum (maxBound :: Char) ->
            Just (PickerKeyChar (toEnum n))
    _ -> Nothing

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just (last xs)

-- | Draw @render@, then feed keys through @step@ until it returns @Left@.
runOverlay :: (state -> Text) -> (PickerKey -> state -> Either result state) -> state -> IO (Maybe result)
runOverlay render step state =
    fmap (fmap fst) (runOverlayInternal render step Nothing state)

-- | Like 'runOverlay', but also redraw when an external update arrives.
runOverlayWithUpdates
    :: (state -> Text)
    -> (PickerKey -> state -> Either result state)
    -> IO update
    -> (update -> state -> state)
    -> state
    -> IO (Maybe (result, state))
runOverlayWithUpdates render step awaitUpdate applyUpdate =
    runOverlayInternal render step (Just (awaitUpdate, applyUpdate))

runOverlayInternal
    :: (state -> Text)
    -> (PickerKey -> state -> Either result state)
    -> Maybe (IO update, update -> state -> state)
    -> state
    -> IO (Maybe (result, state))
runOverlayInternal render step updates state0 =
    withRawTty do
        terminal <- detectTerminalCapabilities stderr
        let frame0 = render state0
            lines0 = frameLineCount frame0
        withSynchronizedOutput terminal stderr (drawFrame stderr frame0)
        top0 <- frameTop stderr lines0
        loop terminal stderr state0 lines0 frame0 top0
  where
    loop terminal h state drawnLines frame top = do
        event <- case updates of
            Nothing -> Left <$> readPickerInput
            Just (awaitUpdate, _) -> race readPickerInput awaitUpdate
        case event of
            Left Nothing -> do
                withSynchronizedOutput terminal h (clearDrawn h drawnLines)
                pure Nothing
            Left (Just raw) ->
                let keys = case decodeMouseEvent raw of
                        Just mouse -> mouseKeysForFrame top frame mouse
                        Nothing -> maybe [] pure (decodePickerKey raw)
                in applyKeys terminal h state drawnLines frame top keys
            Right update -> case updates of
                Nothing -> loop terminal h state drawnLines frame top
                Just (_, applyUpdate) -> do
                    let state' = applyUpdate update state
                        frame' = render state'
                        n = frameLineCount frame'
                    withSynchronizedOutput terminal h
                        (redraw h drawnLines frame')
                    top' <- frameTop h n
                    loop terminal h state' n frame' top'

    applyKeys terminal h state drawnLines frame top = \case
        [] -> loop terminal h state drawnLines frame top
        key : keys -> case step key state of
            Left result -> do
                withSynchronizedOutput terminal h (clearDrawn h drawnLines)
                pure (Just (result, state))
            Right state'
                | null keys -> do
                    let frame' = render state'
                        n = frameLineCount frame'
                    withSynchronizedOutput terminal h
                        (redraw h drawnLines frame')
                    top' <- frameTop h n
                    loop terminal h state' n frame' top'
                | otherwise ->
                    applyKeys terminal h state' drawnLines frame top keys

withRawTty :: IO a -> IO a
withRawTty action = do
    oldTerm <- getTerminalAttributes stdInput
    oldBuf <- hGetBuffering stdin
    terminal <- detectTerminalCapabilities stderr
    let enter = do
            setTerminalAttributes stdInput (rawAttrs oldTerm) Immediately
            hSetBuffering stdin NoBuffering
            hHideCursor stderr
            when terminal.terminalKittyKeyboard $
                emitTerminalSequence terminal stderr kittyKeyboardPush
            hPutStr stderr enableMouse
            hFlush stderr
        restore = do
            hPutStr stderr disableMouse
            when terminal.terminalKittyKeyboard $
                emitTerminalSequence terminal stderr kittyKeyboardPop
            hShowCursor stderr
            hFlush stderr
            setTerminalAttributes stdInput oldTerm Immediately
            hSetBuffering stdin oldBuf
    bracket enter (const restore) \() -> action

frameLineCount :: Text -> Int
frameLineCount frame = length (Text.splitOn "\n" frame)

drawFrame :: Handle -> Text -> IO ()
drawFrame h frame = do
    Text.hPutStr h frame
    Text.hPutStr h "\n"
    hFlush h

redraw :: Handle -> Int -> Text -> IO ()
redraw h drawnLines frame = do
    when (drawnLines > 0) do
        hPutStr h (cursorUpCode drawnLines)
    mapM_ (redrawLine h) (Text.splitOn "\n" frame)
    let newLines = frameLineCount frame
    when (drawnLines > newLines) do
        mapM_ (\_ -> do
            hPutStr h clearLineCode
            Text.hPutStr h "\n")
            [1 .. drawnLines - newLines]
        hPutStr h (cursorUpCode (drawnLines - newLines))
    hFlush h

redrawLine :: Handle -> Text -> IO ()
redrawLine h line = do
    hPutStr h clearLineCode
    Text.hPutStr h line
    Text.hPutStr h "\n"

clearDrawn :: Handle -> Int -> IO ()
clearDrawn h drawnLines = when (drawnLines > 0) do
    hPutStr h (cursorUpCode drawnLines)
    mapM_ (\_ -> do
        hPutStr h clearLineCode
        Text.hPutStr h "\n")
        [1 .. drawnLines]
    hPutStr h (cursorUpCode drawnLines)
    hFlush h

readPickerInput :: IO (Maybe String)
readPickerInput = do
    result <- tryIO (hGetChar stdin)
    case result of
        Left err
            | isEOFError err -> pure Nothing
            | otherwise -> throwIO err
        Right '\ESC' -> do
            more <- hWaitForInput stdin 50
            if not more
                then pure (Just "\ESC")
                else do
                    c2 <- hGetChar stdin
                    case c2 of
                        '[' -> Just . ("\ESC[" <>) <$> readCsiTail
                        'O' -> do
                            c3 <- hGetChar stdin
                            pure (Just ['\ESC', 'O', c3])
                        _ -> pure (Just ['\ESC', c2])
        Right c -> pure (Just [c])

readCsiTail :: IO String
readCsiTail = go []
  where
    go reversed = do
        char <- hGetChar stdin
        let accumulated = char : reversed
        if char >= '@' && char <= '~'
            then pure (reverse accumulated)
            else go accumulated

rawAttrs :: TerminalAttributes -> TerminalAttributes
rawAttrs oldTerm =
    flip withMinInput 1
        . flip withTime 0
        . flip withoutMode EnableEcho
        . flip withoutMode ProcessInput
        . flip withMode KeyboardInterrupts
        $ oldTerm

frameTop :: Handle -> Int -> IO (Maybe Int)
frameTop handle lineCount =
    fmap (\(row, _) -> row - lineCount) <$> hGetCursorPosition handle

selectableRows :: Text -> Maybe (Int, [Int])
selectableRows frame = do
    selected <- findIndex (Text.isPrefixOf "› " . stripAnsi) rows
    let selectable =
            map fst
                . filter (isSelectable . snd)
                $ zip [0 ..] rows
    pure (selected, selectable)
  where
    rows = Text.splitOn "\n" frame
    isSelectable line =
        case Text.unpack (stripAnsi line) of
            '›' : ' ' : _ -> True
            ' ' : ' ' : c : _ -> c /= ' '
            _ -> False

stripAnsi :: Text -> Text
stripAnsi = Text.pack . goNormal . Text.unpack
  where
    goNormal = \case
        [] -> []
        '\ESC' : '[' : rest -> goCsi rest
        char : rest -> char : goNormal rest
    goCsi = \case
        [] -> []
        char : rest
            | char >= '@' && char <= '~' -> goNormal rest
            | otherwise -> goCsi rest

stripPrefix :: String -> String -> Maybe String
stripPrefix prefix value
    | Text.pack prefix `Text.isPrefixOf` Text.pack value =
        Just (drop (length prefix) value)
    | otherwise = Nothing

splitOnce :: Char -> String -> Maybe (String, String)
splitOnce delimiter value =
    case break (== delimiter) value of
        (before, _ : after) -> Just (before, after)
        _ -> Nothing

splitOnceOrSingleton :: Char -> String -> [String]
splitOnceOrSingleton delimiter value =
    case break (== delimiter) value of
        (before, _ : after) -> [before, after]
        _ -> [value]

listAt :: Int -> [a] -> Maybe a
listAt index values
    | index < 0 = Nothing
    | otherwise = case drop index values of
        value : _ -> Just value
        [] -> Nothing

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc xs = Just (init xs, last xs)

readDecimal :: String -> Maybe Int
readDecimal chars
    | null chars || not (all isDigit chars) = Nothing
    | otherwise =
        Just (foldl (\n c -> n * 10 + fromEnum c - fromEnum '0') 0 chars)

enableMouse, disableMouse :: String
enableMouse = "\ESC[?1000h\ESC[?1006h"
disableMouse = "\ESC[?1006l\ESC[?1000l"
