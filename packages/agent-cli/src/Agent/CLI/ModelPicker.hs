-- | Interactive TTY model picker for bare @/model@.
module Agent.CLI.ModelPicker
    ( pickModel
    , formatCatalogListing
    , renderPickerFrame
    , decodePickerKey
    ) where

import Agent.CLI.Models
import Agent.CLI.Style
    ( glyphOk
    , roleMuted
    , rolePrompt
    , roleSuccess
    , roleWarn
    )
import Agent.Provider (Provider, providerSlug)
import Control.Exception.Safe (bracket, throwIO, tryIO)
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Console.ANSI (hHideCursor, hShowCursor)
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
    , hIsTerminalDevice
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

-- | Open the picker when stdin is a TTY; otherwise print the catalog.
-- Returns @Just name@ on confirm, @Nothing@ on cancel / EOF / non-TTY.
pickModel :: Bool -> Provider -> Text -> IO (Maybe Text)
pickModel color provider current = do
    isTty <- hIsTerminalDevice stdin
    if not isTty
        then do
            Text.hPutStrLn stderr (formatCatalogListing color provider current)
            hFlush stderr
            pure Nothing
        else runInteractivePicker color provider current

formatCatalogListing :: Bool -> Provider -> Text -> Text
formatCatalogListing color provider current =
    let state = initialPickerState provider current
        header =
            roleMuted color
                (glyphSessionLike
                    <> "model: "
                    <> current
                    <> " · "
                    <> providerSlug provider)
        rows =
            map
                (\opt ->
                    let mark
                            | opt.modelId == current =
                                roleSuccess color (glyphOk <> opt.modelId)
                            | otherwise = roleMuted color ("  " <> opt.modelId)
                        label = case opt.modelLabel of
                            Nothing -> ""
                            Just l -> roleMuted color ("  " <> l)
                    in mark <> label)
                state.pickerAll
    in Text.intercalate "\n" (header : rows)

-- | Pure frame used by the interactive loop (and tests).
renderPickerFrame :: Bool -> PickerState -> Text
renderPickerFrame color state =
    let visible = visibleOptions state
        n = length visible
        idx = if n == 0 then 0 else min state.pickerIndex (n - 1)
        header =
            rolePrompt color "model"
                <> roleMuted color
                    (" · "
                        <> providerSlug state.pickerProvider
                        <> " · current "
                        <> state.pickerCurrent)
        filterLine
            | Text.null state.pickerFilter =
                roleMuted color "filter: (type to narrow)"
            | otherwise =
                roleMuted color "filter: "
                    <> roleWarn color state.pickerFilter
        body = case visible of
            [] -> [roleMuted color "(no matches)"]
            opts ->
                zipWith
                    (\i opt -> renderRow color (i == idx) state.pickerCurrent opt)
                    [0 ..]
                    opts
        footer =
            roleMuted color "↑↓/jk · enter · esc/q · type to filter"
    in Text.intercalate "\n" (header : filterLine : body <> [footer])

renderRow :: Bool -> Bool -> Text -> ModelOption -> Text
renderRow color selected current opt =
    let cursor = if selected then roleWarn color "› " else "  "
        name
            | selected = roleSuccess color opt.modelId
            | otherwise = roleMuted color opt.modelId
        currentMark
            | opt.modelId == current = roleSuccess color " ✓"
            | otherwise = ""
        label = case opt.modelLabel of
            Nothing -> ""
            Just l -> roleMuted color ("  " <> l)
    in cursor <> name <> currentMark <> label

-- | Decode one keypress (including CSI arrow sequences) into a picker event.
decodePickerKey :: String -> Maybe PickerEvent
decodePickerKey = \case
    "\n" -> Just PickerConfirm
    "\r" -> Just PickerConfirm
    "\ESC" -> Just PickerCancel
    "q" -> Just PickerCancel
    "Q" -> Just PickerCancel
    "k" -> Just PickerUp
    "K" -> Just PickerUp
    "j" -> Just PickerDown
    "J" -> Just PickerDown
    "\ESC[A" -> Just PickerUp
    "\ESC[B" -> Just PickerDown
    "\ESC[OA" -> Just PickerUp
    "\ESC[OB" -> Just PickerDown
    "\DEL" -> Just PickerBackspace
    "\b" -> Just PickerBackspace
    [c] -> Just (PickerType c)
    _ -> Nothing

runInteractivePicker :: Bool -> Provider -> Text -> IO (Maybe Text)
runInteractivePicker color provider current = do
    let state0 = initialPickerState provider current
    oldTerm <- getTerminalAttributes stdInput
    oldBuf <- hGetBuffering stdin
    let enter = do
            setTerminalAttributes stdInput (rawAttrs oldTerm) Immediately
            hSetBuffering stdin NoBuffering
            hHideCursor stderr
        restore = do
            hShowCursor stderr
            setTerminalAttributes stdInput oldTerm Immediately
            hSetBuffering stdin oldBuf
    bracket enter (const restore) \() -> do
        let frame0 = renderPickerFrame color state0
            lines0 = frameLineCount frame0
        drawFrame stderr frame0
        loop color stderr state0 lines0

loop :: Bool -> Handle -> PickerState -> Int -> IO (Maybe Text)
loop color h state drawnLines = do
    mkey <- readPickerKey
    case mkey of
        Nothing -> do
            clearDrawn h drawnLines
            pure Nothing
        Just key -> case decodePickerKey key of
            Nothing -> loop color h state drawnLines
            Just event -> case applyPickerEvent event state of
                Left result -> do
                    clearDrawn h drawnLines
                    pure result
                Right state' -> do
                    let frame = renderPickerFrame color state'
                        n = frameLineCount frame
                    redraw h drawnLines frame
                    loop color h state' n

frameLineCount :: Text -> Int
frameLineCount frame =
    length (Text.splitOn "\n" frame)

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
    -- Clear any leftover lines if the new frame is shorter.
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

readPickerKey :: IO (Maybe String)
readPickerKey = do
    result <- tryIO (hGetChar stdin)
    case result of
        Left err
            | isEOFError err -> pure Nothing
            | otherwise -> throwIO err
        Right '\ESC' -> do
            -- Distinguish bare Esc from CSI / SS3 arrow sequences.
            more <- hWaitForInput stdin 50
            if not more
                then pure (Just "\ESC")
                else do
                    c2 <- hGetChar stdin
                    case c2 of
                        '[' -> do
                            c3 <- hGetChar stdin
                            pure (Just ['\ESC', '[', c3])
                        'O' -> do
                            c3 <- hGetChar stdin
                            pure (Just ['\ESC', 'O', c3])
                        _ -> pure (Just ['\ESC', c2])
        Right c -> pure (Just [c])

rawAttrs :: TerminalAttributes -> TerminalAttributes
rawAttrs oldTerm =
    flip withMinInput 1
        . flip withTime 0
        . flip withoutMode EnableEcho
        . flip withoutMode ProcessInput
        . flip withMode KeyboardInterrupts
        $ oldTerm

-- Matches Style.glyphSession without pulling unicode env probing here.
glyphSessionLike :: Text
glyphSessionLike = "⧉ "
