-- | Shared raw-TTY overlay loop used by the model picker, permission card,
-- and session resume list.
module Agent.CLI.Picker
    ( PickerKey(..)
    , decodePickerKey
    , runOverlay
    , withRawTty
    ) where

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

decodePickerKey :: String -> Maybe PickerKey
decodePickerKey = \case
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
    [c] -> Just (PickerKeyChar c)
    _ -> Nothing

-- | Draw @render@, then feed keys through @step@ until it returns @Left@.
runOverlay :: (state -> Text) -> (PickerKey -> state -> Either result state) -> state -> IO (Maybe result)
runOverlay render step state0 =
    withRawTty do
        let frame0 = render state0
            lines0 = frameLineCount frame0
        drawFrame stderr frame0
        loop stderr state0 lines0
  where
    loop h state drawnLines = do
        mkey <- readPickerKey
        case mkey of
            Nothing -> do
                clearDrawn h drawnLines
                pure Nothing
            Just raw -> case decodePickerKey raw of
                Nothing -> loop h state drawnLines
                Just key -> case step key state of
                    Left result -> do
                        clearDrawn h drawnLines
                        pure (Just result)
                    Right state' -> do
                        let frame = render state'
                            n = frameLineCount frame
                        redraw h drawnLines frame
                        loop h state' n

withRawTty :: IO a -> IO a
withRawTty action = do
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

readPickerKey :: IO (Maybe String)
readPickerKey = do
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
