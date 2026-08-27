-- | Interactive approval and multiple-choice input.
module Agent.CLI.Input.Approval
    ( readApprovalLine
    , readChoiceSelection
    ) where

import Agent.CLI.Input.Picker
    ( approvalKeyText
    , choiceMoveIndex
    , parseChoiceKey
    )
import Agent.CLI.Input.Types (ChoiceKey(..))
import Control.Exception.Safe
    ( bracket
    , throwIO
    , tryIO
    )
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Console.ANSI (hHideCursor, hShowCursor)
import System.Console.ANSI.Codes (clearLineCode, cursorUpCode)
import System.IO
    ( BufferMode(..)
    , Handle
    , hFlush
    , hGetBuffering
    , hGetChar
    , hIsTerminalDevice
    , hSetBuffering
    , hWaitForInput
    , isEOF
    , stderr
    , stdin
    )
import System.IO.Error (isEOFError)
import System.Posix.IO (stdInput)
import System.Posix.Terminal
    ( TerminalMode(..)
    , TerminalState(..)
    , getTerminalAttributes
    , setTerminalAttributes
    , withMinInput
    , withMode
    , withTime
    , withoutMode
    )

-- | Read a one-shot approval answer. TTY input submits on one keypress.
readApprovalLine :: Text -> IO (Maybe Text)
readApprovalLine prompt = do
    Text.hPutStr stderr prompt
    hFlush stderr
    isTty <- hIsTerminalDevice stdin
    if isTty
        then readApprovalKey
        else readAnswerOnly

-- | Interactive multiple-choice picker on a TTY.
readChoiceSelection
    :: (Bool -> Text -> Text)
    -> [Text]
    -> IO (Maybe Text)
readChoiceSelection formatLine options = do
    isTty <- hIsTerminalDevice stdin
    case options of
        [] -> pure Nothing
        _
            | not isTty -> pure Nothing
            | otherwise -> withChoiceRawStdin $
                bracket
                    (hHideCursor stderr)
                    (\_ -> hShowCursor stderr)
                    \() -> do
                        let len = length options
                            menuLines = len + 1
                        drawMenu formatLine options 0
                        pickLoop formatLine options len menuLines 0

pickLoop
    :: (Bool -> Text -> Text)
    -> [Text]
    -> Int
    -> Int
    -> Int
    -> IO (Maybe Text)
pickLoop formatLine options len menuLines idx = do
    mkey <- readChoiceKey
    case mkey of
        Nothing -> pure Nothing
        Just ChoiceCancel -> pure Nothing
        Just ChoiceEnter -> do
            redrawMenu formatLine options menuLines idx
            pure (Just (options !! idx))
        Just (ChoiceDigit n)
            | n >= 1 && n <= len -> do
                let idx' = n - 1
                redrawMenu formatLine options menuLines idx'
                pure (Just (options !! idx'))
            | otherwise ->
                pickLoop formatLine options len menuLines idx
        Just key -> do
            let idx' = choiceMoveIndex len idx key
            when (idx' /= idx) $
                redrawMenu formatLine options menuLines idx'
            pickLoop formatLine options len menuLines idx'

redrawMenu
    :: (Bool -> Text -> Text)
    -> [Text]
    -> Int
    -> Int
    -> IO ()
redrawMenu formatLine options menuLines idx = do
    Text.hPutStr stderr (Text.pack (cursorUpCode menuLines))
    drawMenu formatLine options idx

drawMenu
    :: (Bool -> Text -> Text)
    -> [Text]
    -> Int
    -> IO ()
drawMenu formatLine options idx = do
    mapM_
        (\(i, opt) -> do
            let selected = i == idx
                marker = if selected then "> " else "  "
                line = formatLine selected (marker <> opt)
            putChoiceLine stderr line)
        (zip [0 ..] options)
    putChoiceLine stderr (formatLine False "  ↑/↓ move · Enter select · Esc cancel")

putChoiceLine :: Handle -> Text -> IO ()
putChoiceLine handle line = do
    Text.hPutStr handle (Text.pack clearLineCode)
    Text.hPutStrLn handle line
    hFlush handle

readApprovalKey :: IO (Maybe Text)
readApprovalKey =
    withRawStdin do
        result <- tryIO (hGetChar stdin)
        case result of
            Left err
                | isEOFError err -> pure Nothing
                | otherwise -> throwIO err
            Right c -> do
                let answer = approvalKeyText c
                Text.hPutStrLn stderr answer
                pure (Just answer)

readChoiceKey :: IO (Maybe ChoiceKey)
readChoiceKey = do
    result <- tryIO (hGetChar stdin)
    case result of
        Left err
            | isEOFError err -> pure Nothing
            | otherwise -> throwIO err
        Right '\ESC' -> do
            ready <- hWaitForInput stdin 50
            if not ready
                then pure (Just ChoiceCancel)
                else do
                    c2 <- hGetChar stdin
                    case c2 of
                        '[' -> do
                            c3 <- hGetChar stdin
                            case parseChoiceKey ['\ESC', '[', c3] of
                                Just key -> pure (Just key)
                                Nothing -> drainCsiTail c3 >> readChoiceKey
                        'O' -> do
                            c3 <- hGetChar stdin
                            case parseChoiceKey ['\ESC', 'O', c3] of
                                Just key -> pure (Just key)
                                Nothing -> readChoiceKey
                        _ ->
                            case parseChoiceKey ['\ESC', c2] of
                                Just key -> pure (Just key)
                                Nothing -> readChoiceKey
        Right c ->
            case parseChoiceKey [c] of
                Just key -> pure (Just key)
                Nothing -> readChoiceKey

drainCsiTail :: Char -> IO ()
drainCsiTail c
    | c >= '@' && c <= '~' = pure ()
    | otherwise = go
  where
    go = do
        ready <- hWaitForInput stdin 50
        when ready do
            c' <- hGetChar stdin
            if c' >= '@' && c' <= '~'
                then pure ()
                else go

withRawStdin :: IO a -> IO a
withRawStdin action = do
    oldTerm <- getTerminalAttributes stdInput
    oldBuf <- hGetBuffering stdin
    let enter = do
            let raw =
                    flip withMinInput 1
                        . flip withTime 0
                        . flip withoutMode EnableEcho
                        $ oldTerm
            setTerminalAttributes stdInput raw Immediately
            hSetBuffering stdin NoBuffering
        restore = do
            setTerminalAttributes stdInput oldTerm Immediately
            hSetBuffering stdin oldBuf
    bracket enter (const restore) \() -> action

withChoiceRawStdin :: IO a -> IO a
withChoiceRawStdin action = do
    oldTerm <- getTerminalAttributes stdInput
    oldBuf <- hGetBuffering stdin
    let enter = do
            let raw =
                    flip withMinInput 1
                        . flip withTime 0
                        . flip withoutMode EnableEcho
                        . flip withoutMode ProcessInput
                        . flip withMode KeyboardInterrupts
                        $ oldTerm
            setTerminalAttributes stdInput raw Immediately
            hSetBuffering stdin NoBuffering
        restore = do
            setTerminalAttributes stdInput oldTerm Immediately
            hSetBuffering stdin oldBuf
    bracket enter (const restore) \() -> action

readAnswerOnly :: IO (Maybe Text)
readAnswerOnly = fmap (fmap Text.strip) readRawLine

readRawLine :: IO (Maybe Text)
readRawLine = do
    done <- isEOF
    if done
        then pure Nothing
        else Just <$> Text.getLine
