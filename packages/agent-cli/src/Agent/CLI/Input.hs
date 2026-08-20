-- | Line editing for interactive prompts. TTY sessions use haskeline so
-- arrow keys move the cursor / recall history instead of echoing escape
-- sequences; non-TTY falls back to plain 'getLine'.
module Agent.CLI.Input
    ( ReplLine(..)
    , readReplLine
    , readApprovalLine
    , readChoiceSelection
    , approvalKeyText
    , ChoiceKey(..)
    , parseChoiceKey
    , choiceMoveIndex
    , classifyPastedText
    , formatPasteChip
    , replHistoryPath
    ) where

import Agent.CLI.Command (slashCompletionCandidates)
import Agent.CLI.Interrupt
    ( IdleCtrlCResult(..)
    , InterruptState
    , noteIdleCtrlC
    )
import Control.Exception.Safe (bracket, catchIO, throwIO, tryIO)
import Control.Monad (when)
import Data.Char (ord)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Console.ANSI
    ( hHideCursor
    , hShowCursor
    )
import System.Console.ANSI.Codes
    ( clearLineCode
    , cursorUpCode
    )
import System.Console.Haskeline
    ( Settings(..)
    , defaultSettings
    , getInputLine
    , handleInterrupt
    , runInputT
    , setComplete
    , withInterrupt
    )
import System.Console.Haskeline.Completion
    ( Completion(..)
    , CompletionFunc
    , completeWordWithPrev
    )
import System.Directory
    ( createDirectoryIfMissing
    , getHomeDirectory
    )
import System.FilePath (takeDirectory, (</>))
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
    , stdout
    )
import System.IO.Error (isEOFError)
import System.Posix.Files (setFileMode)
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
import System.Posix.Types (FileMode)

-- | Outcome of an interactive REPL read.
data ReplLine
    = ReplEof
    | ReplText Text
    -- | Submitted text that arrived as a paste (bracketed paste, or a
    -- multi-line / burst heuristic). @replText@ is the payload with CSI
    -- paste wrappers stripped.
    | ReplPasted Text
    | ReplQuitInterrupt
    deriving (Eq, Show)

-- | Strip terminal bracketed-paste wrappers (@CSI 200~@ / @CSI 201~@)
-- and decide whether the buffer looks pasted rather than typed.
classifyPastedText :: Text -> (Text, Bool)
classifyPastedText raw =
    let stripped = stripBracketedPaste raw
        looksPasted =
            raw /= stripped
                || Text.count "\n" stripped >= 3
    in (stripped, looksPasted)

-- | Compact prompt chip for a multi-line paste. Single-line pastes stay inline.
formatPasteChip :: Text -> Text
formatPasteChip text =
    let n = max 1 (length (Text.lines text))
    in if n < 4
        then text
        else "[Pasted: " <> Text.pack (show n) <> " lines]"

stripBracketedPaste :: Text -> Text
stripBracketedPaste text =
    Text.replace pasteEnd "" (Text.replace pasteStart "" text)
  where
    pasteStart = "\ESC[200~"
    pasteEnd = "\ESC[201~"

-- | @~/.haskell-agent/history@ given the user's home directory.
replHistoryPath :: FilePath -> FilePath
replHistoryPath home = home </> ".haskell-agent" </> "history"

-- | Keys understood by the multiple-choice TTY picker.
data ChoiceKey
    = ChoiceUp
    | ChoiceDown
    | ChoiceEnter
    | ChoiceCancel
    | ChoiceDigit Int
    deriving (Eq, Show)

-- | Parse a short key / CSI sequence into a 'ChoiceKey'.
-- Used by the picker and unit tests; 'Nothing' means ignore and keep reading.
parseChoiceKey :: String -> Maybe ChoiceKey
parseChoiceKey = \case
    "\n" -> Just ChoiceEnter
    "\r" -> Just ChoiceEnter
    "\ESC" -> Just ChoiceCancel
    "\ESC[A" -> Just ChoiceUp
    "\ESC[B" -> Just ChoiceDown
    "\ESCOA" -> Just ChoiceUp
    "\ESCOB" -> Just ChoiceDown
    "k" -> Just ChoiceUp
    "j" -> Just ChoiceDown
    "q" -> Just ChoiceCancel
    "Q" -> Just ChoiceCancel
    [c]
        | c >= '1' && c <= '9' ->
            Just (ChoiceDigit (ord c - ord '0'))
    _ -> Nothing

-- | Wrap highlight index for ↑ / ↓ (and j / k). Other keys leave @idx@ alone.
choiceMoveIndex :: Int -> Int -> ChoiceKey -> Int
choiceMoveIndex len idx key
    | len <= 0 = 0
    | otherwise = case key of
        ChoiceUp -> if idx <= 0 then len - 1 else idx - 1
        ChoiceDown -> if idx >= len - 1 then 0 else idx + 1
        _ -> idx

-- | Read a REPL prompt line. Persists history under 'replHistoryPath' when
-- stdin is a TTY. 'ReplEof' means EOF; 'ReplQuitInterrupt' means a confirmed
-- double Ctrl-C. The prompt is drawn on stdout.
--
-- Haskeline installs its own SIGINT handler while reading, so idle Ctrl-C
-- goes through 'noteIdleCtrlC' rather than the outer signal handler.
readReplLine :: InterruptState -> Text -> IO ReplLine
readReplLine interrupt prompt = do
    isTty <- hIsTerminalDevice stdin
    if isTty
        then do
            home <- getHomeDirectory
            let path = replHistoryPath home
            ensureHistoryParent path
            classifyLine <$>
                readEditedLine interrupt
                    (setComplete completeSlash
                        defaultSettings
                            { historyFile = Just path
                            , autoAddHistory = True
                            })
                    prompt
        else do
            Text.hPutStr stdout prompt
            hFlush stdout
            fmap (maybe ReplEof (classifySubmitted . Text.strip)) readAnswerOnly
  where
    classifyLine = \case
        ReplText text -> classifySubmitted text
        other -> other
    classifySubmitted text =
        let (stripped, pasted) = classifyPastedText text
        in if pasted then ReplPasted stripped else ReplText stripped
-- | Read a one-shot approval answer. The question is always written to
-- stderr (matching the pre-haskeline behavior) so redirected stdout does
-- not swallow the prompt. Does not touch REPL history.
--
-- On a TTY, a single keypress submits immediately (@y@ / @n@ / @a@, or
-- Enter for the default deny) so the user does not need a trailing Enter.
-- Non-TTY keeps cooked 'getLine' for scripts that pipe a full line.
readApprovalLine :: Text -> IO (Maybe Text)
readApprovalLine prompt = do
    Text.hPutStr stderr prompt
    hFlush stderr
    isTty <- hIsTerminalDevice stdin
    if isTty
        then readApprovalKey
        else readAnswerOnly

-- | Interactive multiple-choice picker on a TTY. Writes the menu to
-- @stderr@. ↑ / ↓ (or j / k) move, Enter selects, digits 1–9 jump, Esc / q
-- cancel. Returns 'Nothing' on cancel, EOF, empty @options@, or non-TTY.
--
-- @formatLine selected label@ styles each row; a muted hint is shown under
-- the list.
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

-- | Map one approval keypress to the text 'parseApprovalAnswer' expects.
-- Enter / Return become empty (deny by default).
approvalKeyText :: Char -> Text
approvalKeyText c
    | c == '\n' || c == '\r' = ""
    | otherwise = Text.singleton c

-- | Single-key approval on a TTY: disable canonical input, read one byte,
-- echo it (except bare Enter), then restore the previous terminal state.
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

-- | Read one picker key, absorbing CSI / SS3 arrow sequences.
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

-- | Finish reading a CSI sequence whose final byte may not have arrived yet.
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

-- | Approval raw mode: turn off echo but keep canonical/'ProcessInput' so
-- Ctrl-C stays SIGINT (same contract as the pre-picker approval path).
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

-- | Choice-picker raw mode: non-canonical so CSI arrows arrive as bytes,
-- no echo, and 'KeyboardInterrupts' so Ctrl-C still raises SIGINT.
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

readEditedLine :: InterruptState -> Settings IO -> Text -> IO ReplLine
readEditedLine interrupt settings prompt =
    withBracketedPaste go
  where
    go =
        -- Haskeline turns Ctrl-C into 'Interrupt' and replaces our SIGINT
        -- handler for the duration of the prompt. Soft-warn / double-quit
        -- are applied here via 'noteIdleCtrlC'.
        handleInterrupt onInterrupt $
            runInputT settings $
                withInterrupt $
                    fmap (maybe ReplEof (ReplText . Text.pack))
                        (getInputLine (Text.unpack prompt))
    onInterrupt = do
        noteIdleCtrlC interrupt >>= \case
            ContinuePrompt -> go
            QuitProcess -> pure ReplQuitInterrupt

-- | Ask the terminal to wrap pastes in CSI 200~ … CSI 201~ so we can
-- distinguish them from typed keystrokes. Restored on exit.
withBracketedPaste :: IO a -> IO a
withBracketedPaste action = do
    isTty <- hIsTerminalDevice stdin
    if not isTty
        then action
        else bracket enable disable (const action)
  where
    enable = do
        Text.hPutStr stdout "\ESC[?2004h"
        hFlush stdout
    disable _ = do
        Text.hPutStr stdout "\ESC[?2004l"
        hFlush stdout

-- | Tab-complete slash command names and a few known argument tokens.
completeSlash :: CompletionFunc IO
completeSlash =
    completeWordWithPrev Nothing " \t" \reversedPrev word ->
        pure $ map toCompletion (slashCompletionCandidates reversedPrev word)
  where
    toCompletion replacement =
        Completion
            { replacement
            , display = replacement
            , isFinished = True
            }

readAnswerOnly :: IO (Maybe Text)
readAnswerOnly = do
    done <- isEOF
    if done
        then pure Nothing
        else Just . Text.strip <$> Text.getLine

ensureHistoryParent :: FilePath -> IO ()
ensureHistoryParent path = do
    let dir = takeDirectory path
    createDirectoryIfMissing True dir
    -- Best-effort private mode; ignore failures on filesystems that refuse.
    trySetMode dir 0o700

trySetMode :: FilePath -> FileMode -> IO ()
trySetMode path mode =
    setFileMode path mode `catchIO` \_ -> pure ()
