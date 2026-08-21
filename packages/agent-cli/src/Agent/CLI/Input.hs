-- | First-party line editing for interactive prompts. TTY sessions use a raw
-- inline editor with live slash-command completion; non-TTY falls back to
-- plain 'getLine'.
module Agent.CLI.Input
    ( ReplLine(..)
    , readReplLine
    , readReplLineWithInitial
    , readApprovalLine
    , readChoiceSelection
    , approvalKeyText
    , ChoiceKey(..)
    , parseChoiceKey
    , choiceMoveIndex
    , classifyPastedText
    , displayEditorText
    , formatPasteChip
    , replHistoryPath
    , terminalTextWidth
    , truncateDisplayText
    , visibleEditorText
    ) where

import Agent.CLI.Command
    ( SlashMenu(..)
    , SlashSuggestion(..)
    , slashMenuFor
    )
import Agent.CLI.Interrupt
    ( IdleCtrlCResult(..)
    , InterruptState
    , noteIdleCtrlC
    )
import Control.Exception.Safe (bracket, catchIO, throwIO, tryIO)
import Control.Monad (unless, when)
import Data.Char
    ( GeneralCategory(..)
    , chr
    , generalCategory
    , isSpace
    , ord
    )
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Console.ANSI
    ( getTerminalSize
    , hHideCursor
    , hShowCursor
    )
import System.Console.ANSI.Codes
    ( clearLineCode
    , cursorUpCode
    )
import System.Console.Haskeline.History
    ( addHistory
    , emptyHistory
    , historyLines
    , readHistory
    , writeHistory
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
    | ReplCycleMode Text
    -- ^ Shift+Tab: cycle idle mode and keep the current draft.
    | ReplQuitInterrupt
    deriving (Eq, Show)

-- | Strip terminal bracketed-paste wrappers (raw @CSI 200~@ / @CSI 201~@,
-- or the printable sentinels used by older history/input versions) and decide
-- whether the buffer looks pasted rather than typed.
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
    Text.filter (not . isPasteSentinel)
        (Text.replace pasteEnd "" (Text.replace pasteStart "" text))
  where
    pasteStart = "\ESC[200~"
    pasteEnd = "\ESC[201~"

pasteStartSentinel, pasteEndSentinel :: Char
pasteStartSentinel = '\x27E6'
pasteEndSentinel = '\x27E7'

isPasteSentinel :: Char -> Bool
isPasteSentinel char =
    char == pasteStartSentinel || char == pasteEndSentinel

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
-- The raw editor reads Ctrl-C as an input byte, so idle Ctrl-C goes through
-- 'noteIdleCtrlC' rather than the outer signal handler.
readReplLine :: InterruptState -> Text -> IO ReplLine
readReplLine interrupt prompt =
    readReplLineWithInitial interrupt prompt ""

-- | Like 'readReplLine', restoring @initial@ as the in-progress draft.
readReplLineWithInitial :: InterruptState -> Text -> Text -> IO ReplLine
readReplLineWithInitial interrupt prompt initial = do
    isTty <- hIsTerminalDevice stdin
    if isTty
        then do
            home <- getHomeDirectory
            let path = replHistoryPath home
            ensureHistoryParent path
            classifyLine <$>
                readInlineEditor interrupt path prompt initial
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

data EditorState = EditorState
    { editorText :: !Text
    , editorCursor :: !Int
    , editorSelected :: !Int
    , editorHistoryIndex :: !(Maybe Int)
    , editorHistoryDraft :: !Text
    , editorKillBuffer :: !Text
    , editorPasted :: !Bool
    , editorSlashDismissed :: !Bool
    }

data DisplayCell = DisplayCell
    { displayCellText :: !Text
    , displayCellWidth :: !Int
    }

terminalTextWidth :: Text -> Int
terminalTextWidth = cellsWidth . displayCells

displayCells :: Text -> [DisplayCell]
displayCells = map displayCell . Text.unpack

displayCell :: Char -> DisplayCell
displayCell char =
    let shown = safeDisplayChar char
    in DisplayCell
        { displayCellText = shown
        , displayCellWidth = textColumns shown
        }

safeDisplayChar :: Char -> Text
safeDisplayChar char
    | char == '\n' || char == '\r' = "↵"
    | char == '\t' = "⇥"
    | code >= 0 && code <= 0x1f =
        Text.singleton (chr (0x2400 + code))
    | code == 0x7f = "␡"
    | code >= 0x80 && code <= 0x9f = "�"
    | generalCategory char == Format = "�"
    | otherwise = Text.singleton char
  where
    code = ord char

textColumns :: Text -> Int
textColumns = sum . map charColumns . Text.unpack

charColumns :: Char -> Int
charColumns char
    | category `elem` [NonSpacingMark, SpacingCombiningMark, EnclosingMark] = 0
    | category `elem` [Control, Surrogate, NotAssigned] = 0
    | isWideCharacter char = 2
    | otherwise = 1
  where
    category = generalCategory char

isWideCharacter :: Char -> Bool
isWideCharacter char =
    let code = ord char
    in code >= 0x1100
        && ( code <= 0x115f
            || code == 0x2329
            || code == 0x232a
            || (code >= 0x2e80 && code <= 0xa4cf && code /= 0x303f)
            || (code >= 0xac00 && code <= 0xd7a3)
            || (code >= 0xf900 && code <= 0xfaff)
            || (code >= 0xfe10 && code <= 0xfe19)
            || (code >= 0xfe30 && code <= 0xfe6f)
            || (code >= 0xff00 && code <= 0xff60)
            || (code >= 0xffe0 && code <= 0xffe6)
            || (code >= 0x1f300 && code <= 0x1faff)
            || (code >= 0x20000 && code <= 0x3fffd)
           )

cellsWidth :: [DisplayCell] -> Int
cellsWidth = sum . map (.displayCellWidth)

renderCells :: [DisplayCell] -> Text
renderCells = Text.concat . map (.displayCellText)

takeColumns :: Int -> [DisplayCell] -> [DisplayCell]
takeColumns width = go 0
  where
    go _ [] = []
    go used (cell:rest)
        | used + cell.displayCellWidth > width = []
        | otherwise = cell : go (used + cell.displayCellWidth) rest

takeSuffixColumns :: Int -> [DisplayCell] -> [DisplayCell]
takeSuffixColumns width = reverse . takeColumns width . reverse

data EditorKey
    = EditorChar !Char
    | EditorEnter
    | EditorBackspace
    | EditorDelete
    | EditorLeft
    | EditorRight
    | EditorHome
    | EditorEnd
    | EditorUp
    | EditorDown
    | EditorTab
    | EditorEscape
    | EditorInterrupt
    | EditorEof
    | EditorKillStart
    | EditorKillEnd
    | EditorKillWord
    | EditorYank
    | EditorClearScreen
    | EditorCycleMode
    | EditorPaste !Text
    | EditorIgnore
    deriving (Eq, Show)

-- | First-party inline editor for the interactive TTY path. It owns the
-- prompt redraw so slash suggestions can update after every keystroke.
readInlineEditor
    :: InterruptState
    -> FilePath
    -> Text
    -> Text
    -> IO ReplLine
readInlineEditor interrupt historyPath prompt initial = do
    withBracketedPaste $
        withEditorRawStdin $
            bracket
                (hHideCursor stdout)
                (\_ -> hShowCursor stdout >> hFlush stdout)
                \() -> do
                    history <- readHistory historyPath `catchIO` \_ -> pure emptyHistory
                    let entries = map Text.pack (historyLines history)
                        state = EditorState
                            { editorText = initial
                            , editorCursor = Text.length initial
                            , editorSelected = 0
                            , editorHistoryIndex = Nothing
                            , editorHistoryDraft = initial
                            , editorKillBuffer = ""
                            , editorPasted = False
                            , editorSlashDismissed = False
                            }
                    redrawEditor prompt state
                    editorLoop history entries state
  where
    editorLoop history entries state = do
        key <- readEditorKey
        let menu = currentMenu state
        case key of
            EditorEnter ->
                case selectedSuggestion state menu of
                    Just suggestion
                        | not (exactCommandSelected state suggestion) -> do
                            let accepted = acceptSuggestion state menu suggestion
                            if suggestion.slashSuggestionTakesArguments
                                then redrawEditor prompt accepted
                                    >> editorLoop history entries accepted
                                else finish history accepted
                    _ -> finish history state
            EditorCycleMode -> do
                finishEditorLine prompt state
                pure (ReplCycleMode state.editorText)
            EditorInterrupt ->
                noteIdleCtrlC interrupt >>= \case
                    ContinuePrompt -> do
                        finishEditorLine prompt state
                        Text.putStrLn "^C"
                        let cleared = state
                                { editorText = ""
                                , editorCursor = 0
                                , editorSelected = 0
                                , editorHistoryIndex = Nothing
                                , editorHistoryDraft = ""
                                , editorSlashDismissed = False
                                }
                        redrawEditor prompt cleared
                        editorLoop history entries cleared
                    QuitProcess -> do
                        finishEditorLine prompt state
                        pure ReplQuitInterrupt
            EditorEof
                | Text.null state.editorText -> do
                    finishEditorLine prompt state
                    pure ReplEof
                | otherwise ->
                    continue history entries (deleteAtCursor state)
            EditorUp
                | menuHasRows menu ->
                    continue history entries (moveMenuSelection (-1) menu state)
                | otherwise ->
                    continue history entries (historyMove (-1) entries state)
            EditorDown
                | menuHasRows menu ->
                    continue history entries (moveMenuSelection 1 menu state)
                | otherwise ->
                    continue history entries (historyMove 1 entries state)
            EditorTab ->
                case selectedSuggestion state menu of
                    Nothing -> editorLoop history entries state
                    Just suggestion ->
                        continue history entries (acceptSuggestion state menu suggestion)
            EditorEscape
                | menuHasRows menu ->
                    continue history entries state
                        { editorSelected = 0
                        , editorSlashDismissed = True
                        }
                | otherwise ->
                    editorLoop history entries state
            EditorBackspace -> continue history entries (backspace state)
            EditorDelete -> continue history entries (deleteAtCursor state)
            EditorLeft -> continue history entries state
                { editorCursor = max 0 (state.editorCursor - 1)
                , editorSelected = 0
                , editorSlashDismissed = False
                }
            EditorRight -> continue history entries state
                { editorCursor = min (Text.length state.editorText) (state.editorCursor + 1)
                , editorSelected = 0
                , editorSlashDismissed = False
                }
            EditorHome -> continue history entries state
                { editorCursor = 0, editorSelected = 0, editorSlashDismissed = False }
            EditorEnd -> continue history entries state
                { editorCursor = Text.length state.editorText
                , editorSelected = 0
                , editorSlashDismissed = False
                }
            EditorKillStart -> continue history entries (killStart state)
            EditorKillEnd -> continue history entries (killEnd state)
            EditorKillWord -> continue history entries (killWord state)
            EditorYank -> continue history entries (insertText state.editorKillBuffer state)
            EditorClearScreen -> do
                Text.hPutStr stdout "\ESC[2J\ESC[H"
                redrawEditor prompt state
                editorLoop history entries state
            EditorPaste pasted ->
                continue history entries
                    (insertText pasted state) { editorPasted = True }
            EditorChar char ->
                continue history entries (insertText (Text.singleton char) state)
            EditorIgnore -> editorLoop history entries state
      where
        continue history' entries' state' = do
            let normalized = normalizeSelection state'
            redrawEditor prompt normalized
            editorLoop history' entries' normalized

        finish history' state' = do
            finishEditorLine prompt state'
            let text = state'.editorText
            unless (Text.all (== ' ') text) $
                writeHistory historyPath (addHistory (Text.unpack text) history')
                    `catchIO` \_ -> pure ()
            pure $
                if state'.editorPasted
                    then ReplPasted text
                    else ReplText text

menuHasRows :: Maybe SlashMenu -> Bool
menuHasRows = maybe False (not . null . (.slashMenuSuggestions))

currentMenu :: EditorState -> Maybe SlashMenu
currentMenu state
    | state.editorSlashDismissed = Nothing
    | otherwise = slashMenuFor state.editorText state.editorCursor

normalizeSelection :: EditorState -> EditorState
normalizeSelection state =
    case currentMenu state of
        Nothing -> state { editorSelected = 0 }
        Just menu ->
            let count = length menu.slashMenuSuggestions
            in state
                { editorSelected =
                    if count == 0 then 0 else state.editorSelected `mod` count
                }

moveMenuSelection :: Int -> Maybe SlashMenu -> EditorState -> EditorState
moveMenuSelection delta menu state =
    case menu of
        Nothing -> state
        Just SlashMenu{slashMenuSuggestions}
            | null slashMenuSuggestions -> state
            | otherwise ->
                let count = length slashMenuSuggestions
                    selected = (state.editorSelected + delta) `mod` count
                in state { editorSelected = selected }

selectedSuggestion :: EditorState -> Maybe SlashMenu -> Maybe SlashSuggestion
selectedSuggestion state menu = do
    SlashMenu{slashMenuSuggestions} <- menu
    if null slashMenuSuggestions
        then Nothing
        else Just (slashMenuSuggestions !! (state.editorSelected `mod` length slashMenuSuggestions))

exactCommandSelected :: EditorState -> SlashSuggestion -> Bool
exactCommandSelected state suggestion =
    Text.strip state.editorText == suggestion.slashSuggestionDisplay

acceptSuggestion
    :: EditorState
    -> Maybe SlashMenu
    -> SlashSuggestion
    -> EditorState
acceptSuggestion state menu suggestion =
    case menu of
        Nothing -> state
        Just SlashMenu{slashMenuReplaceStart, slashMenuReplaceEnd} ->
            let (before, rest) = Text.splitAt slashMenuReplaceStart state.editorText
                (_, after) = Text.splitAt
                    (slashMenuReplaceEnd - slashMenuReplaceStart)
                    rest
                replacement = suggestion.slashSuggestionReplacement
                text = before <> replacement <> after
            in state
                { editorText = text
                , editorCursor = slashMenuReplaceStart + Text.length replacement
                , editorSelected = 0
                , editorHistoryIndex = Nothing
                , editorHistoryDraft = text
                , editorSlashDismissed = False
                }

insertText :: Text -> EditorState -> EditorState
insertText inserted state =
    let (before, after) = Text.splitAt state.editorCursor state.editorText
        text = before <> inserted <> after
    in state
        { editorText = text
        , editorCursor = state.editorCursor + Text.length inserted
        , editorSelected = 0
        , editorHistoryIndex = Nothing
        , editorHistoryDraft = text
        , editorSlashDismissed = False
        }

backspace :: EditorState -> EditorState
backspace state
    | state.editorCursor <= 0 = state
    | otherwise =
        let start = state.editorCursor - 1
            (before, rest) = Text.splitAt start state.editorText
            (_, after) = Text.splitAt 1 rest
            text = before <> after
        in state
            { editorText = text
            , editorCursor = start
            , editorSelected = 0
            , editorHistoryIndex = Nothing
            , editorHistoryDraft = text
            , editorSlashDismissed = False
            }

deleteAtCursor :: EditorState -> EditorState
deleteAtCursor state
    | state.editorCursor >= Text.length state.editorText = state
    | otherwise =
        let (before, rest) = Text.splitAt state.editorCursor state.editorText
            (_, after) = Text.splitAt 1 rest
            text = before <> after
        in state
            { editorText = text
            , editorSelected = 0
            , editorHistoryIndex = Nothing
            , editorHistoryDraft = text
            , editorSlashDismissed = False
            }

killStart :: EditorState -> EditorState
killStart state =
    let (killed, after) = Text.splitAt state.editorCursor state.editorText
    in state
        { editorText = after
        , editorCursor = 0
        , editorSelected = 0
        , editorHistoryIndex = Nothing
        , editorHistoryDraft = after
        , editorKillBuffer = killed
        , editorSlashDismissed = False
        }

killEnd :: EditorState -> EditorState
killEnd state =
    let (before, killed) = Text.splitAt state.editorCursor state.editorText
    in state
        { editorText = before
        , editorSelected = 0
        , editorHistoryIndex = Nothing
        , editorHistoryDraft = before
        , editorKillBuffer = killed
        , editorSlashDismissed = False
        }

killWord :: EditorState -> EditorState
killWord state
    | state.editorCursor <= 0 = state
    | otherwise =
        let before = Text.take state.editorCursor state.editorText
            trailingSpaces = Text.length (Text.takeWhileEnd isSpace before)
            withoutSpaces = Text.dropEnd trailingSpaces before
            wordLength = Text.length (Text.takeWhileEnd (not . isSpace) withoutSpaces)
            start = state.editorCursor - trailingSpaces - wordLength
            killed = Text.take (state.editorCursor - start) (Text.drop start state.editorText)
            text = Text.take start state.editorText <> Text.drop state.editorCursor state.editorText
        in state
            { editorText = text
            , editorCursor = start
            , editorSelected = 0
            , editorHistoryIndex = Nothing
            , editorHistoryDraft = text
            , editorKillBuffer = killed
            , editorSlashDismissed = False
            }

historyMove :: Int -> [Text] -> EditorState -> EditorState
historyMove delta entries state
    | null entries = state
    | otherwise =
        let current = fromMaybe (-1) state.editorHistoryIndex
            next = current - delta
        in if next < 0
            then state
                { editorText = state.editorHistoryDraft
                , editorCursor = Text.length state.editorHistoryDraft
                , editorHistoryIndex = Nothing
                , editorSelected = 0
                , editorSlashDismissed = False
                }
            else if next >= length entries
                then state
                else
                    let text = entries !! next
                        draft = case state.editorHistoryIndex of
                            Nothing -> state.editorText
                            Just _ -> state.editorHistoryDraft
                    in state
                        { editorText = text
                        , editorCursor = Text.length text
                        , editorHistoryIndex = Just next
                        , editorHistoryDraft = draft
                        , editorSelected = 0
                        , editorSlashDismissed = False
                        }

readEditorKey :: IO EditorKey
readEditorKey = do
    result <- tryIO (hGetChar stdin)
    case result of
        Left err
            | isEOFError err -> pure EditorEof
            | otherwise -> throwIO err
        Right char -> case char of
            '\n' -> pure EditorEnter
            '\r' -> pure EditorEnter
            '\DEL' -> pure EditorBackspace
            '\BS' -> pure EditorBackspace
            '\t' -> pure EditorTab
            '\ESC' -> readEscapeKey
            '\ETX' -> pure EditorInterrupt
            '\EOT' -> pure EditorEof
            '\SOH' -> pure EditorHome
            '\ENQ' -> pure EditorEnd
            '\STX' -> pure EditorLeft
            '\ACK' -> pure EditorRight
            '\DLE' -> pure EditorUp
            '\SO' -> pure EditorDown
            '\NAK' -> pure EditorKillStart
            '\VT' -> pure EditorKillEnd
            '\ETB' -> pure EditorKillWord
            '\EM' -> pure EditorYank
            '\FF' -> pure EditorClearScreen
            _
                | char >= ' ' -> pure (EditorChar char)
                | otherwise -> pure EditorIgnore

readEscapeKey :: IO EditorKey
readEscapeKey = do
    ready <- hWaitForInput stdin 25
    if not ready
        then pure EditorEscape
        else do
            second <- hGetChar stdin
            case second of
                '[' -> readCsiKey
                'O' -> do
                    third <- hGetChar stdin
                    pure $ case third of
                        'A' -> EditorUp
                        'B' -> EditorDown
                        'C' -> EditorRight
                        'D' -> EditorLeft
                        'H' -> EditorHome
                        'F' -> EditorEnd
                        'Z' -> EditorCycleMode
                        _ -> EditorIgnore
                '\n' -> pure (EditorChar '\n')
                '\r' -> pure (EditorChar '\n')
                _ -> pure EditorIgnore

readCsiKey :: IO EditorKey
readCsiKey = do
    body <- readCsiBody ""
    case body of
        "A" -> pure EditorUp
        "B" -> pure EditorDown
        "C" -> pure EditorRight
        "D" -> pure EditorLeft
        "H" -> pure EditorHome
        "F" -> pure EditorEnd
        "Z" -> pure EditorCycleMode
        "1~" -> pure EditorHome
        "4~" -> pure EditorEnd
        "3~" -> pure EditorDelete
        "1;2Z" -> pure EditorCycleMode
        "200~" -> EditorPaste <$> readBracketedPaste
        _ -> pure EditorIgnore

readCsiBody :: String -> IO String
readCsiBody reversed = do
    char <- hGetChar stdin
    let next = char : reversed
    if char >= '@' && char <= '~'
        then pure (reverse next)
        else readCsiBody next

readBracketedPaste :: IO Text
readBracketedPaste = go ""
  where
    end = "\ESC[201~"
    go acc = do
        char <- hGetChar stdin
        let next = Text.snoc acc char
        if end `Text.isSuffixOf` next
            then pure (Text.dropEnd (Text.length end) next)
            else go next

withEditorRawStdin :: IO a -> IO a
withEditorRawStdin action = do
    oldTerm <- getTerminalAttributes stdInput
    oldBuf <- hGetBuffering stdin
    let enter = do
            let raw =
                    flip withMinInput 1
                        . flip withTime 0
                        . flip withoutMode EnableEcho
                        . flip withoutMode ProcessInput
                        . flip withoutMode KeyboardInterrupts
                        $ oldTerm
            setTerminalAttributes stdInput raw Immediately
            hSetBuffering stdin NoBuffering
        restore = do
            setTerminalAttributes stdInput oldTerm Immediately
            hSetBuffering stdin oldBuf
    bracket enter (const restore) \() -> action

redrawEditor :: Text -> EditorState -> IO ()
redrawEditor prompt state = do
    width <- maybe 80 snd <$> getTerminalSize
    let menu = currentMenu state
        rows = visibleMenuRows width state menu
        (shown, cursorColumn) =
            visibleEditorText
                (max 1 (width - visibleWidth prompt))
                state.editorText
                state.editorCursor
    Text.hPutStr stdout "\r\ESC[J"
    Text.hPutStr stdout prompt
    Text.hPutStr stdout shown
    Text.hPutStr stdout "\ESC[K\ESC[0m"
    mapM_ (\row -> Text.hPutStr stdout ("\n" <> row <> "\ESC[K")) rows
    when (not (null rows)) $
        Text.hPutStr stdout (Text.pack (cursorUpCode (length rows)))
    Text.hPutStr stdout "\r"
    let target = visibleWidth prompt + cursorColumn
    when (target > 0) $
        Text.hPutStr stdout ("\ESC[" <> Text.pack (show target) <> "C")
    hFlush stdout

finishEditorLine :: Text -> EditorState -> IO ()
finishEditorLine prompt state = do
    Text.hPutStr stdout "\r\ESC[J"
    Text.hPutStr stdout prompt
    Text.hPutStr stdout (displayEditorText state.editorText)
    Text.hPutStr stdout "\ESC[K\ESC[0m\n"
    hFlush stdout

visibleMenuRows :: Int -> EditorState -> Maybe SlashMenu -> [Text]
visibleMenuRows width state = \case
    Nothing -> []
    Just SlashMenu{slashMenuSuggestions} ->
        let count = length slashMenuSuggestions
            selected
                | count == 0 = 0
                | otherwise = state.editorSelected `mod` count
            start = max 0 (min selected (max 0 (count - maxMenuRows)))
            visible = take maxMenuRows (drop start slashMenuSuggestions)
        in zipWith (renderMenuRow width selected start) [0 ..] visible
  where
    maxMenuRows = 6

renderMenuRow :: Int -> Int -> Int -> Int -> SlashSuggestion -> Text
renderMenuRow width selected start localIndex suggestion =
    let absoluteIndex = start + localIndex
        marker = if absoluteIndex == selected then "❯ " else "  "
        label = suggestion.slashSuggestionDisplay
        available =
            max 0
                (width - terminalTextWidth marker - terminalTextWidth label - 2)
        summary = truncateDisplayText available suggestion.slashSuggestionSummary
        gap = if Text.null summary then "" else "  "
        row = truncateDisplayText width (marker <> label <> gap <> summary)
    in if absoluteIndex == selected
        then "\ESC[1;36m" <> row <> "\ESC[0m"
        else "\ESC[2m" <> row <> "\ESC[0m"

visibleEditorText :: Int -> Text -> Int -> (Text, Int)
visibleEditorText available raw cursor =
    let cells = displayCells raw
        before = take cursor cells
    in if cellsWidth cells <= available
        then (renderCells cells, cellsWidth before)
        else
            let leftRoom = max 1 (available * 2 `div` 3)
                visibleBefore = takeSuffixColumns leftRoom before
                start = length before - length visibleBefore
                shownCells = takeColumns available (drop start cells)
            in (renderCells shownCells, cellsWidth visibleBefore)

displayEditorText :: Text -> Text
displayEditorText = renderCells . displayCells

truncateDisplayText :: Int -> Text -> Text
truncateDisplayText width text
    | width <= 0 = ""
    | cellsWidth cells <= width = renderCells cells
    | width == 1 = "…"
    | otherwise = renderCells (takeColumns (width - 1) cells) <> "…"
  where
    cells = displayCells text

visibleWidth :: Text -> Int
visibleWidth = terminalTextWidth . stripAnsi

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
-- | Read a one-shot approval answer. The question is always written to
-- stderr (matching the historical behavior) so redirected stdout does
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
