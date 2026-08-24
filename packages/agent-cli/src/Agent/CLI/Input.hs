-- | First-party line editing for interactive prompts. TTY sessions use a raw
-- inline editor with live slash-command completion; non-TTY falls back to
-- plain 'getLine'.
module Agent.CLI.Input
    ( ReplLine(..)
    , readReplLine
    , readReplLineWithInitial
    , readReplLineWithSkills
    , readReplLineWithSkillsAndModels
    , readApprovalLine
    , readChoiceSelection
    , approvalKeyText
    , ChoiceKey(..)
    , parseChoiceKey
    , choiceMoveIndex
    , classifyPastedText
    , decodeBracketedPastePayload
    , displayEditorText
    , formatPasteChip
    , isClipboardPasteCsiBody
    , isClipboardPasteKey
    , isShiftEnterCsiBody
    , submissionPromptText
    , appendReplHistory
    , readReplHistory
    , replHistoryPath
    , terminalCharWidth
    , terminalTextWidth
    , truncateDisplayText
    , visibleEditorText
    ) where

import Agent.CLI.Clipboard
    ( nonEmptyClipboardText
    , readClipboardText
    )
import Agent.CLI.Command
    ( SkillCommand
    , SlashMenu(..)
    , SlashSuggestion(..)
    , slashMenuForWithSkillsAndModels
    )
import Agent.CLI.Input.Display
import Agent.CLI.Input.History
import Agent.CLI.Input.KeyDecoder
import Agent.CLI.Input.Paste
import Agent.CLI.Input.Picker
import Agent.CLI.Input.Types
import Agent.CLI.Interrupt
    ( IdleCtrlCResult(..)
    , InterruptState
    , noteIdleCtrlC
    )
import Agent.CLI.Terminal
    ( TerminalCapabilities(..)
    , detectTerminalCapabilities
    , emitTerminalSequence
    , kittyKeyboardDisambiguatePush
    , kittyKeyboardPop
    , stripAnsi
    )
import Control.Exception.Safe (bracket, bracket_, catchIO, throwIO, tryIO)
import Control.Monad (unless, when)
import Data.Char
    ( isSpace
    )
import Data.List (isPrefixOf)
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
    ( getHomeDirectory
    )
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

-- | Read a REPL prompt line. Persists history under 'replHistoryPath' when
-- stdin is a TTY. 'ReplEof' means EOF; 'ReplQuitInterrupt' means a confirmed
-- double Ctrl-C. The prompt is drawn on stdout.
--
-- The raw editor reads Ctrl-C as an input byte, so idle Ctrl-C goes through
-- 'noteIdleCtrlC' rather than the outer signal handler.
readReplLine :: InterruptState -> Text -> IO ReplLine
readReplLine interrupt prompt =
    readReplLineConfigured [] [] False interrupt prompt ""

-- | Like 'readReplLine', restoring @initial@ as the in-progress draft.
readReplLineWithInitial :: InterruptState -> Text -> Text -> IO ReplLine
readReplLineWithInitial =
    readReplLineConfigured [] [] True

readReplLineWithSkills
    :: [SkillCommand]
    -> InterruptState
    -> Text
    -> Text
    -> IO ReplLine
readReplLineWithSkills skills =
    readReplLineConfigured skills [] True

readReplLineWithSkillsAndModels
    :: [SkillCommand]
    -> [Text]
    -> InterruptState
    -> Text
    -> Text
    -> IO ReplLine
readReplLineWithSkillsAndModels skills modelIds =
    readReplLineConfigured skills modelIds True

readReplLineConfigured
    :: [SkillCommand]
    -> [Text]
    -> Bool
    -> InterruptState
    -> Text
    -> Text
    -> IO ReplLine
readReplLineConfigured skills modelIds slashEnabled interrupt prompt initial = do
    isTty <- hIsTerminalDevice stdin
    if isTty
        then do
            home <- getHomeDirectory
            let path = replHistoryPath home
            ensureHistoryParent path
            classifyLine <$>
                readInlineEditor
                    skills modelIds slashEnabled interrupt path prompt initial
        else do
            Text.hPutStr stdout prompt
            hFlush stdout
            fmap (maybe ReplEof classifySubmitted) readRawLine
  where
    classifyLine = \case
        ReplText text -> classifySubmitted text
        other -> other
    classifySubmitted text =
        let (stripped, pasted) = classifyPastedText text
        in if pasted then ReplPasted stripped else ReplText stripped

-- | First-party inline editor for the interactive TTY path. It owns the
-- prompt redraw so slash suggestions can update after every keystroke.
readInlineEditor
    :: [SkillCommand]
    -> [Text]
    -> Bool
    -> InterruptState
    -> FilePath
    -> Text
    -> Text
    -> IO ReplLine
readInlineEditor
        skills modelIds slashEnabled interrupt historyPath prompt initial = do
    withBracketedPaste $
        withEditorKittyKeyboard $
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
                                , editorSlashEnabled = slashEnabled
                                , editorSlashDismissed = False
                                , editorSkillCommands = skills
                                , editorModelIds = modelIds
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
            EditorClipboardPaste images -> do
                finishEditorLine prompt state
                pure (ReplClipboardPaste state.editorText images)
            EditorInterrupt ->
                noteIdleCtrlC interrupt >>= \case
                    ContinuePrompt -> do
                        finishEditorLine prompt state
                        Text.putStrLn "^C"
                        redrawEditor prompt state
                        editorLoop history entries state
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
            EditorPaste pasted -> do
                finishEditorLine prompt state
                let pastedState =
                        (insertText pasted state) { editorPasted = True }
                pure $ ReplClipboardPasteOrText
                    state.editorText
                    pasted
                    pastedState.editorText
            EditorInputError message -> do
                finishEditorLine prompt state
                Text.putStrLn ("input ignored: " <> message)
                redrawEditor prompt state
                editorLoop history entries state
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
    | not state.editorSlashEnabled = Nothing
    | state.editorSlashDismissed = Nothing
    | otherwise =
        slashMenuForWithSkillsAndModels
            state.editorSkillCommands
            state.editorModelIds
            state.editorText
            state.editorCursor

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
        Right char
            | isClipboardPasteKey char -> readClipboardEditorKey
            | otherwise -> case char of
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
        else readTimedChar escapeSequenceTimeoutMs >>= \case
            TimedOut -> pure (EditorInputError "escape sequence timed out")
            TimedEof -> pure (EditorInputError "input ended during escape sequence")
            TimedChar second -> case second of
                '[' -> readCsiKey
                'O' ->
                    readTimedChar escapeSequenceTimeoutMs >>= \case
                        TimedOut -> pure (EditorInputError "SS3 sequence timed out")
                        TimedEof -> pure (EditorInputError "input ended during SS3 sequence")
                        TimedChar third ->
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
readCsiKey =
    readCsiBody >>= \case
        Left err -> pure (EditorInputError err)
        Right body
            | isShiftEnterCsiBody body -> pure (EditorChar '\n')
            | isClipboardPasteCsiBody body -> readClipboardEditorKey
            | Just key <- decodeKittyEditorKey body -> pure key
            | otherwise -> case body of
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
                "200~" ->
                    readBracketedPaste >>= \case
                        Left err -> pure (EditorInputError err)
                        Right pasted
                            | Text.null pasted ->
                                pure (EditorClipboardPaste Nothing)
                            | otherwise -> pure (EditorPaste pasted)
                _ -> pure EditorIgnore

readClipboardEditorKey :: IO EditorKey
readClipboardEditorKey =
    readClipboardText >>= \result ->
        pure $ case nonEmptyClipboardText result of
            Just text -> EditorPaste text
            Nothing -> EditorClipboardPaste Nothing

readCsiBody :: IO (Either Text String)
readCsiBody = go 0 []
  where
    go count reversed
        | count >= maxCsiBodyLength = do
            drainCsiBody
            pure (Left "CSI sequence exceeded 32 bytes")
        | otherwise =
            readTimedChar escapeSequenceTimeoutMs >>= \case
                TimedOut -> pure (Left "CSI sequence timed out")
                TimedEof -> pure (Left "input ended during CSI sequence")
                TimedChar char ->
                    let next = char : reversed
                    in if char >= '@' && char <= '~'
                        then pure (Right (reverse next))
                        else go (count + 1) next

    drainCsiBody :: IO ()
    drainCsiBody =
        readTimedChar escapeSequenceTimeoutMs >>= \case
            TimedChar char
                | char >= '@' && char <= '~' -> pure ()
                | otherwise -> drainCsiBody
            TimedOut -> pure ()
            TimedEof -> pure ()

readBracketedPaste :: IO (Either Text Text)
readBracketedPaste = go 0 []
  where
    markerLength = length bracketedPasteEnd
    markerReversed = reverse bracketedPasteEnd
    maximumBuffered = maxBracketedPasteChars + markerLength

    go count reversed =
        readTimedChar pasteIdleTimeoutMs >>= \case
            TimedOut ->
                pure (Left "bracketed paste timed out before its end marker")
            TimedEof ->
                pure (Left "input ended during bracketed paste")
            TimedChar char -> do
                let next = char : reversed
                    count' = count + 1
                if markerReversed `isPrefixOf` next
                    then pure $
                        decodeBracketedPastePayload
                            maxBracketedPasteChars
                            (Text.pack (reverse next))
                    else if count' > maximumBuffered
                        then do
                            drainBracketedPaste (take (markerLength - 1) next)
                            pure (Left "bracketed paste exceeded 8 million characters")
                        else go count' next

    drainBracketedPaste recentReversed =
        readTimedChar pasteIdleTimeoutMs >>= \case
            TimedChar char ->
                let next = char : recentReversed
                in unless (markerReversed `isPrefixOf` next) $
                    drainBracketedPaste (take (markerLength - 1) next)
            TimedOut -> pure ()
            TimedEof -> pure ()

readTimedChar :: Int -> IO TimedRead
readTimedChar timeoutMs = do
    ready <- hWaitForInput stdin timeoutMs
    if not ready
        then pure TimedOut
        else
            tryIO (hGetChar stdin) >>= \case
                Left err
                    | isEOFError err -> pure TimedEof
                    | otherwise -> throwIO err
                Right char -> pure (TimedChar char)

maxCsiBodyLength :: Int
maxCsiBodyLength = 32

escapeSequenceTimeoutMs :: Int
escapeSequenceTimeoutMs = 100

pasteIdleTimeoutMs :: Int
pasteIdleTimeoutMs = 2000

maxBracketedPasteChars :: Int
maxBracketedPasteChars = 8 * 1024 * 1024

bracketedPasteEnd :: String
bracketedPasteEnd = "\ESC[201~"

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
                        . flip withoutMode ExtendedFunctions
                        . flip withoutMode KeyboardInterrupts
                        $ oldTerm
            setTerminalAttributes stdInput raw Immediately
            hSetBuffering stdin NoBuffering
        restore = do
            setTerminalAttributes stdInput oldTerm Immediately
            hSetBuffering stdin oldBuf
    bracket enter (const restore) \() -> action

withEditorKittyKeyboard :: IO a -> IO a
withEditorKittyKeyboard action = do
    terminal <- detectTerminalCapabilities stdout
    if terminal.terminalKittyKeyboard
        then
            bracket_
                (emitTerminalSequence terminal stdout kittyKeyboardDisambiguatePush)
                (emitTerminalSequence terminal stdout kittyKeyboardPop)
                action
        else action

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

visibleWidth :: Text -> Int
visibleWidth = terminalTextWidth . stripAnsi

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
readAnswerOnly = fmap (fmap Text.strip) readRawLine

-- | Read one line without changing whitespace. Prompt payloads can contain
-- indentation or trailing blank data; approval answers normalize separately.
readRawLine :: IO (Maybe Text)
readRawLine = do
    done <- isEOF
    if done
        then pure Nothing
        else Just <$> Text.getLine
