-- | First-party line editing for interactive prompts. TTY sessions use a raw
-- inline editor with live slash-command completion; non-TTY falls back to
-- plain 'getLine'.
module Agent.CLI.Input
    ( ReplLine(..)
    , readReplLine
    , readReplLineForProvider
    , readReplLineWithInitial
    , readReplLineWithCatalog
    , readReplLineWithCatalogForProvider
    , readReplLineWithCatalogForTarget
    , readReplLineWithSkills
    , readReplLineWithSkillsAndModels
    , readModalText
    , readApprovalLine
    , readChoiceSelection
    , readChoiceSelectionAt
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
import Agent.CLI.Dictation
    ( DictationTarget(..)
    , dictateForTarget
    , insertDictation
    )
import Agent.CLI.Command
    ( SkillCommand
    , SlashCatalog(..)
    , SlashMenu(..)
    , SlashSuggestion(..)
    , defaultSlashCatalog
    , slashCatalogWithSkills
    )
import Agent.CLI.Input.Display
import Agent.CLI.Input.Approval
import Agent.CLI.Input.Editor
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
import Agent.Provider (Provider(XAIProvider))
import Agent.CLI.Terminal
    ( TerminalCapabilities(..)
    , detectTerminalCapabilities
    , emitTerminalSequence
    , kittyKeyboardDisambiguatePush
    , kittyKeyboardPop
    , stripAnsi
    )
import Control.Exception.Safe
    ( bracket
    , bracket_
    , catchIO
    , throwIO
    , tryAny
    , tryIO
    )
import Control.Monad (unless, when)
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Console.ANSI
    ( getTerminalSize
    , hHideCursor
    , hShowCursor
    )
import System.Console.ANSI.Codes
    ( cursorUpCode )
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
    , hFlush
    , hGetBuffering
    , hGetChar
    , hIsTerminalDevice
    , hSetBuffering
    , hWaitForInput
    , isEOF
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
    readReplLineConfigured
        (Just (DirectDictation XAIProvider))
        defaultSlashCatalog False interrupt prompt ""

-- | Read a prompt whose dictation backend follows the active model provider.
readReplLineForProvider :: Provider -> InterruptState -> Text -> IO ReplLine
readReplLineForProvider provider interrupt prompt =
    readReplLineConfigured
        (Just (DirectDictation provider))
        defaultSlashCatalog False interrupt prompt ""

-- | Like 'readReplLine', restoring @initial@ as the in-progress draft.
readReplLineWithInitial :: InterruptState -> Text -> Text -> IO ReplLine
readReplLineWithInitial =
    readReplLineConfigured
        (Just (DirectDictation XAIProvider))
        defaultSlashCatalog
        True

readReplLineWithCatalog
    :: SlashCatalog
    -> InterruptState
    -> Text
    -> Text
    -> IO ReplLine
readReplLineWithCatalog catalog =
    readReplLineConfigured Nothing catalog True

readReplLineWithCatalogForProvider
    :: Provider
    -> SlashCatalog
    -> InterruptState
    -> Text
    -> Text
    -> IO ReplLine
readReplLineWithCatalogForProvider provider catalog =
    readReplLineWithCatalogForTarget
        (DirectDictation provider)
        catalog

readReplLineWithCatalogForTarget
    :: DictationTarget
    -> SlashCatalog
    -> InterruptState
    -> Text
    -> Text
    -> IO ReplLine
readReplLineWithCatalogForTarget target catalog =
    readReplLineConfigured (Just target) catalog True

readReplLineWithSkills
    :: [SkillCommand]
    -> InterruptState
    -> Text
    -> Text
    -> IO ReplLine
readReplLineWithSkills skills =
    readReplLineConfigured
        (Just (DirectDictation XAIProvider))
        (slashCatalogWithSkills skills defaultSlashCatalog)
        True

readReplLineWithSkillsAndModels
    :: [SkillCommand]
    -> [Text]
    -> InterruptState
    -> Text
    -> Text
    -> IO ReplLine
readReplLineWithSkillsAndModels skills modelIds =
    readReplLineConfigured
        (Just (DirectDictation XAIProvider))
        ((slashCatalogWithSkills skills defaultSlashCatalog)
            { slashCatalogModelIds = modelIds
            })
        True

readReplLineConfigured
    :: Maybe DictationTarget
    -> SlashCatalog
    -> Bool
    -> InterruptState
    -> Text
    -> Text
    -> IO ReplLine
readReplLineConfigured
        dictationProvider catalog slashEnabled interrupt prompt initial = do
    readLineConfigured
        True
        dictationProvider
        catalog
        slashEnabled
        interrupt
        prompt
        initial

-- | Read transient modal text without slash-command completion or modifying
-- normal REPL history. Escape, Ctrl-C, and EOF cancel on the TTY path.
readModalText :: InterruptState -> Text -> Text -> IO (Maybe Text)
readModalText interrupt prompt initial =
    readLineConfigured
        False
        Nothing
        defaultSlashCatalog
        False
        interrupt
        prompt
        initial
        >>= \case
            ReplText text -> pure (Just text)
            ReplPasted text -> pure (Just text)
            ReplEof -> pure Nothing
            ReplQuitInterrupt -> pure Nothing
            _ -> pure Nothing

readLineConfigured
    :: Bool
    -> Maybe DictationTarget
    -> SlashCatalog
    -> Bool
    -> InterruptState
    -> Text
    -> Text
    -> IO ReplLine
readLineConfigured
        historyEnabled
        dictationProvider
        catalog
        slashEnabled
        interrupt
        prompt
        initial = do
    isTty <- hIsTerminalDevice stdin
    if isTty
        then do
            home <- getHomeDirectory
            let path = replHistoryPath home
            when historyEnabled (ensureHistoryParent path)
            classifyLine <$>
                readInlineEditor
                    historyEnabled
                    dictationProvider
                    catalog
                    slashEnabled
                    interrupt
                    path
                    prompt
                    initial
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
    :: Bool
    -> Maybe DictationTarget
    -> SlashCatalog
    -> Bool
    -> InterruptState
    -> FilePath
    -> Text
    -> Text
    -> IO ReplLine
readInlineEditor
        historyEnabled
        dictationProvider
        catalog
        slashEnabled
        interrupt
        historyPath
        prompt
        initial = do
    withBracketedPaste $
        withEditorKittyKeyboard $
            withEditorRawStdin $
                bracket
                    (hHideCursor stdout)
                    (\_ -> hShowCursor stdout >> hFlush stdout)
                    \() -> do
                        history <-
                            if historyEnabled
                                then
                                    readHistory historyPath
                                        `catchIO` \_ -> pure emptyHistory
                                else pure emptyHistory
                        let entries = map Text.pack (historyLines history)
                            state =
                                initialEditorState catalog slashEnabled initial
                        redrawEditor prompt state
                        editorLoop history entries state
  where
    editorLoop history entries state = do
        key <- readEditorKey
        case (historyEnabled, key, currentMenu state) of
            (False, EditorEscape, Nothing) ->
                cancelModal state
            (False, EditorEof, _) ->
                cancelModal state
            _ -> applyStep key
      where
        cancelModal state = do
            finishEditorLine prompt state
            pure ReplQuitInterrupt
        applyStep key = do
          let EditorStep
                  { editorStepState = next
                  , editorStepEffect = requested
                  } = reduceEditorKey entries state key
          case requested of
            RedrawEditor -> do
                redrawEditor prompt next
                editorLoop history entries next
            SubmitEditor ->
                finish history next
            ReturnEditor line -> do
                finishEditorLine prompt next
                pure line
            CheckEditorInterrupt ->
                if not historyEnabled
                    then cancelModal next
                    else noteIdleCtrlC interrupt >>= \case
                        ContinuePrompt -> do
                            finishEditorLine prompt next
                            Text.putStrLn "^C"
                            redrawEditor prompt next
                            editorLoop history entries next
                        QuitProcess -> do
                            finishEditorLine prompt next
                            pure ReplQuitInterrupt
            ClearEditorScreen -> do
                Text.hPutStr stdout "\ESC[2J\ESC[H"
                redrawEditor prompt next
                editorLoop history entries next
            DictateIntoEditor -> do
                finishEditorLine prompt next
                result <- tryAny $
                    maybe
                        (fail "Dictation is unavailable in this prompt.")
                        dictateForTarget
                        dictationProvider
                case result of
                    Left err ->
                        Text.putStrLn
                            ("dictation failed: " <> Text.pack (show err))
                    Right _ ->
                        Text.putStrLn "Dictation inserted."
                let state' = case result of
                        Left _ -> next
                        Right transcript ->
                            let (text, cursor) =
                                    insertDictation
                                        next.editorText
                                        next.editorCursor
                                        transcript
                            in next
                                { editorText = text
                                , editorCursor = cursor
                                , editorHistoryIndex = Nothing
                                , editorHistoryDraft = text
                                , editorSlashDismissed = False
                                }
                redrawEditor prompt state'
                editorLoop history entries state'
            ReportEditorError message -> do
                finishEditorLine prompt next
                Text.putStrLn ("input ignored: " <> message)
                redrawEditor prompt next
                editorLoop history entries next
            IgnoreEditorInput ->
                editorLoop history entries next
        finish history' state' = do
            finishEditorLine prompt state'
            let text = state'.editorText
            when historyEnabled $
                unless (Text.all (== ' ') text) $
                    writeHistory
                        historyPath
                        (addHistory (Text.unpack text) history')
                        `catchIO` \_ -> pure ()
            pure $
                if state'.editorPasted
                    then ReplPasted text
                    else ReplText text

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
                '\DC2' -> pure EditorDictate
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

-- | Read one line without changing whitespace. Prompt payloads can contain
-- indentation or trailing blank data.
readRawLine :: IO (Maybe Text)
readRawLine = do
    done <- isEOF
    if done
        then pure Nothing
        else Just <$> Text.getLine
