-- | Stream renderer and mutating-tool approval prompts.
module Agent.CLI.Render
    ( RenderConfig(..)
    , clearThinking
    , commitThinking
    , formatActivityLine
    , formatElapsed
    , formatLoopError
    , formatLoopErrorColored
    , formatSearchReplaceDiff
    , formatThinkingBlock
    , formatToolStarted
    , formatTurnStatus
    , putTextLn
    , renderAssistantText
    , renderEvent
    , summarizeToolCall
    , thinkingMaxWidth
    , truncateToolOutput
    , visibleDisplayRows
    , wrapThinkingLines
    ) where

import Agent.CLI.Markdown (renderMarkdown)
import Agent.CLI.Progress
    ( osc9ProgressIndeterminate
    , osc9ProgressRemove
    , wrapOscForTmux
    )
import Agent.CLI.Style
    ( agentBackground
    , glyphCancel
    , glyphErr
    , glyphOk
    , glyphThink
    , glyphTool
    , glyphToolAccent
    , glyphToolOut
    , paintBackgroundLines
    , roleError
    , roleMuted
    , roleSuccess
    , roleThinking
    , roleToolArrow
    , roleToolCommand
    , roleToolDetail
    , roleToolName
    , roleToolOutput
    , roleToolPath
    , solarizedGreen
    , solarizedRed
    , spinnerFrames
    , style
    )
import Agent.JsonText (jsonTextFieldDefault)
import Agent.Loop (LoopError(..), LoopEvent(..), TurnOutput(..))
import Agent.ToolDispatch (ToolCall(..), ToolCallResult(..))
import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, withMVar)
import Control.Exception.Safe (tryIO)
import Control.Monad (unless, void, when)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.IORef
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.IO as Text
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import System.Console.ANSI (ConsoleLayer(..), SGR(..))
import System.Console.ANSI.Codes
    ( clearFromCursorToScreenEndCode
    , setCursorColumnCode
    )
import System.Environment (lookupEnv)
import System.IO (Handle, hFlush)

data RenderConfig = RenderConfig
    { renderShowThinking :: !Bool
    , renderThinkingVisible :: !(IORef Bool)
    , renderThinkingSpinner :: !(IORef (Maybe ThreadId))
    -- | Accumulated reasoning-summary text for the current model round.
    , renderReasoningBuffer :: !(IORef Text)
    -- | True after a live thinking-block paint on stderr.
    , renderReasoningLive :: !(IORef Bool)
    , renderColor :: !Bool
    , renderPrintedText :: !(IORef Bool)
    , renderTextBuffer :: !(IORef Text)
    -- | True after a live color-mode paint; next redraw restores the saved
    -- cursor and clears from there before restyling the buffer.
    , renderLiveActive :: !(IORef Bool)
    , renderLock :: !(MVar ())
    , renderStdout :: !Handle
    , renderStderr :: !Handle
    , renderModelRef :: !(IORef Text)
    , renderActivityRef :: !(IORef Text)
    , renderStartedAt :: !(IORef (Maybe UTCTime))
    , renderNativeProgress :: !Bool -- ^ Ghostty / WT OSC 9;4; off in tests
    }

-- | Grok-build @max_thoughts_width@: wrap reasoning display at this column.
thinkingMaxWidth :: Int
thinkingMaxWidth = 120

renderEvent :: RenderConfig -> LoopEvent -> IO ()
renderEvent config event =
    withMVar config.renderLock \_ -> renderEventUnlocked config event

renderEventUnlocked :: RenderConfig -> LoopEvent -> IO ()
renderEventUnlocked config = \case
    TextDelta delta -> do
        commitThinkingUnlocked config
        if config.renderColor
            then do
                modifyIORef' config.renderTextBuffer (<> delta)
                buffered <- readIORef config.renderTextBuffer
                redrawLiveAssistant config buffered
            else do
                writeIORef config.renderPrintedText True
                Text.hPutStr config.renderStdout delta
                hFlush config.renderStdout
    ReasoningDelta delta ->
        appendReasoningUnlocked config delta
    TurnStarted -> do
        writeIORef config.renderTextBuffer ""
        writeIORef config.renderLiveActive False
        writeIORef config.renderReasoningBuffer ""
        writeIORef config.renderReasoningLive False
        writeIORef config.renderActivityRef "Thinking…"
        now <- getCurrentTime
        writeIORef config.renderStartedAt (Just now)
        startThinkingSpinnerUnlocked config
    -- Finalize live color output (or paint once for non-streaming backends).
    -- Pre-tool prose ("I'll check…") is shown before tool lines; the final
    -- tool-free turn is the main answer.
    TurnFinished turn -> do
        commitThinkingUnlocked config
        when config.renderColor do
            didPrint <- finalizeAssistantBuffer config turn.assistantText
            when (didPrint && not (null turn.toolCalls)) do
                putTextLn config.renderStdout ""
    ToolStarted call -> do
        commitThinkingUnlocked config
        writeIORef config.renderActivityRef (summarizeToolCall call)
        putTextLn config.renderStderr (formatToolStarted config.renderColor call)
        let extra = formatToolBody config.renderColor call
        unless (Text.null extra) do
            putTextLn config.renderStderr extra
        when config.renderShowThinking do
            visible <- readIORef config.renderThinkingVisible
            if visible
                then paintThinkingFrame config 0
                else startThinkingSpinnerUnlocked config
    ToolFinished result ->
        putTextLn config.renderStderr
            (roleToolOutput config.renderColor (truncateToolOutput result.output))

-- | Style assistant markdown when color is enabled; otherwise return plain text.
-- Color mode also paints each line with 'agentBackground'.
renderAssistantText :: Bool -> Text -> Text
renderAssistantText color text =
    paintBackgroundLines color agentBackground (renderMarkdown color text)

-- | Live color-mode redraw: restyle the full buffer and replace the previous
-- painted region in-place so tokens appear as they stream.
--
-- Uses DECSC/DECRC (ESC 7 / ESC 8) instead of counting display rows. Row
-- counting breaks on soft-wrap, wide glyphs, and background erase sequences
-- from 'paintBackgroundLines', which left garbled leftover text on redraw.
redrawLiveAssistant :: RenderConfig -> Text -> IO ()
redrawLiveAssistant config raw
    | Text.null raw = pure ()
    | otherwise = do
        let painted = renderAssistantText True raw
        eraseLiveAssistant config
        -- Save cursor at the start of this paint so the next delta can return.
        Text.hPutStr config.renderStdout "\ESC7"
        writeIORef config.renderPrintedText True
        writeIORef config.renderLiveActive True
        Text.hPutStr config.renderStdout painted
        hFlush config.renderStdout

-- | End-of-turn: keep live paint when deltas already drew; otherwise paint
-- once from the buffer or completed 'assistantText' (non-streaming backends).
-- Returns whether anything was written.
finalizeAssistantBuffer :: RenderConfig -> Maybe Text -> IO Bool
finalizeAssistantBuffer config assistantText = do
    buffered <- readIORef config.renderTextBuffer
    writeIORef config.renderTextBuffer ""
    live <- readIORef config.renderLiveActive
    writeIORef config.renderLiveActive False
    if live
        then do
            writeIORef config.renderPrintedText True
            pure True
        else do
            let raw
                    | not (Text.null buffered) = buffered
                    | otherwise = fromMaybe "" assistantText
            if Text.null raw
                then pure False
                else do
                    writeIORef config.renderPrintedText True
                    Text.hPutStr config.renderStdout (renderAssistantText True raw)
                    hFlush config.renderStdout
                    pure True

eraseLiveAssistant :: RenderConfig -> IO ()
eraseLiveAssistant config = do
    live <- readIORef config.renderLiveActive
    when live do
        -- Restore to the saved start of the previous live paint, then clear
        -- everything below so soft-wrapped leftovers disappear.
        Text.hPutStr config.renderStdout $
            "\ESC8"
                <> Text.pack (setCursorColumnCode 0)
                <> Text.pack clearFromCursorToScreenEndCode
        hFlush config.renderStdout
    writeIORef config.renderLiveActive False


-- | How many terminal rows a painted assistant block occupies, accounting for
-- soft wrap at @width@. ANSI/OSC sequences do not consume columns.
-- Kept for tests / diagnostics; live redraw no longer depends on it.
visibleDisplayRows :: Int -> Text -> Int
visibleDisplayRows width painted
    | Text.null painted = 0
    | otherwise =
        let width' = max 1 width
            endsWithNewline = Text.isSuffixOf "\n" painted
            parts = Text.splitOn "\n" painted
            logical
                | endsWithNewline && not (null parts) = init parts
                | otherwise = parts
            rowCount line =
                let cols = Text.length (stripAnsiOsc line)
                in if cols == 0
                    then 1
                    else max 1 ((cols + width' - 1) `div` width')
        in sum (map rowCount logical)

-- | Drop CSI and OSC-8 hyperlink wrappers so length matches visible cells.
stripAnsiOsc :: Text -> Text
stripAnsiOsc = go
  where
    go t
        | Text.null t = t
        | Text.isPrefixOf "\ESC[" t =
            let rest = Text.drop 2 t
                (_, after) = Text.break isCsiFinal rest
            in go (Text.drop 1 after)
        | Text.isPrefixOf "\ESC]8;" t =
            case Text.breakOn "\ESC\\" (Text.drop 4 t) of
                (_params, rest)
                    | Text.isPrefixOf "\ESC\\" rest -> go (Text.drop 2 rest)
                    | otherwise ->
                        case Text.break (== '\a') (Text.drop 4 t) of
                            (_params, rest2)
                                | Text.isPrefixOf "\a" rest2 -> go (Text.drop 1 rest2)
                                | otherwise -> go (Text.drop 1 t)
        | Text.head t == '\ESC' = go (Text.drop 1 t)
        | otherwise =
            let (plain, rest) = Text.break (== '\ESC') t
            in plain <> go rest

isCsiFinal :: Char -> Bool
isCsiFinal c = c >= '@' && c <= '~'

-- | Clear a leftover thinking status line. Safe to call when none is visible.
-- Commits a streamed reasoning block first so cancel/error still leave the
-- summary in the transcript.
clearThinking :: RenderConfig -> IO ()
clearThinking config =
    withMVar config.renderLock \_ -> commitThinkingUnlocked config

-- | Flush a completed reasoning block to stderr. Safe when none is pending.
commitThinking :: RenderConfig -> IO ()
commitThinking config =
    withMVar config.renderLock \_ -> commitThinkingUnlocked config

startThinkingSpinnerUnlocked :: RenderConfig -> IO ()
startThinkingSpinnerUnlocked config
    | not config.renderShowThinking = pure ()
    | otherwise = do
        emitNativeProgress config True
        -- A live reasoning block already owns stderr; don't overlay a spinner.
        live <- readIORef config.renderReasoningLive
        unless live do
            visible <- readIORef config.renderThinkingVisible
            if visible
                then paintThinkingFrame config 0
                else do
                    writeIORef config.renderThinkingVisible True
                    paintThinkingFrame config 0
                    tid <- forkIO (spinnerLoop config 0)
                    writeIORef config.renderThinkingSpinner (Just tid)

-- | Stop the spinner and erase its in-place status line. Does not commit a
-- reasoning block; callers that need that use 'commitThinkingUnlocked'.
stopThinkingSpinnerUnlocked :: RenderConfig -> IO ()
stopThinkingSpinnerUnlocked config = do
    mtid <- atomicModifyIORef' config.renderThinkingSpinner \mt -> (Nothing, mt)
    case mtid of
        Just tid -> killThread tid
        Nothing -> pure ()
    visible <- readIORef config.renderThinkingVisible
    when visible do
        void $ tryIO do
            Text.hPutStr config.renderStderr "\r\ESC[K"
            hFlush config.renderStderr
        writeIORef config.renderThinkingVisible False

-- | Ghostty (and Windows Terminal / ConEmu) native loading indicator.
-- Harmless no-op on terminals that do not implement OSC 9;4.
emitNativeProgress :: RenderConfig -> Bool -> IO ()
emitNativeProgress config active
    | not config.renderNativeProgress = pure ()
    | otherwise = do
        inTmux <- isJust <$> lookupEnv "TMUX"
        let seq_ =
                wrapOscForTmux inTmux $
                    if active then osc9ProgressIndeterminate else osc9ProgressRemove
        void $ tryIO do
            Text.hPutStr config.renderStderr seq_
            hFlush config.renderStderr

spinnerLoop :: RenderConfig -> Int -> IO ()
spinnerLoop config frame = do
    threadDelay 80000
    visible <- readIORef config.renderThinkingVisible
    when visible do
        let frames = spinnerFrames
            next = (frame + 1) `mod` length frames
        withMVar config.renderLock \_ -> do
            still <- readIORef config.renderThinkingVisible
            when still do
                -- Ghostty hides OSC 9;4 after ~15s without an update.
                when (next == 0) (emitNativeProgress config True)
                paintThinkingFrame config next
        spinnerLoop config next

paintThinkingFrame :: RenderConfig -> Int -> IO ()
paintThinkingFrame config frame = do
    live <- readIORef config.renderReasoningLive
    if live
        then do
            buffered <- readIORef config.renderReasoningBuffer
            elapsed <- thinkingElapsed config
            redrawLiveReasoning config True elapsed buffered
        else do
            activity <- readIORef config.renderActivityRef
            elapsed <- thinkingElapsed config
            let frames = spinnerFrames
                glyph = frames !! (frame `mod` length frames)
                line =
                    formatActivityLine
                        config.renderColor
                        glyph
                        activity
                        elapsed
            void $ tryIO do
                Text.hPutStr config.renderStderr ("\r\ESC[K" <> line)
                hFlush config.renderStderr

thinkingElapsed :: RenderConfig -> IO Double
thinkingElapsed config = do
    started <- readIORef config.renderStartedAt
    now <- getCurrentTime
    pure $ case started of
        Nothing -> 0
        Just t0 -> realToFrac (diffUTCTime now t0)

appendReasoningUnlocked :: RenderConfig -> Text -> IO ()
appendReasoningUnlocked config delta
    | Text.null delta = pure ()
    | not config.renderShowThinking = pure ()
    | otherwise = do
        modifyIORef' config.renderReasoningBuffer (<> delta)
        buffered <- readIORef config.renderReasoningBuffer
        elapsed <- thinkingElapsed config
        -- First summary token replaces the one-line spinner; later tokens
        -- restyle the live truncated block in place. Elapsed time on the
        -- header updates with each delta rather than a dedicated spinner.
        -- Keep Ghostty's native bar up until the round is committed.
        stopThinkingSpinnerUnlocked config
        emitNativeProgress config True
        redrawLiveReasoning config True elapsed buffered

commitThinkingUnlocked :: RenderConfig -> IO ()
commitThinkingUnlocked config = do
    stopThinkingSpinnerUnlocked config
    emitNativeProgress config False
    buffered <- readIORef config.renderReasoningBuffer
    writeIORef config.renderReasoningBuffer ""
    if Text.null (Text.strip buffered)
        then eraseLiveReasoning config
        else do
            elapsed <- thinkingElapsed config
            let painted =
                    formatThinkingBlock
                        config.renderColor
                        False
                        elapsed
                        buffered
            eraseLiveReasoning config
            Text.hPutStr config.renderStderr painted
            unless (Text.isSuffixOf "\n" painted) do
                Text.hPutStr config.renderStderr "\n"
            hFlush config.renderStderr

redrawLiveReasoning :: RenderConfig -> Bool -> Double -> Text -> IO ()
redrawLiveReasoning config streaming elapsed raw
    | Text.null (Text.strip raw) = pure ()
    | otherwise = do
        let painted = formatThinkingBlock config.renderColor streaming elapsed raw
        eraseLiveReasoning config
        Text.hPutStr config.renderStderr "\ESC7"
        writeIORef config.renderReasoningLive True
        Text.hPutStr config.renderStderr painted
        hFlush config.renderStderr

eraseLiveReasoning :: RenderConfig -> IO ()
eraseLiveReasoning config = do
    live <- readIORef config.renderReasoningLive
    when live do
        Text.hPutStr config.renderStderr $
            "\ESC8"
                <> Text.pack (setCursorColumnCode 0)
                <> Text.pack clearFromCursorToScreenEndCode
        hFlush config.renderStderr
    writeIORef config.renderReasoningLive False

-- | Thinking/reasoning block: accented header plus wrapped summary, matching
-- grok-build's collapsible thought chrome in a linear CLI.
--
-- Live (@streaming@) shows a truncated preview so the block cannot grow
-- without bound while tokens arrive; the committed form keeps the full text
-- wrapped at 'thinkingMaxWidth'.
formatThinkingBlock :: Bool -> Bool -> Double -> Text -> Text
formatThinkingBlock color streaming elapsed raw =
    let header =
            if streaming
                then roleThinking color (glyphThink <> "Thinking…")
                    <> roleMuted color ("  " <> formatElapsed elapsed)
                else
                    roleThinking color (glyphThink <> "Thought")
                        <> roleMuted color (" for " <> formatElapsed elapsed)
        wrapped = wrapThinkingLines thinkingMaxWidth (Text.strip raw)
        preview
            | streaming = take thinkingPreviewLines wrapped
            | otherwise = wrapped
        hidden = length wrapped - length preview
        more
            | streaming && hidden > 0 =
                [roleMuted color ("  … " <> Text.pack (show hidden) <> " more")]
            | otherwise = []
        body =
            map (\line -> roleMuted color (glyphToolAccent <> line)) preview
                <> more
    in Text.intercalate "\n" (header : body)

thinkingPreviewLines :: Int
thinkingPreviewLines = 3

-- | Wrap reasoning text at @width@, splitting on spaces when possible.
wrapThinkingLines :: Int -> Text -> [Text]
wrapThinkingLines width text
    | Text.null text = []
    | otherwise =
        concatMap (wrapOne (max 1 width)) (Text.splitOn "\n" text)

wrapOne :: Int -> Text -> [Text]
wrapOne width line
    | Text.null line = [""]
    | Text.length line <= width = [line]
    | otherwise = go (Text.words line) ""
  where
    go [] acc
        | Text.null acc = []
        | otherwise = [acc]
    go (word : rest) acc
        | Text.null acc && Text.length word > width =
            let (chunk, leftover) = Text.splitAt width word
            in chunk : go (leftover : rest) ""
        | Text.null acc = go rest word
        | Text.length acc + 1 + Text.length word <= width =
            go rest (acc <> " " <> word)
        | otherwise = acc : go (word : rest) ""

-- | One-line live status: spinner, current activity, elapsed time.
formatActivityLine :: Bool -> Text -> Text -> Double -> Text
formatActivityLine color glyph activity seconds =
    roleThinking color (glyph <> " " <> activity)
        <> roleMuted color ("  " <> formatElapsed seconds)

-- | Compact elapsed time: @0.4s@, @12.4s@, @1m20s@.
formatElapsed :: Double -> Text
formatElapsed seconds
    | seconds < 0 = "0.0s"
    | seconds < 60 =
        let tenths = round (seconds * 10) :: Int
            whole = tenths `div` 10
            frac = tenths `mod` 10
        in Text.pack (show whole <> "." <> show frac <> "s")
    | otherwise =
        let total = round seconds :: Int
            m = total `div` 60
            s = total `mod` 60
        in Text.pack (show m <> "m" <> pad2 s <> "s")
  where
    pad2 n
        | n < 10 = "0" <> show n
        | otherwise = show n

formatTurnStatus :: Bool -> Text -> Text -> Text
formatTurnStatus color outcome detail =
    let mark
            | outcome == "ok" = roleSuccess color glyphOk
            | outcome == "cancelled" = roleMuted color glyphCancel
            | otherwise = roleError color glyphErr
        body
            | Text.null detail = outcome
            | otherwise = outcome <> " · " <> detail
    in mark <> roleMuted color body

-- | Write @text@ plus a newline as one 'Text.hPutStr'. @hPutStrLn@ on a
-- 'String' is @hPutStr@ then @hPutChar '\n'@ over a @[Char]@ spine, so
-- concurrent tool threads can interleave characters on the TTY.
--
-- Does not take 'renderLock': callers that also read stdin (approval)
-- must hold that lock themselves. Nested 'withMVar' on the same 'MVar'
-- deadlocks.
putTextLn :: Handle -> Text -> IO ()
putTextLn handle text = do
    Text.hPutStr handle (text <> "\n")
    hFlush handle

summarizeToolCall :: ToolCall -> Text
summarizeToolCall call =
    let verb = toolVerb call.name
        detail = toolDetail call
    in if Text.null detail then verb else verb <> " " <> detail

-- | Colored tool-start line for stderr chrome.
--
-- Known coding tools use English verbs (Read / Listed / $) instead of
-- wire names, matching grok-build's linear chrome while staying Solarized.
formatToolStarted :: Bool -> ToolCall -> Text
formatToolStarted color call =
    let arrow = roleToolArrow color glyphTool
        detail = toolDetail call
    in case toolChrome call.name of
        ToolChromeShell ->
            let prompt = roleMuted color "$ "
                command =
                    if Text.null detail
                        then roleMuted color "…"
                        else roleToolCommand color detail
            in arrow <> prompt <> command
        ToolChrome verb kind ->
            let name = roleToolName color verb
                paintedDetail = case kind of
                    ToolDetailNone -> ""
                    ToolDetailMuted
                        | Text.null detail -> ""
                        | otherwise -> " " <> roleToolDetail color detail
                    ToolDetailPath
                        | Text.null detail -> ""
                        | otherwise -> " " <> roleToolPath color detail
                    ToolDetailCommand
                        | Text.null detail -> ""
                        | otherwise -> " " <> roleToolCommand color detail
            in arrow <> name <> paintedDetail

data ToolChrome
    = ToolChromeShell
    | ToolChrome Text ToolDetailKind

data ToolDetailKind
    = ToolDetailNone
    | ToolDetailMuted
    | ToolDetailPath
    | ToolDetailCommand

toolChrome :: Text -> ToolChrome
toolChrome = \case
    "read_file" -> ToolChrome "Read" ToolDetailPath
    "list_dir" -> ToolChrome "Listed" ToolDetailPath
    "grep" -> ToolChrome "Searched" ToolDetailMuted
    "search_replace" -> ToolChrome "Edited" ToolDetailPath
    "apply_patch" -> ToolChrome "Edited" ToolDetailPath
    "run_terminal_cmd" -> ToolChromeShell
    "shell_command" -> ToolChromeShell
    "run_ghci" -> ToolChromeShell
    "get_task_output" -> ToolChrome "Read" ToolDetailMuted
    "kill_task" -> ToolChrome "Killed" ToolDetailMuted
    "task" -> ToolChrome "Ran" ToolDetailMuted
    "spawn_agent" -> ToolChrome "Spawned" ToolDetailMuted
    "wait_agent" -> ToolChrome "Waited" ToolDetailMuted
    "send_message" -> ToolChrome "Sent" ToolDetailMuted
    "followup_task" -> ToolChrome "Followed up" ToolDetailMuted
    "list_agents" -> ToolChrome "Listed" ToolDetailMuted
    "interrupt_agent" -> ToolChrome "Interrupted" ToolDetailMuted
    "update_plan" -> ToolChrome "Updated" ToolDetailMuted
    "enter_plan_mode" -> ToolChrome "Entered" ToolDetailMuted
    "exit_plan_mode" -> ToolChrome "Exited" ToolDetailMuted
    "ask_user_question" -> ToolChrome "Asked" ToolDetailMuted
    name -> ToolChrome name ToolDetailMuted

toolVerb :: Text -> Text
toolVerb name = case toolChrome name of
    ToolChromeShell -> "$"
    ToolChrome verb _ -> verb

formatToolBody :: Bool -> ToolCall -> Text
formatToolBody color call = case call.name of
    "search_replace" -> formatSearchReplaceDiff color call.arguments
    _ -> ""

-- | Compact unified-diff preview for @search_replace@ arguments.
formatSearchReplaceDiff :: Bool -> Text -> Text
formatSearchReplaceDiff color arguments =
    let path = jsonTextFieldDefault "file_path" arguments
        oldText = jsonTextFieldDefault "old_string" arguments
        newText = jsonTextFieldDefault "new_string" arguments
        header = case (Text.null oldText, Text.null newText) of
            (True, False) ->
                roleMuted color "  create " <> roleToolPath color path
            (False, True) ->
                roleMuted color "  delete " <> roleToolPath color path
            _ -> ""
        oldLines = Text.lines oldText
        newLines = Text.lines newText
        minus = map (\line -> style color [SetRGBColor Foreground solarizedRed] ("  -" <> line)) oldLines
        plus = map (\line -> style color [SetRGBColor Foreground solarizedGreen] ("  +" <> line)) newLines
        raw = minus <> plus
        (shown, hidden)
            | length raw <= 20 = (raw, 0)
            | otherwise = (take 20 raw, length raw - 20)
        more =
            if hidden == 0
                then []
                else [roleMuted color ("  … " <> Text.pack (show hidden) <> " more")]
        body = shown <> more
    in Text.intercalate "\n" (filter (not . Text.null) (header : body))

toolDetail :: ToolCall -> Text
toolDetail call = case call.name of
    "read_file" -> jsonTextFieldDefault "target_file" call.arguments
    "list_dir" -> jsonTextFieldDefault "target_directory" call.arguments
    "search_replace" -> jsonTextFieldDefault "file_path" call.arguments
    "grep" -> jsonTextFieldDefault "pattern" call.arguments
    "run_terminal_cmd" -> firstLine (jsonTextFieldDefault "command" call.arguments)
    "run_ghci" -> firstLine (jsonTextFieldDefault "expression" call.arguments)
    "shell_command" -> firstLine (jsonTextFieldDefault "command" call.arguments)
    "apply_patch" -> fromMaybe "patch" (firstPatchPath call.arguments)
    "update_plan" -> "plan"
    "enter_plan_mode" -> "enter"
    "exit_plan_mode" -> "exit"
    "ask_user_question" -> firstLine (jsonTextFieldDefault "question" call.arguments)
    _ -> ""

firstPatchPath :: Text -> Maybe Text
firstPatchPath patch =
    case
        [ Text.drop (Text.length prefix) line
        | line <- Text.lines patch
        , prefix <- ["*** Add File: ", "*** Update File: ", "*** Delete File: "]
        , prefix `Text.isPrefixOf` line
        ] of
        (path : _) | not (Text.null path) -> Just path
        _ -> Nothing

truncateToolOutput :: Text -> Text
truncateToolOutput output =
    let stripped = Text.strip output
        lines_ = take 8 (filter (not . Text.null) (Text.lines stripped))
        rest = length (filter (not . Text.null) (Text.lines stripped)) - length lines_
        paint line =
            let shortened
                    | Text.length line <= 160 = line
                    | otherwise = Text.take 157 line <> "..."
            in glyphToolAccent <> glyphToolOut <> shortened
    in if Text.null stripped
        then glyphToolAccent <> glyphToolOut <> "(empty)"
        else
            Text.intercalate "\n" (map paint lines_)
                <> if rest > 0
                    then "\n" <> glyphToolAccent <> glyphToolOut <> "… " <> Text.pack (show rest) <> " more"
                    else ""

firstLine :: Text -> Text
firstLine = Text.takeWhile (/= '\n')

formatLoopError :: LoopError -> Text
formatLoopError = formatLoopErrorColored False

-- | Like 'formatLoopError', with optional ANSI styling for TTY stderr.
formatLoopErrorColored :: Bool -> LoopError -> Text
formatLoopErrorColored color = \case
    LoopTransport err ->
        roleError color (glyphErr <> "transport error: " <> Text.pack (show err))
    LoopMaxTurns turn ->
        roleError color (glyphErr <> "stopped: max turns reached")
            <> maybe "" (\text -> "\n" <> text) turn.assistantText
    LoopNoResponseId ->
        roleError color (glyphErr <> "transport error: response had no id")
    LoopCancelled _ ->
        roleMuted color (glyphCancel <> "cancelled")
