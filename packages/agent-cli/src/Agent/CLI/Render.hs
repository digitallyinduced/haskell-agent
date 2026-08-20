-- | Stream renderer and mutating-tool approval prompts.
module Agent.CLI.Render
    ( RenderConfig(..)
    , clearThinking
    , formatActivityLine
    , formatElapsed
    , formatLoopError
    , formatLoopErrorColored
    , formatSearchReplaceDiff
    , formatToolStarted
    , formatTurnStatus
    , putTextLn
    , renderAssistantText
    , renderEvent
    , summarizeToolCall
    , truncateToolOutput
    , visibleDisplayRows
    ) where

import Agent.CLI.Markdown (renderMarkdown)
import Agent.CLI.Style
    ( agentBackground
    , glyphCancel
    , glyphErr
    , glyphOk
    , glyphTool
    , glyphToolAccent
    , glyphToolOut
    , paintBackgroundLines
    , roleError
    , roleMuted
    , roleSuccess
    , roleThinking
    , roleToolArrow
    , roleToolDetail
    , roleToolName
    , roleToolOutput
    , solarizedGreen
    , solarizedRed
    , spinnerFrames
    , style
    )
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
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.IO as Text
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import System.Console.ANSI (ConsoleLayer(..), SGR(..), getTerminalSize)
import System.Console.ANSI.Codes
    ( clearFromCursorToScreenEndCode
    , cursorUpLineCode
    , setCursorColumnCode
    )
import System.IO (Handle, hFlush)

data RenderConfig = RenderConfig
    { renderShowThinking :: !Bool
    , renderThinkingVisible :: !(IORef Bool)
    , renderThinkingSpinner :: !(IORef (Maybe ThreadId))
    , renderColor :: !Bool
    , renderPrintedText :: !(IORef Bool)
    , renderTextBuffer :: !(IORef Text)
    -- | Display rows occupied by the last live color-mode redraw of the
    -- assistant buffer. Used to move the cursor back before restyling.
    , renderLiveRows :: !(IORef Int)
    -- | Whether the last live paint ended with a newline (cursor sits on the
    -- following empty row rather than at the end of the last content row).
    , renderLiveEndsWithNewline :: !(IORef Bool)
    , renderLock :: !(MVar ())
    , renderStdout :: !Handle
    , renderStderr :: !Handle
    , renderModelRef :: !(IORef Text)
    , renderActivityRef :: !(IORef Text)
    , renderStartedAt :: !(IORef (Maybe UTCTime))
    }

renderEvent :: RenderConfig -> LoopEvent -> IO ()
renderEvent config event =
    withMVar config.renderLock \_ -> renderEventUnlocked config event

renderEventUnlocked :: RenderConfig -> LoopEvent -> IO ()
renderEventUnlocked config = \case
    TextDelta delta ->
        if config.renderColor
            then do
                clearThinkingUnlocked config
                modifyIORef' config.renderTextBuffer (<> delta)
                buffered <- readIORef config.renderTextBuffer
                redrawLiveAssistant config buffered
            else do
                clearThinkingUnlocked config
                writeIORef config.renderPrintedText True
                Text.hPutStr config.renderStdout delta
                hFlush config.renderStdout
    ReasoningDelta _ -> pure ()
    TurnStarted -> do
        writeIORef config.renderTextBuffer ""
        writeIORef config.renderLiveRows 0
        writeIORef config.renderLiveEndsWithNewline False
        writeIORef config.renderActivityRef "thinking…"
        now <- getCurrentTime
        writeIORef config.renderStartedAt (Just now)
        startThinkingSpinnerUnlocked config
    -- Finalize live color output (or paint once for non-streaming backends).
    -- Pre-tool prose ("I'll check…") is shown before tool lines; the final
    -- tool-free turn is the main answer.
    TurnFinished turn -> do
        clearThinkingUnlocked config
        when config.renderColor do
            didPrint <- finalizeAssistantBuffer config turn.assistantText
            when (didPrint && not (null turn.toolCalls)) do
                putTextLn config.renderStdout ""
    ToolStarted call -> do
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
redrawLiveAssistant :: RenderConfig -> Text -> IO ()
redrawLiveAssistant config raw
    | Text.null raw = pure ()
    | otherwise = do
        width <- terminalWidth
        let painted = renderAssistantText True raw
            rows = visibleDisplayRows width painted
            endsNL = Text.isSuffixOf "\n" painted
        eraseLiveAssistant config
        writeIORef config.renderPrintedText True
        writeIORef config.renderLiveRows rows
        writeIORef config.renderLiveEndsWithNewline endsNL
        Text.hPutStr config.renderStdout painted
        hFlush config.renderStdout

-- | End-of-turn: keep live paint when deltas already drew; otherwise paint
-- once from the buffer or completed 'assistantText' (non-streaming backends).
-- Returns whether anything was written.
finalizeAssistantBuffer :: RenderConfig -> Maybe Text -> IO Bool
finalizeAssistantBuffer config assistantText = do
    buffered <- readIORef config.renderTextBuffer
    writeIORef config.renderTextBuffer ""
    liveRows <- readIORef config.renderLiveRows
    writeIORef config.renderLiveRows 0
    writeIORef config.renderLiveEndsWithNewline False
    if liveRows > 0
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
    rows <- readIORef config.renderLiveRows
    endsNL <- readIORef config.renderLiveEndsWithNewline
    when (rows > 0) (eraseRows config rows endsNL)
    writeIORef config.renderLiveRows 0
    writeIORef config.renderLiveEndsWithNewline False

eraseRows :: RenderConfig -> Int -> Bool -> IO ()
eraseRows config rows endsWithNewline
    | rows <= 0 = pure ()
    | otherwise = do
        -- After a trailing newline the cursor sits on the empty row below
        -- the content, so move up @rows@; otherwise @rows - 1@.
        let upCount
                | endsWithNewline = rows
                | rows > 1 = rows - 1
                | otherwise = 0
            up = if upCount > 0 then Text.pack (cursorUpLineCode upCount) else ""
            home = Text.pack (setCursorColumnCode 1)
            clear = Text.pack clearFromCursorToScreenEndCode
        Text.hPutStr config.renderStdout (up <> home <> clear)
        hFlush config.renderStdout

terminalWidth :: IO Int
terminalWidth = do
    -- getTerminalSize may probe stdin; non-TTY / EOF (unit tests) → 80.
    result <- tryIO getTerminalSize
    pure $ case result of
        Right (Just (_height, width)) | width > 0 -> width
        _ -> 80

-- | How many terminal rows a painted assistant block occupies, accounting for
-- soft wrap at @width@. ANSI/OSC sequences do not consume columns.
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
                in max 1 ((cols + width' - 1) `div` width')
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
            case Text.breakOn "\ESC\\" (Text.drop 5 t) of
                (_params, rest)
                    | Text.isPrefixOf "\ESC\\" rest -> go (Text.drop 2 rest)
                    | otherwise ->
                        case Text.break (== '\a') (Text.drop 5 t) of
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
clearThinking :: RenderConfig -> IO ()
clearThinking config =
    withMVar config.renderLock \_ -> clearThinkingUnlocked config

startThinkingSpinnerUnlocked :: RenderConfig -> IO ()
startThinkingSpinnerUnlocked config
    | not config.renderShowThinking = pure ()
    | otherwise = do
        visible <- readIORef config.renderThinkingVisible
        if visible
            then paintThinkingFrame config 0
            else do
                writeIORef config.renderThinkingVisible True
                paintThinkingFrame config 0
                tid <- forkIO (spinnerLoop config 0)
                writeIORef config.renderThinkingSpinner (Just tid)

clearThinkingUnlocked :: RenderConfig -> IO ()
clearThinkingUnlocked config = do
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

spinnerLoop :: RenderConfig -> Int -> IO ()
spinnerLoop config frame = do
    threadDelay 80000
    visible <- readIORef config.renderThinkingVisible
    when visible do
        let frames = spinnerFrames
            next = (frame + 1) `mod` length frames
        withMVar config.renderLock \_ -> do
            still <- readIORef config.renderThinkingVisible
            when still (paintThinkingFrame config next)
        spinnerLoop config next

paintThinkingFrame :: RenderConfig -> Int -> IO ()
paintThinkingFrame config frame = do
    activity <- readIORef config.renderActivityRef
    started <- readIORef config.renderStartedAt
    now <- getCurrentTime
    let seconds = case started of
            Nothing -> 0
            Just t0 -> realToFrac (diffUTCTime now t0)
        frames = spinnerFrames
        glyph = frames !! (frame `mod` length frames)
        line =
            formatActivityLine
                config.renderColor
                glyph
                activity
                seconds
    void $ tryIO do
        Text.hPutStr config.renderStderr ("\r\ESC[K" <> line)
        hFlush config.renderStderr

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
    let detail = toolDetail call
    in if Text.null detail then call.name else call.name <> " " <> detail

-- | Colored tool-start line for stderr chrome.
formatToolStarted :: Bool -> ToolCall -> Text
formatToolStarted color call =
    let detail = toolDetail call
        arrow = roleToolArrow color glyphTool
        name = roleToolName color call.name
    in if Text.null detail
        then arrow <> name
        else arrow <> name <> " " <> roleToolDetail color detail

formatToolBody :: Bool -> ToolCall -> Text
formatToolBody color call = case call.name of
    "search_replace" -> formatSearchReplaceDiff color call.arguments
    _ -> ""

-- | Compact unified-diff preview for @search_replace@ arguments.
formatSearchReplaceDiff :: Bool -> Text -> Text
formatSearchReplaceDiff color arguments =
    let path = jsonField "file_path" arguments
        oldText = jsonField "old_string" arguments
        newText = jsonField "new_string" arguments
        header = case (Text.null oldText, Text.null newText) of
            (True, False) -> roleMuted color ("  create " <> path)
            (False, True) -> roleMuted color ("  delete " <> path)
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
    "read_file" -> jsonField "target_file" call.arguments
    "list_dir" -> jsonField "target_directory" call.arguments
    "search_replace" -> jsonField "file_path" call.arguments
    "grep" -> jsonField "pattern" call.arguments
    "run_terminal_cmd" -> firstLine (jsonField "command" call.arguments)
    "run_ghci" -> firstLine (jsonField "expression" call.arguments)
    "shell_command" -> firstLine (jsonField "command" call.arguments)
    "apply_patch" -> fromMaybe "patch" (firstPatchPath call.arguments)
    "update_plan" -> "plan"
    "enter_plan_mode" -> "enter"
    "exit_plan_mode" -> "exit"
    "ask_user_question" -> firstLine (jsonField "question" call.arguments)
    _ -> ""

jsonField :: Text -> Text -> Text
jsonField key arguments = case Aeson.decodeStrict (TextEncoding.encodeUtf8 arguments) of
    Just (Aeson.Object object) -> case KeyMap.lookup (Key.fromText key) object of
        Just (Aeson.String value) -> value
        _ -> ""
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
