-- | Stream renderer and mutating-tool approval prompts.
module Agent.CLI.Render
    ( MarkdownStreamState
    , RenderConfig(..)
    , clearThinking
    , commitThinking
    , emptyMarkdownStreamState
    , formatActivityLine
    , formatElapsed
    , formatLoopError
    , formatLoopErrorAt
    , formatLoopErrorColored
    , formatLoopErrorColoredAt
    , formatLoopErrorPersistedAt
    , formatSearchReplaceDiff
    , formatThinkingBlock
    , formatToolOutput
    , formatToolStarted
    , formatTurnStatus
    , putTextLn
    , renderAssistantText
    , renderEvent
    , summarizeToolCall
    , thinkingMaxWidth
    , truncateToolOutput
    , wrapThinkingLines
    ) where

import Agent.CLI.Markdown
    ( renderMarkdown
    , renderMarkdownFragment
    , splitMarkdownFragment
    )
import Agent.CLI.Progress
    ( osc9ProgressIndeterminate
    , osc9ProgressRemove
    , wrapOscForTmux
    )
import Agent.CLI.Terminal (fileUri)
import Agent.CLI.Error
    ( formatApiError
    , formatApiErrorAt
    , formatApiErrorPersistedAt
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
    , osc8Link
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
    , motionGlyphSet
    , style
    )
import Agent.JsonText (jsonTextField, jsonTextFieldDefault)
import Agent.Loop (LoopError(..), LoopEvent(..), TurnOutput(..))
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallResult(..)
    , canonicalToolName
    )
import Agent.TextBuffer
    ( TextBuffer
    , appendTextBuffer
    , emptyTextBuffer
    , textBufferToText
    )
import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, withMVar)
import Control.Exception.Safe (tryIO)
import Control.Monad (unless, void, when)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Foldable as Foldable
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.IO as Text
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Agent.TUI.Motion
    ( MotionDemand(..)
    , MotionMode
    , foregroundIndicator
    , motionIntervalMicros
    , nativeProgressAnimationEnabled
    )
import System.Console.ANSI (ConsoleLayer(..), SGR(..))
import System.Environment (lookupEnv)
import System.IO (Handle, hFlush)

data RenderConfig = RenderConfig
    { renderShowThinking :: !Bool
    , renderThinkingVisible :: !(IORef Bool)
    , renderThinkingSpinner :: !(IORef (Maybe ThreadId))
    -- | Accumulated reasoning-summary text for the current model round.
    , renderReasoningBuffer :: !(IORef TextBuffer)
    , renderColor :: !Bool
    , renderPrintedText :: !(IORef Bool)
    , renderMarkdownState :: !(IORef MarkdownStreamState)
    -- | True after assistant text has been streamed for the current round.
    , renderLiveActive :: !(IORef Bool)
    , renderLock :: !(MVar ())
    , renderStdout :: !Handle
    , renderStderr :: !Handle
    , renderModelRef :: !(IORef Text)
    , renderActivityRef :: !(IORef Text)
    , renderStartedAt :: !(IORef (Maybe UTCTime))
    , renderToolCalls :: !(IORef (Map.Map Text ToolCall))
    , renderNativeProgress :: !Bool -- ^ Ghostty / WT OSC 9;4; off in tests
    , renderMotionMode :: !MotionMode
    }

data MarkdownStreamState = MarkdownStreamState
    { pending :: !Text
    , context :: !(Maybe Char)
    }

emptyMarkdownStreamState :: MarkdownStreamState
emptyMarkdownStreamState = MarkdownStreamState "" Nothing

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
                streamAssistantDelta config delta
            else do
                writeIORef config.renderPrintedText True
                Text.hPutStr config.renderStdout delta
                hFlush config.renderStdout
    ReasoningDelta delta ->
        appendReasoningUnlocked config delta
    ActivityUpdated activity -> do
        writeIORef config.renderActivityRef activity
        visible <- readIORef config.renderThinkingVisible
        if visible
            then paintThinkingFrame config
            else if config.renderShowThinking
                then startThinkingSpinnerUnlocked config
                else putTextLn config.renderStderr (roleMuted config.renderColor activity)
    TurnStarted -> do
        writeIORef config.renderMarkdownState emptyMarkdownStreamState
        writeIORef config.renderLiveActive False
        writeIORef config.renderReasoningBuffer emptyTextBuffer
        writeIORef config.renderActivityRef "Thinking…"
        writeIORef config.renderToolCalls Map.empty
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
        modifyIORef' config.renderToolCalls (Map.insert call.callId call)
        writeIORef config.renderActivityRef (summarizeToolCall call)
        putTextLn config.renderStderr (formatToolStarted config.renderColor call)
        let extra = formatToolBody config.renderColor call
        unless (Text.null extra) do
            putTextLn config.renderStderr extra
        when config.renderShowThinking do
            visible <- readIORef config.renderThinkingVisible
            if visible
                then paintThinkingFrame config
                else startThinkingSpinnerUnlocked config
    -- The append-only renderer cannot safely repaint accumulated snapshots
    -- without duplicating output in terminal scrollback. The retained TUI
    -- handles these updates; minimal mode prints the final ToolFinished result.
    ToolOutputUpdated _callId _output ->
        pure ()
    ToolFinished result -> do
        calls <- readIORef config.renderToolCalls
        modifyIORef' config.renderToolCalls (Map.delete result.callId)
        let output = maybe result.output
                (`formatToolOutput` result.output)
                (Map.lookup result.callId calls)
        putTextLn config.renderStderr
            (roleToolOutput config.renderColor (truncateToolOutput output))

-- | Style assistant markdown when color is enabled; otherwise return plain text.
-- Color mode also paints each line with 'agentBackground'.
renderAssistantText :: Bool -> Text -> Text
renderAssistantText color text =
    paintBackgroundLines color agentBackground (renderMarkdown color text)

-- | Stream assistant text append-only while buffering incomplete inline
-- markdown constructs. Once a closing delimiter arrives, the whole construct
-- is emitted with styling and neither delimiter reaches the terminal.
--
-- Repainting the full accumulated response with DECSC/DECRC looks correct
-- inside the current viewport, but once a repaint scrolls the terminal the
-- previous frame has already entered scrollback and cannot be erased. Long
-- responses therefore appeared there many times.
streamAssistantDelta :: RenderConfig -> Text -> IO ()
streamAssistantDelta config delta
    | Text.null delta = pure ()
    | otherwise = do
        let safe = Text.filter (/= '\ESC') delta
        (ready, context) <-
            atomicModifyIORef' config.renderMarkdownState \state ->
                let (ready, pending', nextContext) =
                        splitMarkdownFragment state.context (state.pending <> safe)
                    state' = MarkdownStreamState pending' nextContext
                in (state', (ready, state.context))
        unless (Text.null ready) do
            writeIORef config.renderPrintedText True
            writeIORef config.renderLiveActive True
            Text.hPutStr config.renderStdout
                ( paintBackgroundLines True agentBackground
                    (renderMarkdownFragment True context ready)
                )
            hFlush config.renderStdout

-- | End-of-turn: keep live paint when deltas already drew; otherwise paint
-- once from the pending fragment or completed 'assistantText'
-- (non-streaming backends).
-- Returns whether anything was written.
finalizeAssistantBuffer :: RenderConfig -> Maybe Text -> IO Bool
finalizeAssistantBuffer config assistantText = do
    (pending, context) <-
        atomicModifyIORef' config.renderMarkdownState \state ->
            (emptyMarkdownStreamState, (state.pending, state.context))
    live <- readIORef config.renderLiveActive
    writeIORef config.renderLiveActive False
    if live
        then do
            writeIORef config.renderPrintedText True
            unless (Text.null pending) do
                Text.hPutStr config.renderStdout
                    ( paintBackgroundLines True agentBackground
                        (renderMarkdownFragment True context pending)
                    )
                hFlush config.renderStdout
            pure True
        else do
            let raw
                    | not (Text.null pending) = pending
                    | otherwise = fromMaybe "" assistantText
            if Text.null raw
                then pure False
                else do
                    writeIORef config.renderPrintedText True
                    Text.hPutStr config.renderStdout (renderAssistantText True raw)
                    hFlush config.renderStdout
                    pure True

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
        visible <- readIORef config.renderThinkingVisible
        if visible
            then paintThinkingFrame config
            else do
                writeIORef config.renderThinkingVisible True
                paintThinkingFrame config
                started <- getMonotonicTimeNSec
                tid <- forkIO (spinnerLoop config started 0)
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
    | not config.renderNativeProgress
        || not (nativeProgressAnimationEnabled config.renderMotionMode) =
        pure ()
    | otherwise = do
        inTmux <- isJust <$> lookupEnv "TMUX"
        let seq_ =
                wrapOscForTmux inTmux $
                    if active then osc9ProgressIndeterminate else osc9ProgressRemove
        void $ tryIO do
            Text.hPutStr config.renderStderr seq_
            hFlush config.renderStderr

spinnerLoop :: RenderConfig -> Word64 -> Int -> IO ()
spinnerLoop config startedAt lastKeepaliveBucket = do
    threadDelay
        (motionIntervalMicros config.renderMotionMode MotionFast)
    visible <- readIORef config.renderThinkingVisible
    when visible do
        now <- getMonotonicTimeNSec
        let elapsedMillis =
                fromIntegral ((now - startedAt) `div` 1000000)
            keepaliveBucket = elapsedMillis `div` 5000
        withMVar config.renderLock \_ -> do
            still <- readIORef config.renderThinkingVisible
            when still do
                -- Ghostty hides OSC 9;4 after ~15s without an update.
                when (keepaliveBucket > lastKeepaliveBucket) $
                    emitNativeProgress config True
                paintThinkingFrameAt config (monotonicMillis now)
        spinnerLoop config startedAt keepaliveBucket

paintThinkingFrame :: RenderConfig -> IO ()
paintThinkingFrame config = do
    now <- getMonotonicTimeNSec
    paintThinkingFrameAt config (monotonicMillis now)

paintThinkingFrameAt :: RenderConfig -> Int -> IO ()
paintThinkingFrameAt config motionMillis = do
    activity <- readIORef config.renderActivityRef
    elapsed <- thinkingElapsed config
    let glyph =
            foregroundIndicator
                motionGlyphSet
                config.renderMotionMode
                motionMillis
        line =
            formatActivityLine
                config.renderColor
                glyph
                activity
                elapsed
    void $ tryIO do
        Text.hPutStr config.renderStderr ("\r\ESC[K" <> line)
        hFlush config.renderStderr

monotonicMillis :: Word64 -> Int
monotonicMillis now =
    fromIntegral (now `div` 1000000)

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
    | otherwise =
        -- Keep the one-line spinner while buffering. Repainting even a bounded
        -- multi-line preview can scroll old frames into terminal scrollback.
        modifyIORef' config.renderReasoningBuffer
            (appendTextBuffer delta)

commitThinkingUnlocked :: RenderConfig -> IO ()
commitThinkingUnlocked config = do
    stopThinkingSpinnerUnlocked config
    emitNativeProgress config False
    buffered <- textBufferToText
        <$> readIORef config.renderReasoningBuffer
    writeIORef config.renderReasoningBuffer emptyTextBuffer
    if Text.null (Text.strip buffered)
        then pure ()
        else do
            elapsed <- thinkingElapsed config
            let painted =
                    formatThinkingBlock
                        config.renderColor
                        False
                        elapsed
                        buffered
            Text.hPutStr config.renderStderr painted
            unless (Text.isSuffixOf "\n" painted) do
                Text.hPutStr config.renderStderr "\n"
            hFlush config.renderStderr

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
                        | otherwise -> " " <> renderToolPath color detail
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
toolChrome name = case canonicalToolName name of
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
    "spawn_agent" -> ToolChrome "Spawned agent" ToolDetailMuted
    "wait_agent" -> ToolChrome "Waited for agent updates" ToolDetailMuted
    "send_message" -> ToolChrome "Sent message to" ToolDetailMuted
    "followup_task" -> ToolChrome "Followed up with" ToolDetailMuted
    "list_agents" -> ToolChrome "Listed agents" ToolDetailMuted
    "interrupt_agent" -> ToolChrome "Interrupted" ToolDetailMuted
    "update_plan" -> ToolChrome "Updated" ToolDetailMuted
    "enter_plan_mode" -> ToolChrome "Entered" ToolDetailMuted
    "exit_plan_mode" -> ToolChrome "Exited" ToolDetailMuted
    "ask_user_question" -> ToolChrome "Asked" ToolDetailMuted
    _ -> ToolChrome name ToolDetailMuted

toolVerb :: Text -> Text
toolVerb name = case toolChrome name of
    ToolChromeShell -> "$"
    ToolChrome verb _ -> verb

formatToolBody :: Bool -> ToolCall -> Text
formatToolBody color call = case canonicalToolName call.name of
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
                roleMuted color "  create " <> renderToolPath color path
            (False, True) ->
                roleMuted color "  delete " <> renderToolPath color path
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

renderToolPath :: Bool -> Text -> Text
renderToolPath color path =
    let styled = roleToolPath color path
    in if "/" `Text.isPrefixOf` path
        then osc8Link color (fileUri (Text.unpack path)) styled
        else styled

toolDetail :: ToolCall -> Text
toolDetail call = case canonicalToolName call.name of
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
    "spawn_agent" -> jsonTextFieldDefault "task_name" call.arguments
    "send_message" -> jsonTextFieldDefault "target" call.arguments
    "followup_task" -> jsonTextFieldDefault "target" call.arguments
    "interrupt_agent" -> jsonTextFieldDefault "target" call.arguments
    "list_agents" ->
        maybe "" ("under " <>) (nonEmptyJsonText "path_prefix" call.arguments)
    _ -> ""

-- | Turn structured collaboration results into compact terminal text. The
-- provider still receives the original JSON result; this is display-only.
formatToolOutput :: ToolCall -> Text -> Text
formatToolOutput call output = case canonicalToolName call.name of
    "spawn_agent" ->
        maybe output ("Agent: " <>) (nonEmptyJsonText "task_name" output)
    "wait_agent" ->
        fromMaybe output (nonEmptyJsonText "message" output)
    "list_agents" ->
        fromMaybe output (formatAgentList output)
    "interrupt_agent" ->
        maybe output ("Previous status: " <>)
            (nonEmptyJsonText "previous_status" output)
    _ -> output

nonEmptyJsonText :: Text -> Text -> Maybe Text
nonEmptyJsonText key input = jsonTextField key input >>= \value ->
    let stripped = Text.strip value
    in if Text.null stripped then Nothing else Just stripped

formatAgentList :: Text -> Maybe Text
formatAgentList output = do
    Aeson.Object object <- Aeson.decodeStrict (TextEncoding.encodeUtf8 output)
    Aeson.Array agents <- KeyMap.lookup (Key.fromText "agents") object
    let rows = mapMaybeAgent (Foldable.toList agents)
    pure $ case rows of
        [] -> "(no live agents)"
        _ -> Text.intercalate "\n" rows
  where
    mapMaybeAgent = foldr
        (\value rest ->
            case value of
                Aeson.Object agent ->
                    case
                        ( jsonObjectText "agent_name" agent
                        , jsonObjectText "agent_status" agent
                        ) of
                        (Just name, Just status) ->
                            (name <> " · " <> status) : rest
                        (Just name, Nothing) -> name : rest
                        _ -> rest
                _ -> rest)
        []

jsonObjectText :: Text -> Aeson.Object -> Maybe Text
jsonObjectText key object =
    case KeyMap.lookup (Key.fromText key) object of
        Just (Aeson.String value)
            | not (Text.null (Text.strip value)) -> Just (Text.strip value)
        _ -> Nothing

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

-- | Format a loop error relative to the time it is shown.
formatLoopErrorAt :: UTCTime -> LoopError -> Text
formatLoopErrorAt = formatLoopErrorColoredAt False

-- | Like 'formatLoopError', with optional ANSI styling for TTY stderr.
formatLoopErrorColored :: Bool -> LoopError -> Text
formatLoopErrorColored color =
    formatLoopErrorColoredMaybeAt color Nothing

-- | Like 'formatLoopErrorAt', with optional ANSI styling for TTY stderr.
formatLoopErrorColoredAt :: Bool -> UTCTime -> LoopError -> Text
formatLoopErrorColoredAt color now =
    formatLoopErrorColoredMaybeAt color (Just now)

-- | Stable, unstyled text for session persistence. Retry timestamps stay
-- absolute so resumed sessions never show stale relative guidance.
formatLoopErrorPersistedAt :: UTCTime -> LoopError -> Text
formatLoopErrorPersistedAt now = \case
    LoopTransport err -> formatApiErrorPersistedAt now err
    LoopMaxTurns turn ->
        "Stopped: maximum turns reached."
            <> maybe "" ("\n" <>) turn.assistantText
    LoopNoResponseId ->
        "Provider returned an incomplete response.\nRetry the message."
    LoopCancelled _ ->
        "Cancelled."

formatLoopErrorColoredMaybeAt :: Bool -> Maybe UTCTime -> LoopError -> Text
formatLoopErrorColoredMaybeAt color maybeNow = \case
    LoopTransport err ->
        roleError color $
            glyphErr
                <> case maybeNow of
                    Nothing -> formatApiError err
                    Just now -> formatApiErrorAt now err
    LoopMaxTurns turn ->
        roleError color (glyphErr <> "stopped: max turns reached")
            <> maybe "" (\text -> "\n" <> text) turn.assistantText
    LoopNoResponseId ->
        roleError color
            (glyphErr
                <> "Provider returned an incomplete response.\n"
                <> "Retry the message.")
    LoopCancelled _ ->
        roleMuted color (glyphCancel <> "cancelled")
