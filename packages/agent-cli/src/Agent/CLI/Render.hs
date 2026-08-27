-- | Stream renderer and mutating-tool approval prompts.
module Agent.CLI.Render
    ( MarkdownStreamState
    , RenderState(..)
    , stateActivity
    , stateLiveActive
    , stateMarkdownState
    , statePrintedText
    , stateReasoningBuffer
    , stateStartedAt
    , stateThinkingVisible
    , stateToolCalls
    , RenderConfig(..)
    , clearThinking
    , commitThinking
    , emptyMarkdownStreamState
    , emptyRenderState
    , appendRenderReasoning
    , beginRenderTurn
    , clearRenderTokenRate
    , countGenerationChars
    , formatActivityLine
    , recordRenderTurnRate
    , renderTokensPerSecond
    , resetRenderGeneration
    , stateLastTokensPerSecond
    , formatElapsed
    , formatLoopError
    , formatLoopErrorAt
    , formatLoopErrorColored
    , formatLoopErrorColoredAt
    , formatLoopErrorPersistedAt
    , formatSearchReplaceDiff
    , formatThinkingBlock
    , formatToolBody
    , formatToolOutput
    , formatToolStarted
    , formatToolStartedRelative
    , formatTurnStatus
    , putTextLn
    , renderAssistantText
    , renderEvent
    , renderPrintedText
    , resetRenderPrintedText
    , setRenderActivity
    , streamMarkdown
    , summarizeToolCall
    , summarizeToolCallRelative
    , thinkingMaxWidth
    , truncateToolOutput
    , wrapThinkingLines
    ) where

import Agent.CLI.Markdown
    ( renderMarkdown
    )
import Agent.CLI.Render.MarkdownStream
    ( MarkdownStreamState
    , emptyMarkdownStreamState
    , feedMarkdownStream
    , flushMarkdownStream
    )
import Agent.CLI.Render.Status
    ( formatActivityLine
    , formatElapsed
    , formatThinkingBlock
    , formatTurnStatus
    , thinkingMaxWidth
    , wrapThinkingLines
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
    , glyphTool
    , glyphToolAccent
    , glyphToolOut
    , glyphWarn
    , paintBackgroundLines
    , osc8Link
    , roleError
    , roleMuted
    , roleToolArrow
    , roleToolCommand
    , roleToolDetail
    , roleToolName
    , roleToolOutput
    , roleToolPath
    , roleWarn
    , terminalGreen
    , terminalRed
    , motionGlyphSet
    , style
    )
import Agent.Loop
    ( LoopError(..)
    , LoopEvent(..)
    , TokenUsage(..)
    , TurnCompletion(..)
    , TurnOutput(..)
    , generationTokensPerSecond
    , liveTokensPerSecond
    )
import Agent.TUI.Presentation
    ( SearchReplaceAction(..)
    , SearchReplaceDiff(..)
    , SearchReplaceLine(..)
    , formatToolOutput
    , formatToolOutputRelative
    , parseSearchReplaceDiff
    , summarizeToolCall
    , summarizeToolCallRelative
    , toolDetail
    , workspaceRelativeDisplayPath
    )
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
import Control.Applicative ((<|>))
import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, withMVar)
import Control.Exception.Safe (tryIO)
import Control.Monad (unless, void, when)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
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
import System.Environment (lookupEnv)
import System.IO (Handle, hFlush)

data RenderConfig = RenderConfig
    { renderShowThinking :: !Bool
    , renderThinkingSpinner :: !(IORef (Maybe ThreadId))
    , renderState :: !(IORef RenderState)
    , renderColor :: !Bool
    , renderLock :: !(MVar ())
    , renderStdout :: !Handle
    , renderStderr :: !Handle
    , renderModelRef :: !(IORef Text)
    , renderNativeProgress :: !Bool -- ^ Ghostty / WT OSC 9;4; off in tests
    , renderMotionMode :: !MotionMode
    , renderWorkspace :: !Text
    }

-- | Immutable logical renderer state. IO effects (terminal output and the
-- spinner thread) remain in 'RenderConfig'; this value is replaced atomically
-- under 'renderLock' by the event handlers.
data RenderState = RenderState
    { stateThinkingVisible :: !Bool
    , stateReasoningBuffer :: !TextBuffer
    , statePrintedText :: !Bool
    , stateMarkdownState :: !MarkdownStreamState
    , stateLiveActive :: !Bool
    , stateActivity :: !Text
    , stateStartedAt :: !(Maybe UTCTime)
    , stateToolCalls :: !(Map.Map Text ToolCall)
    , stateGenerationChars :: !Int
    , stateGenerationStartedAt :: !(Maybe UTCTime)
    , stateLastTokensPerSecond :: !(Maybe Double)
    }

stateThinkingVisible :: RenderState -> Bool
stateThinkingVisible state = state.stateThinkingVisible

stateReasoningBuffer :: RenderState -> TextBuffer
stateReasoningBuffer state = state.stateReasoningBuffer

statePrintedText :: RenderState -> Bool
statePrintedText state = state.statePrintedText

stateMarkdownState :: RenderState -> MarkdownStreamState
stateMarkdownState state = state.stateMarkdownState

stateLiveActive :: RenderState -> Bool
stateLiveActive state = state.stateLiveActive

stateActivity :: RenderState -> Text
stateActivity state = state.stateActivity

stateStartedAt :: RenderState -> Maybe UTCTime
stateStartedAt state = state.stateStartedAt

stateToolCalls :: RenderState -> Map.Map Text ToolCall
stateToolCalls state = state.stateToolCalls

stateLastTokensPerSecond :: RenderState -> Maybe Double
stateLastTokensPerSecond state = state.stateLastTokensPerSecond

emptyRenderState :: RenderState
emptyRenderState =
    RenderState
        { stateThinkingVisible = False
        , stateReasoningBuffer = emptyTextBuffer
        , statePrintedText = False
        , stateMarkdownState = emptyMarkdownStreamState
        , stateLiveActive = False
        , stateActivity = "Thinking…"
        , stateStartedAt = Nothing
        , stateToolCalls = Map.empty
        , stateGenerationChars = 0
        , stateGenerationStartedAt = Nothing
        , stateLastTokensPerSecond = Nothing
        }

modifyRenderState
    :: RenderConfig
    -> (RenderState -> (RenderState, a))
    -> IO a
modifyRenderState config transition =
    atomicModifyIORef' config.renderState transition

readRenderState :: RenderConfig -> IO RenderState
readRenderState config = readIORef config.renderState

renderPrintedText :: RenderConfig -> IO Bool
renderPrintedText config =
    (.statePrintedText) <$> readRenderState config

resetRenderPrintedText :: RenderConfig -> IO ()
resetRenderPrintedText config =
    modifyRenderState config \state ->
        (state{statePrintedText = False}, ())

setRenderActivity :: Text -> RenderState -> RenderState
setRenderActivity activity state = state{stateActivity = activity}

beginRenderTurn :: UTCTime -> RenderState -> RenderState
beginRenderTurn now state =
    emptyRenderState
        { stateThinkingVisible = state.stateThinkingVisible
        , stateStartedAt = Just now
        , stateGenerationStartedAt = Just now
        , stateLastTokensPerSecond = state.stateLastTokensPerSecond
        }

-- | Start a new generation-rate window without resetting the turn timer
-- shown on the thinking spinner.
resetRenderGeneration :: UTCTime -> RenderState -> RenderState
resetRenderGeneration now state =
    state
        { stateGenerationChars = 0
        , stateGenerationStartedAt = Just now
        }

-- | Drop a previous conversation's saved speed after /clear or /new.
clearRenderTokenRate :: RenderState -> RenderState
clearRenderTokenRate state =
    state
        { stateGenerationChars = 0
        , stateGenerationStartedAt = Nothing
        , stateLastTokensPerSecond = Nothing
        }

countGenerationChars :: Text -> RenderState -> RenderState
countGenerationChars delta state =
    state
        { stateGenerationChars =
            state.stateGenerationChars + Text.length delta
        }

generationElapsedMillis :: UTCTime -> RenderState -> Int
generationElapsedMillis now state =
    case state.stateGenerationStartedAt of
        Nothing -> 0
        Just started ->
            max 0 (floor (diffUTCTime now started * 1000))

recordRenderTurnRate :: UTCTime -> TurnOutput -> RenderState -> RenderState
recordRenderTurnRate now turn state =
    state
        { stateLastTokensPerSecond =
            generationTokensPerSecond
                turn.tokenUsage.outputTokens
                state.stateGenerationChars
                (generationElapsedMillis now state)
                <|> state.stateLastTokensPerSecond
        }

renderTokensPerSecond :: UTCTime -> RenderState -> Maybe Double
renderTokensPerSecond now state
    | Map.null state.stateToolCalls =
        liveTokensPerSecond
            state.stateGenerationChars
            (generationElapsedMillis now state)
            <|> state.stateLastTokensPerSecond
    | otherwise = state.stateLastTokensPerSecond

appendRenderReasoning :: Text -> RenderState -> RenderState
appendRenderReasoning delta state =
    state{stateReasoningBuffer = appendTextBuffer delta state.stateReasoningBuffer}

streamMarkdown :: Text -> RenderState -> (RenderState, Text)
streamMarkdown input state =
    let (markdown', output) =
            feedMarkdownStream state.stateMarkdownState input
    in (state{stateMarkdownState = markdown'}, output)

renderEvent :: RenderConfig -> LoopEvent -> IO ()
renderEvent config event =
    withMVar config.renderLock \_ -> renderEventUnlocked config event

renderEventUnlocked :: RenderConfig -> LoopEvent -> IO ()
renderEventUnlocked config = \case
    TextDelta delta -> do
        modifyRenderState config \state ->
            (countGenerationChars delta state, ())
        commitThinkingUnlocked config
        if config.renderColor
            then do
                streamAssistantDelta config delta
            else do
                modifyRenderState config \state ->
                    (state{statePrintedText = True}, ())
                Text.hPutStr config.renderStdout delta
                hFlush config.renderStdout
    ReasoningDelta delta -> do
        modifyRenderState config \state ->
            (countGenerationChars delta state, ())
        appendReasoningUnlocked config delta
    ActivityUpdated activity -> do
        modifyRenderState config \state ->
            (setRenderActivity activity state, ())
        visible <- (.stateThinkingVisible) <$> readRenderState config
        if visible
            then paintThinkingFrame config
            else if config.renderShowThinking
                then startThinkingSpinnerUnlocked config
                else putTextLn config.renderStderr (roleMuted config.renderColor activity)
    WarningRaised warning -> do
        visible <- (.stateThinkingVisible) <$> readRenderState config
        when visible (stopThinkingSpinnerUnlocked config)
        putTextLn config.renderStderr
            (roleWarn config.renderColor (glyphWarn <> warning))
        when visible (startThinkingSpinnerUnlocked config)
    ResponseRestarted message -> do
        commitThinkingUnlocked config
        if config.renderColor
            then do
                didPrint <- finalizeAssistantBuffer config Nothing
                when didPrint (putTextLn config.renderStdout "")
            else do
                Text.hPutStr config.renderStdout "\n"
                hFlush config.renderStdout
                modifyRenderState config \state ->
                    (state
                        { stateMarkdownState = emptyMarkdownStreamState
                        , stateLiveActive = False
                        }
                    , ())
        now <- getCurrentTime
        modifyRenderState config \state ->
            ( setRenderActivity
                "Retrying response…"
                (resetRenderGeneration now state)
            , ()
            )
        putTextLn config.renderStderr
            (roleWarn config.renderColor (glyphWarn <> message))
        startThinkingSpinnerUnlocked config
    TurnStarted -> do
        -- A later sample (tool follow-up or empty reasoning continuation)
        -- must commit any buffered thought before resetting render state.
        -- Otherwise beginRenderTurn discards the summary without painting it.
        buffered <-
            textBufferToText . stateReasoningBuffer
                <$> readRenderState config
        unless (Text.null (Text.strip buffered)) $
            commitThinkingUnlocked config
        now <- getCurrentTime
        modifyRenderState config \state ->
            (beginRenderTurn now state, ())
        startThinkingSpinnerUnlocked config
    -- Finalize live color output (or paint once for non-streaming backends).
    -- Pre-tool prose ("I'll check…") is shown before tool lines; the final
    -- tool-free turn is the main answer.
    TurnFinished turn -> do
        now <- getCurrentTime
        modifyRenderState config \state ->
            (recordRenderTurnRate now turn state, ())
        commitThinkingUnlocked config
        when config.renderColor do
            didPrint <- finalizeAssistantBuffer config turn.assistantText
            when (didPrint && not (null turn.toolCalls)) do
                putTextLn config.renderStdout ""
    ToolStarted call -> do
        commitThinkingUnlocked config
        modifyRenderState config \state ->
            ( state
                { stateToolCalls = Map.insert call.callId call state.stateToolCalls
                , stateActivity =
                    summarizeToolCallRelative config.renderWorkspace call
                }
            , ()
            )
        unless (isTodoTool call.name) do
            putTextLn config.renderStderr
                (formatToolStartedRelative
                    config.renderColor
                    config.renderWorkspace
                    call)
            let extra =
                    formatToolBodyRelative
                        config.renderColor
                        config.renderWorkspace
                        call
            unless (Text.null extra) do
                putTextLn config.renderStderr extra
        when config.renderShowThinking do
            visible <- (.stateThinkingVisible) <$> readRenderState config
            if visible
                then paintThinkingFrame config
                else startThinkingSpinnerUnlocked config
    -- The append-only renderer cannot safely repaint accumulated snapshots
    -- without duplicating output in terminal scrollback. The retained TUI
    -- handles these updates; minimal mode prints the final ToolFinished result.
    ToolOutputUpdated _callId _output ->
        pure ()
    ToolFinished result -> do
        calls <- modifyRenderState config \state ->
            ( state{stateToolCalls = Map.delete result.callId state.stateToolCalls}
            , state.stateToolCalls
            )
        let maybeCall = Map.lookup result.callId calls
            formatted = maybe result.output
                (\call ->
                    formatToolOutputRelative
                        config.renderWorkspace
                        call
                        result.output)
                maybeCall
            painted = case maybeCall of
                Just call
                    | isTodoTool call.name -> Nothing
                _ ->
                    Just
                        (roleToolOutput
                            config.renderColor
                            (truncateToolOutput formatted))
        case painted of
            Nothing -> pure ()
            Just line -> putTextLn config.renderStderr line
    ToolUpdated _ ->
        pure ()
    ToolRetracted _ ->
        pure ()
    ResponseAttemptDiscarded ->
        pure ()
    NativeAgentStarted{} ->
        pure ()
    NativeAgentOutput{} ->
        pure ()
    NativeAgentFinished{} ->
        pure ()

-- | Style assistant markdown when color is enabled; otherwise return plain text.
-- The terminal theme owns the default assistant background.
renderAssistantText :: Bool -> Text -> Text
renderAssistantText color text =
    paintBackgroundLines color agentBackground (renderMarkdown color text)

-- | Stream assistant text append-only. Ordinary prose is emitted as soon as
-- incomplete inline constructs permit, while line prefixes that may introduce
-- blocks are held until they can be classified. Fences and tables are buffered
-- until their extent is known, so no raw markers need to be repainted.
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
        ready <-
            modifyRenderState config (streamMarkdown safe)
        unless (Text.null ready) do
            modifyRenderState config \state ->
                (state{statePrintedText = True, stateLiveActive = True}, ())
            Text.hPutStr config.renderStdout
                ( paintBackgroundLines True agentBackground
                    ready
                )
            hFlush config.renderStdout

-- | End-of-turn: keep live paint when deltas already drew; otherwise paint
-- once from the pending fragment or completed 'assistantText'
-- (non-streaming backends).
-- Returns whether anything was written.
finalizeAssistantBuffer :: RenderConfig -> Maybe Text -> IO Bool
finalizeAssistantBuffer config assistantText = do
    (pendingOutput, live) <-
        modifyRenderState config \state ->
            ( state
                { stateMarkdownState = emptyMarkdownStreamState
                , stateLiveActive = False
                }
            , ( flushMarkdownStream state.stateMarkdownState
              , state.stateLiveActive
              )
            )
    if live
        then do
            modifyRenderState config \state ->
                (state{statePrintedText = True}, ())
            unless (Text.null pendingOutput) do
                Text.hPutStr config.renderStdout
                    ( paintBackgroundLines True agentBackground
                        pendingOutput
                    )
                hFlush config.renderStdout
            pure True
        else do
            let raw
                    | not (Text.null pendingOutput) = pendingOutput
                    | otherwise = fromMaybe "" assistantText
            if Text.null raw
                then pure False
                else do
                    modifyRenderState config \state ->
                        (state{statePrintedText = True}, ())
                    Text.hPutStr config.renderStdout $
                        if Text.null pendingOutput
                            then renderAssistantText True raw
                            else paintBackgroundLines
                                True agentBackground pendingOutput
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
        visible <- (.stateThinkingVisible) <$> readRenderState config
        if visible
            then paintThinkingFrame config
            else do
                modifyRenderState config \state ->
                    (state{stateThinkingVisible = True}, ())
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
    visible <- (.stateThinkingVisible) <$> readRenderState config
    when visible do
        void $ tryIO do
            Text.hPutStr config.renderStderr "\r\ESC[K"
            hFlush config.renderStderr
        modifyRenderState config \state ->
            (state{stateThinkingVisible = False}, ())

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
    visible <- (.stateThinkingVisible) <$> readRenderState config
    when visible do
        now <- getMonotonicTimeNSec
        let elapsedMillis =
                fromIntegral ((now - startedAt) `div` 1000000)
            keepaliveBucket = elapsedMillis `div` 5000
        withMVar config.renderLock \_ -> do
            still <- (.stateThinkingVisible) <$> readRenderState config
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
    state <- readRenderState config
    now <- getCurrentTime
    let activity = state.stateActivity
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
                (renderTokensPerSecond now state)
    void $ tryIO do
        Text.hPutStr config.renderStderr ("\r\ESC[K" <> line)
        hFlush config.renderStderr

monotonicMillis :: Word64 -> Int
monotonicMillis now =
    fromIntegral (now `div` 1000000)

thinkingElapsed :: RenderConfig -> IO Double
thinkingElapsed config = do
    started <- (.stateStartedAt) <$> readRenderState config
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
        modifyRenderState config \state ->
            (appendRenderReasoning delta state, ())

commitThinkingUnlocked :: RenderConfig -> IO ()
commitThinkingUnlocked config = do
    stopThinkingSpinnerUnlocked config
    emitNativeProgress config False
    buffered <- modifyRenderState config \state ->
        (state{stateReasoningBuffer = emptyTextBuffer}
        , textBufferToText state.stateReasoningBuffer)
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

-- | Colored tool-start line for stderr chrome.
--
-- Known coding tools use English verbs (Read / Listed / $) instead of
-- wire names, matching grok-build's linear chrome while staying Solarized.
formatToolStarted :: Bool -> ToolCall -> Text
formatToolStarted color = formatToolStartedRelative color ""

formatToolStartedRelative :: Bool -> Text -> ToolCall -> Text
formatToolStartedRelative color workspace call =
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
                        | otherwise ->
                            " " <> renderToolPath color workspace detail
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
    "write_stdin" -> ToolChrome "Continued" ToolDetailMuted
    "run_ghci" -> ToolChromeShell
    "exec" -> ToolChrome "$ exec" ToolDetailNone
    "get_task_output" -> ToolChrome "Read" ToolDetailMuted
    "wait_tasks" -> ToolChrome "Waited" ToolDetailMuted
    "kill_task" -> ToolChrome "Killed" ToolDetailMuted
    "task" -> ToolChrome "Ran" ToolDetailMuted
    "spawn_agent" -> ToolChrome "Spawned agent" ToolDetailMuted
    "wait_agent" -> ToolChrome "Waited for agent updates" ToolDetailMuted
    "send_message" -> ToolChrome "Sent message to" ToolDetailMuted
    "followup_task" -> ToolChrome "Followed up with" ToolDetailMuted
    "list_agents" -> ToolChrome "Listed agents" ToolDetailMuted
    "interrupt_agent" -> ToolChrome "Interrupted" ToolDetailMuted
    "todo_write" -> ToolChrome "todo_write" ToolDetailNone
    "update_plan" -> ToolChrome "update_plan" ToolDetailNone
    "enter_plan_mode" -> ToolChrome "Entered" ToolDetailMuted
    "exit_plan_mode" -> ToolChrome "Exited" ToolDetailMuted
    "ask_user_question" -> ToolChrome "Asked" ToolDetailMuted
    "skill_search" -> ToolChrome "Searched skills" ToolDetailMuted
    "view_skill" -> ToolChrome "Viewed skill" ToolDetailMuted
    "skill_create" -> ToolChrome "Learned" ToolDetailMuted
    "skill_update" -> ToolChrome "Updated skill" ToolDetailMuted
    "skill_archive" -> ToolChrome "Archived skill" ToolDetailMuted
    "skill_rollback" -> ToolChrome "Restored skill" ToolDetailMuted
    _ -> ToolChrome name ToolDetailMuted

isTodoTool :: Text -> Bool
isTodoTool name =
    canonicalToolName name `elem` ["todo_write", "update_plan"]

formatToolBody :: Bool -> ToolCall -> Text
formatToolBody color = formatToolBodyRelative color ""

formatToolBodyRelative :: Bool -> Text -> ToolCall -> Text
formatToolBodyRelative color workspace call = case canonicalToolName call.name of
    "search_replace" ->
        formatSearchReplaceDiffRelative color workspace call.arguments
    "exec" -> roleToolCommand color call.arguments
    _ -> ""

-- | Compact unified-diff preview for @search_replace@ arguments.
formatSearchReplaceDiff :: Bool -> Text -> Text
formatSearchReplaceDiff color = formatSearchReplaceDiffRelative color ""

formatSearchReplaceDiffRelative :: Bool -> Text -> Text -> Text
formatSearchReplaceDiffRelative color workspace arguments =
    let SearchReplaceDiff { diffPath, diffAction, diffLines, diffHiddenLines } =
            parseSearchReplaceDiff arguments
        header = case diffAction of
            Just SearchReplaceCreate ->
                roleMuted color "  create "
                    <> renderToolPath color workspace diffPath
            Just SearchReplaceDelete ->
                roleMuted color "  delete "
                    <> renderToolPath color workspace diffPath
            _ -> ""
        shown = map paintLine diffLines
        more =
            if diffHiddenLines == 0
                then []
                else
                    [ roleMuted color
                        ("  … " <> Text.pack (show diffHiddenLines) <> " more")
                    ]
        body = shown <> more
    in Text.intercalate "\n" (filter (not . Text.null) (header : body))
  where
    paintLine = \case
        SearchReplaceRemoved line ->
            style color [terminalRed] ("  -" <> line)
        SearchReplaceAdded line ->
            style color [terminalGreen] ("  +" <> line)

renderToolPath :: Bool -> Text -> Text -> Text
renderToolPath color workspace path =
    let displayed = workspaceRelativeDisplayPath workspace path
        styled = roleToolPath color displayed
        absolute = absoluteToolPath workspace path
    in if "/" `Text.isPrefixOf` absolute
        then osc8Link color (fileUri (Text.unpack absolute)) styled
        else styled

absoluteToolPath :: Text -> Text -> Text
absoluteToolPath workspace path
    | "/" `Text.isPrefixOf` path = path
    | Text.null root = path
    | path == "." = root
    | otherwise = root <> "/" <> Text.dropWhile (== '/') path
  where
    root = Text.dropWhileEnd (== '/') workspace

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
    LoopTransportAfterOutput err ->
        formatInterruptedResponse (formatApiErrorPersistedAt now err)
    LoopMaxTurns turn ->
        "Stopped: maximum turns reached."
            <> maybe "" ("\n" <>) turn.assistantText
    LoopIncomplete turn ->
        formatIncompleteResponse turn
    LoopNoResponseId ->
        "Provider returned an incomplete response.\nRetry the message."
    LoopUnexpected message ->
        "Unexpected agent error: " <> message <> "\nRetry the message."
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
    LoopTransportAfterOutput err ->
        roleError color $
            glyphErr
                <> formatInterruptedResponse
                    (case maybeNow of
                        Nothing -> formatApiError err
                        Just now -> formatApiErrorAt now err)
    LoopMaxTurns turn ->
        roleError color (glyphErr <> "stopped: max turns reached")
            <> maybe "" (\text -> "\n" <> text) turn.assistantText
    LoopIncomplete turn ->
        roleError color (glyphErr <> formatIncompleteResponse turn)
    LoopNoResponseId ->
        roleError color
            (glyphErr
                <> "Provider returned an incomplete response.\n"
                <> "Retry the message.")
    LoopUnexpected message ->
        roleError color
            (glyphErr
                <> "Unexpected agent error: "
                <> message
                <> "\nRetry the message.")
    LoopCancelled _ ->
        roleMuted color (glyphCancel <> "cancelled")

formatIncompleteResponse :: TurnOutput -> Text
formatIncompleteResponse turn =
    "Response incomplete: "
        <> reason
        <> "."
        <> tokenDetails
        <> "\nUse /retry to retry the same message and attachments."
  where
    (reason, reasoningTokens) = case turn.completion of
        TurnIncomplete incompleteReason incompleteReasoningTokens ->
            (incompleteReason, incompleteReasoningTokens)
        TurnCompleted -> ("unknown", Nothing)
    outputTokens = turn.tokenUsage.outputTokens
    tokenDetails
        | outputTokens <= 0 && reasoningTokens == Nothing = ""
        | otherwise =
            "\nProvider reported "
                <> Text.pack (show outputTokens)
                <> " output tokens"
                <> maybe ""
                    (\tokens ->
                        " (" <> Text.pack (show tokens) <> " reasoning tokens)")
                    reasoningTokens
                <> "."

formatInterruptedResponse :: Text -> Text
formatInterruptedResponse details =
    "Response interrupted after partial output.\n"
        <> "The turn has stopped; nothing is still running.\n"
        <> details
        <> "\nSend \"continue\" to continue the task, or retry your last message."
