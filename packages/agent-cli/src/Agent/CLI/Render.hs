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
    , formatActivityLine
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
    , formatTurnStatus
    , putTextLn
    , renderAssistantText
    , renderEvent
    , renderPrintedText
    , resetRenderPrintedText
    , setRenderActivity
    , streamMarkdown
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
import Agent.TUI.FencedCode
    ( FenceMarker
    , fenceOpener
    , isFenceCloser
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
    , glyphWarn
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
    , roleWarn
    , terminalGreen
    , terminalRed
    , motionGlyphSet
    , style
    )
import Agent.Loop (LoopError(..), LoopEvent(..), TurnOutput(..))
import Agent.TUI.Presentation
    ( SearchReplaceAction(..)
    , SearchReplaceDiff(..)
    , SearchReplaceLine(..)
    , formatToolOutput
    , parseSearchReplaceDiff
    , summarizeToolCall
    , toolDetail
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
import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, withMVar)
import Control.Exception.Safe (tryIO)
import Control.Monad (unless, void, when)
import Data.Char (isDigit, isSpace)
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
import Agent.TUI.TextWidth (splitTerminalGraphemeSuffix)
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
        }

data MarkdownStreamState = MarkdownStreamState
    { pending :: !Text
    , context :: !(Maybe Char)
    , streamMode :: !MarkdownStreamMode
    , blockPending :: !Text
    }

data MarkdownStreamMode
    = StreamLineStart
    | StreamProse
    | StreamFence !FenceMarker
    | StreamTableCandidate
    | StreamTable

emptyMarkdownStreamState :: MarkdownStreamState
emptyMarkdownStreamState =
    MarkdownStreamState "" Nothing StreamLineStart ""

feedMarkdownStream
    :: MarkdownStreamState
    -> Text
    -> (MarkdownStreamState, Text)
feedMarkdownStream state input = case state.streamMode of
    StreamLineStart -> feedLineStart state input
    StreamProse -> feedProse state input
    StreamFence marker -> feedFence marker state input
    StreamTableCandidate -> feedTableCandidate state input
    StreamTable -> feedTable state input

feedLineStart
    :: MarkdownStreamState
    -> Text
    -> (MarkdownStreamState, Text)
feedLineStart state input =
    let buffered = state.blockPending <> input
    in case takeCompleteLine buffered of
        Just (line, rest) ->
            classifyCompleteLine state{blockPending = ""} line rest
        Nothing
            | lineNeedsLookahead buffered ->
                (state{blockPending = buffered}, "")
            | otherwise ->
                feedProse
                    state
                        { streamMode = StreamProse
                        , blockPending = ""
                        }
                    buffered

classifyCompleteLine
    :: MarkdownStreamState
    -> Text
    -> Text
    -> (MarkdownStreamState, Text)
classifyCompleteLine state line rest
    | Just (marker, _) <- fenceOpener (dropLineEnding line) =
        feedMarkdownStream
            state
                { streamMode = StreamFence marker
                , blockPending = line
                }
            rest
    | isPossibleTableHeader line =
        feedMarkdownStream
            state
                { streamMode = StreamTableCandidate
                , blockPending = line
                }
            rest
    | lineIsBlock line =
        let (nextState, output) =
                feedMarkdownStream
                    state
                        { streamMode = StreamLineStart
                        , blockPending = ""
                        }
                    rest
        in (nextState, renderMarkdown True line <> output)
    | otherwise =
        feedProse
            state
                { streamMode = StreamProse
                , blockPending = ""
                }
            (line <> rest)

feedProse
    :: MarkdownStreamState
    -> Text
    -> (MarkdownStreamState, Text)
feedProse state input =
    case Text.breakOn "\n" input of
        (linePart, rest)
            | Text.null rest ->
                let source = state.pending <> linePart
                    (parsedReady, parsedPending, parsedContext) =
                        splitMarkdownFragment state.context source
                    (stablePrefix, graphemePending) =
                        splitTerminalGraphemeSuffix parsedReady
                    (ready, reparsedPending, nextContext)
                        | Text.null graphemePending =
                            (parsedReady, "", parsedContext)
                        | otherwise =
                            splitMarkdownFragment
                                state.context
                                stablePrefix
                    pending' =
                        reparsedPending
                            <> graphemePending
                            <> parsedPending
                in ( state
                        { pending = pending'
                        , context = nextContext
                        , streamMode = StreamProse
                        }
                   , renderMarkdownFragment True state.context ready
                   )
            | otherwise ->
                let source = state.pending <> linePart <> "\n"
                    (ready, pending', _) =
                        splitMarkdownFragment state.context source
                    rendered =
                        renderMarkdownFragment True state.context
                            (ready <> pending')
                    reset =
                        state
                            { pending = ""
                            , context = Nothing
                            , streamMode = StreamLineStart
                            , blockPending = ""
                            }
                    (nextState, following) =
                        feedMarkdownStream reset (Text.drop 1 rest)
                in (nextState, rendered <> following)

feedFence
    :: FenceMarker
    -> MarkdownStreamState
    -> Text
    -> (MarkdownStreamState, Text)
feedFence marker state input =
    let buffered = state.blockPending <> input
        (lines_, partial) = completeLines buffered
        (beforeCloser, closingAndAfter) =
            break (isFenceCloser marker . dropLineEnding) (drop 1 lines_)
    in case closingAndAfter of
        [] -> (state{blockPending = buffered}, "")
        closing : after ->
            let block = Text.concat (take 1 lines_ <> beforeCloser <> [closing])
                rest = Text.concat after <> partial
                reset =
                    state
                        { streamMode = StreamLineStart
                        , blockPending = ""
                        , pending = ""
                        , context = Nothing
                        }
                (nextState, following) = feedMarkdownStream reset rest
            in (nextState, renderMarkdown True block <> following)

feedTableCandidate
    :: MarkdownStreamState
    -> Text
    -> (MarkdownStreamState, Text)
feedTableCandidate state input =
    let buffered = state.blockPending <> input
        (lines_, partial) = completeLines buffered
    in case lines_ of
        header : separator : after
            | isTableSeparator separator ->
                feedMarkdownStream
                    state
                        { streamMode = StreamTable
                        , blockPending = header <> separator
                        }
                    (Text.concat after <> partial)
            | otherwise ->
                let reset =
                        state
                            { streamMode = StreamLineStart
                            , blockPending = ""
                            }
                    (nextState, following) =
                        feedMarkdownStream reset
                            (separator <> Text.concat after <> partial)
                in (nextState, renderMarkdown True header <> following)
        _ -> (state{blockPending = buffered}, "")

feedTable
    :: MarkdownStreamState
    -> Text
    -> (MarkdownStreamState, Text)
feedTable state input =
    let buffered = state.blockPending <> input
        (lines_, partial) = completeLines buffered
        (tableLines, after) =
            case lines_ of
                header : separator : rows ->
                    let (body, following) =
                            span isPossibleTableHeader rows
                    in (header : separator : body, following)
                _ -> (lines_, [])
    in case after of
        [] -> (state{blockPending = buffered}, "")
        line : rest ->
                let table = Text.concat tableLines
                    reset =
                        state
                            { streamMode = StreamLineStart
                            , blockPending = ""
                            }
                    (nextState, following) =
                        feedMarkdownStream reset
                            (line <> Text.concat rest <> partial)
                in (nextState, renderMarkdown True table <> following)

flushMarkdownStream :: MarkdownStreamState -> Text
flushMarkdownStream state = case state.streamMode of
    StreamProse ->
        renderMarkdownFragment True state.context state.pending
    StreamLineStart ->
        renderMarkdown True state.blockPending
    StreamFence _ ->
        renderMarkdown True state.blockPending
    StreamTableCandidate ->
        renderMarkdown True state.blockPending
    StreamTable ->
        renderMarkdown True state.blockPending

takeCompleteLine :: Text -> Maybe (Text, Text)
takeCompleteLine text =
    case Text.breakOn "\n" text of
        (_, rest) | Text.null rest -> Nothing
        (line, rest) -> Just (line <> "\n", Text.drop 1 rest)

completeLines :: Text -> ([Text], Text)
completeLines = go []
  where
    go reversed remaining =
        case takeCompleteLine remaining of
            Nothing -> (reverse reversed, remaining)
            Just (line, rest) -> go (line : reversed) rest

dropLineEnding :: Text -> Text
dropLineEnding = Text.dropWhileEnd (== '\n')

lineNeedsLookahead :: Text -> Bool
lineNeedsLookahead line =
    let stripped = Text.dropWhile isSpace line
        markerRun marker = Text.span (== marker) stripped
        allMarkerOrSpace marker =
            Text.all (\character -> character == marker || isSpace character)
                stripped
    in case Text.uncons stripped of
        Nothing -> True
        Just ('#', _) ->
            let (marks, after) = markerRun '#'
            in Text.length marks <= 6
                && (Text.null after || Text.isPrefixOf " " after)
        Just ('>', _) -> True
        Just ('|', _) -> True
        Just ('`', _) ->
            let (ticks, after) = markerRun '`'
            in Text.null after || Text.length ticks >= 3
        Just ('~', _) ->
            let (tildes, after) = markerRun '~'
            in Text.null after || Text.length tildes >= 3
        Just ('+', after) -> Text.null after || Text.isPrefixOf " " after
        Just ('*', after) ->
            Text.null after
                || Text.isPrefixOf " " after
                || allMarkerOrSpace '*'
        Just ('-', after) ->
            Text.null after
                || Text.isPrefixOf " " after
                || allMarkerOrSpace '-'
        Just ('_', _) -> allMarkerOrSpace '_'
        Just (character, _)
            | isDigit character ->
                let (digits, after) = Text.span isDigit stripped
                in not (Text.null digits)
                    && ( Text.null after
                        || after == "."
                        || Text.isPrefixOf ". " after
                       )
        _ -> False

lineIsBlock :: Text -> Bool
lineIsBlock line =
    let stripped = Text.dropWhile isSpace (dropLineEnding line)
        (marks, afterHeading) = Text.span (== '#') stripped
        heading =
            not (Text.null marks)
                && Text.length marks <= 6
                && Text.isPrefixOf " " afterHeading
        quote = Text.isPrefixOf ">" stripped
        bullet = any (`Text.isPrefixOf` stripped) ["- ", "* ", "+ "]
        (digits, orderedRest) = Text.span isDigit stripped
        ordered =
            not (Text.null digits) && Text.isPrefixOf ". " orderedRest
        thematic marker =
            let compact = Text.filter (not . isSpace) stripped
            in Text.length compact >= 3 && Text.all (== marker) compact
    in heading
        || quote
        || bullet
        || ordered
        || thematic '-'
        || thematic '*'
        || thematic '_'

isPossibleTableHeader :: Text -> Bool
isPossibleTableHeader =
    Text.isPrefixOf "|" . Text.dropWhile isSpace . dropLineEnding

isTableSeparator :: Text -> Bool
isTableSeparator line =
    let stripped =
            Text.dropWhile (== '|')
                (Text.dropWhileEnd (== '|')
                    (Text.strip (dropLineEnding line)))
        cells = map Text.strip (Text.splitOn "|" stripped)
        valid cell =
            Text.any (== '-') cell
                && Text.null
                    (Text.filter (`notElem` ['-', ':', ' ']) cell)
    in length cells >= 1 && all valid cells

-- | Grok-build @max_thoughts_width@: wrap reasoning display at this column.
thinkingMaxWidth :: Int
thinkingMaxWidth = 120

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
        }

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
        commitThinkingUnlocked config
        if config.renderColor
            then do
                streamAssistantDelta config delta
            else do
                modifyRenderState config \state ->
                    (state{statePrintedText = True}, ())
                Text.hPutStr config.renderStdout delta
                hFlush config.renderStdout
    ReasoningDelta delta ->
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
        modifyRenderState config \state ->
            (setRenderActivity "Retrying response…" state, ())
        putTextLn config.renderStderr
            (roleWarn config.renderColor (glyphWarn <> message))
        startThinkingSpinnerUnlocked config
    TurnStarted -> do
        now <- getCurrentTime
        modifyRenderState config \state ->
            (beginRenderTurn now state, ())
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
        modifyRenderState config \state ->
            ( state
                { stateToolCalls = Map.insert call.callId call state.stateToolCalls
                , stateActivity = summarizeToolCall call
                }
            , ()
            )
        unless (isTodoTool call.name) do
            putTextLn config.renderStderr (formatToolStarted config.renderColor call)
            let extra = formatToolBody config.renderColor call
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
                (`formatToolOutput` result.output)
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
    activity <- (.stateActivity) <$> readRenderState config
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
    "write_stdin" -> ToolChrome "Continued" ToolDetailMuted
    "run_ghci" -> ToolChromeShell
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
formatToolBody color call = case canonicalToolName call.name of
    "search_replace" -> formatSearchReplaceDiff color call.arguments
    _ -> ""

-- | Compact unified-diff preview for @search_replace@ arguments.
formatSearchReplaceDiff :: Bool -> Text -> Text
formatSearchReplaceDiff color arguments =
    let SearchReplaceDiff { diffPath, diffAction, diffLines, diffHiddenLines } =
            parseSearchReplaceDiff arguments
        header = case diffAction of
            Just SearchReplaceCreate ->
                roleMuted color "  create " <> renderToolPath color diffPath
            Just SearchReplaceDelete ->
                roleMuted color "  delete " <> renderToolPath color diffPath
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

renderToolPath :: Bool -> Text -> Text
renderToolPath color path =
    let styled = roleToolPath color path
    in if "/" `Text.isPrefixOf` path
        then osc8Link color (fileUri (Text.unpack path)) styled
        else styled

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

formatInterruptedResponse :: Text -> Text
formatInterruptedResponse details =
    "Response interrupted after partial output.\n"
        <> "The turn has stopped; nothing is still running.\n"
        <> details
        <> "\nSend \"continue\" to continue the task, or retry your last message."
