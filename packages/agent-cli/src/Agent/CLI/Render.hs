-- | Stream renderer and mutating-tool approval prompts.
module Agent.CLI.Render
    ( RenderConfig(..)
    , clearThinking
    , formatLoopError
    , formatLoopErrorColored
    , formatToolStarted
    , putTextLn
    , renderAssistantText
    , renderEvent
    , summarizeToolCall
    , truncateToolOutput
    ) where

import Agent.CLI.Markdown (renderMarkdown)
import Agent.CLI.Style
    ( agentBackground
    , paintBackgroundLines
    , roleError
    , roleThinking
    , roleToolArrow
    , roleToolDetail
    , roleToolName
    , roleToolOutput
    )
import Agent.Loop (LoopError(..), LoopEvent(..), TurnOutput(..))
import Agent.ToolDispatch (ToolCall(..), ToolCallResult(..))
import Control.Concurrent.MVar (MVar, withMVar)
import Control.Monad (when)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.IO as Text
import System.IO (Handle, hFlush)

data RenderConfig = RenderConfig
    { renderShowThinking :: !Bool
      -- | True while the static "thinking…" status line is on stderr.
    , renderThinkingVisible :: !(IORef Bool)
    , renderColor :: !Bool
    , renderPrintedText :: !(IORef Bool)
    , renderTextBuffer :: !(IORef Text)
    , renderLock :: !(MVar ())
    , renderStdout :: !Handle
    , renderStderr :: !Handle
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
            else do
                clearThinkingUnlocked config
                writeIORef config.renderPrintedText True
                Text.hPutStr config.renderStdout delta
                hFlush config.renderStdout
    ReasoningDelta _ -> pure ()
    TurnStarted -> do
        writeIORef config.renderTextBuffer ""
        showThinkingUnlocked config
    -- Flush styled text on every completed turn. Pre-tool prose ("I'll check…")
    -- is shown before tool lines; the final tool-free turn is the main answer.
    TurnFinished turn -> do
        clearThinkingUnlocked config
        when config.renderColor do
            didPrint <- flushAssistantBuffer config turn.assistantText
            when (didPrint && not (null turn.toolCalls)) do
                putTextLn config.renderStdout ""
    ToolStarted call -> do
        clearThinkingUnlocked config
        putTextLn config.renderStderr (formatToolStarted config.renderColor call)
    ToolFinished result ->
        putTextLn config.renderStderr
            (roleToolOutput config.renderColor (truncateToolOutput result.output))

-- | Style assistant markdown when color is enabled; otherwise return plain text.
-- Color mode also paints each line with 'agentBackground'.
renderAssistantText :: Bool -> Text -> Text
renderAssistantText color text =
    paintBackgroundLines color agentBackground (renderMarkdown color text)

-- | End-of-turn flush for color mode: prefer streamed deltas, else the
-- completed 'assistantText' from non-streaming backends.
-- Returns whether anything was written.
flushAssistantBuffer :: RenderConfig -> Maybe Text -> IO Bool
flushAssistantBuffer config assistantText = do
    buffered <- readIORef config.renderTextBuffer
    writeIORef config.renderTextBuffer ""
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

-- | Clear a leftover thinking status line. Safe to call when none is visible.
clearThinking :: RenderConfig -> IO ()
clearThinking config =
    withMVar config.renderLock \_ -> clearThinkingUnlocked config

showThinkingUnlocked :: RenderConfig -> IO ()
showThinkingUnlocked config
    | not config.renderShowThinking = pure ()
    | otherwise = do
        visible <- readIORef config.renderThinkingVisible
        if visible
            then pure ()
            else do
                Text.hPutStr config.renderStderr
                    (roleThinking config.renderColor "thinking…")
                hFlush config.renderStderr
                writeIORef config.renderThinkingVisible True

clearThinkingUnlocked :: RenderConfig -> IO ()
clearThinkingUnlocked config = do
    visible <- readIORef config.renderThinkingVisible
    if not visible
        then pure ()
        else do
            Text.hPutStr config.renderStderr "\r\ESC[K"
            hFlush config.renderStderr
            writeIORef config.renderThinkingVisible False

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
        arrow = roleToolArrow color "→ "
        name = roleToolName color call.name
    in if Text.null detail
        then arrow <> name
        else arrow <> name <> " " <> roleToolDetail color detail

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
    let line = firstLine (Text.strip output)
        shortened
            | Text.length line <= 160 = line
            | otherwise = Text.take 157 line <> "..."
    in if Text.null shortened then "(empty)" else shortened

firstLine :: Text -> Text
firstLine = Text.takeWhile (/= '\n')

formatLoopError :: LoopError -> Text
formatLoopError = formatLoopErrorColored False

-- | Like 'formatLoopError', with optional ANSI styling for TTY stderr.
formatLoopErrorColored :: Bool -> LoopError -> Text
formatLoopErrorColored color = \case
    LoopTransport err ->
        roleError color ("transport error: " <> Text.pack (show err))
    LoopMaxTurns turn ->
        roleError color "stopped: max turns reached"
            <> maybe "" (\text -> "\n" <> text) turn.assistantText
    LoopNoResponseId ->
        roleError color "transport error: response had no id"
