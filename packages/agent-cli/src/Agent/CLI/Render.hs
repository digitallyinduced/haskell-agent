-- | Stream renderer and mutating-tool approval prompts.
module Agent.CLI.Render
    ( RenderConfig(..)
    , formatLoopError
    , putTextLn
    , renderEvent
    , summarizeToolCall
    , truncateToolOutput
    ) where

import Agent.Loop (LoopError(..), LoopEvent(..), TurnOutput(..))
import Agent.ToolDispatch (ToolCall(..), ToolCallResult(..))
import Control.Concurrent.MVar (MVar, withMVar)
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
    { renderShowReasoning :: !Bool
    , renderPrintedText :: !(IORef Bool)
    , renderLock :: !(MVar ())
    , renderStdout :: !Handle
    , renderStderr :: !Handle
    }

renderEvent :: RenderConfig -> LoopEvent -> IO ()
renderEvent config event =
    withMVar config.renderLock \_ -> renderEventUnlocked config event

renderEventUnlocked :: RenderConfig -> LoopEvent -> IO ()
renderEventUnlocked config = \case
    TextDelta delta -> do
        writeIORef config.renderPrintedText True
        Text.hPutStr config.renderStdout delta
        hFlush config.renderStdout
    ReasoningDelta delta
        | config.renderShowReasoning -> do
            Text.hPutStr config.renderStderr (dimText delta)
            hFlush config.renderStderr
        | otherwise -> pure ()
    TurnStarted -> pure ()
    TurnFinished _ -> pure ()
    ToolStarted call ->
        putTextLn config.renderStderr ("→ " <> summarizeToolCall call)
    ToolFinished result ->
        putTextLn config.renderStderr (truncateToolOutput result.output)

-- | Write @text@ plus a newline as one 'Text.hPutStr'. @hPutStrLn@ on a
-- 'String' is @hPutStr@ then @hPutChar '\\n'@ over a @[Char]@ spine, so
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

toolDetail :: ToolCall -> Text
toolDetail call = case call.name of
    "read_file" -> jsonField "target_file" call.arguments
    "list_dir" -> jsonField "target_directory" call.arguments
    "search_replace" -> jsonField "file_path" call.arguments
    "grep" -> jsonField "pattern" call.arguments
    "run_terminal_cmd" -> firstLine (jsonField "command" call.arguments)
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

dimText :: Text -> Text
dimText text = "\ESC[2m" <> text <> "\ESC[0m"

formatLoopError :: LoopError -> Text
formatLoopError = \case
    LoopTransport err -> "transport error: " <> Text.pack (show err)
    LoopMaxTurns turn ->
        "stopped: max turns reached"
            <> maybe "" (\text -> "\n" <> text) turn.assistantText
    LoopNoResponseId -> "transport error: response had no id"
