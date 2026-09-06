-- | Tool-result presentation, including compatibility with imported transcripts.
module Agent.TUI.Model.ToolResult
    ( resultBlockState
    , runningShellOutcome
    , terminalShellOutcome
    , isTodoTool
    , isShellProcessTool
    , toolBlockKind
    , blockCodeLanguage
    , setBlockState
    ) where

import Agent.TUI.Model.Types
import Agent.TUI.Presentation (isInspectionTool)
import Agent.ToolDispatch
    ( ToolOutcome(..)
    , ToolCallResult(..)
    , canonicalToolName
    , toolCallResultOutcome
    )
import Control.Applicative ((<|>))
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as Text

-- Older/imported results have no execution facts and use the legacy adapter.
resultBlockState :: ToolCallResult -> BlockState
resultBlockState result = maybe (toolResultState result.output) outcomeBlockState
    (toolCallResultOutcome result)

outcomeBlockState :: ToolOutcome -> BlockState
outcomeBlockState = \case
    ToolSucceeded -> BlockComplete
    ToolFailed -> BlockFailed
    ToolDenied -> BlockDenied
    ToolCancelled -> BlockCancelled
    ShellRunning _ -> BlockRunning
    ShellExited 0 -> BlockComplete
    ShellExited _ -> BlockFailed
    ShellCancelled -> BlockCancelled
    ShellTimedOut -> BlockFailed

runningShellOutcome :: ToolCallResult -> Maybe (Int, Text)
runningShellOutcome result = case toolCallResultOutcome result of
    Just (ShellRunning sessionId) -> Just (sessionId, dropOutputLine (dropOutputLine result.output))
    Just _ -> Nothing
    Nothing -> runningShellResult result.output

terminalShellOutcome :: ToolCallResult -> Maybe (BlockState, Text)
terminalShellOutcome result = case toolCallResultOutcome result of
    Just outcome@(ShellExited _) -> Just (outcomeBlockState outcome, dropOutputLine result.output)
    Just ShellCancelled -> Just (BlockCancelled, result.output)
    Just ShellTimedOut -> Just (BlockFailed, result.output)
    Just _ -> Nothing
    Nothing -> terminalShellResult result.output

-- Only remove the transport prologue for presentation. It is never parsed for
-- execution status, exit codes or session identifiers.
dropOutputLine :: Text -> Text
dropOutputLine = Text.drop 1 . Text.dropWhile (/= '\n')

runningShellResult :: Text -> Maybe (Int, Text)
runningShellResult output = do
    rest <- Text.stripPrefix
        "Process still running.\nsession_id: "
        output
    let (sessionText, withNewline) = Text.breakOn "\n" rest
    body <- Text.stripPrefix "\n" withNewline
    sessionId <- readWholeInt sessionText
    pure (sessionId, body)

terminalShellResult :: Text -> Maybe (BlockState, Text)
terminalShellResult output =
    case Text.stripPrefix "Exit code: " output of
        Just rest ->
            let (codeText, withNewline) = Text.breakOn "\n" rest
            in case (readWholeInt codeText, Text.stripPrefix "\n" withNewline) of
                (Just code, Just body) ->
                    Just
                        ( if code == 0 then BlockComplete else BlockFailed
                        , body
                        )
                _ -> Nothing
        Nothing
            | "Error: Command cancelled" `Text.isPrefixOf` output ->
                Just (toolResultState output, output)
            | "Error: Command timed out" `Text.isPrefixOf` output ->
                Just (toolResultState output, output)
            | otherwise -> Nothing

readWholeInt :: Text -> Maybe Int
readWholeInt text =
    case reads (Text.unpack text) of
        [(value, "")] -> Just value
        _ -> Nothing

isTodoTool :: Text -> Bool
isTodoTool name =
    canonicalToolName name `elem` ["todo_write", "update_plan"]

isShellProcessTool :: Text -> Bool
isShellProcessTool name =
    canonicalToolName name `elem` ["shell_command", "write_stdin"]

toolBlockKind :: Text -> BlockKind
toolBlockKind rawName
    | name `elem` ["run_terminal_cmd", "shell_command", "write_stdin", "run_ghci", "exec"] =
        BlockShell
    | name `elem` ["search_replace", "apply_patch", "Write", "NotebookEdit"] =
        BlockEdit
    | name `elem` ["todo_write", "update_plan"] =
        BlockTodo
    | isInspectionTool rawName = BlockInspect
    | otherwise = BlockTool
  where
    name = canonicalToolName rawName

-- | Syntax grammar for code carried in a shell-style tool block.
-- The title is retained alongside the source after the original call leaves
-- the live-tool map, so it also identifies exec's JavaScript code here.
blockCodeLanguage :: UiBlock -> Maybe Text
blockCodeLanguage block
    | block.blockKind /= BlockShell = Nothing
    | Text.null (Text.strip block.blockDetail) = Nothing
    | block.blockTitle == "$ exec" = Just "javascript"
    | otherwise = Just "haskell"

outputLooksFailed :: Text -> Bool
outputLooksFailed output =
    let lowered = Text.toLower (Text.strip output)
    in "error:" `Text.isPrefixOf` lowered
        || "exit: 1" `Text.isPrefixOf` lowered
        || "exit: 2" `Text.isPrefixOf` lowered

toolResultState :: Text -> BlockState
toolResultState output
    | "tool call rejected by user" `Text.isInfixOf` lowered =
        BlockDenied
    | structuredCancellation lowered =
        BlockCancelled
    | outputLooksFailed output = BlockFailed
    | Just code <- exitCodeFrom lowered
    , code /= 0 = BlockFailed
    | otherwise = BlockComplete
  where
    lowered = Text.toLower (Text.strip output)

structuredCancellation :: Text -> Bool
structuredCancellation output =
    let header = Text.strip (headLine output)
    in header == "exit: cancelled"
        || header == "error: command cancelled"
        || header == "cancelled"
        || "cancelled (" `Text.isPrefixOf` header

headLine :: Text -> Text
headLine = Text.takeWhile (/= '\n')

exitCodeFrom :: Text -> Maybe Int
exitCodeFrom text = do
    rest <-
        Text.stripPrefix "exit:" text
            <|> Text.stripPrefix "exit code:" text
    case reads (Text.unpack (Text.takeWhile (not . isSpace) (Text.strip rest))) of
        [(code, "")] -> Just code
        _ -> Nothing

-- Shell commands start open while output is live, then compact to a one-line
-- summary. Preserve any manual folding while they are still running; a user
-- can also expand the completed block again with the normal block toggle.
setBlockState :: BlockState -> UiBlock -> UiBlock
setBlockState blockState block =
    block
        { blockState
        , blockExpanded =
            if block.blockKind == BlockShell
                && block.blockState `elem` [BlockStreaming, BlockRunning]
                && blockState `notElem` [BlockStreaming, BlockRunning]
                then False
                else block.blockExpanded
        }
