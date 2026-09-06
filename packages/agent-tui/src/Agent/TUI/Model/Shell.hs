-- | Keep background shell sessions attached to their original visible block.
module Agent.TUI.Model.Shell
    ( trackedShellOwner
    , reconcileVisibleShellContinuation
    , retainRunningShell
    , finishShellPoll
    , retainShellProcesses
    , retainShellPolls
    , writeStdinInput
    , writeStdinSession
    , emptyWriteStdinSession
    ) where

import qualified Agent.Json.Decode as Hermes
import Agent.TUI.Model.Selection (lookupBlockIndex)
import Agent.TUI.Model.ToolResult
    ( runningShellOutcome, terminalShellOutcome, setBlockState )
import Agent.TUI.Model.Types
import Agent.ToolDispatch (ToolCall(..), ToolCallResult(..), canonicalToolName)
import Control.Monad (guard)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text

trackedShellOwner :: ToolCall -> UiState -> Maybe BlockId
trackedShellOwner call state = do
    sessionId <- writeStdinSession call
    Map.lookup sessionId state.uiShellProcesses

-- A non-empty write_stdin remains visible as an input action, but it can also
-- observe the underlying command finishing. Keep the process owner in sync
-- without duplicating the continuation output across both blocks.
reconcileVisibleShellContinuation
    :: ToolCall
    -> ToolCallResult
    -> UiState
    -> UiState
reconcileVisibleShellContinuation call result state =
    case writeStdinSession call of
        Nothing -> state
        Just sessionId ->
            case Map.lookup sessionId state.uiShellProcesses of
                Nothing -> state
                Just ownerId ->
                    case runningShellOutcome result of
                        Just (returnedSessionId, _) ->
                            state
                                { uiShellProcesses =
                                    Map.insert
                                        returnedSessionId
                                        ownerId
                                        (Map.delete
                                            sessionId
                                            state.uiShellProcesses)
                                }
                        Nothing ->
                            case terminalShellOutcome result of
                                Nothing -> state
                                Just (blockState, _) ->
                                    let
                                        processes =
                                            Map.delete
                                                sessionId
                                                state.uiShellProcesses
                                        blocks =
                                            case lookupBlockIndex ownerId state of
                                                Nothing -> state.uiBlocks
                                                Just (blockIndex, _) ->
                                                    Seq.adjust
                                                        (setBlockState blockState)
                                                        blockIndex
                                                        state.uiBlocks
                                    in state
                                        { uiBlocks = blocks
                                        , uiShellProcesses = processes
                                        , uiShellPolls =
                                            retainShellPolls
                                                processes
                                                state.uiShellPolls
                                        }

retainRunningShell :: Int -> Int -> Text -> UiState -> UiState
retainRunningShell blockIndex sessionId output state =
    case Seq.lookup blockIndex state.uiBlocks of
        Nothing -> state
        Just owner ->
            state
                { uiBlocks =
                    Seq.adjust
                        (\block ->
                            setBlockState BlockRunning block
                                { blockBody = output })
                        blockIndex
                        state.uiBlocks
                , uiShellProcesses =
                    Map.insert
                        sessionId
                        owner.blockId
                        state.uiShellProcesses
                }

finishShellPoll :: Int -> ToolCallResult -> UiState -> UiState
finishShellPoll sessionId result state =
    let
        next =
            state
                { uiRunning = True
                , uiAwaitingInput = False
                , uiActivity = "Thinking…"
                , uiShellPolls =
                    Map.delete result.callId state.uiShellPolls
                }
        withoutSession =
            Map.delete sessionId next.uiShellProcesses
    in case Map.lookup sessionId state.uiShellProcesses of
        Nothing ->
            next { uiShellProcesses = withoutSession }
        Just ownerId ->
            case lookupBlockIndex ownerId next of
                Nothing ->
                    next { uiShellProcesses = withoutSession }
                Just (blockIndex, _) ->
                    case runningShellOutcome result of
                        Just (returnedSessionId, output) ->
                            appendShellOutput blockIndex output BlockRunning
                                next
                                    { uiShellProcesses =
                                        Map.insert
                                            returnedSessionId
                                            ownerId
                                            withoutSession
                                    }
                        Nothing ->
                            case terminalShellOutcome result of
                                Nothing -> next
                                Just (blockState, output) ->
                                    appendShellOutput
                                        blockIndex
                                        output
                                        blockState
                                        next
                                            { uiShellProcesses =
                                                withoutSession
                                            }

appendShellOutput :: Int -> Text -> BlockState -> UiState -> UiState
appendShellOutput blockIndex output blockState state =
    state
        { uiBlocks =
            Seq.adjust
                (\block ->
                    setBlockState blockState block
                        { blockBody = block.blockBody <> output })
                blockIndex
                state.uiBlocks
        }

retainShellProcesses
    :: Seq UiBlock
    -> Map.Map Int BlockId
    -> Map.Map Int BlockId
retainShellProcesses blocks =
    Map.filter \ownerId ->
        any
            (\block ->
                block.blockId == ownerId
                    && block.blockState == BlockRunning)
            blocks

retainShellPolls
    :: Map.Map Int BlockId
    -> Map.Map Text Int
    -> Map.Map Text Int
retainShellPolls processes =
    Map.filter (`Map.member` processes)

writeStdinInput :: ToolCall -> Maybe (Int, Maybe Text)
writeStdinInput call = do
    guard (canonicalToolName call.name == "write_stdin")
    either (const Nothing) Just $
        Hermes.decodeText
            (Hermes.object $
                (,)
                    <$> Hermes.atKey "session_id" Hermes.int
                    <*> Hermes.optionalKey "chars" Hermes.text)
            call.arguments

writeStdinSession :: ToolCall -> Maybe Int
writeStdinSession = fmap fst . writeStdinInput

emptyWriteStdinSession :: ToolCall -> Maybe Int
emptyWriteStdinSession call = do
    (sessionId, chars) <- writeStdinInput call
    guard (maybe True Text.null chars)
    pure sessionId
