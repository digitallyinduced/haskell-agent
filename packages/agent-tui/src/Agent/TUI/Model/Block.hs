-- | Append visible blocks and close the preceding inspection burst.
module Agent.TUI.Model.Block
    ( appendBlock
    , closeInspectionGroups
    ) where

import Agent.TUI.Model.Types
import Agent.TUI.Markdown.Stream (emptyMarkdownStreamState, feedMarkdownStream)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Text (Text)

appendBlock
    :: BlockKind
    -> Text
    -> Text
    -> Text
    -> BlockState
    -> Maybe Text
    -> UiState
    -> UiState
appendBlock kind title body detail blockState callId state =
    let prepared = closeInspectionGroups state
        index = Seq.length prepared.uiBlocks
        ident = BlockId prepared.uiNextBlockId
        block = UiBlock
            { blockId = ident
            , blockKind = kind
            , blockTitle = title
            , blockBody = body
            , blockTimestamp = ""
            , blockDetail = detail
            , blockState
            , blockExpanded =
                kind `elem` [BlockUser, BlockAssistant, BlockSystem, BlockRecap, BlockError]
                    || (kind == BlockShell
                        -- Execution source stays available on expansion,
                        -- while nested operations provide the live activity.
                        && title `notElem` ["JavaScript execution", "$ exec"]
                        && blockState `elem` [BlockStreaming, BlockRunning])
            , blockCallId = callId
            , blockInspectionGroupable = False
            }
    in prepared
        { uiBlocks = prepared.uiBlocks Seq.|> block
        , uiStreamingMarkdown =
            if kind == BlockAssistant && blockState == BlockStreaming
                then let parsed = feedMarkdownStream emptyMarkdownStreamState body
                     in parsed `seq` Just (ident, parsed)
                else Nothing
        , uiNextBlockId = prepared.uiNextBlockId + 1
        , uiSelectedBlock = Just ident
        , uiSelectedBlockIndex = Just index
        , uiBlockIndices = Map.insert ident index prepared.uiBlockIndices
        }

-- | Close a burst when another visible event intervenes. Keep its item
-- metadata until the turn ends so a provider can still retract an individual
-- completed call without removing the rest of the grouped block.
closeInspectionGroups :: UiState -> UiState
closeInspectionGroups state =
    state
        { uiInspectionGroups =
            Map.map
                (\group -> group { inspectionGroupOpen = False })
                state.uiInspectionGroups
        }
