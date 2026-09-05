-- | Block lookup and selection, independent of turn and tool lifecycles.
module Agent.TUI.Model.Selection
    ( moveSelection
    , selectBlock
    , lookupBlock
    , lookupBlockIndex
    , selectedBlockIndex
    , toggleSelected
    , selectionAfterTruncate
    , selectedIndexFor
    ) where

import Agent.TUI.Model.Types
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

moveSelection :: Int -> UiState -> UiState
moveSelection delta state =
    case Seq.lookup next blocks of
        Nothing ->
            state
                { uiSelectedBlock = Nothing
                , uiSelectedBlockIndex = Nothing
                }
        Just block ->
            state
                { uiSelectedBlock = Just block.blockId
                , uiSelectedBlockIndex = Just next
                , uiFollow = next == lastIndex
                }
  where
    blocks = state.uiBlocks
    lastIndex = Seq.length blocks - 1
    next = max 0 (min lastIndex (selectedBlockIndex state + delta))

selectBlock :: BlockId -> UiState -> UiState
selectBlock ident state =
    case lookupBlockIndex ident state of
        Nothing -> state
        Just (index, _) ->
            state
                { uiSelectedBlock = Just ident
                , uiSelectedBlockIndex = Just index
                , uiFollow =
                    index == Seq.length state.uiBlocks - 1
                }

lookupBlock :: BlockId -> UiState -> Maybe UiBlock
lookupBlock ident state =
    snd <$> lookupBlockIndex ident state

lookupBlockIndex :: BlockId -> UiState -> Maybe (Int, UiBlock)
lookupBlockIndex ident state = do
    index <- Map.lookup ident state.uiBlockIndices
    block <- Seq.lookup index state.uiBlocks
    if block.blockId == ident
        then Just (index, block)
        else Nothing

selectedBlockIndex :: UiState -> Int
selectedBlockIndex state =
    maybe fallback fst (selectedBlockEntry state)
  where
    fallback = max 0 (Seq.length state.uiBlocks - 1)

toggleSelected :: UiState -> UiState
toggleSelected state =
    case selectedBlockEntry state of
        Nothing -> state
        Just (index, _) ->
            state
                { uiBlocks =
                    Seq.adjust
                        (\block ->
                            block
                                { blockExpanded = not block.blockExpanded })
                        index
                        state.uiBlocks
                }

selectedBlockEntry :: UiState -> Maybe (Int, UiBlock)
selectedBlockEntry state = do
    ident <- state.uiSelectedBlock
    index <- state.uiSelectedBlockIndex
    storedIndex <- Map.lookup ident state.uiBlockIndices
    block <- Seq.lookup index state.uiBlocks
    if storedIndex == index && block.blockId == ident
        then Just (index, block)
        else Nothing

selectionAfterTruncate
    :: Seq UiBlock
    -> Maybe Int
    -> Maybe (Int, UiBlock)
selectionAfterTruncate blocks selected =
    case selected of
        Just index
            | Just block <- Seq.lookup index blocks ->
                Just (index, block)
        _ ->
            let index = Seq.length blocks - 1
            in (\block -> (index, block)) <$> Seq.lookup index blocks

selectedIndexFor :: Maybe BlockId -> Seq UiBlock -> Maybe Int
selectedIndexFor selected remaining =
    selected >>= \ident -> Seq.findIndexL ((== ident) . (.blockId)) remaining
