-- | Frozen pre-batching page insertion/eviction algorithm. Keep this baseline
-- independent of production trimming so future benchmarks remain comparable.
module HistoryEvictionBaseline (applyHistoryPageBaseline) where

import Agent.CLI.TUI.History
import Agent.TUI.Model (UiBlock(..))
import Data.Foldable (toList)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Set as Set
import qualified Data.Text as Text

applyHistoryPageBaseline
    :: HistoryPage
    -> HistoryWindow
    -> Either HistoryPageRejection HistoryWindow
applyHistoryPageBaseline page window
    | page.historyPageGeneration /= window.historyWindowGeneration =
        Left (HistoryPageStale page.historyPageGeneration)
    | otherwise =
        Right $
            trimWindowPrefer
                (case direction of
                    HistoryOlder -> HistoryNewer
                    HistoryNewer -> HistoryOlder)
                window'
                    { historyWindowPending =
                        Set.delete direction window.historyWindowPending
                    , historyWindowGenerationStart = page.historyPageGenerationStart
                    , historyWindowTotalTurns = page.historyPageTotalTurns
                    , historyWindowHasOlder = page.historyPageHasOlder
                    , historyWindowHasNewer = page.historyPageHasNewer
                    }
  where
    direction = page.historyPageDirection
    incoming = uniqueTurns page.historyPageTurns
    existing = window.historyWindowTurns
    merged = case direction of
        HistoryOlder -> incoming <> existing
        HistoryNewer -> existing <> incoming
    window' =
        setTurns
            (uniqueTurns (Seq.fromList (sortOn (.historyTurnCursor) (toList merged))))
            window

setTurns :: Seq HistoryTurn -> HistoryWindow -> HistoryWindow
setTurns turns window =
    window
        { historyWindowTurns = turns
        , historyWindowTurnsByCursor =
            Map.fromList [(turn.historyTurnCursor, turn) | turn <- toList turns]
        , historyWindowBlocksById =
            Map.fromList
                [ (block.blockId, block)
                | turn <- toList turns
                , block <- toList turn.historyTurnBlocks
                ]
        }

uniqueTurns :: Seq HistoryTurn -> Seq HistoryTurn
uniqueTurns turns = Seq.fromList (reverse uniqueReversed)
  where
    (uniqueReversed, _) =
        foldl
            (\(kept, seen) turn ->
                if turn.historyTurnCursor `Set.member` seen
                    then (kept, seen)
                    else (turn : kept, Set.insert turn.historyTurnCursor seen))
            ([], Set.empty)
            (toList turns)

trimWindowPrefer :: HistoryDirection -> HistoryWindow -> HistoryWindow
trimWindowPrefer preferred window
    | withinBudget window = window
    | Seq.length window.historyWindowTurns <= 1 = window
    | otherwise = case preferredEvictionDirection preferred window of
        Nothing -> window
        Just direction -> trimWindowPrefer preferred (evictOne direction window)

withinBudget :: HistoryWindow -> Bool
withinBudget window =
    loadedTurns window <= window.historyWindowMaxTurns
        && loadedBlocks window <= window.historyWindowMaxBlocks
        && loadedBytes window <= window.historyWindowMaxBytes

loadedTurns :: HistoryWindow -> Int
loadedTurns = Seq.length . (.historyWindowTurns)

loadedBlocks :: HistoryWindow -> Int
loadedBlocks =
    sum . fmap (Seq.length . (.historyTurnBlocks)) . (.historyWindowTurns)

loadedBytes :: HistoryWindow -> Int
loadedBytes = sum . fmap historyTurnBytes . (.historyWindowTurns)

historyTurnBytes :: HistoryTurn -> Int
historyTurnBytes =
    sum . fmap historyBlockBytes . toList . (.historyTurnBlocks)

historyBlockBytes :: UiBlock -> Int
historyBlockBytes block =
    96
        + textBytes block.blockTitle
        + textBytes block.blockBody
        + textBytes block.blockTimestamp
        + textBytes block.blockDetail
        + maybe 0 textBytes block.blockCallId
  where
    textBytes = (2 *) . Text.length

preferredEvictionDirection
    :: HistoryDirection -> HistoryWindow -> Maybe HistoryDirection
preferredEvictionDirection preferred window
    | canEvict preferred = Just preferred
    | canEvict fallback = Just fallback
    | otherwise = Nothing
  where
    fallback = case preferred of
        HistoryOlder -> HistoryNewer
        HistoryNewer -> HistoryOlder
    canEvict direction = case edgeTurn direction window of
        Nothing -> False
        Just turn ->
            let cursor = turn.historyTurnCursor
            in Just cursor /= window.historyWindowVisibleAnchor
                && Just cursor /= window.historyWindowSelectedAnchor

edgeTurn :: HistoryDirection -> HistoryWindow -> Maybe HistoryTurn
edgeTurn direction window = case direction of
    HistoryOlder -> window.historyWindowTurns Seq.!? 0
    HistoryNewer ->
        window.historyWindowTurns Seq.!? (Seq.length window.historyWindowTurns - 1)

evictOne :: HistoryDirection -> HistoryWindow -> HistoryWindow
evictOne direction window = case direction of
    HistoryOlder ->
        setTurns dropFirst
            window { historyWindowHasOlder = True }
    HistoryNewer ->
        setTurns dropLast
            window { historyWindowHasNewer = True }
  where
    turns = window.historyWindowTurns
    dropFirst = case turns of
        _ Seq.:<| rest -> rest
        _ -> Seq.empty
    dropLast = case Seq.viewr turns of
        Seq.EmptyR -> Seq.empty
        rest Seq.:> _ -> rest
