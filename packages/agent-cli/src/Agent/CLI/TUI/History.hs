{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Pure state transitions for a bounded, cursor-backed transcript window.
--
-- The fullscreen renderer owns the actual request worker.  This module only
-- describes the durable turn range currently materialised in memory, making
-- page insertion, stale-result rejection, and budgeted eviction testable
-- without Brick or PostgreSQL.
module Agent.CLI.TUI.History
    ( HistoryCursor(..)
    , HistoryDirection(..)
    , HistoryGeneration(..)
    , HistoryPage(..)
    , HistoryPageRejection(..)
    , HistoryRequest(..)
    , HistoryTurn(..)
    , HistoryWindow(..)
    , appendHistoryTurn
    , applyHistoryPage
    , clearHistoryRequest
    , emptyHistoryWindow
    , historyWindowCanRequest
    , historyWindowCursors
    , historyWindowHasBlocks
    , historyWindowLoadedBytes
    , historyWindowOlderAvailable
    , historyWindowNewerAvailable
    , historyWindowLoadedBlocks
    , historyWindowLoadedTurns
    , historyWindowRequest
    , historyWindowSelected
    , historyWindowSetAnchors
    , historyWindowSetGeneration
    , historyWindowTurn
    , historyWindowVisible
    , markHistoryRequest
    ) where

import Agent.TUI.Model (UiBlock(..))
import Data.Foldable (find, toList)
import Data.Int (Int64)
import Data.List (sortOn)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as Text

newtype HistoryCursor = HistoryCursor Int64
    deriving (Eq, Ord, Show)

newtype HistoryGeneration = HistoryGeneration Int64
    deriving (Eq, Ord, Show)

data HistoryDirection
    = HistoryOlder
    | HistoryNewer
    deriving (Eq, Ord, Show)

data HistoryTurn = HistoryTurn
    { historyTurnCursor :: !HistoryCursor
    , historyTurnBlocks :: !(Seq UiBlock)
    }
    deriving (Eq, Show)

-- | A page is ordered in ascending durable turn order, irrespective of the
-- direction in which it was requested.
data HistoryPage = HistoryPage
    { historyPageGeneration :: !HistoryGeneration
    , historyPageDirection :: !HistoryDirection
    , historyPageTurns :: !(Seq HistoryTurn)
    , historyPageGenerationStart :: !HistoryCursor
    , historyPageTotalTurns :: !Int64
    , historyPageHasOlder :: !Bool
    , historyPageHasNewer :: !Bool
    }
    deriving (Eq, Show)

data HistoryRequest = HistoryRequest
    { historyRequestGeneration :: !HistoryGeneration
    , historyRequestDirection :: !HistoryDirection
    , historyRequestCursor :: !(Maybe HistoryCursor)
    }
    deriving (Eq, Show)

data HistoryWindow = HistoryWindow
    { historyWindowGeneration :: !HistoryGeneration
    , historyWindowTurns :: !(Seq HistoryTurn)
    , historyWindowHasOlder :: !Bool
    , historyWindowHasNewer :: !Bool
    , historyWindowMaxTurns :: !Int
    , historyWindowMaxBlocks :: !Int
    , historyWindowMaxBytes :: !Int
    , historyWindowGenerationStart :: !HistoryCursor
    , historyWindowTotalTurns :: !Int64
    , historyWindowPending :: !(Set HistoryDirection)
    , historyWindowVisibleAnchor :: !(Maybe HistoryCursor)
    , historyWindowSelectedAnchor :: !(Maybe HistoryCursor)
    }
    deriving (Eq, Show)

data HistoryPageRejection
    = HistoryPageStale !HistoryGeneration
    | HistoryPageWrongDirection !HistoryDirection
    deriving (Eq, Show)

emptyHistoryWindow
    :: HistoryGeneration
    -> Int
    -> Int
    -> Int
    -> HistoryWindow
emptyHistoryWindow generation maxTurns maxBlocks maxBytes =
    HistoryWindow
        { historyWindowGeneration = generation
        , historyWindowTurns = Seq.empty
        , historyWindowHasOlder = False
        , historyWindowHasNewer = False
        , historyWindowMaxTurns = max 1 maxTurns
        , historyWindowMaxBlocks = max 1 maxBlocks
        , historyWindowMaxBytes = max 1 maxBytes
        , historyWindowGenerationStart = HistoryCursor 0
        , historyWindowTotalTurns = 0
        , historyWindowPending = Set.empty
        , historyWindowVisibleAnchor = Nothing
        , historyWindowSelectedAnchor = Nothing
        }

historyWindowLoadedTurns :: HistoryWindow -> Int
historyWindowLoadedTurns = Seq.length . (.historyWindowTurns)

historyWindowLoadedBlocks :: HistoryWindow -> Int
historyWindowLoadedBlocks =
    sum . fmap (Seq.length . (.historyTurnBlocks)) . (.historyWindowTurns)

historyWindowLoadedBytes :: HistoryWindow -> Int
historyWindowLoadedBytes =
    sum . fmap historyTurnBytes . (.historyWindowTurns)

historyWindowHasBlocks :: HistoryWindow -> Bool
historyWindowHasBlocks = not . Seq.null . (.historyWindowTurns)

historyWindowOlderAvailable :: HistoryWindow -> Bool
historyWindowOlderAvailable = (.historyWindowHasOlder)

historyWindowNewerAvailable :: HistoryWindow -> Bool
historyWindowNewerAvailable = (.historyWindowHasNewer)

historyWindowVisible :: HistoryWindow -> Maybe HistoryCursor
historyWindowVisible = (.historyWindowVisibleAnchor)

historyWindowSelected :: HistoryWindow -> Maybe HistoryCursor
historyWindowSelected = (.historyWindowSelectedAnchor)

historyWindowCursors :: HistoryWindow -> Seq HistoryCursor
historyWindowCursors = fmap (.historyTurnCursor) . (.historyWindowTurns)

historyWindowTurn
    :: HistoryCursor
    -> HistoryWindow
    -> Maybe HistoryTurn
historyWindowTurn cursor =
    find (\turn -> turn.historyTurnCursor == cursor)
        . (.historyWindowTurns)

historyWindowSetAnchors
    :: Maybe HistoryCursor
    -> Maybe HistoryCursor
    -> HistoryWindow
    -> HistoryWindow
historyWindowSetAnchors visible selected window =
    window
        { historyWindowVisibleAnchor = keepLoaded visible
        , historyWindowSelectedAnchor = keepLoaded selected
        }
  where
    keepLoaded cursor =
        cursor >>= \value ->
            if value `Set.member` loaded
                then Just value
                else Nothing

    loaded = Set.fromList (toList (historyWindowCursors window))

-- | Switch to a new provider/session generation and discard all materialised
-- blocks.  Budgets are intentionally retained across the switch.
historyWindowSetGeneration
    :: HistoryGeneration
    -> HistoryWindow
    -> HistoryWindow
historyWindowSetGeneration generation window =
    window
        { historyWindowGeneration = generation
        , historyWindowTurns = Seq.empty
        , historyWindowHasOlder = False
        , historyWindowHasNewer = False
        , historyWindowGenerationStart = HistoryCursor 0
        , historyWindowTotalTurns = 0
        , historyWindowPending = Set.empty
        , historyWindowVisibleAnchor = Nothing
        , historyWindowSelectedAnchor = Nothing
        }

historyWindowCanRequest
    :: HistoryDirection
    -> HistoryWindow
    -> Bool
historyWindowCanRequest direction window =
    not (direction `Set.member` window.historyWindowPending)
        && case direction of
            HistoryOlder -> window.historyWindowHasOlder
            HistoryNewer -> window.historyWindowHasNewer

historyWindowRequest
    :: HistoryDirection
    -> HistoryWindow
    -> Maybe HistoryRequest
historyWindowRequest direction window
    | not (historyWindowCanRequest direction window) = Nothing
    | otherwise =
        Just HistoryRequest
            { historyRequestGeneration = window.historyWindowGeneration
            , historyRequestDirection = direction
            , historyRequestCursor =
                case direction of
                    HistoryOlder ->
                        window.historyWindowTurns
                            Seq.!? 0
                            >>= Just . (.historyTurnCursor)
                    HistoryNewer ->
                        window.historyWindowTurns
                            Seq.!? (Seq.length window.historyWindowTurns - 1)
                            >>= Just . (.historyTurnCursor)
            }

-- | Mark a request as in flight.  A request that cannot currently be made is
-- ignored, which keeps event handlers idempotent when scroll ticks coalesce.
markHistoryRequest
    :: HistoryDirection
    -> HistoryWindow
    -> HistoryWindow
markHistoryRequest direction window =
    case historyWindowRequest direction window of
        Nothing -> window
        Just _ ->
            window
                { historyWindowPending =
                    Set.insert direction window.historyWindowPending
                }

clearHistoryRequest :: HistoryRequest -> HistoryWindow -> HistoryWindow
clearHistoryRequest request window
    | request.historyRequestGeneration /= window.historyWindowGeneration =
        window
    | otherwise =
        window
            { historyWindowPending =
                Set.delete
                    request.historyRequestDirection
                    window.historyWindowPending
            }

-- | Archive a newly committed latest turn. If the current window is not at
-- the durable tail, reset it to that turn rather than creating an internal
-- cursor gap that the edge-only paging model cannot represent.
appendHistoryTurn :: HistoryTurn -> HistoryWindow -> HistoryWindow
appendHistoryTurn turn window =
    either (const base) id (applyHistoryPage page base)
  where
    disconnected = window.historyWindowHasNewer
    base
        | disconnected =
            emptyHistoryWindow
                window.historyWindowGeneration
                window.historyWindowMaxTurns
                window.historyWindowMaxBlocks
                window.historyWindowMaxBytes
        | otherwise = window
    page =
        HistoryPage
            { historyPageGeneration = window.historyWindowGeneration
            , historyPageDirection = HistoryNewer
            , historyPageTurns = Seq.singleton turn
            , historyPageGenerationStart =
                window.historyWindowGenerationStart
            , historyPageTotalTurns =
                window.historyWindowTotalTurns + 1
            , historyPageHasOlder =
                if disconnected
                    then window.historyWindowTotalTurns > 0
                    else window.historyWindowHasOlder
            , historyPageHasNewer = False
            }

applyHistoryPage
    :: HistoryPage
    -> HistoryWindow
    -> Either HistoryPageRejection HistoryWindow
applyHistoryPage page window
    | page.historyPageGeneration /= window.historyWindowGeneration =
        Left (HistoryPageStale page.historyPageGeneration)
    | otherwise =
        Right (mergePage page window)

mergePage :: HistoryPage -> HistoryWindow -> HistoryWindow
mergePage page window =
    trimWindowPrefer
        (case direction of
            HistoryOlder -> HistoryNewer
            HistoryNewer -> HistoryOlder)
        window'
            { historyWindowPending =
                Set.delete direction window.historyWindowPending
            , historyWindowGenerationStart =
                page.historyPageGenerationStart
            , historyWindowTotalTurns =
                page.historyPageTotalTurns
            , historyWindowHasOlder =
                page.historyPageHasOlder
            , historyWindowHasNewer =
                page.historyPageHasNewer
            }
  where
    direction = page.historyPageDirection
    incoming = uniqueTurns page.historyPageTurns
    existing = window.historyWindowTurns
    merged =
        case direction of
            HistoryOlder -> incoming <> existing
            HistoryNewer -> existing <> incoming
    window' = window { historyWindowTurns = uniqueTurns (sortTurns merged) }

sortTurns :: Seq HistoryTurn -> Seq HistoryTurn
sortTurns = Seq.fromList . sortOn (.historyTurnCursor) . toList

uniqueTurns :: Seq HistoryTurn -> Seq HistoryTurn
uniqueTurns turns = Seq.fromList (reverse uniqueReversed)
  where
    (uniqueReversed, _) =
        foldl
            (\(kept, seen) turn ->
                if turn.historyTurnCursor `Set.member` seen
                    then (kept, seen)
                    else
                        ( turn : kept
                        , Set.insert turn.historyTurnCursor seen
                        )
            )
            ([], Set.empty)
            (toList turns)

trimWindowPrefer
    :: HistoryDirection
    -> HistoryWindow
    -> HistoryWindow
trimWindowPrefer preferred window
    | withinBudget window = window
    | otherwise =
        case preferredEvictionDirection preferred window of
            Nothing -> window
            Just direction ->
                trimWindowPrefer preferred (evictOne direction window)

withinBudget :: HistoryWindow -> Bool
withinBudget window =
    historyWindowLoadedTurns window <= window.historyWindowMaxTurns
        && historyWindowLoadedBlocks window <= window.historyWindowMaxBlocks
        && historyWindowLoadedBytes window <= window.historyWindowMaxBytes

historyTurnBytes :: HistoryTurn -> Int
historyTurnBytes =
    sum
        . fmap historyBlockBytes
        . toList
        . (.historyTurnBlocks)

historyBlockBytes :: UiBlock -> Int
historyBlockBytes block =
    96
        + textBytes block.blockTitle
        + textBytes block.blockBody
        + textBytes block.blockTimestamp
        + textBytes block.blockDetail
        + maybe 0 textBytes block.blockCallId
  where
    -- UTF-16-ish accounting deliberately overestimates common ASCII content
    -- and keeps retained history bounded without serialising blocks again.
    textBytes = (2 *) . Text.length

preferredEvictionDirection
    :: HistoryDirection
    -> HistoryWindow
    -> Maybe HistoryDirection
preferredEvictionDirection preferred window
    | canEvict preferred = Just preferred
    | canEvict fallback = Just fallback
    | otherwise = Nothing
  where
    fallback = case preferred of
        HistoryOlder -> HistoryNewer
        HistoryNewer -> HistoryOlder
    canEvict direction =
        case edgeTurn direction window of
            Nothing -> False
            Just turn ->
                let cursor = turn.historyTurnCursor
                in Just cursor /= window.historyWindowVisibleAnchor
                    && Just cursor
                        /= window.historyWindowSelectedAnchor

edgeTurn :: HistoryDirection -> HistoryWindow -> Maybe HistoryTurn
edgeTurn direction window =
    case direction of
        HistoryOlder -> window.historyWindowTurns Seq.!? 0
        HistoryNewer ->
            window.historyWindowTurns
                Seq.!? (Seq.length (window.historyWindowTurns) - 1)

evictOne :: HistoryDirection -> HistoryWindow -> HistoryWindow
evictOne direction window =
    case direction of
        HistoryOlder ->
            window
                { historyWindowTurns = dropFirst
                , historyWindowHasOlder = True
                }
        HistoryNewer ->
            window
                { historyWindowTurns = dropLast
                , historyWindowHasNewer = True
                }
  where
    turns = window.historyWindowTurns
    dropFirst =
        case turns of
            _ Seq.:<| rest -> rest
            _ -> Seq.empty
    dropLast =
        case Seq.viewr turns of
            Seq.EmptyR -> Seq.empty
            rest Seq.:> _ -> rest
