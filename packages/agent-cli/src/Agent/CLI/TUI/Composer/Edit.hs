-- | Pure text decoding and visual layout for the fullscreen composer.
module Agent.CLI.TUI.Composer.Edit
    ( decodePaste
    , draftCursorLocation
    , verticalCursorMove
    , wrapDraft
    , wrapDraftWindow
    ) where

import Agent.CLI.Input (terminalTextWidth)
import Agent.CLI.Input.Display (displayCells)
import Agent.CLI.Input.Types (DisplayCell(..))
import Agent.TUI.TextWidth (clampGraphemeCursor)
import Data.ByteString (ByteString)
import Data.Char (isControl)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error (lenientDecode)

-- | Accumulator for 'wrapDraft'. Rows are collected in reverse, and the
-- current row is a reversed list of grapheme-cell texts.
data WrapState = WrapState
    { wrapRowsRev :: ![Text]
    , wrapCellsRev :: ![Text]
    , wrapColumn :: !Int
    , wrapRow :: !Int
    , wrapIndex :: !Int
    , wrapCursor :: !(Maybe (Int, Int))
    }

-- | Visually wrap a draft without changing its underlying editor state. The
-- cursor offset is measured in the original text and returned in wrapped
-- row/column coordinates. Long unbroken text is split at terminal cell
-- boundaries; a glyph wider than the entire viewport is shown as an ellipsis.
wrapDraft :: Int -> Text -> Int -> ([Text], (Int, Int))
wrapDraft requestedWidth text requestedCursor =
    let width = max 1 requestedWidth
        cursor = clampGraphemeCursor text requestedCursor
        initial = WrapState
            { wrapRowsRev = []
            , wrapCellsRev = []
            , wrapColumn = 0
            , wrapRow = 0
            , wrapIndex = 0
            , wrapCursor = Nothing
            }
        final =
            foldl' (stepToken width cursor) initial (draftTokens text)
        -- A cursor at the end of a visually full row belongs on the next
        -- (empty) continuation row.
        (finalRowsRev, finalCellsRev, finalRow, finalColumn)
            | cursor == Text.length text && final.wrapColumn >= width =
                ( finishRow final.wrapRowsRev final.wrapCellsRev
                , []
                , final.wrapRow + 1
                , 0
                )
            | otherwise =
                ( final.wrapRowsRev
                , final.wrapCellsRev
                , final.wrapRow
                , final.wrapColumn
                )
        rows =
            reverse
                (Text.concat (reverse finalCellsRev) : finalRowsRev)
        location =
            fromMaybe (finalRow, finalColumn) final.wrapCursor
    in (rows, location)
  where
    stepToken :: Int -> Int -> WrapState -> DraftToken -> WrapState
    stepToken width cursor state = \case
        DraftLineBreak ->
            state
                { wrapRowsRev = finishRow state.wrapRowsRev state.wrapCellsRev
                , wrapCellsRev = []
                , wrapColumn = 0
                , wrapRow = state.wrapRow + 1
                , wrapIndex = state.wrapIndex + 1
                , wrapCursor =
                    -- A cursor on the newline itself renders at the end of
                    -- the line being terminated, even when that row is
                    -- visually full.
                    recordCursor
                        cursor
                        state.wrapIndex
                        (state.wrapRow, state.wrapColumn)
                        state.wrapCursor
                }
        DraftCell cell ->
            let displayText
                    | cell.displayCellWidth > width = "…"
                    | otherwise = cell.displayCellText
                displayWidth
                    | cell.displayCellWidth > width = 1
                    | otherwise = cell.displayCellWidth
                shouldWrap =
                    displayWidth > 0
                        && ( state.wrapColumn >= width
                            || (state.wrapColumn > 0
                                && state.wrapColumn + displayWidth > width)
                           )
                rowsRev
                    | shouldWrap =
                        finishRow state.wrapRowsRev state.wrapCellsRev
                    | otherwise = state.wrapRowsRev
                cellsRev = if shouldWrap then [] else state.wrapCellsRev
                column = if shouldWrap then 0 else state.wrapColumn
                row = if shouldWrap then state.wrapRow + 1 else state.wrapRow
            in WrapState
                { wrapRowsRev = rowsRev
                , wrapCellsRev = displayText : cellsRev
                , wrapColumn = column + displayWidth
                , wrapRow = row
                , wrapIndex = state.wrapIndex + cell.displayCellSourceLength
                , wrapCursor =
                    recordCursor
                        cursor
                        state.wrapIndex
                        (row, column)
                        state.wrapCursor
                }

    finishRow rowsRev cellsRev =
        Text.concat (reverse cellsRev) : rowsRev

    recordCursor cursor index location = \case
        Nothing
            | cursor == index -> Just location
        previous -> previous

-- | Lay out only the logical lines that can contribute to a bounded composer
-- viewport around the cursor. Starting at newline boundaries preserves visual
-- wrapping, while avoiding a full-draft grapheme pass for large pastes.
wrapDraftWindow :: Int -> Int -> Text -> Int -> ([Text], (Int, Int))
wrapDraftWindow requestedRows requestedWidth text requestedCursor =
    let rows = max 1 requestedRows
        cursor = max 0 (min (Text.length text) requestedCursor)
        before = Text.take cursor text
        start = precedingLineStart rows before
        after = Text.drop cursor text
        end = cursor + followingLinesLength rows after
        window = Text.take (end - start) (Text.drop start text)
    in wrapDraft requestedWidth window (cursor - start)

precedingLineStart :: Int -> Text -> Int
precedingLineStart lineCount text =
    go lineCount text
  where
    go remaining current
        | remaining <= 0 = Text.length current
        | otherwise =
            case Text.unsnoc current of
                Nothing -> 0
                Just (rest, character)
                    | character == '\n' ->
                        if remaining == 1
                            then Text.length rest + 1
                            else go (remaining - 1) rest
                    | otherwise -> go remaining rest

followingLinesLength :: Int -> Text -> Int
followingLinesLength lineCount =
    go lineCount 0
  where
    go remaining consumed current
        | remaining <= 0 = consumed
        | otherwise =
            let (line, rest) = Text.break (== '\n') current
                next = consumed + Text.length line
            in if Text.null rest
                then next
                else
                    go
                        (remaining - 1)
                        (next + 1)
                        (Text.drop 1 rest)

draftCursorLocation :: Text -> Int -> (Int, Int)
draftCursorLocation text requestedCursor =
    let cursor = clampGraphemeCursor text requestedCursor
        before = Text.take cursor text
        rows = Text.splitOn "\n" before
    in case reverse rows of
        [] -> (0, 0)
        lastRow : rest -> (length rest, terminalTextWidth lastRow)

-- | Move the cursor one logical line up (negative delta) or down (positive
-- delta) while preserving the visual column. Nothing when the cursor is
-- already on the first or last logical line, so callers can fall back to
-- history navigation at the draft's edges.
verticalCursorMove :: Int -> Text -> Int -> Maybe Int
verticalCursorMove delta text requestedCursor =
    let cursor = clampGraphemeCursor text requestedCursor
        draftLines = Seq.fromList (Text.splitOn "\n" text)
        (row, column) = draftCursorLocation text cursor
        targetRow = row + delta
    in if targetRow < 0 || targetRow >= Seq.length draftLines
        then Nothing
        else do
            line <- Seq.lookup targetRow draftLines
            let lineStart =
                    sum
                        [ Text.length prefix + 1
                        | prefix <- toList (Seq.take targetRow draftLines)
                        ]
            Just (lineStart + offsetAtColumn line column)

-- | Source offset of the grapheme boundary in a single line that renders
-- closest to the requested terminal column without crossing it.
offsetAtColumn :: Text -> Int -> Int
offsetAtColumn line column = go 0 0 (displayCells line)
  where
    go offset _ [] = offset
    go offset width (cell : rest)
        | width + cell.displayCellWidth > column = offset
        | otherwise =
            go
                (offset + cell.displayCellSourceLength)
                (width + cell.displayCellWidth)
                rest

decodePaste :: ByteString -> Text
decodePaste =
    Text.filter
        (\character ->
            character == '\n'
                || character == '\t'
                || not (isControl character))
        . Text.decodeUtf8With lenientDecode

data DraftToken
    = DraftLineBreak
    | DraftCell !DisplayCell

draftTokens :: Text -> [DraftToken]
draftTokens raw
    | Text.null raw = []
    | otherwise =
        let (line, rest) = Text.break (== '\n') raw
            lineTokens = map DraftCell (displayCells line)
        in if Text.null rest
            then lineTokens
            else lineTokens
                <> (DraftLineBreak : draftTokens (Text.drop 1 rest))
