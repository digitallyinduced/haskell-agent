-- | Pure text decoding and visual layout for the fullscreen composer.
module Agent.CLI.TUI.Composer.Edit
    ( decodePaste
    , draftCursorLocation
    , wrapDraft
    , wrapDraftWindow
    ) where

import Agent.CLI.Input (terminalTextWidth)
import Agent.CLI.Input.Display (displayCells)
import Agent.CLI.Input.Types (DisplayCell(..))
import Agent.TUI.TextWidth (clampGraphemeCursor)
import Data.ByteString (ByteString)
import Data.Char (isControl)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error (lenientDecode)

-- | Visually wrap a draft without changing its underlying editor state. The
-- cursor offset is measured in the original text and returned in wrapped
-- row/column coordinates. Long unbroken text is split at terminal cell
-- boundaries; a glyph wider than the entire viewport is shown as an ellipsis.
wrapDraft :: Int -> Text -> Int -> ([Text], (Int, Int))
wrapDraft requestedWidth text requestedCursor =
    let width = max 1 requestedWidth
        cursor = clampGraphemeCursor text requestedCursor
        (rowsRev, currentRev, currentWidth, currentRow, cursorLocation) =
            go width cursor 0 [] [] 0 0 Nothing (draftTokens text)
        (finalRowsRev, finalCurrentRev, finalRow, finalColumn)
            | cursor == Text.length text && currentWidth >= width =
                (finishRow rowsRev currentRev, [], currentRow + 1, 0)
            | otherwise =
                (rowsRev, currentRev, currentRow, currentWidth)
        rows =
            reverse
                (Text.concat (reverse finalCurrentRev) : finalRowsRev)
        location =
            fromMaybe
                (finalRow, finalColumn)
                cursorLocation
    in (rows, location)
  where
    go
        :: Int
        -> Int
        -> Int
        -> [Text]
        -> [Text]
        -> Int
        -> Int
        -> Maybe (Int, Int)
        -> [DraftToken]
        -> ([Text], [Text], Int, Int, Maybe (Int, Int))
    go _ _ _ rowsRev currentRev currentWidth currentRow location [] =
        (rowsRev, currentRev, currentWidth, currentRow, location)
    go width cursor index rowsRev currentRev currentWidth currentRow location
        (DraftLineBreak : rest) =
            let atVisualBoundary = currentWidth >= width
                rowsAfterBoundary
                    | atVisualBoundary = finishRow rowsRev currentRev
                    | otherwise = rowsRev
                rowAfterBoundary
                    | atVisualBoundary = currentRow + 1
                    | otherwise = currentRow
                location' =
                    recordCursor
                        cursor
                        index
                        (currentRow, currentWidth)
                        location
                (nextRows, nextRow)
                    | atVisualBoundary =
                        (rowsAfterBoundary, rowAfterBoundary)
                    | otherwise =
                        (finishRow rowsAfterBoundary currentRev, currentRow + 1)
            in go
                width cursor (index + 1) nextRows [] 0 nextRow location' rest
    go width cursor index rowsRev currentRev currentWidth currentRow location
        (DraftCell cell : rest) =
        let displayText
                | cell.displayCellWidth > width = "…"
                | otherwise = cell.displayCellText
            displayWidth
                | cell.displayCellWidth > width = 1
                | otherwise = cell.displayCellWidth
            shouldWrap =
                displayWidth > 0
                    && ( currentWidth >= width
                        || (currentWidth > 0
                            && currentWidth + displayWidth > width)
                       )
            rows' =
                if shouldWrap
                    then finishRow rowsRev currentRev
                    else rowsRev
            currentRev' = if shouldWrap then [] else currentRev
            currentWidth' = if shouldWrap then 0 else currentWidth
            currentRow' = if shouldWrap then currentRow + 1 else currentRow
            location' =
                recordCursor
                    cursor
                    index
                    (currentRow', currentWidth')
                    location
        in go
            width
            cursor
            (index + cell.displayCellSourceLength)
            rows'
            (displayText : currentRev')
            (currentWidth' + displayWidth)
            currentRow'
            location'
            rest

    finishRow rowsRev currentRev =
        Text.concat (reverse currentRev) : rowsRev

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
