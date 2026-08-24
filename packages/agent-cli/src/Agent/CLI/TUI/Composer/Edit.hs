-- | Pure text decoding and visual layout for the fullscreen composer.
module Agent.CLI.TUI.Composer.Edit
    ( decodePaste
    , draftCursorLocation
    , wrapDraft
    ) where

import Agent.CLI.Input (terminalCharWidth, terminalTextWidth)
import Data.ByteString (ByteString)
import Data.Char (isControl)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error (lenientDecode)

-- | Visually wrap a draft without changing its underlying text. The cursor
-- offset is measured in the original text and returned in wrapped row/column
-- coordinates. Long unbroken text is split at terminal cell boundaries.
wrapDraft :: Int -> Text -> Int -> ([Text], (Int, Int))
wrapDraft requestedWidth text requestedCursor =
    let width = max 1 requestedWidth
        cursor = max 0 (min (Text.length text) requestedCursor)
        (rowsRev, currentRev, currentWidth, currentRow, cursorLocation) =
            go width cursor 0 [] [] 0 0 Nothing (Text.unpack text)
        (finalRowsRev, finalCurrentRev, finalRow, finalColumn)
            | cursor == Text.length text && currentWidth >= width =
                (finishRow rowsRev currentRev, [], currentRow + 1, 0)
            | otherwise =
                (rowsRev, currentRev, currentRow, currentWidth)
        rows = reverse (Text.pack (reverse finalCurrentRev) : finalRowsRev)
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
        -> [Char]
        -> Int
        -> Int
        -> Maybe (Int, Int)
        -> [Char]
        -> ([Text], [Char], Int, Int, Maybe (Int, Int))
    go _ _ _ rowsRev currentRev currentWidth currentRow location [] =
        (rowsRev, currentRev, currentWidth, currentRow, location)
    go width cursor index rowsRev currentRev currentWidth currentRow location
        (character : rest)
        | character == '\n' =
            let atVisualBoundary = currentWidth >= width
                rowsAfterBoundary
                    | atVisualBoundary = finishRow rowsRev currentRev
                    | otherwise = rowsRev
                rowAfterBoundary
                    | atVisualBoundary = currentRow + 1
                    | otherwise = currentRow
                columnAfterBoundary
                    | atVisualBoundary = 0
                    | otherwise = currentWidth
                location' =
                    recordCursor
                        cursor
                        index
                        (rowAfterBoundary, columnAfterBoundary)
                        location
                (nextRows, nextRow)
                    | atVisualBoundary =
                        (rowsAfterBoundary, rowAfterBoundary)
                    | otherwise =
                        (finishRow rowsAfterBoundary currentRev, currentRow + 1)
            in go
                width cursor (index + 1) nextRows [] 0 nextRow location' rest
        | otherwise =
            let characterWidth = terminalCharWidth character
                shouldWrap =
                    characterWidth > 0
                        && ( currentWidth >= width
                            || (currentWidth > 0
                                && currentWidth + characterWidth > width)
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
                (index + 1)
                rows'
                (character : currentRev')
                (currentWidth' + characterWidth)
                currentRow'
                location'
                rest

    finishRow rowsRev currentRev =
        Text.pack (reverse currentRev) : rowsRev

    recordCursor cursor index location = \case
        Nothing
            | cursor == index -> Just location
        previous -> previous

draftCursorLocation :: Text -> Int -> (Int, Int)
draftCursorLocation text cursor =
    let before = Text.take cursor text
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
