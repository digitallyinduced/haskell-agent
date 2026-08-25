-- | Width-aware rendering helpers for inline terminal input.
module Agent.CLI.Input.Display
    ( terminalTextWidth
    , terminalCharWidth
    , displayCells
    , displayCell
    , safeDisplayChar
    , textColumns
    , charColumns
    , cellsWidth
    , renderCells
    , takeColumns
    , takeSuffixColumns
    , visibleEditorText
    , displayEditorText
    , truncateDisplayText
    ) where

import Agent.CLI.Input.Types (DisplayCell(..))
import Agent.TUI.TextWidth
    ( charCellWidth
    , displayTerminalText
    , graphemeCellWidth
    , graphemeClusters
    )
import Data.Text (Text)
import qualified Data.Text as Text

terminalTextWidth :: Text -> Int
terminalTextWidth = cellsWidth . displayCells

terminalCharWidth :: Char -> Int
terminalCharWidth = (.displayCellWidth) . displayCell

displayCells :: Text -> [DisplayCell]
displayCells = map displayCluster . graphemeClusters

displayCell :: Char -> DisplayCell
displayCell = displayCluster . Text.singleton

displayCluster :: Text -> DisplayCell
displayCluster cluster =
    let lineSafe =
            Text.concatMap
                (\character ->
                    if character == '\n'
                        then "↵"
                        else Text.singleton character)
                cluster
        shown = displayTerminalText lineSafe
    in DisplayCell
        { displayCellText = shown
        , displayCellWidth = textColumns shown
        , displayCellSourceLength = Text.length cluster
        }

safeDisplayChar :: Char -> Text
safeDisplayChar char
    | char == '\n' = "↵"
    | otherwise = displayTerminalText (Text.singleton char)

textColumns :: Text -> Int
textColumns = sum . map graphemeCellWidth . graphemeClusters

charColumns :: Char -> Int
charColumns = charCellWidth

cellsWidth :: [DisplayCell] -> Int
cellsWidth = sum . map (.displayCellWidth)

renderCells :: [DisplayCell] -> Text
renderCells = Text.concat . map (.displayCellText)

takeColumns :: Int -> [DisplayCell] -> [DisplayCell]
takeColumns width = go 0
  where
    go _ [] = []
    go used (cell : rest)
        | used + cell.displayCellWidth > width = []
        | otherwise = cell : go (used + cell.displayCellWidth) rest

takeSuffixColumns :: Int -> [DisplayCell] -> [DisplayCell]
takeSuffixColumns width =
    dropWhile ((== 0) . (.displayCellWidth))
        . reverse
        . takeColumns width
        . reverse

visibleEditorText :: Int -> Text -> Int -> (Text, Int)
visibleEditorText available raw requestedCursor
    | available <= 0 = ("", 0)
    | otherwise =
        let cells = fitCells False (displayCells raw)
            cursor = max 0 (min (Text.length raw) requestedCursor)
            before = cellsBeforeCursor cursor cells
        in if cellsWidth cells <= available
            then (renderCells cells, cellsWidth before)
            else
                let leftRoom = max 1 (available * 2 `div` 3)
                    preferredBefore = takeSuffixColumns leftRoom before
                    -- A wide glyph may not fit in the preferred left-hand
                    -- budget. If it fits in the full viewport, keep it
                    -- rather than dropping all text before the cursor.
                    visibleBefore =
                        if null preferredBefore
                            then takeSuffixColumns available before
                            else preferredBefore
                    start = length before - length visibleBefore
                    shownCells = takeColumns available (drop start cells)
                in (renderCells shownCells, cellsWidth visibleBefore)
  where
    -- A single terminal glyph can occupy two cells, so it cannot be shown
    -- verbatim in a one-cell viewport. Keep a visible placeholder and,
    -- crucially, preserve the cursor's cell position.
    fitCells _ [] = []
    fitCells suppressCombining (cell : rest)
        | cell.displayCellWidth > available =
            cell
                { displayCellText = "…"
                , displayCellWidth = 1
                }
                : fitCells True rest
        | suppressCombining
        , cell.displayCellWidth == 0 =
            cell { displayCellText = "" } : fitCells True rest
        | otherwise =
            cell : fitCells False rest

    cellsBeforeCursor cursor = go 0
      where
        go _ [] = []
        go offset (cell : rest)
            | offset + cell.displayCellSourceLength <= cursor =
                cell : go
                    (offset + cell.displayCellSourceLength)
                    rest
            | otherwise = []

displayEditorText :: Text -> Text
displayEditorText = renderCells . displayCells

truncateDisplayText :: Int -> Text -> Text
truncateDisplayText width text
    | width <= 0 = ""
    | cellsWidth cells <= width = renderCells cells
    | width == 1 = "…"
    | otherwise = renderCells (takeColumns (width - 1) cells) <> "…"
  where
    cells = displayCells text
