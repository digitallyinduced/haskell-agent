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
import Agent.TUI.TextWidth (charCellWidth)
import Data.Char (GeneralCategory(..), chr, generalCategory, ord)
import Data.Text (Text)
import qualified Data.Text as Text

terminalTextWidth :: Text -> Int
terminalTextWidth = cellsWidth . displayCells

terminalCharWidth :: Char -> Int
terminalCharWidth = (.displayCellWidth) . displayCell

displayCells :: Text -> [DisplayCell]
displayCells = map displayCell . Text.unpack

displayCell :: Char -> DisplayCell
displayCell char =
    let shown = safeDisplayChar char
    in DisplayCell
        { displayCellText = shown
        , displayCellWidth = textColumns shown
        }

safeDisplayChar :: Char -> Text
safeDisplayChar char
    | char == '\n' || char == '\r' = "↵"
    | char == '\t' = "⇥"
    | code >= 0 && code <= 0x1f = Text.singleton (chr (0x2400 + code))
    | code == 0x7f = "␡"
    | code >= 0x80 && code <= 0x9f = "�"
    | generalCategory char == Format = "�"
    | otherwise = Text.singleton char
  where
    code = ord char

textColumns :: Text -> Int
textColumns = sum . map charColumns . Text.unpack

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
takeSuffixColumns width = reverse . takeColumns width . reverse

visibleEditorText :: Int -> Text -> Int -> (Text, Int)
visibleEditorText available raw cursor =
    let cells = displayCells raw
        before = take cursor cells
    in if cellsWidth cells <= available
        then (renderCells cells, cellsWidth before)
        else
            let leftRoom = max 1 (available * 2 `div` 3)
                visibleBefore = takeSuffixColumns leftRoom before
                start = length before - length visibleBefore
                shownCells = takeColumns available (drop start cells)
            in (renderCells shownCells, cellsWidth visibleBefore)

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
