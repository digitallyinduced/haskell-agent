-- | Text layout helpers shared by fixed-size CLI overlays.
module Agent.CLI.TextLayout
    ( clampSelectionIndex
    , fitTextCell
    , selectionWindow
    , transcriptPreviewRows
    ) where

import Data.Text (Text)
import qualified Data.Text as Text

clampSelectionIndex :: Int -> Int -> Int
clampSelectionIndex count index
    | count <= 0 = 0
    | index < 0 = 0
    | index >= count = count - 1
    | otherwise = index

selectionWindow :: Int -> Int -> [a] -> [(Int, a)]
selectionWindow count selected values =
    let total = length values
        start = max 0 (min selected (total - count))
    in zip [start ..] (take count (drop start values))

transcriptPreviewRows :: Int -> Int -> [Text] -> [Text]
transcriptPreviewRows width count logicalLines =
    let wrapped = concatMap (hardWrapText width) logicalLines
        rows
            | null wrapped = ["(empty transcript)"]
            | otherwise = wrapped
    in drop (max 0 (length rows - count)) rows

hardWrapText :: Int -> Text -> [Text]
hardWrapText width raw
    | Text.null raw = [""]
    | otherwise = go raw
  where
    width' = max 1 width
    go text
        | Text.null text = []
        | otherwise =
            let (line, rest) = Text.splitAt width' text
            in line : go rest

fitTextCell :: Int -> Text -> Text
fitTextCell width raw
    | width <= 0 = ""
    | Text.length clean <= width =
        clean <> Text.replicate (width - Text.length clean) " "
    | width == 1 = "…"
    | otherwise = Text.take (width - 1) clean <> "…"
  where
    clean =
        Text.map
            (\c -> if c == '\t' || c == '\r' || c == '\n' then ' ' else c)
            raw
