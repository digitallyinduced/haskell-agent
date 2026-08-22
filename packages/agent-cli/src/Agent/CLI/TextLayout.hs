-- | Text layout helpers shared by fixed-size CLI overlays.
module Agent.CLI.TextLayout
    ( SplitPaneFrame(..)
    , clampSelectionIndex
    , fitTextCell
    , renderSplitPaneFrame
    , selectionWindow
    , transcriptPreviewRows
    ) where

import Data.Text (Text)
import qualified Data.Text as Text

-- | Presentation inputs for a fixed-height, two-column selection overlay.
--
-- The renderer owns the shared layout, selection-window, padding, and
-- transcript-tail behavior. Callers supply domain labels and styling.
data SplitPaneFrame item = SplitPaneFrame
    { splitPaneMinColumns :: !Int
    , splitPaneColumns :: !Int
    , splitPaneBodyRows :: !Int
    , splitPaneLeftMinWidth :: !Int
    , splitPaneLeftMaxWidth :: !Int
    , splitPaneDivider :: !Text
    , splitPaneTitle :: !Text
    , splitPaneHeaderDetail :: !(Int -> Text)
    , splitPaneLeftHeading :: !Text
    , splitPaneRightHeading :: !(Maybe item -> Text)
    , splitPaneItems :: ![item]
    , splitPaneSelectedIndex :: !Int
    , splitPaneLeftLabel :: !(Int -> item -> Text)
    , splitPaneTranscript :: !(item -> [Text])
    , splitPaneEmptyTranscript :: !Text
    , splitPaneFooter :: !Text
    , splitPanePromptStyle :: !(Text -> Text)
    , splitPaneMutedStyle :: !(Text -> Text)
    , splitPaneSelectedStyle :: !(Text -> Text)
    }

renderSplitPaneFrame :: SplitPaneFrame item -> Text
renderSplitPaneFrame frame =
    Text.intercalate "\n" (header : headings : body <> [footer])
  where
    cols = max frame.splitPaneMinColumns frame.splitPaneColumns
    bodyRows = frame.splitPaneBodyRows
    dividerWidth = Text.length frame.splitPaneDivider
    divider = frame.splitPaneMutedStyle frame.splitPaneDivider
    leftWidth =
        max frame.splitPaneLeftMinWidth $
            min frame.splitPaneLeftMaxWidth
                ((cols - dividerWidth) * 2 `div` 5)
    rightWidth = max 1 (cols - leftWidth - dividerWidth)
    items = frame.splitPaneItems
    itemCount = length items
    selectedIndex =
        clampSelectionIndex itemCount frame.splitPaneSelectedIndex
    shown = selectionWindow bodyRows selectedIndex items
    selected = atMay selectedIndex items
    header =
        frame.splitPanePromptStyle frame.splitPaneTitle
            <> frame.splitPaneMutedStyle
                (fitTextCell
                    (max 0 (cols - Text.length frame.splitPaneTitle))
                    (" · " <> frame.splitPaneHeaderDetail itemCount))
    headings =
        frame.splitPanePromptStyle
            (fitTextCell leftWidth frame.splitPaneLeftHeading)
            <> divider
            <> frame.splitPanePromptStyle
                (fitTextCell rightWidth
                    (frame.splitPaneRightHeading selected))
    leftRows =
        map
            (\(absoluteIndex, item) ->
                let prefix =
                        if absoluteIndex == selectedIndex then "› " else "  "
                    text =
                        fitTextCell leftWidth
                            (prefix
                                <> frame.splitPaneLeftLabel absoluteIndex item)
                in if absoluteIndex == selectedIndex
                    then frame.splitPaneSelectedStyle text
                    else text)
            shown
            <> repeat (Text.replicate leftWidth " ")
    rightRows = case selected of
        Nothing ->
            frame.splitPaneMutedStyle
                (fitTextCell rightWidth frame.splitPaneEmptyTranscript)
                : repeat ""
        Just item ->
            map (fitTextCell rightWidth)
                (transcriptPreviewRows
                    rightWidth bodyRows (frame.splitPaneTranscript item))
                <> repeat ""
    body =
        take bodyRows $
            zipWith (\left right -> left <> divider <> right) leftRows rightRows
    footer =
        frame.splitPaneMutedStyle $
            fitTextCell cols frame.splitPaneFooter

atMay :: Int -> [a] -> Maybe a
atMay index values
    | index < 0 = Nothing
    | otherwise = case drop index values of
        value : _ -> Just value
        [] -> Nothing

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
