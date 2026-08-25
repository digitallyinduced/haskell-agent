-- | Text layout helpers shared by fixed-size CLI overlays.
module Agent.CLI.TextLayout
    ( SplitPaneFrame(..)
    , clampSelectionIndex
    , fitTextCell
    , renderSplitPaneFrame
    , selectionWindow
    , transcriptPreviewRows
    ) where

import Agent.CLI.Input
    ( displayEditorText
    , terminalTextWidth
    , truncateDisplayText
    )
import Agent.CLI.Input.Display (displayCells)
import Agent.CLI.Input.Types (DisplayCell(..))
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
    cols = max 0 (max frame.splitPaneMinColumns frame.splitPaneColumns)
    bodyRows = max 0 frame.splitPaneBodyRows
    dividerText = truncateDisplayText cols frame.splitPaneDivider
    dividerWidth = terminalTextWidth dividerText
    divider = frame.splitPaneMutedStyle dividerText
    paneWidth = max 0 (cols - dividerWidth)
    desiredLeftWidth =
        max frame.splitPaneLeftMinWidth $
            min frame.splitPaneLeftMaxWidth
                (paneWidth * 2 `div` 5)
    leftWidth = max 0 (min paneWidth desiredLeftWidth)
    rightWidth = paneWidth - leftWidth
    items = frame.splitPaneItems
    itemCount = length items
    selectedIndex =
        clampSelectionIndex itemCount frame.splitPaneSelectedIndex
    shown = selectionWindow bodyRows selectedIndex items
    selected = atMay selectedIndex items
    fittedTitle = truncateDisplayText cols frame.splitPaneTitle
    header =
        frame.splitPanePromptStyle fittedTitle
            <> frame.splitPaneMutedStyle
                (fitTextCell
                    (max 0 (cols - terminalTextWidth fittedTitle))
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
                : repeat (Text.replicate rightWidth " ")
        Just item ->
            map (fitTextCell rightWidth)
                (transcriptPreviewRows
                    rightWidth bodyRows (frame.splitPaneTranscript item))
                <> repeat (Text.replicate rightWidth " ")
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
    | otherwise = go [] 0 (displayCells raw)
  where
    width' = max 1 width
    go currentRev _ [] =
        [Text.concat (reverse currentRev) | not (null currentRev)]
    go currentRev used input@(cell : rest)
        | cell.displayCellWidth > width' =
            let replacement =
                    truncateDisplayText width' cell.displayCellText
                prefix =
                    [ Text.concat (reverse currentRev)
                    | not (null currentRev)
                    ]
            in prefix <> (replacement : go [] 0 rest)
        | cell.displayCellWidth > 0
            && used > 0
            && used + cell.displayCellWidth > width' =
                Text.concat (reverse currentRev) : go [] 0 input
        | otherwise =
            go
                (cell.displayCellText : currentRev)
                (used + cell.displayCellWidth)
                rest

fitTextCell :: Int -> Text -> Text
fitTextCell width raw
    | width <= 0 = ""
    | otherwise =
        fitted
            <> Text.replicate
                (max 0 (width - terminalTextWidth fitted))
                " "
  where
    clean =
        displayEditorText $
            Text.map
                (\c -> if c == '\t' || c == '\r' || c == '\n' then ' ' else c)
                raw
    fitted = truncateDisplayText width clean
