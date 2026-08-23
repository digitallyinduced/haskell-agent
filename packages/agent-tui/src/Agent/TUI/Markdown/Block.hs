-- | Renderer-neutral parsing for the Markdown block constructs supported by
-- both terminal renderers.
module Agent.TUI.Markdown.Block
    ( headingParts
    , headingPartsWith
    , bulletParts
    , bulletPartsWith
    , orderedParts
    , blockQuoteRemainder
    , isThematicBreak
    , takeTableRows
    , splitTableRow
    ) where

import Data.Char (isDigit, isSpace)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

headingParts :: Text -> Maybe (Int, Text)
headingParts = headingPartsWith isSpace

headingPartsWith :: (Char -> Bool) -> Text -> Maybe (Int, Text)
headingPartsWith isSeparator line =
    let stripped = Text.stripStart line
        (marks, after) = Text.span (== '#') stripped
        level = Text.length marks
    in if level >= 1
        && level <= 6
        && startsWith isSeparator after
        then Just (level, Text.strip after)
        else Nothing

bulletParts :: Text -> Maybe (Text, Text)
bulletParts = bulletPartsWith isSpace

bulletPartsWith :: (Char -> Bool) -> Text -> Maybe (Text, Text)
bulletPartsWith isSeparator line =
    let (indent, stripped) = Text.span isSpace line
    in case Text.uncons stripped of
        Just (marker, rest)
            | marker `elem` ['-', '*', '+']
            , Just (separator, after) <- Text.uncons rest
            , isSeparator separator ->
                Just (indent, Text.strip after)
        _ -> Nothing

orderedParts :: Text -> Maybe (Text, Text, Text)
orderedParts line =
    let (indent, stripped) = Text.span isSpace line
        (number, rest) = Text.span isDigit stripped
    in if Text.null number
        then Nothing
        else
            (\item -> (indent, number, Text.strip item))
                <$> Text.stripPrefix ". " rest

-- | Remove indentation before the quote marker and the marker itself. The
-- caller chooses how much whitespace after the marker to discard.
blockQuoteRemainder :: Text -> Maybe Text
blockQuoteRemainder =
    Text.stripPrefix ">" . Text.stripStart

isThematicBreak :: Text -> Bool
isThematicBreak line =
    let stripped = Text.filter (not . isSpace) (Text.strip line)
    in Text.length stripped >= 3
        && (Text.all (== '-') stripped
            || Text.all (== '*') stripped
            || Text.all (== '_') stripped)

-- | Parse a complete pipe table from the front of a line sequence. The first
-- row is the header; the separator row is consumed but omitted from the
-- returned rows.
takeTableRows :: [Text] -> Maybe ([[Text]], [Text])
takeTableRows (header : separator : rest)
    | Just headerCells <- splitTableRow header
    , Just separatorCells <- splitTableRow separator
    , length separatorCells == length headerCells
    , all isSeparatorCell separatorCells =
        let (body, after) = span isTableRow rest
        in Just (headerCells : mapMaybe splitTableRow body, after)
takeTableRows _ = Nothing

isTableRow :: Text -> Bool
isTableRow = isJust . splitTableRow

isSeparatorCell :: Text -> Bool
isSeparatorCell cell =
    Text.any (== '-') cell
        && Text.null
            (Text.filter (`notElem` ['-', ':', ' ']) cell)

-- | Split a pipe row on actual column delimiters. Escaped pipes and pipes
-- inside matching backtick spans remain part of their cell.
splitTableRow :: Text -> Maybe [Text]
splitTableRow raw =
    let stripped = Text.strip raw
        content = fromMaybe stripped (Text.stripPrefix "|" stripped)
        (cells, delimiterCount) =
            scan Nothing [] [] 0 False (Text.unpack content)
    in if delimiterCount >= 1 then Just cells else Nothing
  where
    scan
        :: Maybe Int
        -> String
        -> [Text]
        -> Int
        -> Bool
        -> String
        -> ([Text], Int)
    scan _ current cells delimiterCount trailingDelimiter [] =
        let allCells = reverse (finishCell current : cells)
            withoutOuterBorder
                | trailingDelimiter = dropLast allCells
                | otherwise = allCells
        in (withoutOuterBorder, delimiterCount)
    scan codeRun current cells delimiterCount _ ('\\' : rest) =
        let (slashes, afterSlashes) = span (== '\\') rest
            slashCount = 1 + length slashes
            literalSlashes =
                replicate
                    (if startsWithPipe afterSlashes
                        then slashCount `div` 2
                        else slashCount)
                    '\\'
            current' = literalSlashes <> current
        in case afterSlashes of
            '|' : afterPipe
                | odd slashCount ->
                    scan codeRun ('|' : current') cells
                        delimiterCount False afterPipe
                | codeRun == Nothing ->
                    splitCell codeRun current' cells delimiterCount afterPipe
            _ ->
                scan codeRun current' cells
                    delimiterCount False afterSlashes
    scan codeRun current cells delimiterCount _ ('`' : rest) =
        let (ticks, afterTicks) = span (== '`') rest
            tickCount = 1 + length ticks
            marker = replicate tickCount '`'
            nextCodeRun = case codeRun of
                Just openCount
                    | tickCount >= openCount -> Nothing
                Just openCount -> Just openCount
                Nothing
                    | hasClosingRun tickCount afterTicks -> Just tickCount
                Nothing -> Nothing
        in scan nextCodeRun (marker <> current) cells
            delimiterCount False afterTicks
    scan Nothing current cells delimiterCount _ ('|' : rest) =
        splitCell Nothing current cells delimiterCount rest
    scan codeRun current cells delimiterCount _ (character : rest) =
        scan codeRun (character : current) cells
            delimiterCount False rest

    splitCell codeRun current cells delimiterCount rest =
        scan codeRun [] (finishCell current : cells)
            (delimiterCount + 1) True rest

    finishCell = Text.strip . Text.pack . reverse
    startsWithPipe ('|' : _) = True
    startsWithPipe _ = False
    hasClosingRun count =
        Text.isInfixOf (Text.replicate count "`") . Text.pack
    dropLast values = case reverse values of
        _ : rest -> reverse rest
        [] -> []

startsWith :: (Char -> Bool) -> Text -> Bool
startsWith predicate text = case Text.uncons text of
    Just (character, _) -> predicate character
    Nothing -> False
