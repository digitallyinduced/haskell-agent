-- | Turn CommonMark-ish assistant text into ANSI-styled terminal output.
module Agent.CLI.Markdown
    ( MarkdownFragmentSplit(..)
    , renderMarkdown
    , renderMarkdownAtWidth
    , renderMarkdownFragment
    , splitMarkdownFragment
    ) where

import Agent.CLI.Style
    ( agentBackground
    , osc8Link
    , terminalBlue
    , terminalCyan
    , terminalGreen
    , terminalMagenta
    , terminalMuted
    , terminalViolet
    , terminalYellow
    , styleBase
    )
import Agent.TUI.FencedCode
    ( FenceChunk(..)
    , FencedBlock(..)
    , fenceChunks
    )
import Agent.TUI.Markdown.Inline
    ( Inline(..)
    , inlinePlainText
    , parseInline
    )
import qualified Agent.TUI.Markdown.Block as Block
import Agent.TUI.TextWidth
    ( displayTerminalText
    , graphemeCellWidth
    , graphemeClusters
    )
import Data.Char (isAlphaNum, isAscii, isSpace)
import Data.List (intersperse, transpose)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import System.Console.ANSI
    ( ConsoleIntensity(..)
    , SGR(..)
    , Underlining(..)
    )

-- | When @color@ is 'True', style a useful subset of GFM for a TTY.
-- When 'False', return @text@ unchanged (pipes, redirects, tests).
-- Nested spans restore the terminal-owned 'agentBackground' after each 'Reset'.
renderMarkdown :: Bool -> Text -> Text
renderMarkdown color = renderMarkdownWithin color Nothing

-- | Render Markdown while constraining table rows to a terminal width.
renderMarkdownAtWidth :: Bool -> Int -> Text -> Text
renderMarkdownAtWidth color width =
    renderMarkdownWithin color (Just (max 1 width))

renderMarkdownWithin :: Bool -> Maybe Int -> Text -> Text
renderMarkdownWithin color availableWidth text
    | not color = text
    | otherwise = Text.concat (map renderChunk (fenceChunks cleaned))
  where
    cleaned = displayTerminalText text
    renderChunk (FenceText prose) = renderProse availableWidth prose
    renderChunk (FenceBlock block) =
        renderFenceBody block.fencedBody

renderProse :: Maybe Int -> Text -> Text
renderProse availableWidth text =
    let endsWithNewline = Text.isSuffixOf "\n" text
        lines_ = Text.splitOn "\n" text
        linesForParse
            | endsWithNewline && not (null lines_) = init lines_
            | otherwise = lines_
        rendered =
            Text.intercalate "\n"
                (renderBlocks availableWidth linesForParse)
    in if endsWithNewline then rendered <> "\n" else rendered

renderFenceBody :: Text -> Text
renderFenceBody body =
    let endsWithNewline = Text.isSuffixOf "\n" body
        lines_ = Text.splitOn "\n" body
        actualLines
            | endsWithNewline && not (null lines_) = init lines_
            | otherwise = lines_
        rendered =
            Text.intercalate "\n"
                (map (md [terminalMuted]) actualLines)
    in if endsWithNewline then rendered <> "\n" else rendered

-- | Style helper that restores the agent's terminal-default background.
md :: [SGR] -> Text -> Text
md = styleBase True agentBackground

renderBlocks :: Maybe Int -> [Text] -> [Text]
renderBlocks availableWidth = go
  where
    go [] = []
    go (line : rest)
        | Just (table, after) <- Block.takeTableRows (line : rest) =
            renderTable availableWidth table ++ go after
        | Just (level, title) <- Block.headingParts line =
            -- Hide the markdown `#` markers (grok pretty mode); color the title.
            renderInlineWith (headingPrefixStyle level) (parseInline title)
                : go rest
        | Just (indent, item) <- Block.bulletParts line =
            ( indent
                <> md listMarkerStyle "• "
                <> styleInline item
            )
                : go rest
        | Just (indent, digits, item) <- Block.orderedParts line =
            ( indent
                <> md listMarkerStyle (digits <> ". ")
                <> styleInline item
            )
                : go rest
        | Just quote <- Block.blockQuoteRemainder line =
            let body =
                    renderInlineWith quoteStyle
                        (parseInline (Text.stripStart quote))
            in (md [terminalMuted] "│ " <> body)
                : go rest
        | Block.isThematicBreak line =
            md [terminalMuted] (Text.replicate 40 "─")
                : go rest
        | Text.null (Text.strip line) = line : go rest
        | otherwise = styleInline line : go rest

-- | Heading title style: bold + level color. Markdown `#` markers are hidden.
headingPrefixStyle :: Int -> [SGR]
headingPrefixStyle level =
    SetConsoleIntensity BoldIntensity : headingColor level

headingColor :: Int -> [SGR]
headingColor = \case
    1 -> [terminalMagenta]
    2 -> [terminalCyan]
    3 -> [terminalBlue]
    4 -> [terminalYellow]
    5 -> [terminalGreen]
    _ -> [terminalViolet]

listMarkerStyle :: [SGR]
listMarkerStyle =
    [ SetConsoleIntensity BoldIntensity
    , terminalCyan
    ]

-- | Blockquote body: muted so it sits behind surrounding prose.
quoteStyle :: [SGR]
quoteStyle = [terminalMuted]

inlineCodeStyle :: [SGR]
inlineCodeStyle =
    [ SetConsoleIntensity BoldIntensity
    , terminalCyan
    ]

inlineLinkStyle :: [SGR]
inlineLinkStyle =
    [ terminalBlue
    , SetUnderlining SingleUnderline
    ]

inlineUrlStyle :: [SGR]
inlineUrlStyle = [terminalMuted]

safeInlineUrl :: Text -> Bool
safeInlineUrl url =
    not (Text.null url)
        && displayTerminalText url == url

data TableFragment = TableFragment
    { tableFragmentStyles :: ![SGR]
    , tableFragmentUrl :: !(Maybe Text)
    , tableFragmentText :: !Text
    }

renderTable :: Maybe Int -> Block.MarkdownTable -> [Text]
renderTable availableWidth table = case table.tableRows of
    [] -> []
    (headerCells : bodyCells) ->
        let columnCount = length headerCells
            normalize cells = take columnCount (cells <> repeat "")
            normalizedHeader = normalize headerCells
            normalizedBody = map normalize bodyCells
            normalizedRows = normalizedHeader : normalizedBody
            styledRows =
                [ [ tableCellFragments
                        (if rowIndex == 0
                            then [SetConsoleIntensity BoldIntensity]
                            else [])
                        cell
                  | cell <- row
                  ]
                | (rowIndex, row) <-
                    zip [0 :: Int ..] normalizedRows
                ]
            naturalWidths =
                map
                    (maximum . (1 :) . map tableFragmentsWidth)
                    (transpose styledRows)
            minimumWidths =
                map
                    (maximum . (1 :) . map tableFragmentsMinimumWidth)
                    (transpose styledRows)
            alignments = table.tableAlignments
            borderWidth = columnCount + 1
            gridMinimumWidth = borderWidth + sum minimumWidths
            paddedGridMinimumWidth =
                gridMinimumWidth + 2 * columnCount
            renderGrid horizontalPadding widths =
                let top =
                        md [terminalMuted] $
                            tableBorder
                                horizontalPadding '┌' '┬' '┐' widths
                    divider =
                        md [terminalMuted] $
                            tableBorder
                                horizontalPadding '├' '┼' '┤' widths
                    bottom =
                        md [terminalMuted] $
                            tableBorder
                                horizontalPadding '└' '┴' '┘' widths
                    logicalRows =
                        map
                            (renderTableLogicalRow
                                alignments horizontalPadding widths)
                            styledRows
                in top
                    : (concat (intersperse [divider] logicalRows)
                        <> [bottom])
        in case availableWidth of
            Nothing -> renderGrid 1 naturalWidths
            Just requestedWidth
                | available < gridMinimumWidth ->
                    renderCompactTable available styledRows
                | otherwise ->
                    let horizontalPadding =
                            if available >= paddedGridMinimumWidth
                                then 1
                                else 0
                        chromeWidth =
                            borderWidth
                                + 2 * horizontalPadding * columnCount
                        contentBudget =
                            max 0 (available - chromeWidth)
                        widths =
                            fitTableColumnWidths
                                contentBudget minimumWidths naturalWidths
                    in renderGrid horizontalPadding widths
              where
                available = max 1 requestedWidth

tableBorder :: Int -> Char -> Char -> Char -> [Int] -> Text
tableBorder horizontalPadding left middle right widths =
    Text.singleton left
        <> Text.intercalate
            (Text.singleton middle)
            [ Text.replicate
                (width + 2 * horizontalPadding)
                "─"
            | width <- widths
            ]
        <> Text.singleton right

fitTableColumnWidths :: Int -> [Int] -> [Int] -> [Int]
fitTableColumnWidths budget minimumWidths naturalWidths
    | null preferred = []
    | sum preferred <= budget = preferred
    | otherwise =
        grow minimum (max 0 (budget - sum minimum))
  where
    minimum = map (max 1) minimumWidths
    preferred =
        zipWith max
            (minimum <> repeat 1)
            (map (max 1) naturalWidths)

    grow widths remaining
        | remaining <= 0 = widths
        | otherwise =
            let (remaining', widths') =
                    List.mapAccumL grant remaining (zip preferred widths)
            in if remaining' == remaining
                then widths
                else grow widths' remaining'

    grant remaining (wanted, current)
        | remaining > 0
        , current < wanted =
            (remaining - 1, current + 1)
        | otherwise =
            (remaining, current)

renderCompactTable :: Int -> [[[TableFragment]]] -> [Text]
renderCompactTable width =
    concatMap $
        map renderTableFragments
            . wrapTableFragments width
            . List.intercalate [tableSeparator]
  where
    tableSeparator =
        TableFragment [terminalMuted] Nothing " │ "

renderTableLogicalRow
    :: [Block.TableAlignment]
    -> Int
    -> [Int]
    -> [[TableFragment]]
    -> [Text]
renderTableLogicalRow alignments horizontalPadding widths cells =
    map renderPhysicalRow physicalRows
  where
    border = md [terminalMuted] "│"
    normalizedAlignments = alignments <> repeat Block.AlignDefault
    wrappedCells =
        zipWith
            (\width fragments ->
                wrapTableFragments (max 1 width) fragments)
            widths
            (cells <> repeat [])
    rowHeight = maximum (1 : map length wrappedCells)
    physicalRows =
        transpose
            [ take rowHeight (wrapped <> repeat [])
            | wrapped <- wrappedCells
            ]
    renderPhysicalRow fragments =
        border
            <> Text.intercalate
                border
                [ renderTableCell
                    alignment horizontalPadding width cellFragments
                | (alignment, width, cellFragments) <-
                    zip3 normalizedAlignments widths fragments
                ]
            <> border

renderTableCell
    :: Block.TableAlignment
    -> Int
    -> Int
    -> [TableFragment]
    -> Text
renderTableCell alignment horizontalPadding width fragments =
    Text.replicate (horizontalPadding + leftPadding) " "
        <> renderTableFragments fragments
        <> Text.replicate (rightPadding + horizontalPadding) " "
  where
    padding = max 0 (width - tableFragmentsWidth fragments)
    (leftPadding, rightPadding) =
        alignmentPadding alignment padding

tableCellFragments :: [SGR] -> Text -> [TableFragment]
tableCellFragments base = concatMap (go base Nothing) . parseInline
  where
    go context link = \case
        InlineText text -> one context link text
        InlineCode text -> one (context <> inlineCodeStyle) link text
        InlineStrong children ->
            concatMap
                (go (context <> [SetConsoleIntensity BoldIntensity]) link)
                children
        InlineEmphasis children ->
            concatMap
                (go (context <> [SetItalicized True]) link)
                children
        InlineLink url children ->
            let linkTarget
                    | safeInlineUrl url = Just url
                    | otherwise = link
                label =
                    concatMap
                        (go (context <> inlineLinkStyle) linkTarget)
                        children
                suffix
                    | Text.null url || inlinePlainText children == url = []
                    | otherwise =
                        one
                            (context <> inlineUrlStyle)
                            linkTarget
                            (" (" <> url <> ")")
            in label <> suffix

    one styles link text =
        [TableFragment styles link text]

renderTableFragments :: [TableFragment] -> Text
renderTableFragments = go
  where
    go [] = ""
    go (fragment : rest) =
        case fragment.tableFragmentUrl of
            Nothing -> renderFragment fragment <> go rest
            Just url ->
                let (linked, after) =
                        span
                            (\next ->
                                next.tableFragmentUrl == Just url)
                            rest
                in osc8Link True url
                    (Text.concat (map renderFragment (fragment : linked)))
                        <> go after

    renderFragment fragment
        | null fragment.tableFragmentStyles =
            fragment.tableFragmentText
        | otherwise =
            md
                fragment.tableFragmentStyles
                fragment.tableFragmentText

tableFragmentsWidth :: [TableFragment] -> Int
tableFragmentsWidth =
    sum . map (terminalDisplayWidth . (.tableFragmentText))

tableFragmentsMinimumWidth :: [TableFragment] -> Int
tableFragmentsMinimumWidth fragments =
    maximum
        (1
            : [ graphemeCellWidth cluster
              | fragment <- fragments
              , cluster <- graphemeClusters fragment.tableFragmentText
              ])

type StyledTableCell = ([SGR], Maybe Text, Text, Int)

wrapTableFragments :: Int -> [TableFragment] -> [[TableFragment]]
wrapTableFragments width fragments =
    map groupStyledTableCells (wrapCells (styledTableCells fragments))
  where
    width' = max 1 width

    wrapCells [] = [[]]
    wrapCells remaining =
        let (fitting, overflow) = takeFitting remaining
        in case overflow of
            [] -> [dropTrailingSpace fitting]
            _ ->
                case lastSpaceIndex fitting of
                    Just index
                        | index > 0 ->
                            let (line, carried) = splitAt index fitting
                                next =
                                    dropWhile styledTableCellIsSpace
                                        (carried <> overflow)
                            in dropTrailingSpace line : wrapCells next
                    _ -> fitting : wrapCells overflow

    takeFitting = go 0 []
      where
        go _ taken [] = (reverse taken, [])
        go used taken allCells@(cell@(_, _, _, cellWidth) : rest)
            | used > 0
            , used + cellWidth > width' =
                (reverse taken, allCells)
            | otherwise =
                go (used + cellWidth) (cell : taken) rest

    lastSpaceIndex =
        List.foldl'
            (\found (index, cell) ->
                if styledTableCellIsSpace cell
                    then Just index
                    else found)
            Nothing
            . zip [0 :: Int ..]

    dropTrailingSpace =
        reverse . dropWhile styledTableCellIsSpace . reverse

styledTableCells :: [TableFragment] -> [StyledTableCell]
styledTableCells fragments =
    [ ( fragment.tableFragmentStyles
      , fragment.tableFragmentUrl
      , cluster
      , graphemeCellWidth cluster
      )
    | fragment <- fragments
    , cluster <- graphemeClusters fragment.tableFragmentText
    ]

styledTableCellIsSpace :: StyledTableCell -> Bool
styledTableCellIsSpace (_, _, cluster, _) =
    not (Text.null cluster) && Text.all isSpace cluster

groupStyledTableCells :: [StyledTableCell] -> [TableFragment]
groupStyledTableCells =
    reverse . List.foldl' appendCell []
  where
    appendCell grouped (styles, url, cluster, _) =
        case grouped of
            previous : rest
                | previous.tableFragmentStyles == styles
                , previous.tableFragmentUrl == url ->
                    previous
                        { tableFragmentText =
                            previous.tableFragmentText <> cluster
                        }
                        : rest
            _ -> TableFragment styles url cluster : grouped

alignmentPadding :: Block.TableAlignment -> Int -> (Int, Int)
alignmentPadding alignment padding = case alignment of
    Block.AlignRight -> (padding, 0)
    Block.AlignCenter -> (padding `div` 2, padding - padding `div` 2)
    Block.AlignLeft -> (0, padding)
    Block.AlignDefault -> (0, padding)

terminalDisplayWidth :: Text -> Int
terminalDisplayWidth =
    sum
        . map graphemeCellWidth
        . graphemeClusters
        . displayTerminalText

styleInline :: Text -> Text
styleInline = renderInlineWith [] . parseInline

-- | Render an inline markdown fragment whose first character may continue
-- prose emitted by an earlier streaming chunk.
renderMarkdownFragment :: Bool -> Maybe Char -> Text -> Text
renderMarkdownFragment color prevChar text
    | not color = text
    | otherwise =
        renderInlineWith [] $
            parseInline $
                protectLeadingUnderscore prevChar (displayTerminalText text)
  where
    protectLeadingUnderscore previous input
        | maybe False isWordChar previous
        , Text.isPrefixOf "_" input =
            "\\" <> input
        | otherwise = input

renderInlineWith :: [SGR] -> [Inline] -> Text
renderInlineWith base = Text.concat . map (go base)
  where
    go context = \case
        InlineText text -> styled context text
        InlineCode text -> styled (context <> inlineCodeStyle) text
        InlineStrong children ->
            renderInlineWith
                (context <> [SetConsoleIntensity BoldIntensity])
                children
        InlineEmphasis children ->
            renderInlineWith (context <> [SetItalicized True]) children
        InlineLink url children ->
            let label =
                    renderInlineWith (context <> inlineLinkStyle) children
                suffix
                    | Text.null url || inlinePlainText children == url = ""
                    | otherwise =
                        styled
                            (context <> inlineUrlStyle)
                            (" (" <> url <> ")")
                displayedLink = label <> suffix
            in if safeInlineUrl url
                then osc8Link True url displayedLink
                else displayedLink

    styled [] value = value
    styled styles value = md styles value

-- | Split an inline markdown stream into a prefix that can be rendered without
-- future input and a suffix beginning at a possibly incomplete construct.
--
-- This lets the TUI remain append-only while holding delimiters such as @**@
-- until their closing delimiter arrives. A newline makes an unmatched inline
-- construct literal because the renderer does not span inline markup across
-- lines.
data MarkdownFragmentSplit = MarkdownFragmentSplit
    { markdownReady :: !Text
    , markdownPending :: !Text
    , markdownPrevChar :: !(Maybe Char)
    }
    deriving (Eq, Show)

data MarkdownLink = MarkdownLink
    { markdownLinkText :: !Text
    , markdownLinkUrl :: !Text
    , markdownLinkRest :: !Text
    }
    deriving (Eq, Show)

splitMarkdownFragment :: Maybe Char -> Text -> MarkdownFragmentSplit
splitMarkdownFragment initialPrev = go initialPrev []
  where
    go prev chunks t
        | Text.null t =
            MarkdownFragmentSplit
                { markdownReady = Text.concat (reverse chunks)
                , markdownPending = ""
                , markdownPrevChar = prev
                }
        | Just (escaped, rest) <- takeEscapedPunctuation t =
            consume (Just escaped) rest
        | Just (code, rest) <- takeInlineCode t =
            consume (lastChar code) rest
        | Just MarkdownLink{markdownLinkRest = rest} <- takeLink t =
            consume (Just ')') rest
        | Just (inner, rest) <- takeEmphasis prev "**" t =
            consume (lastChar inner) rest
        | Just (inner, rest) <- takeEmphasis prev "__" t =
            consume (lastChar inner) rest
        | Just (inner, rest) <- takeEmphasis prev "*" t =
            consume (lastChar inner) rest
        | Just (inner, rest) <- takeEmphasis prev "_" t =
            consume (lastChar inner) rest
        | shouldWait prev t =
            MarkdownFragmentSplit
                { markdownReady = Text.concat (reverse chunks)
                , markdownPending = t
                , markdownPrevChar = prev
                }
        | otherwise =
            let (plain, rest) =
                    Text.break (`elem` ['\\', '`', '[', '*', '_']) t
            in if Text.null plain
                then
                    let literal = Text.take 1 t
                    in go (lastChar literal) (literal : chunks) (Text.drop 1 t)
                else go (lastChar plain) (plain : chunks) rest
      where
        consume nextPrev rest =
            let consumedLength = Text.length t - Text.length rest
                consumed = Text.take consumedLength t
            in go nextPrev (consumed : chunks) rest

    shouldWait prev t
        | Text.any (== '\n') t = False
        | t == "\\" = True
        | Text.isPrefixOf "`" t = True
        | Text.isPrefixOf "[" t = incompleteLink t
        | Text.isPrefixOf "**" t = potentialEmphasis prev "**" t
        | Text.isPrefixOf "__" t = potentialEmphasis prev "__" t
        | Text.isPrefixOf "*" t = potentialEmphasis prev "*" t
        | Text.isPrefixOf "_" t = potentialEmphasis prev "_" t
        | otherwise = False

    incompleteLink t =
        case linkLabelEnd (Text.drop 1 t) of
            Nothing -> True
            Just afterBracket ->
                Text.null afterBracket
                    || case Text.stripPrefix "(" afterBracket of
                        Nothing -> False
                        Just destination -> destinationEnd destination == Nothing

    potentialEmphasis prev delim t =
        let after = Text.drop (Text.length delim) t
        in case Text.uncons after of
            Nothing -> True
            Just (first, _)
                | isSpace first -> False
                | delim == "_" || delim == "__" ->
                    maybe True (not . isWordChar) prev
                | otherwise -> True

    lastChar value = snd <$> Text.unsnoc value

takeEscapedPunctuation :: Text -> Maybe (Char, Text)
takeEscapedPunctuation text = do
    afterSlash <- Text.stripPrefix "\\" text
    (character, rest) <- Text.uncons afterSlash
    if isAscii character
        && character >= '!'
        && character <= '~'
        && not (isAlphaNum character)
        then Just (character, rest)
        else Nothing

takeInlineCode :: Text -> Maybe (Text, Text)
takeInlineCode t =
    let (ticks, afterOpen) = Text.span (== '`') t
        n = Text.length ticks
    in if n == 0 then Nothing else findClose n afterOpen
  where
    findClose n body =
        case Text.break (== '`') body of
            (_, rest) | Text.null rest -> Nothing
            (before, rest) ->
                let (closeRun, afterClose) = Text.span (== '`') rest
                    closeLen = Text.length closeRun
                in if closeLen == n
                    then Just (before, afterClose)
                    else do
                        (code, rest') <- findClose n afterClose
                        Just (before <> closeRun <> code, rest')

takeLink :: Text -> Maybe MarkdownLink
takeLink t = do
    afterBracket <- Text.stripPrefix "[" t
    (linkText, afterBracketClose) <- takeLinkLabel afterBracket
    afterParen <- Text.stripPrefix "(" afterBracketClose
    (url, rest) <- takeLinkDestination afterParen
    if Text.null linkText || Text.null url
        then Nothing
        else Just MarkdownLink
            { markdownLinkText = linkText
            , markdownLinkUrl = url
            , markdownLinkRest = rest
            }

linkLabelEnd :: Text -> Maybe Text
linkLabelEnd = fmap snd . takeLinkLabel

takeLinkLabel :: Text -> Maybe (Text, Text)
takeLinkLabel = go 0 ""
  where
    go :: Int -> Text -> Text -> Maybe (Text, Text)
    go depth consumed remaining =
        case Text.uncons remaining of
            Nothing -> Nothing
            Just ('\n', _) -> Nothing
            Just ('\\', afterSlash) ->
                case Text.uncons afterSlash of
                    Nothing -> Nothing
                    Just (escaped, rest) ->
                        go depth
                            (consumed <> Text.pack ['\\', escaped])
                            rest
            Just ('[', rest) ->
                go (depth + 1) (consumed <> "[") rest
            Just (']', rest)
                | depth == 0 -> Just (consumed, rest)
                | otherwise ->
                    go (depth - 1) (consumed <> "]") rest
            Just (character, rest) ->
                go depth (consumed <> Text.singleton character) rest

destinationEnd :: Text -> Maybe Text
destinationEnd = fmap snd . takeLinkDestination

takeLinkDestination :: Text -> Maybe (Text, Text)
takeLinkDestination = go 0 ""
  where
    go :: Int -> Text -> Text -> Maybe (Text, Text)
    go depth consumed remaining =
        case Text.uncons remaining of
            Nothing -> Nothing
            Just ('\n', _) -> Nothing
            Just ('\\', afterSlash) ->
                case Text.uncons afterSlash of
                    Nothing -> Nothing
                    Just (escaped, rest) ->
                        go depth
                            (consumed <> Text.pack ['\\', escaped])
                            rest
            Just ('(', rest) ->
                go (depth + 1) (consumed <> "(") rest
            Just (')', rest)
                | depth == 0 -> Just (consumed, rest)
                | otherwise ->
                    go (depth - 1) (consumed <> ")") rest
            Just (character, rest) ->
                go depth (consumed <> Text.singleton character) rest

takeEmphasis :: Maybe Char -> Text -> Text -> Maybe (Text, Text)
takeEmphasis prevChar delim t
    | not (delim `Text.isPrefixOf` t) = Nothing
    | not (canOpen prevChar delim (Text.drop (Text.length delim) t)) = Nothing
    | otherwise = do
        afterOpen <- Text.stripPrefix delim t
        case Text.breakOn delim afterOpen of
            (inner, rest)
                | not (Text.null inner)
                , not (Text.any (== '\n') inner)
                , Just afterClose <- Text.stripPrefix delim rest
                , canClose delim inner afterClose ->
                    Just (inner, afterClose)
            _ -> Nothing

canOpen :: Maybe Char -> Text -> Text -> Bool
canOpen prevChar delim after =
    case Text.uncons after of
        Nothing -> False
        Just (first, _)
            | isSpace first -> False
            | delim == "_" || delim == "__" ->
                maybe True (not . isWordChar) prevChar
            | otherwise -> True

canClose :: Text -> Text -> Text -> Bool
canClose delim inner after =
    case Text.unsnoc inner of
        Nothing -> False
        Just (_, lastChar)
            | isSpace lastChar -> False
            | delim == "_" || delim == "__" ->
                case Text.uncons after of
                    Just (c, _) -> not (isWordChar c)
                    Nothing -> True
            | otherwise -> True

isWordChar :: Char -> Bool
isWordChar c = isAlphaNum c || c == '_'
