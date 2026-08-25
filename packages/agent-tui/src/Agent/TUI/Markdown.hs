-- | Lightweight fullscreen Markdown block rendering.
module Agent.TUI.Markdown
    ( Inline(..)
    , codeWidgetWithSyntaxHighlighting
    , inlinePlainText
    , markdownWidget
    , markdownWidgetWithLinks
    , markdownWidgetWithCodeControls
    , markdownWidgetWithSyntaxHighlighting
    , markdownWidgetWithSyntaxHighlightingAndLinks
    , parseInline
    ) where

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
import Agent.Syntax
    ( SyntaxHighlighter
    , SyntaxSpan(..)
    , highlightCode
    )
import Agent.TUI.TextWidth
    ( displayTerminalText
    , graphemeCellWidth
    , graphemeClusters
    , terminalTextImage
    )
import qualified Agent.TUI.Theme as Theme
import Brick
import qualified Brick.Types as B
import Data.Bits ((.|.))
import Data.Char (isSpace)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as Builder
import qualified Graphics.Vty as V

markdownWidget :: Ord n => Text -> Widget n
markdownWidget =
    markdownWidgetWithCodeControls \_ language ->
        if Text.null language
            then emptyWidget
            else withAttr Theme.mutedAttr
                (txt (displayTerminalText language))

-- | Render Markdown with application-level click targets for links.
markdownWidgetWithLinks
    :: Ord n
    => (Text -> n)
    -> Text
    -> Widget n
markdownWidgetWithLinks linkName =
    markdownWidgetWithSyntaxHighlightingAndLinks
        Nothing
        linkName
        (\_ widget -> widget)
        (\_ language ->
            if Text.null language
                then emptyWidget
                else withAttr Theme.mutedAttr
                    (txt (displayTerminalText language)))

-- | Render Markdown, allowing callers to add an interactive control to each
-- fenced code block header. Code block indices are one-based.
markdownWidgetWithCodeControls
    :: Ord n
    => (Int -> Text -> Widget n)
    -> Text
    -> Widget n
markdownWidgetWithCodeControls codeHeader input =
    markdownWidgetWithSyntaxHighlighting
        Nothing
        (\_ widget -> widget)
        codeHeader
        input

-- | Render Markdown with optional token-level highlighting for closed fenced
-- code blocks. The cache callback receives the stable one-based fence index;
-- callers can use it to retain completed code bodies while later prose
-- continues streaming.
markdownWidgetWithSyntaxHighlighting
    :: Ord n
    => Maybe SyntaxHighlighter
    -> (Int -> Widget n -> Widget n)
    -> (Int -> Text -> Widget n)
    -> Text
    -> Widget n
markdownWidgetWithSyntaxHighlighting syntaxHighlighter cacheCode codeHeader input =
    markdownWidgetWithInteractions
        syntaxHighlighter
        Nothing
        cacheCode
        codeHeader
        input

-- | Render Markdown with syntax highlighting, code controls, and
-- application-level click targets for links.
markdownWidgetWithSyntaxHighlightingAndLinks
    :: Ord n
    => Maybe SyntaxHighlighter
    -> (Text -> n)
    -> (Int -> Widget n -> Widget n)
    -> (Int -> Text -> Widget n)
    -> Text
    -> Widget n
markdownWidgetWithSyntaxHighlightingAndLinks
    syntaxHighlighter
    linkName
    cacheCode
    codeHeader
    input =
        markdownWidgetWithInteractions
            syntaxHighlighter
            (Just linkName)
            cacheCode
            codeHeader
            input

markdownWidgetWithInteractions
    :: Ord n
    => Maybe SyntaxHighlighter
    -> Maybe (Text -> n)
    -> (Int -> Widget n -> Widget n)
    -> (Int -> Text -> Widget n)
    -> Text
    -> Widget n
markdownWidgetWithInteractions
    syntaxHighlighter
    linkName
    cacheCode
    codeHeader
    input =
    vBox $
        concatMap
            (renderChunk syntaxHighlighter linkName cacheCode codeHeader)
            (fenceChunks input)

-- | Render a standalone code body with the same width bounding and optional
-- syntax highlighting used by fenced Markdown blocks.
codeWidgetWithSyntaxHighlighting
    :: Maybe SyntaxHighlighter
    -> Text
    -> Text
    -> Widget n
codeWidgetWithSyntaxHighlighting syntaxHighlighter language =
    renderCodeBodyWith wrapStyledCode syntaxHighlighter language
        . codeBodyLines

renderChunk
    :: Ord n
    => Maybe SyntaxHighlighter
    -> Maybe (Text -> n)
    -> (Int -> Widget n -> Widget n)
    -> (Int -> Text -> Widget n)
    -> FenceChunk
    -> [Widget n]
renderChunk _ linkName _ _ (FenceText prose) =
    renderLines linkName (Text.lines prose)
renderChunk syntaxHighlighter _ cacheCode codeHeader (FenceBlock block) =
    [ codeHeader block.fencedIndex block.fencedInfo
    , if block.fencedClosed
        then cacheCode block.fencedIndex bodyWidget
        else bodyWidget
    ]
  where
    bodyWidget =
        renderCodeBody
            (if block.fencedClosed then syntaxHighlighter else Nothing)
            block.fencedInfo
            (codeBodyLines block.fencedBody)

renderLines
    :: Ord n
    => Maybe (Text -> n)
    -> [Text]
    -> [Widget n]
renderLines _ [] = []
renderLines linkName lines_
    | Just (rows, rest) <- Block.takeTableRows lines_ =
        tableWidget rows : renderLines linkName rest
renderLines linkName (line : rest)
    | Just (_, heading) <- Block.headingPartsWith (== ' ') line =
        padTop (Pad 1)
            (inlineWidgetWithAttr linkName Theme.headingAttr (parseInline heading))
            : renderLines linkName rest
    | Just (indent, item) <- Block.bulletPartsWith (== ' ') line =
        hBox
            [ txt indent
            , withAttr Theme.headingAttr (txt "• ")
            , inlineWidgetWithAttr linkName Theme.assistantAttr (parseInline item)
            ]
            : renderLines linkName rest
    | Just (indent, number, item) <- Block.orderedParts line =
        hBox
            [ txt indent
            , withAttr Theme.headingAttr (txt (number <> ". "))
            , inlineWidgetWithAttr linkName Theme.assistantAttr (parseInline item)
            ]
            : renderLines linkName rest
    | Just rawQuote <- Block.blockQuoteRemainder line =
        let quote = fromMaybe rawQuote (Text.stripPrefix " " rawQuote)
        in
        hBox
            [ withAttr Theme.mutedAttr (txt "│ ")
            , inlineWidgetWithAttr linkName Theme.mutedAttr (parseInline quote)
            ]
            : renderLines linkName rest
    | Text.null (Text.strip line) =
        txt " " : renderLines linkName rest
    | Block.isThematicBreak line =
        withAttr Theme.mutedAttr (vLimit 1 (fill '─'))
            : renderLines linkName rest
    | otherwise =
        inlineWidgetWithAttr linkName Theme.assistantAttr (parseInline line)
            : renderLines linkName rest

codeBodyLines :: Text -> [Text]
codeBodyLines body
    | Text.null body = []
    | Text.isSuffixOf "\n" body =
        dropLast (Text.splitOn "\n" body)
    | otherwise =
        Text.splitOn "\n" body
  where
    dropLast = reverse . drop 1 . reverse

renderCodeBody
    :: Maybe SyntaxHighlighter
    -> Text
    -> [Text]
    -> Widget n
renderCodeBody =
    renderCodeBodyWith wrapStyled

renderCodeBodyWith
    :: (Int -> [(V.Attr, Text)] -> [[(V.Attr, Text)]])
    -> Maybe SyntaxHighlighter
    -> Text
    -> [Text]
    -> Widget n
renderCodeBodyWith wrapLine syntaxHighlighter language bodyLines =
    -- Keep tokenization inside the render action. Brick's 'cached' inspects a
    -- widget's size policy before consulting its cache; returning a concrete
    -- vBox here would therefore force tokenization on every streamed redraw.
    --
    -- Code must also be bounded here rather than relying on the surrounding
    -- vertical viewport. An over-wide Vty image can reach the terminal as one
    -- physical line, where terminal-side wrapping corrupts subsequent rows.
    B.Widget B.Greedy B.Fixed do
        context <- B.getContext
        codeAttr <- B.lookupAttrName Theme.codeAttr
        styledLines <-
            case syntaxHighlighter >>= highlighted of
                Just highlightedLines
                    | length highlightedLines == length bodyLines ->
                        traverse (traverse resolveSyntaxSpan) highlightedLines
                _ ->
                    pure
                        [ [(codeAttr, displayTerminalText line)]
                        | line <- bodyLines
                        ]
        let availableWidth = max 1 context.availWidth
            horizontalPadding =
                if availableWidth >= 3 then 1 else 0
            contentWidth =
                max 1 (availableWidth - 2 * horizontalPadding)
            rows =
                concatMap (wrapLine contentWidth) styledLines
            image =
                V.vertCat
                    [ renderCodeRow
                        codeAttr
                        horizontalPadding
                        row
                    | row <- rows
                    ]
            boundedImage
                | V.imageWidth image > availableWidth =
                    V.cropRight availableWidth image
                | otherwise = image
        pure B.emptyResult { B.image = boundedImage }
  where
    highlighted highlighter =
        either (const Nothing) Just $
            highlightCode
                highlighter
                language
                (Text.intercalate "\n" bodyLines)
    resolveSyntaxSpan span_ = do
        attr <- B.lookupAttrName (Theme.syntaxClassAttr span_.syntaxClass)
        pure (attr, displayTerminalText span_.syntaxText)

renderCodeRow
    :: V.Attr
    -> Int
    -> [(V.Attr, Text)]
    -> V.Image
renderCodeRow paddingAttr horizontalPadding fragments =
    V.horizCat
        [ blank
        , V.horizCat
            [ terminalTextImage attr text
            | (attr, text) <- fragments
            ]
        , blank
        ]
  where
    blank
        | horizontalPadding <= 0 = V.emptyImage
        | otherwise =
            V.charFill paddingAttr ' ' horizontalPadding 1

-- | Render a table against the width Brick actually gives it. Natural column
-- widths are only preferences: short columns keep their size while verbose
-- columns share the remaining space and wrap inside it.
tableWidget :: [[Text]] -> Widget n
tableWidget [] = emptyWidget
tableWidget rows@(headerCells : _) =
    B.Widget B.Greedy B.Fixed do
        context <- B.getContext
        borderAttr <- B.lookupAttrName Theme.mutedAttr
        headerAttr <- B.lookupAttrName Theme.headingAttr
        bodyAttr <- B.lookupAttrName Theme.assistantAttr
        styledRows <-
            traverse
                (\(rowIndex, cells) ->
                    traverse
                        (resolveInline
                            (if rowIndex == 0
                                then Theme.headingAttr
                                else Theme.assistantAttr)
                            . parseInline)
                        cells)
                (zip [0 :: Int ..] normalizedRows)
        let availableWidth = max 1 context.availWidth
            gridMinimumWidth =
                columnCount + 1 + sum minimumWidths
            paddedGridMinimumWidth =
                gridMinimumWidth + 2 * columnCount
            gridFits = availableWidth >= gridMinimumWidth
            horizontalPadding =
                if availableWidth >= paddedGridMinimumWidth
                    then 1
                    else 0
            chromeWidth =
                columnCount + 1
                    + 2 * horizontalPadding * columnCount
            contentBudget = max 0 (availableWidth - chromeWidth)
            widths =
                fitColumnWidths
                    contentBudget minimumWidths naturalWidths
            top = tableRule borderAttr horizontalPadding '┌' '┬' '┐' widths
            divider =
                tableRule borderAttr horizontalPadding '├' '┼' '┤' widths
            bottom =
                tableRule borderAttr horizontalPadding '└' '┴' '┘' widths
            renderedRows =
                case styledRows of
                    [] -> []
                    header : body ->
                        renderTableRow
                            borderAttr headerAttr horizontalPadding widths header
                            : divider
                            : map
                                (renderTableRow
                                    borderAttr bodyAttr horizontalPadding widths)
                                body
            image =
                if gridFits
                    then V.vertCat (top : renderedRows <> [bottom])
                    else compactTableImage
                        availableWidth borderAttr styledRows
            boundedImage
                | V.imageWidth image > availableWidth =
                    V.cropRight availableWidth image
                | otherwise = image
        pure B.emptyResult { B.image = boundedImage }
  where
    columnCount = length headerCells
    normalizedRows =
        [ take columnCount (cells <> repeat "")
        | cells <- rows
        ]
    naturalWidths =
        [ maximum
            (1
                : [ cellDisplayWidth cell
                  | row <- normalizedRows
                  , cell <- take 1 (drop columnIndex row)
                  ])
        | columnIndex <- [0 .. columnCount - 1]
        ]
    minimumWidths =
        [ maximum
            (1
                : [ cellMinimumWidth cell
                  | row <- normalizedRows
                  , cell <- take 1 (drop columnIndex row)
                  ])
        | columnIndex <- [0 .. columnCount - 1]
        ]

cellDisplayWidth :: Text -> Int
cellDisplayWidth =
    sum
        . map graphemeCellWidth
        . graphemeClusters
        . displayTerminalText
        . inlinePlainText
        . parseInline

cellMinimumWidth :: Text -> Int
cellMinimumWidth =
    maximum
        . (1 :)
        . map graphemeCellWidth
        . graphemeClusters
        . displayTerminalText
        . inlinePlainText
        . parseInline

-- | Fairly distribute a fixed content budget. Each column starts at one cell;
-- columns that reach their natural width drop out while the longer columns
-- continue growing.
fitColumnWidths :: Int -> [Int] -> [Int] -> [Int]
fitColumnWidths budget minimumWidths naturalWidths
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

compactTableImage
    :: Int
    -> V.Attr
    -> [[[(V.Attr, Text)]]]
    -> V.Image
compactTableImage width borderAttr rows =
    V.vertCat $
        concat
            [ map renderStyledLine $
                wrapStyledWords width $
                    List.intercalate
                        [(borderAttr, " │ ")]
                        cells
            | cells <- rows
            ]

renderStyledLine :: [(V.Attr, Text)] -> V.Image
renderStyledLine fragments =
    V.horizCat
        [ terminalTextImage attr text
        | (attr, text) <- fragments
        ]

tableRule
    :: V.Attr
    -> Int
    -> Char
    -> Char
    -> Char
    -> [Int]
    -> V.Image
tableRule attr horizontalPadding left middle right widths =
    V.text' attr $
        Text.singleton left
            <> Text.intercalate
                (Text.singleton middle)
                [ Text.replicate
                    (width + 2 * horizontalPadding)
                    "─"
                | width <- widths
                ]
            <> Text.singleton right

renderTableRow
    :: V.Attr
    -> V.Attr
    -> Int
    -> [Int]
    -> [[(V.Attr, Text)]]
    -> V.Image
renderTableRow borderAttr paddingAttr horizontalPadding widths cells =
    V.vertCat
        [ V.horizCat $
            V.char borderAttr '│'
                : concat
                    [ [ renderTableCell
                            paddingAttr horizontalPadding width fragments
                      , V.char borderAttr '│'
                      ]
                    | (width, fragments) <- zip widths rowFragments
                    ]
        | rowFragments <- physicalRows
        ]
  where
    wrappedCells =
        zipWith
            (\width spans -> wrapStyledWords (max 1 width) spans)
            widths
            (cells <> repeat [])
    rowHeight = maximum (1 : map length wrappedCells)
    physicalRows =
        List.transpose
            [ take rowHeight (wrapped <> repeat [])
            | wrapped <- wrappedCells
            ]

renderTableCell
    :: V.Attr
    -> Int
    -> Int
    -> [(V.Attr, Text)]
    -> V.Image
renderTableCell paddingAttr horizontalPadding width fragments =
    V.horizCat
        [ blank horizontalPadding
        , content
        , blank (max 0 (width - fragmentsDisplayWidth fragments))
        , blank horizontalPadding
        ]
  where
    content =
        V.horizCat
            [ terminalTextImage attr text
            | (attr, text) <- fragments
            ]
    blank count
        | count <= 0 = V.emptyImage
        | otherwise = V.charFill paddingAttr ' ' count 1

fragmentsDisplayWidth :: [(V.Attr, Text)] -> Int
fragmentsDisplayWidth =
    sum
        . map
            (sum
                . map graphemeCellWidth
                . graphemeClusters
                . snd)

inlineWidgetWithAttr
    :: Ord n
    => Maybe (Text -> n)
    -> AttrName
    -> [Inline]
    -> Widget n
inlineWidgetWithAttr linkName plainAttr inlines =
    B.Widget B.Greedy B.Fixed do
        context <- B.getContext
        styled <- resolveInline plainAttr inlines
        let width = max 1 context.availWidth
            rows = wrapStyled width styled
            rowWidget row =
                hBox
                    [ linkWidget attr text
                    | (attr, text) <- row
                    ]
            linkWidget attr text =
                case (linkName, V.attrURL attr) of
                    (Just toName, V.SetTo url) ->
                        clickable (toName url) (spanWidget attr text)
                    _ -> spanWidget attr text
            spanWidget attr text =
                B.Widget B.Fixed B.Fixed $
                    pure B.emptyResult
                        { B.image =
                            terminalTextImage attr text
                        }
        B.render (vBox (map rowWidget rows))

data InlineContext = InlineContext
    { inlineStrong :: !Bool
    , inlineEmphasis :: !Bool
    , inlineUrl :: !(Maybe Text)
    }

resolveInline :: AttrName -> [Inline] -> B.RenderM n [(V.Attr, Text)]
resolveInline plainAttr = fmap concat . traverse (go emptyContext)
  where
    emptyContext = InlineContext False False Nothing

    go context = \case
        InlineText text -> one plainAttr context text
        InlineCode text -> one Theme.inlineCodeAttr context text
        InlineStrong children ->
            concat <$> traverse (go context{inlineStrong = True}) children
        InlineEmphasis children ->
            concat <$> traverse (go context{inlineEmphasis = True}) children
        InlineLink url children -> do
            let linkContext = context{inlineUrl = Just url}
            label <- concat <$> traverse (go linkContext) children
            suffix <-
                if Text.null url || inlinePlainText children == url
                    then pure []
                    else one Theme.linkAttr linkContext (" (" <> url <> ")")
            pure (label <> suffix)

    one baseName context text = do
        base <- B.lookupAttrName $
            case context.inlineUrl of
                Just _ -> Theme.linkAttr
                Nothing -> baseName
        let addedStyle =
                (if context.inlineStrong then V.bold else 0)
                    .|. (if context.inlineEmphasis then V.italic else 0)
            style = V.styleMask base .|. addedStyle
            emphasisAttr
                | style == 0 = base
                | otherwise = base `V.withStyle` style
            linkedAttr = case context.inlineUrl of
                Just url
                    | safeUrl url -> emphasisAttr `V.withURL` url
                _ -> emphasisAttr
        pure [(linkedAttr, displayTerminalText text)]

    safeUrl url =
        not (Text.null url)
            && displayTerminalText url == url

wrapStyled :: Int -> [(V.Attr, Text)] -> [[(V.Attr, Text)]]
wrapStyled width spans =
    finalize $
        List.foldl' addCell ([[]], 0) (styledCells spans)
  where
    addCell (rows, used) cell@(_, cluster, cellWidth)
        | cluster == "\n" = ([] : rows, 0)
        | used > 0
        , used + cellWidth > width =
            ([cell] : rows, cellWidth)
        | otherwise =
            case rows of
                [] -> ([[cell]], cellWidth)
                row : rest ->
                    ((cell : row) : rest, used + cellWidth)
    finalize (rows, _) =
        let ordered = map (groupStyledCells . reverse) (reverse rows)
        in if null ordered then [[]] else ordered

-- | Prefer source whitespace as a wrap point without deleting it. The
-- whitespace stays at the end of the previous visual row, so concatenating
-- the rendered payload reconstructs the original code exactly. Tokens wider
-- than the viewport still hard-wrap.
wrapStyledCode :: Int -> [(V.Attr, Text)] -> [[(V.Attr, Text)]]
wrapStyledCode width spans =
    map groupStyledCells (wrapCells (styledCells spans))
  where
    width' = max 1 width

    wrapCells [] = [[]]
    wrapCells remaining =
        let (fitting, overflow) = takeFitting remaining
        in case overflow of
            [] -> [fitting]
            _ ->
                case lastSpaceIndex fitting of
                    Just index
                        | index > 0 ->
                            let (line, nextPrefix) =
                                    splitAt (index + 1) fitting
                            in line : wrapCells (nextPrefix <> overflow)
                    _ -> fitting : wrapCells overflow

    takeFitting = go 0 []
      where
        go _ taken [] = (reverse taken, [])
        go used taken allCells@(cell@(_, _, cellWidth) : rest)
            | used > 0
            , used + cellWidth > width' =
                (reverse taken, allCells)
            | otherwise =
                go (used + cellWidth) (cell : taken) rest

    lastSpaceIndex =
        List.foldl'
            (\found (index, cell) ->
                if styledCellIsSpace cell then Just index else found)
            Nothing
            . zip [0 :: Int ..]

-- | Table cells prefer word boundaries, but still hard-wrap an individual
-- token that is wider than its column.
wrapStyledWords :: Int -> [(V.Attr, Text)] -> [[(V.Attr, Text)]]
wrapStyledWords width spans =
    map groupStyledCells (wrapCells (styledCells spans))
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
                                    dropWhile styledCellIsSpace
                                        (carried <> overflow)
                            in dropTrailingSpace line : wrapCells next
                    _ -> fitting : wrapCells overflow

    takeFitting = go 0 []
      where
        go _ taken [] = (reverse taken, [])
        go used taken allCells@(cell@(_, _, cellWidth) : rest)
            | used > 0
            , used + cellWidth > width' =
                (reverse taken, allCells)
            | otherwise =
                go (used + cellWidth) (cell : taken) rest

    lastSpaceIndex =
        List.foldl'
            (\found (index, cell) ->
                if styledCellIsSpace cell then Just index else found)
            Nothing
            . zip [0 :: Int ..]

    dropTrailingSpace =
        reverse . dropWhile styledCellIsSpace . reverse

type StyledCell = (V.Attr, Text, Int)

styledCells :: [(V.Attr, Text)] -> [StyledCell]
styledCells spans =
    [ (attr, cluster, graphemeCellWidth cluster)
    | (attr, text) <- spans
    , cluster <- graphemeClusters text
    ]

styledCellIsSpace :: StyledCell -> Bool
styledCellIsSpace (_, cluster, _) =
    not (Text.null cluster) && Text.all isSpace cluster

groupStyledCells :: [StyledCell] -> [(V.Attr, Text)]
groupStyledCells =
    map
        (\(attr, text) ->
            (attr, LazyText.toStrict (Builder.toLazyText text)))
        . reverse
        . List.foldl' appendCell []
  where
    appendCell grouped (attr, cluster, _) =
        case grouped of
            (previousAttr, previousText) : rest
                | previousAttr == attr ->
                    (previousAttr, previousText <> Builder.fromText cluster)
                        : rest
            _ -> (attr, Builder.fromText cluster) : grouped
