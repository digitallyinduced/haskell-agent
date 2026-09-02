-- | Lightweight fullscreen Markdown block rendering.
module Agent.TUI.Markdown
    ( Inline(..)
    , codeWidgetWithSyntaxHighlighting
    , diffSyntaxLanguages
    , diffWidgetWithSyntaxHighlighting
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
    , resolvePathLanguage
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
import Data.Char (isDigit, isSpace)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as Builder
import qualified Graphics.Vty as V

markdownWidget :: Ord n => Text -> Widget n
markdownWidget =
    markdownWidgetWithCodeControls \_ _ -> emptyWidget

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
        (\_ _ -> emptyWidget)

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

-- | Languages needed to render a compact edit preview. The first file path is
-- supplied separately, while later files are introduced by body headers.
diffSyntaxLanguages :: Text -> Text -> [Text]
diffSyntaxLanguages initialPath body =
    List.nub
        [ language
        | line <- parseDiffLines initialPath body
        , Just path <- [line.diffPathHint]
        , Just language <- [resolvePathLanguage path]
        ]

-- | Render a compact edit preview with token-level syntax foregrounds over
-- full-width added/removed backgrounds.
diffWidgetWithSyntaxHighlighting
    :: Maybe SyntaxHighlighter
    -> Text
    -> Text
    -> Widget n
diffWidgetWithSyntaxHighlighting syntaxHighlighter initialPath body =
    B.Widget B.Greedy B.Fixed do
        context <- B.getContext
        codeAttr <- B.lookupAttrName Theme.codeAttr
        baseAttr <- B.lookupAttrName Theme.assistantAttr
        mutedAttr <- B.lookupAttrName Theme.mutedAttr
        addedAttr <- B.lookupAttrName Theme.diffAddedAttr
        removedAttr <- B.lookupAttrName Theme.diffRemovedAttr
        styledRows <-
            fmap concat $
                traverse
                    (resolveDiffRun
                        syntaxHighlighter
                        codeAttr
                        baseAttr
                        mutedAttr
                        addedAttr
                        removedAttr)
                    (diffRuns (parseDiffLines initialPath body))
        let availableWidth = max 1 context.availWidth
            rows =
                concatMap
                    (renderStyledDiffRow availableWidth)
                    styledRows
            image = V.vertCat rows
            boundedImage
                | V.imageWidth image > availableWidth =
                    V.cropRight availableWidth image
                | otherwise = image
        pure B.emptyResult { B.image = boundedImage }

data DiffLineKind
    = DiffLineAdded
    | DiffLineRemoved
    | DiffLineHeader
    | DiffLineMeta
    | DiffLinePlain
    deriving (Eq)

data ParsedDiffLine = ParsedDiffLine
    { diffLineKind :: !DiffLineKind
    , diffPathHint :: !(Maybe Text)
    , diffLinePrefix :: !Text
    , diffLineText :: !Text
    }

data DiffRun
    = DiffChangedRun !DiffLineKind !(Maybe Text) ![(Text, Text)]
    | DiffPlainRun !ParsedDiffLine

data StyledDiffRow = StyledDiffRow
    { styledDiffBackground :: !(Maybe V.Attr)
    , styledDiffFragments :: ![(V.Attr, Text)]
    }

parseDiffLines :: Text -> Text -> [ParsedDiffLine]
parseDiffLines initialPath = go (nonEmptyText initialPath) . Text.lines
  where
    go _ [] = []
    go currentPath (line : rest)
        | Just nextPath <- diffHeaderPath line =
            ParsedDiffLine DiffLineHeader (Just nextPath) "" line
                : go (Just nextPath) rest
        | Just (kind, prefix, source) <- changedDiffLine line =
            ParsedDiffLine kind currentPath prefix source
                : go currentPath rest
        | "  …" `Text.isPrefixOf` line
            || "… +" `Text.isPrefixOf` line =
            ParsedDiffLine DiffLineMeta currentPath "" line
                : go currentPath rest
        | otherwise =
            ParsedDiffLine DiffLinePlain currentPath "" line
                : go currentPath rest

changedDiffLine :: Text -> Maybe (DiffLineKind, Text, Text)
changedDiffLine line =
    case Text.stripPrefix "  -" line of
        Just source -> Just (DiffLineRemoved, "  -", source)
        Nothing ->
            case Text.stripPrefix "  +" line of
                Just source -> Just (DiffLineAdded, "  +", source)
                Nothing -> numberedDiffLine line

numberedDiffLine :: Text -> Maybe (DiffLineKind, Text, Text)
numberedDiffLine line = do
    afterIndent <- Text.stripPrefix "  " line
    let (padding, afterPadding) = Text.span (== ' ') afterIndent
        (digits, afterDigits) = Text.span isDigit afterPadding
    if Text.null digits
        then Nothing
        else do
            markerAndSource <- Text.stripPrefix " " afterDigits
            (marker, source) <- Text.uncons markerAndSource
            kind <- case marker of
                '-' -> Just DiffLineRemoved
                '+' -> Just DiffLineAdded
                _ -> Nothing
            pure
                ( kind
                , "  " <> padding <> digits <> " " <> Text.singleton marker
                , source
                )

diffHeaderPath :: Text -> Maybe Text
diffHeaderPath line =
    actionPath
        [ "  create "
        , "  delete "
        , "  write "
        , "  update "
        ]
  where
    actionPath [] = movePath
    actionPath (prefix : rest) =
        case Text.stripPrefix prefix line of
            Just path -> nonEmptyText path
            Nothing -> actionPath rest

    movePath = do
        moved <- Text.stripPrefix "  move " line
        let (source, destinationWithArrow) = Text.breakOn " → " moved
        case Text.stripPrefix " → " destinationWithArrow of
            Just destination -> nonEmptyText destination
            Nothing -> nonEmptyText source

nonEmptyText :: Text -> Maybe Text
nonEmptyText value =
    let stripped = Text.strip value
    in if Text.null stripped then Nothing else Just stripped

diffRuns :: [ParsedDiffLine] -> [DiffRun]
diffRuns [] = []
diffRuns (line : rest)
    | changedKind line.diffLineKind =
        let (matching, remaining) =
                span
                    (\next ->
                        next.diffLineKind == line.diffLineKind
                            && next.diffPathHint == line.diffPathHint)
                    rest
        in DiffChangedRun
                line.diffLineKind
                line.diffPathHint
                (map
                    (\changed ->
                        (changed.diffLinePrefix, changed.diffLineText))
                    (line : matching))
                : diffRuns remaining
    | otherwise =
        DiffPlainRun line : diffRuns rest
  where
    changedKind kind =
        kind == DiffLineAdded || kind == DiffLineRemoved

resolveDiffRun
    :: Maybe SyntaxHighlighter
    -> V.Attr
    -> V.Attr
    -> V.Attr
    -> V.Attr
    -> V.Attr
    -> DiffRun
    -> B.RenderM n [StyledDiffRow]
resolveDiffRun
    syntaxHighlighter
    codeAttr
    baseAttr
    mutedAttr
    addedAttr
    removedAttr = \case
        DiffPlainRun line ->
            pure
                [ StyledDiffRow
                    Nothing
                    [ ( if line.diffLineKind == DiffLineMeta
                            then mutedAttr
                            else baseAttr
                      , displayTerminalText line.diffLineText
                      )
                    ]
                ]
        DiffChangedRun kind path changedLines -> do
            let background =
                    if kind == DiffLineAdded then addedAttr else removedAttr
                sourceLines = map snd changedLines
                highlightedLines = do
                    highlighter <- syntaxHighlighter
                    sourcePath <- path
                    language <- resolvePathLanguage sourcePath
                    either (const Nothing) Just $
                        highlightCode
                            highlighter
                            language
                            (Text.intercalate "\n" sourceLines)
            contentRows <-
                case highlightedLines of
                    Just rows
                        | length rows == length sourceLines ->
                            traverse
                                (traverse
                                    (\span_ -> do
                                        attr <-
                                            B.lookupAttrName
                                                (Theme.syntaxClassAttr
                                                    span_.syntaxClass)
                                        pure
                                            ( withDiffBackground
                                                background
                                                attr
                                            , displayTerminalText
                                                span_.syntaxText
                                            )))
                                rows
                    _ ->
                        pure
                            [ [ ( withDiffBackground background codeAttr
                                , displayTerminalText source
                                )
                              ]
                            | source <- sourceLines
                            ]
            pure
                [ StyledDiffRow
                    (Just background)
                    ((background, prefix) : content)
                | ((prefix, _), content) <- zip changedLines contentRows
                ]

withDiffBackground :: V.Attr -> V.Attr -> V.Attr
withDiffBackground background attr =
    case V.attrBackColor background of
        V.SetTo color -> attr `V.withBackColor` color
        _ -> attr

renderStyledDiffRow :: Int -> StyledDiffRow -> [V.Image]
renderStyledDiffRow availableWidth row =
    map renderPhysicalRow $
        wrapStyledCode availableWidth row.styledDiffFragments
  where
    renderPhysicalRow fragments =
        let content = renderStyledLine fragments
        in case row.styledDiffBackground of
            Nothing -> content
            Just background ->
                let padding =
                        max 0 (availableWidth - V.imageWidth content)
                in V.horizCat
                    [ content
                    , if padding == 0
                        then V.emptyImage
                        else V.charFill background ' ' padding 1
                    ]

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
    | Just (table, rest) <- Block.takeTableRows lines_ =
        tableWidget table : renderLines linkName rest
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
tableWidget :: Block.MarkdownTable -> Widget n
tableWidget table = case table.tableRows of
  [] -> emptyWidget
  rows@(headerCells : _) -> tableRowsWidget table rows headerCells

tableRowsWidget
    :: Block.MarkdownTable
    -> [[Text]]
    -> [Text]
    -> Widget n
tableRowsWidget table rows headerCells =
    B.Widget B.Greedy B.Fixed do
        context <- B.getContext
        borderAttr <- B.lookupAttrName Theme.borderAttr
        headerAttr <- B.lookupAttrName Theme.strongAttr
        bodyAttr <- B.lookupAttrName Theme.assistantAttr
        styledRows <-
            traverse
                (\(rowIndex, cells) ->
                    traverse
                        (resolveInline
                            (if rowIndex == 0
                                then Theme.strongAttr
                                else Theme.assistantAttr)
                            . parseInline)
                        cells)
                (zip [0 :: Int ..] normalizedRows)
        let availableWidth = max 1 context.availWidth
            borderWidth = columnCount + 1
            gridMinimumWidth =
                borderWidth + sum minimumWidths
            paddedGridMinimumWidth =
                gridMinimumWidth + 2 * columnCount
            gridFits = availableWidth >= gridMinimumWidth
            horizontalPadding =
                if availableWidth >= paddedGridMinimumWidth
                    then 1
                    else 0
            chromeWidth =
                borderWidth + 2 * horizontalPadding * columnCount
            contentBudget = max 0 (availableWidth - chromeWidth)
            widths =
                fitColumnWidths
                    contentBudget minimumWidths naturalWidths
            top =
                tableRule
                    borderAttr horizontalPadding '┌' '┬' '┐' widths
            divider =
                tableRule
                    borderAttr horizontalPadding '├' '┼' '┤' widths
            bottom =
                tableRule
                    borderAttr horizontalPadding '└' '┴' '┘' widths
            renderedRows =
                case styledRows of
                    [] -> []
                    header : body ->
                        let logicalRows =
                                renderTableRow
                                    borderAttr headerAttr horizontalPadding
                                    table.tableAlignments widths header
                                    : map
                                        (renderTableRow
                                            borderAttr bodyAttr
                                            horizontalPadding
                                            table.tableAlignments widths)
                                        body
                        in top
                            : (List.intersperse divider logicalRows
                                <> [bottom])
            image =
                if gridFits
                    then V.vertCat renderedRows
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
    -> [Block.TableAlignment]
    -> [Int]
    -> [[(V.Attr, Text)]]
    -> V.Image
renderTableRow
    borderAttr paddingAttr horizontalPadding alignments widths cells =
    V.vertCat
        [ V.horizCat $
            border
                : (List.intersperse border
                    [ renderTableCell
                        paddingAttr alignment horizontalPadding width fragments
                    | (alignment, width, fragments) <-
                        zip3 normalizedAlignments widths rowFragments
                    ]
                    <> [border])
        | rowFragments <- physicalRows
        ]
  where
    border = V.char borderAttr '│'
    normalizedAlignments = alignments <> repeat Block.AlignDefault
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
    -> Block.TableAlignment
    -> Int
    -> Int
    -> [(V.Attr, Text)]
    -> V.Image
renderTableCell paddingAttr alignment horizontalPadding width fragments =
    V.horizCat
        [ blank horizontalPadding
        , blank leftPadding
        , content
        , blank rightPadding
        , blank horizontalPadding
        ]
  where
    (leftPadding, rightPadding) =
        tableAlignmentPadding alignment $
            max 0 (width - fragmentsDisplayWidth fragments)
    content =
        V.horizCat
            [ terminalTextImage attr text
            | (attr, text) <- fragments
            ]
    blank count
        | count <= 0 = V.emptyImage
        | otherwise = V.charFill paddingAttr ' ' count 1

tableAlignmentPadding :: Block.TableAlignment -> Int -> (Int, Int)
tableAlignmentPadding alignment padding = case alignment of
    Block.AlignRight -> (padding, 0)
    Block.AlignCenter -> (padding `div` 2, padding - padding `div` 2)
    Block.AlignLeft -> (0, padding)
    Block.AlignDefault -> (0, padding)

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
