-- | Lightweight fullscreen Markdown block rendering.
module Agent.TUI.Markdown
    ( InlineSpan(..)
    , InlineStyle(..)
    , inlinePlainText
    , markdownWidget
    , markdownWidgetWithCodeControls
    , markdownWidgetWithSyntaxHighlighting
    , parseInline
    ) where

import Agent.TUI.FencedCode
    ( FenceChunk(..)
    , FencedBlock(..)
    , fenceChunks
    )
import Agent.Syntax
    ( HighlightedLine
    , SyntaxHighlighter
    , SyntaxSpan(..)
    , highlightCode
    )
import Agent.TUI.TextWidth (displayCharCellWidth)
import qualified Agent.TUI.Theme as Theme
import Brick
import qualified Brick.Types as B
import Data.Char
    ( isDigit
    , isSpace
    )
import qualified Data.List as List
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as Builder
import qualified Graphics.Vty as V

terminalCharWidth :: Char -> Int
terminalCharWidth = displayCharCellWidth

data InlineStyle
    = InlinePlain
    | InlineStrong
    | InlineEmphasis
    | InlineCode
    | InlineLink !Text
    deriving (Eq, Show)

data InlineSpan = InlineSpan
    { inlineStyle :: !InlineStyle
    , inlineText :: !Text
    }
    deriving (Eq, Show)

markdownWidget :: Text -> Widget n
markdownWidget =
    markdownWidgetWithCodeControls \_ language ->
        if Text.null language
            then emptyWidget
            else withAttr Theme.mutedAttr (txt language)

-- | Render Markdown, allowing callers to add an interactive control to each
-- fenced code block header. Code block indices are one-based.
markdownWidgetWithCodeControls
    :: (Int -> Text -> Widget n)
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
    :: Maybe SyntaxHighlighter
    -> (Int -> Widget n -> Widget n)
    -> (Int -> Text -> Widget n)
    -> Text
    -> Widget n
markdownWidgetWithSyntaxHighlighting syntaxHighlighter cacheCode codeHeader input =
    vBox $
        concatMap
            (renderChunk syntaxHighlighter cacheCode codeHeader)
            (fenceChunks input)

renderChunk
    :: Maybe SyntaxHighlighter
    -> (Int -> Widget n -> Widget n)
    -> (Int -> Text -> Widget n)
    -> FenceChunk
    -> [Widget n]
renderChunk _ _ _ (FenceText prose) =
    renderLines (Text.lines prose)
renderChunk syntaxHighlighter cacheCode codeHeader (FenceBlock block) =
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
    :: [Text]
    -> [Widget n]
renderLines [] = []
renderLines lines_
    | Just (table, rest) <- takeTable lines_ =
        table : renderLines rest
renderLines (line : rest)
    | Just heading <- stripHeading line =
        withAttr Theme.headingAttr
            (padTop (Pad 1) (inlineWidget (parseInline heading)))
            : renderLines rest
    | Just item <- stripBullet line =
        hBox
            [ withAttr Theme.headingAttr (txt "• ")
            , inlineWidget (parseInline item)
            ]
            : renderLines rest
    | Just (number, item) <- stripOrdered line =
        hBox
            [ withAttr Theme.headingAttr (txt (number <> ". "))
            , inlineWidget (parseInline item)
            ]
            : renderLines rest
    | Just quote <- Text.stripPrefix "> " (Text.stripStart line) =
        hBox
            [ withAttr Theme.mutedAttr (txt "│ ")
            , withAttr Theme.mutedAttr (inlineWidget (parseInline quote))
            ]
            : renderLines rest
    | Text.null (Text.strip line) =
        txt " " : renderLines rest
    | isThematicBreak line =
        withAttr Theme.mutedAttr (vLimit 1 (fill '─'))
            : renderLines rest
    | otherwise =
        inlineWidget (parseInline line)
            : renderLines rest

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
renderCodeBody syntaxHighlighter language bodyLines =
    -- Keep tokenization inside the render action. Brick's 'cached' inspects a
    -- widget's size policy before consulting its cache; returning a concrete
    -- vBox here would therefore force tokenization on every streamed redraw.
    B.Widget B.Fixed B.Fixed $
        B.render (buildCodeBody syntaxHighlighter language bodyLines)

buildCodeBody
    :: Maybe SyntaxHighlighter
    -> Text
    -> [Text]
    -> Widget n
buildCodeBody syntaxHighlighter language bodyLines =
    vBox $
        case syntaxHighlighter >>= highlighted of
            Just highlightedLines
                | length highlightedLines == length bodyLines ->
                    map renderHighlightedCodeLine highlightedLines
            _ -> map renderFlatCodeLine bodyLines
  where
    highlighted highlighter =
        either (const Nothing) Just $
            highlightCode
                highlighter
                language
                (Text.intercalate "\n" bodyLines)

renderFlatCodeLine :: Text -> Widget n
renderFlatCodeLine =
    withAttr Theme.codeAttr . padLeftRight 1 . txt

renderHighlightedCodeLine :: HighlightedLine -> Widget n
renderHighlightedCodeLine spans =
    withAttr Theme.codeAttr $
        padLeftRight 1 $
            case spans of
                [] -> txt ""
                _ ->
                    hBox
                        [ withAttr
                            (Theme.syntaxClassAttr span_.syntaxClass)
                            (txt span_.syntaxText)
                        | span_ <- spans
                        ]

stripHeading :: Text -> Maybe Text
stripHeading line =
    let stripped = Text.stripStart line
        (marks, rest) = Text.span (== '#') stripped
    in if not (Text.null marks)
        && Text.length marks <= 6
        && Text.isPrefixOf " " rest
        then Just (Text.strip rest)
        else Nothing

stripBullet :: Text -> Maybe Text
stripBullet line =
    let stripped = Text.stripStart line
    in asumPrefix ["- ", "* ", "+ "] stripped

stripOrdered :: Text -> Maybe (Text, Text)
stripOrdered line =
    let
        stripped = Text.stripStart line
        (number, rest) = Text.span isDigit stripped
    in if Text.null number
        then Nothing
        else
            (\item -> (number, Text.strip item))
                <$> Text.stripPrefix ". " rest

isThematicBreak :: Text -> Bool
isThematicBreak line =
    let stripped = Text.filter (not . isSpace) (Text.strip line)
    in Text.length stripped >= 3
        && (Text.all (== '-') stripped
            || Text.all (== '*') stripped
            || Text.all (== '_') stripped)

takeTable :: [Text] -> Maybe (Widget n, [Text])
takeTable (rawHeader : separator : rest)
    | Just headerCells <- splitTableRow rawHeader
    , Just separatorCells <- splitTableRow separator
    , length separatorCells == length headerCells
    , all isSeparatorCell separatorCells =
        let
            (body, after) = span isTableRow rest
            rows = headerCells : mapMaybe splitTableRow body
        in Just (tableWidget rows, after)
takeTable _ = Nothing

isSeparatorCell :: Text -> Bool
isSeparatorCell cell =
    Text.any (== '-') cell
        && Text.null
            (Text.filter (`notElem` ['-', ':', ' ']) cell)

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
                        (traverse
                            (resolveInlineSpan
                                (if rowIndex == 0
                                    then Theme.headingAttr
                                    else Theme.assistantAttr))
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
    Text.foldl' (\width char -> width + terminalCharWidth char) 0
        . inlinePlainText
        . parseInline

cellMinimumWidth :: Text -> Int
cellMinimumWidth =
    maximum
        . (1 :)
        . map terminalCharWidth
        . Text.unpack
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
        [ V.text attr (LazyText.fromStrict text)
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
            [ V.text attr (LazyText.fromStrict text)
            | (attr, text) <- fragments
            ]
    blank count
        | count <= 0 = V.emptyImage
        | otherwise = V.charFill paddingAttr ' ' count 1

fragmentsDisplayWidth :: [(V.Attr, Text)] -> Int
fragmentsDisplayWidth =
    sum
        . map
            (Text.foldl'
                (\width character -> width + terminalCharWidth character)
                0
                . snd)

isTableRow :: Text -> Bool
isTableRow = isJust . splitTableRow

-- | Split a pipe row on actual column delimiters. Escaped pipes and pipes
-- inside matching backtick spans remain part of their cell.
splitTableRow :: Text -> Maybe [Text]
splitTableRow raw =
    let stripped = Text.strip raw
        content = fromMaybe stripped (Text.stripPrefix "|" stripped)
        (cells, delimiterCount) = scan Nothing [] [] 0 False
            (Text.unpack content)
    in if delimiterCount >= 1
        then Just cells
        else Nothing
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
        let allCells =
                reverse
                    (finishCell current : cells)
            withoutOuterBorder
                | trailingDelimiter = dropLast allCells
                | otherwise = allCells
        in (withoutOuterBorder, delimiterCount)
    scan codeRun current cells delimiterCount _ ('\\' : rest) =
        let (slashes, afterSlashes) = span (== '\\') rest
            slashCount = 1 + length slashes
            literalSlashes = replicate
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
                    splitCell codeRun current' cells
                        delimiterCount afterPipe
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
                    | hasClosingRun tickCount afterTicks ->
                        Just tickCount
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

asumPrefix :: [Text] -> Text -> Maybe Text
asumPrefix prefixes text = case prefixes of
    [] -> Nothing
    prefix : rest -> case Text.stripPrefix prefix text of
        Just value -> Just value
        Nothing -> asumPrefix rest text

parseInline :: Text -> [InlineSpan]
parseInline = go Nothing []
  where
    go _ plain text
        | Text.null text = flushPlain plain []
    go previous plain text
        | Just (body, rest) <- delimited "**" text =
            flushPlain plain $
                strongSpans body
                    <> go (lastChar body) [] rest
        | Just (body, rest) <- delimited "__" text =
            flushPlain plain $
                strongSpans body
                    <> go (lastChar body) [] rest
        | Just (body, rest) <- codeSpan text =
            flushPlain plain $
                InlineSpan InlineCode body
                    : go (lastChar body) [] rest
        | Just (label, url, rest) <- linkSpan text =
            flushPlain plain $
                InlineSpan (InlineLink url)
                    (label
                        <> if Text.null url || label == url
                            then ""
                            else " (" <> url <> ")")
                    : go (lastChar label) [] rest
        | Just (body, rest) <- emphasis previous '*' text =
            flushPlain plain $
                InlineSpan InlineEmphasis body
                    : go (lastChar body) [] rest
        | Just (body, rest) <- emphasis previous '_' text =
            flushPlain plain $
                InlineSpan InlineEmphasis body
                    : go (lastChar body) [] rest
        | otherwise =
            case Text.uncons text of
                Nothing -> flushPlain plain []
                Just (character, rest) ->
                    let (ordinary, remaining) =
                            Text.span (not . inlineMarker) rest
                        chunk = Text.cons character ordinary
                    in go (lastChar chunk) (chunk : plain) remaining

    delimited marker text = do
        after <- Text.stripPrefix marker text
        let (body, closing) = Text.breakOn marker after
        if Text.null body || Text.null closing
            then Nothing
            else Just (body, Text.drop (Text.length marker) closing)

    codeSpan text = do
        let (ticks, after) = Text.span (== '`') text
        if Text.null ticks
            then Nothing
            else
                let (body, closing) = Text.breakOn ticks after
                in if Text.null closing
                    then Nothing
                    else Just
                        ( body
                        , Text.drop (Text.length ticks) closing
                        )

    linkSpan text = do
        afterOpen <- Text.stripPrefix "[" text
        let (label, afterLabel) = Text.breakOn "](" afterOpen
        afterUrl <- Text.stripPrefix "](" afterLabel
        let (url, closing) = Text.breakOn ")" afterUrl
        if Text.null label || Text.null closing
            then Nothing
            else Just (label, url, Text.drop 1 closing)

    emphasis previous marker text = do
        after <- Text.stripPrefix (Text.singleton marker) text
        let (body, closing) =
                Text.breakOn (Text.singleton marker) after
            openingBoundary =
                maybe True (\character -> isSpace character
                    || character `elem` ("([{\"'" :: String)) previous
            closingRest = Text.drop 1 closing
            closingBoundary = case Text.uncons closingRest of
                Nothing -> True
                Just (character, _) ->
                    isSpace character
                        || character `elem` (".,;:!?)]}\"'" :: String)
        if Text.null body
            || Text.null closing
            || Text.any isSpace (Text.take 1 body)
            || not openingBoundary
            || not closingBoundary
            then Nothing
            else Just (body, closingRest)

    lastChar value =
        snd <$> Text.unsnoc value

    -- Semantic inline styles such as code and links must override the
    -- surrounding strong marker. Only otherwise-plain text inherits bold.
    strongSpans body =
        map
            (\span ->
                case span.inlineStyle of
                    InlinePlain -> span { inlineStyle = InlineStrong }
                    _ -> span)
            (parseInline body)

    inlineMarker character =
        character `elem` ("*_`[" :: String)

    flushPlain [] rest = rest
    flushPlain chunks rest =
        InlineSpan InlinePlain (Text.concat (reverse chunks))
            : rest

inlinePlainText :: [InlineSpan] -> Text
inlinePlainText = Text.concat . map (.inlineText)

inlineWidget :: [InlineSpan] -> Widget n
inlineWidget spans =
    B.Widget B.Greedy B.Fixed do
        context <- B.getContext
        styled <- traverse (resolveInlineSpan Theme.assistantAttr) spans
        let width = max 1 context.availWidth
            rows = wrapStyled width styled
            rendered =
                V.vertCat
                    [ V.horizCat
                        [ V.text attr (LazyText.fromStrict text)
                        | (attr, text) <- row
                        ]
                    | row <- rows
                    ]
        pure B.emptyResult { B.image = rendered }

resolveInlineSpan
    :: AttrName
    -> InlineSpan
    -> B.RenderM n (V.Attr, Text)
resolveInlineSpan plainAttr InlineSpan{inlineStyle, inlineText} = do
    attr <-
        B.lookupAttrName $
            case inlineStyle of
                InlinePlain -> plainAttr
                _ -> styleAttr inlineStyle
    pure
        ( case inlineStyle of
            InlineLink url
                | not (Text.null url) -> attr `V.withURL` url
            _ -> attr
        , inlineText
        )

styleAttr :: InlineStyle -> AttrName
styleAttr = \case
    InlinePlain -> Theme.assistantAttr
    InlineStrong -> Theme.strongAttr
    InlineEmphasis -> Theme.emphasisAttr
    InlineCode -> Theme.inlineCodeAttr
    InlineLink _ -> Theme.linkAttr

wrapStyled :: Int -> [(V.Attr, Text)] -> [[(V.Attr, Text)]]
wrapStyled width spans =
    finalize $
        List.foldl' addCell ([[]], 0) cells
  where
    cells =
        [ (attr, character)
        | (attr, text) <- spans
        , character <- Text.unpack text
        ]
    addCell (rows, used) (attr, cell)
        | cell == '\n' = ([] : rows, 0)
        | used > 0
        , used + cellWidth > width =
            ([(attr, Builder.singleton cell)] : rows, cellWidth)
        | otherwise =
            case rows of
                [] ->
                    ([[(attr, Builder.singleton cell)]], cellWidth)
                row : rest ->
                    (appendCell attr cell row : rest, used + cellWidth)
      where
        cellWidth = terminalCharWidth cell
    appendCell attr cell row =
        case row of
            (previousAttr, previousText) : prior
                | previousAttr == attr ->
                    ( previousAttr
                    , previousText <> Builder.singleton cell
                    ) : prior
            _ -> (attr, Builder.singleton cell) : row
    finalize (rows, _) =
        let ordered =
                map
                    (map
                        (\(attr, text) ->
                            ( attr
                            , LazyText.toStrict (Builder.toLazyText text)
                            ))
                        . reverse)
                    (reverse rows)
        in if null ordered then [[]] else ordered

-- | Table cells prefer word boundaries, but still hard-wrap an individual
-- token that is wider than its column.
wrapStyledWords :: Int -> [(V.Attr, Text)] -> [[(V.Attr, Text)]]
wrapStyledWords width spans =
    map groupCells (wrapCells cells)
  where
    width' = max 1 width
    cells =
        [ (attr, character)
        | (attr, text) <- spans
        , character <- Text.unpack text
        ]

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
                                    dropWhile (isSpace . snd)
                                        (carried <> overflow)
                            in dropTrailingSpace line : wrapCells next
                    _ -> fitting : wrapCells overflow

    takeFitting = go 0 []
      where
        go _ taken [] = (reverse taken, [])
        go used taken allCells@((attr, character) : rest)
            | used > 0
            , used + cellWidth > width' =
                (reverse taken, allCells)
            | otherwise =
                go (used + cellWidth) ((attr, character) : taken) rest
          where
            cellWidth = terminalCharWidth character

    lastSpaceIndex =
        List.foldl'
            (\found (index, (_, character)) ->
                if isSpace character then Just index else found)
            Nothing
            . zip [0 :: Int ..]

    dropTrailingSpace =
        reverse . dropWhile (isSpace . snd) . reverse

    groupCells =
        map
            (\(attr, text) ->
                (attr, LazyText.toStrict (Builder.toLazyText text)))
            . reverse
            . List.foldl' appendCell []

    appendCell grouped (attr, character) =
        case grouped of
            (previousAttr, previousText) : rest
                | previousAttr == attr ->
                    (previousAttr, previousText <> Builder.singleton character)
                        : rest
            _ -> (attr, Builder.singleton character) : grouped
