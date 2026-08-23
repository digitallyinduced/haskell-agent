-- | Turn CommonMark-ish assistant text into ANSI-styled terminal output.
module Agent.CLI.Markdown
    ( renderMarkdown
    , renderMarkdownFragment
    , splitMarkdownFragment
    ) where

import Agent.CLI.Style
    ( agentBackground
    , osc8Link
    , solarizedBase01
    , solarizedBlue
    , solarizedCyan
    , solarizedGreen
    , solarizedMagenta
    , solarizedViolet
    , solarizedYellow
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
import Agent.TUI.TextWidth
    ( displayCharCellWidth
    , displayTerminalText
    )
import Data.Char (isAlphaNum, isAscii, isDigit, isSpace)
import Data.Colour (Colour)
import Data.List (transpose)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Console.ANSI
    ( ConsoleIntensity(..)
    , ConsoleLayer(..)
    , SGR(..)
    , Underlining(..)
    )

-- | When @color@ is 'True', style a useful subset of GFM for a TTY.
-- When 'False', return @text@ unchanged (pipes, redirects, tests).
-- Nested spans restore 'agentBackground' after each 'Reset'.
renderMarkdown :: Bool -> Text -> Text
renderMarkdown color text
    | not color = text
    | otherwise = Text.concat (map renderChunk (fenceChunks cleaned))
  where
    cleaned = displayTerminalText text
    renderChunk (FenceText prose) = renderProse prose
    renderChunk (FenceBlock block) =
        let header
                | Text.null block.fencedInfo = ""
                | otherwise = md [fg solarizedCyan] block.fencedInfo <> "\n"
            body = renderFenceBody block.fencedBody
        in header <> body

renderProse :: Text -> Text
renderProse text =
    let endsWithNewline = Text.isSuffixOf "\n" text
        lines_ = Text.splitOn "\n" text
        linesForParse
            | endsWithNewline && not (null lines_) = init lines_
            | otherwise = lines_
        rendered = Text.intercalate "\n" (renderBlocks linesForParse)
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
                (map (md [fg solarizedBase01]) actualLines)
    in if endsWithNewline then rendered <> "\n" else rendered

-- | Style helper that keeps the agent line wash across nested SGR.
md :: [SGR] -> Text -> Text
md = styleBase True agentBackground

renderBlocks :: [Text] -> [Text]
renderBlocks = go
  where
    go [] = []
    go (line : rest)
        | Just (styled, after) <- takeTable (line : rest) =
            styled ++ go after
        | Just (level, title) <- headingLine line =
            -- Hide the markdown `#` markers (grok pretty mode); color the title.
            renderInlineWith (headingPrefixStyle level) (parseInline title)
                : go rest
        | Just (indent, item) <- unorderedItemParts line =
            ( indent
                <> md listMarkerStyle "• "
                <> styleInline item
            )
                : go rest
        | Just (indent, digits, item) <- orderedItemParts line =
            ( indent
                <> md listMarkerStyle (digits <> ". ")
                <> styleInline item
            )
                : go rest
        | Just quote <- blockQuote line =
            let body = renderInlineWith quoteStyle (parseInline quote)
            in (md [fg solarizedBase01] "│ " <> body)
                : go rest
        | isThematicBreak line =
            md [fg solarizedBase01] (Text.replicate 40 "─")
                : go rest
        | Text.null (Text.strip line) = line : go rest
        | otherwise = styleInline line : go rest

-- | Heading title style: bold + level color. Markdown `#` markers are hidden.
headingPrefixStyle :: Int -> [SGR]
headingPrefixStyle level =
    SetConsoleIntensity BoldIntensity : headingColor level

headingColor :: Int -> [SGR]
headingColor = \case
    1 -> [fg solarizedMagenta]
    2 -> [fg solarizedCyan]
    3 -> [fg solarizedBlue]
    4 -> [fg solarizedYellow]
    5 -> [fg solarizedGreen]
    _ -> [fg solarizedViolet]

listMarkerStyle :: [SGR]
listMarkerStyle =
    [ SetConsoleIntensity BoldIntensity
    , fg solarizedCyan
    ]

fg :: Colour Float -> SGR
fg = SetRGBColor Foreground

-- | Blockquote body: muted so it sits behind surrounding prose.
quoteStyle :: [SGR]
quoteStyle = [fg solarizedBase01]

headingLine :: Text -> Maybe (Int, Text)
headingLine line =
    let stripped = Text.stripStart line
        (marks, after) = Text.span (== '#') stripped
        level = Text.length marks
    in if level >= 1 && level <= 6 && startsWithSpace after
        then Just (level, Text.strip after)
        else Nothing

startsWithSpace :: Text -> Bool
startsWithSpace t = case Text.uncons t of
    Just (c, _) -> isSpace c
    Nothing -> False

unorderedItemParts :: Text -> Maybe (Text, Text)
unorderedItemParts line =
    let (indent, stripped) = Text.span isSpace line
    in case Text.uncons stripped of
        Just (c, rest)
            | c `elem` ['-', '*', '+']
            , Just (sp, after) <- Text.uncons rest
            , isSpace sp ->
                Just (indent, Text.strip after)
        _ -> Nothing

-- | Ordered list: number marker and item body separately so the marker can
-- be colored without restyling the whole line twice.
orderedItemParts :: Text -> Maybe (Text, Text, Text)
orderedItemParts line =
    let (indent, stripped) = Text.span isSpace line
        (digits, after) = Text.span isDigit stripped
    in if not (Text.null digits)
        then case Text.stripPrefix ". " after of
            Just item -> Just (indent, digits, Text.strip item)
            Nothing -> Nothing
        else Nothing

blockQuote :: Text -> Maybe Text
blockQuote line = case Text.stripPrefix ">" (Text.stripStart line) of
    Just rest -> Just (Text.stripStart rest)
    Nothing -> Nothing

isThematicBreak :: Text -> Bool
isThematicBreak line =
    let stripped = Text.filter (not . isSpace) (Text.strip line)
    in Text.length stripped >= 3
        && (Text.all (== '-') stripped
            || Text.all (== '*') stripped
            || Text.all (== '_') stripped)

takeTable :: [Text] -> Maybe ([Text], [Text])
takeTable (header : sep : rest)
    | Just headerCells <- splitTableRow header
    , Just separatorCells <- splitTableRow sep
    , length separatorCells == length headerCells
    , all isSeparatorCell separatorCells =
        let (body, after) = span isTableRow rest
            bodyCells = mapMaybe splitTableRow body
            rows = headerCells : bodyCells
            widths = columnWidths rows
            top = md [fg solarizedBase01] (boxLine '┌' '┬' '┐' '─' widths)
            mid = md [fg solarizedBase01] (boxLine '├' '┼' '┤' '─' widths)
            bot = md [fg solarizedBase01] (boxLine '└' '┴' '┘' '─' widths)
            headerRow = styleTableRow True widths headerCells
            bodyRows = map (styleTableRow False widths) bodyCells
            styled = [top, headerRow, mid] ++ bodyRows ++ [bot]
        in Just (styled, after)
takeTable _ = Nothing

boxLine :: Char -> Char -> Char -> Char -> [Int] -> Text
boxLine left mid right fill widths =
    Text.singleton left
        <> Text.intercalate (Text.singleton mid)
            [ Text.replicate (w + 2) (Text.singleton fill) | w <- widths ]
        <> Text.singleton right

isTableRow :: Text -> Bool
isTableRow = isJust . splitTableRow

isSeparatorCell :: Text -> Bool
isSeparatorCell cell =
    Text.any (== '-') cell
        && Text.null (Text.filter (`notElem` ['-', ':', ' ']) cell)

-- | Split on actual table delimiters, preserving escaped pipes and pipes
-- inside matching backtick spans.
splitTableRow :: Text -> Maybe [Text]
splitTableRow raw =
    let stripped = Text.strip raw
        content = fromMaybe stripped (Text.stripPrefix "|" stripped)
        (cells, delimiterCount) = scan Nothing [] [] 0 False
            (Text.unpack content)
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

columnWidths :: [[Text]] -> [Int]
columnWidths rows =
    let cols = transpose rows
    in map (maximum . (0 :) . map renderedWidth) cols
  where
    renderedWidth =
        Text.foldl'
            (\width character -> width + displayCharCellWidth character)
            0
            . inlinePlainText
            . parseInline

styleTableRow :: Bool -> [Int] -> [Text] -> Text
styleTableRow isHeader widths cells =
    let cellText w c =
            let inlines = parseInline c
                visible = inlinePlainText inlines
                width' =
                    Text.foldl'
                        (\total character ->
                            total + displayCharCellWidth character)
                        0
                        visible
                padding = max 0 (w - width')
                base
                    | isHeader = [SetConsoleIntensity BoldIntensity]
                    | otherwise = []
            in " "
                <> renderInlineWith base inlines
                <> Text.replicate padding " "
                <> " "
        parts = zipWith cellText widths (cells ++ repeat "")
        bar = md [fg solarizedBase01] "│"
    in bar <> Text.intercalate bar (take (length widths) parts) <> bar

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
        InlineCode text -> styled (context <> codeStyle) text
        InlineStrong children ->
            renderInlineWith
                (context <> [SetConsoleIntensity BoldIntensity])
                children
        InlineEmphasis children ->
            renderInlineWith (context <> [SetItalicized True]) children
        InlineLink url children ->
            let label =
                    renderInlineWith (context <> linkStyle) children
                linked
                    | safeUrl url = osc8Link True url label
                    | otherwise = label
                suffix
                    | Text.null url || inlinePlainText children == url = ""
                    | otherwise =
                        styled (context <> urlStyle) (" (" <> url <> ")")
            in linked <> suffix

    styled [] value = value
    styled styles value = md styles value

    safeUrl url =
        not (Text.null url)
            && displayTerminalText url == url

    codeStyle =
        [ SetConsoleIntensity BoldIntensity
        , fg solarizedCyan
        ]
    linkStyle =
        [ fg solarizedBlue
        , SetUnderlining SingleUnderline
        ]
    urlStyle = [fg solarizedBase01]

-- | Split an inline markdown stream into a prefix that can be rendered without
-- future input and a suffix beginning at a possibly incomplete construct.
--
-- This lets the TUI remain append-only while holding delimiters such as @**@
-- until their closing delimiter arrives. A newline makes an unmatched inline
-- construct literal because the renderer does not span inline markup across
-- lines.
splitMarkdownFragment :: Maybe Char -> Text -> (Text, Text, Maybe Char)
splitMarkdownFragment initialPrev = go initialPrev []
  where
    go prev chunks t
        | Text.null t = (Text.concat (reverse chunks), "", prev)
        | Just (escaped, rest) <- takeEscapedPunctuation t =
            consume (Just escaped) rest
        | Just (code, rest) <- takeInlineCode t =
            consume (lastChar code) rest
        | Just (_linkText, _url, rest) <- takeLink t =
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
            (Text.concat (reverse chunks), t, prev)
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

takeLink :: Text -> Maybe (Text, Text, Text)
takeLink t = do
    afterBracket <- Text.stripPrefix "[" t
    (linkText, afterBracketClose) <- takeLinkLabel afterBracket
    afterParen <- Text.stripPrefix "(" afterBracketClose
    (url, rest) <- takeLinkDestination afterParen
    if Text.null linkText || Text.null url
        then Nothing
        else Just (linkText, url, rest)

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
