-- | Turn CommonMark-ish assistant text into ANSI-styled terminal output.
module Agent.CLI.Markdown
    ( renderMarkdown
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
import Data.List (transpose)
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
renderMarkdown color text
    | not color = text
    | otherwise = Text.concat (map renderChunk (fenceChunks cleaned))
  where
    cleaned = displayTerminalText text
    renderChunk (FenceText prose) = renderProse prose
    renderChunk (FenceBlock block) =
        let header
                | Text.null block.fencedInfo = ""
                | otherwise = md [terminalCyan] block.fencedInfo <> "\n"
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
                (map (md [terminalMuted]) actualLines)
    in if endsWithNewline then rendered <> "\n" else rendered

-- | Style helper that restores the agent's terminal-default background.
md :: [SGR] -> Text -> Text
md = styleBase True agentBackground

renderBlocks :: [Text] -> [Text]
renderBlocks = go
  where
    go [] = []
    go (line : rest)
        | Just (rows, after) <- Block.takeTableRows (line : rest) =
            renderTable rows ++ go after
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

renderTable :: [[Text]] -> [Text]
renderTable [] = []
renderTable rows@(headerCells : bodyCells) =
    let widths = columnWidths rows
        top = md [terminalMuted] (boxLine '┌' '┬' '┐' '─' widths)
        mid = md [terminalMuted] (boxLine '├' '┼' '┤' '─' widths)
        bot = md [terminalMuted] (boxLine '└' '┴' '┘' '─' widths)
        headerRow = styleTableRow True widths headerCells
        bodyRows = map (styleTableRow False widths) bodyCells
    in [top, headerRow, mid] ++ bodyRows ++ [bot]

boxLine :: Char -> Char -> Char -> Char -> [Int] -> Text
boxLine left mid right fill widths =
    Text.singleton left
        <> Text.intercalate (Text.singleton mid)
            [ Text.replicate (w + 2) (Text.singleton fill) | w <- widths ]
        <> Text.singleton right

columnWidths :: [[Text]] -> [Int]
columnWidths rows =
    let cols = transpose rows
    in map (maximum . (0 :) . map renderedWidth) cols
  where
    renderedWidth =
        terminalDisplayWidth
            . inlinePlainText
            . parseInline

styleTableRow :: Bool -> [Int] -> [Text] -> Text
styleTableRow isHeader widths cells =
    let cellText w c =
            let inlines = parseInline c
                visible = inlinePlainText inlines
                width' = terminalDisplayWidth visible
                padding = max 0 (w - width')
                base
                    | isHeader = [SetConsoleIntensity BoldIntensity]
                    | otherwise = []
            in " "
                <> renderInlineWith base inlines
                <> Text.replicate padding " "
                <> " "
        parts = zipWith cellText widths (cells ++ repeat "")
        bar = md [terminalMuted] "│"
    in bar <> Text.intercalate bar (take (length widths) parts) <> bar

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
                suffix
                    | Text.null url || inlinePlainText children == url = ""
                    | otherwise =
                        styled (context <> urlStyle) (" (" <> url <> ")")
                displayedLink = label <> suffix
            in if safeUrl url
                then osc8Link True url displayedLink
                else displayedLink

    styled [] value = value
    styled styles value = md styles value

    safeUrl url =
        not (Text.null url)
            && displayTerminalText url == url

    codeStyle =
        [ SetConsoleIntensity BoldIntensity
        , terminalCyan
        ]
    linkStyle =
        [ terminalBlue
        , SetUnderlining SingleUnderline
        ]
    urlStyle = [terminalMuted]

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
