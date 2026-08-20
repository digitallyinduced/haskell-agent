-- | Turn CommonMark-ish assistant text into ANSI-styled terminal output.
module Agent.CLI.Markdown
    ( renderMarkdown
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
import Control.Applicative ((<|>))
import Data.Char (isAlphaNum, isDigit, isSpace)
import Data.Colour (Colour)
import Data.List (transpose)
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
    | otherwise =
        let cleaned = Text.filter (/= '\ESC') text
            endsWithNewline = Text.isSuffixOf "\n" cleaned
            lines_ = Text.splitOn "\n" cleaned
            linesForParse
                | endsWithNewline && not (null lines_) = init lines_
                | otherwise = lines_
            rendered = Text.intercalate "\n" (renderBlocks linesForParse)
        in if endsWithNewline then rendered <> "\n" else rendered

-- | Style helper that keeps the agent line wash across nested SGR.
md :: [SGR] -> Text -> Text
md = styleBase True agentBackground

renderBlocks :: [Text] -> [Text]
renderBlocks = go
  where
    go [] = []
    go (line : rest)
        | Just (marker, info) <- fenceOpen line =
            let (body, after) = takeFenceBody marker rest
                header =
                    if Text.null info
                        then []
                        else
                            [ md [fg solarizedCyan] info ]
                styledBody = map (md [fg solarizedBase01]) body
            in header ++ styledBody ++ go after
        | Just (styled, after) <- takeTable (line : rest) =
            styled ++ go after
        | Just (level, title) <- headingLine line =
            let marker = Text.replicate level "#"
                prefix = md (headingPrefixStyle level) (marker <> " ")
            in (prefix <> styleInline title) : go rest
        | Just item <- unorderedItem line =
            ( md listMarkerStyle "• "
                <> styleInline item
            )
                : go rest
        | Just (digits, item) <- orderedItemParts line =
            ( md listMarkerStyle (digits <> ". ")
                <> styleInline item
            )
                : go rest
        | Just quote <- blockQuote line =
            let body
                    | Text.any (`elem` ['`', '*', '_', '[']) quote = styleInline quote
                    | otherwise = md quoteStyle quote
            in (md [fg solarizedBase01] "│ " <> body)
                : go rest
        | isThematicBreak line =
            md [fg solarizedBase01] (Text.replicate 40 "─")
                : go rest
        | Text.null (Text.strip line) = line : go rest
        | otherwise = styleInline line : go rest

-- | Heading marker style: bold + underline + level color.
headingPrefixStyle :: Int -> [SGR]
headingPrefixStyle level =
    SetConsoleIntensity BoldIntensity
        : SetUnderlining SingleUnderline
        : headingColor level

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

data FenceMarker = BacktickFence !Int | TildeFence !Int

fenceOpen :: Text -> Maybe (FenceMarker, Text)
fenceOpen line =
    let stripped = Text.stripStart line
        tryFence char ctor =
            let (run, after) = Text.span (== char) stripped
                n = Text.length run
            in if n >= 3
                then Just (ctor n, Text.strip after)
                else Nothing
    in tryFence '`' BacktickFence <|> tryFence '~' TildeFence

takeFenceBody :: FenceMarker -> [Text] -> ([Text], [Text])
takeFenceBody marker = go
  where
    go [] = ([], [])
    go (line : rest)
        | isFenceClose marker line = ([], rest)
        | otherwise =
            let (body, after) = go rest
            in (line : body, after)

isFenceClose :: FenceMarker -> Text -> Bool
isFenceClose marker line =
    let stripped = Text.strip line
        (char, need) = case marker of
            BacktickFence n -> ('`', n)
            TildeFence n -> ('~', n)
        (run, after) = Text.span (== char) stripped
    in Text.length run >= need && Text.null (Text.strip after)

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

unorderedItem :: Text -> Maybe Text
unorderedItem line =
    let stripped = Text.stripStart line
    in case Text.uncons stripped of
        Just (c, rest)
            | c `elem` ['-', '*', '+']
            , Just (sp, after) <- Text.uncons rest
            , isSpace sp ->
                Just (Text.strip after)
        _ -> Nothing

-- | Ordered list: number marker and item body separately so the marker can
-- be colored without restyling the whole line twice.
orderedItemParts :: Text -> Maybe (Text, Text)
orderedItemParts line =
    let stripped = Text.stripStart line
        (digits, after) = Text.span isDigit stripped
    in if not (Text.null digits)
        then case Text.stripPrefix ". " after of
            Just item -> Just (digits, Text.strip item)
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
    | isTableRow header
    , isSeparatorRow sep =
        let (body, after) = span isTableRow rest
            rows = map splitRow (header : body)
            widths = columnWidths rows
            top = md [fg solarizedBase01] (boxLine '┌' '┬' '┐' '─' widths)
            mid = md [fg solarizedBase01] (boxLine '├' '┼' '┤' '─' widths)
            bot = md [fg solarizedBase01] (boxLine '└' '┴' '┘' '─' widths)
            headerRow = styleTableRow True widths (head rows)
            bodyRows = map (styleTableRow False widths) (drop 1 rows)
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
isTableRow line =
    let stripped = Text.strip line
    in Text.isPrefixOf "|" stripped && Text.count "|" stripped >= 2

isSeparatorRow :: Text -> Bool
isSeparatorRow line =
    isTableRow line && all isSepCell (splitRow line)
  where
    isSepCell cell =
        let t = Text.filter (`notElem` ['-', ':', ' ']) cell
        in Text.null t && Text.any (== '-') cell

splitRow :: Text -> [Text]
splitRow line =
    let stripped = Text.strip line
        trimmed =
            Text.dropWhile (== '|')
                (Text.dropWhileEnd (== '|') stripped)
    in map Text.strip (Text.splitOn "|" trimmed)

columnWidths :: [[Text]] -> [Int]
columnWidths rows =
    let cols = transpose rows
    in map (maximum . (0 :) . map (Text.length . visibleCellText)) cols

-- | Approximate rendered cell text width by stripping common inline markers
-- before padding, so borders align when body cells contain ``code`` / emphasis.
visibleCellText :: Text -> Text
visibleCellText = Text.replace "`" "" . Text.replace "**" "" . Text.replace "__" ""
    . Text.replace "*" "" . Text.replace "_" ""

styleTableRow :: Bool -> [Int] -> [Text] -> Text
styleTableRow isHeader widths cells =
    let cellText w c =
            let visible = visibleCellText c
                pad = max 0 (w - Text.length visible)
                body = " " <> visible <> Text.replicate pad " " <> " "
            in if isHeader
                then md [SetConsoleIntensity BoldIntensity] body
                else
                    -- Markers were stripped for width; re-apply inline styling
                    -- only for bare visible text (no markers left to parse).
                    md [] body
        parts = zipWith cellText widths (cells ++ repeat "")
        bar = md [fg solarizedBase01] "│"
    in bar <> Text.intercalate bar (take (length widths) parts) <> bar

styleInline :: Text -> Text
styleInline = Text.concat . go Nothing
  where
    go :: Maybe Char -> Text -> [Text]
    go prev t
        | Text.null t = []
        | Just (code, rest) <- takeInlineCode t =
            md codeStyle code : go (Text.unsnoc code >>= Just . snd) rest
        | Just (linkText, url, rest) <- takeLink t =
            let label = md linkStyle (styleInline linkText)
            in osc8Link True url label
                : md urlStyle (" (" <> url <> ")")
                : go (Just ')') rest
        | Just (inner, rest) <- takeEmphasis prev "**" t =
            md [SetConsoleIntensity BoldIntensity] (styleInline inner)
                : go (Text.unsnoc inner >>= Just . snd) rest
        | Just (inner, rest) <- takeEmphasis prev "__" t =
            md [SetConsoleIntensity BoldIntensity] (styleInline inner)
                : go (Text.unsnoc inner >>= Just . snd) rest
        | Just (inner, rest) <- takeEmphasis prev "*" t =
            md [SetItalicized True] (styleInline inner)
                : go (Text.unsnoc inner >>= Just . snd) rest
        | Just (inner, rest) <- takeEmphasis prev "_" t =
            md [SetItalicized True] (styleInline inner)
                : go (Text.unsnoc inner >>= Just . snd) rest
        | otherwise =
            let (plain, rest) = Text.break (`elem` ['`', '[', '*', '_']) t
            in if Text.null plain
                then
                    let c = Text.take 1 t
                        rest' = Text.drop 1 t
                    in c : go (Text.uncons c >>= Just . fst) rest'
                else
                    plain
                        : go (Text.unsnoc plain >>= Just . snd) rest

    codeStyle =
        [ SetConsoleIntensity BoldIntensity
        , fg solarizedCyan
        ]
    linkStyle =
        [ fg solarizedBlue
        , SetUnderlining SingleUnderline
        ]
    urlStyle = [fg solarizedBase01]

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
    case Text.breakOn "]" afterBracket of
        (linkText, rest0)
            | not (Text.null linkText)
            , Just afterBracketClose <- Text.stripPrefix "]" rest0
            , Just afterParen <- Text.stripPrefix "(" afterBracketClose ->
                case Text.breakOn ")" afterParen of
                    (url, rest1)
                        | not (Text.null url)
                        , Just rest <- Text.stripPrefix ")" rest1 ->
                            Just (linkText, url, rest)
                    _ -> Nothing
            | otherwise -> Nothing

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
