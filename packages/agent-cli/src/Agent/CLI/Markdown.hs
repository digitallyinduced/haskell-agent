-- | Turn CommonMark-ish assistant text into ANSI-styled terminal output.
module Agent.CLI.Markdown
    ( renderMarkdown
    ) where

import Agent.CLI.Style (style)
import Control.Applicative ((<|>))
import Data.Char (isAlphaNum, isDigit, isSpace)
import Data.List (transpose)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Console.ANSI
    ( Color(..)
    , ColorIntensity(..)
    , ConsoleIntensity(..)
    , ConsoleLayer(..)
    , SGR(..)
    , Underlining(..)
    )

-- | When @color@ is 'True', style a useful subset of GFM for a TTY.
-- When 'False', return @text@ unchanged (pipes, redirects, tests).
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
                            [ style True
                                [ SetConsoleIntensity FaintIntensity
                                , SetColor Foreground Dull Cyan
                                ]
                                info
                            ]
                styledBody = map (style True [SetConsoleIntensity FaintIntensity]) body
            in header ++ styledBody ++ go after
        | Just (styled, after) <- takeTable (line : rest) =
            styled ++ go after
        | Just (level, title) <- headingLine line =
            let marker = Text.replicate level "#"
                prefix = style True (headingPrefixStyle level) (marker <> " ")
            in (prefix <> styleInline title) : go rest
        | Just item <- unorderedItem line =
            ( style True listMarkerStyle "• "
                <> styleInline item
            )
                : go rest
        | Just (digits, item) <- orderedItemParts line =
            ( style True listMarkerStyle (digits <> ". ")
                <> styleInline item
            )
                : go rest
        | Just quote <- blockQuote line =
            let body
                    | Text.any (`elem` ['`', '*', '_', '[']) quote = styleInline quote
                    | otherwise = style True quoteStyle quote
            in (style True [SetConsoleIntensity FaintIntensity] "│ " <> body)
                : go rest
        | isThematicBreak line =
            style True [SetConsoleIntensity FaintIntensity] (Text.replicate 40 "─")
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
    1 -> [SetColor Foreground Dull Magenta]
    2 -> [SetColor Foreground Dull Cyan]
    3 -> [SetColor Foreground Dull Blue]
    4 -> [SetColor Foreground Dull Yellow]
    5 -> [SetColor Foreground Dull Green]
    _ -> []

listMarkerStyle :: [SGR]
listMarkerStyle =
    [ SetConsoleIntensity BoldIntensity
    , SetColor Foreground Dull Cyan
    ]

-- | Blockquote body: faint so it sits behind surrounding prose.
quoteStyle :: [SGR]
quoteStyle = [SetConsoleIntensity FaintIntensity]

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
            styled =
                zipWith
                    (\i row -> styleTableRow (i == 0) widths row)
                    [0 :: Int ..]
                    rows
        in Just (styled, after)
takeTable _ = Nothing

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
    in map (maximum . (0 :) . map Text.length) cols

styleTableRow :: Bool -> [Int] -> [Text] -> Text
styleTableRow isHeader widths cells =
    let padded =
            zipWith
                (\w c -> c <> Text.replicate (max 0 (w - Text.length c)) " ")
                widths
                (cells ++ repeat "")
        line = Text.intercalate "  " (take (length widths) padded)
    in if isHeader
        then style True [SetConsoleIntensity BoldIntensity] line
        else styleInline line

styleInline :: Text -> Text
styleInline = Text.concat . go Nothing
  where
    go :: Maybe Char -> Text -> [Text]
    go prev t
        | Text.null t = []
        | Just (code, rest) <- takeInlineCode t =
            style True codeStyle code : go (Text.unsnoc code >>= Just . snd) rest
        | Just (linkText, url, rest) <- takeLink t =
            style True linkStyle (styleInline linkText)
                : style True urlStyle (" (" <> url <> ")")
                : go (Just ')') rest
        | Just (inner, rest) <- takeEmphasis prev "**" t =
            style True [SetConsoleIntensity BoldIntensity] (styleInline inner)
                : go (Text.unsnoc inner >>= Just . snd) rest
        | Just (inner, rest) <- takeEmphasis prev "__" t =
            style True [SetConsoleIntensity BoldIntensity] (styleInline inner)
                : go (Text.unsnoc inner >>= Just . snd) rest
        | Just (inner, rest) <- takeEmphasis prev "*" t =
            style True [SetItalicized True] (styleInline inner)
                : go (Text.unsnoc inner >>= Just . snd) rest
        | Just (inner, rest) <- takeEmphasis prev "_" t =
            style True [SetItalicized True] (styleInline inner)
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
        , SetColor Foreground Dull Cyan
        ]
    linkStyle =
        [ SetColor Foreground Dull Blue
        , SetUnderlining SingleUnderline
        ]
    urlStyle = [SetConsoleIntensity FaintIntensity]

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
