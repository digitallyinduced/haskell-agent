-- | Markdown subset conversion for Telegram HTML messages.
module Agent.Telegram.Markdown
    ( markdownToTelegramHtml
    , telegramRenderedLength
    , escapeTelegramHtml
    ) where

import Data.Text (Text)
import qualified Data.Text as Text

markdownToTelegramHtml :: Text -> Text
markdownToTelegramHtml input =
    Text.concat (zipWith renderSegment [0 :: Int ..] (Text.splitOn "```" input))
  where
    renderSegment index segment
        | odd index =
            "<pre>" <> escapeTelegramHtml (stripLanguage segment) <> "</pre>"
        | otherwise =
            renderBlocks segment

    stripLanguage segment =
        case Text.breakOn "\n" segment of
            (language, rest)
                | not (Text.null language)
                , Text.all isLanguageCharacter language ->
                    Text.drop 1 rest
            _ -> segment

    isLanguageCharacter char =
        ('a' <= char && char <= 'z')
            || ('A' <= char && char <= 'Z')
            || ('0' <= char && char <= '9')
            || char `elem` ("-+#_" :: String)

data ColumnAlignment
    = AlignLeft
    | AlignCenter
    | AlignRight

renderBlocks :: Text -> Text
renderBlocks = Text.intercalate "<br>" . go . Text.splitOn "\n"
  where
    go [] = []
    go (header : separator : rest)
        | Just alignments <- parseTableSeparator separator
        , let headerCells = parseTableRow header
        , length headerCells == length alignments
        , length headerCells > 1 =
            let (bodyLines, remaining) =
                    span (isTableRow (length headerCells)) rest
                rows = headerCells : map parseTableRow bodyLines
            in renderTable alignments rows : go remaining
    go (line : rest)
        | Just heading <- parseHeading line =
            ("<b>" <> renderInline heading <> "</b>") : go rest
        | otherwise =
            renderInline line : go rest

    isTableRow columnCount line =
        Text.isInfixOf "|" line
            && length (parseTableRow line) == columnCount

parseHeading :: Text -> Maybe Text
parseHeading line =
    let (hashes, rest) = Text.span (== '#') line
        level = Text.length hashes
    in if level >= 1 && level <= 6
        then Text.stripPrefix " " rest
        else Nothing

parseTableRow :: Text -> [Text]
parseTableRow =
    map (renderInlinePlain . Text.strip)
        . Text.splitOn "|"
        . stripOptionalBoundary "|"
        . Text.strip

parseTableSeparator :: Text -> Maybe [ColumnAlignment]
parseTableSeparator line =
    traverse parseCell rawCells
  where
    rawCells =
        map Text.strip
            . Text.splitOn "|"
            . stripOptionalBoundary "|"
            $ Text.strip line

    parseCell cell =
        let leftAligned = Text.isPrefixOf ":" cell
            rightAligned = Text.isSuffixOf ":" cell
            rule = Text.dropAround (== ':') cell
        in if Text.length rule >= 3 && Text.all (== '-') rule
            then Just case (leftAligned, rightAligned) of
                (True, True) -> AlignCenter
                (False, True) -> AlignRight
                _ -> AlignLeft
            else Nothing

stripOptionalBoundary :: Text -> Text -> Text
stripOptionalBoundary boundary =
    stripSuffix . stripPrefix
  where
    stripPrefix value =
        maybe value id (Text.stripPrefix boundary value)
    stripSuffix value =
        maybe value id (Text.stripSuffix boundary value)

renderTable :: [ColumnAlignment] -> [[Text]] -> Text
renderTable _ [] = ""
renderTable alignments (headerRow : bodyRows) =
    "<pre>"
        <> escapeTelegramHtml
            (Text.intercalate "\n" (header : divider : body))
        <> "</pre>"
  where
    widths =
        foldl
            (zipWith max)
            (map Text.length headerRow)
            (map (map Text.length) bodyRows)
    renderRow row =
        Text.intercalate " | "
            (zipWith3 renderCell alignments widths row)
    header = renderRow headerRow
    divider =
        Text.intercalate "-+-"
            (map (`Text.replicate` "-") widths)
    body = map renderRow bodyRows

    renderCell alignment width cell =
        let missing = width - Text.length cell
            leftPadding =
                case alignment of
                    AlignRight -> missing
                    AlignCenter -> missing `div` 2
                    AlignLeft -> 0
            rightPadding = missing - leftPadding
        in Text.replicate leftPadding " "
            <> cell
            <> Text.replicate rightPadding " "

telegramRenderedLength :: Text -> Int
telegramRenderedLength = go 0 . markdownToTelegramHtml
  where
    go count html
        | Text.null html = count
        | Just rest <- Text.stripPrefix "<br>" html =
            go (count + 1) rest
        | Just rest <- Text.stripPrefix "<" html =
            case Text.breakOn ">" rest of
                (_, afterTag)
                    | Text.null afterTag -> go (count + 1) rest
                    | otherwise -> go count (Text.drop 1 afterTag)
        | Just rest <- Text.stripPrefix "&" html =
            case Text.breakOn ";" rest of
                (_, afterEntity)
                    | Text.null afterEntity -> go (count + 1) rest
                    | otherwise -> go (count + 1) (Text.drop 1 afterEntity)
        | otherwise =
            go (count + 1) (Text.drop 1 html)

renderInline :: Text -> Text
renderInline text
    | Text.null text = ""
    | Just rest <- Text.stripPrefix "`" text =
        renderDelimited "`" "<code>" "</code>" rest
    | Just rest <- Text.stripPrefix "**" text =
        renderDelimited "**" "<b>" "</b>" rest
    | Just rest <- Text.stripPrefix "~~" text =
        renderDelimited "~~" "<s>" "</s>" rest
    | Just rest <- Text.stripPrefix "*" text =
        renderDelimited "*" "<i>" "</i>" rest
    | Just rest <- Text.stripPrefix "[" text =
        case parseMarkdownLink rest of
            Just (label, url, remaining) ->
                "<a href=\"" <> escapeTelegramHtml url <> "\">"
                    <> escapeTelegramHtml label
                    <> "</a>"
                    <> renderInline remaining
            Nothing -> "&#91;" <> renderInline rest
    | otherwise =
        let (plain, rest) = Text.break isMarkdownStarter text
        in if Text.null plain
            then escapeTelegramHtml (Text.take 1 text)
                <> renderInline (Text.drop 1 text)
            else escapeTelegramHtml plain <> renderInline rest
  where
    renderDelimited delimiter opening closing rest =
        case Text.breakOn delimiter rest of
            (_, after) | Text.null after ->
                escapeTelegramHtml delimiter <> renderInline rest
            (inside, after) ->
                opening
                    <> escapeTelegramHtml inside
                    <> closing
                    <> renderInline (Text.drop (Text.length delimiter) after)

    isMarkdownStarter char =
        char == '`' || char == '*' || char == '~' || char == '['

renderInlinePlain :: Text -> Text
renderInlinePlain text
    | Text.null text = ""
    | Just rest <- Text.stripPrefix "`" text =
        renderDelimited "`" rest
    | Just rest <- Text.stripPrefix "**" text =
        renderDelimited "**" rest
    | Just rest <- Text.stripPrefix "~~" text =
        renderDelimited "~~" rest
    | Just rest <- Text.stripPrefix "*" text =
        renderDelimited "*" rest
    | Just rest <- Text.stripPrefix "[" text =
        case parseMarkdownLink rest of
            Just (label, url, remaining) ->
                renderInlinePlain label
                    <> " ("
                    <> url
                    <> ")"
                    <> renderInlinePlain remaining
            Nothing -> "[" <> renderInlinePlain rest
    | otherwise =
        let (plain, rest) = Text.break isMarkdownStarter text
        in if Text.null plain
            then Text.take 1 text <> renderInlinePlain (Text.drop 1 text)
            else plain <> renderInlinePlain rest
  where
    renderDelimited delimiter rest =
        case Text.breakOn delimiter rest of
            (_, after) | Text.null after ->
                delimiter <> renderInlinePlain rest
            (inside, after) ->
                renderInlinePlain inside
                    <> renderInlinePlain
                        (Text.drop (Text.length delimiter) after)

    isMarkdownStarter char =
        char == '`' || char == '*' || char == '~' || char == '['

parseMarkdownLink :: Text -> Maybe (Text, Text, Text)
parseMarkdownLink text = do
    let (label, afterLabel) = Text.breakOn "](" text
    guardNotNull afterLabel
    let afterOpen = Text.drop 2 afterLabel
        (url, afterUrl) = Text.breakOn ")" afterOpen
    guardNotNull afterUrl
    pure (label, url, Text.drop 1 afterUrl)

guardNotNull :: Text -> Maybe ()
guardNotNull value
    | Text.null value = Nothing
    | otherwise = Just ()

escapeTelegramHtml :: Text -> Text
escapeTelegramHtml = Text.concatMap \case
    '<' -> "&lt;"
    '>' -> "&gt;"
    '&' -> "&amp;"
    '"' -> "&quot;"
    '\'' -> "&#39;"
    char -> Text.singleton char
