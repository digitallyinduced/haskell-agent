-- | Lightweight fullscreen Markdown block rendering.
module Agent.TUI.Markdown
    ( InlineSpan(..)
    , InlineStyle(..)
    , inlinePlainText
    , markdownWidget
    , markdownWidgetWithCodeControls
    , parseInline
    ) where

import qualified Agent.TUI.Theme as Theme
import Brick
import qualified Brick.Types as B
import Data.Char
    ( GeneralCategory(..)
    , generalCategory
    , isDigit
    , isSpace
    , ord
    )
import Data.List (transpose)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as Builder
import qualified Graphics.Vty as V

terminalCharWidth :: Char -> Int
terminalCharWidth char
    | char == '\n' || char == '\r' || char == '\t' = 1
    | code <= 0x1f || code == 0x7f = 1
    | code >= 0x80 && code <= 0x9f = 1
    | category == Format = 1
    | category `elem` [NonSpacingMark, SpacingCombiningMark, EnclosingMark] = 0
    | category `elem` [Control, Surrogate, NotAssigned] = 0
    | isWideCharacter char = 2
    | otherwise = 1
  where
    code = ord char
    category = generalCategory char

isWideCharacter :: Char -> Bool
isWideCharacter char =
    let code = ord char
    in code >= 0x1100
        && ( code <= 0x115f
            || code == 0x2329
            || code == 0x232a
            || (code >= 0x2e80 && code <= 0xa4cf && code /= 0x303f)
            || (code >= 0xac00 && code <= 0xd7a3)
            || (code >= 0xf900 && code <= 0xfaff)
            || (code >= 0xfe10 && code <= 0xfe19)
            || (code >= 0xfe30 && code <= 0xfe6f)
            || (code >= 0xff00 && code <= 0xff60)
            || (code >= 0xffe0 && code <= 0xffe6)
            || (code >= 0x1f300 && code <= 0x1faff)
            || (code >= 0x20000 && code <= 0x3fffd)
           )

data InlineStyle
    = InlinePlain
    | InlineStrong
    | InlineEmphasis
    | InlineCode
    | InlineLink
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
    vBox (renderLines codeHeader 1 False (Text.lines input))

renderLines
    :: (Int -> Text -> Widget n)
    -> Int
    -> Bool
    -> [Text]
    -> [Widget n]
renderLines _ _ _ [] = []
renderLines codeHeader codeIndex False lines_
    | Just (table, rest) <- takeTable lines_ =
        table : renderLines codeHeader codeIndex False rest
renderLines codeHeader codeIndex inFence (line : rest)
    | isFence line =
        let language = Text.strip (Text.drop 3 (Text.stripStart line))
        in if inFence
            then renderLines codeHeader codeIndex False rest
            else
                codeHeader codeIndex language
                    : renderLines codeHeader (codeIndex + 1) True rest
    | inFence =
        withAttr Theme.codeAttr
            (padLeftRight 1 (txt line))
            : renderLines codeHeader codeIndex True rest
    | Just heading <- stripHeading line =
        withAttr Theme.headingAttr
            (padTop (Pad 1) (inlineWidget (parseInline heading)))
            : renderLines codeHeader codeIndex False rest
    | Just item <- stripBullet line =
        hBox
            [ withAttr Theme.headingAttr (txt "• ")
            , inlineWidget (parseInline item)
            ]
            : renderLines codeHeader codeIndex False rest
    | Just (number, item) <- stripOrdered line =
        hBox
            [ withAttr Theme.headingAttr (txt (number <> ". "))
            , inlineWidget (parseInline item)
            ]
            : renderLines codeHeader codeIndex False rest
    | Just quote <- Text.stripPrefix "> " (Text.stripStart line) =
        hBox
            [ withAttr Theme.mutedAttr (txt "│ ")
            , withAttr Theme.mutedAttr (inlineWidget (parseInline quote))
            ]
            : renderLines codeHeader codeIndex False rest
    | Text.null (Text.strip line) =
        txt " " : renderLines codeHeader codeIndex False rest
    | isThematicBreak line =
        withAttr Theme.mutedAttr (fill '─')
            : renderLines codeHeader codeIndex False rest
    | otherwise =
        inlineWidget (parseInline line)
            : renderLines codeHeader codeIndex False rest

isFence :: Text -> Bool
isFence line =
    let stripped = Text.stripStart line
    in "```" `Text.isPrefixOf` stripped
        || "~~~" `Text.isPrefixOf` stripped

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
    | isTableRow rawHeader
    , isSeparatorRow separator =
        let
            (body, after) = span isTableRow rest
            rows = map splitRow (rawHeader : body)
            headerCells = splitRow rawHeader
            widths = map (maximum . (0 :) . map Text.length) (transpose rows)
            renderRow headerRow cells =
                hBox $
                    [withAttr Theme.mutedAttr (txt "│")]
                        <> concat
                            [ [ padLeftRight 1 $
                                    (if headerRow
                                        then withAttr Theme.headingAttr
                                        else id)
                                    (hLimit width
                                        (inlineWidget (parseInline cell)))
                              , withAttr Theme.mutedAttr (txt "│")
                              ]
                            | (width, cell) <- zip widths
                                (cells <> repeat "")
                            ]
            divider =
                withAttr Theme.mutedAttr $
                    txt $
                        "├"
                            <> Text.intercalate "┼"
                                [ Text.replicate (width + 2) "─"
                                | width <- widths
                                ]
                            <> "┤"
        in Just
            (vBox
                (renderRow True headerCells
                    : divider
                    : map (renderRow False) (drop 1 rows))
            , after)
takeTable _ = Nothing

isTableRow :: Text -> Bool
isTableRow line =
    let stripped = Text.strip line
    in Text.isPrefixOf "|" stripped && Text.count "|" stripped >= 2

isSeparatorRow :: Text -> Bool
isSeparatorRow line =
    isTableRow line && all isSeparatorCell (splitRow line)
  where
    isSeparatorCell cell =
        Text.any (== '-') cell
            && Text.null
                (Text.filter (`notElem` ['-', ':', ' ']) cell)

splitRow :: Text -> [Text]
splitRow =
    map Text.strip
        . Text.splitOn "|"
        . Text.dropWhileEnd (== '|')
        . Text.dropWhile (== '|')
        . Text.strip

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
                InlineSpan InlineStrong body
                    : go (lastChar body) [] rest
        | Just (body, rest) <- delimited "__" text =
            flushPlain plain $
                InlineSpan InlineStrong body
                    : go (lastChar body) [] rest
        | Just (body, rest) <- codeSpan text =
            flushPlain plain $
                InlineSpan InlineCode body
                    : go (lastChar body) [] rest
        | Just (label, url, rest) <- linkSpan text =
            flushPlain plain $
                InlineSpan InlineLink
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
        styled <- traverse resolve spans
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
  where
    resolve InlineSpan{inlineStyle, inlineText} = do
        attr <- B.lookupAttrName (styleAttr inlineStyle)
        pure (attr, inlineText)

styleAttr :: InlineStyle -> AttrName
styleAttr = \case
    InlinePlain -> Theme.assistantAttr
    InlineStrong -> Theme.strongAttr
    InlineEmphasis -> Theme.emphasisAttr
    InlineCode -> Theme.inlineCodeAttr
    InlineLink -> Theme.linkAttr

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
