-- | Lightweight fullscreen Markdown block rendering.
module Agent.CLI.TUI.Markdown
    ( InlineSpan(..)
    , InlineStyle(..)
    , inlinePlainText
    , markdownWidget
    , parseInline
    ) where

import Agent.CLI.Input (terminalCharWidth)
import qualified Agent.CLI.TUI.Theme as Theme
import Brick
import qualified Brick.Types as B
import Data.Char (isDigit, isSpace)
import Data.List (transpose)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as Builder
import qualified Graphics.Vty as V

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
markdownWidget input =
    vBox (renderLines False (Text.lines input))

renderLines :: Bool -> [Text] -> [Widget n]
renderLines _ [] = []
renderLines False lines_
    | Just (table, rest) <- takeTable lines_ =
        table : renderLines False rest
renderLines inFence (line : rest)
    | isFence line =
        let language = Text.strip (Text.drop 3 (Text.stripStart line))
            label
                | inFence || Text.null language = []
                | otherwise =
                    [withAttr Theme.mutedAttr (txt language)]
        in label <> renderLines (not inFence) rest
    | inFence =
        withAttr Theme.codeAttr
            (padLeftRight 1 (txt line))
            : renderLines True rest
    | Just heading <- stripHeading line =
        withAttr Theme.headingAttr
            (padTop (Pad 1) (inlineWidget (parseInline heading)))
            : renderLines False rest
    | Just item <- stripBullet line =
        hBox
            [ withAttr Theme.headingAttr (txt "• ")
            , inlineWidget (parseInline item)
            ]
            : renderLines False rest
    | Just (number, item) <- stripOrdered line =
        hBox
            [ withAttr Theme.headingAttr (txt (number <> ". "))
            , inlineWidget (parseInline item)
            ]
            : renderLines False rest
    | Just quote <- Text.stripPrefix "> " (Text.stripStart line) =
        hBox
            [ withAttr Theme.mutedAttr (txt "│ ")
            , withAttr Theme.mutedAttr (inlineWidget (parseInline quote))
            ]
            : renderLines False rest
    | Text.null (Text.strip line) =
        txt " " : renderLines False rest
    | isThematicBreak line =
        withAttr Theme.mutedAttr (fill '─')
            : renderLines False rest
    | otherwise =
        inlineWidget (parseInline line) : renderLines False rest

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
