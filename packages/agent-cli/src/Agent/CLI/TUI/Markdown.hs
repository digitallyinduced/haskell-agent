-- | Lightweight fullscreen Markdown block rendering.
module Agent.CLI.TUI.Markdown
    ( markdownWidget
    ) where

import qualified Agent.CLI.TUI.Theme as Theme
import Brick
import Data.Char (isDigit, isSpace)
import Data.List (transpose)
import Data.Text (Text)
import qualified Data.Text as Text

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
            (padTop (Pad 1) (txtWrap heading))
            : renderLines False rest
    | Just item <- stripBullet line =
        hBox
            [ withAttr Theme.headingAttr (txt "• ")
            , txtWrap item
            ]
            : renderLines False rest
    | Just (number, item) <- stripOrdered line =
        hBox
            [ withAttr Theme.headingAttr (txt (number <> ". "))
            , txtWrap item
            ]
            : renderLines False rest
    | Just quote <- Text.stripPrefix "> " (Text.stripStart line) =
        hBox
            [ withAttr Theme.mutedAttr (txt "│ ")
            , withAttr Theme.mutedAttr (txtWrap quote)
            ]
            : renderLines False rest
    | Text.null (Text.strip line) =
        txt " " : renderLines False rest
    | isThematicBreak line =
        withAttr Theme.mutedAttr (fill '─')
            : renderLines False rest
    | otherwise =
        txtWrap line : renderLines False rest

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
                                    (hLimit width (txt cell))
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
