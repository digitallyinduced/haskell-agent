-- | Lightweight fullscreen Markdown block rendering.
module Agent.CLI.TUI.Markdown
    ( markdownWidget
    ) where

import qualified Agent.CLI.TUI.Theme as Theme
import Brick
import Data.Text (Text)
import qualified Data.Text as Text

markdownWidget :: Text -> Widget n
markdownWidget input =
    vBox (renderLines False (Text.lines input))

renderLines :: Bool -> [Text] -> [Widget n]
renderLines _ [] = []
renderLines inFence (line : rest)
    | isFence line =
        renderLines (not inFence) rest
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
    | Just quote <- Text.stripPrefix "> " (Text.stripStart line) =
        hBox
            [ withAttr Theme.mutedAttr (txt "│ ")
            , withAttr Theme.mutedAttr (txtWrap quote)
            ]
            : renderLines False rest
    | Text.null (Text.strip line) =
        txt " " : renderLines False rest
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

asumPrefix :: [Text] -> Text -> Maybe Text
asumPrefix prefixes text = case prefixes of
    [] -> Nothing
    prefix : rest -> case Text.stripPrefix prefix text of
        Just value -> Just value
        Nothing -> asumPrefix rest text
