-- | Markdown subset conversion for Telegram HTML messages.
module Agent.Telegram.Markdown
    ( markdownToTelegramHtml
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
            Text.replace "\n" "<br>" (renderInline segment)

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
