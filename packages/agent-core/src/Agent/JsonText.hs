-- | Extract a string field from a JSON object payload (tool arguments).
module Agent.JsonText
    ( jsonTextField
    , jsonTextFieldDefault
    , jsonTextFieldPartial
    ) where

import qualified Agent.Json.Decode as Json
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Char (isHexDigit, isSpace)
import Data.Text (Text)
import qualified Data.Text as Text

-- | Read a string field from a JSON object encoded as 'Text'.
jsonTextField :: Text -> Text -> Maybe Text
jsonTextField key arguments =
    either (const Nothing) Just $
        Json.decodeText (Json.object (Json.atKey key Json.text)) arguments

-- | Like 'jsonTextField', returning empty text when missing or not a string.
jsonTextFieldDefault :: Text -> Text -> Text
jsonTextFieldDefault key arguments =
    maybe "" id (jsonTextField key arguments)

-- | Read a string field from a JSON object that may still be streaming.
--
-- Complete JSON takes the normal decoder path. For an incomplete object, this
-- locates the requested string value and decodes the complete JSON escapes
-- received so far. A trailing incomplete escape is omitted until its remaining
-- characters arrive.
jsonTextFieldPartial :: Text -> Text -> Maybe Text
jsonTextFieldPartial key arguments =
    jsonTextField key arguments <|> do
        let marker = "\"" <> key <> "\""
            (_, marked) = Text.breakOn marker arguments
        guard (not (Text.null marked))
        let afterKey = Text.dropWhile isSpace (Text.drop (Text.length marker) marked)
        (':', afterColon) <- Text.uncons afterKey
        ('"', bodyStart) <- Text.uncons (Text.dropWhile isSpace afterColon)
        let encodedBody = completeEscapePrefix (takeStringBody bodyStart)
        either (const Nothing) Just $
            Json.decodeText Json.text ("\"" <> encodedBody <> "\"")

takeStringBody :: Text -> Text
takeStringBody = Text.pack . go False . Text.unpack
  where
    go _ [] = []
    go escaped (char : rest)
        | char == '"' && not escaped = []
        | char == '\\' && not escaped = char : go True rest
        | otherwise = char : go False rest

completeEscapePrefix :: Text -> Text
completeEscapePrefix body
    | odd (Text.length trailingSlashes) = Text.dropEnd 1 body
    | otherwise =
        case Text.breakOnEnd "\\u" body of
            ("", _) -> body
            (_throughMarker, suffix)
                | Text.length suffix < 4
                , Text.all isHexDigit suffix ->
                    Text.dropEnd (2 + Text.length suffix) body
                | otherwise -> body
  where
    trailingSlashes = Text.takeWhileEnd (== '\\') body
