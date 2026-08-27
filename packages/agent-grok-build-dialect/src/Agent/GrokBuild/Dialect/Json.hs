module Agent.GrokBuild.Dialect.Json
    ( optionalBool
    , optionalInt
    , optionalIntOrString
    , optionalText
    , optionalTextValue
    , requiredTextList
    , textList
    ) where

import qualified Agent.Json.Decode as Json
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text

optionalText :: Text -> Json.FieldsDecoder (Maybe Text)
optionalText key =
    fmap (>>= nonEmpty) (optionalTextValue key)
  where
    nonEmpty value
        | Text.null value = Nothing
        | otherwise = Just value

optionalTextValue :: Text -> Json.FieldsDecoder (Maybe Text)
optionalTextValue key =
    Json.optionalKey key Json.text

optionalInt :: Text -> Json.FieldsDecoder (Maybe Int)
optionalInt key =
    Json.optionalKey key Json.int

optionalIntOrString :: Text -> Json.FieldsDecoder (Maybe Int)
optionalIntOrString key =
    Json.optionalKey key intOrString

optionalBool :: Text -> Json.FieldsDecoder (Maybe Bool)
optionalBool key =
    Json.optionalKey key boolOrString

requiredTextList :: Text -> Json.FieldsDecoder [Text]
requiredTextList key = Json.atKey key textList

textList :: Json.Decoder [Text]
textList = Json.withType \case
    Json.VArray -> Json.list Json.text
    Json.VString -> (: []) <$> Json.text
    _ -> fail "expected a string or array of strings"

intOrString :: Json.Decoder Int
intOrString = Json.withType \case
    Json.VNumber -> Json.int
    Json.VString -> Json.withText \value ->
        case Text.signed Text.decimal (Text.strip value) of
            Right (number, rest)
                | Text.null rest -> pure number
            _ -> fail "expected integer"
    _ -> fail "expected integer"

boolOrString :: Json.Decoder Bool
boolOrString = Json.withType \case
    Json.VBoolean -> Json.bool
    Json.VString -> Json.withText \value ->
        case Text.toLower (Text.strip value) of
            "true" -> pure True
            "1" -> pure True
            "false" -> pure False
            "0" -> pure False
            _ -> fail "expected boolean"
    _ -> fail "expected boolean"
