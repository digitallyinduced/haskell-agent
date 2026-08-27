module Agent.ToolArgs
    ( Object
    , objectArgs
    , objectArgsExact
    , objectArgsLenient
    , reqText
    , reqTextList
    , reqInt
    , optText
    , optTextList
    , optInt
    , optIntOrString
    , readExactInt
    , optBool
    , optBoolStrict
    , intOr
    , optList
    , rejectField
    ) where

import Control.Applicative ((<|>))
import Control.Monad (join)
import Agent.Json.Decode (Decoder, FieldsDecoder)
import Agent.Json (rawJsonDecoder)
import qualified Agent.Json.Decode as Json
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as TextRead

-- | A marker passed to field helpers. Hermes keeps the actual simdjson object
-- cursor private, which prevents a decoder from accidentally retaining it.
data Object = Object

-- | Decode model-facing tool arguments from an object. Unknown fields are
-- intentionally ignored.
objectArgs :: (Object -> FieldsDecoder a) -> Decoder a
objectArgs fields =
    Json.object (fields Object)

-- | Kept as a source-compatible name for protocol-shaped arguments. The JSON
-- contract is "known fields only", so unknown fields are ignored everywhere.
objectArgsExact :: [Text] -> (Object -> FieldsDecoder a) -> Decoder a
objectArgsExact _ = objectArgs

-- | All tool argument documents are JSON objects. Optional/defaulted fields do
-- not make a scalar or array a valid argument document.
objectArgsLenient :: (Object -> FieldsDecoder a) -> Decoder a
objectArgsLenient = objectArgs

reqText :: Object -> Text -> FieldsDecoder Text
reqText _ key =
    Json.atKey key Json.text

-- | Required array-of-strings, tolerating a single bare string.
reqTextList :: Object -> Text -> FieldsDecoder [Text]
reqTextList _ key =
    Json.atKey key (Json.list Json.text <|> ((: []) <$> Json.text))

reqInt :: Object -> Text -> FieldsDecoder Int
reqInt _ key =
    Json.atKey key Json.int

-- | Optional string; an empty string counts as absent.
optText :: Object -> Text -> FieldsDecoder (Maybe Text)
optText _ key =
    fmap (maybe Nothing nonEmpty . join) $
        Json.atKeyOptional key (Json.nullable Json.text)
  where
    nonEmpty value
        | Text.null value = Nothing
        | otherwise = Just value

optTextList :: Object -> Text -> FieldsDecoder (Maybe [Text])
optTextList _ key =
    join <$> Json.atKeyOptional key
        (Json.nullable (Json.list Json.text <|> ((: []) <$> Json.text)))

-- | Optional exact, bounded integer. An absent or null field is 'Nothing';
-- fractional, out-of-range, and wrongly typed values fail.
optInt :: Object -> Text -> FieldsDecoder (Maybe Int)
optInt _ key =
    join <$> Json.atKeyOptional key (Json.nullable Json.int)

-- | 'optInt' plus compatibility for integer strings emitted by some model
-- tool surfaces.
optIntOrString :: Object -> Text -> FieldsDecoder (Maybe Int)
optIntOrString _ key =
    join <$> Json.atKeyOptional key (Json.nullable intOrString)
  where
    intOrString =
        Json.int
            <|> Json.withText
                (\value -> maybe (fail "Expected integer") pure (readExactInt value))

-- | Read a signed decimal 'Int' without rounding or overflow.
readExactInt :: Text -> Maybe Int
readExactInt value = do
    (integer, rest) <-
        either (const Nothing) Just $
            TextRead.signed TextRead.decimal (Text.strip value)
    if Text.null rest
        && integer >= toInteger (minBound :: Int)
        && integer <= toInteger (maxBound :: Int)
        then Just (fromInteger integer)
        else Nothing

-- | Optional boolean accepting stringy forms.
optBool :: Object -> Text -> FieldsDecoder (Maybe Bool)
optBool _ key =
    join <$> Json.atKeyOptional key (Json.nullable boolValue)
  where
    boolValue =
        Json.bool
            <|> Json.withText \value ->
                case Text.toLower (Text.strip value) of
                    "true" -> pure True
                    "false" -> pure False
                    "1" -> pure True
                    "0" -> pure False
                    _ -> fail "Expected boolean"

-- | Optional boolean that accepts only a real JSON boolean.
optBoolStrict :: Object -> Text -> FieldsDecoder (Maybe Bool)
optBoolStrict _ key =
    join <$> Json.atKeyOptional key (Json.nullable Json.bool)

intOr :: Object -> Text -> Int -> FieldsDecoder Int
intOr _ key def =
    maybe def id . join
        <$> Json.atKeyOptional key (Json.nullable Json.int)

-- | Optional array field decoded element-wise with the supplied Hermes
-- decoder. The message is retained in the API for model-facing call sites;
-- Hermes supplies the precise path and expected-array error.
optList
    :: Decoder a
    -> Object
    -> Text
    -> Text
    -> FieldsDecoder (Maybe [a])
optList decoder _ key _ =
    Json.atKeyOptional key (Json.list decoder)

-- | Reject one specifically retired parameter while continuing to ignore
-- unrelated unknown fields.
rejectField :: Object -> Text -> Text -> FieldsDecoder ()
rejectField _ key message =
    Json.atKeyOptional key rawJsonDecoder >>= \case
        Nothing -> pure ()
        Just _ -> fail (Text.unpack message)
