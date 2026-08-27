module Agent.ToolArgs
    ( extractText
    , extractTextList
    , extractTextOr
    , extractIntOr
    , extractMaybeText
    , extractMaybeInt
    , extractMaybeBool
    , objectArgs
    , objectArgsExact
    , objectArgsLenient
    , reqText
    , reqTextList
    , reqInt
    , optText
    , optInt
    , optIntOrString
    , readExactInt
    , optBool
    , optBoolStrict
    , intOr
    , optList
    , stripAesonPrefix
    ) where

import Data.Aeson (FromJSON(..), Object, Value(..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser)
import Data.Scientific (Scientific, toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as TextRead
import qualified Data.Vector as Vector

extractText :: Value -> Text -> Either Text Text
extractText (Object obj) key =
    case KeyMap.lookup (Key.fromText key) obj of
        Just (String t) -> Right t
        Just _ -> Left ("Expected string for key: " <> key)
        Nothing -> Left ("Missing parameter: " <> key)
extractText _ _ = Left "Expected object input"

-- | Parse an array-of-strings parameter. Tolerates a single bare string too,
-- so the LLM can get away with @"transaction_ids": "abc"@ when it forgets to
-- wrap the one id in a list.
extractTextList :: Value -> Text -> Either Text [Text]
extractTextList (Object obj) key =
    case KeyMap.lookup (Key.fromText key) obj of
        Just (Array arr) -> mapM extract (Vector.toList arr)
          where
            extract (String t) = Right t
            extract _ = Left ("Expected string entries in array for key: " <> key)
        Just (String t) -> Right [t]
        Just _ -> Left ("Expected array for key: " <> key)
        Nothing -> Left ("Missing parameter: " <> key)
extractTextList _ _ = Left "Expected object input"

extractTextOr :: Value -> Text -> Text -> Text
extractTextOr (Object obj) key def =
    case KeyMap.lookup (Key.fromText key) obj of
        Just (String t) -> t
        _ -> def
extractTextOr _ _ def = def

extractIntOr :: Value -> Text -> Int -> Int
extractIntOr (Object obj) key def =
    case KeyMap.lookup (Key.fromText key) obj of
        Just (Number n) -> maybe def id (exactInt n)
        _ -> def
extractIntOr _ _ def = def

extractMaybeText :: Value -> Text -> Maybe Text
extractMaybeText (Object obj) key =
    case KeyMap.lookup (Key.fromText key) obj of
        Just (String t) | not (Text.null t) -> Just t
        _ -> Nothing
extractMaybeText _ _ = Nothing

extractMaybeInt :: Value -> Text -> Maybe Int
extractMaybeInt (Object obj) key =
    case KeyMap.lookup (Key.fromText key) obj of
        Just (Number n) -> exactInt n
        _ -> Nothing
extractMaybeInt _ _ = Nothing

-- | Accept a real JSON boolean as well as the stringy forms LLMs sometimes
-- emit ("true"/"false"/"1"/"0", case-insensitive, whitespace-tolerant).
extractMaybeBool :: Value -> Text -> Maybe Bool
extractMaybeBool (Object obj) key =
    KeyMap.lookup (Key.fromText key) obj >>= boolValue
extractMaybeBool _ _ = Nothing

look :: Object -> Text -> Maybe Value
look obj key = KeyMap.lookup (Key.fromText key) obj

-- | Begin a tool-argument 'FromJSON' instance. A non-object input fails with
-- the same message the legacy 'extractText' produced for non-objects, so the
-- model gets an identical, retry-able error.
objectArgs :: (Object -> Parser a) -> Value -> Parser a
objectArgs f (Object obj) = f obj
objectArgs _ _ = fail "Expected object input"

-- | Like 'objectArgs', but a non-object input decodes as if it were the empty
-- object instead of failing. Use only for all-optional/defaulted records.
objectArgsLenient :: (Object -> Parser a) -> Value -> Parser a
objectArgsLenient f (Object obj) = f obj
objectArgsLenient f _            = f KeyMap.empty

-- | 'objectArgs' that also rejects any key outside @allowed@. Use for
-- protocol-style argument records where an unexpected field indicates a
-- misunderstanding rather than harmless extra data.
objectArgsExact :: [Text] -> (Object -> Parser a) -> Value -> Parser a
objectArgsExact allowed f = objectArgs \obj -> do
    rejectUnknownKeys allowed obj
    f obj

rejectUnknownKeys :: [Text] -> Object -> Parser ()
rejectUnknownKeys allowed obj =
    case filter (`notElem` allowed) present of
        [] -> pure ()
        unexpected ->
            failText $
                "Unexpected parameter"
                    <> (if length unexpected == 1 then ": " else "s: ")
                    <> Text.intercalate ", " unexpected
  where
    present = map Key.toText (KeyMap.keys obj)

-- | Required string. Mirrors 'extractText'.
reqText :: Object -> Text -> Parser Text
reqText obj key =
    case look obj key of
        Just (String t) -> pure t
        Just _ -> failText ("Expected string for key: " <> key)
        Nothing -> failText ("Missing parameter: " <> key)

-- | Required array-of-strings, tolerating a single bare string.
reqTextList :: Object -> Text -> Parser [Text]
reqTextList obj key =
    case look obj key of
        Just (Array arr) -> mapM one (Vector.toList arr)
          where
            one (String t) = pure t
            one _ = failText ("Expected string entries in array for key: " <> key)
        Just (String t) -> pure [t]
        Just _ -> failText ("Expected array for key: " <> key)
        Nothing -> failText ("Missing parameter: " <> key)

-- | Required exact, bounded integer.
reqInt :: Object -> Text -> Parser Int
reqInt obj key =
    case look obj key of
        Just (Number n) -> parseExactInt key n
        Just _ -> expectedInteger key
        Nothing -> failText ("Missing parameter: " <> key)

-- | Optional string; an empty string counts as absent.
optText :: Object -> Text -> Parser (Maybe Text)
optText obj key = pure $ case look obj key of
    Just (String t) | not (Text.null t) -> Just t
    _ -> Nothing

-- | Optional exact, bounded integer. An absent or null field is 'Nothing';
-- fractional, out-of-range, and wrongly typed values fail.
optInt :: Object -> Text -> Parser (Maybe Int)
optInt obj key = case look obj key of
    Nothing -> pure Nothing
    Just Null -> pure Nothing
    Just (Number n) -> Just <$> parseExactInt key n
    Just _ -> expectedInteger key

-- | 'optInt' plus compatibility for integer strings emitted by some
-- model tool surfaces. Strings are still required to be exact and in range.
optIntOrString :: Object -> Text -> Parser (Maybe Int)
optIntOrString obj key = case look obj key of
    Nothing -> pure Nothing
    Just Null -> pure Nothing
    Just (Number n) -> Just <$> parseExactInt key n
    Just (String value) ->
        maybe (expectedInteger key) (pure . Just) (readExactInt value)
    Just _ -> expectedInteger key

-- | Read a signed decimal 'Int' without rounding or overflow. Leading and
-- trailing whitespace is ignored; non-decimal syntax and trailing input fail.
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
optBool :: Object -> Text -> Parser (Maybe Bool)
optBool obj key = pure (look obj key >>= boolValue)

-- | Optional boolean that accepts only a real JSON boolean. Protocol-style
-- arguments should fail on stringy forms instead of guessing.
optBoolStrict :: Object -> Text -> Parser (Maybe Bool)
optBoolStrict obj key = case look obj key of
    Nothing -> pure Nothing
    Just Null -> pure Nothing
    Just (Bool value) -> pure (Just value)
    Just _ -> failText ("Expected boolean for key: " <> key)

-- | Exact, bounded integer with a default for an absent or null field.
intOr :: Object -> Text -> Int -> Parser Int
intOr obj key def = case look obj key of
    Nothing -> pure def
    Just Null -> pure def
    Just (Number n) -> parseExactInt key n
    Just _ -> expectedInteger key

-- | Optional array field decoded element-wise via 'FromJSON'. Absent key
-- means 'Nothing'; present-but-not-an-array fails with the supplied message.
optList :: FromJSON a => Object -> Text -> Text -> Parser (Maybe [a])
optList obj key notArrayMsg = case look obj key of
    Nothing -> pure Nothing
    Just (Array arr) -> Just <$> mapM parseJSON (Vector.toList arr)
    Just _ -> failText notArrayMsg

-- | Strip aeson's @parseEither@ error wrapper while preserving the parser's
-- own model-facing message.
stripAesonPrefix :: Text -> Text
stripAesonPrefix t
    | "Error in $" `Text.isPrefixOf` t =
        case Text.breakOn ": " t of
            (_, rest) | not (Text.null rest) -> Text.drop 2 rest
            _ -> t
    | otherwise = t

failText :: Text -> Parser a
failText = fail . Text.unpack

exactInt :: Scientific -> Maybe Int
exactInt = toBoundedInteger

boolValue :: Value -> Maybe Bool
boolValue = \case
    Bool value -> Just value
    String value -> case Text.toLower (Text.strip value) of
        "true" -> Just True
        "false" -> Just False
        "1" -> Just True
        "0" -> Just False
        _ -> Nothing
    _ -> Nothing

parseExactInt :: Text -> Scientific -> Parser Int
parseExactInt key =
    maybe (expectedInteger key) pure . exactInt

expectedInteger :: Text -> Parser a
expectedInteger key =
    failText ("Expected integer for key: " <> key)
