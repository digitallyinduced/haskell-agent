module Agent.ToolArgs
    ( extractText
    , extractTextList
    , extractTextOr
    , extractIntOr
    , extractMaybeText
    , extractMaybeInt
    , extractMaybePositiveInt
    , extractMaybeBool
    , objectArgs
    , objectArgsLenient
    , rawValue
    , reqText
    , reqDouble
    , reqTextList
    , rawOptText
    , optText
    , optInt
    , optPosInt
    , optBool
    , intOr
    , textOr
    , optList
    , stripAesonPrefix
    ) where

import Data.Aeson (FromJSON(..), Object, Value(..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as Text
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
        Just (Number n) -> round n
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
        Just (Number n) -> Just (round n)
        _ -> Nothing
extractMaybeInt _ _ = Nothing

-- | Like 'extractMaybeInt' but treats a non-positive number as "not provided".
extractMaybePositiveInt :: Value -> Text -> Maybe Int
extractMaybePositiveInt v key = extractMaybeInt v key >>= \n -> if n > 0 then Just n else Nothing

-- | Accept a real JSON boolean as well as the stringy forms LLMs sometimes
-- emit ("true"/"false"/"1"/"0", case-insensitive, whitespace-tolerant).
extractMaybeBool :: Value -> Text -> Maybe Bool
extractMaybeBool (Object obj) key =
    case KeyMap.lookup (Key.fromText key) obj of
        Just (Bool b) -> Just b
        Just (String t) ->
            case Text.toLower (Text.strip t) of
                "true"  -> Just True
                "false" -> Just False
                "1"     -> Just True
                "0"     -> Just False
                _       -> Nothing
        _ -> Nothing
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

-- | The raw JSON value under a key, untouched.
rawValue :: Object -> Text -> Parser (Maybe Value)
rawValue obj key = pure (look obj key)

-- | Required string. Mirrors 'extractText'.
reqText :: Object -> Text -> Parser Text
reqText obj key =
    case look obj key of
        Just (String t) -> pure t
        Just _ -> failText ("Expected string for key: " <> key)
        Nothing -> failText ("Missing parameter: " <> key)

-- | Required JSON number as a 'Double'.
reqDouble :: Object -> Text -> Parser Double
reqDouble obj key =
    case look obj key of
        Just (Number n) -> pure (realToFrac n)
        Just _ -> failText ("Expected number for key: " <> key)
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

-- | Optional string, returned verbatim when present (even @""@).
rawOptText :: Object -> Text -> Parser (Maybe Text)
rawOptText obj key = pure $ case look obj key of
    Just (String t) -> Just t
    _ -> Nothing

-- | Optional string; an empty string counts as absent.
optText :: Object -> Text -> Parser (Maybe Text)
optText obj key = pure $ case look obj key of
    Just (String t) | not (Text.null t) -> Just t
    _ -> Nothing

-- | Optional integer (JSON number, rounded).
optInt :: Object -> Text -> Parser (Maybe Int)
optInt obj key = pure $ case look obj key of
    Just (Number n) -> Just (round n)
    _ -> Nothing

-- | Optional integer where a non-positive value counts as absent.
optPosInt :: Object -> Text -> Parser (Maybe Int)
optPosInt obj key = do
    mb <- optInt obj key
    pure (mb >>= \n -> if n > 0 then Just n else Nothing)

-- | Optional boolean accepting stringy forms.
optBool :: Object -> Text -> Parser (Maybe Bool)
optBool obj key = pure $ case look obj key of
    Just (Bool b) -> Just b
    Just (String t) ->
        case Text.toLower (Text.strip t) of
            "true"  -> Just True
            "false" -> Just False
            "1"     -> Just True
            "0"     -> Just False
            _       -> Nothing
    _ -> Nothing

-- | Integer with a default.
intOr :: Object -> Text -> Int -> Parser Int
intOr obj key def = pure $ case look obj key of
    Just (Number n) -> round n
    _ -> def

-- | String with a default.
textOr :: Object -> Text -> Text -> Parser Text
textOr obj key def = pure $ case look obj key of
    Just (String t) -> t
    _ -> def

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
