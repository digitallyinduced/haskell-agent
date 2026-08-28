-- | Form-mode elicitation schemas.
--
-- Servers describe the input they need with a restricted JSON Schema: a flat
-- object whose properties are strings, numbers, booleans, or (multi-)select
-- enums. Hosts render those fields, validate the answers, and return the
-- resulting object.
module Agent.MCP.Elicitation
    ( McpFormField(..)
    , McpFieldKind(..)
    , McpEnumOption(..)
    , parseElicitForm
    , parseFieldAnswer
    , encodeFormContent
    , describeFieldKind
    , fieldDefaultText
    ) where

import Agent.Json (RawJson, rawJsonBytes, rawJsonFromEncoding)
import Data.Aeson (Value(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Char (isSpace)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Scientific (Scientific, floatingOrInteger, fromFloatDigits)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as TextRead
import qualified Data.Vector as Vector

data McpEnumOption = McpEnumOption
    { optionValue :: !Text
    , optionTitle :: !Text
    } deriving (Eq, Show)

data McpFieldKind
    = McpStringField
        { stringFormat :: !(Maybe Text)
        , stringMinLength :: !(Maybe Int)
        , stringMaxLength :: !(Maybe Int)
        }
    | McpNumberField
        { numberInteger :: !Bool
        , numberMinimum :: !(Maybe Scientific)
        , numberMaximum :: !(Maybe Scientific)
        }
    | McpBooleanField
    | McpEnumField
        { enumOptions :: ![McpEnumOption]
        , enumMultiple :: !Bool
        , enumMinItems :: !(Maybe Int)
        , enumMaxItems :: !(Maybe Int)
        }
    deriving (Eq, Show)

data McpFormField = McpFormField
    { fieldName :: !Text
    , fieldTitle :: !(Maybe Text)
    , fieldDescription :: !(Maybe Text)
    , fieldRequired :: !Bool
    , fieldDefault :: !(Maybe Value)
    , fieldKind :: !McpFieldKind
    } deriving (Eq, Show)

-- | Interpret a @requestedSchema@. Unsupported constructs are rejected so
-- the host never renders a form it cannot validate.
parseElicitForm :: RawJson -> Either Text [McpFormField]
parseElicitForm raw =
    case Aeson.decodeStrict (rawJsonBytes raw) of
        Nothing -> Left "requestedSchema is not valid JSON"
        Just (Object root) -> do
            let required =
                    case KeyMap.lookup "required" root of
                        Just (Array values) ->
                            mapMaybe asText (Vector.toList values)
                        _ -> []
            properties <- case KeyMap.lookup "properties" root of
                Just (Object fields) -> Right (KeyMap.toList fields)
                Nothing -> Right []
                Just _ -> Left "requestedSchema.properties must be an object"
            mapM (parseField required) properties
        Just _ -> Left "requestedSchema must be an object schema"
  where
    asText = \case
        String text -> Just text
        _ -> Nothing

parseField :: [Text] -> (Key.Key, Value) -> Either Text McpFormField
parseField required (key, schema) = case schema of
    Object fields -> do
        let name = Key.toText key
            lookupText field = case KeyMap.lookup field fields of
                Just (String text) -> Just text
                _ -> Nothing
            lookupInt field = case KeyMap.lookup field fields of
                Just (Number number) ->
                    either (const Nothing) Just (floatingOrInteger @Double number)
                _ -> Nothing
            lookupNumber field = case KeyMap.lookup field fields of
                Just (Number number) -> Just number
                _ -> Nothing
            kindName = fromMaybe "" (lookupText "type")
        kind <- case kindName of
            "string"
                | Just (Array values) <- KeyMap.lookup "enum" fields ->
                    Right McpEnumField
                        { enumOptions =
                            [ McpEnumOption value value
                            | String value <- Vector.toList values
                            ]
                        , enumMultiple = False
                        , enumMinItems = Nothing
                        , enumMaxItems = Nothing
                        }
                | Just (Array values) <- KeyMap.lookup "oneOf" fields ->
                    Right McpEnumField
                        { enumOptions = mapMaybe titledOption (Vector.toList values)
                        , enumMultiple = False
                        , enumMinItems = Nothing
                        , enumMaxItems = Nothing
                        }
                | otherwise ->
                    Right McpStringField
                        { stringFormat = lookupText "format"
                        , stringMinLength = lookupInt "minLength"
                        , stringMaxLength = lookupInt "maxLength"
                        }
            "number" -> Right (numberField False lookupNumber)
            "integer" -> Right (numberField True lookupNumber)
            "boolean" -> Right McpBooleanField
            "array" -> case KeyMap.lookup "items" fields of
                Just (Object items)
                    | Just (Array values) <- KeyMap.lookup "enum" items ->
                        Right McpEnumField
                            { enumOptions =
                                [ McpEnumOption value value
                                | String value <- Vector.toList values
                                ]
                            , enumMultiple = True
                            , enumMinItems = lookupInt "minItems"
                            , enumMaxItems = lookupInt "maxItems"
                            }
                    | Just (Array values) <- KeyMap.lookup "anyOf" items ->
                        Right McpEnumField
                            { enumOptions = mapMaybe titledOption (Vector.toList values)
                            , enumMultiple = True
                            , enumMinItems = lookupInt "minItems"
                            , enumMaxItems = lookupInt "maxItems"
                            }
                _ -> Left ("field " <> name <> " uses an unsupported array schema")
            other ->
                Left ("field " <> name <> " has unsupported type \"" <> other <> "\"")
        Right McpFormField
            { fieldName = name
            , fieldTitle = lookupText "title"
            , fieldDescription = lookupText "description"
            , fieldRequired = name `elem` required
            , fieldDefault = KeyMap.lookup "default" fields
            , fieldKind = kind
            }
    _ -> Left ("field " <> Key.toText key <> " is not an object schema")
  where
    numberField integer lookupNumber = McpNumberField
        { numberInteger = integer
        , numberMinimum = lookupNumber "minimum"
        , numberMaximum = lookupNumber "maximum"
        }
    titledOption = \case
        Object option
            | Just (String value) <- KeyMap.lookup "const" option ->
                Just McpEnumOption
                    { optionValue = value
                    , optionTitle = case KeyMap.lookup "title" option of
                        Just (String title) -> title
                        _ -> value
                    }
        _ -> Nothing

-- | Human-readable description of what a field accepts.
describeFieldKind :: McpFormField -> Text
describeFieldKind field = case field.fieldKind of
    McpStringField format minLength maxLength ->
        Text.intercalate ", " $
            ("text" <> maybe "" (\value -> " (" <> value <> ")") format)
                : [ "at least " <> plural n "character" | Just n <- [minLength] ]
                <> [ "at most " <> plural n "character" | Just n <- [maxLength] ]
    McpNumberField integer minimum maximum ->
        Text.intercalate ", " $
            (if integer then "integer" else "number")
                : [ "minimum " <> showScientific n | Just n <- [minimum] ]
                <> [ "maximum " <> showScientific n | Just n <- [maximum] ]
    McpBooleanField -> "yes or no"
    McpEnumField options multiple minItems maxItems ->
        (if multiple then "comma-separated choices from: " else "one of: ")
            <> Text.intercalate ", "
                [ if option.optionTitle == option.optionValue
                    then option.optionValue
                    else option.optionTitle <> " (" <> option.optionValue <> ")"
                | option <- options
                ]
            <> mconcat [ "; at least " <> Text.pack (show n) | Just n <- [minItems] ]
            <> mconcat [ "; at most " <> Text.pack (show n) | Just n <- [maxItems] ]

-- | The field's default rendered the way a user would type it.
fieldDefaultText :: McpFormField -> Maybe Text
fieldDefaultText field = field.fieldDefault >>= \case
    String text -> Just text
    Number number -> Just (showScientific number)
    Bool flag -> Just (if flag then "yes" else "no")
    Array values ->
        Just (Text.intercalate ", " [ value | String value <- Vector.toList values ])
    _ -> Nothing

-- | Validate one typed answer. An empty answer resolves to the default when
-- there is one and to 'Nothing' when the field is optional.
parseFieldAnswer :: McpFormField -> Text -> Either Text (Maybe Value)
parseFieldAnswer field answer
    | Text.null trimmed =
        case field.fieldDefault of
            Just value -> Right (Just value)
            Nothing
                | field.fieldRequired -> Left (label <> " is required")
                | otherwise -> Right Nothing
    | otherwise = Just <$> parseValue
  where
    trimmed = Text.strip answer
    label = fromMaybe field.fieldName field.fieldTitle
    parseValue = case field.fieldKind of
        McpStringField format minLength maxLength -> do
            checkLength minLength maxLength
            checkFormat format
            Right (String trimmed)
        McpNumberField integer minimum maximum ->
            case TextRead.signed TextRead.rational trimmed of
                Right (value, rest) | Text.null rest -> do
                    let number = fromFloatDigits (value :: Double)
                    if integer && not (isInteger number)
                        then Left (label <> " must be an integer")
                        else do
                            mapM_ (\bound -> if number < bound then Left (label <> " must be at least " <> showScientific bound) else Right ()) minimum
                            mapM_ (\bound -> if number > bound then Left (label <> " must be at most " <> showScientific bound) else Right ()) maximum
                            Right (Number number)
                _ -> Left (label <> " must be a number")
        McpBooleanField ->
            case Text.toLower trimmed of
                value | value `elem` ["y", "yes", "true", "1", "on"] -> Right (Bool True)
                      | value `elem` ["n", "no", "false", "0", "off"] -> Right (Bool False)
                _ -> Left (label <> " must be yes or no")
        McpEnumField options multiple minItems maxItems
            | multiple -> do
                let parts = filter (not . Text.null) (map Text.strip (Text.splitOn "," trimmed))
                values <- mapM (resolveOption options) parts
                mapM_ (\n -> if length values < n then Left (label <> " needs at least " <> Text.pack (show n) <> " choices") else Right ()) minItems
                mapM_ (\n -> if length values > n then Left (label <> " allows at most " <> Text.pack (show n) <> " choices") else Right ()) maxItems
                Right (Array (Vector.fromList (map String values)))
            | otherwise -> String <$> resolveOption options trimmed
    checkLength minLength maxLength = do
        mapM_ (\n -> if Text.length trimmed < n then Left (label <> " must have at least " <> Text.pack (show n) <> " characters") else Right ()) minLength
        mapM_ (\n -> if Text.length trimmed > n then Left (label <> " must have at most " <> Text.pack (show n) <> " characters") else Right ()) maxLength
    checkFormat = \case
        Just "email"
            | not ("@" `Text.isInfixOf` trimmed) || Text.any isSpace trimmed ->
                Left (label <> " must be an email address")
        Just "uri"
            | not ("://" `Text.isInfixOf` trimmed) || Text.any isSpace trimmed ->
                Left (label <> " must be a URI")
        Just "date"
            | not (isDate trimmed) -> Left (label <> " must be a date (YYYY-MM-DD)")
        Just "date-time"
            | not (isDate (Text.take 10 trimmed) && Text.length trimmed > 10) ->
                Left (label <> " must be an ISO 8601 date-time")
        _ -> Right ()
    isDate text =
        Text.length text == 10
            && Text.all (\character -> character == '-' || character `elem` ['0' .. '9']) text
            && Text.index text 4 == '-'
            && Text.index text 7 == '-'
    resolveOption :: [McpEnumOption] -> Text -> Either Text Text
    resolveOption options input =
        let lowered = Text.toLower input
            matches :: McpEnumOption -> Bool
            matches option =
                Text.toLower option.optionValue == lowered
                    || Text.toLower option.optionTitle == lowered
            byIndex = case TextRead.decimal input of
                Right (index, rest)
                    | Text.null rest, index >= 1, index <= length options ->
                        Just (options !! (index - 1))
                _ -> Nothing
        in case filter matches options of
            option : _ -> Right option.optionValue
            [] -> case byIndex of
                Just option -> Right option.optionValue
                Nothing -> Left (label <> ": \"" <> input <> "\" is not one of the choices")
    isInteger number = either (const False) (const True) (floatingOrInteger @Double @Integer number)

encodeFormContent :: [(Text, Value)] -> RawJson
encodeFormContent answers =
    rawJsonFromEncoding . Aeson.toEncoding $
        Object (KeyMap.fromList [ (Key.fromText name, value) | (name, value) <- answers ])

plural :: Int -> Text -> Text
plural count noun =
    Text.pack (show count) <> " " <> noun <> (if count == 1 then "" else "s")

showScientific :: Scientific -> Text
showScientific number = case floatingOrInteger @Double @Integer number of
    Right integer -> Text.pack (show integer)
    Left floating -> Text.pack (show floating)
