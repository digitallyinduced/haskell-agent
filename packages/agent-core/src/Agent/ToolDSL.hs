-- | Provider-neutral JSON Schema fragments for function-tool parameters.
--
-- 'buildTool' stays in @agent-openai@ because it produces a Responses
-- 'ResponseTool'. This module owns the schema AST so coding-tool packages
-- can declare parameters without depending on the OpenAI wire types.
module Agent.ToolDSL
    ( PropertySchema(..)
    , PropertyType(..)
    , parametersObject
    , propertyToValue
    ) where

import Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Vector as Vector

-- | Minimal JSON-schema type descriptor for function-tool parameters.
-- Use 'PropertyRaw' to inject a custom schema fragment.
data PropertyType
    = PropertyString
    | PropertyInteger
    | PropertyNumber
    | PropertyBoolean
    | PropertyArray !PropertyType
    | PropertyEnum ![Text]
    | PropertyObject ![PropertySchema]
    | PropertyRaw !Aeson.Value
    deriving (Eq, Show)

-- | One property in a function-tool's parameters schema. 'required' describes
-- application-level requiredness. Strict Structured Outputs schemas still list
-- every property in @"required"@; properties with @required = False@ are
-- rendered as nullable instead.
data PropertySchema = PropertySchema
    { propertyName :: !Text
    , propertyType :: !PropertyType
    , required     :: !Bool
    , description  :: !(Maybe Text)
    } deriving (Eq, Show)

-- | The JSON Schema object used as a function tool's @parameters@ field.
parametersObject :: [PropertySchema] -> Value
parametersObject properties = object
    [ "type" .= ("object" :: Text)
    , "properties" .= object
        [ Key.fromText p.propertyName .= propertyToValue p
        | p <- properties
        ]
    , "required" .= [ p.propertyName | p <- properties ]
    , "additionalProperties" .= False
    ]

propertyToValue :: PropertySchema -> Value
propertyToValue property =
    applyOptionality property.required
        (applyDescription property.description
            (typeSchema property.propertyType))

instance ToJSON PropertyType where
    toJSON = typeSchema

typeSchema :: PropertyType -> Value
typeSchema = \case
    PropertyString -> primitiveSchema "string"
    PropertyInteger -> primitiveSchema "integer"
    PropertyNumber -> primitiveSchema "number"
    PropertyBoolean -> primitiveSchema "boolean"
    PropertyArray inner -> object
        [ "type" .= ("array" :: Text)
        , "items" .= typeSchema inner
        ]
    PropertyEnum values -> object
        [ "type" .= ("string" :: Text)
        , "enum" .= values
        ]
    PropertyObject fields -> object
        [ "type" .= ("object" :: Text)
        , "properties" .= object
            [ Key.fromText field.propertyName .= propertyToValue field
            | field <- fields
            ]
        , "required" .= [ field.propertyName | field <- fields ]
        , "additionalProperties" .= False
        ]
    PropertyRaw value -> strictifyRawSchema value

primitiveSchema :: Text -> Value
primitiveSchema typeName = object ["type" .= typeName]

applyDescription :: Maybe Text -> Value -> Value
applyDescription Nothing value = value
applyDescription (Just description) (Object schema) =
    Object (KeyMap.insert "description" (String description) schema)
applyDescription _ value = value

applyOptionality :: Bool -> Value -> Value
applyOptionality True value = value
applyOptionality False value = makeNullable value

makeNullable :: Value -> Value
makeNullable (Object schema) =
    Object (nullableEnum (nullableType schema))
  where
    nullableType object =
        case KeyMap.lookup "type" object of
            Just (String typeName) ->
                KeyMap.insert "type" (toJSON [typeName, "null" :: Text]) object
            Just (Array types)
                | String "null" `notElem` Vector.toList types ->
                    KeyMap.insert "type" (Array (Vector.snoc types (String "null"))) object
            _ -> object
    nullableEnum object =
        case KeyMap.lookup "enum" object of
            Just (Array values)
                | Null `notElem` Vector.toList values ->
                    KeyMap.insert "enum" (Array (Vector.snoc values Null)) object
            _ -> object
makeNullable value = value

-- Raw schemas are supported for advanced callers and must obey the same
-- recursive strictness contract. Their pre-existing @required@ list carries
-- the application-level requiredness information.
strictifyRawSchema :: Value -> Value
strictifyRawSchema (Object schema) =
    case KeyMap.lookup "type" schema of
        Just (String "object") ->
            case KeyMap.lookup "properties" schema of
                Just (Object properties) ->
                    let originallyRequired =
                            case KeyMap.lookup "required" schema of
                                Just (Array names) ->
                                    [ name | String name <- Vector.toList names ]
                                _ -> []
                        strictProperties = KeyMap.mapWithKey
                            (\key value ->
                                applyOptionality
                                    (Key.toText key `elem` originallyRequired)
                                    (strictifyRawSchema value)
                            )
                            properties
                    in Object
                        (KeyMap.insert "additionalProperties" (Bool False)
                            (KeyMap.insert "required"
                                (toJSON (map Key.toText (KeyMap.keys properties)))
                                (KeyMap.insert "properties" (Object strictProperties) schema)))
                _ -> Object schema
        Just (String "array") ->
            Object (case KeyMap.lookup "items" schema of
                Just items -> KeyMap.insert "items" (strictifyRawSchema items) schema
                Nothing -> schema
            )
        _ -> Object schema
strictifyRawSchema value = value
