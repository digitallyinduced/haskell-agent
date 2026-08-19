module Agent.OpenAI.ToolDSLSpec (spec) where

import Agent.OpenAI.ToolDSL
import Agent.OpenAI.Responses.Types (FunctionTool(..), ResponseTool(..))
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import Test.Hspec

spec :: Spec
spec = do
    describe "buildTool" $ do
        it "emits a function tool with the given name and description" $ do
            let tool = buildTool "search" "Find things." []
            case tool of
                FunctionToolValue FunctionTool { name, description } -> do
                    name        `shouldBe` "search"
                    description `shouldBe` Just "Find things."
                _ -> expectationFailure "expected FunctionToolValue"

        it "lists every property in required and makes application-optional fields nullable" $ do
            let tool = buildTool "t" "d"
                    [ PropertySchema { propertyName = "a", propertyType = PropertyString
                                     , required = True,  description = Nothing }
                    , PropertySchema { propertyName = "b", propertyType = PropertyInteger
                                     , required = False, description = Nothing }
                    , PropertySchema { propertyName = "c", propertyType = PropertyString
                                     , required = True,  description = Nothing }
                    ]
            required_ tool `shouldBe` Just (Aeson.toJSON (["a", "b", "c"] :: [Text]))
            propertyType "a" tool `shouldBe` Just (Aeson.String "string")
            propertyType "b" tool `shouldBe`
                Just (Aeson.toJSON (["integer", "null"] :: [Text]))
            additionalProperties_ tool `shouldBe` Just (Aeson.Bool False)

        it "merges description into each property's schema object" $ do
            let tool = buildTool "t" "d"
                    [ PropertySchema
                        { propertyName = "q"
                        , propertyType = PropertyString
                        , required     = True
                        , description  = Just "A search query"
                        }
                    ]
            case propertyField "q" tool of
                Just (Aeson.Object o) -> do
                    KeyMap.lookup "type" o        `shouldBe` Just (Aeson.String "string")
                    KeyMap.lookup "description" o `shouldBe` Just (Aeson.String "A search query")
                other -> expectationFailure ("expected property object, got " <> show other)

        it "encodes each primitive PropertyType with its JSON Schema type" $ do
            typeOf PropertyString  `shouldBe` Just (Aeson.String "string")
            typeOf PropertyInteger `shouldBe` Just (Aeson.String "integer")
            typeOf PropertyNumber  `shouldBe` Just (Aeson.String "number")
            typeOf PropertyBoolean `shouldBe` Just (Aeson.String "boolean")

        it "encodes PropertyArray with an items sub-schema" $ do
            let tool = buildTool "t" "d"
                    [ PropertySchema
                        { propertyName = "tags"
                        , propertyType = PropertyArray PropertyString
                        , required     = True
                        , description  = Nothing
                        }
                    ]
            case propertyField "tags" tool of
                Just (Aeson.Object o) -> do
                    KeyMap.lookup "type" o `shouldBe` Just (Aeson.String "array")
                    case KeyMap.lookup "items" o of
                        Just (Aeson.Object items) ->
                            KeyMap.lookup "type" items `shouldBe` Just (Aeson.String "string")
                        other -> expectationFailure
                            ("expected items object, got " <> show other)
                other -> expectationFailure ("unexpected property value: " <> show other)

        it "encodes PropertyEnum as string type with enum values" $ do
            let tool = buildTool "t" "d"
                    [ PropertySchema
                        { propertyName = "status"
                        , propertyType = PropertyEnum ["open", "closed"]
                        , required     = True
                        , description  = Nothing
                        }
                    ]
            case propertyField "status" tool of
                Just (Aeson.Object o) -> do
                    KeyMap.lookup "type" o `shouldBe` Just (Aeson.String "string")
                    KeyMap.lookup "enum" o `shouldBe` Just
                        (Aeson.toJSON (["open", "closed"] :: [Text]))
                other -> expectationFailure ("unexpected property value: " <> show other)

        it "recursively renders optional object fields as required-but-nullable" $ do
            let tool = buildTool "t" "d"
                    [ PropertySchema
                        { propertyName = "filter"
                        , propertyType = PropertyObject
                            [ PropertySchema "key"   PropertyString  True  Nothing
                            , PropertySchema "value" PropertyInteger False Nothing
                            ]
                        , required     = True
                        , description  = Nothing
                        }
                    ]
            case propertyField "filter" tool of
                Just (Aeson.Object o) -> do
                    KeyMap.lookup "type" o `shouldBe` Just (Aeson.String "object")
                    KeyMap.lookup "required" o `shouldBe`
                        Just (Aeson.toJSON (["key", "value"] :: [Text]))
                    nestedPropertyType "key" o `shouldBe` Just (Aeson.String "string")
                    nestedPropertyType "value" o `shouldBe`
                        Just (Aeson.toJSON (["integer", "null"] :: [Text]))
                    KeyMap.lookup "additionalProperties" o `shouldBe` Just (Aeson.Bool False)
                other -> expectationFailure ("unexpected filter property: " <> show other)

        it "makes optional arrays and their nested optional object fields nullable" $ do
            let tool = buildTool "t" "d"
                    [ PropertySchema
                        { propertyName = "entries"
                        , propertyType = PropertyArray (PropertyObject
                            [ PropertySchema "id" PropertyString True Nothing
                            , PropertySchema "note" PropertyString False Nothing
                            ])
                        , required = False
                        , description = Nothing
                        }
                    ]
            case propertyField "entries" tool of
                Just (Aeson.Object entries) -> do
                    KeyMap.lookup "type" entries `shouldBe`
                        Just (Aeson.toJSON (["array", "null"] :: [Text]))
                    case KeyMap.lookup "items" entries of
                        Just (Aeson.Object items) -> do
                            KeyMap.lookup "required" items `shouldBe`
                                Just (Aeson.toJSON (["id", "note"] :: [Text]))
                            nestedPropertyType "id" items `shouldBe` Just (Aeson.String "string")
                            nestedPropertyType "note" items `shouldBe`
                                Just (Aeson.toJSON (["string", "null"] :: [Text]))
                        other -> expectationFailure ("expected object items, got " <> show other)
                other -> expectationFailure ("expected entries property, got " <> show other)

        it "adds null to optional enum values" $ do
            let tool = buildTool "t" "d"
                    [ PropertySchema "status" (PropertyEnum ["open", "closed"]) False Nothing ]
            case propertyField "status" tool of
                Just (Aeson.Object status) -> do
                    KeyMap.lookup "type" status `shouldBe`
                        Just (Aeson.toJSON (["string", "null"] :: [Text]))
                    KeyMap.lookup "enum" status `shouldBe`
                        Just (Aeson.toJSON ([Just "open", Just "closed", Nothing] :: [Maybe Text]))
                other -> expectationFailure ("expected status property, got " <> show other)

    describe "buildGrokTool" $ do
        it "omits strict and keeps optional fields optional" $ do
            let tool = buildGrokTool "read_file" "Read a file."
                    [ PropertySchema "target_file" PropertyString True Nothing
                    , PropertySchema "offset" PropertyInteger False Nothing
                    ]
            case tool of
                FunctionToolValue fn -> do
                    fn.strict `shouldBe` Nothing
                    required_ tool `shouldBe` Just (Aeson.toJSON (["target_file"] :: [Text]))
                    propertyType "offset" tool `shouldBe` Just (Aeson.String "integer")
                other -> expectationFailure ("expected FunctionToolValue, got " <> show other)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Extract the parameters.properties.<name> schema from a built tool.
propertyField :: Text -> ResponseTool -> Maybe Aeson.Value
propertyField name (FunctionToolValue FunctionTool { parameters = Just (Aeson.Object params) }) =
    case KeyMap.lookup "properties" params of
        Just (Aeson.Object props) -> KeyMap.lookup (Key.fromText name) props
        _                         -> Nothing
propertyField _ _ = Nothing

-- | Extract the parameters.required array.
required_ :: ResponseTool -> Maybe Aeson.Value
required_ (FunctionToolValue FunctionTool { parameters = Just (Aeson.Object params) }) =
    KeyMap.lookup "required" params
required_ _ = Nothing

additionalProperties_ :: ResponseTool -> Maybe Aeson.Value
additionalProperties_ (FunctionToolValue FunctionTool { parameters = Just (Aeson.Object params) }) =
    KeyMap.lookup "additionalProperties" params
additionalProperties_ _ = Nothing

propertyType :: Text -> ResponseTool -> Maybe Aeson.Value
propertyType name tool = do
    Aeson.Object property <- propertyField name tool
    KeyMap.lookup "type" property

nestedPropertyType :: Text -> Aeson.Object -> Maybe Aeson.Value
nestedPropertyType name schema = do
    Aeson.Object properties <- KeyMap.lookup "properties" schema
    Aeson.Object property <- KeyMap.lookup (Key.fromText name) properties
    KeyMap.lookup "type" property

-- | Render a single primitive 'PropertyType' as JSON Schema and pull its
-- @type@ field out. Used by the "primitive encoding" test.
typeOf :: PropertyType -> Maybe Aeson.Value
typeOf pt =
    case Aeson.toJSON pt of
        Aeson.Object o -> KeyMap.lookup "type" o
        _              -> Nothing

-- Silence unused-import style warnings from (.=)
_unused :: Aeson.Value
_unused = Aeson.object ["k" .= ("v" :: Text)]
