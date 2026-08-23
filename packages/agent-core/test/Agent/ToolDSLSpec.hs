module Agent.ToolDSLSpec (spec) where

import Agent.ToolDSL
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import Test.Hspec

spec :: Spec
spec = do
    describe "parametersObjectLoose" do
        it "lists only application-required properties in required" do
            let schema = parametersObjectLoose
                    [ PropertySchema "target_file" PropertyString True Nothing
                    , PropertySchema "offset" PropertyInteger False Nothing
                    ]
            object <- expectObject schema
            KeyMap.lookup "required" object
                `shouldBe` Just (Aeson.toJSON (["target_file"] :: [Text]))
            propertyType "offset" object `shouldBe` Just (Aeson.String "integer")

        it "does not wrap optional fields as nullable" do
            let strict = parametersObject
                    [ PropertySchema "offset" PropertyInteger False Nothing ]
                loose = parametersObjectLoose
                    [ PropertySchema "offset" PropertyInteger False Nothing ]
            propertyTypeOf strict "offset"
                `shouldBe` Just (Aeson.toJSON (["integer", "null"] :: [Text]))
            propertyTypeOf loose "offset"
                `shouldBe` Just (Aeson.String "integer")

propertyType :: Text -> Aeson.Object -> Maybe Aeson.Value
propertyType name object = do
    Aeson.Object properties <- KeyMap.lookup "properties" object
    Aeson.Object field <- KeyMap.lookup (Key.fromText name) properties
    KeyMap.lookup "type" field

propertyTypeOf :: Aeson.Value -> Text -> Maybe Aeson.Value
propertyTypeOf value name = case value of
    Aeson.Object object -> propertyType name object
    _ -> Nothing

expectObject :: Aeson.Value -> IO Aeson.Object
expectObject = \case
    Aeson.Object object -> pure object
    other -> expectationFailure ("expected object, got " <> show other) >> fail "unreachable"
