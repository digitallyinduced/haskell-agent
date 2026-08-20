module Agent.ToolDSLSpec (spec) where

import Agent.ToolDSL
import Agent.Tools.Grok.Prompt
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Time.Calendar (fromGregorian)
import Data.Text (Text)
import qualified Data.Text as Text
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

    describe "grokSystemPrompt" do
        it "names grok-build tools including background task helpers" do
            let prompt = grokSystemPrompt codingGrokPromptTools "/tmp/repo"
                    (fromGregorian 2026 8 20) False
            prompt `shouldSatisfy` Text.isInfixOf "read_file"
            prompt `shouldSatisfy` Text.isInfixOf "search_replace"
            prompt `shouldSatisfy` Text.isInfixOf "run_terminal_cmd"
            prompt `shouldSatisfy` Text.isInfixOf "get_task_output"
            prompt `shouldSatisfy` Text.isInfixOf "kill_task"
            prompt `shouldSatisfy` Text.isInfixOf "<tool_calling>"
            prompt `shouldSatisfy` Text.isInfixOf "<work_policy>"
            prompt `shouldSatisfy` Text.isInfixOf "<background_tasks>"
            prompt `shouldNotSatisfy` Text.isInfixOf "shell_command"
            prompt `shouldNotSatisfy` Text.isInfixOf "apply_patch"
            prompt `shouldNotSatisfy` Text.isInfixOf "run_terminal_command"
            prompt `shouldSatisfy` Text.isInfixOf "/tmp/repo"
            prompt `shouldSatisfy` Text.isInfixOf "2026-08-20"

        it "uses the autonomous identity for one-shot sessions" do
            let prompt = grokSystemPrompt codingGrokPromptTools "/tmp/repo"
                    (fromGregorian 2026 8 20) True
            prompt `shouldSatisfy` Text.isInfixOf "no human operator"

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
