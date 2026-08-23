module Agent.ToolDSLSpec (spec) where

import System.OsPath (unsafeEncodeUtf)
import Agent.ToolDSL
import Agent.Tools.Grok.Prompt
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Time.Calendar (fromGregorian)
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

fromFilePath = unsafeEncodeUtf

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
            let prompt = grokSystemPrompt codingGrokPromptTools (fromFilePath "/tmp/repo")
                    (fromGregorian 2026 8 20) False
            prompt `shouldSatisfy` Text.isInfixOf "read_file"
            prompt `shouldSatisfy` Text.isInfixOf "search_replace"
            prompt `shouldSatisfy` Text.isInfixOf "run_terminal_command"
            prompt `shouldSatisfy` Text.isInfixOf "get_command_or_subagent_output"
            prompt `shouldSatisfy` Text.isInfixOf "kill_command_or_subagent"
            prompt `shouldSatisfy` Text.isInfixOf "spawn_subagent"
            prompt `shouldSatisfy` Text.isInfixOf
                "make the `spawn_subagent` calls near the start"
            prompt `shouldSatisfy` Text.isInfixOf "web_search"
            prompt `shouldSatisfy` Text.isInfixOf "<tool_calling>"
            prompt `shouldSatisfy` Text.isInfixOf "<work_policy>"
            prompt `shouldSatisfy` Text.isInfixOf "<background_tasks>"
            prompt `shouldNotSatisfy` Text.isInfixOf "shell_command"
            prompt `shouldNotSatisfy` Text.isInfixOf "apply_patch"
            prompt `shouldSatisfy` Text.isInfixOf "/tmp/repo"
            prompt `shouldSatisfy` Text.isInfixOf "2026-08-20"

        it "uses the autonomous identity for one-shot sessions" do
            let prompt = grokSystemPrompt codingGrokPromptTools (fromFilePath "/tmp/repo")
                    (fromGregorian 2026 8 20) True
            prompt `shouldSatisfy` Text.isInfixOf "no human operator"

        it "omits sections and names for tools unavailable to a child" do
            let prompt =
                    grokSystemPromptForTools
                        codingGrokPromptTools
                        ["read_file", "list_dir", "grep", "web_search"]
                        (fromFilePath "/tmp/repo")
                        (fromGregorian 2026 8 20)
                        True
            prompt `shouldSatisfy` Text.isInfixOf "read_file"
            prompt `shouldSatisfy` Text.isInfixOf "web_search"
            prompt `shouldNotSatisfy` Text.isInfixOf "search_replace"
            prompt `shouldNotSatisfy` Text.isInfixOf "run_terminal_command"
            prompt `shouldNotSatisfy` Text.isInfixOf "get_command_or_subagent_output"
            prompt `shouldNotSatisfy` Text.isInfixOf "spawn_subagent"
            prompt `shouldNotSatisfy` Text.isInfixOf
                "When the user explicitly asks you to use subagents"
            prompt `shouldNotSatisfy` Text.isInfixOf "<background_tasks>"
            prompt `shouldNotSatisfy` Text.isInfixOf "<plan_mode>"

    describe "grokSubagentSystemPrompt" do
        it "renders the dedicated Grok child contract" do
            let prompt =
                    grokSubagentSystemPrompt
                        codingGrokPromptTools
                        [ "read_file"
                        , "search_replace"
                        , "run_terminal_cmd"
                        , "get_task_output"
                        ]
                        (fromFilePath "/tmp/repo")
                        (fromGregorian 2026 8 20)
                        "darwin"
                        "/bin/zsh"
                        "general-purpose"
                        "agent-123"
            prompt `shouldSatisfy` Text.isPrefixOf "You are a Grok Build subagent"
            prompt `shouldSatisfy` Text.isInfixOf "Do not reproduce"
            prompt `shouldSatisfy` Text.isInfixOf "Parallelize independent tool calls"
            prompt `shouldSatisfy` Text.isInfixOf "<system-reminder>"
            prompt `shouldSatisfy` Text.isInfixOf "<making_code_changes>"
            prompt `shouldSatisfy` Text.isInfixOf "LINE_NUMBER\8594LINE_CONTENT"
            prompt `shouldSatisfy` Text.isInfixOf "<project_instructions_spec>"
            prompt `shouldSatisfy` Text.isInfixOf "Workspace Path: /tmp/repo"
            prompt `shouldSatisfy` Text.isInfixOf "Shell: /bin/zsh"
            prompt `shouldSatisfy` Text.isInfixOf "Agent id: agent-123"
            prompt `shouldSatisfy` Text.isInfixOf "run_terminal_command"
            prompt `shouldSatisfy` Text.isInfixOf "get_command_or_subagent_output"
            prompt `shouldNotSatisfy` Text.isInfixOf
                "Your main goal is to complete the user's request"

        it "omits edit and background guidance for explore children" do
            let prompt =
                    grokSubagentSystemPrompt
                        codingGrokPromptTools
                        ["read_file", "list_dir", "grep"]
                        (fromFilePath "/tmp/repo")
                        (fromGregorian 2026 8 20)
                        "linux"
                        "/bin/bash"
                        "explore"
                        "agent-456"
            prompt `shouldSatisfy` Text.isInfixOf "=== READ-ONLY MODE ==="
            prompt `shouldNotSatisfy` Text.isInfixOf "<making_code_changes>"
            prompt `shouldNotSatisfy` Text.isInfixOf "<background_tasks>"
            prompt `shouldNotSatisfy` Text.isInfixOf "search_replace"

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
