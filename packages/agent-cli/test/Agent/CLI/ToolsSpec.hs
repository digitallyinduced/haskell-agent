module Agent.CLI.ToolsSpec (spec) where

import Agent.CLI.Tools
import Agent.OpenAI.Responses.Types
import Agent.Provider (Provider(..))
import Agent.ToolDispatch (noArgsTool)
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.Tools.ApplyPatch (applyPatchGrammar)
import Agent.Tools.Types (AppTool(..), AppToolKind(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import Test.Hspec

spec :: Spec
spec = describe "schemasFromAppTools" do
    it "enables built-in web_search ahead of app tools" do
        case schemasFromAppTools OpenAIProvider [jsonTool] of
            KnownResponseTool ToolWebSearch tagged : _ -> do
                tagged.tag `shouldBe` "web_search"
                tagged.fields `shouldBe` KeyMap.empty
            other -> expectationFailure ("expected web_search first, got " <> show other)

    it "builds a strict function tool for OpenAI JSON tools" do
        case schemasFromAppTools OpenAIProvider [jsonTool] of
            [_, FunctionToolValue tool] -> do
                tool.name `shouldBe` "read_file"
                tool.strict `shouldBe` Just True
            other -> expectationFailure ("expected function tool, got " <> show other)

    it "builds a loose grok-build function tool for xAI" do
        case schemasFromAppTools XAIProvider [jsonTool] of
            [_, FunctionToolValue tool] -> do
                tool.name `shouldBe` "read_file"
                tool.strict `shouldBe` Nothing
                required_ tool `shouldBe` Just (Aeson.toJSON (["target_file"] :: [Text]))
                offsetType tool `shouldBe` Just (Aeson.String "integer")
            other -> expectationFailure ("expected function tool, got " <> show other)

    it "registers apply_patch as a custom Lark tool" do
        case schemasFromAppTools OpenAIProvider [patchTool] of
            [_, KnownResponseTool ToolCustom tagged] -> do
                tagged.tag `shouldBe` "custom"
                KeyMap.lookup "name" tagged.fields
                    `shouldBe` Just (Aeson.String "apply_patch")
                case KeyMap.lookup "format" tagged.fields of
                    Just (Aeson.Object format) -> do
                        KeyMap.lookup "syntax" format `shouldBe` Just (Aeson.String "lark")
                        KeyMap.lookup "definition" format
                            `shouldBe` Just (Aeson.String applyPatchGrammar)
                    other -> expectationFailure ("expected format object, got " <> show other)
            other -> expectationFailure ("expected custom tool, got " <> show other)

    it "emits collaboration as a Responses namespace tool" do
        let spawn = AppTool
                { appToolName = "spawn_agent"
                , appToolDescription = "Spawn."
                , appToolParameters =
                    [ PropertySchema "message" PropertyString False Nothing ]
                , appToolHandler = noArgsTool "spawn_agent" (pure (Right "ok"))
                , appToolKind = JsonFunction
                , appToolReadOnly = False
                , appToolIsReadOnlyCall = Nothing
                }
        case schemasFromAppTools OpenAIProvider [jsonTool, spawn] of
            [_, FunctionToolValue _, KnownResponseTool ToolNamespace tagged] -> do
                tagged.tag `shouldBe` "namespace"
                KeyMap.lookup "name" tagged.fields
                    `shouldBe` Just (Aeson.String "collaboration")
            other -> expectationFailure ("expected namespace tool, got " <> show other)

jsonTool :: AppTool
jsonTool = AppTool
    { appToolName = "read_file"
    , appToolDescription = "Read a file."
    , appToolParameters =
        [ PropertySchema "target_file" PropertyString True Nothing
        , PropertySchema "offset" PropertyInteger False Nothing
        ]
    , appToolHandler = noArgsTool "read_file" (pure (Right "ok"))
    , appToolKind = JsonFunction
    , appToolReadOnly = True
    , appToolIsReadOnlyCall = Nothing
    }

patchTool :: AppTool
patchTool = AppTool
    { appToolName = "apply_patch"
    , appToolDescription = "Apply a patch."
    , appToolParameters = []
    , appToolHandler = noArgsTool "apply_patch" (pure (Right "ok"))
    , appToolKind = FreeformApplyPatch
    , appToolReadOnly = False
    , appToolIsReadOnlyCall = Nothing
    }

required_ :: FunctionTool -> Maybe Aeson.Value
required_ tool = do
    Aeson.Object parameters <- tool.parameters
    KeyMap.lookup "required" parameters

offsetType :: FunctionTool -> Maybe Aeson.Value
offsetType tool = do
    Aeson.Object parameters <- tool.parameters
    Aeson.Object properties <- KeyMap.lookup "properties" parameters
    Aeson.Object offset <- KeyMap.lookup (Key.fromText "offset") properties
    KeyMap.lookup "type" offset
