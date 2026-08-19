module Agent.CLI.ToolsSpec (spec) where

import Agent.CLI.Tools
import Agent.OpenAI.Responses.Types
import Agent.ToolDispatch (noArgsTool)
import Agent.Tools.ApplyPatch (applyPatchGrammar)
import Agent.Tools.Types (AppTool(..), AppToolKind(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Test.Hspec

spec :: Spec
spec = describe "schemasFromAppTools" do
    it "builds a strict function tool for JSON tools" do
        case schemasFromAppTools [jsonTool] of
            [FunctionToolValue tool] -> do
                tool.name `shouldBe` "read_file"
                tool.strict `shouldBe` Just True
            other -> expectationFailure ("expected function tool, got " <> show other)

    it "registers apply_patch as a custom Lark tool" do
        case schemasFromAppTools [patchTool] of
            [KnownResponseTool ToolCustom tagged] -> do
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

jsonTool :: AppTool
jsonTool = AppTool
    { appToolName = "read_file"
    , appToolDescription = "Read a file."
    , appToolParameters = []
    , appToolHandler = noArgsTool "read_file" (pure (Right "ok"))
    , appToolKind = JsonFunction
    , appToolReadOnly = True
    }

patchTool :: AppTool
patchTool = AppTool
    { appToolName = "apply_patch"
    , appToolDescription = "Apply a patch."
    , appToolParameters = []
    , appToolHandler = noArgsTool "apply_patch" (pure (Right "ok"))
    , appToolKind = FreeformApplyPatch
    , appToolReadOnly = False
    }
