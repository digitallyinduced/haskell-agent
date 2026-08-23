module Agent.CLI.ToolsSpec (spec) where

import Agent.CLI.Tools
import Agent.Dialect (codexDialect, grokBuildDialect)
import Agent.Loop (LoopError(..))
import Agent.Responses.Types
import Agent.Subagents
    ( closeSubagentRegistry
    , defaultSubagentConfig
    , newSubagentRegistry
    )
import Agent.Subagents.TaskPath (taskPathRoot)
import Agent.ToolDispatch (noArgsTool)
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.Codex.Dialect.ApplyPatch (applyPatchGrammar)
import Agent.Tools.MultiAgents (MultiAgentContext(..), multiAgentTools)
import Agent.Tools.Types
    ( AppTool
    , ApprovalRule(..)
    , freeformApplyPatchAppTool
    , jsonAppTool
    )
import Control.Exception.Safe (bracket)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Foldable (toList)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import System.OsPath (unsafeEncodeUtf)
import Test.Hspec

spec :: Spec
spec = describe "schemasFromAppTools" do
    it "enables built-in web_search ahead of app tools" do
        case schemasFromAppTools codexDialect [jsonTool] of
            KnownResponseTool ToolWebSearch tagged : _ -> do
                tagged.tag `shouldBe` "web_search"
                tagged.fields `shouldBe` KeyMap.empty
            other -> expectationFailure ("expected web_search first, got " <> show other)

    it "builds a strict function tool for OpenAI JSON tools" do
        case schemasFromAppTools codexDialect [jsonTool] of
            [_, FunctionToolValue tool] -> do
                tool.name `shouldBe` "read_file"
                tool.strict `shouldBe` Just True
            other -> expectationFailure ("expected function tool, got " <> show other)

    it "builds a loose grok-build function tool for xAI" do
        case schemasFromAppTools grokBuildDialect [jsonTool] of
            [_, FunctionToolValue tool] -> do
                tool.name `shouldBe` "read_file"
                tool.strict `shouldBe` Nothing
                required_ tool `shouldBe` Just (Aeson.toJSON (["target_file"] :: [Text]))
                offsetType tool `shouldBe` Just (Aeson.String "integer")
            other -> expectationFailure ("expected function tool, got " <> show other)

    it "registers apply_patch as a custom Lark tool" do
        case schemasFromAppTools codexDialect [patchTool] of
            [_, KnownResponseTool ToolCustom tagged] -> do
                tagged.tag `shouldBe` "custom"
                KeyMap.lookup "name" tagged.fields
                    `shouldBe` Just (Aeson.String "apply_patch")
                case KeyMap.lookup "format" tagged.fields of
                    Just (Aeson.Object format) -> do
                        KeyMap.lookup "syntax" format `shouldBe` Just (Aeson.String "lark")
                        let definition = KeyMap.lookup "definition" format
                        definition `shouldBe` Just (Aeson.String applyPatchGrammar)
                        definition `shouldSatisfy` \case
                            Just (Aeson.String grammar) ->
                                Text.isInfixOf "%import common.LF" grammar
                            _ -> False
                    other -> expectationFailure ("expected format object, got " <> show other)
            other -> expectationFailure ("expected custom tool, got " <> show other)

    it "emits collaboration as a Responses namespace tool" do
        let spawn = jsonAppTool "spawn_agent" "Spawn."
                [ PropertySchema "message" PropertyString False Nothing ]
                AlwaysPrompt
                (noArgsTool "spawn_agent" (pure (Right "ok")))
        case schemasFromAppTools codexDialect [jsonTool, spawn] of
            [_, FunctionToolValue _, KnownResponseTool ToolNamespace tagged] -> do
                tagged.tag `shouldBe` "namespace"
                KeyMap.lookup "name" tagged.fields
                    `shouldBe` Just (Aeson.String "collaboration")
            other -> expectationFailure ("expected namespace tool, got " <> show other)

    it "omits an empty required list from reserved collaboration schemas" do
        let wait = jsonAppTool "wait_agent" "Wait."
                [ PropertySchema "timeout_ms" PropertyNumber False Nothing ]
                AlwaysReadOnly
                (noArgsTool "wait_agent" (pure (Right "ok")))
        case schemasFromAppTools codexDialect [wait] of
            [_, KnownResponseTool ToolNamespace tagged] ->
                case KeyMap.lookup "tools" tagged.fields of
                    Just (Aeson.Array tools) -> case toList tools of
                        [Aeson.Object tool] -> do
                            Just (Aeson.Object parameters) <-
                                pure (KeyMap.lookup "parameters" tool)
                            KeyMap.lookup "required" parameters `shouldBe` Nothing
                        other -> expectationFailure
                            ("expected one nested tool, got " <> show other)
                    other -> expectationFailure
                        ("expected namespace tools, got " <> show other)
            other -> expectationFailure
                ("expected collaboration namespace, got " <> show other)

    it "matches the reserved wait_agent schema with the production tool" do
        bracket
            (newSubagentRegistry
                defaultSubagentConfig
                (unsafeEncodeUtf "/tmp")
                (\_ _ _ _ -> pure (Left LoopNoResponseId))
                (\_ _ -> pure ()))
            closeSubagentRegistry
            \registry -> do
                let context = MultiAgentContext
                        { multiRegistry = registry
                        , multiSelfId = Nothing
                        , multiDepth = 0
                        , multiTaskPath = taskPathRoot
                        , multiRootTurnId = pure Nothing
                        , multiResumeFromDisk = Nothing
                        , multiCreateWorktree = Nothing
                        , multiPrepareSpawn = Nothing
                        , multiSendToRoot = Nothing
                        }
                    namespaces =
                        [ tagged
                        | KnownResponseTool ToolNamespace tagged <-
                            schemasFromAppTools
                                codexDialect
                                (multiAgentTools context)
                        ]
                case namespaces of
                    [tagged] ->
                        case KeyMap.lookup "tools" tagged.fields of
                            Just (Aeson.Array tools) -> do
                                let waitTools =
                                        mapMaybe waitAgentObject (toList tools)
                                case waitTools of
                                    [tool] -> do
                                        KeyMap.lookup "strict" tool
                                            `shouldBe` Just (Aeson.Bool False)
                                        Just (Aeson.Object parameters) <-
                                            pure (KeyMap.lookup "parameters" tool)
                                        KeyMap.lookup "required" parameters
                                            `shouldBe` Nothing
                                        KeyMap.lookup "additionalProperties" parameters
                                            `shouldBe` Just (Aeson.Bool False)
                                        Just (Aeson.Object properties) <-
                                            pure (KeyMap.lookup "properties" parameters)
                                        Just (Aeson.Object timeout) <-
                                            pure (KeyMap.lookup "timeout_ms" properties)
                                        KeyMap.lookup "type" timeout
                                            `shouldBe` Just (Aeson.String "number")
                                    other -> expectationFailure
                                        ("expected production wait_agent, got "
                                            <> show other)
                            other -> expectationFailure
                                ("expected namespace tools, got " <> show other)
                    other -> expectationFailure
                        ("expected one collaboration namespace, got "
                            <> show other)

jsonTool :: AppTool
jsonTool = jsonAppTool "read_file" "Read a file."
        [ PropertySchema "target_file" PropertyString True Nothing
        , PropertySchema "offset" PropertyInteger False Nothing
        ]
        AlwaysReadOnly
        (noArgsTool "read_file" (pure (Right "ok")))

patchTool :: AppTool
patchTool =
    freeformApplyPatchAppTool
        "apply_patch" "Apply a patch." AlwaysPrompt
        (noArgsTool "apply_patch" (pure (Right "ok")))

waitAgentObject :: Aeson.Value -> Maybe Aeson.Object
waitAgentObject (Aeson.Object tool)
    | KeyMap.lookup "name" tool == Just (Aeson.String "wait_agent") =
        Just tool
waitAgentObject _ = Nothing

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
