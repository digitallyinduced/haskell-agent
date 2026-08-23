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
    , rawJsonAppTool
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

    it "projects current Grok Build public tool and parameter names" do
        let task = jsonAppTool "task"
                "Use `task`, then get_task_output or kill_task."
                [ PropertySchema "prompt" PropertyString True Nothing
                , PropertySchema "run_in_background" PropertyBoolean False
                    (Just "Use get_task_output after run_in_background=true.")
                ]
                AlwaysPrompt
                (noArgsTool "task" (pure (Right "ok")))
            terminal = jsonAppTool "run_terminal_cmd"
                "Run with run_terminal_cmd."
                []
                AlwaysPrompt
                (noArgsTool "run_terminal_cmd" (pure (Right "ok")))
            getOutput = testTool "get_task_output"
            waitTasks = testTool "wait_tasks"
            killTask = testTool "kill_task"
        case schemasFromAppTools grokBuildDialect
            [terminal, task, getOutput, waitTasks, killTask] of
            [ _
                , FunctionToolValue terminalTool
                , FunctionToolValue taskTool
                , FunctionToolValue getOutputTool
                , FunctionToolValue waitTasksTool
                , FunctionToolValue killTaskTool
                ] -> do
                terminalTool.name `shouldBe` "run_terminal_command"
                terminalTool.description
                    `shouldBe` Just "Run with run_terminal_command."
                taskTool.name `shouldBe` "spawn_subagent"
                getOutputTool.name `shouldBe` "get_command_or_subagent_output"
                waitTasksTool.name `shouldBe` "wait_commands_or_subagents"
                killTaskTool.name `shouldBe` "kill_command_or_subagent"
                taskTool.description `shouldBe`
                    Just
                        "Use `spawn_subagent`, then get_command_or_subagent_output or kill_command_or_subagent."
                Just (Aeson.Object parameters) <- pure taskTool.parameters
                Just (Aeson.Object properties) <-
                    pure (KeyMap.lookup "properties" parameters)
                KeyMap.member "background" properties `shouldBe` True
                KeyMap.member "run_in_background" properties `shouldBe` False
                Just (Aeson.Object background) <-
                    pure (KeyMap.lookup "background" properties)
                KeyMap.lookup "description" background `shouldBe`
                    Just
                        (Aeson.String
                            "Use get_command_or_subagent_output after background=true.")
            other -> expectationFailure
                ("expected projected Grok tools, got " <> show other)

    it "keeps internal tool names for non-Grok dialects" do
        let task = testTool "task"
            terminal = testTool "run_terminal_cmd"
        case schemasFromAppTools codexDialect [terminal, task] of
            [_, FunctionToolValue terminalTool, FunctionToolValue taskTool] -> do
                terminalTool.name `shouldBe` "run_terminal_cmd"
                taskTool.name `shouldBe` "task"
            other -> expectationFailure
                ("expected stable internal tool names, got " <> show other)

    it "builds a loose grok-build function tool for xAI" do
        case schemasFromAppTools grokBuildDialect [jsonTool] of
            [_, FunctionToolValue tool] -> do
                tool.name `shouldBe` "read_file"
                tool.strict `shouldBe` Nothing
                required_ tool `shouldBe` Just (Aeson.toJSON (["target_file"] :: [Text]))
                offsetType tool `shouldBe` Just (Aeson.String "integer")
            other -> expectationFailure ("expected function tool, got " <> show other)

    it "preserves a raw MCP schema and disables strict mode for OpenAI" do
        let parameters = Aeson.object
                [ "type" Aeson..= ("object" :: Text)
                , "properties" Aeson..= Aeson.object
                    [ "siteUrl" Aeson..= Aeson.object
                        [ "type" Aeson..= ("string" :: Text)
                        ]
                    ]
                , "required" Aeson..= (["siteUrl"] :: [Text])
                , "additionalProperties" Aeson..= False
                ]
            tool = rawJsonAppTool
                "gsc_site_get"
                "Get a Search Console property."
                parameters
                AlwaysReadOnly
                (noArgsTool "gsc_site_get" (pure (Right "ok")))
        case schemasFromAppTools codexDialect [tool] of
            [_, FunctionToolValue function] -> do
                function.name `shouldBe` "gsc_site_get"
                function.parameters `shouldBe` Just parameters
                function.strict `shouldBe` Just False
            other -> expectationFailure
                ("expected raw OpenAI function tool, got " <> show other)

    it "preserves raw MCP names and schemas for Grok without strict mode" do
        let parameters = Aeson.object
                [ "type" Aeson..= ("object" :: Text)
                , "properties" Aeson..= Aeson.object []
                ]
            tool = rawJsonAppTool
                "seo_auth_status"
                "Show SEO authentication status."
                parameters
                AlwaysReadOnly
                (noArgsTool "seo_auth_status" (pure (Right "ok")))
        case schemasFromAppTools grokBuildDialect [tool] of
            [_, FunctionToolValue function] -> do
                function.name `shouldBe` "seo_auth_status"
                function.parameters `shouldBe` Just parameters
                function.strict `shouldBe` Nothing
            other -> expectationFailure
                ("expected raw Grok function tool, got " <> show other)

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
                        , multiSpawnModelGuidance = Nothing
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

testTool :: Text -> AppTool
testTool name =
    jsonAppTool name ("Use " <> name <> ".") [] AlwaysPrompt
        (noArgsTool name (pure (Right "ok")))

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
