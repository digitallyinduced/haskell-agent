module Agent.CLI.ToolsSpec (spec) where

import Agent.CLI.Tools
import Agent.CLI.ComputerUse (computerUseTool)
import Agent.CLI.CodeModeRuntime
    ( CodeModeProjectionStrategy(..)
    , CodeModeToolProjection(..)
    , filterStartupUnavailableTools
    , imageGenerationCodeModeProjection
    , projectCodeModeTools
    , projectCodeModeToolsFor
    )
import Agent.Dialect
    ( claudeCodeDialect
    , codexDialect
    , grokBuildDialect
    )
import Agent.Loop (LoopError(..))
import Agent.Json (RawJson, rawJsonBytes, rawJsonFromEncoding)
import Agent.Json.Decode qualified as Hermes
import Agent.Responses.Types
import Agent.Subagents
    ( closeSubagentRegistry
    , defaultSubagentConfig
    , newSubagentRegistry
    )
import Agent.Subagents.TaskPath (taskPathRoot)
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallKind(..)
    , ToolCallResult(..)
    , noArgsTool
    )
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.Codex.Dialect.ApplyPatch (applyPatchGrammar)
import Agent.Codex.Dialect.Runtime
    ( CodexCodingTools(..)
    , newCodexCodingTools
    )
import Agent.Tools.MultiAgents (MultiAgentContext(..), multiAgentTools)
import Agent.Tools.CodeMode.Tool (ToolMode(..))
import Agent.Tools.FileSystem.Grep (grepTool)
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRule(..)
    , ToolSchema(..)
    , defaultToolEnv
    , freeformApplyPatchAppTool
    , jsonAppTool
    , mkToolRegistry
    , rawJsonAppTool
    )
import Control.Exception.Safe (bracket)
import Control.Monad (join)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import System.Info (os)
import System.OsPath (unsafeEncodeUtf)
import Test.Hspec

spec :: Spec
spec = describe "schemasFromAppTools" do
    it "advertises computer use as an ordinary strict function" do
        if os == "darwin"
            then case schemasFromAppTools codexDialect [computerUseTool] of
                [_, FunctionToolValue function] -> do
                    function.name `shouldBe` computerFunctionName
                    function.strict `shouldBe` Just True
                    function.parameters `shouldSatisfy` (/= Nothing)
                other -> expectationFailure
                    ("expected ordinary computer function, got " <> show other)
            else schemasFromAppTools codexDialect [computerUseTool]
                `shouldBe` [webSearchTool]

    it "reserves the model-facing computer_use function identity" do
        let collision =
                jsonAppTool computerFunctionName "Unrelated MCP function" []
                    AlwaysPrompt
                    (noArgsTool computerFunctionName (pure (Right "ok")))
        schemasFromAppTools codexDialect [collision]
            `shouldBe` [webSearchTool]
        requireToolRegistry [computerUseTool, collision]
            `shouldThrow` anyIOException

    it "keeps an unrelated function named computer as a function" do
        let computer = jsonAppTool "computer" "Unrelated MCP function" []
                AlwaysPrompt
                (noArgsTool "computer" (pure (Right "ok")))
        case schemasFromAppTools codexDialect [computer] of
            [_, FunctionToolValue function] ->
                function.name `shouldBe` "computer"
            other -> expectationFailure
                ("expected ordinary computer function, got " <> show other)

    it "advertises the local computer function only for the Codex dialect" do
        schemasFromAppTools grokBuildDialect [computerUseTool]
            `shouldBe` [webSearchTool, xSearchTool]
        schemasFromAppTools claudeCodeDialect [computerUseTool]
            `shouldBe` []

    it "keeps native shell tools both direct and nested for code-only models" do
        let tools = map testTool
                ["read_file", "shell_command", "write_stdin", "apply_patch"]
            projection = projectCodeModeTools CodeOnlyToolMode tools
        map (.appToolName) projection.directCodeModeTools
            `shouldBe` ["shell_command", "write_stdin"]
        map (.appToolName) projection.nestedCodeModeTools
            `shouldBe` ["read_file", "shell_command", "write_stdin", "apply_patch"]
    it "keeps hosted computer use direct and out of code mode" do
        let tools =
                [ testTool "read_file"
                , testTool "shell_command"
                , computerUseTool
                ]
            projection = projectCodeModeTools CodeOnlyToolMode tools
        map (.appToolName) projection.directCodeModeTools
            `shouldBe` ["shell_command", "computer"]
        map (.appToolName) projection.nestedCodeModeTools
            `shouldBe` ["read_file", "shell_command"]

    it "reprojects direct computer use when a code-mode tool set changes" do
        let withoutComputer =
                map testTool ["read_file", "imagegen", "shell_command"]
            withComputer = withoutComputer <> [computerUseTool]
            directNames strategy =
                map (.appToolName)
                    . (.directCodeModeTools)
                    . projectCodeModeToolsFor strategy
        directNames FullCodeModeProjection withoutComputer
            `shouldBe` ["shell_command"]
        directNames FullCodeModeProjection withComputer
            `shouldBe` ["shell_command", "computer"]
        directNames ImageGenerationOnlyCodeModeProjection withoutComputer
            `shouldBe` ["read_file", "shell_command"]
        directNames ImageGenerationOnlyCodeModeProjection withComputer
            `shouldBe` ["read_file", "shell_command", "computer"]

    it "retains toggleable computer use when imagegen fails at startup" do
        let refreshTools =
                filterStartupUnavailableTools
                    True
                    [ testTool "read_file"
                    , testTool "imagegen"
                    , computerUseTool
                    ]
        map (.appToolName) refreshTools
            `shouldBe` ["read_file", "computer"]

    it "nests only imagegen for code-only models when full code mode is off" do
        let tools = map testTool ["read_file", "imagegen", "shell_command"]
        case imageGenerationCodeModeProjection CodeOnlyToolMode tools of
            Just projection -> do
                map (.appToolName) projection.directCodeModeTools
                    `shouldBe` ["read_file", "shell_command"]
                map (.appToolName) projection.nestedCodeModeTools
                    `shouldBe` ["imagegen"]
            Nothing ->
                expectationFailure "expected an image-generation projection"
        case imageGenerationCodeModeProjection ConventionalToolMode tools of
            Nothing -> pure ()
            Just _ ->
                expectationFailure
                    "conventional models should keep imagegen direct"

    it "enables built-in web_search ahead of app tools" do
        case schemasFromAppTools codexDialect [jsonTool] of
            KnownResponseTool ToolWebSearch : _ -> pure ()
            other -> expectationFailure ("expected web_search first, got " <> show other)

    it "omits provider-hosted search at a sandboxed network boundary" do
        schemasFromAppToolsWithHostedSearch
            False
            codexDialect
            []
            `shouldBe` []
        schemasFromAppToolsWithHostedSearch
            False
            grokBuildDialect
            []
            `shouldBe` []
        schemasFromAppToolsCodeModeWithHostedSearch
            False
            codexDialect
            []
            `shouldBe` []

    it "disables strict mode for all OpenAI JSON tools" do
        case schemasFromAppTools codexDialect [jsonTool] of
            [_, FunctionToolValue tool] -> do
                tool.name `shouldBe` "read_file"
                tool.strict `shouldBe` Just False
                required_ tool `shouldBe` Just ["target_file"]
                propertyNames tool `shouldContain` ["offset"]
            other -> expectationFailure ("expected function tool, got " <> show other)

    it "keeps grep path optional" do
        env <- defaultToolEnv (unsafeEncodeUtf "/tmp")
        case schemasFromAppTools codexDialect [grepTool env] of
            [_, FunctionToolValue tool] -> do
                tool.name `shouldBe` "grep"
                tool.strict `shouldBe` Just False
                required_ tool `shouldBe` Just ["pattern"]
                propertyNames tool `shouldContain` ["path"]
            other -> expectationFailure
                ("expected non-strict grep function tool, got " <> show other)

    it "keeps shell_command workdir optional in direct and code-only mode" do
        env <- defaultToolEnv (unsafeEncodeUtf "/tmp")
        bracket
            (newCodexCodingTools env Nothing Nothing)
            (.codexClose)
            \coding ->
                case
                    [ tool
                    | tool <- coding.codexAppTools
                    , tool.appToolName == "shell_command"
                    ] of
                    [shell] -> do
                        shell.appToolSchema `shouldSatisfy` \case
                            RawJsonFunctionSchema _ -> True
                            _ -> False

                        assertOptionalShellSchema $
                            schemasFromAppTools
                                codexDialect
                                coding.codexAppTools

                        let projection =
                                projectCodeModeTools
                                    CodeOnlyToolMode
                                    coding.codexAppTools
                            projectedSchemas tools =
                                [ tool.appToolSchema
                                | tool <- tools
                                , tool.appToolName == "shell_command"
                                ]
                        projectedSchemas projection.directCodeModeTools
                            `shouldBe` [shell.appToolSchema]
                        projectedSchemas projection.nestedCodeModeTools
                            `shouldBe` [shell.appToolSchema]
                        assertOptionalShellSchema $
                            schemasFromAppToolsCodeMode
                                codexDialect
                                projection.directCodeModeTools
                    other -> expectationFailure
                        ("expected one shell_command app tool, got "
                            <> show (map (.appToolName) other))

    it "does not advertise harness tools to the Claude Code subprocess" do
        schemasFromAppTools claudeCodeDialect [jsonTool, patchTool]
            `shouldBe` []

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
            [ KnownResponseTool ToolWebSearch
                , KnownResponseTool ToolXSearch
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
                propertyNames taskTool `shouldMatchList`
                    ["prompt", "background"]
                propertyDescription "background" taskTool `shouldBe`
                    Just
                        "Use get_command_or_subagent_output after background=true."
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
            [KnownResponseTool ToolWebSearch, KnownResponseTool ToolXSearch, FunctionToolValue tool] -> do
                tool.name `shouldBe` "read_file"
                tool.strict `shouldBe` Nothing
                required_ tool `shouldBe` Just ["target_file"]
                offsetType tool `shouldBe` Just "integer"
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
                function.parameters `shouldBe` Just (rawJsonValue parameters)
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
            [KnownResponseTool ToolWebSearch, KnownResponseTool ToolXSearch, FunctionToolValue function] -> do
                function.name `shouldBe` "seo_auth_status"
                function.parameters `shouldBe` Just (rawJsonValue parameters)
                function.strict `shouldBe` Nothing
            other -> expectationFailure
                ("expected raw Grok function tool, got " <> show other)

    it "registers apply_patch as a custom Lark tool" do
        case schemasFromAppTools codexDialect [patchTool] of
            [_, CustomToolValue tool] -> do
                tool.name `shouldBe` "apply_patch"
                fmap (rawTextField "syntax") tool.format
                    `shouldBe` Just (Just "lark")
                let definition = tool.format >>= rawTextField "definition"
                definition `shouldBe` Just applyPatchGrammar
                definition `shouldSatisfy`
                    maybe False (Text.isInfixOf "%import common.LF")
            other -> expectationFailure ("expected custom tool, got " <> show other)

    it "emits collaboration as a Responses namespace tool" do
        let spawn = jsonAppTool "spawn_agent" "Spawn."
                [ PropertySchema "message" PropertyString False Nothing ]
                AlwaysPrompt
                (noArgsTool "spawn_agent" (pure (Right "ok")))
        case schemasFromAppTools codexDialect [jsonTool, spawn] of
            [_, FunctionToolValue _, NamespaceToolValue namespace] -> do
                namespace.name `shouldBe` "collaboration"
            other -> expectationFailure ("expected namespace tool, got " <> show other)

    it "emits imagegen in the reserved image_gen namespace with its raw schema" do
        let parameters = Aeson.object
                [ "type" Aeson..= ("object" :: Text)
                , "properties" Aeson..= Aeson.object
                    [ "prompt" Aeson..= Aeson.object
                        ["type" Aeson..= ("string" :: Text)]
                    ]
                , "required" Aeson..= (["prompt"] :: [Text])
                , "additionalProperties" Aeson..= False
                ]
            imagegen = rawJsonAppTool
                "imagegen"
                "Generate an image."
                parameters
                AlwaysAllowed
                (noArgsTool "imagegen" (pure (Right "ok")))
        case schemasFromAppTools codexDialect [imagegen] of
            [_, NamespaceToolValue namespace] -> do
                namespace.name `shouldBe` "image_gen"
                case namespace.tools of
                    [FunctionToolValue tool] -> do
                        tool.name `shouldBe` "imagegen"
                        tool.strict `shouldBe` Just False
                        tool.parameters `shouldBe`
                            Just (rawJsonValue parameters)
                    other -> expectationFailure
                        ("expected one imagegen function, got " <> show other)
            other -> expectationFailure
                ("expected image_gen namespace, got " <> show other)

    it "omits an empty required list from reserved collaboration schemas" do
        let wait = jsonAppTool "wait_agent" "Wait."
                [ PropertySchema "timeout_ms" PropertyNumber False Nothing ]
                AlwaysReadOnly
                (noArgsTool "wait_agent" (pure (Right "ok")))
        case schemasFromAppTools codexDialect [wait] of
            [_, NamespaceToolValue namespace] ->
                case namespace.tools of
                    [FunctionToolValue tool] ->
                        required_ tool `shouldBe` Nothing
                    other -> expectationFailure
                        ("expected one nested tool, got " <> show other)
            other -> expectationFailure
                ("expected collaboration namespace, got " <> show other)

    it "keeps reserved collaboration schemas exact and exposes worktree spawn separately" do
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
                        , multiCwd = unsafeEncodeUtf "/tmp"
                        , multiSelfId = Nothing
                        , multiDepth = 0
                        , multiTaskPath = taskPathRoot
                        , multiRootTurnId = pure Nothing
                        , multiResumeFromDisk = Nothing
                        , multiCreateWorktree = Just
                            (\_ -> pure (Left "not used"))
                        , multiPrepareSpawn = Nothing
                        , multiSendToRoot = Nothing
                        , multiSpawnModelGuidance = Nothing
                        , multiAllowedChildModels = Nothing
                        , multiResolveChildModel = Nothing
                        , multiChildModelAllowed = Nothing
                        }
                    schemas =
                        schemasFromAppTools
                            codexDialect
                            (multiAgentTools context)
                    namespaces =
                        [ namespace
                        | NamespaceToolValue namespace <-
                            schemas
                        ]
                    worktreeFunctions =
                        [ function
                        | FunctionToolValue function <- schemas
                        , function.name == "spawn_agent_in_worktree"
                        ]
                case worktreeFunctions of
                    [function] -> do
                        function.strict `shouldBe` Just False
                        required_ function `shouldBe` Just
                            ["task_name", "message"]
                        propertyNames function `shouldMatchList`
                            [ "task_name", "message", "model"
                            , "reasoning_effort", "fork_turns"
                            ]
                    other -> expectationFailure
                        ("expected top-level spawn_agent_in_worktree, got "
                            <> show other)
                case namespaces of
                    [namespace] -> do
                        let spawnTools =
                                functionToolsNamed
                                    "spawn_agent"
                                    namespace.tools
                        case spawnTools of
                            [tool] -> do
                                required_ tool `shouldBe`
                                    Just ["task_name", "message"]
                                propertyNames tool
                                    `shouldMatchList`
                                        [ "task_name"
                                        , "message"
                                        , "model"
                                        , "reasoning_effort"
                                        , "fork_turns"
                                        ]
                            other -> expectationFailure
                                ("expected production spawn_agent, got "
                                    <> show other)
                        let waitTools =
                                functionToolsNamed
                                    "wait_agent"
                                    namespace.tools
                        case waitTools of
                            [tool] -> do
                                tool.strict `shouldBe` Just False
                                required_ tool `shouldBe` Nothing
                                additionalProperties tool `shouldBe`
                                    Just False
                                propertyType "timeout_ms" tool `shouldBe`
                                    Just "number"
                            other -> expectationFailure
                                ("expected production wait_agent, got "
                                    <> show other)
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

functionToolsNamed :: Text -> [ResponseTool] -> [FunctionTool]
functionToolsNamed expected =
    foldr
        (\case
            FunctionToolValue tool
                | tool.name == expected -> (tool :)
            _ -> id)
        []

required_ :: FunctionTool -> Maybe [Text]
required_ tool =
    join $ tool.parameters >>= decodeRaw (Hermes.object $
        Hermes.optionalKey "required" (Hermes.list Hermes.text))

offsetType :: FunctionTool -> Maybe Text
offsetType = propertyType "offset"

propertyNames :: FunctionTool -> [Text]
propertyNames tool =
    maybe [] id $ tool.parameters >>= decodeRaw (Hermes.object $
        Hermes.atKey "properties" $
            Hermes.objectFold []
                (\key names ->
                    (key : names) <$ Hermes.withOwnedRawJson (const (pure ()))))

propertyDescription :: Text -> FunctionTool -> Maybe Text
propertyDescription propertyName tool =
    tool.parameters >>= decodeRaw (Hermes.object $
        Hermes.atKey "properties" $ Hermes.object $
            Hermes.atKey propertyName $ Hermes.object $
                Hermes.atKey "description" Hermes.text)

propertyType :: Text -> FunctionTool -> Maybe Text
propertyType propertyName tool =
    tool.parameters >>= decodeRaw (Hermes.object $
        Hermes.atKey "properties" $ Hermes.object $
            Hermes.atKey propertyName $ Hermes.object $
                Hermes.atKey "type" Hermes.text)

additionalProperties :: FunctionTool -> Maybe Bool
additionalProperties tool =
    join $ tool.parameters >>= decodeRaw (Hermes.object $
        Hermes.optionalKey "additionalProperties" Hermes.bool)

rawTextField :: Text -> RawJson -> Maybe Text
rawTextField fieldName =
    decodeRaw (Hermes.object (Hermes.atKey fieldName Hermes.text))

decodeRaw :: Hermes.Decoder value -> RawJson -> Maybe value
decodeRaw decoder =
    either (const Nothing) Just . Hermes.decodeEither decoder . rawJsonBytes

rawJsonValue :: Aeson.ToJSON value => value -> RawJson
rawJsonValue = rawJsonFromEncoding . Aeson.toEncoding

assertOptionalShellSchema :: [ResponseTool] -> Expectation
assertOptionalShellSchema tools =
    case functionToolsNamed "shell_command" tools of
        [shell] -> do
            shell.strict `shouldBe` Just False
            required_ shell `shouldBe` Just ["command"]
            shell.description `shouldSatisfy`
                maybe False (Text.isInfixOf "`workdir` is optional")
            propertyNames shell `shouldMatchList`
                ["command", "workdir", "timeout_ms", "yield_time_ms"]
            propertyType "workdir" shell `shouldBe` Just "string"
        other -> expectationFailure
            ("expected one shell_command function schema, got " <> show other)
