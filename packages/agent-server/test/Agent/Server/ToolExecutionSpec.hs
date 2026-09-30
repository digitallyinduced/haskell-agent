module Agent.Server.ToolExecutionSpec (spec) where

import Agent.Dialect (codexDialect)
import Agent.Loop (LoopError(LoopNoResponseId))
import Agent.Runtime.Config
    ( HarnessConfig(..)
    , WebFetchConfig(..)
    , defaultHarnessConfig
    )
import Agent.Runtime.Tools.Dialects
    ( CodingTools(..)
    , codingToolsFor
    )
import Agent.Runtime.WebFetch
    ( WebFetchOverflow(..)
    , newWebFetchRuntime
    , webFetchToolGroup
    )
import Agent.Server.ToolExecution (composeHostOnlyTools)
import Agent.Subagents
    ( closeSubagentRegistry
    , defaultSubagentConfig
    , newSubagentRegistry
    )
import Agent.Subagents.TaskPath (taskPathRoot)
import Agent.ToolDispatch
    ( ToolCallResult(..)
    , ToolDispatchConfig(..)
    , ToolDispatchOutcome(..)
    , dispatchToolCallDetailed
    , functionToolCall
    , noArgsTool
    )
import Agent.Tools.MultiAgents (MultiAgentContext(..))
import Agent.Tools.OutputArtifact (artifactTools, writeOutputArtifact)
import Agent.Tools.Types
    ( AppTool(..)
    , AppToolGroup(..)
    , ApprovalRule(..)
    , defaultToolEnv
    , jsonAppTool
    )
import Control.Exception.Safe (bracket, finally)
import Control.Monad (forM_)
import Data.Text qualified as Text
import System.OsPath (unsafeEncodeUtf)
import Test.Hspec

spec :: Spec
spec = describe "tenants without tool execution" do
    it "keeps host services and delegation but withholds every execution tool" do
        withCollaborationContext \collaboration -> do
            env <- defaultToolEnv (unsafeEncodeUtf "/workspace")
            coding <-
                codingToolsFor
                    codexDialect env Nothing Nothing Nothing (Just collaboration)
            let names =
                    map (.appToolName)
                        (composeHostOnlyTools coding.codingAppToolGroups)
                assertions = do
                    forM_
                        [ "update_plan"
                        , "ask_user_question"
                        , "spawn_agent"
                        , "wait_agent"
                        , "send_message"
                        , "list_agents"
                        , "read_tool_output"
                        , "search_tool_output"
                        , "analyze_tool_output"
                        ]
                        \name -> names `shouldContain` [name]
                    forM_
                        [ "shell_command"
                        , "write_stdin"
                        , "run_ghci"
                        , "view_image"
                        , "read_file"
                        , "grep"
                        , "list_dir"
                        , "apply_patch"
                        , "set_environment"
                        , "export_tool_output"
                        ]
                        \name -> names `shouldNotContain` [name]
            assertions `finally` coding.codingClose

    it "keeps host-service order unchanged" do
        let groups =
                [ ExecutionToolGroup
                    [testTool "read_file", testTool "read_tool_output"]
                , HostToolGroup [testTool "update_plan"]
                , ExecutionToolGroup [testTool "shell_command"]
                , HostToolGroup
                    [testTool "spawn_agent", testTool "mcp__docs"]
                ]
        map (.appToolName) (composeHostOnlyTools groups)
            `shouldBe`
                ["read_tool_output", "update_plan", "spawn_agent", "mcp__docs"]

    it "keeps the configured web_fetch, which still enforces its policy" do
        env <- defaultToolEnv (unsafeEncodeUtf ".")
        runtime <-
            newWebFetchRuntime
                OverflowToOutputArtifact
                defaultHarnessConfig.configWebFetch
                    { webFetchEnabled = True
                    , webFetchAllowedDomains = ["127.0.0.1"]
                    }
                env
                >>= either (fail . Text.unpack) pure
        let tools =
                composeHostOnlyTools
                    [ webFetchToolGroup runtime
                    , ExecutionToolGroup [testTool "lsp"]
                    ]
        map (.appToolName) tools `shouldBe` ["web_fetch"]
        outcome <-
            dispatchToolCallDetailed
                testDispatchConfig
                (map (.appToolHandler) tools)
                (functionToolCall
                    "fetch"
                    "web_fetch"
                    "{\"url\":\"https://127.0.0.1/\"}")
        outcome.toolDispatchSucceeded `shouldBe` False
        outcome.toolDispatchResult.output
            `shouldSatisfy` Text.isInfixOf "blocked non-public address"

    it "reads retained output on the host" do
        env <- defaultToolEnv (unsafeEncodeUtf ".")
        handle <-
            writeOutputArtifact env "{\"amount\":19900}"
                >>= either (fail . Text.unpack) pure
        let tools =
                composeHostOnlyTools
                    [ExecutionToolGroup (artifactTools env Nothing)]
        map (.appToolName) tools
            `shouldBe` ["read_tool_output", "search_tool_output"]
        outcome <-
            dispatchToolCallDetailed
                testDispatchConfig
                (map (.appToolHandler) tools)
                (functionToolCall
                    "read"
                    "read_tool_output"
                    ("{\"handle\":\"" <> handle <> "\"}"))
        outcome.toolDispatchSucceeded `shouldBe` True
        outcome.toolDispatchResult.output `shouldSatisfy` Text.isInfixOf "19900"

testTool :: Text.Text -> AppTool
testTool name =
    jsonAppTool
        name
        "test"
        []
        AlwaysReadOnly
        (noArgsTool name (pure (Right "ran")))

testDispatchConfig :: ToolDispatchConfig
testDispatchConfig = ToolDispatchConfig
    { toolDispatchUnknownTool = \name -> "unknown tool: " <> name
    , toolDispatchFormatResult = either id id
    , toolDispatchFormatException = \name _ ->
        "tool exception: " <> name
    , toolDispatchOnException = \_ _ -> pure ()
    , toolDispatchOnOutput = \_ _ -> pure ()
    , toolDispatchFinalizeOutput = \_ output -> pure output
    }

withCollaborationContext :: (MultiAgentContext -> IO value) -> IO value
withCollaborationContext action =
    bracket
        (newSubagentRegistry
            defaultSubagentConfig
            (unsafeEncodeUtf "/workspace")
            (\_ _ _ _ -> pure (Left LoopNoResponseId))
            (\_ _ -> pure ()))
        closeSubagentRegistry
        \registry ->
            action MultiAgentContext
                { multiRegistry = registry
                , multiCwd = unsafeEncodeUtf "/workspace"
                , multiSelfId = Nothing
                , multiDepth = 0
                , multiTaskPath = taskPathRoot
                , multiRootTurnId = pure Nothing
                , multiResumeFromDisk = Nothing
                , multiCreateWorktree = Nothing
                , multiPrepareSpawn = Nothing
                , multiSendToRoot = Nothing
                , multiSpawnModelGuidance = Nothing
                , multiAllowedChildModels = Nothing
                , multiResolveChildModel = Nothing
                , multiChildModelAllowed = Nothing
                }
