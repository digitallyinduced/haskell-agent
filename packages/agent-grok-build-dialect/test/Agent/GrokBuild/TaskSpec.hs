module Agent.GrokBuild.TaskSpec (spec) where

import Agent.GrokBuild.Dialect.Task
import Agent.Loop (LoopError(..), LoopResult(..), defaultLoopDispatch, emptyTokenUsage)
import Agent.InterAgentMessage
    ( interAgentMessagePayload
    , plainInterAgentContent
    )
import System.OsPath (OsPath, unsafeEncodeUtf)
import Agent.Subagents
import Agent.ToolDispatch
    ( ToolCallResult(..)
    , dispatchToolCall
    , functionToolCall
    , noArgsTool
    )
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.Subagents.TaskPath (taskPathRoot)
import Agent.Tools.MultiAgents (MultiAgentContext(..), SubagentWorktree(..))
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRule(..)
    , PlanModeCapability(..)
    , ToolBatchPhase(..)
    , ToolExecutionPolicy(..)
    , ToolSchema(..)
    , jsonToolParameters
    , toolAllowsWithoutPrompt
    )
import Control.Concurrent.MVar
import Data.Either (isLeft)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

fromFilePath = unsafeEncodeUtf

spec :: Spec
spec = describe "Agent.GrokBuild.Dialect.Task" do
    it "advertises the public task contract without contradicting defaults" do
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ _ _ -> pure $ Left LoopNoResponseId)
            (\_ _ -> pure ())
        typesRef <- newIORef Map.empty
        let ctx = MultiAgentContext registry (fromFilePath "/tmp") Nothing 0 taskPathRoot
                (pure Nothing) Nothing Nothing Nothing Nothing Nothing Nothing
                (pure False)
            tool = taskTool (fromFilePath "/tmp") ctx typesRef
            parameters = fromMaybe [] (jsonToolParameters tool)
        let isolationTypes =
                [ property.propertyType
                | property <- parameters
                , property.propertyName == "isolation"
                ]
        isolationTypes `shouldBe` [PropertyEnum ["none", "worktree"]]
        tool.appToolDescription `shouldSatisfy`
            Text.isInfixOf "get_command_or_subagent_output"
        tool.appToolDescription `shouldSatisfy`
            Text.isInfixOf "prior spawn_subagent call"
        tool.appToolDescription `shouldSatisfy`
            Text.isInfixOf "subagent_type defaults to general-purpose"
        tool.appToolDescription `shouldSatisfy`
            Text.isInfixOf "limited to 4 levels"
        tool.appToolDescription `shouldNotSatisfy`
            Text.isInfixOf "must specify a subagent_type"
        expectClassifiedReadOnly tool.appToolApproval
        toolAllowsWithoutPrompt tool
            (functionToolCall "approval-explore" "task"
                "{\"subagent_type\":\"explore\"}")
            `shouldReturn` True
        toolAllowsWithoutPrompt tool
            (functionToolCall "approval-general" "task"
                "{\"subagent_type\":\"general-purpose\"}")
            `shouldReturn` False
        toolAllowsWithoutPrompt tool
            (functionToolCall "approval-isolated" "task"
                "{\"subagent_type\":\"explore\",\"isolation\":\"worktree\"}")
            `shouldReturn` False
        closeSubagentRegistry registry

    it "canonicalizes Grok-root child model aliases" do
        canonicalizeGrokChildModel "Grok 4.6" `shouldBe` Just "grok-4.6"
        canonicalizeGrokChildModel "grok-4-5" `shouldBe` Just "grok-4.5"
        canonicalizeGrokChildModel "luna" `shouldBe` Just lunaSubagentModel
        canonicalizeGrokChildModel "openai/gpt-5.6-luna"
            `shouldBe` Just lunaSubagentModel
        canonicalizeGrokChildModel "grok-4-1-fast" `shouldBe` Nothing
        canonicalizeGrokChildModel "grok-4.6-mini" `shouldBe` Nothing

    it "rejects unknown Grok-root child models against the allowlist" do
        resolveRequestedGrokChildModel
            (Just (grokRootChildModels True))
            (Just "grok-4-1-fast")
            `shouldSatisfy` isLeft
        resolveRequestedGrokChildModel
            (Just (grokRootChildModels False))
            (Just lunaSubagentModel)
            `shouldSatisfy` isLeft
        resolveRequestedGrokChildModel
            (Just (grokRootChildModels True))
            (Just "luna")
            `shouldBe` Right (Just lunaSubagentModel)
        resolveRequestedGrokChildModel
            (Just (grokRootChildModels True))
            Nothing
            `shouldBe` Right Nothing
        grokRootChildModels False `shouldBe` ["grok-4.6", "grok-4.5"]
        grokRootChildModels True
            `shouldBe` ["grok-4.6", "grok-4.5", lunaSubagentModel]

    it "advertises the Grok-root allowlist and records Luna at high effort" do
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ prompt _ -> pure $ Right LoopResult
                { finalResponseId = "c"
                , finalText = Just ("done:" <> interAgentMessagePayload prompt)
                , turnsUsed = 1
                , tokenUsage = emptyTokenUsage
                })
            (\_ _ -> pure ())
        typesRef <- newIORef Map.empty
        let ctx = MultiAgentContext registry (fromFilePath "/tmp") Nothing 0 taskPathRoot
                (pure Nothing) Nothing Nothing Nothing Nothing Nothing
                (Just (grokRootChildModels True)) (pure False)
            tool = taskTool (fromFilePath "/tmp") ctx typesRef
        tool.appToolDescription `shouldSatisfy`
            Text.isInfixOf "gpt-5.6-luna"
        tool.appToolDescription `shouldSatisfy`
            Text.isInfixOf "ONLY use model slugs"
        result <- dispatchToolCall defaultLoopDispatch [tool.appToolHandler]
            (functionToolCall "c1" "task"
                "{\"prompt\":\"hello\",\"description\":\"luna child\",\"model\":\"luna\"}")
        result.output `shouldSatisfy` Text.isInfixOf "Subagent started in background"
        specs <- Map.elems <$> readIORef typesRef
        map (\entry -> entry.modelOverride) specs
            `shouldBe` [Just lunaSubagentModel]
        map (\entry -> entry.reasoningEffortOverride) specs
            `shouldBe` [Just lunaSubagentEffort]
        closeSubagentRegistry registry

    it "rejects grok-4-1-fast when a Grok-root allowlist is set" do
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ _ _ -> pure $ Left LoopNoResponseId)
            (\_ _ -> pure ())
        typesRef <- newIORef Map.empty
        let ctx = MultiAgentContext registry (fromFilePath "/tmp") Nothing 0 taskPathRoot
                (pure Nothing) Nothing Nothing Nothing Nothing Nothing
                (Just (grokRootChildModels True)) (pure False)
            tool = taskTool (fromFilePath "/tmp") ctx typesRef
        result <- dispatchToolCall defaultLoopDispatch [tool.appToolHandler]
            (functionToolCall "c1" "task"
                "{\"prompt\":\"hello\",\"description\":\"search\",\"model\":\"grok-4-1-fast\"}")
        result.output `shouldSatisfy`
            Text.isInfixOf "Unknown spawn_subagent model"
        Map.null <$> readIORef typesRef `shouldReturn` True
        closeSubagentRegistry registry

    it "defaults run_in_background and spawns a background agent" do
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ prompt _ -> pure $ Right LoopResult
                { finalResponseId = "c"
                , finalText = Just ("done:" <> interAgentMessagePayload prompt)
                , turnsUsed = 1
                , tokenUsage = emptyTokenUsage
                })
            (\_ _ -> pure ())
        typesRef <- newIORef Map.empty
        let ctx = MultiAgentContext registry (fromFilePath "/tmp") Nothing 0 taskPathRoot
                (pure Nothing) Nothing Nothing Nothing Nothing Nothing Nothing
                (pure False)
            tool = taskTool (fromFilePath "/tmp") ctx typesRef
        result <- dispatchToolCall defaultLoopDispatch [tool.appToolHandler]
            (functionToolCall "c1" "task"
                "{\"prompt\":\"hello\",\"description\":\"test task\",\"model\":\"grok-4.5-mini\"}")
        result.output `shouldSatisfy` Text.isInfixOf "Subagent started in background"
        result.output `shouldSatisfy` Text.isInfixOf "subagent_id: agent-"
        specs <- Map.elems <$> readIORef typesRef
        map (\entry -> entry.modelOverride) specs `shouldBe` [Just "grok-4.5-mini"]
        closeSubagentRegistry registry

    it "records overrides before the child supervisor starts" do
        typesRef <- newIORef Map.empty
        observed <- newEmptyMVar
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (observeSpec typesRef observed)
            (\_ _ -> pure ())
        let ctx = MultiAgentContext registry (fromFilePath "/tmp") Nothing 0 taskPathRoot
                (pure Nothing) Nothing Nothing Nothing Nothing Nothing Nothing
                (pure False)
            tool = taskTool (fromFilePath "/tmp") ctx typesRef
        _ <- dispatchToolCall defaultLoopDispatch [tool.appToolHandler]
            (functionToolCall "c1" "task" raceArgs)
        takeMVar observed `shouldReturn` (Just "explore", Just "grok-4.5-mini")
        closeSubagentRegistry registry

    it "updates an agent type without discarding its overrides" do
        specsRef <- newIORef Map.empty
        let agentId = SubagentId "agent-1"
        recordAgentSpec specsRef agentId GrokSubagentSpec
            { agentType = "explore"
            , modelOverride = Just "grok-4.5-mini"
            , reasoningEffortOverride = Just "high"
            }
        recordAgentType specsRef agentId "plan"
        lookupAgentType specsRef agentId `shouldReturn` Just "plan"
        lookupAgentModel specsRef agentId
            `shouldReturn` Just "grok-4.5-mini"
        lookupAgentReasoningEffort specsRef agentId
            `shouldReturn` Just "high"

    it "propagates restore failures for resume_from" do
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ _ _ -> pure $ Left LoopNoResponseId)
            (\_ _ -> pure ())
        typesRef <- newIORef Map.empty
        let restore _ =
                pure (Left "persisted subagent dialect is incompatible")
            ctx = MultiAgentContext registry (fromFilePath "/tmp") Nothing 0 taskPathRoot
                (pure Nothing) (Just restore) Nothing Nothing Nothing Nothing Nothing
                (pure False)
            tool = taskTool (fromFilePath "/tmp") ctx typesRef
        result <- dispatchToolCall defaultLoopDispatch [tool.appToolHandler]
            (functionToolCall "c1" "task"
                "{\"prompt\":\"continue\",\"description\":\"resume\",\
                \\"resume_from\":\"agent-old\",\"subagent_type\":\"explore\"}")
        result.output `shouldSatisfy`
            Text.isInfixOf "persisted subagent dialect is incompatible"
        closeSubagentRegistry registry

    it "filters explore tools to the Grok Build read-only set" do
        let tools =
                [ fake "read_file"
                , fake "search_replace"
                , fake "task"
                , fake "grep"
                , fake "run_terminal_cmd"
                ]
            names = map (.appToolName) (filterGrokToolsForType "explore" tools)
        names `shouldBe` ["read_file", "grep"]
        names `shouldNotContain` ["search_replace"]
        names `shouldNotContain` ["task"]

    it "permits only shared explore agents while plan mode is active" do
        observed <- newEmptyMVar
        worktreeRequested <- newIORef False
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\env _ _ _ -> do
                putMVar observed env.subAccessProfile
                pure $ Right LoopResult
                    { finalResponseId = "done"
                    , finalText = Just "done"
                    , turnsUsed = 1
                    , tokenUsage = emptyTokenUsage
                    })
            (\_ _ -> pure ())
        typesRef <- newIORef Map.empty
        let createWorktree _ = do
                writeIORef worktreeRequested True
                pure (Left "must not run")
            ctx = MultiAgentContext
                registry
                (fromFilePath "/tmp")
                Nothing
                0
                taskPathRoot
                (pure Nothing)
                Nothing
                (Just createWorktree)
                Nothing
                Nothing
                Nothing
                Nothing
                (pure True)
            tool = taskTool (fromFilePath "/tmp") ctx typesRef
            invoke args =
                dispatchToolCall defaultLoopDispatch [tool.appToolHandler]
                    (functionToolCall "plan-task" "task" args)
        rejected <- invoke
            "{\"prompt\":\"edit\",\"description\":\"unsafe\",\
            \\"subagent_type\":\"general-purpose\"}"
        rejected.output `shouldSatisfy`
            Text.isInfixOf "only permits subagent_type=\"explore\""
        isolated <- invoke
            "{\"prompt\":\"read\",\"description\":\"isolated\",\
            \\"subagent_type\":\"explore\",\"isolation\":\"worktree\"}"
        isolated.output `shouldSatisfy`
            Text.isInfixOf "unavailable in plan mode"
        readIORef worktreeRequested `shouldReturn` False
        started <- invoke
            "{\"prompt\":\"read\",\"description\":\"safe\",\
            \\"subagent_type\":\"explore\"}"
        started.output `shouldSatisfy`
            Text.isInfixOf "Subagent started in background"
        takeMVar observed `shouldReturn` SubagentReadOnly
        agents <- listAgents registry Nothing
        case agents of
            [(_, agentId, _)] ->
                getSubagentAccessProfile registry agentId
                    `shouldReturn` Just SubagentReadOnly
            _ -> expectationFailure "expected exactly one explore child"
        closeSubagentRegistry registry

    it "resumes only known explore agents and restricts them before the turn" do
        profiles <- newIORef []
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\env _ _ _ -> do
                atomicModifyIORef' profiles (\seen ->
                    (seen <> [env.subAccessProfile], ()))
                pure $ Right LoopResult
                    { finalResponseId = "done"
                    , finalText = Just "done"
                    , turnsUsed = 1
                    , tokenUsage = emptyTokenUsage
                    })
            (\_ _ -> pure ())
        typesRef <- newIORef Map.empty
        Right (agentId, _) <-
            spawnSubagentAt
                registry
                Nothing
                taskPathRoot
                0
                "existing"
                (plainInterAgentContent "initial")
                Nothing
        _ <- waitSubagents registry [agentId] 15000
        recordAgentType typesRef agentId "general-purpose"
        let ctx = MultiAgentContext
                registry
                (fromFilePath "/tmp")
                Nothing
                0
                taskPathRoot
                (pure Nothing)
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                (pure True)
            tool = taskTool (fromFilePath "/tmp") ctx typesRef
            resumeArgs =
                "{\"prompt\":\"continue\",\"description\":\"resume\",\
                \\"subagent_type\":\"explore\",\"resume_from\":\""
                    <> agentId.unSubagentId
                    <> "\"}"
            invoke =
                dispatchToolCall defaultLoopDispatch [tool.appToolHandler]
                    (functionToolCall "resume-plan-task" "task" resumeArgs)

        mismatched <- invoke
        mismatched.output `shouldSatisfy`
            Text.isInfixOf "subagent_type mismatch"
        getSubagentAccessProfile registry agentId
            `shouldReturn` Just SubagentFullAccess

        recordAgentType typesRef agentId "explore"
        resumed <- invoke
        resumed.output `shouldSatisfy`
            Text.isInfixOf "Subagent started in background"
        _ <- waitSubagents registry [agentId] 15000
        readIORef profiles
            `shouldReturn` [SubagentFullAccess, SubagentReadOnly]
        getSubagentAccessProfile registry agentId
            `shouldReturn` Just SubagentReadOnly
        restrictSubagentAccess registry agentId SubagentFullAccess
            `shouldReturn` Right ()
        getSubagentAccessProfile registry agentId
            `shouldReturn` Just SubagentReadOnly
        closeSubagentRegistry registry

    it "keeps task available to general-purpose children" do
        let tools = [fake "read_file", fake "task"]
            names = map (.appToolName) (filterGrokToolsForType "general-purpose" tools)
        names `shouldBe` ["read_file", "task"]

    it "allows child delegation without an interactive approval prompt" do
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ _ _ -> pure $ Left LoopNoResponseId)
            (\_ _ -> pure ())
        typesRef <- newIORef Map.empty
        let childId = SubagentId "agent-child"
            ctx = MultiAgentContext registry (fromFilePath "/tmp") (Just childId) 1 taskPathRoot
                (pure Nothing) Nothing Nothing Nothing Nothing Nothing Nothing
                (pure False)
            tool = taskTool (fromFilePath "/tmp") ctx typesRef
        expectAlwaysReadOnly tool.appToolApproval
        closeSubagentRegistry registry

    it "keeps an isolated worktree until its child is closed" do
        cleaned <- newIORef False
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ _ _ -> pure $ Left LoopNoResponseId)
            (\_ _ -> pure ())
        typesRef <- newIORef Map.empty
        let createIsolated = cleanupLease cleaned
            ctx = MultiAgentContext registry (fromFilePath "/tmp") Nothing 0 taskPathRoot (pure Nothing)
                Nothing (Just createIsolated) Nothing Nothing Nothing Nothing
                (pure False)
            tool = taskTool (fromFilePath "/tmp") ctx typesRef
        result <- dispatchToolCall defaultLoopDispatch [tool.appToolHandler]
            (functionToolCall "c1" "task"
                "{\"prompt\":\"x\",\"description\":\"y\",\"isolation\":\"worktree\"}")
        result.output `shouldSatisfy` Text.isInfixOf "worktree_path: /tmp/isolate"
        agents <- listAgents registry Nothing
        case agents of
            [(_, agentId, _)] -> do
                _ <- waitSubagents registry [agentId] 15000
                readIORef cleaned `shouldReturn` False
                _ <- closeSubagent registry agentId
                readIORef cleaned `shouldReturn` True
            _ -> expectationFailure "expected exactly one isolated child"
        closeSubagentRegistry registry

    it "cleans up worktrees after failed registry admission" do
        cleaned <- newIORef False
        registry <- closedRegistry
        typesRef <- newIORef Map.empty
        let ctx = MultiAgentContext registry (fromFilePath "/tmp") Nothing 0 taskPathRoot
                (pure Nothing) Nothing (Just (cleanupLease cleaned)) Nothing Nothing Nothing Nothing
                (pure False)
            tool = taskTool (fromFilePath "/tmp") ctx typesRef
        result <- dispatchToolCall defaultLoopDispatch [tool.appToolHandler]
            (functionToolCall "c1" "task" worktreeArgs)
        result.output `shouldSatisfy` Text.isInfixOf "registry is closed"
        readIORef cleaned `shouldReturn` True

expectClassifiedReadOnly :: ApprovalRule -> Expectation
expectClassifiedReadOnly ClassifyReadOnly{} = pure ()
expectClassifiedReadOnly _ =
    expectationFailure "expected dynamic read-only classification"

expectAlwaysReadOnly :: ApprovalRule -> Expectation
expectAlwaysReadOnly AlwaysReadOnly = pure ()
expectAlwaysReadOnly _ = expectationFailure "expected AlwaysReadOnly"

fake :: Text -> AppTool
fake name = AppTool
    { appToolName = name
    , appToolDescription = name
    , appToolSchema = JsonFunctionSchema []
    , appToolHandler = noArgsTool name (pure (Right "ok"))
    , appToolApproval = AlwaysReadOnly
    , appToolExecution = ParallelSafe
    , appToolResourceClaims = Nothing
    , appToolBatchPhase = ToolBatchNormal
    , appToolCallNormalizer = Nothing
    , appToolPlanModeCapability = PlanModeReadOnly
    }

raceArgs :: Text
raceArgs =
    "{\"prompt\":\"hello\",\"description\":\"race test\",\
    \\"subagent_type\":\"explore\",\"model\":\"grok-4.5-mini\"}"

observeSpec
    :: GrokSubagentSpecs
    -> MVar (Maybe Text, Maybe Text)
    -> RunSubagent
observeSpec specs observed env _ _ _ = do
    agentType <- lookupAgentType specs env.subId
    agentModel <- lookupAgentModel specs env.subId
    putMVar observed (agentType, agentModel)
    pure $ Right LoopResult
        { finalResponseId = "c"
        , finalText = Just "done"
        , turnsUsed = 1
        , tokenUsage = emptyTokenUsage
        }

worktreeArgs :: Text
worktreeArgs =
    "{\"prompt\":\"x\",\"description\":\"cleanup\",\
    \\"isolation\":\"worktree\"}"

closedRegistry :: IO SubagentRegistry
closedRegistry = do
    registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
        (\_ _ _ _ -> pure $ Left LoopNoResponseId)
        (\_ _ -> pure ())
    closeSubagentRegistry registry
    pure registry

cleanupLease :: IORef Bool -> OsPath -> IO (Either Text SubagentWorktree)
cleanupLease cleaned _ =
    pure $ Right SubagentWorktree
        { subagentWorktreePath = fromFilePath "/tmp/isolate"
        , subagentWorktreeCleanup =
            writeIORef cleaned True >> pure (Right ())
        }
