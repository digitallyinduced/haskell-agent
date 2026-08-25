module Agent.Tools.MultiAgentsSpec (spec) where

import Agent.InterAgentMessage
import Agent.Loop
    ( LoopError(..)
    , LoopResult(..)
    , defaultLoopDispatch
    , emptyTokenUsage
    )
import System.OsPath (unsafeEncodeUtf)
import Agent.Subagents
import Agent.Subagents.TaskPath (joinTaskPath, taskPathRoot, taskPathText)
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallKind(..)
    , ToolCallResult(..)
    , dispatchToolCall
    )
import Agent.ToolDSL (PropertySchema(..))
import Agent.Tools.MultiAgents
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRule(..)
    , appToolHandlers
    , jsonToolParameters
    )
import Control.Concurrent.STM
import Control.Monad (unless)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

fromFilePath = unsafeEncodeUtf

spec :: Spec
spec = describe "Agent.Tools.MultiAgents" do
    it "keeps plaintext inter-agent content useful in Show output" do
        let rendered = show (PlainInterAgentContent "visible task")
        rendered `shouldContain` "PlainInterAgentContent"
        rendered `shouldContain` "visible task"

    it "redacts encrypted inter-agent content from Show output" do
        let secret = "gAAAAA-secret-ciphertext"
            message = InterAgentMessage
                { messageAuthor = "/root"
                , messageRecipient = "/root/worker"
                , messageType = NewTaskMessage
                , messageContent = EncryptedInterAgentContent secret
                }
            rendered = show message
        rendered `shouldContain` "/root/worker"
        rendered `shouldContain` "NewTaskMessage"
        rendered `shouldContain` "<redacted>"
        rendered `shouldNotContain` "gAAAAA-secret-ciphertext"

    it "allows collaboration coordination without approval" do
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ _ _ -> pure $ Left LoopNoResponseId)
            (\_ _ -> pure ())
        let tools = multiAgentTools (rootContext registry Nothing)
        map (\tool -> (tool.appToolName, isReadOnly tool.appToolApproval)) tools
            `shouldBe`
                [ ("spawn_agent", True)
                , ("wait_agent", True)
                , ("send_message", True)
                , ("followup_task", True)
                , ("list_agents", True)
                , ("interrupt_agent", True)
                ]
        closeSubagentRegistry registry

    it "spawns canonical paths through depth four and rejects depth five" do
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\env _ _ _ -> pure (resultWithText env.subId.unSubagentId))
            (\_ _ -> pure ())
        level1 <- spawnFrom (rootContext registry Nothing) "level1"
        level1Context <- childContext registry level1 1
        level2 <- spawnFrom level1Context "level2"
        level2Context <- childContext registry level2 2
        level3 <- spawnFrom level2Context "level3"
        level3Context <- childContext registry level3 3
        level4 <- spawnFrom level3Context "level4"
        level4Path <- fromMaybe taskPathRoot <$> getTaskPath registry level4
        taskPathText level4Path
            `shouldBe` "/root/level1/level2/level3/level4"
        level4Context <- childContext registry level4 4
        rejected <- dispatchToolCall defaultLoopDispatch
            (appToolHandlers
                (multiAgentTools level4Context))
            (spawnCall "level5")
        rejected.output `shouldSatisfy`
            Text.isInfixOf "maximum depth 4"
        closeSubagentRegistry registry

    it "adds host-provided model guidance to spawn_agent" do
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ _ _ -> pure $ Left LoopNoResponseId)
            (\_ _ -> pure ())
        let context = (rootContext registry Nothing)
                { multiSpawnModelGuidance =
                    Just "Prefer `gpt-5.6-luna` for small tasks."
                }
            descriptions =
                [ description
                | tool <- multiAgentTools context
                , tool.appToolName == "spawn_agent"
                , property <- fromMaybe [] (jsonToolParameters tool)
                , property.propertyName == "model"
                , description <- maybe [] pure property.description
                ]
        descriptions `shouldSatisfy`
            any (Text.isInfixOf "Prefer `gpt-5.6-luna` for small tasks.")
        closeSubagentRegistry registry

    it "advertises optional worktree isolation on spawn_agent" do
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ _ _ -> pure $ Left LoopNoResponseId)
            (\_ _ -> pure ())
        let propertyNames =
                [ property.propertyName
                | tool <- multiAgentTools (rootContext registry Nothing)
                , tool.appToolName == "spawn_agent"
                , property <- fromMaybe [] (jsonToolParameters tool)
                ]
        propertyNames `shouldContain` ["isolation"]
        closeSubagentRegistry registry

    it "preserves encrypted spawn payloads" do
        spawned <- newEmptyTMVarIO
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ message _ -> do
                atomically (putTMVar spawned message)
                pure $ Right LoopResult
                    { finalResponseId = "child-response"
                    , finalText = Just "done"
                    , turnsUsed = 1
                    , tokenUsage = emptyTokenUsage
                    })
            (\_ _ -> pure ())
        result <- dispatchToolCall defaultLoopDispatch
            (appToolHandlers (multiAgentTools (rootContext registry Nothing)))
            encryptedSpawnCall
        result.output `shouldSatisfy` Text.isInfixOf "/root/worker"
        message <- atomically (takeTMVar spawned)
        message.messageAuthor `shouldBe` "/root"
        message.messageRecipient `shouldBe` "/root/worker"
        message.messageType `shouldBe` NewTaskMessage
        message.messageContent `shouldBe`
            EncryptedInterAgentContent "gAAAAA-task"
        closeSubagentRegistry registry

    it "applies model, effort, and fork overrides before the supervisor starts" do
        prepared <- newEmptyTMVarIO
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\env _ _ _ -> pure (resultWithText env.subId.unSubagentId))
            (\_ _ -> pure ())
        let prepare _ options = atomically (putTMVar prepared options)
            context = (rootContext registry Nothing)
                { multiPrepareSpawn = Just prepare }
            call = ToolCall
                { callId = "spawn-options"
                , name = "collaboration.spawn_agent"
                , arguments =
                    "{\"task_name\":\"worker\",\"message\":\"task\",\
                    \\"model\":\"gpt-test\",\"reasoning_effort\":\"high\",\
                    \\"fork_turns\":\"3\"}"
                , callKind = FunctionCallKind
                , argumentsEncrypted = False
                }
        result <- dispatchToolCall defaultLoopDispatch
            (appToolHandlers (multiAgentTools context)) call
        result.output `shouldSatisfy` Text.isInfixOf "/root/worker"
        options <- atomically (takeTMVar prepared)
        options `shouldBe` CollaborationSpawnOptions
            { collaborationModel = Just "gpt-test"
            , collaborationReasoningEffort = Just "high"
            , collaborationForkTurns = Just "3"
            }
        closeSubagentRegistry registry

    it "spawns an isolated child in a host-provided worktree" do
        childCwd <- newEmptyTMVarIO
        cleaned <- newIORef False
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp/root")
            (\env _ _ _ -> do
                atomically (putTMVar childCwd env.subCwd)
                pure (resultWithText "done"))
            (\_ _ -> pure ())
        let worktreePath = fromFilePath "/tmp/worktree"
            createWorktree source = do
                source `shouldBe` fromFilePath "/tmp/root"
                pure $ Right SubagentWorktree
                    { subagentWorktreePath = worktreePath
                    , subagentWorktreeCleanup =
                        writeIORef cleaned True >> pure (Right ())
                    }
            context = (rootContext registry Nothing)
                { multiCwd = fromFilePath "/tmp/root"
                , multiCreateWorktree = Just createWorktree
                }
            call = ToolCall
                { callId = "spawn-worktree"
                , name = "collaboration.spawn_agent"
                , arguments =
                    "{\"task_name\":\"worker\",\"message\":\"task\",\
                    \\"isolation\":\"worktree\"}"
                , callKind = FunctionCallKind
                , argumentsEncrypted = False
                }
        result <- dispatchToolCall defaultLoopDispatch
            (appToolHandlers (multiAgentTools context)) call
        result.output `shouldSatisfy` Text.isInfixOf "/tmp/worktree"
        atomically (takeTMVar childCwd) `shouldReturn` worktreePath
        readIORef cleaned `shouldReturn` False
        closeSubagentRegistry registry
        readIORef cleaned `shouldReturn` True

    it "rejects worktree isolation when the host does not provide it" do
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ _ _ -> pure $ Left LoopNoResponseId)
            (\_ _ -> pure ())
        let call = ToolCall
                { callId = "spawn-worktree-unavailable"
                , name = "collaboration.spawn_agent"
                , arguments =
                    "{\"task_name\":\"worker\",\"message\":\"task\",\
                    \\"isolation\":\"worktree\"}"
                , callKind = FunctionCallKind
                , argumentsEncrypted = False
                }
        result <- dispatchToolCall defaultLoopDispatch
            (appToolHandlers (multiAgentTools (rootContext registry Nothing))) call
        result.output `shouldSatisfy` Text.isInfixOf "unavailable"
        closeSubagentRegistry registry

    it "rejects zero fork turns" do
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ _ _ -> pure $ Left LoopNoResponseId)
            (\_ _ -> pure ())
        let call = ToolCall
                { callId = "spawn-zero"
                , name = "collaboration.spawn_agent"
                , arguments =
                    "{\"task_name\":\"worker\",\"message\":\"task\",\
                    \\"fork_turns\":\"0\"}"
                , callKind = FunctionCallKind
                , argumentsEncrypted = False
                }
        result <- dispatchToolCall defaultLoopDispatch
            (appToolHandlers (multiAgentTools (rootContext registry Nothing))) call
        result.output `shouldSatisfy` Text.isInfixOf "positive integer"
        closeSubagentRegistry registry

    it "rejects overflowing fork turns instead of wrapping them" do
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ _ _ -> pure $ Left LoopNoResponseId)
            (\_ _ -> pure ())
        let call = ToolCall
                { callId = "spawn-overflow"
                , name = "collaboration.spawn_agent"
                , arguments =
                    "{\"task_name\":\"worker\",\"message\":\"task\",\
                    \\"fork_turns\":\"18446744073709551617\"}"
                , callKind = FunctionCallKind
                , argumentsEncrypted = False
                }
        result <- dispatchToolCall defaultLoopDispatch
            (appToolHandlers (multiAgentTools (rootContext registry Nothing))) call
        result.output `shouldSatisfy` Text.isInfixOf "positive integer"
        closeSubagentRegistry registry

    it "wait_agent excludes the calling child" do
        parentGate <- newTVarIO False
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ message _ ->
                if message.messageRecipient == "/root/parent"
                    then do
                        atomically $ readTVar parentGate >>= \ready -> unless ready retry
                        pure (resultWithText "parent")
                    else pure (resultWithText "child"))
            (\_ _ -> pure ())
        Right (parent, parentPath) <-
            spawnSubagentAt registry Nothing taskPathRoot 0 "parent"
                (plainInterAgentContent "parent") Nothing
        Right (child, _) <-
            spawnSubagentAt registry (Just parent) parentPath 1 "child"
                (plainInterAgentContent "child") Nothing
        let context = (rootContext registry Nothing)
                { multiSelfId = Just parent
                , multiDepth = 1
                , multiTaskPath = parentPath
                }
        result <- dispatchToolCall defaultLoopDispatch
            (appToolHandlers (multiAgentTools context))
            waitCall
        result.output `shouldSatisfy` Text.isInfixOf child.unSubagentId
        result.output `shouldNotSatisfy` Text.isInfixOf parent.unSubagentId
        atomically (writeTVar parentGate True)
        closeSubagentRegistry registry

    it "accepts non-object input for optional-only collaboration tools" do
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ _ _ -> pure (resultWithText "child"))
            (\_ _ -> pure ())
        Right _ <-
            spawnSubagentAt registry Nothing taskPathRoot 0 "child"
                (plainInterAgentContent "child") Nothing
        let handlers =
                appToolHandlers (multiAgentTools (rootContext registry Nothing))
        listed <- dispatchToolCall defaultLoopDispatch handlers
            (ToolCall "list-empty" "collaboration.list_agents" ""
                FunctionCallKind False)
        listed.output `shouldNotSatisfy`
            Text.isInfixOf "Expected object input"
        waited <- dispatchToolCall defaultLoopDispatch handlers
            (ToolCall "wait-array" "collaboration.wait_agent" "[]"
                FunctionCallKind False)
        waited.output `shouldNotSatisfy`
            Text.isInfixOf "Expected object input"
        closeSubagentRegistry registry

    it "propagates restore failures from message tools" do
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ _ _ -> pure (Left LoopNoResponseId))
            (\_ _ -> pure ())
        let context = (rootContext registry Nothing)
                { multiResumeFromDisk = Just (\_ -> pure (Left "restore failed")) }
        result <- dispatchToolCall defaultLoopDispatch
            (appToolHandlers (multiAgentTools context))
            sendMissingCall
        result.output `shouldSatisfy` Text.isInfixOf "restore failed"
        closeSubagentRegistry registry

    it "rejects blank send and follow-up messages before resolving targets" do
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ _ _ -> pure $ Left LoopNoResponseId)
            (\_ _ -> pure ())
        let handlers = appToolHandlers
                (multiAgentTools (rootContext registry Nothing))
            blankCall name = ToolCall
                { callId = "blank-" <> name
                , name = "collaboration." <> name
                , arguments = "{\"target\":\"/root\",\"message\":\" \\n \"}"
                , callKind = FunctionCallKind
                , argumentsEncrypted = False
                }
        sendResult <- dispatchToolCall defaultLoopDispatch handlers
            (blankCall "send_message")
        followupResult <- dispatchToolCall defaultLoopDispatch handlers
            (blankCall "followup_task")
        sendResult.output `shouldBe`
            "Error: send_message requires a non-empty message"
        followupResult.output `shouldBe`
            "Error: followup_task requires a non-empty message"
        closeSubagentRegistry registry

    it "routes encrypted child messages to the root inbox" do
        rootInbox <- newEmptyTMVarIO
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ _ _ -> pure $ Left LoopNoResponseId)
            (\_ _ -> pure ())
        Right workerPath <- pure (joinTaskPath taskPathRoot "worker")
        let deliverRoot message = do
                atomically (putTMVar rootInbox message)
                pure (Right "queued")
            context = MultiAgentContext
                { multiRegistry = registry
                , multiCwd = fromFilePath "/tmp"
                , multiSelfId = Nothing
                , multiDepth = 1
                , multiTaskPath = workerPath
                , multiRootTurnId = pure Nothing
                , multiResumeFromDisk = Nothing
                , multiCreateWorktree = Nothing
                , multiPrepareSpawn = Nothing
                , multiSendToRoot = Just deliverRoot
                , multiSpawnModelGuidance = Nothing
                }
        result <- dispatchToolCall defaultLoopDispatch
            (appToolHandlers (multiAgentTools context))
            encryptedRootMessageCall
        result.output `shouldBe` "queued"
        message <- atomically (takeTMVar rootInbox)
        message.messageAuthor `shouldBe` "/root/worker"
        message.messageRecipient `shouldBe` "/root"
        message.messageType `shouldBe` QueuedMessage
        message.messageContent `shouldBe`
            EncryptedInterAgentContent "gAAAAA-result"
        closeSubagentRegistry registry

isReadOnly :: ApprovalRule -> Bool
isReadOnly AlwaysReadOnly = True
isReadOnly _ = False

rootContext
    :: SubagentRegistry
    -> Maybe (InterAgentMessage -> IO (Either Text Text))
    -> MultiAgentContext
rootContext registry sendToRoot = MultiAgentContext
    { multiRegistry = registry
    , multiCwd = fromFilePath "/tmp"
    , multiSelfId = Nothing
    , multiDepth = 0
    , multiTaskPath = taskPathRoot
    , multiRootTurnId = pure Nothing
    , multiResumeFromDisk = Nothing
    , multiCreateWorktree = Nothing
    , multiPrepareSpawn = Nothing
    , multiSendToRoot = sendToRoot
    , multiSpawnModelGuidance = Nothing
    }

childContext :: SubagentRegistry -> SubagentId -> Int -> IO MultiAgentContext
childContext registry agentId depth = do
    path <- fromMaybe taskPathRoot <$> getTaskPath registry agentId
    pure $ (rootContext registry Nothing)
        { multiSelfId = Just agentId
        , multiDepth = depth
        , multiTaskPath = path
        }

spawnFrom :: MultiAgentContext -> Text -> IO SubagentId
spawnFrom context taskName = do
    result <- dispatchToolCall defaultLoopDispatch
        (appToolHandlers (multiAgentTools context))
        (spawnCall taskName)
    result.output `shouldSatisfy` Text.isInfixOf taskName
    resolved <-
        resolveAgentTarget
            context.multiRegistry context.multiTaskPath taskName
    case resolved of
        Left err -> expectationFailure (Text.unpack err) >> fail "unreachable"
        Right agentId -> pure agentId

spawnCall :: Text -> ToolCall
spawnCall taskName = ToolCall
    { callId = "spawn-" <> taskName
    , name = "collaboration.spawn_agent"
    , arguments =
        "{\"task_name\":\"" <> taskName <> "\",\"message\":\"task\"}"
    , callKind = FunctionCallKind
    , argumentsEncrypted = False
    }

encryptedSpawnCall :: ToolCall
encryptedSpawnCall = ToolCall
    { callId = "spawn-call"
    , name = "collaboration.spawn_agent"
    , arguments = "{\"task_name\":\"worker\",\"message\":\"gAAAAA-task\"}"
    , callKind = FunctionCallKind
    , argumentsEncrypted = True
    }

waitCall :: ToolCall
waitCall = ToolCall
    { callId = "wait-call"
    , name = "collaboration.wait_agent"
    , arguments = "{\"timeout_ms\":10000}"
    , callKind = FunctionCallKind
    , argumentsEncrypted = False
    }

sendMissingCall :: ToolCall
sendMissingCall = ToolCall "send-missing" "collaboration.send_message"
    "{\"target\":\"agent-missing-1\",\"message\":\"hello\"}"
    FunctionCallKind False

resultWithText :: Text -> Either LoopError LoopResult
resultWithText text = Right LoopResult
    { finalResponseId = text
    , finalText = Just text
    , turnsUsed = 1
    , tokenUsage = emptyTokenUsage
    }

encryptedRootMessageCall :: ToolCall
encryptedRootMessageCall = ToolCall
    { callId = "send-call"
    , name = "collaboration.send_message"
    , arguments = "{\"target\":\"/root\",\"message\":\"gAAAAA-result\"}"
    , callKind = FunctionCallKind
    , argumentsEncrypted = True
    }
