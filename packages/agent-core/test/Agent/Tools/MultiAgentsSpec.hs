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
import Agent.Subagents.TaskPath (joinTaskPath, taskPathRoot)
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallKind(..)
    , ToolCallResult(..)
    , dispatchToolCall
    )
import Agent.Tools.MultiAgents
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRule(..)
    , appToolHandlers
    )
import Control.Concurrent.STM
import Control.Monad (unless)
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
                , multiSelfId = Nothing
                , multiDepth = 1
                , multiTaskPath = workerPath
                , multiRootTurnId = pure Nothing
                , multiResumeFromDisk = Nothing
                , multiCreateWorktree = Nothing
                , multiPrepareSpawn = Nothing
                , multiSendToRoot = Just deliverRoot
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
    , multiSelfId = Nothing
    , multiDepth = 0
    , multiTaskPath = taskPathRoot
    , multiRootTurnId = pure Nothing
    , multiResumeFromDisk = Nothing
    , multiCreateWorktree = Nothing
    , multiPrepareSpawn = Nothing
    , multiSendToRoot = sendToRoot
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
