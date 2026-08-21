module Agent.Tools.MultiAgentsSpec (spec) where

import Agent.InterAgentMessage
import Agent.Loop
    ( LoopError(..)
    , LoopResult(..)
    , defaultLoopDispatch
    , emptyTokenUsage
    )
import Agent.OsPath (fromFilePath)
import Agent.Subagents
import Agent.Subagents.TaskPath (joinTaskPath, taskPathRoot)
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallKind(..)
    , ToolCallResult(..)
    , dispatchToolCall
    )
import Agent.Tools.MultiAgents
import Agent.Tools.Types (appToolHandlers)
import Control.Concurrent.STM
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "Agent.Tools.MultiAgents" do
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
                , multiResumeFromDisk = Nothing
                , multiCreateWorktree = Nothing
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

rootContext
    :: SubagentRegistry
    -> Maybe (InterAgentMessage -> IO (Either Text Text))
    -> MultiAgentContext
rootContext registry sendToRoot = MultiAgentContext
    { multiRegistry = registry
    , multiSelfId = Nothing
    , multiDepth = 0
    , multiTaskPath = taskPathRoot
    , multiResumeFromDisk = Nothing
    , multiCreateWorktree = Nothing
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

encryptedRootMessageCall :: ToolCall
encryptedRootMessageCall = ToolCall
    { callId = "send-call"
    , name = "collaboration.send_message"
    , arguments = "{\"target\":\"/root\",\"message\":\"gAAAAA-result\"}"
    , callKind = FunctionCallKind
    , argumentsEncrypted = True
    }
