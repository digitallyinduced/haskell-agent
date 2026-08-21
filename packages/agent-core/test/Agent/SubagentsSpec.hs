module Agent.SubagentsSpec (spec) where

import Agent.Loop (LoopError(..), LoopResult(..), emptyTokenUsage)
import Agent.OsPath (fromFilePath)
import Agent.Subagents
import Agent.Subagents.TaskPath (taskPathRoot, taskPathText)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Monad (unless)
import Data.IORef
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "Agent.Subagents" do
    it "spawns a child, waits for completion, and returns final text" do
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ prompt _ -> pure $ Right LoopResult
                { finalResponseId = "child"
                , finalText = Just ("done:" <> prompt)
                , turnsUsed = 1
                , tokenUsage = emptyTokenUsage
                })
            (\_ _ -> pure ())
        Right agentId <- spawnSubagent registry Nothing 0 "hello" Nothing
        (statuses, timedOut) <- waitSubagents registry [agentId] 15000
        timedOut `shouldBe` False
        Map.lookup agentId statuses `shouldBe` Just (Completed (Just "done:hello"))

    it "rejects spawn past maxDepth" do
        let config = defaultSubagentConfig { maxDepth = Just 1 }
        registry <- newSubagentRegistry config (fromFilePath "/tmp")
            (\_ _ _ _ -> pure $ Right LoopResult
                { finalResponseId = "x"
                , finalText = Just "ok"
                , turnsUsed = 1
                , tokenUsage = emptyTokenUsage
                })
            (\_ _ -> pure ())
        Right child <- spawnSubagent registry Nothing 0 "one" Nothing
        _ <- waitSubagents registry [child] 15000
        result <- spawnSubagent registry (Just child) 1 "two" Nothing
        result `shouldBe` Left "Agent depth limit reached. Solve the task yourself."

    it "enforces maxConcurrent until agents are closed" do
        gate <- newTVarIO False
        let config = defaultSubagentConfig { maxConcurrent = 1 }
        registry <- newSubagentRegistry config (fromFilePath "/tmp")
            (\_ _ _ _ -> do
                atomically $ readTVar gate >>= \ready -> unless ready retry
                pure $ Right LoopResult
                    { finalResponseId = "x"
                    , finalText = Just "ok"
                    , turnsUsed = 1
                    , tokenUsage = emptyTokenUsage
                    })
            (\_ _ -> pure ())
        Right first <- spawnSubagent registry Nothing 0 "a" Nothing
        second <- spawnSubagent registry Nothing 0 "b" Nothing
        second `shouldSatisfy` \case
            Left err -> "Concurrent subagent limit" `Text.isInfixOf` err
            Right _ -> False
        atomically $ writeTVar gate True
        _ <- waitSubagents registry [first] 15000
        third <- spawnSubagent registry Nothing 0 "c" Nothing
        third `shouldSatisfy` \case
            Left err -> "Concurrent subagent limit" `Text.isInfixOf` err
            Right _ -> False
        _ <- closeSubagent registry first
        Right _ <- spawnSubagent registry Nothing 0 "d" Nothing
        pure ()

    it "supports nested spawn when depth is unlimited" do
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ _ _ -> pure $ Left LoopNoResponseId)
            (\_ _ -> pure ())
        setSubagentRunner registry \env _previous prompt _ ->
            if env.subDepth == 1
                then do
                    Right gid <- spawnSubagent registry (Just env.subId) env.subDepth "nested" Nothing
                    _ <- waitSubagents registry [gid] 15000
                    pure $ Right LoopResult
                        { finalResponseId = "child"
                        , finalText = Just ("parent-of-" <> gid.unSubagentId)
                        , turnsUsed = 1
                        , tokenUsage = emptyTokenUsage
                        }
                else
                    pure $ Right LoopResult
                        { finalResponseId = "grand"
                        , finalText = Just ("leaf:" <> prompt)
                        , turnsUsed = 1
                        , tokenUsage = emptyTokenUsage
                        }
        Right child <- spawnSubagent registry Nothing 0 "root-task" Nothing
        (statuses, timedOut) <- waitSubagents registry [child] 20000
        timedOut `shouldBe` False
        case Map.lookup child statuses of
            Just (Completed (Just text)) ->
                text `shouldSatisfy` Text.isPrefixOf "parent-of-agent-"
            other -> expectationFailure ("unexpected status: " <> show other)

    it "waitSubagents returns when any target finishes" do
        gate <- newTVarIO False
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ prompt _ -> case prompt of
                "fast" -> pure $ Right LoopResult
                    { finalResponseId = "fast"
                    , finalText = Just "done-fast"
                    , turnsUsed = 1
                    , tokenUsage = emptyTokenUsage
                    }
                _ -> do
                    atomically $ readTVar gate >>= \ready -> unless ready retry
                    pure $ Right LoopResult
                        { finalResponseId = "slow"
                        , finalText = Just "done-slow"
                        , turnsUsed = 1
                        , tokenUsage = emptyTokenUsage
                        })
            (\_ _ -> pure ())
        Right fast <- spawnSubagent registry Nothing 0 "fast" Nothing
        Right slow <- spawnSubagent registry Nothing 0 "slow" Nothing
        (statuses, timedOut) <- waitSubagents registry [fast, slow] 15000
        timedOut `shouldBe` False
        Map.lookup fast statuses `shouldBe` Just (Completed (Just "done-fast"))
        -- Slow may still be running; wait only required any final.
        Map.lookup slow statuses `shouldSatisfy` \case
            Just Running -> True
            Just Pending -> True
            Just (Completed _) -> True
            _ -> False
        atomically $ writeTVar gate True
        _ <- waitSubagents registry [slow] 15000
        pure ()

    it "passes previous response id on send_input follow-ups" do
        seen <- newIORef ([] :: [Maybe Text])
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ previous prompt _ -> do
                atomicModifyIORef' seen \xs -> (xs <> [previous], ())
                pure $ Right LoopResult
                    { finalResponseId = "resp-" <> prompt
                    , finalText = Just prompt
                    , turnsUsed = 1
                    , tokenUsage = emptyTokenUsage
                    })
            (\_ _ -> pure ())
        Right agentId <- spawnSubagent registry Nothing 0 "one" Nothing
        _ <- waitSubagents registry [agentId] 15000
        Right _ <- sendInput registry agentId "two" False
        _ <- waitSubagents registry [agentId] 15000
        history <- readIORef seen
        history `shouldBe` [Nothing, Just "resp-one"]

    it "does not leave Running when send_input admission fails" do
        gate <- newTVarIO False
        let config = defaultSubagentConfig { maxConcurrent = 2 }
        registry <- newSubagentRegistry config (fromFilePath "/tmp")
            (\_ _ prompt _ -> case prompt of
                "hold" -> do
                    atomically $ readTVar gate >>= \ready -> unless ready retry
                    pure $ Right LoopResult
                        { finalResponseId = "hold"
                        , finalText = Just "holding"
                        , turnsUsed = 1
                        , tokenUsage = emptyTokenUsage
                        }
                other -> pure $ Right LoopResult
                    { finalResponseId = "done"
                    , finalText = Just other
                    , turnsUsed = 1
                    , tokenUsage = emptyTokenUsage
                    })
            (\_ _ -> pure ())
        Right idle <- spawnSubagent registry Nothing 0 "idle" Nothing
        _ <- waitSubagents registry [idle] 15000
        Right holder <- spawnSubagent registry Nothing 0 "hold" Nothing
        -- Free idle's slot, soft-resume without a slot, then fill both slots.
        _ <- closeSubagent registry idle
        Right _ <- resumeSubagent registry idle
        Right filler <- spawnSubagent registry Nothing 0 "filler" Nothing
        _ <- waitSubagents registry [filler] 15000
        -- holder + filler occupy both slots; idle is Completed without a slot.
        result <- sendInput registry idle "follow-up" False
        result `shouldSatisfy` \case
            Left err -> "Concurrent subagent limit" `Text.isInfixOf` err
            Right _ -> False
        status <- getStatus registry idle
        status `shouldBe` Completed Nothing
        atomically $ writeTVar gate True
        _ <- waitSubagents registry [holder] 15000
        pure ()

    it "invokes onComplete when a child finishes" do
        notices <- newIORef ([] :: [(SubagentId, SubagentStatus)])
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ prompt _ -> pure $ Right LoopResult
                { finalResponseId = "c"
                , finalText = Just prompt
                , turnsUsed = 1
                , tokenUsage = emptyTokenUsage
                })
            (\_ _ -> pure ())
        setSubagentOnComplete registry \agentId status ->
            atomicModifyIORef' notices \xs -> (xs <> [(agentId, status)], ())
        Right agentId <- spawnSubagent registry Nothing 0 "notify-me" Nothing
        _ <- waitSubagents registry [agentId] 15000
        -- Allow the completion callback a moment to run after status write.
        threadDelay 50000
        seen <- readIORef notices
        seen `shouldBe` [(agentId, Completed (Just "notify-me"))]

    it "restores a missing agent id into the registry" do
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ previous prompt _ -> pure $ Right LoopResult
                { finalResponseId = fromMaybe "resp" previous
                , finalText = Just prompt
                , turnsUsed = 1
                , tokenUsage = emptyTokenUsage
                })
            (\_ _ -> pure ())
        let agentId = SubagentId "agent-restored-1"
        Right _ <- restoreSubagent registry agentId Nothing 1 Nothing (Just "prev-1")
        status <- getStatus registry agentId
        status `shouldBe` Completed Nothing
        previous <- getPreviousResponseId registry agentId
        previous `shouldBe` Just "prev-1"
        Right _ <- sendInput registry agentId "follow" False
        (statuses, timedOut) <- waitSubagents registry [agentId] 15000
        timedOut `shouldBe` False
        Map.lookup agentId statuses `shouldBe` Just (Completed (Just "follow"))

    it "reopens a closed agent via restoreSubagent" do
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ prompt _ -> pure $ Right LoopResult
                { finalResponseId = "r"
                , finalText = Just prompt
                , turnsUsed = 1
                , tokenUsage = emptyTokenUsage
                })
            (\_ _ -> pure ())
        Right agentId <- spawnSubagent registry Nothing 0 "first" Nothing
        _ <- waitSubagents registry [agentId] 15000
        _ <- closeSubagent registry agentId
        status0 <- getStatus registry agentId
        status0 `shouldBe` Closed
        Right _ <- restoreSubagent registry agentId Nothing 1 Nothing (Just "prev")
        status1 <- getStatus registry agentId
        status1 `shouldBe` Completed Nothing
        previous <- getPreviousResponseId registry agentId
        previous `shouldBe` Just "prev"

    it "spawns at a task path and resolves relative targets" do
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ prompt _ -> pure $ Right LoopResult
                { finalResponseId = "c"
                , finalText = Just prompt
                , turnsUsed = 1
                , tokenUsage = emptyTokenUsage
                })
            (\_ _ -> pure ())
        Right (agentId, path) <-
            spawnSubagentAt registry Nothing taskPathRoot 0 "worker" "do it" Nothing
        taskPathText path `shouldBe` "/root/worker"
        resolved <- resolveAgentTarget registry taskPathRoot "worker"
        resolved `shouldBe` Right agentId
        agents <- listAgents registry (Just "/root")
        map (\(p, _, _) -> taskPathText p) agents `shouldContain` ["/root/worker"]
        closeSubagentRegistry registry

    it "queueMessage does not kick an idle agent" do
        started <- newIORef (0 :: Int)
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ prompt _ -> do
                atomicModifyIORef' started \n -> (n + 1, ())
                pure $ Right LoopResult
                    { finalResponseId = "c"
                    , finalText = Just prompt
                    , turnsUsed = 1
                    , tokenUsage = emptyTokenUsage
                    })
            (\_ _ -> pure ())
        Right agentId <- spawnSubagent registry Nothing 0 "one" Nothing
        _ <- waitSubagents registry [agentId] 15000
        before <- readIORef started
        Right _ <- queueMessage registry agentId "queued-only"
        threadDelay 50000
        after <- readIORef started
        after `shouldBe` before
        status <- getStatus registry agentId
        status `shouldBe` Completed (Just "one")
