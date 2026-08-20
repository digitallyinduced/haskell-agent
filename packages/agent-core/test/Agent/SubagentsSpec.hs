module Agent.SubagentsSpec (spec) where

import Agent.Loop (LoopError(..), LoopResult(..))
import Agent.Subagents
import Control.Concurrent.STM
import Control.Monad (unless)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "Agent.Subagents" do
    it "spawns a child, waits for completion, and returns final text" do
        registry <- newSubagentRegistry defaultSubagentConfig "/tmp"
            (\_ prompt _ -> pure $ Right LoopResult
                { finalResponseId = "child"
                , finalText = Just ("done:" <> prompt)
                , turnsUsed = 1
                })
            (\_ _ -> pure ())
        Right agentId <- spawnSubagent registry Nothing 0 "hello" Nothing
        (statuses, timedOut) <- waitSubagents registry [agentId] 15000
        timedOut `shouldBe` False
        Map.lookup agentId statuses `shouldBe` Just (Completed (Just "done:hello"))

    it "rejects spawn past maxDepth" do
        let config = defaultSubagentConfig { maxDepth = Just 1 }
        registry <- newSubagentRegistry config "/tmp"
            (\_ _ _ -> pure $ Right LoopResult
                { finalResponseId = "x"
                , finalText = Just "ok"
                , turnsUsed = 1
                })
            (\_ _ -> pure ())
        Right child <- spawnSubagent registry Nothing 0 "one" Nothing
        _ <- waitSubagents registry [child] 15000
        result <- spawnSubagent registry (Just child) 1 "two" Nothing
        result `shouldBe` Left "Agent depth limit reached. Solve the task yourself."

    it "enforces maxConcurrent until agents are closed" do
        gate <- newTVarIO False
        let config = defaultSubagentConfig { maxConcurrent = 1 }
        registry <- newSubagentRegistry config "/tmp"
            (\_ _ _ -> do
                atomically $ readTVar gate >>= \ready -> unless ready retry
                pure $ Right LoopResult
                    { finalResponseId = "x"
                    , finalText = Just "ok"
                    , turnsUsed = 1
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
        registry <- newSubagentRegistry defaultSubagentConfig "/tmp"
            (\_ _ _ -> pure $ Left LoopNoResponseId)
            (\_ _ -> pure ())
        setSubagentRunner registry \env prompt _ ->
            if env.subDepth == 1
                then do
                    Right gid <- spawnSubagent registry (Just env.subId) env.subDepth "nested" Nothing
                    _ <- waitSubagents registry [gid] 15000
                    pure $ Right LoopResult
                        { finalResponseId = "child"
                        , finalText = Just ("parent-of-" <> gid.unSubagentId)
                        , turnsUsed = 1
                        }
                else
                    pure $ Right LoopResult
                        { finalResponseId = "grand"
                        , finalText = Just ("leaf:" <> prompt)
                        , turnsUsed = 1
                        }
        Right child <- spawnSubagent registry Nothing 0 "root-task" Nothing
        (statuses, timedOut) <- waitSubagents registry [child] 20000
        timedOut `shouldBe` False
        case Map.lookup child statuses of
            Just (Completed (Just text)) ->
                text `shouldSatisfy` Text.isPrefixOf "parent-of-agent-"
            other -> expectationFailure ("unexpected status: " <> show other)
