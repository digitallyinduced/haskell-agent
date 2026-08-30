module Agent.CLI.MacOS.TaskSchedulerSpec (spec) where

import Agent.CLI.MacOS.TaskScheduler
    ( TaskIdentity(..)
    , selectRunnableTasks
    )
import qualified Data.Set as Set
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )

spec :: Spec
spec = describe "native task scheduling" do
    it "starts distinct sessions up to the available capacity" do
        let queued =
                [ task "first" (Just "session-a")
                , task "second" (Just "session-b")
                , task "third" (Just "session-c")
                ]
            (selected, remaining) =
                selectRunnableTasks 2 Set.empty queued
        map (identityId . fst) selected
            `shouldBe` ["first", "second"]
        map (identityId . fst) remaining
            `shouldBe` ["third"]

    it "keeps a same-session task queued while starting unrelated work" do
        let queued =
                [ task "blocked" (Just "session-a")
                , task "runnable" (Just "session-b")
                ]
            (selected, remaining) =
                selectRunnableTasks 2 (Set.singleton "session-a") queued
        map (identityId . fst) selected
            `shouldBe` ["runnable"]
        map (identityId . fst) remaining
            `shouldBe` ["blocked"]

    it "does not overtake an earlier task for the same queued session" do
        let queued =
                [ task "first-a" (Just "session-a")
                , task "second-a" (Just "session-a")
                , task "first-b" (Just "session-b")
                ]
            (selected, remaining) =
                selectRunnableTasks 3 Set.empty queued
        map (identityId . fst) selected
            `shouldBe` ["first-a", "first-b"]
        map (identityId . fst) remaining
            `shouldBe` ["second-a"]

    it "treats fresh sessions as independent tasks" do
        let queued = [task "first" Nothing, task "second" Nothing]
            (selected, remaining) =
                selectRunnableTasks 2 Set.empty queued
        map (identityId . fst) selected
            `shouldBe` ["first", "second"]
        remaining `shouldBe` []
  where
    task taskId sessionId =
        ( TaskIdentity
            { taskIdentityId = taskId
            , taskIdentitySessionId = sessionId
            }
        , ()
        )

    identityId (TaskIdentity taskId _) = taskId
