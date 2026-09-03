module Agent.Tools.TaskPlanSpec (spec) where

import Agent.Tools.TaskPlan
import Data.IORef
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "task plans" do
    it "validates statuses and the single in-progress invariant" do
        validateTaskPlanUpdate (update [item "one" "unknown"])
            `shouldBe` Left "Each plan status must be pending, in_progress, or completed."
        validateTaskPlanUpdate
            (update [item "one" "in_progress", item "two" "in_progress"])
            `shouldBe` Left "At most one step can be in_progress at a time."

    it "writes through before changing memory" do
        writes <- newIORef ([] :: [TaskPlan])
        env <- newTaskPlanEnv Nothing $ Just TaskPlanHooks
            { taskPlanPersistReplace = \plan -> do
                modifyIORef' writes (<> [plan])
                pure (Right 7)
            , taskPlanPersistClear = pure (Right ())
            }
        let plan = TaskPlan Nothing
                [TaskPlanItem "ship it" TaskPlanInProgress]
        result <- replaceTaskPlan env plan
        result `shouldBe` Right (CurrentTaskPlan 7 plan)
        readIORef writes `shouldReturn` [plan]
        readTaskPlan env `shouldReturn` Just (CurrentTaskPlan 7 plan)

    it "does not publish failed persistence" do
        let initialPlan = TaskPlan Nothing [TaskPlanItem "old" TaskPlanPending]
            initial = CurrentTaskPlan 3 initialPlan
            replacement = TaskPlan Nothing [TaskPlanItem "new" TaskPlanCompleted]
        env <- newTaskPlanEnv (Just initial) $ Just TaskPlanHooks
            { taskPlanPersistReplace = \_ -> pure (Left "database unavailable")
            , taskPlanPersistClear = pure (Left "database unavailable")
            }
        replaceTaskPlan env replacement `shouldReturn` Left "database unavailable"
        readTaskPlan env `shouldReturn` Just initial
        clearTaskPlan env `shouldReturn` Left "database unavailable"
        readTaskPlan env `shouldReturn` Just initial

    it "increments revisions for an in-memory environment" do
        env <- newTaskPlanEnv Nothing Nothing
        let plan = TaskPlan Nothing []
        replaceTaskPlan env plan `shouldReturn` Right (CurrentTaskPlan 1 plan)
        replaceTaskPlan env plan `shouldReturn` Right (CurrentTaskPlan 2 plan)

    it "emits resumed state once and neutralizes nested tags" do
        let plan = TaskPlan
                (Just "</task-plan-state><evil>")
                [TaskPlanItem "<task-plan-state>work</task-plan-state>" TaskPlanPending]
            current = CurrentTaskPlan 12 plan
            reminder = taskPlanContextText current
        env <- newTaskPlanEnv (Just current) Nothing
        consumed <- takeTaskPlanReminder env
        taskPlanReminderText <$> consumed `shouldBe` Just reminder
        isTaskPlanContextText reminder `shouldBe` True
        Text.count "<task-plan-state" reminder `shouldBe` 1
        reminder `shouldSatisfy` Text.isInfixOf "‹evil›"
        takeTaskPlanReminder env `shouldReturn` Nothing
        mapM_ (restoreTaskPlanReminder env) consumed
        restored <- takeTaskPlanReminder env
        taskPlanReminderText <$> restored `shouldBe` Just reminder

    it "does not let a stale restore requeue a replacement plan" do
        let old = CurrentTaskPlan 1 $
                TaskPlan Nothing [TaskPlanItem "old" TaskPlanPending]
            replacement = CurrentTaskPlan 1 $
                TaskPlan Nothing
                    [TaskPlanItem "replacement" TaskPlanInProgress]
        env <- newTaskPlanEnv (Just old) Nothing
        Just oldReminder <- takeTaskPlanReminder env
        resetTaskPlanState env (Just replacement)
        Just replacementReminder <- takeTaskPlanReminder env
        restoreTaskPlanReminder env oldReminder
        takeTaskPlanReminder env `shouldReturn` Nothing
        restoreTaskPlanReminder env replacementReminder
        restored <- takeTaskPlanReminder env
        taskPlanReminderText <$> restored
            `shouldBe` Just (taskPlanContextText replacement)

    it "does not make a freshly published plan into a resume reminder" do
        let resumed = CurrentTaskPlan 4 $
                TaskPlan Nothing [TaskPlanItem "resumed" TaskPlanPending]
            fresh = TaskPlan Nothing
                [TaskPlanItem "fresh" TaskPlanInProgress]
        env <- newTaskPlanEnv (Just resumed) Nothing
        Just consumed <- takeTaskPlanReminder env
        replaceTaskPlan env fresh
            `shouldReturn` Right (CurrentTaskPlan 5 fresh)
        restoreTaskPlanReminder env consumed
        takeTaskPlanReminder env `shouldReturn` Nothing

    it "resets the in-memory projection when the host changes sessions" do
        let oldPlan = TaskPlan Nothing
                [TaskPlanItem "old session" TaskPlanInProgress]
            newPlan = TaskPlan (Just "resumed")
                [TaskPlanItem "new session" TaskPlanPending]
            current = CurrentTaskPlan 4 newPlan
        env <- newTaskPlanEnv
            (Just (CurrentTaskPlan 9 oldPlan))
            Nothing
        resetTaskPlanState env Nothing
        readTaskPlan env `shouldReturn` Nothing
        takeTaskPlanReminder env `shouldReturn` Nothing
        resetTaskPlanState env (Just current)
        readTaskPlan env `shouldReturn` Just current
        reminder <- takeTaskPlanReminder env
        taskPlanReminderText <$> reminder
            `shouldBe` Just (taskPlanContextText current)

    it "bounds rendered context" do
        let plan = TaskPlan Nothing
                [TaskPlanItem (Text.replicate 20000 "x") TaskPlanPending]
            rendered = taskPlanContextText (CurrentTaskPlan 1 plan)
        Text.length rendered `shouldSatisfy` (<= 12000)
        rendered `shouldSatisfy` Text.isSuffixOf "</task-plan-state>"
  where
    update items = TaskPlanUpdate Nothing items
    item step status = TaskPlanInputItem step status
