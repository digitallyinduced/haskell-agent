module Agent.Subagents.TaskPathSpec (spec) where

import Agent.Subagents.TaskPath
import Test.Hspec

spec :: Spec
spec = describe "Agent.Subagents.TaskPath" do
    it "joins and names child paths" do
        joinTaskPath taskPathRoot "worker"
            `shouldBe` Right (either (error "bad") id (parseTaskPath "/root/worker"))
        fmap taskPathName (parseTaskPath "/root/worker") `shouldBe` Right "worker"

    it "resolves relative and absolute targets" do
        let current = either (error "bad") id (parseTaskPath "/root/task1")
        resolveTaskPath current "task_3"
            `shouldBe` parseTaskPath "/root/task1/task_3"
        resolveTaskPath current "/root/other"
            `shouldBe` parseTaskPath "/root/other"

    it "rejects invalid names and paths" do
        validateTaskName "Bad" `shouldSatisfy` isLeft
        validateTaskName "a/b" `shouldSatisfy` isLeft
        parseTaskPath "/not-root" `shouldSatisfy` isLeft
        parseTaskPath "/root//worker" `shouldSatisfy` isLeft
        parseTaskPath "/root/worker/" `shouldSatisfy` isLeft
  where
    isLeft = either (const True) (const False)
