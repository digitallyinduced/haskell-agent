module Agent.Tools.PlanModeSpec (spec) where

import Agent.Tools.PlanMode
import Test.Hspec
import qualified Data.Text as Text
import System.Directory (getTemporaryDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.Posix.Temp (mkdtemp)
import Control.Exception.Safe (bracket)

spec :: Spec
spec = describe "Agent.Tools.PlanMode" do
    it "activates and deactivates plan mode" do
        withTempPlan \env -> do
            isPlanModeActive env `shouldReturn` False
            activatePlanMode env
            isPlanModeActive env `shouldReturn` True
            deactivatePlanMode env
            isPlanModeActive env `shouldReturn` False

    it "writes and reads plan.md under the fallback directory" do
        withTempPlan \env -> do
            writePlanMarkdown env "# Hello\n" `shouldReturn` Right ()
            content <- readPlanMarkdown env
            content `shouldBe` "# Hello\n"
            path <- planFilePath env
            path `shouldSatisfy` Text.isSuffixOf "plan.md" . Text.pack

    it "recognizes plan.md edit targets" do
        isPlanFileEditTarget "/tmp/sess/plan.md" "/tmp/sess/plan.md" `shouldBe` True
        isPlanFileEditTarget "/tmp/sess/plan.md" "plan.md" `shouldBe` True
        isPlanFileEditTarget "/tmp/sess/plan.md" "/tmp/sess/other.hs" `shouldBe` False

withTempPlan :: (PlanModeEnv -> IO a) -> IO a
withTempPlan action = do
    tmp <- getTemporaryDirectory
    bracket
        (mkdtemp (tmp </> "agent-plan-XXXXXX"))
        removeDirectoryRecursive
        (\dir -> newPlanModeEnv dir Nothing >>= action)
