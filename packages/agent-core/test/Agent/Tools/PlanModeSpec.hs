module Agent.Tools.PlanModeSpec (spec) where

import Agent.OsPath (toText)
import System.OsPath (unsafeEncodeUtf)
import Agent.Tools.PlanMode
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRule(..)
    )
import Test.Hspec
import qualified Data.Text as Text
import System.Directory (getTemporaryDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.Posix.Temp (mkdtemp)
import Control.Exception.Safe (bracket)

fromFilePath = unsafeEncodeUtf

spec :: Spec
spec = describe "Agent.Tools.PlanMode" do
    it "uses its dedicated confirmation instead of generic tool approval" do
        withTempPlan \env ->
            case (enterPlanModeTool env).appToolApproval of
                AlwaysReadOnly -> pure ()
                _ -> expectationFailure "enter_plan_mode should be read-only"

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
            path `shouldSatisfy` Text.isSuffixOf "plan.md" . toText

    it "recognizes plan.md edit targets" do
        isPlanFileEditTarget (fromFilePath "/tmp/sess/plan.md")
            (fromFilePath "/tmp/sess/plan.md") `shouldBe` True
        isPlanFileEditTarget (fromFilePath "/tmp/sess/plan.md")
            (fromFilePath "plan.md") `shouldBe` True
        isPlanFileEditTarget (fromFilePath "/tmp/sess/plan.md")
            (fromFilePath "/tmp/other/plan.md") `shouldBe` False
        isPlanFileEditTarget (fromFilePath "/tmp/sess/plan.md")
            (fromFilePath "/tmp/sess/other.hs") `shouldBe` False

withTempPlan :: (PlanModeEnv -> IO a) -> IO a
withTempPlan action = do
    tmp <- getTemporaryDirectory
    bracket
        (mkdtemp (tmp </> "agent-plan-XXXXXX"))
        removeDirectoryRecursive
        (\dir -> newPlanModeEnv (fromFilePath dir) Nothing >>= action)
