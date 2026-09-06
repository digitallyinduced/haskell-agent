module Agent.CLI.WorktreeAdminSpec (spec) where

import Agent.CLI.Worktree (WorktreeCleanupReport(..))
import Agent.CLI.WorktreeAdmin (renderWorktreeCleanupReport)
import qualified Data.Text as Text
import System.OsPath (unsafeEncodeUtf)
import Test.Hspec

spec :: Spec
spec = describe "worktree cleanup report" do
    it "shows eligibility, estimates, retained reasons, failures and the ignored-file warning" do
        let report = WorktreeCleanupReport
                { cleanupRemoved = []
                , cleanupFailures = [(unsafeEncodeUtf "/broken", "snapshot verification failed")]
                , cleanupEligible = [(unsafeEncodeUtf "/ready", 1234)]
                , cleanupRetained = [(unsafeEncodeUtf "/legacy", "unknown saved-session activity")]
                }
            rendered = renderWorktreeCleanupReport True 14 report
        mapM_ (\part -> rendered `shouldSatisfy` Text.isInfixOf part)
            [ "Dry run"
            , "14 days"
            , "estimated bytes: 1234"
            , "unknown saved-session activity"
            , "Automatic adoption is simulated; no registry or snapshot is written."
            , "Eligible checkout gross apparent bytes: 1234"
            , "snapshot verification failed"
            , "Ignored untracked files are NOT backed up or restored."
            , "1 eligible, 0 collected, 1 retained, 1 failed."
            ]
    it "escapes newlines in filenames" do
        let report = mempty { cleanupRetained = [(unsafeEncodeUtf "/line\nbreak", "protected")] }
            rendered = renderWorktreeCleanupReport True 7 report
        rendered `shouldSatisfy` Text.isInfixOf "/line\\nbreak"
        length (Text.lines rendered) `shouldBe` 6
    it "sums only eligible estimates and does not describe estimates as recovered space" do
        let report = mempty
                { cleanupEligible = [(unsafeEncodeUtf "/one", 1234), (unsafeEncodeUtf "/two", 4321)]
                , cleanupRetained = [(unsafeEncodeUtf "/protected", "protected")]
                }
            rendered = renderWorktreeCleanupReport True 7 report
        rendered `shouldSatisfy` Text.isInfixOf "Eligible checkout gross apparent bytes: 5555"
        rendered `shouldSatisfy` Text.isInfixOf "not guaranteed net disk savings"
        rendered `shouldSatisfy` Text.isInfixOf "2 eligible, 0 collected, 1 retained, 0 failed."
