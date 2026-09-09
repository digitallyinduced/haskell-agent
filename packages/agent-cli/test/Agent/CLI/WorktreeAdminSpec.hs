module Agent.CLI.WorktreeAdminSpec (spec) where

import Agent.CLI.Worktree (WorktreeCleanupReport(..))
import Agent.CLI.WorktreeAdmin (renderWorktreeCleanupReport)
import qualified Data.Text as Text
import System.OsPath (unsafeEncodeUtf)
import Test.Hspec

spec :: Spec
spec = describe "worktree cleanup report" do
    it "shows eligibility, estimates, evidence, retained reasons, failures and unexamined candidates" do
        let report = mempty
                { cleanupRemoved = []
                , cleanupFailures = [(unsafeEncodeUtf "/broken", "snapshot verification failed")]
                , cleanupEligible = [(unsafeEncodeUtf "/ready", 1234)]
                , cleanupRetained = [(unsafeEncodeUtf "/legacy", "unknown saved-session activity")]
                , cleanupNotExamined = [(unsafeEncodeUtf "/pending", "pass budget exhausted")]
                , cleanupEvidence = [(unsafeEncodeUtf "/ready", "refs/heads/release")]
                }
            rendered = renderWorktreeCleanupReport True 14 report
        mapM_ (\part -> rendered `shouldSatisfy` Text.isInfixOf part)
            [ "Dry run"
            , "minimum inactivity: 14 days"
            , "never expire solely because of age"
            , "explicitly ignored build/cache directories"
            , "estimated bytes: 1234"
            , "unknown saved-session activity"
            , "Eligible checkout gross apparent bytes: 1234"
            , "snapshot verification failed"
            , "refs/heads/release"
            , "pass budget exhausted"
            , "1 eligible"
            , "0 collected"
            , "1 retained"
            , "1 failed"
            , "1 not examined"
            ]
    it "escapes newlines in filenames" do
        let report = mempty { cleanupRetained = [(unsafeEncodeUtf "/line\nbreak", "protected")] }
            rendered = renderWorktreeCleanupReport True 7 report
        rendered `shouldSatisfy` Text.isInfixOf "/line\\nbreak"
        rendered `shouldSatisfy` (not . Text.isInfixOf "/line\nbreak")
    it "sums only eligible estimates and does not describe estimates as recovered space" do
        let report = mempty
                { cleanupEligible = [(unsafeEncodeUtf "/one", 1234), (unsafeEncodeUtf "/two", 4321)]
                , cleanupRetained = [(unsafeEncodeUtf "/protected", "protected")]
                }
            rendered = renderWorktreeCleanupReport True 7 report
        rendered `shouldSatisfy` Text.isInfixOf "Eligible checkout gross apparent bytes: 5555"
        rendered `shouldSatisfy` Text.isInfixOf "not guaranteed net disk savings"
        rendered `shouldSatisfy` Text.isInfixOf "2 eligible, 0 collected, 1 retained, 0 failed"
