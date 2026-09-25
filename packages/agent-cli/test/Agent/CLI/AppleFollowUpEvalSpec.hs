-- | Live quality check for on-device steer-or-queue classification.
-- Absent unless HASKELL_AGENT_APPLE_FOLLOW_UP_EVAL=1, so the default suite
-- does not call Apple Intelligence.
module Agent.CLI.AppleFollowUpEvalSpec (spec) where

import Agent.CLI.AppleFollowUp
    ( AppleFollowUpTiming(..)
    , FollowUpRoute(..)
    , classifyFollowUps
    , defaultAppleFollowUpTiming
    )
import Agent.CLI.AppleTitle (probeAppleFoundationTitle)
import Control.Monad (unless, when)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified System.Directory as Directory
import System.Environment (getEnv, lookupEnv)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import qualified System.Info
import Test.Hspec

spec :: Spec
spec = do
    enabled <- runIO (lookupEnv "HASKELL_AGENT_APPLE_FOLLOW_UP_EVAL")
    when (enabled == Just "1") $
        describe "AppleFollowUpEval" $
            it "classifies corrections as steer and later tasks as queue" runEvaluation

data FollowUpExample = FollowUpExample
    { exampleName :: !Text
    , exampleTask :: !Text
    , exampleMessage :: !Text
    , exampleExpected :: !FollowUpRoute
    }

runEvaluation :: IO ()
runEvaluation = do
    executable <- probeAppleFoundationTitle >>= \case
        Just path -> pure path
        Nothing -> do
            cwd <- Directory.getCurrentDirectory
            xcode <- Directory.doesDirectoryExist
                "/Applications/Xcode.app/Contents/Developer"
            stop $
                Text.unlines
                    [ "Apple Intelligence follow-up helper is not available"
                    , "os=" <> Text.pack System.Info.os
                    , "cwd=" <> Text.pack cwd
                    , "xcode=" <> Text.pack (show xcode)
                    , "expected HASKELL_AGENT_APPLE_SESSION_TITLE or apple-session-title on PATH"
                    ]
    result <-
        classifyFollowUps
            evaluationTiming
            executable
            [ (example.exampleTask, example.exampleMessage)
            | example <- evaluationExamples
            ]
    routes <- either stop pure result
    let rows =
            zipWith
                (\example route -> (example, route, route == example.exampleExpected))
                evaluationExamples
                routes
        misses =
            [ row
            | row@(_, _, correct) <- rows
            , not correct
            ]
        newMisses =
            [ example.exampleName
            | (example, _, False) <- misses
            , example.exampleName `Set.notMember` knownMisses
            ]
        fixed =
            [ name
            | name <- Set.toList knownMisses
            , any
                (\(example, _, correct) ->
                    correct && example.exampleName == name)
                rows
            ]
        report =
            formatEvaluation rows
                <> "\nnew misses: "
                <> Text.intercalate ", " newMisses
                <> "\nfixed known misses: "
                <> Text.intercalate ", " fixed
    directory <- getEnv "TMPDIR"
    Text.writeFile (directory </> "apple-follow-up-eval.txt") report
    hPutStrLn stderr (Text.unpack report)
    unless (null newMisses && null fixed) $
        stop report

stop :: Text -> IO a
stop message = do
    expectationFailure (Text.unpack message)
    error "expectationFailure returned"

-- | Wrong answers measured for the current prompt. These are not accepted
-- behavior. Remove a name when the model starts classifying it correctly.
-- A miss that is not in this list fails the evaluation.
knownMisses :: Set.Set Text
knownMisses =
    Set.fromList
        [ "separate-module-rename"
        , "file-flaky-test-issue"
        ]

evaluationTiming :: AppleFollowUpTiming
evaluationTiming =
    defaultAppleFollowUpTiming
        { appleFollowUpReadyTimeoutMicros = 30_000_000
        , appleFollowUpRequestTimeoutMicros = 8_000_000
        }

formatEvaluation :: [(FollowUpExample, FollowUpRoute, Bool)] -> Text
formatEvaluation rows =
    let total = length rows
        correct = length [ () | (_, _, True) <- rows ]
        header =
            "on-device follow-up classification: "
                <> Text.pack (show correct)
                <> "/"
                <> Text.pack (show total)
        lines_ =
            header
                : [ mark
                    <> " "
                    <> example.exampleName
                    <> " expected "
                    <> routeName example.exampleExpected
                    <> " got "
                    <> routeName route
                  | (example, route, matched) <- rows
                  , let mark = if matched then "ok  " else "MISS"
                  ]
    in Text.unlines lines_

routeName :: FollowUpRoute -> Text
routeName = \case
    FollowUpSteer -> "steer"
    FollowUpQueue -> "queue"

evaluationExamples :: [FollowUpExample]
evaluationExamples =
    [ steer
        "keep-names"
        "Add validation for empty project names and a regression test."
        "Keep the existing public function names."
    , steer
        "whitespace-detail"
        "Fix the auth token refresh race in the session runner."
        "Also handle the whitespace-only case."
    , steer
        "redirect-package"
        "Rewrite the queue parser in the TUI package."
        "Wait, do that in the CLI package, not the TUI."
    , steer
        "no-public-api"
        "Implement follow-up routing and update the docs."
        "Don't change the public API."
    , steer
        "existing-helpers"
        "Add validation for empty project names."
        "Use the existing assertion helpers."
    , steer
        "stop-unrelated"
        "Add validation for empty project names."
        "Stop editing unrelated files."
    , steer
        "text-instead-of-string"
        "Replace the path conversions in the session store."
        "Use Text instead of String."
    , steer
        "leave-tests"
        "Add validation for empty project names."
        "Hold on, leave the tests alone."
    , steer
        "different-module"
        "Move session title generation into the runtime package."
        "Put this in Agent.CLI.Session instead."
    , steer
        "existing-error-wording"
        "Add validation for empty project names."
        "Match the existing error wording."
    , steer
        "no-schema-change"
        "Add a column for the session title."
        "Leave the database schema alone."
    , steer
        "stale-fixture"
        "Fix the failing session title test."
        "The expected title in the fixture is stale."
    , steer
        "empty-string-too"
        "Add validation for empty project names."
        "Handle the empty string too."
    , steer
        "only-the-parser"
        "Rewrite the queue parser in the TUI package."
        "Only change the parser."
    , steer
        "next-free-port"
        "Make the preview server bind a local port."
        "Use the next free port."
    , steer
        "when-name-empty"
        "Add validation for empty project names."
        "When the name is empty, return the existing error."
    , steer
        "after-parsing-step"
        "Add validation for empty project names."
        "After parsing, reject an empty name."
    , steer
        "do-not-wait-until-end"
        "Add validation for empty project names."
        "Don't wait until the end to check the empty input."
    , steer
        "then-keep-comment"
        "Extend the queue parser so it accepts comments."
        "Then the parser should keep the comment text."
    , steer
        "add-regression-test"
        "Add validation for empty project names."
        "Add a regression test."
    , steer
        "status-question"
        "Add validation for empty project names and a regression test."
        "What is still left in this change?"
    , queue
        "after-tests-review-docs"
        "Add validation for empty project names and a regression test."
        "After the tests finish, review the documentation changes."
    , queue
        "next-open-pr"
        "Fix the auth token refresh race in the session runner."
        "Next, open a pull request."
    , queue
        "blog-post"
        "Fix the auth token refresh race in the session runner."
        "Write a short blog post about this bug."
    , queue
        "when-done-full-tests"
        "Rename the exported session types."
        "When this is done, run the full test suite."
    , queue
        "then-changelog"
        "Rename the exported session types."
        "Then update the changelog."
    , queue
        "after-you-finish-ci"
        "Fix the auth token refresh race."
        "Please look at the failing CI job after you finish."
    , queue
        "afterward-bump-version"
        "Move session title generation into the runtime package."
        "Afterward, bump the version."
    , queue
        "once-this-lands-release-notes"
        "Fix the auth token refresh race in the session runner."
        "Once this lands, write the release notes."
    , queue
        "later-screenshot"
        "Add validation for empty project names."
        "Later, add a screenshot to the docs."
    , queue
        "separate-module-rename"
        "Fix the auth token refresh race in the session runner."
        "Start a separate change to rename the modules."
    , queue
        "file-flaky-test-issue"
        "Fix the auth token refresh race in the session runner."
        "File an issue about the flaky test in the other package."
    , queue
        "update-readme"
        "Fix the auth token refresh race in the session runner."
        "Update the README examples."
    , queue
        "also-after-finish-changelog"
        "Rename the exported session types."
        "Also, after you finish, update the changelog."
    , queue
        "bare-open-pr"
        "Fix the auth token refresh race in the session runner."
        "Open a pull request."
    , queue
        "windows-build-next"
        "Fix the auth token refresh race in the session runner."
        "Look at the failing Windows build next."
    ]
  where
    steer name task message =
        FollowUpExample name task message FollowUpSteer
    queue name task message =
        FollowUpExample name task message FollowUpQueue
