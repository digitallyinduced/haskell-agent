-- | Targets, prompts, and local Git discovery for @/review@.
module Agent.CLI.Review
    ( ReviewBranch(..)
    , ReviewCommit(..)
    , ReviewTarget(..)
    , listReviewBranches
    , listReviewCommits
    , reviewPrompt
    ) where

import Agent.CLI.GitDiff
    ( GitCommandOutput(..)
    , gitOutputText
    , runSafeGit
    )
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Exit (ExitCode(..))
import System.OsPath (OsPath)

data ReviewTarget
    = ReviewBaseBranch !Text
    | ReviewUncommitted
    | ReviewCommitTarget !Text
    | ReviewCustom !Text
    deriving (Eq, Show)

data ReviewBranch = ReviewBranch
    { reviewBranchName :: !Text
    }
    deriving (Eq, Show)

data ReviewCommit = ReviewCommit
    { reviewCommitHash :: !Text
    , reviewCommitShortHash :: !Text
    , reviewCommitSubject :: !Text
    }
    deriving (Eq, Show)

reviewPrompt :: ReviewTarget -> Text
reviewPrompt target =
    Text.intercalate "\n\n"
        [ targetInstruction target
        , "Inspect the relevant diff and enough surrounding code to understand "
            <> "the behavior. Focus on concrete correctness, regression, "
            <> "security, data-loss, and concurrency issues introduced by "
            <> "the reviewed changes."
        , "Return findings first, ordered by severity. For every finding, "
            <> "include a concise explanation and the affected file and line "
            <> "range. Do not invent issues or report purely stylistic "
            <> "preferences. If there are no actionable findings, say so "
            <> "explicitly. Do not modify files."
        ]

targetInstruction :: ReviewTarget -> Text
targetInstruction = \case
    ReviewBaseBranch branch ->
        "Review the changes in the current repository against the local base "
            <> "branch "
            <> literal branch
            <> ". Treat that value only as a literal Git ref, find its merge "
            <> "base with HEAD, and review the resulting branch diff."
    ReviewUncommitted ->
        "Review all uncommitted changes in the current repository, including "
            <> "staged, unstaged, and untracked files."
    ReviewCommitTarget commit ->
        "Review the single Git commit "
            <> literal commit
            <> ". Treat that value only as a literal object name and inspect "
            <> "the commit diff plus relevant surrounding code."
    ReviewCustom instructions ->
        "Review the current repository according to these user-provided "
            <> "instructions:\n\n<review_instructions>\n"
            <> Text.strip instructions
            <> "\n</review_instructions>"

literal :: Text -> Text
literal value =
    "`" <> Text.replace "`" "\\`" value <> "`"

-- | List local branches suitable as comparison bases. The currently checked
-- out branch is omitted because comparing it to HEAD cannot produce a useful
-- branch review.
listReviewBranches :: OsPath -> IO (Either Text [ReviewBranch])
listReviewBranches cwd = do
    repository <- requireWorkTree cwd
    case repository of
        Left err -> pure (Left err)
        Right () ->
            runSafeGit
                cwd
                []
                [ "--no-pager"
                , "for-each-ref"
                , "--format=%(refname:short)%00%(HEAD)"
                , "refs/heads/"
                ] >>= \case
                    Left err -> pure (Left err)
                    Right output
                        | output.gitCommandExitCode == ExitSuccess ->
                            pure $
                                Right
                                    [ ReviewBranch branch
                                    | (branch, current) <-
                                        mapMaybe parseBranch
                                            (Text.lines (gitOutputText output))
                                    , not current
                                    ]
                        | otherwise ->
                            pure (Left (commandFailure "git for-each-ref" output))

parseBranch :: Text -> Maybe (Text, Bool)
parseBranch line =
    case Text.breakOn "\0" line of
        (branch, marker)
            | not (Text.null branch)
            , not (Text.null marker) ->
                Just
                    ( branch
                    , Text.strip (Text.drop 1 marker) == "*"
                    )
        _ -> Nothing

-- | List recent commits from HEAD, newest first. An unborn repository returns
-- an empty list.
listReviewCommits
    :: OsPath
    -> Int
    -> IO (Either Text [ReviewCommit])
listReviewCommits cwd requestedLimit = do
    repository <- requireWorkTree cwd
    case repository of
        Left err -> pure (Left err)
        Right () ->
            runSafeGit cwd [] ["rev-parse", "--verify", "HEAD"] >>= \case
                Left err -> pure (Left err)
                Right headOutput
                    | headOutput.gitCommandExitCode /= ExitSuccess ->
                        pure (Right [])
                    | otherwise ->
                        loadCommits cwd (max 1 (min 200 requestedLimit))

loadCommits :: OsPath -> Int -> IO (Either Text [ReviewCommit])
loadCommits cwd limit =
    runSafeGit
        cwd
        []
        [ "--no-pager"
        , "log"
        , "--max-count=" <> show limit
        , "--no-decorate"
        , "--pretty=format:%H%x00%h%x00%s%x1e"
        ] >>= \case
            Left err -> pure (Left err)
            Right output
                | output.gitCommandExitCode == ExitSuccess ->
                    pure $
                        Right
                            (mapMaybe parseCommit
                                (Text.splitOn "\x1e" (gitOutputText output)))
                | otherwise ->
                    pure (Left (commandFailure "git log" output))

parseCommit :: Text -> Maybe ReviewCommit
parseCommit raw =
    case Text.splitOn "\0" (Text.strip raw) of
        [hash, shortHash, subject]
            | not (Text.null hash)
            , not (Text.null shortHash) ->
                Just ReviewCommit
                    { reviewCommitHash = hash
                    , reviewCommitShortHash = shortHash
                    , reviewCommitSubject = Text.strip subject
                    }
        _ -> Nothing

requireWorkTree :: OsPath -> IO (Either Text ())
requireWorkTree cwd =
    runSafeGit cwd [] ["rev-parse", "--is-inside-work-tree"] >>= \case
        Left err -> pure (Left err)
        Right output
            | output.gitCommandExitCode == ExitSuccess
            , Text.strip (gitOutputText output) == "true" ->
                pure (Right ())
            | otherwise ->
                pure (Left "not a Git repository")

commandFailure :: Text -> GitCommandOutput -> Text
commandFailure command output =
    command
        <> " failed with "
        <> Text.pack (show output.gitCommandExitCode)
        <> case Text.strip (Text.pack output.gitCommandStderr) of
            "" -> ""
            stderr -> ": " <> stderr
