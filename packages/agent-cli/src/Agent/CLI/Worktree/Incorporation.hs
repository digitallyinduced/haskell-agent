-- | Read-only, fail-closed evidence that another branch incorporated HEAD.
-- Merely publishing the current branch is not incorporation. GitHub's merged
-- flag is likewise insufficient without an exact head and local content proof.
module Agent.CLI.Worktree.Incorporation
    ( inspectIncorporation
    , inspectIncorporationWith
    , MergedPullRequest(..)
    ) where

import Agent.OsPath (unsafeToFilePath)
import Control.Exception.Safe (tryAny, displayException)
import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE)
import Data.Aeson (FromJSON(..), eitherDecodeStrict', withObject, (.:), (.:?))
import Data.Aeson.Types (Parser)
import Data.Char (isAscii, isAlphaNum, isHexDigit)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.Directory (doesPathExist)
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.OsPath (OsPath)
import System.Process (CreateProcess(..), proc, readCreateProcessWithExitCode)
import System.Timeout (timeout)

data MergedPullRequest = MergedPullRequest
    { pullRequestHead :: !Text
    , pullRequestMerge :: !Text
    , pullRequestBase :: !Text
    } deriving (Eq, Show)

newtype PullRequestResponse = PullRequestResponse (Maybe MergedPullRequest)

instance FromJSON PullRequestResponse where
    parseJSON = withObject "pull request" $ \object -> do
        mergedAt <- object .:? "merged_at" :: Parser (Maybe Text)
        mergeCommit <- object .:? "merge_commit_sha"
        headObject <- object .: "head"
        baseObject <- object .: "base"
        headCommit <- headObject .: "sha"
        baseBranch <- baseObject .: "ref"
        pure $ PullRequestResponse $
            case (mergedAt, mergeCommit) of
                (Just _, Just mergeOid) ->
                    Just (MergedPullRequest headCommit mergeOid baseBranch)
                _ -> Nothing

inspectIncorporation :: OsPath -> IO (Either Text Text)
inspectIncorporation path = inspectIncorporationWith (loadMergedPullRequests path) path

-- | The injected reader is used only after local ancestry fails. Production
-- uses the authenticated GitHub API; tests provide authoritative fixtures.
inspectIncorporationWith
    :: (Text -> IO (Either Text [MergedPullRequest]))
    -> OsPath -> IO (Either Text Text)
inspectIncorporationWith loadPullRequests path = do
    result <- tryAny $ runExceptT do
        shallow <- git ["rev-parse", "--is-shallow-repository"]
        unless (shallow == "false") (throwE "shallow history cannot prove incorporation")
        graftPath <- git ["rev-parse", "--path-format=absolute", "--git-path", "info/grafts"]
        grafts <- lift $ doesPathExist (Text.unpack graftPath)
        unless (not grafts) (throwE "Git grafts cannot prove incorporation")
        headCommit <- git ["rev-parse", "--verify", "HEAD^{commit}"]
        current <- optionalGit ["symbolic-ref", "-q", "HEAD"]
        upstream <- optionalGit ["rev-parse", "--symbolic-full-name", "@{upstream}"]
        defaultRemote <- optionalGit ["symbolic-ref", "-q", "refs/remotes/origin/HEAD"]
        references <- Text.lines <$> git
            ["for-each-ref", "--format=%(refname) %(objectname) %(symref)",
             "refs/heads/", "refs/remotes/"]
        let currentName = current >>= Text.stripPrefix "refs/heads/"
            eligible reference =
                Just reference /= current && Just reference /= upstream
                && not (maybe False (\name ->
                    "refs/remotes/" `Text.isPrefixOf` reference
                    && Text.intercalate "/" (drop 3 (Text.splitOn "/" reference)) == name)
                    currentName)
            defaultReference reference =
                Just reference == defaultRemote
                || Just ("refs/remotes/origin/" <>
                    Text.drop (Text.length "refs/heads/") reference) == defaultRemote
                || (defaultRemote == Nothing && reference `elem` ["refs/heads/main", "refs/heads/master"])
            candidates =
                [ (reference, objectId)
                | line <- references
                , [reference, objectId] <- [Text.words line]
                , eligible reference
                , objectId /= headCommit || defaultReference reference
                ]
        containing <- Set.fromList . Text.lines <$> git
            ["for-each-ref", "--format=%(refname)", "--contains=" <> Text.unpack headCommit,
             "refs/heads/", "refs/remotes/"]
        case [reference | (reference, _) <- candidates, Set.member reference containing] of
            reference : _ -> pure ("incorporated into " <> reference)
            [] -> do
                requests <- ExceptT (loadPullRequests headCommit)
                proof <- firstProof requests $ \request -> do
                    if request.pullRequestHead /= headCommit
                        || not (validOid request.pullRequestMerge)
                        || not (validBranch request.pullRequestBase)
                        then pure Nothing
                        else do
                            let targets = filter (\(reference, _) ->
                                    reference == "refs/heads/" <> request.pullRequestBase
                                    || reference == "refs/remotes/origin/" <> request.pullRequestBase)
                                    candidates
                            firstProof targets $ \(reference, target) -> do
                                contained <- isAncestor request.pullRequestMerge target
                                if not contained then pure Nothing else do
                                    -- Every path changed by the branch must have
                                    -- its exact final blob, mode or deletion at
                                    -- the merge commit. Other target changes are
                                    -- irrelevant. Comparing tree objects avoids
                                    -- filters, patch equivalence and rehashing.
                                    commonBases <- Text.lines <$> git
                                        ["merge-base", "--all", Text.unpack headCommit,
                                         Text.unpack request.pullRequestMerge]
                                    matches <- case commonBases of
                                        [base] -> do
                                            changed <- changedPaths base headCommit
                                            different <- changedPaths headCommit request.pullRequestMerge
                                            pure (Set.disjoint changed different)
                                        _ -> pure False
                                    pure $ if matches
                                        then Just ("merged pull request incorporated into " <> reference)
                                        else Nothing
                maybe (throwE "work is not proven incorporated into another branch") pure proof
    pure $ either (Left . Text.pack . displayException) id result
  where
    git arguments = ExceptT $ command path "git" (gitArguments arguments)
    optionalGit arguments = lift $
        either (const Nothing) Just <$> command path "git" (gitArguments arguments)
    changedPaths older newer = do
        (code, output, _) <- lift $ runCommand path "git" $
            gitArguments ["diff", "--name-only", "-z", "--no-renames",
                "--no-ext-diff", "--no-textconv", "--ignore-submodules=none",
                Text.unpack older, Text.unpack newer, "--"]
        unless (code == ExitSuccess) (throwE "Git merge content could not be verified")
        -- Do not strip whitespace: it can be part of a filename. Invalid
        -- output encoding raises an exception and retains the checkout.
        pure $ Set.fromList $ filter (not . Text.null) $
            Text.splitOn "\0" (Text.pack output)
    isAncestor older newer = do
        (code, _, _) <- lift $ runCommand path "git"
            (gitArguments ["merge-base", "--is-ancestor", Text.unpack older, Text.unpack newer])
        case code of
            ExitSuccess -> pure True
            ExitFailure 1 -> pure False
            _ -> throwE "Git ancestry could not be verified"

firstProof :: [a] -> (a -> ExceptT Text IO (Maybe b)) -> ExceptT Text IO (Maybe b)
firstProof [] _ = pure Nothing
firstProof (candidate : remaining) inspect = inspect candidate >>= \case
    Just proof -> pure (Just proof)
    Nothing -> firstProof remaining inspect

validOid :: Text -> Bool
validOid value = Text.length value `elem` [40, 64] && Text.all isHexDigit value

validBranch :: Text -> Bool
validBranch value = not (Text.null value)
    && Text.all (\character -> isAscii character
        && (isAlphaNum character || character `elem` ("/._-" :: String))) value
    && not (".." `Text.isInfixOf` value)

loadMergedPullRequests :: OsPath -> Text -> IO (Either Text [MergedPullRequest])
loadMergedPullRequests path headCommit = runExceptT do
    remote <- ExceptT $ command path "git" (gitArguments ["remote", "get-url", "origin"])
    repository <- maybe (throwE "no supported GitHub origin; incorporation unproven") pure $
        githubRepository remote
    response <- lift $ timeout (10 * 1000000) $ command path "gh"
        ["api", "--hostname", "github.com",
         "repos/" <> Text.unpack repository <> "/commits/" <> Text.unpack headCommit <> "/pulls?per_page=100"]
    body <- ExceptT $ pure $ maybe (Left "GitHub merge verification deadline exceeded") id response
    case eitherDecodeStrict' (Text.encodeUtf8 body) of
        Left _ -> throwE "GitHub merge verification returned invalid data"
        Right requests -> pure $ catMaybes [request | PullRequestResponse request <- requests]

githubRepository :: Text -> Maybe Text
githubRepository remote = do
    repository <- firstJust
        [Text.stripPrefix prefix remote
        | prefix <- ["https://github.com/", "git@github.com:", "ssh://git@github.com/"]]
    let withoutSuffix = maybe repository id (Text.stripSuffix ".git" repository)
        parts = Text.splitOn "/" withoutSuffix
    if length parts == 2 && all (\part -> not (Text.null part)
        && part /= "." && part /= ".."
        && Text.all (\character -> isAscii character &&
            (isAlphaNum character || character `elem` ("._-" :: String))) part) parts
        then Just withoutSuffix else Nothing
  where
    firstJust [] = Nothing
    firstJust (Just value : _) = Just value
    firstJust (Nothing : values) = firstJust values

gitArguments :: [String] -> [String]
gitArguments arguments = ["--no-replace-objects", "--no-optional-locks",
    "-c", "core.fsmonitor=false", "-c", "core.hooksPath=/dev/null"] <> arguments

command :: OsPath -> String -> [String] -> IO (Either Text Text)
command path executable arguments = do
    (code, output, _) <- runCommand path executable arguments
    pure $ case code of
        ExitSuccess -> Right (Text.strip (Text.pack output))
        _ -> Left (Text.pack executable <> " could not verify incorporation")

runCommand :: OsPath -> String -> [String] -> IO (ExitCode, String, String)
runCommand path executable arguments = do
    environment <- getEnvironment
    readCreateProcessWithExitCode
        (proc executable arguments)
            { cwd = Just (unsafeToFilePath path)
            , env = Just $ [("GIT_TERMINAL_PROMPT", "0"), ("GIT_NO_LAZY_FETCH", "1")] <>
                filter (\(name, _) -> not ("GIT_" `isPrefixOf` name)) environment
            } ""
