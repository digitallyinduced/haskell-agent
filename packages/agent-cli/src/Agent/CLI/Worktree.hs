-- | Create isolated git worktrees under @~/.haskell-agent/worktrees@.
module Agent.CLI.Worktree
    ( createWorktree
    , createWorktreeWithFetch
    , createManagedWorktree
    , removeWorktree
    , isUnderWorktreeRoot
    , worktreePath
    , worktreeRoot
    ) where

import Agent.CLI.Config
    ( HarnessConfig(..)
    , WorktreeConfig(..)
    , loadHarnessConfig
    )
import Agent.OsPath (unsafeToFilePath)
import Control.Applicative ((<|>))
import Control.Exception.Safe (finally, mask, onException, tryAny)
import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
    ( ExceptT(..)
    , runExceptT
    , throwE
    , withExceptT
    )
import qualified Data.ByteString as ByteString
import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime(..), getCurrentTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Numeric (showHex)
import System.Directory.OsPath
    ( createDirectoryIfMissing
    , doesPathExist
    , removePathForcibly
    )
import System.Entropy (getEntropy)
import System.Exit (ExitCode(..))
import System.OsPath
    ( OsPath
    , equalFilePath
    , splitDirectories
    , takeDirectory
    , takeFileName
    , unsafeEncodeUtf
    , (</>)
    )
import System.Process (CreateProcess(..), proc, readCreateProcessWithExitCode)

-- | @~/.haskell-agent/worktrees@ given the user's home directory.
worktreeRoot :: OsPath -> OsPath
worktreeRoot home =
    home </> unsafeEncodeUtf ".haskell-agent" </> unsafeEncodeUtf "worktrees"

-- | True when @path@ is @root@ or a subdirectory of it.
-- Both paths should already be absolute (or otherwise comparable).
isUnderWorktreeRoot :: OsPath -> OsPath -> Bool
isUnderWorktreeRoot root path =
    equalFilePath root path
        || splitDirectories root `isPrefixOf` splitDirectories path

-- | @root/repo/YYYY-MM-DD-\<hex8\>@.
worktreePath :: OsPath -> OsPath -> Day -> String -> OsPath
worktreePath root repoName day hex8 =
    root </> repoName </> unsafeEncodeUtf (formatDay day <> "-" <> hex8)

-- | Add a new worktree of @source@ under @root@ using the current @HEAD@.
-- @root@ is injected so tests can use a temp directory instead of the real
-- home.
createWorktree :: OsPath -> OsPath -> IO (Either Text OsPath)
createWorktree = createWorktreeWithFetch False

-- | Add a new worktree, optionally fetching the selected remote's current
-- default branch and using that commit as the base.
createWorktreeWithFetch
    :: Bool -> OsPath -> OsPath -> IO (Either Text OsPath)
createWorktreeWithFetch fetchLatest source root = runExceptT do
    repo <- gitToplevel source
    repoName <- gitRepositoryName repo
    base <-
        if fetchLatest
            then Just <$> fetchLatestUpstream repo
            else pure Nothing
    now <- lift getCurrentTime
    let day = utctDay now
        start = posixMicros now
    lift (createDirectoryIfMissing True (root </> repoName))
    addUnique repo root repoName day start base 0

-- | Create a worktree using the machine-wide policy under the supplied home.
-- Configuration is read for every creation so startup, slash-command, and
-- subagent worktrees all follow the same current setting.
createManagedWorktree :: OsPath -> OsPath -> IO (Either Text OsPath)
createManagedWorktree home source =
    loadHarnessConfig home >>= \case
        Left err -> pure (Left err)
        Right config ->
            createWorktreeWithFetch
                config.configWorktree.worktreeFetchLatestUpstream
                source
                (worktreeRoot home)

-- | Remove a managed worktree and the branch created for it.
removeWorktree :: OsPath -> OsPath -> IO (Either Text ())
removeWorktree source path = runExceptT do
    repo <- gitToplevel source
    void $ ExceptT $
        git repo ["worktree", "remove", "--force", unsafeToFilePath path]
    void $ ExceptT $
        git repo ["branch", "-D", unsafeToFilePath (takeFileName path)]

addUnique
    :: OsPath
    -> OsPath
    -> OsPath
    -> Day
    -> Integer
    -> Maybe Text
    -> Int
    -> ExceptT Text IO OsPath
addUnique repo root repoName day start base attempt
    | attempt >= 32 =
        throwE "could not pick a unique worktree path"
    | otherwise = do
        let path = worktreePath root repoName day (hex8 (start + fromIntegral attempt))
        exists <- lift (doesPathExist path)
        if exists
            then addUnique repo root repoName day start base (attempt + 1)
            else do
                let branch = unsafeToFilePath (takeFileName path)
                    addArgs = case base of
                        Nothing ->
                            ["worktree", "add", unsafeToFilePath path]
                        Just commit ->
                            [ "worktree", "add", "-b", branch
                            , unsafeToFilePath path, Text.unpack commit
                            ]
                added <- lift $ mask \restore ->
                    restore (git repo addArgs)
                        `onException` cleanupWorktreeCandidate repo path
                case added of
                    Left err
                        | branchTaken err ->
                            addUnique
                                repo root repoName day start base (attempt + 1)
                        | otherwise -> do
                            lift (cleanupWorktreeCandidate repo path)
                            throwE err
                    Right _ -> pure path

cleanupWorktreeCandidate :: OsPath -> OsPath -> IO ()
cleanupWorktreeCandidate repo path = do
    exists <- doesPathExist path
    if not exists
        then pure ()
        else do
            _ <- git repo ["worktree", "remove", "--force", unsafeToFilePath path]
            _ <- tryAny (removePathForcibly path)
            _ <- git repo ["worktree", "prune"]
            _ <- git repo ["branch", "-D", unsafeToFilePath (takeFileName path)]
            pure ()

gitToplevel :: OsPath -> ExceptT Text IO OsPath
gitToplevel source = do
    path <- withExceptT
        (\err -> "--worktree requires a git repository (" <> Text.strip err <> ")")
        (ExceptT (git source ["rev-parse", "--show-toplevel"]))
    pure (unsafeEncodeUtf (Text.unpack (Text.strip path)))

gitRepositoryName :: OsPath -> ExceptT Text IO OsPath
gitRepositoryName repo = do
    commonDir <- ExceptT $
        git repo ["rev-parse", "--path-format=absolute", "--git-common-dir"]
    let path = unsafeEncodeUtf (Text.unpack (Text.strip commonDir))
    pure $
        if takeFileName path == unsafeEncodeUtf ".git"
            then takeFileName (takeDirectory path)
            else takeFileName path

-- | Fetch and return the commit at the selected remote's advertised default
-- branch. The current branch's configured remote wins, followed by conventional
-- @upstream@ and @origin@ names, then a sole remaining remote.
fetchLatestUpstream :: OsPath -> ExceptT Text IO Text
fetchLatestUpstream repo = do
    remote <- selectUpstreamRemote repo
    remoteHead <- remoteDefaultBranch repo remote
    localRef <- lift freshFetchRef
    ExceptT $
        runExceptT (fetchIntoRef repo remote remoteHead localRef)
            `finally` cleanupFetchRef repo localRef

fetchIntoRef :: OsPath -> Text -> Text -> Text -> ExceptT Text IO Text
fetchIntoRef repo remote remoteHead localRef = do
    let refspec = remoteHead <> ":" <> localRef
        context action err =
            "failed to " <> action <> " from git remote "
                <> quote remote <> ": " <> Text.strip err
    -- An empty refmap prevents Git from also updating the configured
    -- remote-tracking ref, which would reintroduce a shared ref-lock race.
    void $
        withExceptT (context "fetch the latest default branch") $
            ExceptT $
                git repo
                    [ "fetch"
                    , "--no-tags"
                    , "--no-write-fetch-head"
                    , "--refmap="
                    , Text.unpack remote
                    , Text.unpack refspec
                    ]
    commit <-
        withExceptT (context "resolve the fetched default branch") $
            ExceptT $
                git repo
                    [ "rev-parse"
                    , "--verify"
                    , Text.unpack (localRef <> "^{commit}")
                    ]
    pure (Text.strip commit)

freshFetchRef :: IO Text
freshFetchRef = do
    bytes <- ByteString.unpack <$> getEntropy 16
    pure $
        "refs/haskell-agent/worktree-fetches/"
            <> Text.pack (concatMap hexByte bytes)
  where
    hexByte byte =
        let encoded = showHex byte ""
        in replicate (2 - length encoded) '0' <> encoded

cleanupFetchRef :: OsPath -> Text -> IO ()
cleanupFetchRef repo localRef =
    void $
        tryAny $
            git repo ["update-ref", "-d", Text.unpack localRef]

selectUpstreamRemote :: OsPath -> ExceptT Text IO Text
selectUpstreamRemote repo = do
    output <- ExceptT (git repo ["remote"])
    let remotes = filter (not . Text.null) (map Text.strip (Text.lines output))
    configured <- lift (configuredBranchRemote repo)
    case
        listToMaybe
            [ remote
            | remote <- maybe [] pure configured <> ["upstream", "origin"]
            , remote `elem` remotes
            ]
        <|> case remotes of
            [remote] -> Just remote
            _ -> Nothing
      of
        Just remote -> pure remote
        Nothing
            | null remotes ->
                throwE
                    "worktree.fetchLatestUpstream requires a git remote"
            | otherwise ->
                throwE
                    ( "could not choose an upstream git remote; configure the "
                        <> "current branch's remote or name one 'upstream' or 'origin'"
                    )

configuredBranchRemote :: OsPath -> IO (Maybe Text)
configuredBranchRemote repo =
    git repo ["branch", "--show-current"] >>= \case
        Right rawBranch
            | not (Text.null (Text.strip rawBranch)) ->
                git repo
                    [ "config"
                    , "--get"
                    , "branch." <> Text.unpack (Text.strip rawBranch) <> ".remote"
                    ] >>= \case
                        Right rawRemote ->
                            let remote = Text.strip rawRemote
                            in pure $
                                if Text.null remote || remote == "."
                                    then Nothing
                                    else Just remote
                        Left _ -> pure Nothing
        _ -> pure Nothing

remoteDefaultBranch :: OsPath -> Text -> ExceptT Text IO Text
remoteDefaultBranch repo remote = do
    output <-
        withExceptT
            (\err ->
                "failed to inspect git remote " <> quote remote
                    <> ": " <> Text.strip err)
            (ExceptT
                (git repo
                    [ "ls-remote"
                    , "--symref"
                    , Text.unpack remote
                    , "HEAD"
                    ]))
    case
        [ ref
        | line <- Text.lines output
        , ["ref:", ref, "HEAD"] <- [Text.words line]
        , "refs/heads/" `Text.isPrefixOf` ref
        ]
      of
        ref : _ -> pure ref
        [] ->
            throwE
                ( "git remote " <> quote remote
                    <> " did not advertise a default branch"
                )

quote :: Text -> Text
quote value = "'" <> value <> "'"

git :: OsPath -> [String] -> IO (Either Text Text)
git dir args = do
    (code, out, err) <-
        readCreateProcessWithExitCode
            (proc "git" args) { cwd = Just (unsafeToFilePath dir) }
            ""
    case code of
        ExitSuccess -> pure (Right (Text.pack out))
        ExitFailure _ ->
            pure $ Left $
                let stderrText = Text.strip (Text.pack err)
                    stdoutText = Text.strip (Text.pack out)
                    message
                        | Text.null stderrText = stdoutText
                        | otherwise = stderrText
                in if Text.null message
                    then "git " <> Text.pack (unwords args) <> " failed"
                    else message

branchTaken :: Text -> Bool
branchTaken err =
    "already used by worktree" `Text.isInfixOf` err
        || "already checked out" `Text.isInfixOf` err
        || "already exists" `Text.isInfixOf` err

posixMicros :: UTCTime -> Integer
posixMicros t =
    floor (nominalDiffTimeToSeconds (utcTimeToPOSIXSeconds t) * 1000000)

hex8 :: Integer -> String
hex8 n =
    let s = showHex (n `mod` 0x100000000) ""
    in replicate (8 - length s) '0' <> s

formatDay :: Day -> String
formatDay = formatTime defaultTimeLocale "%Y-%m-%d"
