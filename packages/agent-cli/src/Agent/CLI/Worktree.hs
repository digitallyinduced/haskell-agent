-- | Create isolated git worktrees under @~/.haskell-agent/worktrees@.
module Agent.CLI.Worktree
    ( createWorktree
    , removeWorktree
    , isUnderWorktreeRoot
    , worktreePath
    , worktreeRoot
    ) where

import Agent.OsPath (unsafeToFilePath)
import Control.Exception.Safe (mask, onException, tryAny)
import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
    ( ExceptT(..)
    , runExceptT
    , throwE
    , withExceptT
    )
import Data.List (isPrefixOf)
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

-- | Add a new worktree of @source@ under @root@. @root@ is injected so tests
-- can use a temp directory instead of the real home.
createWorktree :: OsPath -> OsPath -> IO (Either Text OsPath)
createWorktree source root = runExceptT do
    repo <- gitToplevel source
    repoName <- gitRepositoryName repo
    now <- lift getCurrentTime
    let day = utctDay now
        start = posixMicros now
    lift (createDirectoryIfMissing True (root </> repoName))
    addUnique repo root repoName day start 0

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
    -> Int
    -> ExceptT Text IO OsPath
addUnique repo root repoName day start attempt
    | attempt >= 32 =
        throwE "could not pick a unique worktree path"
    | otherwise = do
        let path = worktreePath root repoName day (hex8 (start + fromIntegral attempt))
        exists <- lift (doesPathExist path)
        if exists
            then addUnique repo root repoName day start (attempt + 1)
            else do
                added <- lift $ mask \restore ->
                    restore (git repo ["worktree", "add", unsafeToFilePath path])
                        `onException` cleanupWorktreeCandidate repo path
                case added of
                    Left err
                        | branchTaken err ->
                            addUnique repo root repoName day start (attempt + 1)
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
