-- | Create isolated git worktrees under @~/.haskell-agent/worktrees@.
module Agent.CLI.Worktree
    ( createWorktree
    , isUnderWorktreeRoot
    , worktreePath
    , worktreeRoot
    ) where

import Agent.OsPath (OsPath, fromFilePath, toFilePath)
import Data.Char (isSpace)
import Data.List (dropWhileEnd, isInfixOf, isPrefixOf)
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime(..), getCurrentTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Numeric (showHex)
import System.Directory.OsPath (createDirectoryIfMissing, doesPathExist)
import System.Exit (ExitCode(..))
import System.OsPath
    ( equalFilePath
    , splitDirectories
    , takeFileName
    , (</>)
    )
import System.Process (CreateProcess(..), proc, readCreateProcessWithExitCode)

-- | @~/.haskell-agent/worktrees@ given the user's home directory.
worktreeRoot :: OsPath -> OsPath
worktreeRoot home =
    home </> fromFilePath ".haskell-agent" </> fromFilePath "worktrees"

-- | True when @path@ is @root@ or a subdirectory of it.
-- Both paths should already be absolute (or otherwise comparable).
isUnderWorktreeRoot :: OsPath -> OsPath -> Bool
isUnderWorktreeRoot root path =
    equalFilePath root path
        || splitDirectories root `isPrefixOf` splitDirectories path

-- | @root/repo/YYYY-MM-DD-\<hex8\>@.
worktreePath :: OsPath -> OsPath -> Day -> String -> OsPath
worktreePath root repoName day hex8 =
    root </> repoName </> fromFilePath (formatDay day <> "-" <> hex8)

-- | Add a new worktree of @source@ under @root@. @root@ is injected so tests
-- can use a temp directory instead of the real home.
createWorktree :: OsPath -> OsPath -> IO (Either String OsPath)
createWorktree source root = do
    gitToplevel source >>= \case
        Left err -> pure (Left err)
        Right repo -> do
            now <- getCurrentTime
            let repoName = takeFileName repo
                day = utctDay now
                start = posixMicros now
            createDirectoryIfMissing True (root </> repoName)
            addUnique repo root repoName day start 0

addUnique
    :: OsPath
    -> OsPath
    -> OsPath
    -> Day
    -> Integer
    -> Int
    -> IO (Either String OsPath)
addUnique repo root repoName day start attempt
    | attempt >= 32 =
        pure (Left "could not pick a unique worktree path")
    | otherwise = do
        let path = worktreePath root repoName day (hex8 (start + fromIntegral attempt))
        exists <- doesPathExist path
        if exists
            then addUnique repo root repoName day start (attempt + 1)
            else git repo ["worktree", "add", toFilePath path] >>= \case
                Left err
                    | branchTaken err ->
                        addUnique repo root repoName day start (attempt + 1)
                    | otherwise -> pure (Left err)
                Right _ -> pure (Right path)

gitToplevel :: OsPath -> IO (Either String OsPath)
gitToplevel source = git source ["rev-parse", "--show-toplevel"] >>= \case
    Left err ->
        pure $ Left $
            "--worktree requires a git repository ("
                <> trim err
                <> ")"
    Right path -> pure (Right (fromFilePath (trim path)))

git :: OsPath -> [String] -> IO (Either String String)
git dir args = do
    (code, out, err) <-
        readCreateProcessWithExitCode
            (proc "git" args) { cwd = Just (toFilePath dir) }
            ""
    case code of
        ExitSuccess -> pure (Right out)
        ExitFailure _ ->
            pure $ Left $
                let msg = trim (if null (trim err) then out else err)
                in if null msg then "git " <> unwords args <> " failed" else msg

branchTaken :: String -> Bool
branchTaken err =
    "already used by worktree" `isInfixOf` err
        || "already checked out" `isInfixOf` err
        || "already exists" `isInfixOf` err

posixMicros :: UTCTime -> Integer
posixMicros t =
    floor (nominalDiffTimeToSeconds (utcTimeToPOSIXSeconds t) * 1000000)

hex8 :: Integer -> String
hex8 n =
    let s = showHex (n `mod` 0x100000000) ""
    in replicate (8 - length s) '0' <> s

formatDay :: Day -> String
formatDay = formatTime defaultTimeLocale "%Y-%m-%d"

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
