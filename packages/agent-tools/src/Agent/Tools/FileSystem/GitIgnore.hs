module Agent.Tools.FileSystem.GitIgnore
    ( isGitIgnored
    , ignoredPaths
    ) where

import Agent.OsPath (unsafeToFilePath)
import qualified Data.Set as Set
import System.Directory (findExecutable)
import System.Exit (ExitCode(..))
import System.OsPath (OsPath)
import System.Process (readProcessWithExitCode)

isGitIgnored :: OsPath -> OsPath -> IO Bool
isGitIgnored cwd path =
    Set.member (unsafeToFilePath path) <$> ignoredPaths cwd [path]

-- | Check a directory's immediate entries in one Git invocation.
--
-- @git check-ignore@ returns only ignored paths and uses exit code 1 when
-- none match. Other non-zero codes are operational failures and are treated
-- like the old per-path check: no path is considered ignored.
ignoredPaths :: OsPath -> [OsPath] -> IO (Set.Set FilePath)
ignoredPaths _ [] = pure Set.empty
ignoredPaths cwd paths = findExecutable "git" >>= \case
    Nothing -> pure Set.empty
    Just git -> do
        (code, output, _) <- readProcessWithExitCode git
            ["-C", unsafeToFilePath cwd, "check-ignore", "-z", "--stdin"]
            (concatMap (\path -> unsafeToFilePath path <> "\0") paths)
        pure case code of
            ExitSuccess -> parseOutput output
            ExitFailure 1 -> parseOutput output
            ExitFailure _ -> Set.empty
  where
    parseOutput = Set.fromList . filter (not . null) . splitNuls

splitNuls :: String -> [String]
splitNuls = \case
    "" -> []
    value ->
        let (part, rest) = break (== '\0') value
        in part : case rest of
            [] -> []
            _ : more -> splitNuls more
