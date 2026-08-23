module Agent.Tools.FileSystem.GitIgnore (isGitIgnored) where

import Agent.OsPath (unsafeToFilePath)
import System.Directory (findExecutable)
import System.Exit (ExitCode(..))
import System.OsPath (OsPath)
import System.Process (readProcessWithExitCode)

isGitIgnored :: OsPath -> OsPath -> IO Bool
isGitIgnored cwd path = findExecutable "git" >>= \case
    Nothing -> pure False
    Just git -> do
        (code, _, _) <- readProcessWithExitCode git
            ["-C", unsafeToFilePath cwd, "check-ignore", "-q", "--", unsafeToFilePath path] ""
        pure (code == ExitSuccess)
