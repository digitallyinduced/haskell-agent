module Agent.CLI.ProcessSecurity
    ( canonicalPathOutside
    , resolveExecutableOutside
    , sanitizeSearchPathOutside
    ) where

import Control.Exception.Safe
    ( isAsyncException
    , throwIO
    , tryAny
    )
import Data.Maybe (catMaybes, fromMaybe)
import Data.List (intercalate)
import Data.Text (Text)
import System.Directory
    ( canonicalizePath
    , doesPathExist
    , findExecutable
    , makeAbsolute
    )
import System.FilePath
    ( isAbsolute
    , normalise
    , searchPathSeparator
    , splitSearchPath
    , takeDirectory
    , (</>)
    )
import System.Posix.Files
    ( deviceID
    , fileID
    , getFileStatus
    )

resolveExecutableOutside
    :: FilePath
    -> FilePath
    -> IO (Either Text FilePath)
resolveExecutableOutside repositoryRoot executable =
    repositoryBoundary repositoryRoot >>= \boundary ->
        findExecutable executable >>= \case
            Nothing -> pure (Left "command is unavailable")
            Just path ->
                canonicalPathOutside boundary path >>= \case
                    Nothing ->
                        pure (Left "repository-local executables are not allowed")
                    Just checked -> pure (Right checked)

sanitizeSearchPathOutside
    :: FilePath
    -> String
    -> IO (Maybe String)
sanitizeSearchPathOutside repositoryRoot raw = do
    boundary <- repositoryBoundary repositoryRoot
    entries <- catMaybes <$> mapM (sanitize boundary) (splitSearchPath raw)
    pure
        (if null entries
            then Nothing
            else Just (intercalate [searchPathSeparator] entries))
  where
    sanitize boundary entry
        | not (isAbsolute entry) = pure Nothing
        | otherwise =
            tryAny
                (canonicalPathOutside boundary entry) >>= \case
                            Left exception
                                | isAsyncException exception ->
                                    throwIO exception
                                | otherwise -> pure Nothing
                            Right result -> pure result

repositoryBoundary :: FilePath -> IO FilePath
repositoryBoundary requested =
    canonicalizePath requested >>= \canonical ->
        walk canonical Nothing
  where
    walk current candidate = do
        present <- doesPathExist (current </> ".git")
        let selected = if present then Just current else candidate
            parent = takeDirectory current
        if parent == current
            then
                canonicalizePath requested
                    >>= \canonical ->
                        pure (fromMaybe canonical selected)
            else walk parent selected

-- | Return the canonical child path only when neither its lexical nor
-- canonical ancestry crosses the supplied directory boundary. Identity
-- comparisons keep this correct for symlinks and case-insensitive filesystems.
canonicalPathOutside :: FilePath -> FilePath -> IO (Maybe FilePath)
canonicalPathOutside parent child = do
    absoluteChild <- normalise <$> makeAbsolute child
    canonicalParent <- canonicalizePath parent
    canonicalChild <- canonicalizePath child
    parentStatus <- getFileStatus canonicalParent
    contained <-
        or <$> mapM (sameIdentity parentStatus)
            (ancestors absoluteChild <> ancestors canonicalChild)
    pure (if contained then Nothing else Just canonicalChild)
  where
    sameIdentity expected candidate = do
        actual <- getFileStatus candidate
        pure
            (deviceID actual == deviceID expected
                && fileID actual == fileID expected)
    ancestors path =
        path
            : let parentPath = takeDirectory path
              in if parentPath == path
                    then []
                    else ancestors parentPath
