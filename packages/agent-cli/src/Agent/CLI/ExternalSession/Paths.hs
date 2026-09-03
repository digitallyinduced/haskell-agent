module Agent.CLI.ExternalSession.Paths
    ( canonicalPath
    , directoryChildren
    , expandHome
    , isPathLike
    , isSafeDirectory
    , isSafeFile
    , literalPathComponent
    , pathIsWithin
    , recursiveFiles
    , samePath
    ) where

import Control.Exception.Safe (IOException, tryIO)
import Data.Char (isAlphaNum)
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory
    ( canonicalizePath
    , doesDirectoryExist
    , doesFileExist
    , listDirectory
    , pathIsSymbolicLink
    )
import System.FilePath
    ( isAbsolute
    , makeRelative
    , normalise
    , pathSeparator
    , (</>)
    )

canonicalPath :: FilePath -> IO (Maybe FilePath)
canonicalPath path
    | '\NUL' `elem` path = pure Nothing
    | otherwise =
        tryIO (canonicalizePath path) >>= \case
            Left (_ :: IOException) -> pure Nothing
            Right value -> pure (Just (normalise value))

samePath :: FilePath -> FilePath -> IO Bool
samePath left right =
    (,) <$> canonicalPath left <*> canonicalPath right >>= \case
        (Just canonicalLeft, Just canonicalRight) ->
            pure (canonicalLeft == canonicalRight)
        _ -> pure False

pathIsWithin :: FilePath -> FilePath -> IO Bool
pathIsWithin path root =
    (,) <$> canonicalPath path <*> canonicalPath root >>= \case
        (Just canonicalPathValue, Just canonicalRoot) -> do
            let relative = makeRelative canonicalRoot canonicalPathValue
            pure $
                relative == "."
                    || ( not (isAbsolute relative)
                        && relative /= ".."
                        && not ((".." <> [pathSeparator]) `isPrefixOf` relative)
                       )
        _ -> pure False

isSafeFile :: FilePath -> FilePath -> IO Bool
isSafeFile root path = do
    exists <- doesFileExist path
    symlink <- if exists then pathIsSymbolicLinkSafe path else pure True
    inside <- if exists && not symlink then pathIsWithin path root else pure False
    pure (exists && not symlink && inside)

isSafeDirectory :: FilePath -> FilePath -> IO Bool
isSafeDirectory root path = do
    exists <- doesDirectoryExist path
    symlink <- if exists then pathIsSymbolicLinkSafe path else pure True
    inside <- if exists && not symlink then pathIsWithin path root else pure False
    pure (exists && not symlink && inside)

pathIsSymbolicLinkSafe :: FilePath -> IO Bool
pathIsSymbolicLinkSafe path =
    tryIO (pathIsSymbolicLink path) >>= \case
        Left (_ :: IOException) -> pure True
        Right value -> pure value

directoryChildren :: FilePath -> IO [FilePath]
directoryChildren path =
    tryIO (listDirectory path) >>= \case
        Left (_ :: IOException) -> pure []
        Right names -> pure [path </> name | name <- names]

recursiveFiles :: FilePath -> (FilePath -> Bool) -> IO [FilePath]
recursiveFiles root predicate = go root
  where
    go directory = do
        children <- directoryChildren directory
        fmap concat $ traverse visit children
    visit path = do
        file <- doesFileExist path
        directory <- doesDirectoryExist path
        symlink <- pathIsSymbolicLinkSafe path
        if symlink
            then pure []
            else if file
                then pure [path | predicate path]
                else if directory
                    then go path
                    else pure []

literalPathComponent :: Text -> Bool
literalPathComponent value =
    not (Text.null value)
        && value `notElem` [".", ".."]
        && not (Text.any (`elem` ['\NUL', '/', '\\']) value)

isPathLike :: Text -> Bool
isPathLike value =
    Text.isPrefixOf "~" value
        || Text.isPrefixOf "." value
        || Text.any (`elem` ['/', '\\']) value
        || (Text.length value >= 2
            && Text.index value 1 == ':'
            && isAlphaNum (Text.head value))

expandHome :: FilePath -> FilePath -> FilePath
expandHome home path =
    case path of
        "~" -> home
        '~' : '/' : rest -> home </> rest
        _ -> path
