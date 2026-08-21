-- | Workspace-confined filesystem helpers for coding tools.
module Agent.Tools.FileSystem
    ( deleteTextFile
    , listDirectoryEntries
    , readTextFile
    , renameTextFile
    , resolveUnderCwd
    , writeTextFile
    ) where

import Agent.Tools.Types (ToolEnv(..))
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (SomeException, catchIO, throwIO, try)
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.ByteString as BS
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import System.Directory
    ( canonicalizePath
    , createDirectoryIfMissing
    , doesDirectoryExist
    , doesPathExist
    , listDirectory
    , removeFile
    , renameFile
    )
import System.FilePath
    ( addTrailingPathSeparator
    , isAbsolute
    , joinPath
    , splitDirectories
    , takeDirectory
    , takeFileName
    , (</>)
    )
import System.IO.Error (isAlreadyInUseError)

-- | Resolve a model-supplied path against the tool cwd and reject anything
-- that canonicalizes outside that tree, including via symlinks.
resolveUnderCwd :: ToolEnv -> FilePath -> IO (Either Text FilePath)
resolveUnderCwd env requested = do
    absCwd <- canonicalizePath env.toolCwd
    let combined
            | isAbsolute requested = requested
            | otherwise = absCwd </> requested
    exists <- doesPathExist combined
    resolved <- if exists
        then canonicalizePath combined
        else resolveMissing combined
    if isInside absCwd resolved
        then pure (Right resolved)
        else pure $ Left $
            "Path escapes the working directory: " <> Text.pack requested

resolveMissing :: FilePath -> IO FilePath
resolveMissing combined = do
    let parent = takeDirectory combined
    parentExists <- doesDirectoryExist parent
    if parentExists
        then (</> takeFileName combined) <$> canonicalizePath parent
        else pure (collapseDots combined)

collapseDots :: FilePath -> FilePath
collapseDots path = joinPath (go [] (splitDirectories path))
  where
    go acc [] = reverse acc
    go acc ("." : xs) = go acc xs
    go acc (".." : xs) = case acc of
        [] -> go acc xs
        ("/" : _) -> go acc xs
        (_ : rest) -> go rest xs
    go acc (x : xs) = go (x : acc) xs

isInside :: FilePath -> FilePath -> Bool
isInside root path =
    let root' = addTrailingPathSeparator root
    in path == root || root' `isPrefixOf` path

lockRetryDelaysUs :: [Int]
lockRetryDelaysUs = [1000, 2000, 4000, 8000, 16000]

retryOnBusy :: IO a -> IO a
retryOnBusy action = go lockRetryDelaysUs
  where
    go [] = action
    go (delayUs : rest) =
        catchIO action \err ->
            if isAlreadyInUseError err
                then threadDelay delayUs >> go rest
                else throwIO err

readTextFile :: FilePath -> IO (Either Text Text)
readTextFile path = try @_ @SomeException (retryOnBusy (BS.readFile path)) >>= \case
    Left err -> pure $ Left $ "Failed to read file: " <> Text.pack (show err)
    Right bytes
        | BS.elem 0 (BS.take 8192 bytes) ->
            pure $ Left "Cannot read binary file"
        | otherwise ->
            pure $ Right $ decodeUtf8With lenientDecode bytes

writeTextFile :: FilePath -> Text -> IO (Either Text ())
writeTextFile path content = do
    createDirectoryIfMissing True (takeDirectory path)
    try @_ @SomeException (retryOnBusy (BS.writeFile path (encodeUtf8 content))) >>= \case
        Left err -> pure $ Left $ "Failed to write file: " <> Text.pack (show err)
        Right () -> pure (Right ())

deleteTextFile :: FilePath -> IO (Either Text ())
deleteTextFile path =
    try @_ @SomeException (removeFile path) >>= \case
        Left err -> pure $ Left $ "Failed to delete file: " <> Text.pack (show err)
        Right () -> pure (Right ())

renameTextFile :: FilePath -> FilePath -> IO (Either Text ())
renameTextFile from to = do
    createDirectoryIfMissing True (takeDirectory to)
    try @_ @SomeException (renameFile from to) >>= \case
        Left err -> pure $ Left $ "Failed to move file: " <> Text.pack (show err)
        Right () -> pure (Right ())

listDirectoryEntries :: FilePath -> IO (Either Text [(FilePath, Bool)])
listDirectoryEntries path = try @_ @SomeException (listDirectory path) >>= \case
    Left err -> pure $ Left $ "Failed to list directory: " <> Text.pack (show err)
    Right names -> Right <$> mapM (classify path) names
  where
    classify root name = do
        isDir <- doesDirectoryExist (root </> name)
        pure (name, isDir)
