-- | Workspace-confined filesystem helpers for coding tools.
module Agent.Tools.FileSystem
    ( deleteTextFile
    , listDirectoryEntries
    , readTextFile
    , renameTextFile
    , resolveUnderCwd
    , writeTextFile
    ) where

import Agent.OsPath (OsPath, fromFilePath, toFilePath, toText)
import Agent.Tools.Types (ToolEnv(..))
import Control.Exception.Safe (SomeException, throwIO, try, tryIO)
import Control.Retry
    ( RetryPolicyM
    , exponentialBackoff
    , limitRetries
    , retrying
    )
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.ByteString as BS
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import System.Directory.OsPath
    ( canonicalizePath
    , createDirectoryIfMissing
    , doesDirectoryExist
    , doesPathExist
    , listDirectory
    , removeFile
    , renameFile
    )
import System.OsPath
    ( equalFilePath
    , isAbsolute
    , joinPath
    , makeRelative
    , splitDirectories
    , takeDirectory
    , takeFileName
    , (</>)
    )
import System.IO.Error (isAlreadyInUseError)

-- | Resolve a model-supplied path against the tool cwd and reject anything
-- that canonicalizes outside that tree, including via symlinks.
resolveUnderCwd :: ToolEnv -> OsPath -> IO (Either Text OsPath)
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
            "Path escapes the working directory: " <> toText requested

resolveMissing :: OsPath -> IO OsPath
resolveMissing combined = do
    let parent = takeDirectory combined
    parentExists <- doesDirectoryExist parent
    if parentExists
        then (</> takeFileName combined) <$> canonicalizePath parent
        else pure (collapseDots combined)

collapseDots :: OsPath -> OsPath
collapseDots path = joinPath (go [] (splitDirectories path))
  where
    go acc [] = reverse acc
    go acc (part : xs)
        | part == dot = go acc xs
        | part == dotDot = case acc of
            [] -> go acc xs
            (root : _) | root == slash -> go acc xs
            (_ : rest) -> go rest xs
        | otherwise = go (part : acc) xs
    dot = fromFilePath "."
    dotDot = fromFilePath ".."
    slash = fromFilePath "/"

isInside :: OsPath -> OsPath -> Bool
isInside root path
    | equalFilePath root path = True
    | otherwise =
        let relative = makeRelative root path
        in not (isAbsolute relative)
            && case splitDirectories relative of
                (first : _) | first == fromFilePath ".." -> False
                _ -> True

lockRetryPolicy :: RetryPolicyM IO
lockRetryPolicy = exponentialBackoff 1000 <> limitRetries 5

retryOnBusy :: IO a -> IO a
retryOnBusy action = do
    result <- retrying lockRetryPolicy shouldRetry (const (tryIO action))
    either throwIO pure result
  where
    shouldRetry _ = pure . either isAlreadyInUseError (const False)

readTextFile :: OsPath -> IO (Either Text Text)
readTextFile path =
    try @_ @SomeException (retryOnBusy (BS.readFile (toFilePath path))) >>= \case
    Left err -> pure $ Left $ "Failed to read file: " <> Text.pack (show err)
    Right bytes
        | BS.elem 0 (BS.take 8192 bytes) ->
            pure $ Left "Cannot read binary file"
        | otherwise ->
            pure $ Right $ decodeUtf8With lenientDecode bytes

writeTextFile :: OsPath -> Text -> IO (Either Text ())
writeTextFile path content = do
    createDirectoryIfMissing True (takeDirectory path)
    try @_ @SomeException
        (retryOnBusy (BS.writeFile (toFilePath path) (encodeUtf8 content))) >>= \case
        Left err -> pure $ Left $ "Failed to write file: " <> Text.pack (show err)
        Right () -> pure (Right ())

deleteTextFile :: OsPath -> IO (Either Text ())
deleteTextFile path =
    try @_ @SomeException (removeFile path) >>= \case
        Left err -> pure $ Left $ "Failed to delete file: " <> Text.pack (show err)
        Right () -> pure (Right ())

renameTextFile :: OsPath -> OsPath -> IO (Either Text ())
renameTextFile from to = do
    createDirectoryIfMissing True (takeDirectory to)
    try @_ @SomeException (renameFile from to) >>= \case
        Left err -> pure $ Left $ "Failed to move file: " <> Text.pack (show err)
        Right () -> pure (Right ())

listDirectoryEntries :: OsPath -> IO (Either Text [(OsPath, Bool)])
listDirectoryEntries path = try @_ @SomeException (listDirectory path) >>= \case
    Left err -> pure $ Left $ "Failed to list directory: " <> Text.pack (show err)
    Right names -> Right <$> mapM (classify path) names
  where
    classify root name = do
        isDir <- doesDirectoryExist (root </> name)
        pure (name, isDir)
