-- | Authoritative, fail-closed access to a session's @plan.md@.
module Agent.Tools.PlanMode.File
    ( PlanDigest(..)
    , PlanSnapshot(..)
    , PlanFileError(..)
    , PlanReadResult(..)
    , planDigest
    , renderPlanFileError
    , readPlanFile
    , writePlanFile
    , ensurePlanFile
    ) where

import Agent.FileRetry (retryOnFileBusy, writeLazyFileAtomically)
import Agent.OsPath (toText, unsafeToFilePath)
import Control.Exception.Safe
    ( IOException
    , bracket
    , bracketOnError
    , displayException
    , tryIO
    )
import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.Directory.OsPath (createDirectoryIfMissing)
import System.IO (Handle, hClose)
import System.IO.Error (isAlreadyExistsError, isDoesNotExistError)
import System.OsPath (OsPath, takeDirectory)
import System.Posix.Files
    ( getSymbolicLinkStatus
    , isRegularFile
    , isSymbolicLink
    , setFdMode
    )
import System.Posix.IO
    ( OpenFileFlags(..)
    , OpenMode(ReadOnly, WriteOnly)
    , closeFd
    , defaultFileFlags
    , fdToHandle
    , openFd
    )

-- | Lower-case hexadecimal SHA-256 of the exact persisted UTF-8 bytes.
newtype PlanDigest = PlanDigest
    { unPlanDigest :: Text
    } deriving (Eq, Ord, Show)

data PlanSnapshot = PlanSnapshot
    { planSnapshotMarkdown :: !Text
    , planSnapshotBytes :: !BS.ByteString
    , planSnapshotDigest :: !PlanDigest
    } deriving (Eq, Show)

data PlanFileError
    = PlanFileSymlink !OsPath
    | PlanFileNotRegular !OsPath
    | PlanFileInvalidUtf8 !OsPath !Text
    | PlanFileIoError !OsPath !Text
    | PlanFileReadbackMissing !OsPath
    | PlanFileReadbackChanged !OsPath
    deriving (Eq, Show)

data PlanReadResult
    = PlanAbsent
    | PlanPresent !PlanSnapshot
    | PlanUnreadable !PlanFileError
    deriving (Eq, Show)

planDigest :: BS.ByteString -> PlanDigest
planDigest bytes =
    PlanDigest (Text.pack (show (hash bytes :: Digest SHA256)))

renderPlanFileError :: PlanFileError -> Text
renderPlanFileError = \case
    PlanFileSymlink path ->
        "refusing to use plan file because it is a symbolic link: "
            <> toText path
    PlanFileNotRegular path ->
        "refusing to use plan file because it is not a regular file: "
            <> toText path
    PlanFileInvalidUtf8 path message ->
        "plan file is not valid UTF-8 (" <> toText path <> "): " <> message
    PlanFileIoError path message ->
        "could not access plan file (" <> toText path <> "): " <> message
    PlanFileReadbackMissing path ->
        "plan file disappeared immediately after it was written: " <> toText path
    PlanFileReadbackChanged path ->
        "plan file changed while verifying the completed write: " <> toText path

-- | Read through a descriptor opened with @O_NOFOLLOW@. The preliminary
-- @lstat@ gives a useful error for a stable symlink; @O_NOFOLLOW@ closes the
-- check/open race.
readPlanFile :: OsPath -> IO PlanReadResult
readPlanFile path =
    tryIO (getSymbolicLinkStatus filePath) >>= \case
        Left err
            | isDoesNotExistError err -> pure PlanAbsent
            | otherwise -> pure (PlanUnreadable (ioErrorFor path err))
        Right status
            | isSymbolicLink status ->
                pure (PlanUnreadable (PlanFileSymlink path))
            | not (isRegularFile status) ->
                pure (PlanUnreadable (PlanFileNotRegular path))
            | otherwise ->
                readRegularFile path
  where
    filePath = unsafeToFilePath path

readRegularFile :: OsPath -> IO PlanReadResult
readRegularFile path =
    tryIO (bracket acquire hClose BS.hGetContents) >>= \case
        Left err
            | isDoesNotExistError err -> pure PlanAbsent
            | otherwise -> pure (PlanUnreadable (ioErrorFor path err))
        Right bytes ->
            pure $ case Text.decodeUtf8' bytes of
                Left unicodeError ->
                    PlanUnreadable
                        (PlanFileInvalidUtf8
                            path
                            (Text.pack (displayException unicodeError)))
                Right markdown ->
                    PlanPresent PlanSnapshot
                        { planSnapshotMarkdown = markdown
                        , planSnapshotBytes = bytes
                        , planSnapshotDigest = planDigest bytes
                        }
  where
    acquire :: IO Handle
    acquire =
        bracketOnError
            (retryOnFileBusy
                (openFd
                    (unsafeToFilePath path)
                    ReadOnly
                    defaultFileFlags
                        { nofollow = True
                        , cloexec = True
                        }))
            closeFd
            fdToHandle

-- | Atomically replace @path@ with the supplied Markdown, then read it back.
-- A symlink or other non-regular existing leaf is rejected before replacement.
writePlanFile :: OsPath -> Text -> IO (Either PlanFileError PlanSnapshot)
writePlanFile path markdown = do
    createDirectoryIfMissing True (takeDirectory path)
    inspectLeaf path >>= \case
        Left err -> pure (Left err)
        Right () -> do
            let expected = Text.encodeUtf8 markdown
            tryIO
                (writeLazyFileAtomically path 0o600 (LBS.fromStrict expected))
                >>= \case
                    Left err -> pure (Left (ioErrorFor path err))
                    Right () ->
                        readPlanFile path >>= \case
                            PlanAbsent ->
                                pure (Left (PlanFileReadbackMissing path))
                            PlanUnreadable err -> pure (Left err)
                            PlanPresent snapshot
                                | snapshot.planSnapshotBytes == expected ->
                                    pure (Right snapshot)
                                | otherwise ->
                                    pure (Left (PlanFileReadbackChanged path))

-- | Create an empty plan only when it is absent. Existing content is returned
-- unchanged.
ensurePlanFile :: OsPath -> IO (Either PlanFileError PlanSnapshot)
ensurePlanFile path = do
    createDirectoryIfMissing True (takeDirectory path)
    readPlanFile path >>= \case
        PlanPresent snapshot -> pure (Right snapshot)
        PlanUnreadable err -> pure (Left err)
        PlanAbsent ->
            tryIO createEmptyExclusively >>= \case
                Right () -> readCreated
                Left err
                    | isAlreadyExistsError err ->
                        readPlanFile path >>= resultToEither
                    | otherwise -> pure (Left (ioErrorFor path err))
  where
    createEmptyExclusively =
        bracket
            (retryOnFileBusy
                (openFd
                    (unsafeToFilePath path)
                    WriteOnly
                    defaultFileFlags
                        { exclusive = True
                        , nofollow = True
                        , creat = Just 0o600
                        , cloexec = True
                        }))
            closeFd
            (`setFdMode` 0o600)
    readCreated =
        readPlanFile path >>= resultToEither
    resultToEither = \case
        PlanAbsent -> pure (Left (PlanFileReadbackMissing path))
        PlanUnreadable err -> pure (Left err)
        PlanPresent snapshot -> pure (Right snapshot)

inspectLeaf :: OsPath -> IO (Either PlanFileError ())
inspectLeaf path =
    tryIO (getSymbolicLinkStatus (unsafeToFilePath path)) >>= \case
        Left err
            | isDoesNotExistError err -> pure (Right ())
            | otherwise -> pure (Left (ioErrorFor path err))
        Right status
            | isSymbolicLink status -> pure (Left (PlanFileSymlink path))
            | not (isRegularFile status) ->
                pure (Left (PlanFileNotRegular path))
            | otherwise -> pure (Right ())

ioErrorFor :: OsPath -> IOException -> PlanFileError
ioErrorFor path = PlanFileIoError path . Text.pack . displayException
