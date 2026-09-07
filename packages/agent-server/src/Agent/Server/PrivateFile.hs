-- | Bounded reads of owner-only configuration and credential files.
module Agent.Server.PrivateFile
    ( TrustedPathPolicy
    , fullTrustedPathPolicy
    , trustedPathPolicyWithin
    , readPrivateFile
    , readPrivateTokenFile
    , validateTrustedPathAncestry
    , validateTrustedPathWithPolicy
    , validateToken
    ) where

import Control.Exception.Safe
    ( bracket
    , throwIO
    , tryIO
    )
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString8
import Data.Text (Text)
import System.Directory (canonicalizePath)
import System.FilePath
    ( isAbsolute
    , makeRelative
    , normalise
    , splitDirectories
    , takeDirectory
    )
import System.IO.Error (isEOFError)
import System.Posix.Files
    ( fileMode
    , fileOwner
    , getFdStatus
    , getFileStatus
    , isDirectory
    , isRegularFile
    )
import System.Posix.IO
    ( OpenFileFlags(..)
    , OpenMode(ReadOnly)
    , closeFd
    , defaultFileFlags
    , openFd
    )
import System.Posix.IO.ByteString qualified as Posix
import System.Posix.Types (Fd)
import System.Posix.User (getEffectiveUserID)

readPrivateTokenFile :: FilePath -> IO (Either Text ByteString)
readPrivateTokenFile path =
    readPrivateFile 4096 path >>= pure . (>>= validateToken)

readPrivateFile
    :: Int
    -> FilePath
    -> IO (Either Text ByteString)
readPrivateFile maximumBytes path
    | maximumBytes < 1 =
        pure (Left "the private file size limit must be positive")
    | otherwise = do
        inspected <- tryIO $
            bracket
                (openFd
                    path
                    ReadOnly
                    defaultFileFlags
                        { nofollow = True
                        , cloexec = True
                        })
                closeFd
                \descriptor -> do
                    status <- getFdStatus descriptor
                    user <- getEffectiveUserID
                    if not (isRegularFile status)
                        then
                            pure (Left "the private path must be a regular file")
                        else if fileOwner status /= user
                            then
                                pure
                                    (Left
                                        "the private file must be owned by the current user")
                            else if fileMode status .&. 0o077 /= 0
                                then
                                    pure
                                        (Left
                                            "the private file must not be accessible by group or other users")
                                else
                                    Right
                                        <$> readBytes
                                            (maximumBytes + 1)
                                            descriptor
        pure case inspected of
            Left _ -> Left "could not inspect the private file"
            Right (Right bytes)
                | ByteString.length bytes > maximumBytes ->
                    Left "the private file is too large"
            Right result -> result

validateToken :: ByteString -> Either Text ByteString
validateToken raw =
    let token = stripAsciiSpace raw
    in if ByteString.null token
        then Left "the bearer token must not be empty"
        else Right token

-- | An ancestry-validation boundary. Its constructors stay private so callers
-- cannot replace validation with an arbitrary predicate.
data TrustedPathPolicy
    = TrustAncestryToFilesystemRoot
    | TrustAncestryWithin !FilePath

fullTrustedPathPolicy :: TrustedPathPolicy
fullTrustedPathPolicy = TrustAncestryToFilesystemRoot

-- | Require every component of a canonical path to be controlled by either
-- root or the server account, and never writable by group or other users.
-- Callers may pass a directory's parent when the directory itself is an
-- intentionally writable tenant export.
validateTrustedPathAncestry :: FilePath -> IO (Either Text ())
validateTrustedPathAncestry rawPath =
    validateTrustedPathWithPolicy fullTrustedPathPolicy rawPath

-- | Declare an existing directory as an outer trust boundary after validating
-- that directory itself. The caller owns the trust decision for its parents.
-- This is intended for hermetic build roots whose outer ownership belongs to
-- the build runner rather than the test process.
trustedPathPolicyWithin
    :: FilePath
    -> IO (Either Text TrustedPathPolicy)
trustedPathPolicyWithin rawRoot = do
    resolved <- tryIO (canonicalizePath rawRoot)
    case resolved of
        Left _ ->
            pure (Left "could not inspect trusted path ancestry")
        Right root -> do
            inspected <- tryIO (getFileStatus root)
            case inspected of
                Left _ ->
                    pure (Left "could not inspect trusted path ancestry")
                Right status
                    | not (isDirectory status) ->
                        pure (Left "trusted path boundary must be a directory")
                    | otherwise ->
                        validateTrustedPaths [root] >>= \case
                            Left err -> pure (Left err)
                            Right () ->
                                pure (Right (TrustAncestryWithin root))

validateTrustedPathWithPolicy
    :: TrustedPathPolicy
    -> FilePath
    -> IO (Either Text ())
validateTrustedPathWithPolicy policy rawPath =
    case policy of
        TrustAncestryToFilesystemRoot ->
            validateTrustedPaths
                (pathAndAncestors (normalise rawPath))
        TrustAncestryWithin root -> do
            resolved <- tryIO (canonicalizePath rawPath)
            case resolved of
                Left _ ->
                    pure (Left "could not inspect trusted path ancestry")
                Right path
                    | not (containsPath root path) ->
                        pure (Left "trusted path escapes its declared root")
                    | otherwise ->
                        validateTrustedPaths (pathThroughAncestor root path)

validateTrustedPaths :: [FilePath] -> IO (Either Text ())
validateTrustedPaths paths = do
    user <- getEffectiveUserID
    inspected <- tryIO (mapM getFileStatus paths)
    pure case inspected of
        Left _ ->
            Left "could not inspect trusted path ancestry"
        Right statuses
            | any
                (\status ->
                    fileOwner status /= 0
                        && fileOwner status /= user)
                statuses ->
                Left
                    "trusted path ancestry must be owned by root or the server user"
            | any (\status -> fileMode status .&. 0o022 /= 0) statuses ->
                Left
                    "trusted path ancestry must not be writable by group or other users"
            | otherwise -> Right ()

pathThroughAncestor :: FilePath -> FilePath -> [FilePath]
pathThroughAncestor root path
    | root == path = [path]
    | otherwise = path : pathThroughAncestor root (takeDirectory path)

pathAndAncestors :: FilePath -> [FilePath]
pathAndAncestors path =
    path :
        let parent = takeDirectory path
        in if parent == path
            then []
            else pathAndAncestors parent

containsPath :: FilePath -> FilePath -> Bool
containsPath root candidate =
    let relative = normalise (makeRelative root candidate)
    in not (isAbsolute relative)
        && case splitDirectories relative of
            ".." : _ -> False
            _ -> True

readBytes :: Int -> Fd -> IO ByteString
readBytes maximumBytes descriptor = go maximumBytes []
  where
    go remaining chunks
        | remaining <= 0 =
            pure (ByteString.concat (reverse chunks))
        | otherwise = do
            tryIO
                (Posix.fdRead descriptor (fromIntegral remaining))
                >>= \case
                    Left err
                        | isEOFError err ->
                            pure (ByteString.concat (reverse chunks))
                        | otherwise -> throwIO err
                    Right chunk
                        | ByteString.null chunk ->
                            pure (ByteString.concat (reverse chunks))
                        | otherwise ->
                            go
                                (remaining - ByteString.length chunk)
                                (chunk : chunks)

stripAsciiSpace :: ByteString -> ByteString
stripAsciiSpace =
    ByteString8.dropWhileEnd isAsciiSpace
        . ByteString8.dropWhile isAsciiSpace
  where
    isAsciiSpace character =
        character == ' '
            || character == '\t'
            || character == '\r'
            || character == '\n'
