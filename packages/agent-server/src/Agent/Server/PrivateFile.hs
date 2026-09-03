-- | Bounded reads of owner-only configuration and credential files.
module Agent.Server.PrivateFile
    ( readPrivateFile
    , readPrivateTokenFile
    , validateTrustedPathAncestry
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
import System.FilePath (normalise, takeDirectory)
import System.IO.Error (isEOFError)
import System.Posix.Files
    ( fileMode
    , fileOwner
    , getFdStatus
    , getFileStatus
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

-- | Require every component of a canonical path to be controlled by either
-- root or the server account, and never writable by group or other users.
-- Callers may pass a directory's parent when the directory itself is an
-- intentionally writable tenant export.
validateTrustedPathAncestry :: FilePath -> IO (Either Text ())
validateTrustedPathAncestry rawPath = do
    user <- getEffectiveUserID
    inspected <-
        tryIO (mapM getFileStatus (pathAndAncestors (normalise rawPath)))
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

pathAndAncestors :: FilePath -> [FilePath]
pathAndAncestors path =
    path :
        let parent = takeDirectory path
        in if parent == path
            then []
            else pathAndAncestors parent

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
