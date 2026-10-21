-- | Bounded, owner-checked file exchange between local frontend and server.
module Agent.Runtime.Session.SecretExchange
    ( withSecretExchange, writeSecretExchange, readSecretExchange ) where

import Control.Exception.Safe (bracket, bracketOnError, throwString, tryIO)
import Control.Monad (unless, void, when)
import Data.Bits ((.&.))
import Data.ByteString qualified as ByteString
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import System.Directory (createDirectoryIfMissing, removeFile)
import System.FilePath ((</>))
import System.IO (Handle, hClose)
import System.Posix.Files
import System.Posix.IO
import System.Posix.Temp (mkstemp)
import System.Posix.User (getEffectiveUserID)

withSecretExchange :: FilePath -> (FilePath -> IO a) -> IO a
withSecretExchange directory action = do
    createDirectoryIfMissing True directory
    bracket (openFd directory ReadOnly defaultFileFlags
        { nofollow = True, cloexec = True, directory = True }) closeFd \descriptor -> do
        status <- getFdStatus descriptor
        user <- getEffectiveUserID
        unless (isDirectory status && fileOwner status == user) (throwString "invalid secret exchange directory")
        setFdMode descriptor 0o700
    bracket
        (do
            (path, handle) <- mkstemp (directory </> "input-")
            hClose handle
            pure path)
        (void . tryIO . removeFile)
        action

withExchangeHandle :: FilePath -> OpenMode -> (Handle -> IO a) -> IO a
withExchangeHandle path mode action = bracket
    (bracketOnError (openFd path mode defaultFileFlags{nofollow = True, cloexec = True}) closeFd \descriptor -> do
        status <- getFdStatus descriptor
        user <- getEffectiveUserID
        unless (isRegularFile status && fileOwner status == user && fileMode status .&. 0o077 == 0
            && fileSize status <= 1024 * 1024) (throwString "invalid secret exchange file")
        when (mode == WriteOnly) (setFdSize descriptor 0)
        fdToHandle descriptor)
    hClose action

writeSecretExchange :: FilePath -> Text -> IO ()
writeSecretExchange path value = do
    let bytes = Text.encodeUtf8 value
    unless (ByteString.length bytes <= 1024 * 1024) (throwString "secret input exceeds size limit")
    withExchangeHandle path WriteOnly (\handle -> ByteString.hPut handle bytes)

readSecretExchange :: FilePath -> IO Text
readSecretExchange path = withExchangeHandle path ReadOnly \handle -> do
    bytes <- ByteString.hGet handle (1024 * 1024 + 1)
    unless (ByteString.length bytes <= 1024 * 1024) (throwString "secret input exceeds size limit")
    either (const (throwString "secret input is not UTF-8")) pure (Text.decodeUtf8' bytes)
