module Agent.Runtime.Daemon.Socket
    ( SocketConfig (..)
    , defaultSocketConfig
    , defaultSocketPath
    , withUnixListener
    , acceptOwnedPeer
    , verifyPeerOwner
    ) where

import Control.Exception.Safe (IOException, bracket, catch, onException, throwString, tryIO)
import Control.Monad (unless, when)
import Network.Socket
import System.Directory hiding (isSymbolicLink)
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeDirectory)
import System.Posix.Files
import System.Posix.User (getEffectiveUserID)

data SocketConfig = SocketConfig
    { path :: FilePath
    , backlog :: Int
    }
    deriving stock (Eq, Show)

defaultSocketConfig :: IO SocketConfig
defaultSocketConfig = do
    path <- defaultSocketPath
    pure SocketConfig {path, backlog = 16}

defaultSocketPath :: IO FilePath
defaultSocketPath = do
    home <- getHomeDirectory
    override <- lookupEnv "HASKELL_AGENT_RUNTIME_DIR"
    pure $ maybe (home </> ".haskell-agent" </> "runtime") id override </> "daemon.sock"

withUnixListener :: SocketConfig -> (Socket -> IO value) -> IO value
withUnixListener config =
    bracket (openListener config) (closeListener config)

acceptOwnedPeer :: Socket -> IO Socket
acceptOwnedPeer listener = do
    (peer, _) <- accept listener
    verifyPeerOwner peer `onException` close peer
    pure peer

verifyPeerOwner :: Socket -> IO ()
verifyPeerOwner peer = do
    (_, peerUser, _) <- getPeerCredential peer
    daemonUser <- fromIntegral <$> getEffectiveUserID
    unless (peerUser == Just daemonUser) $
        throwString "refusing Unix socket peer owned by another user"

openListener :: SocketConfig -> IO Socket
openListener config = do
    let directory = takeDirectory config.path
    createDirectoryIfMissing True directory
    verifyPrivateDirectory directory
    setFileMode directory 0o700
    removeStaleSocket config.path
    listener <- socket AF_UNIX Stream defaultProtocol
    (do
            bind listener (SockAddrUnix config.path)
            setFileMode config.path 0o600
            listen listener config.backlog
            pure listener
        )
        `onException` close listener

closeListener :: SocketConfig -> Socket -> IO ()
closeListener config listener = do
    close listener
    removeFile (path config) `catch` \(_ :: IOException) -> pure ()

removeStaleSocket :: FilePath -> IO ()
removeStaleSocket path = do
    exists <- fileExist path
    when exists $ do
        status <- getSymbolicLinkStatus path
        effectiveUser <- getEffectiveUserID
        if isSocket status && fileOwner status == effectiveUser
            then do
                active <- socketAcceptsConnections path
                if active
                    then throwString ("runtime daemon is already listening at: " <> path)
                    else removeFile path
            else throwString ("refusing to replace non-socket path: " <> path)

socketAcceptsConnections :: FilePath -> IO Bool
socketAcceptsConnections path =
    bracket (socket AF_UNIX Stream defaultProtocol) close $ \probe ->
        either (const False) (const True)
            <$> tryIO (connect probe (SockAddrUnix path))

verifyPrivateDirectory :: FilePath -> IO ()
verifyPrivateDirectory directory = do
    status <- getSymbolicLinkStatus directory
    effectiveUser <- getEffectiveUserID
    unless (isDirectory status && not (isSymbolicLink status) && fileOwner status == effectiveUser) $
        throwString ("runtime socket directory is not a user-owned directory: " <> directory)
