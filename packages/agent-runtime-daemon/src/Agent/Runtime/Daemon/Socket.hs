module Agent.Runtime.Daemon.Socket
    ( SocketConfig (..)
    , defaultSocketConfig
    , defaultSocketPath
    , withUnixListener
    , acceptOwnedPeer
    , verifyPeerOwner
    ) where

import Control.Exception.Safe (IOException, bracket, catch, finally, onException, throwString, tryIO)
import Control.Monad (unless, when)
import Network.Socket
import System.Directory hiding (isSymbolicLink)
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeDirectory)
import qualified System.FileLock as FileLock
import System.Posix.Files
import System.Posix.IO
    ( OpenFileFlags (..)
    , OpenMode (ReadOnly, ReadWrite)
    , closeFd
    , defaultFileFlags
    , openFd
    )
import System.Posix.Types (DeviceID, FileID)
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
withUnixListener config action = do
    prepareSocketDirectory config
    prepareLockFile (lockPath config)
    result <-
        FileLock.withTryFileLock (lockPath config) FileLock.Exclusive $ \_ ->
            bracket (openListener config) (closeListener config) (action . (.listenerSocket))
    case result of
        Nothing -> throwString ("runtime daemon already owns: " <> config.path)
        Just value -> pure value

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

data Listener = Listener
    { listenerSocket :: Socket
    , identity :: FileIdentity
    }

data FileIdentity = FileIdentity
    { device :: DeviceID
    , file :: FileID
    }
    deriving stock (Eq)

prepareSocketDirectory :: SocketConfig -> IO ()
prepareSocketDirectory config = do
    let directory = takeDirectory config.path
    createDirectoryIfMissing True directory
    descriptor <-
        openFd
            directory
            ReadOnly
            defaultFileFlags {nofollow = True, cloexec = True, directory = True}
    (do
            status <- getFdStatus descriptor
            effectiveUser <- getEffectiveUserID
            unless (isDirectory status && fileOwner status == effectiveUser) $
                throwString ("runtime socket directory is not a user-owned directory: " <> directory)
            setFdMode descriptor 0o700
        )
        `finally` closeFd descriptor

prepareLockFile :: FilePath -> IO ()
prepareLockFile path = do
    descriptor <-
        openFd path ReadWrite defaultFileFlags {creat = Just 0o600, nofollow = True, cloexec = True}
    (do
            status <- getFdStatus descriptor
            effectiveUser <- getEffectiveUserID
            unless (isRegularFile status && fileOwner status == effectiveUser) $
                throwString ("runtime lock is not a user-owned regular file: " <> path)
            setFdMode descriptor 0o600
        )
        `finally` closeFd descriptor

openListener :: SocketConfig -> IO Listener
openListener config = do
    removeStaleSocket config.path
    listener <- socket AF_UNIX Stream defaultProtocol
    (do
            bind listener (SockAddrUnix config.path)
            setFileMode config.path 0o600
            listen listener config.backlog
            status <- getSymbolicLinkStatus config.path
            pure Listener
                { listenerSocket = listener
                , identity = identityOf status
                }
        )
        `onException` close listener

closeListener :: SocketConfig -> Listener -> IO ()
closeListener config listener = do
    close listener.listenerSocket
    current <- tryIO (getSymbolicLinkStatus config.path)
    case current of
        Right status
            | isSocket status && identityOf status == listener.identity ->
                removeFile config.path `catch` \(_ :: IOException) -> pure ()
        _ -> pure ()

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

identityOf :: FileStatus -> FileIdentity
identityOf status =
    FileIdentity
        { device = deviceID status
        , file = fileID status
        }

lockPath :: SocketConfig -> FilePath
lockPath config = config.path <> ".lock"
