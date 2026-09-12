{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Same-user delivery of follow-up prompts to the process that already owns
-- a persisted session. The wire format is private, explicitly versioned, and
-- accepts only user-message envelopes — never execution commands.
module Agent.Runtime.Session.Inbox
    ( SessionInbox
    , inboxUnavailableError
    , deliverSessionInboxMessage
    , deliverSessionInboxMessageAt
    , inboxSocketPath
    , newSessionInbox
    , releaseInboxPending
    , takeInboxMessage
    , withOptionalSessionInboxServer
    , withSessionInboxServerAt
    ) where

import Agent.Runtime.SessionLock (adjustSessionInboxPending)
import qualified Agent.Runtime.Session.Framing as Framing
import Control.Concurrent.Async (mapConcurrently_, withAsync)
import Control.Concurrent.STM
import Control.Exception.Safe
    ( IOException, bracket, bracketOnError, catch, finally, throwIO, try, tryAny )
import Control.Monad (forever, unless, void, when)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict', encode)
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.Generics (Generic)
import Network.Socket
import qualified Network.Socket.ByteString as Socket
import System.Directory (createDirectoryIfMissing, getHomeDirectory, removeFile)
import qualified System.FileLock as FileLock
import System.FilePath (isAbsolute, (</>))
import System.OsPath (OsPath)
import System.Posix.Files
    ( fileMode, fileOwner, getSymbolicLinkStatus, isDirectory, isSocket
    , setFileMode
    )
import System.Posix.User (getEffectiveUserID)
import System.Timeout (timeout)
import System.Environment (lookupEnv)

inboxUnavailableError :: Text
inboxUnavailableError = "session inbox is unavailable"

inboxMessageCountLimit :: Int
inboxMessageCountLimit = 32

inboxMessageByteLimit :: Int
inboxMessageByteLimit = 1024 * 1024

data SessionInbox = SessionInbox
    { inboxQueue :: !(TBQueue Text)
    , inboxSessionDir :: !(IORef (Maybe OsPath))
    }

data InboxEnvelope = InboxEnvelope
    { version :: !Int
    , sessionID :: !Text
    , message :: !Text
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data InboxReply = InboxReply
    { version :: !Int
    , accepted :: !Bool
    , failure :: !(Maybe Text)
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

newSessionInbox :: IO SessionInbox
newSessionInbox =
    SessionInbox
        <$> newTBQueueIO (fromIntegral inboxMessageCountLimit)
        <*> newIORef Nothing

takeInboxMessage :: SessionInbox -> STM Text
takeInboxMessage inbox = readTBQueue inbox.inboxQueue

releaseInboxPending :: SessionInbox -> IO ()
releaseInboxPending inbox =
    readIORef inbox.inboxSessionDir >>= mapM_ \sessionDir ->
        void (adjustSessionInboxPending sessionDir (-1))

inboxDirectory :: IO FilePath
inboxDirectory = do
    home <- getHomeDirectory
    configured <- lookupEnv "HASKELL_AGENT_INBOX_DIRECTORY"
    pure (fromMaybe (home </> ".haskell-agent" </> "inbox") configured)

inboxSocketPath :: FilePath -> Text -> FilePath
inboxSocketPath directory sessionID =
    directory </> take 16 (show (hash (Text.encodeUtf8 sessionID) :: Digest SHA256))

-- | Observation-style optional service: failure to open the endpoint must not
-- prevent execution. Exceptions from the action or cleanup still propagate.
withOptionalSessionInboxServer
    :: SessionInbox -> Text -> OsPath -> IO a -> IO a
withOptionalSessionInboxServer inbox sessionID sessionDir action = do
    entered <- newIORef False
    directory <- inboxDirectory
    withSessionInboxServerAt directory inbox sessionID sessionDir (do
        writeIORef entered True
        action)
        `catch` \(exception :: IOException) -> do
            hasEntered <- readIORef entered
            if hasEntered
                then throwIO exception
                else action

withSessionInboxServerAt
    :: FilePath
    -> SessionInbox
    -> Text
    -> OsPath
    -> IO a
    -> IO a
withSessionInboxServerAt directory inbox sessionID sessionDir action = do
    validateSocketPath directory (inboxSocketPath directory sessionID)
    createDirectoryIfMissing True directory
    validateOwnedDirectory directory
    setFileMode directory 0o700
    writeIORef inbox.inboxSessionDir (Just sessionDir)
    let path = inboxSocketPath directory sessionID
    bracket (FileLock.tryLockFile (path <> ".lock") FileLock.Exclusive)
        (mapM_ FileLock.unlockFile) \case
            Nothing -> throwIO (userError "Session inbox is already owned.")
            Just _ -> do
                setFileMode (path <> ".lock") 0o600
                removeOwnedSocket path
                bracket (openServer path)
                    (\server -> close server `finally` removeOwnedSocket path)
                    \server ->
                        withAsync
                            (mapConcurrently_
                                (\_ -> serveInbox server inbox sessionID sessionDir)
                                [1 :: Int .. 4])
                            (const action)

serveInbox :: Socket -> SessionInbox -> Text -> OsPath -> IO ()
serveInbox server inbox sessionID sessionDir = forever $
    bracket (fst <$> accept server) close \client ->
        void $ try @_ @IOException do
            envelope <- receiveEnvelope client
            reply <- handleEnvelope inbox sessionID sessionDir envelope
            sendEnvelope client reply

handleEnvelope
    :: SessionInbox
    -> Text
    -> OsPath
    -> InboxEnvelope
    -> IO InboxReply
handleEnvelope inbox sessionID sessionDir envelope
    | envelope.version /= 1 =
        pure (rejectReply "Unsupported session inbox protocol.")
    | envelope.sessionID /= sessionID =
        pure (rejectReply "Session inbox identifier mismatch.")
    | Text.null (Text.strip envelope.message) =
        pure (rejectReply "session inbox requires a non-empty message")
    | BS.length (Text.encodeUtf8 envelope.message) > inboxMessageByteLimit =
        pure (rejectReply "session inbox message exceeds the size limit")
    | otherwise = do
        queued <- enqueueInboxMessage inbox sessionDir envelope.message
        pure $ if queued
            then InboxReply { version = 1, accepted = True, failure = Nothing }
            else rejectReply "session inbox is full"

enqueueInboxMessage :: SessionInbox -> OsPath -> Text -> IO Bool
enqueueInboxMessage inbox sessionDir message = do
    _ <- adjustSessionInboxPending sessionDir 1
    accepted <- atomically do
        full <- isFullTBQueue inbox.inboxQueue
        if full
            then pure False
            else writeTBQueue inbox.inboxQueue message >> pure True
    unless accepted $
        void (adjustSessionInboxPending sessionDir (-1))
    pure accepted

rejectReply :: Text -> InboxReply
rejectReply message =
    InboxReply { version = 1, accepted = False, failure = Just message }

deliverSessionInboxMessage :: Text -> Text -> IO (Either Text ())
deliverSessionInboxMessage sessionID message =
    inboxDirectory >>= \directory ->
        deliverSessionInboxMessageAt directory sessionID message

deliverSessionInboxMessageAt
    :: FilePath -> Text -> Text -> IO (Either Text ())
deliverSessionInboxMessageAt directory sessionID message
    | Text.null (Text.strip message) =
        pure (Left "send_agent_session_message requires a non-empty message")
    | BS.length (Text.encodeUtf8 message) > inboxMessageByteLimit =
        pure (Left "session inbox message exceeds the size limit")
    | otherwise = do
        let path = inboxSocketPath directory sessionID
            envelope = InboxEnvelope
                { version = 1
                , sessionID
                , message
                }
        outcome <- tryAny $
            timeout 1000000 $
                bracket (socket AF_UNIX Stream defaultProtocol) close \client -> do
                    validateSocketPath directory path
                    validateOwnedDirectory directory
                    directoryStatus <- getSymbolicLinkStatus directory
                    unless (fileMode directoryStatus .&. 0o077 == 0) $
                        throwIO (userError "Inbox directory is not private.")
                    validatePrivateSocket path
                    connect client (SockAddrUnix path)
                    sendEnvelope client envelope
                    receiveReply client
        pure $ case outcome of
            Left _ -> Left inboxUnavailableError
            Right Nothing -> Left inboxUnavailableError
            Right (Just reply)
                | reply.version == 1 && reply.accepted -> Right ()
                | reply.version == 1 ->
                    Left (fromMaybe inboxUnavailableError reply.failure)
                | otherwise -> Left inboxUnavailableError

openServer :: FilePath -> IO Socket
openServer path =
    bracketOnError (socket AF_UNIX Stream defaultProtocol) close \server -> do
        bind server (SockAddrUnix path)
        setFileMode path 0o600
        listen server 8
        pure server

validateOwnedDirectory :: FilePath -> IO ()
validateOwnedDirectory path = do
    status <- getSymbolicLinkStatus path
    uid <- getEffectiveUserID
    unless (isDirectory status && fileOwner status == uid) $
        throwIO (userError "Inbox directory is not owned by the current user.")

validateSocketPath :: FilePath -> FilePath -> IO ()
validateSocketPath directory path = do
    unless (isAbsolute directory && BS.length (Text.encodeUtf8 (Text.pack path)) < 104) $
        throwIO (userError
            "Inbox socket path must be absolute and shorter than 104 UTF-8 bytes; configure HASKELL_AGENT_INBOX_DIRECTORY.")

validatePrivateSocket :: FilePath -> IO ()
validatePrivateSocket path = do
    status <- getSymbolicLinkStatus path
    uid <- getEffectiveUserID
    unless (isSocket status && fileOwner status == uid && fileMode status .&. 0o077 == 0) $
        throwIO (userError "Inbox socket is not private to the current user.")

removeOwnedSocket :: FilePath -> IO ()
removeOwnedSocket path = do
    result <- try @_ @IOException (validatePrivateSocket path)
    case result of
        Left _ -> pure ()
        Right () -> removeFile path

sendEnvelope :: ToJSON a => Socket -> a -> IO ()
sendEnvelope client value = do
    let bytes = LBS.toStrict (encode value)
    when (BS.length bytes > Framing.maximumFrameBytes) $
        throwIO (userError "Inbox envelope exceeds the size limit.")
    Framing.sendFrame client bytes

receiveEnvelope :: Socket -> IO InboxEnvelope
receiveEnvelope client =
    either (throwIO . userError) pure . eitherDecodeStrict'
        =<< receiveBounded client

receiveReply :: Socket -> IO InboxReply
receiveReply client =
    either (throwIO . userError) pure . eitherDecodeStrict'
        =<< receiveBounded client

receiveBounded :: Socket -> IO BS.ByteString
receiveBounded client =
    Framing.receiveFrame "Invalid inbox frame size." "Inbox owner disconnected."
        (Socket.recv client)
