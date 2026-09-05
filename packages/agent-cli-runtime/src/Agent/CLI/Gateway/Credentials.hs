-- | Restricted credential storage and the process/cross-process lease boundary.
module Agent.CLI.Gateway.Credentials
    ( gatewayCredentialPath
    , withGatewayCredentialLock
    , withGatewayCredentialLockAt
    , withGatewayCredentialLease
    , withGatewayCredentialLeaseAt
    , withGatewayCredentialTurnLease
    , withGatewayCredentialTurnLeaseAt
    , loadGatewayCredential
    , loadGatewayCredentialAt
    , saveGatewayCredential
    , saveGatewayCredentialAt
    , saveGatewayCredentialWith
    , removeGatewayCredential
    , removeGatewayCredentialWith
    , validateGatewayCredential
    ) where

import Agent.CLI.Gateway.Origin
    ( parseGatewayOrigin
    , validateBaseUrl
    , whenEither
    )
import Agent.CLI.PrivateFileLock
    ( withPrivateFileLock
    , withPrivateSharedFileLock
    , withPrivateSharedFileLocksAfterGate
    )
import Agent.FileRetry (retryOnFileBusy, writeLazyFileAtomically)
import Agent.Json.Decode qualified as Hermes
import Agent.OpenAI.WebSocketClient (validateGatewayWebSocketUrl)
import Agent.OsPath (unsafeToFilePath)
import Agent.Server.Client.GatewayIdentity (GatewayCredential(..))
import Control.Concurrent (ThreadId, myThreadId)
import Control.Concurrent.STM
    ( TVar
    , atomically
    , newTVarIO
    , readTVar
    , retry
    , writeTVar
    )
import Control.Exception.Safe (finally, mask, onException, throwString, tryAny)
import Control.Monad (when)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as Text
import System.Directory.OsPath qualified as Directory
import System.IO.Unsafe (unsafePerformIO)
import System.OsPath (OsPath, takeDirectory, unsafeEncodeUtf, (</>))
import System.Posix.Files (setFileMode)

gatewayCredentialDecoder :: Hermes.Decoder GatewayCredential
gatewayCredentialDecoder =
    Hermes.object $
        GatewayCredential
            <$> Hermes.atKey "base_url" Hermes.text
            <*> Hermes.atKey "websocket_url" Hermes.text
            <*> Hermes.atKey "access_token" Hermes.text

gatewayCredentialPath :: OsPath -> OsPath
gatewayCredentialPath home =
    home
        </> unsafeEncodeUtf ".haskell-agent"
        </> unsafeEncodeUtf "credentials"
        </> unsafeEncodeUtf "gateway.json"

gatewayCredentialLockPath :: OsPath -> OsPath
gatewayCredentialLockPath home =
    takeDirectory (gatewayCredentialPath home)
        </> unsafeEncodeUtf "gateway.lock"

gatewayCredentialTurnAdmissionPath :: OsPath -> OsPath
gatewayCredentialTurnAdmissionPath home =
    takeDirectory (gatewayCredentialPath home)
        </> unsafeEncodeUtf "gateway-turn-admission.lock"

gatewayCredentialTurnLeasePath :: OsPath -> OsPath
gatewayCredentialTurnLeasePath home =
    takeDirectory (gatewayCredentialPath home)
        </> unsafeEncodeUtf "gateway-turn.lock"

-- | Serialize gateway credential changes with operations whose authorization
-- depends on one exact credential snapshot. The process lock is required
-- because advisory file-lock behavior between threads in one process is
-- platform-dependent; the private file lock extends the boundary to CLI and
-- native application processes.
withGatewayCredentialLock :: IO value -> IO value
withGatewayCredentialLock action = do
    home <- Directory.getHomeDirectory
    withGatewayCredentialLockAt home action

withGatewayCredentialLockAt :: OsPath -> IO value -> IO value
withGatewayCredentialLockAt home action =
    withGatewayCredentialProcessWriteLock $
        withPrivateFileLock (gatewayCredentialTurnAdmissionPath home) $
            withPrivateFileLock (gatewayCredentialTurnLeasePath home) $
                withPrivateFileLock
                    (gatewayCredentialLockPath home)
                    action

-- | Hold a shared credential lease. Credential changes wait for every lease,
-- while independent session reads and native turns remain concurrent.
withGatewayCredentialLease :: IO value -> IO value
withGatewayCredentialLease action = do
    home <- Directory.getHomeDirectory
    withGatewayCredentialLeaseAt home action

withGatewayCredentialLeaseAt :: OsPath -> IO value -> IO value
withGatewayCredentialLeaseAt home action =
    withGatewayCredentialProcessReadLock True \needsFileLock ->
        if needsFileLock
            then
                withPrivateSharedFileLock
                    (gatewayCredentialLockPath home)
                    action
            else action

-- | Start a long-running native turn only if no credential writer is already
-- waiting. Unlike short callback leases, a new turn must not prolong an
-- organization transition by joining an existing reader phase.
withGatewayCredentialTurnLease :: IO value -> IO value
withGatewayCredentialTurnLease action = do
    home <- Directory.getHomeDirectory
    withGatewayCredentialTurnLeaseAt home action

withGatewayCredentialTurnLeaseAt :: OsPath -> IO value -> IO value
withGatewayCredentialTurnLeaseAt home action =
    withGatewayCredentialProcessReadLock False \_ ->
        withPrivateSharedFileLocksAfterGate
            (gatewayCredentialTurnAdmissionPath home)
            [ gatewayCredentialTurnLeasePath home
            -- Keep the original credential lock for rolling compatibility
            -- with an older process that does not know the admission protocol.
            , gatewayCredentialLockPath home
            ]
            action

data GatewayCredentialProcessLockState =
    GatewayCredentialProcessLockState
        { processLockReaders :: !Int
        , processLockWriterOwner :: !(Maybe ThreadId)
        , processLockWaitingWriters :: !Int
        }

gatewayCredentialProcessLock :: TVar GatewayCredentialProcessLockState
gatewayCredentialProcessLock =
    unsafePerformIO $
        newTVarIO
            GatewayCredentialProcessLockState
                { processLockReaders = 0
                , processLockWriterOwner = Nothing
                , processLockWaitingWriters = 0
                }
{-# NOINLINE gatewayCredentialProcessLock #-}

withGatewayCredentialProcessReadLock
    :: Bool
    -> (Bool -> IO value)
    -> IO value
withGatewayCredentialProcessReadLock mayJoinReaderPhase action =
    mask \restore -> do
        thread <- myThreadId
        needsFileLock <- atomically do
            state <- readTVar gatewayCredentialProcessLock
            case state.processLockWriterOwner of
                Just owner
                    | mayJoinReaderPhase && owner == thread ->
                        -- A terminal credential callback may synchronously
                        -- query the new state. The writer already owns the
                        -- process and file boundaries on this same thread.
                        pure False
                    | otherwise -> retry
                Nothing
                    -- Short readers may join an active reader phase. This
                    -- matters for native supervisors: a long-running turn can
                    -- need a boundary-checked approval or snapshot callback
                    -- before it can finish and release its lifetime lease.
                    -- Long turn leases pass False and wait behind the writer;
                    -- once the last reader leaves, every reader must wait.
                    | state.processLockWaitingWriters > 0
                            && ( not mayJoinReaderPhase
                                    || state.processLockReaders == 0
                               )
                        -> retry
                    | otherwise -> do
                        writeTVar
                            gatewayCredentialProcessLock
                            state
                                { processLockReaders =
                                    state.processLockReaders + 1
                                }
                        pure True
        if not needsFileLock
            then restore (action False)
            else restore (action True)
                `finally`
                    atomically do
                        state <- readTVar gatewayCredentialProcessLock
                        writeTVar
                            gatewayCredentialProcessLock
                            state
                                { processLockReaders =
                                    max 0 (state.processLockReaders - 1)
                                }

withGatewayCredentialProcessWriteLock :: IO value -> IO value
withGatewayCredentialProcessWriteLock action =
    mask \restore -> do
        thread <- myThreadId
        reentrant <- atomically do
            state <- readTVar gatewayCredentialProcessLock
            case state.processLockWriterOwner of
                Just owner
                    | owner == thread -> pure True
                _ -> do
                    writeTVar
                        gatewayCredentialProcessLock
                        state
                            { processLockWaitingWriters =
                                state.processLockWaitingWriters + 1
                            }
                    pure False
        if reentrant
            then
                throwString
                    "A gateway credential transition is already in progress."
            else do
                let unregisterWaitingWriter =
                        atomically do
                            state <- readTVar gatewayCredentialProcessLock
                            writeTVar
                                gatewayCredentialProcessLock
                                state
                                    { processLockWaitingWriters =
                                        max 0
                                            (state.processLockWaitingWriters - 1)
                                    }
                -- Keep the successful handoff masked. A blocked STM
                -- transaction is still interruptible, so cancellation can
                -- unregister the waiter; once it commits, no async exception
                -- can land before the finalizer is installed below.
                (atomically do
                        state <- readTVar gatewayCredentialProcessLock
                        if
                            state.processLockWriterOwner /= Nothing
                                || state.processLockReaders > 0
                        then retry
                        else
                            writeTVar
                                gatewayCredentialProcessLock
                                state
                                    { processLockWriterOwner = Just thread
                                    , processLockWaitingWriters =
                                        max 0
                                            (state.processLockWaitingWriters - 1)
                                    })
                    `onException` unregisterWaitingWriter
                restore action
                    `finally`
                        atomically do
                            state <- readTVar gatewayCredentialProcessLock
                            writeTVar
                                gatewayCredentialProcessLock
                                state { processLockWriterOwner = Nothing }

loadGatewayCredential :: IO (Either Text (Maybe GatewayCredential))
loadGatewayCredential = do
    home <- Directory.getHomeDirectory
    loadGatewayCredentialAt home

loadGatewayCredentialAt
    :: OsPath
    -> IO (Either Text (Maybe GatewayCredential))
loadGatewayCredentialAt home = do
    let path = gatewayCredentialPath home
    exists <- Directory.doesFileExist path
    if not exists
        then pure (Right Nothing)
        else do
            result <-
                retryOnFileBusy $
                    tryAny (LBS.readFile (unsafeToFilePath path))
            pure case result of
                Left exception -> Left (Text.pack (show exception))
                Right bytes ->
                    case Hermes.decodeEither gatewayCredentialDecoder (LBS.toStrict bytes) of
                        Left err -> Left (Hermes.jsonErrorMessage err)
                        Right credential ->
                            case validateGatewayCredential credential of
                                Left err -> Left err
                                Right () -> Right (Just credential)

saveGatewayCredentialAt :: OsPath -> GatewayCredential -> IO (Either Text ())
saveGatewayCredentialAt home credential =
    fmap (fmap (const ())) $
        saveGatewayCredentialAtWith home credential (pure ())

saveGatewayCredentialAtWith
    :: OsPath
    -> GatewayCredential
    -> IO value
    -> IO (Either Text value)
saveGatewayCredentialAtWith home credential afterSave =
    case validateGatewayCredential credential of
        Left err -> pure (Left err)
        Right () -> do
            result <- tryAny $
                withGatewayCredentialLockAt home do
                    let path = gatewayCredentialPath home
                        directory = takeDirectory path
                    Directory.createDirectoryIfMissing True directory
                    setFileMode (unsafeToFilePath directory) 0o700
                    writeLazyFileAtomically
                        path 0o600 (Aeson.encode credential)
                    afterSave
            pure case result of
                Left exception -> Left (Text.pack (show exception))
                Right value -> Right value

validateGatewayCredential :: GatewayCredential -> Either Text ()
validateGatewayCredential credential = do
    baseUrl <- validateBaseUrl credential.gatewayBaseUrl
    validateGatewayWebSocketUrl credential.gatewayWebSocketUrl
    baseOrigin <-
        parseGatewayOrigin
            "Gateway URL is invalid."
            baseUrl
    websocketOrigin <-
        parseGatewayOrigin
            "Gateway WebSocket URL is invalid."
            credential.gatewayWebSocketUrl
    let expectedWebSocketOrigin =
            case baseOrigin of
                ("https:", host, port) -> ("wss:", host, port)
                ("http:", host, port) -> ("ws:", host, port)
                origin -> origin
    whenEither
        (websocketOrigin /= expectedWebSocketOrigin)
        "Gateway base and WebSocket URLs must use the same origin."
    whenEither
        (Text.null (Text.strip credential.gatewayAccessToken))
        "Gateway access token cannot be empty."
    baseOrigin <-
        parseGatewayOrigin
            "Gateway credential contains an invalid base URL."
            baseUrl
    websocketOrigin <-
        parseGatewayOrigin
            "Gateway credential contains an invalid WebSocket URL."
            credential.gatewayWebSocketUrl
    let expectedWebSocketOrigin =
            case baseOrigin of
                ("https:", host, port) -> ("wss:", host, port)
                ("http:", host, port) -> ("ws:", host, port)
                origin -> origin
    whenEither
        (websocketOrigin /= expectedWebSocketOrigin)
        "Gateway credential WebSocket URL uses a different origin."

saveGatewayCredential :: GatewayCredential -> IO (Either Text ())
saveGatewayCredential credential =
    fmap (fmap (const ())) $
        saveGatewayCredentialWith credential (pure ())

saveGatewayCredentialWith
    :: GatewayCredential
    -> IO value
    -> IO (Either Text value)
saveGatewayCredentialWith credential afterSave =
    tryAny Directory.getHomeDirectory >>= \case
        Left exception -> pure (Left (Text.pack (show exception)))
        Right home ->
            saveGatewayCredentialAtWith home credential afterSave

removeGatewayCredential :: IO (Either Text ())
removeGatewayCredential =
    fmap (fmap (const ())) $
        removeGatewayCredentialWith (pure ())

removeGatewayCredentialWith :: IO value -> IO (Either Text value)
removeGatewayCredentialWith afterRemove = do
    result <- tryAny do
        home <- Directory.getHomeDirectory
        withGatewayCredentialLockAt home do
            let path = gatewayCredentialPath home
            exists <- Directory.doesFileExist path
            when exists (Directory.removeFile path)
            afterRemove
    pure case result of
        Left exception -> Left (Text.pack (show exception))
        Right value -> Right value
