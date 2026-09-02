-- | Persist REPL conversations under @~/.haskell-agent/sessions@.
module Agent.CLI.Session
    ( SessionHandle(..)
    , SessionMeta(..)
    , SessionPromptSnapshot(..)
    , sessionMetaDecoder
    , LegacySubagentTarget(..)
    , TranscriptEffect(..)
    , SessionTurn(..)
    , sessionTurnDecoder
    , SessionTurnPage(..)
    , SessionResumeStats(..)
    , SessionActivity(..)
    , sessionActivityDecoder
    , SessionTransfer(..)
    , sessionTransferDecoder
    , SessionTransferEnvelope(..)
    , sessionTransferFormatVersion
    , validateSessionTransferEnvelope
    , SessionCreate(..)
    , Persistence(..)
    , PersistenceState(..)
    , newPendingPersistence
    , newPendingPersistenceReserved
    , newActivePersistence
    , persistenceTempDir
    , setPersistenceActivity
    , clearPersistenceActivity
    , loadSessionActivity
    , cleanupPendingPersistence
    , createSession
    , forkSession
    , forkSessionAt
    , appendTurn
    , appendTurnIndexed
    , appendTurnWithMetaUpdate
    , appendTurnWithMetaUpdateIndexed
    , appendTurnWithPromptResetIndexed
    , appendTurnKeepTitle
    , appendTurnKeepTitleIndexed
    , sessionRewindChoices
    , rewindSession
    , addSessionUsage
    , deleteSession
    , renameSession
    , loadSession
    , loadSessions
    , loadActiveSession
    , loadSessionMeta
    , loadRecentSessionTurns
    , loadRecentSessionHistoryTurns
    , loadSessionTurnsBefore
    , loadSessionHistoryTurnsBefore
    , loadSessionTurnsAfter
    , loadSessionHistoryTurnsRange
    , loadSessionHistoryTurnsRangeBounded
    , loadSessionHistoryTurnsAround
    , loadSessionHistorySnapshot
    , loadSessionResumeStats
    , importSessionTransfer
    , importSessionTransferRemapped
    , exportSessionTransfer
    , streamSessionTransfer
    , forkSessionAtTurn
    , loadSessionHandle
    , isValidSessionId
    , listSessions
    , listArchivedSessionIds
    , sessionDirForId
    , sessionTempDirForId
    , sessionTempsRoot
    , allocateSessionTemp
    , ensureSessionTemp
    , removeSessionTemp
    , cleanupStaleSessionTemps
    , defaultSessionTempKeepCount
    , SessionTempCleanupReport(..)
    , SessionTempLease
    , acquireSessionTempLease
    , releaseSessionTempLease
    , sessionsRoot
    , sessionTitleFromPrompt
    , setGeneratedSessionTitle
    , setSessionArchived
    , setManualSessionTitle
    , resetSessionTitleToAuto
    , setSessionRecap
    , setSessionTurnSummary
    , sessionConversationText
    , sessionLegacySubagentTarget
    , sessionTitleTurnCountFromSlot
    , writeSessionMeta
    , compatibleSessionPromptSnapshot
    , ensureSession
    , ensurePersistenceSessionId
    , ensurePersistenceSessionIdWithMaterializationHook
    , ensureSessionWithPromptSnapshot
    , resumeHint
    , sessionUsageFromTurns
    ) where

import Agent.FileRetry (retryOnFileBusy, writeLazyFileAtomically)
import Agent.CLI.SessionLock
    ( acquireSessionLock
    , releaseSessionLock
    )
import Agent.CLI.Json (decodeLazy)
import Agent.CLI.ModelConfig (organizationGatewayConnectionId)
import Agent.CLI.Request
    ( requestPromptParts
    , requestToolIdentities
    )
import Agent.CLI.Session.Types
    ( SessionHandle(..)
    , SessionMeta(..)
    , SessionPromptSnapshot(..)
    , sessionMetaDecoder
    , LegacySubagentTarget(..)
    , TranscriptEffect(..)
    , SessionTurn(..)
    , sessionTurnDecoder
    , SessionTurnPage(..)
    , SessionResumeStats(..)
    , SessionActivity(..)
    , sessionActivityDecoder
    , SessionTransfer(..)
    , sessionTransferDecoder
    , SessionCreate(..)
    , Persistence(..)
    , PersistenceState(..)
    )
import Agent.CLI.Session.Codec
    ( contentFingerprint
    , decodeStoredSession
    , fromStoredMetadata
    , fromStoredTurn
    , importLegacySession
    , toStoredMetadata
    , toStoredPromptSnapshot
    , toStoredTurn
    , validateSessionMeta
    )
import Agent.CLI.Models (ModelTarget(..))
import Agent.CLI.Session.TitlePolicy (titleRefreshIndex)
import Agent.Dialect (DialectId)
import Agent.Loop (TokenUsage(..))
import Agent.OpenAI.Compaction (rewindSessionUserText)
import Agent.OsPath (toText, unsafeToFilePath)
import Agent.Provider (Provider)
import Agent.Responses.Types (ResponseCreateParams(model))
import Agent.Store.Postgres (normalizePostgresTimestamp)
import Agent.Store.Postgres.Connection (StorePool)
import qualified Agent.Store.Postgres.Session as Store
import Agent.Store.Types (StoreError, renderStoreError)
import Control.Applicative ((<|>))
import Control.Exception.Safe
    ( SomeException
    , displayException
    , finally
    , mask
    , onException
    , tryAny
    , tryIO
    )
import Control.Monad (foldM, guard, unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
    ( ExceptT(..)
    , except
    , runExceptT
    , throwE
    )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isHexDigit)
import Data.Int (Int64)
import Data.IORef
import Data.Functor ((<&>))
import Data.List (sortOn)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Ord (Down(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (Day)
import Data.Time.Clock
    ( UTCTime
    , getCurrentTime
    , nominalDiffTimeToSeconds
    , utctDay
    )
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import qualified Data.Vector as Vector
import Numeric (showHex)
import System.Directory.OsPath
    ( copyFile
    , createDirectory
    , createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , listDirectory
    , removePathForcibly
    , removeFile
    )
import qualified System.FileLock as FileLock
import System.OsPath
    ( OsPath
    , equalFilePath
    , normalise
    , takeDirectory
    , takeFileName
    , unsafeEncodeUtf
    , (</>)
    )
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files
    ( FileStatus
    , getSymbolicLinkStatus
    , isDirectory
    , isRegularFile
    , isSymbolicLink
    , setFileMode
    )

sessionSchemaVersion :: Int
sessionSchemaVersion = 1

-- | Keep a small recent cache for session artifacts while bounding abandoned
-- shell environments, tool outputs, and other scratch data.
defaultSessionTempKeepCount :: Int
defaultSessionTempKeepCount = 15

-- | Stable interchange envelope for full-fidelity session transfers.
data SessionTransferEnvelope = SessionTransferEnvelope
    { transferFormat :: !Text
    , transferFormatVersion :: !Int
    , transferSession :: !SessionTransfer
    }
    deriving (Eq, Show)

sessionTransferFormatVersion :: Int
sessionTransferFormatVersion = 1

sessionTransferFormatName :: Text
sessionTransferFormatName = "haskell-agent.session-transfer"

instance Aeson.ToJSON SessionTransferEnvelope where
    toJSON envelope = Aeson.object
        [ "format" Aeson..= envelope.transferFormat
        , "version" Aeson..= envelope.transferFormatVersion
        , "session" Aeson..= envelope.transferSession
        ]

instance Aeson.FromJSON SessionTransferEnvelope where
    parseJSON = Aeson.withObject "SessionTransferEnvelope" \object -> do
        envelope <- SessionTransferEnvelope
            <$> object Aeson..: "format"
            <*> object Aeson..: "version"
            <*> object Aeson..: "session"
        either (fail . Text.unpack) pure
            (validateSessionTransferEnvelope envelope)

validateSessionTransferEnvelope
    :: SessionTransferEnvelope
    -> Either Text SessionTransferEnvelope
validateSessionTransferEnvelope envelope
    | envelope.transferFormat /= sessionTransferFormatName =
        Left "unsupported session transfer format"
    | envelope.transferFormatVersion /= sessionTransferFormatVersion =
        Left
            ("unsupported session transfer version: "
                <> Text.pack (show envelope.transferFormatVersion))
    | not (isValidSessionId meta.metaId) =
        Left "invalid transferred session id"
    | meta.metaVersion <= 0 =
        Left "invalid transferred session schema version"
    | length turns > maximumTransferTurns =
        Left "session transfer contains too many turns"
    | any invalidTurn turns =
        Left "session transfer contains an oversized turn"
    | otherwise = Right envelope
  where
    transfer = envelope.transferSession
    meta = transfer.transferMeta
    turns = transfer.transferTurns
    invalidTurn turn =
        Text.length turn.turnUserText > maximumTransferTextLength
            || maybe False
                ((> maximumTransferTextLength) . Text.length)
                turn.turnAssistantText
            || maybe False
                ((> maximumTransferTextLength) . Text.length)
                turn.turnError

maximumTransferTurns :: Int
maximumTransferTurns = 100000

maximumTransferTextLength :: Int
maximumTransferTextLength = 16 * 1024 * 1024

data SessionTempCleanupReport = SessionTempCleanupReport
    { tempCleanupRemoved :: ![OsPath]
    , tempCleanupFailures :: ![(OsPath, Text)]
    }
    deriving (Eq, Show)

instance Semigroup SessionTempCleanupReport where
    left <> right = SessionTempCleanupReport
        { tempCleanupRemoved =
            left.tempCleanupRemoved <> right.tempCleanupRemoved
        , tempCleanupFailures =
            left.tempCleanupFailures <> right.tempCleanupFailures
        }

instance Monoid SessionTempCleanupReport where
    mempty = SessionTempCleanupReport [] []

newtype SessionTempLease = SessionTempLease FileLock.FileLock

-- | Reuse an immutable provider prefix only when the runtime target and the
-- ordered provider-visible tool identities still describe the same session.
-- Tool documentation/schema bytes may evolve between binaries; the persisted
-- versions remain authoritative until a tool is added, removed, reordered, or
-- renamed.
compatibleSessionPromptSnapshot
    :: Provider
    -> Text
    -> DialectId
    -> OsPath
    -> Maybe Text
    -> ResponseCreateParams
    -> Maybe SessionPromptSnapshot
    -> Maybe SessionPromptSnapshot
compatibleSessionPromptSnapshot
    provider connection dialect cwd sessionId params maybeSnapshot = do
        cacheKey <- sessionId
        snapshot <- maybeSnapshot
        let currentTools = snd (requestPromptParts params)
        guard (snapshot.promptSnapshotVersion == 1)
        guard (snapshot.promptSnapshotProvider == provider)
        guard (snapshot.promptSnapshotConnection == connection)
        guard (Just snapshot.promptSnapshotModel == params.model)
        guard (snapshot.promptSnapshotDialect == dialect)
        guard (snapshot.promptSnapshotCwd == cwd)
        guard (snapshot.promptSnapshotCacheKey == cacheKey)
        guard
            ( requestToolIdentities snapshot.promptSnapshotTools
                == requestToolIdentities currentTools
            )
        pure snapshot

-- | @~/.haskell-agent/sessions@ given the user's home directory.
sessionsRoot :: OsPath -> OsPath
sessionsRoot home =
    home </> unsafeEncodeUtf ".haskell-agent" </> unsafeEncodeUtf "sessions"

-- | @~/.haskell-agent/tmp/sessions@ for a corresponding sessions root.
sessionTempsRoot :: OsPath -> OsPath
sessionTempsRoot root =
    takeDirectory root
        </> unsafeEncodeUtf "tmp"
        </> unsafeEncodeUtf "sessions"

newPendingPersistence :: SessionCreate -> IO Persistence
newPendingPersistence spec = mask \restore -> do
    validateSessionCreateGatewayBoundary spec
    (sessionId, tempDir) <-
        restore (allocateSessionTemp spec.createRoot)
    restore (newPendingPersistenceReserved spec sessionId tempDir)
        `onException` do
            _ <- removeSessionTemp spec.createRoot sessionId
            pure ()

newPendingPersistenceReserved
    :: SessionCreate
    -> Text
    -> OsPath
    -> IO Persistence
newPendingPersistenceReserved spec sessionId tempDir = do
    validateSessionCreateGatewayBoundary spec
    expected <- either (fail . Text.unpack) pure
        (sessionTempDirForId spec.createRoot sessionId)
    unless (expected == tempDir) $
        fail "reserved session temp directory does not match session id"
    ensurePrivateDir tempDir
    PersistenceEnabled
        <$> newIORef (PersistencePending spec sessionId tempDir)

newActivePersistence :: SessionHandle -> IO Persistence
newActivePersistence handle = do
    ensurePrivateDir handle.sessionTempDir
    persistence <-
        PersistenceEnabled <$> newIORef (PersistenceActive handle)
    -- A prior process may have died while a cooldown/retry marker was active.
    -- Never attribute that stale activity to a newly resumed turn.
    clearPersistenceActivity persistence
    pure persistence

persistenceTempDir :: Persistence -> IO (Maybe OsPath)
persistenceTempDir = \case
    PersistenceDisabled -> pure Nothing
    PersistenceEnabled slotRef ->
        readIORef slotRef <&> \case
            PersistencePending _ _ tempDir -> Just tempDir
            PersistenceActive handle -> Just handle.sessionTempDir

setPersistenceActivity
    :: Persistence
    -> Text
    -> Text
    -> Maybe UTCTime
    -> IO ()
setPersistenceActivity persistence kind message retryAt =
    persistenceTempDir persistence >>= \case
        Nothing -> pure ()
        Just tempDir -> do
            _ <- tryIO do
                ensurePrivateDir tempDir
                now <- getCurrentTime
                writeLazyFileAtomically
                    (sessionActivityPath tempDir)
                    0o600
                    (Aeson.encode SessionActivity
                        { activityKind = kind
                        , activityMessage = message
                        , activityRetryAt = retryAt
                        , activityUpdatedAt = now
                        })
            pure ()

clearPersistenceActivity :: Persistence -> IO ()
clearPersistenceActivity persistence =
    persistenceTempDir persistence >>= mapM_ \tempDir -> do
        _ <- tryIO (removeFile (sessionActivityPath tempDir))
        pure ()

loadSessionActivity
    :: OsPath
    -> Text
    -> IO (Maybe SessionActivity)
loadSessionActivity root sessionId =
    case sessionTempDirForId root sessionId of
        Left _ -> pure Nothing
        Right tempDir -> do
            let path = sessionActivityPath tempDir
            exists <- doesFileExist path
            if not exists
                then pure Nothing
                else
                    tryIO (retryOnFileBusy (LBS.readFile (unsafeToFilePath path)))
                        <&> \case
                            Left _ -> Nothing
                            Right bytes ->
                                either (const Nothing) Just
                                    (decodeLazy sessionActivityDecoder bytes)

sessionActivityPath :: OsPath -> OsPath
sessionActivityPath tempDir =
    tempDir </> unsafeEncodeUtf "activity.json"

-- | Remove scratch space only when a reserved session never became durable.
cleanupPendingPersistence :: Persistence -> IO ()
cleanupPendingPersistence = \case
    PersistenceDisabled -> pure ()
    PersistenceEnabled slotRef ->
        readIORef slotRef >>= \case
            PersistencePending spec sessionId _ -> do
                _ <- removeSessionTemp spec.createRoot sessionId
                pure ()
            PersistenceActive _ -> pure ()

-- | Create durable session state using a store-owned pool.
--
-- This function does not acquire or own a database pool. Each database
-- operation uses the pool's bracketed 'Pool.use' path, while the enclosing
-- 'Store' owns and releases the pool.
createSession :: SessionCreate -> IO SessionHandle
createSession spec = do
    validateSessionCreateGatewayBoundary spec
    (sessionId, tempDir) <- allocateSessionTemp spec.createRoot
    createReservedSession spec sessionId tempDir Nothing
        `onException` removeReservedTemp spec.createRoot sessionId

-- | Clone a persisted session and its durable branch artifacts under a new
-- id. The returned handle is not locked or installed as the active session;
-- callers should use the same active-session handoff used by @/new@.
forkSession
    :: OsPath
    -> SessionHandle
    -> [SessionTurn]
    -> Maybe Text
    -> IO (Either Text SessionHandle)
forkSession root source turns requestedTitle =
    forkSessionAt
        root
        source
        turns
        requestedTitle
        source.sessionMeta.metaCwd

-- | Clone a persisted session while selecting the fork's working directory.
-- This is used by @/fork --worktree@; ordinary callers retain the source cwd
-- through 'forkSession'.
forkSessionAt
    :: OsPath
    -> SessionHandle
    -> [SessionTurn]
    -> Maybe Text
    -> OsPath
    -> IO (Either Text SessionHandle)
forkSessionAt root source turns requestedTitle targetCwd
    | not (any substantiveTurn (activeTranscriptTurns turns)) =
        pure (Left "a session must contain at least one turn before it can be forked")
    | otherwise = mask \restore -> do
        allocated <- tryIO (restore (allocateSessionTemp root))
        case allocated of
            Left err ->
                pure (Left ("could not reserve fork session: " <> exceptionText err))
            Right (sessionId, tempDir) ->
                case sessionDirForId root sessionId of
                    Left err -> do
                        cleanupForkFiles root sessionId Nothing
                        pure (Left err)
                    Right dir -> do
                        now <- normalizePostgresTimestamp <$> getCurrentTime
                        let meta =
                                forkedMetadata
                                    now
                                    sessionId
                                    requestedTitle
                                    targetCwd
                                    source.sessionMeta
                            storedMeta = toStoredMetadata meta
                            handle = SessionHandle
                                { sessionPool = source.sessionPool
                                , sessionDir = dir
                                , sessionTempDir = tempDir
                                , sessionMetaPath =
                                    dir </> unsafeEncodeUtf "meta.json"
                                , sessionTranscriptPath =
                                    dir </> unsafeEncodeUtf "transcript.jsonl"
                                , sessionMeta = meta
                                }
                            cleanupFiles =
                                cleanupForkFiles root sessionId (Just dir)
                            cleanupOwned = do
                                cleanupForkDatabaseIfOwned
                                    source.sessionPool
                                    storedMeta
                                cleanupFiles
                        prepared <- tryIO $
                            restore (prepareForkDirectory source.sessionDir dir)
                        case prepared of
                            Left err -> do
                                cleanupForkFiles root sessionId Nothing
                                pure
                                    (Left
                                        ("could not copy fork session artifacts: "
                                            <> exceptionText err))
                            Right () -> do
                                stored <-
                                    restore
                                        (Store.createSessionFromSnapshot
                                            source.sessionPool
                                            storedMeta
                                            (map toStoredTurn turns))
                                        `onException` cleanupOwned
                                case stored of
                                    Left err -> do
                                        cleanupOwned
                                        pure (Left (renderStoreError err))
                                    Right False -> do
                                        cleanupFiles
                                        pure
                                            (Left
                                                "could not allocate a unique PostgreSQL session id")
                                    Right True -> pure (Right handle)
  where
    exceptionText = Text.pack . displayException

substantiveTurn :: SessionTurn -> Bool
substantiveTurn turn =
    turn.turnEffect /= TranscriptReset
        && ( not (Text.null (Text.strip turn.turnUserText))
            || maybe False (not . Text.null . Text.strip)
                turn.turnAssistantText
            || maybe False (not . Text.null . Text.strip) turn.turnError
            || not (null turn.turnItems)
           )

activeTranscriptTurns :: [SessionTurn] -> [SessionTurn]
activeTranscriptTurns =
    reverse
        . takeWhile ((/= TranscriptReset) . (.turnEffect))
        . reverse

forkedMetadata
    :: UTCTime
    -> Text
    -> Maybe Text
    -> OsPath
    -> SessionMeta
    -> SessionMeta
forkedMetadata now sessionId requestedTitle targetCwd source =
    source
        { metaId = sessionId
        , metaCreatedAt = now
        , metaUpdatedAt = now
        , metaCwd = targetCwd
        , metaTitle = title
        , metaTitleIsManual =
            maybe source.metaTitleIsManual (const True) normalizedTitle
        , metaTitleRefreshIndex =
            maybe source.metaTitleRefreshIndex
                (const (max 2 source.metaTitleRefreshIndex))
                normalizedTitle
        , metaPromptSnapshot = Nothing
        }
  where
    normalizedTitle =
        requestedTitle
            >>= \raw ->
                let normalized = Text.unwords (Text.words raw)
                in if Text.null normalized then Nothing else Just normalized
    title = fromMaybe source.metaTitle normalizedTitle

prepareForkDirectory :: OsPath -> OsPath -> IO ()
prepareForkDirectory sourceDir destinationDir = do
    createDirectory destinationDir
    (do
        setFileMode (unsafeToFilePath destinationDir) 0o700
        copyOptionalArtifact
            ArtifactRegularFile
            (sourceDir </> unsafeEncodeUtf "plan.md")
            (destinationDir </> unsafeEncodeUtf "plan.md")
        copyOptionalArtifact
            ArtifactDirectory
            (sourceDir </> unsafeEncodeUtf "agents")
            (destinationDir </> unsafeEncodeUtf "agents"))
        `onException` do
            _ <- tryIO (removePathForcibly destinationDir)
            pure ()

data ArtifactKind
    = ArtifactRegularFile
    | ArtifactDirectory
    deriving (Eq)

copyOptionalArtifact :: ArtifactKind -> OsPath -> OsPath -> IO ()
copyOptionalArtifact expected source destination =
    symbolicLinkStatusMaybe source >>= \case
        Nothing -> pure ()
        Just status
            | isSymbolicLink status ->
                fail ("refusing to copy symbolic link: " <> Text.unpack (toText source))
            | expected == ArtifactRegularFile && isRegularFile status ->
                copyPrivateFile source destination
            | expected == ArtifactDirectory && isDirectory status ->
                copyPrivateDirectory source destination
            | otherwise ->
                fail ("unexpected fork artifact type: " <> Text.unpack (toText source))

copyPrivateDirectory :: OsPath -> OsPath -> IO ()
copyPrivateDirectory source destination = do
    createDirectory destination
    setFileMode (unsafeToFilePath destination) 0o700
    entries <- listDirectory source
    mapM_
        (\entry -> do
            let sourcePath = source </> entry
                destinationPath = destination </> entry
            status <- getSymbolicLinkStatus (unsafeToFilePath sourcePath)
            if isSymbolicLink status
                then
                    fail
                        ("refusing to copy symbolic link: "
                            <> Text.unpack (toText sourcePath))
                else if isDirectory status
                    then copyPrivateDirectory sourcePath destinationPath
                    else if isRegularFile status
                        then copyPrivateFile sourcePath destinationPath
                        else
                            fail
                                ("unexpected fork artifact type: "
                                    <> Text.unpack (toText sourcePath)))
        entries

copyPrivateFile :: OsPath -> OsPath -> IO ()
copyPrivateFile source destination = do
    copyFile source destination
    setFileMode (unsafeToFilePath destination) 0o600

symbolicLinkStatusMaybe :: OsPath -> IO (Maybe FileStatus)
symbolicLinkStatusMaybe path =
    tryIO (getSymbolicLinkStatus (unsafeToFilePath path)) >>= \case
        Left err
            | isDoesNotExistError err -> pure Nothing
            | otherwise -> ioError err
        Right status -> pure (Just status)

sessionDatabaseIsOwned
    :: StorePool
    -> Store.SessionMetadata
    -> IO Bool
sessionDatabaseIsOwned pool expected = do
    loaded <- Store.loadSessionMetadata pool expected.sessionMetadataKey
    pure (loaded == Right (Just expected))

cleanupForkDatabaseIfOwned
    :: StorePool
    -> Store.SessionMetadata
    -> IO ()
cleanupForkDatabaseIfOwned pool expected = do
    owned <- sessionDatabaseIsOwned pool expected
    when owned do
        now <- getCurrentTime
        _ <- Store.deleteSession pool expected.sessionMetadataKey now
        pure ()

cleanupForkFiles :: OsPath -> Text -> Maybe OsPath -> IO ()
cleanupForkFiles root sessionId dir = do
    mapM_
        (\path -> do
            _ <- tryIO (removePathForcibly path)
            pure ())
        dir
    _ <- removeSessionTemp root sessionId
    pure ()

createReservedSession
    :: SessionCreate
    -> Text
    -> OsPath
    -> Maybe SessionPromptSnapshot
    -> IO SessionHandle
createReservedSession spec sessionId tempDir promptSnapshot =
    createReservedSessionWithHandoff
        spec
        sessionId
        tempDir
        promptSnapshot
        (pure ())
        Nothing

-- | Create a durable session and publish it while async exceptions remain
-- masked. Stable metadata in host-owned recovery state makes the
-- interruptible PostgreSQL write idempotent: a later attempt can adopt an
-- exact committed row or retry the same reservation.
createReservedSessionWithHandoff
    :: SessionCreate
    -> Text
    -> OsPath
    -> Maybe SessionPromptSnapshot
    -> IO ()
    -> Maybe (SessionHandle -> IO ())
    -> IO SessionHandle
createReservedSessionWithHandoff
        spec
        sessionId
        tempDir
        promptSnapshot
        afterStored
        publication =
    mask \restore -> do
        validateSessionCreateGatewayBoundary spec
        let pool = spec.createPool
        ensurePrivateDir spec.createRoot
        dir <- either (fail . Text.unpack) pure
            (sessionDirForId spec.createRoot sessionId)
        recoveredMeta <- case publication of
            Nothing -> pure Nothing
            Just _ -> loadMaterializationMeta spec.createRoot sessionId
        case recoveredMeta of
            Nothing -> pure ()
            Just meta ->
                unless
                    ( meta.metaConnection
                        == spec.createTarget.targetConnectionId
                        && meta.metaGatewayIdentity
                            == spec.createGatewayIdentity
                    )
                    (fail
                        "materialized session gateway routing does not match \
                        \its creation boundary")
        now <- normalizePostgresTimestamp <$> getCurrentTime
        let title = case spec.createTitleHint of
                Just hint | not (Text.null hint) -> hint
                _ -> "untitled"
            generatedMeta = SessionMeta
                { metaVersion = sessionSchemaVersion
                , metaId = sessionId
                , metaCreatedAt = now
                , metaUpdatedAt = now
                , metaProvider = spec.createTarget.targetProvider
                , metaConnection = spec.createTarget.targetConnectionId
                , metaGatewayIdentity = spec.createGatewayIdentity
                , metaModel = spec.createTarget.targetModelId
                , metaTransportModel = Just spec.createTarget.targetWireModelId
                , metaDialect = spec.createTarget.targetDialect
                , metaLegacySubagentTarget = Just LegacySubagentTarget
                    { legacyTargetProvider = spec.createTarget.targetProvider
                    , legacyTargetConnection = spec.createTarget.targetConnectionId
                    , legacyTargetEffectiveModel =
                        spec.createTarget.targetWireModelId
                    , legacyTargetDialect = spec.createTarget.targetDialect
                    }
                , metaCwd = spec.createCwd
                , metaEffort = spec.createEffort
                , metaTitle = title
                , metaTitleIsManual = spec.createTitleIsManual
                , metaTitleRefreshIndex = 0
                , metaTitleUserTurns = 0
                , metaLastResponseId = Nothing
                , metaInputTokens = 0
                , metaOutputTokens = 0
                , metaCachedTokens = 0
                , metaLastRecap = Nothing
                , metaLastTurnSummary = Nothing
                , metaLastRecapMainTurns = 0
                , metaPromptSnapshot = promptSnapshot
                }
            meta = fromMaybe generatedMeta recoveredMeta
            handle = SessionHandle
                { sessionPool = pool
                , sessionDir = dir
                , sessionTempDir = tempDir
                , sessionMetaPath = dir </> unsafeEncodeUtf "meta.json"
                , sessionTranscriptPath = dir </> unsafeEncodeUtf "transcript.jsonl"
                , sessionMeta = meta
                }
            storedMeta = toStoredMetadata meta
            recoveryPath =
                sessionMaterializationMetaPath spec.createRoot sessionId
        case (publication, recoveredMeta) of
            (Just _, Nothing) ->
                writeLazyFileAtomically recoveryPath 0o600 (Aeson.encode meta)
            _ -> pure ()
        let createStored = case meta.metaPromptSnapshot of
                Nothing -> Store.createSession pool (toStoredMetadata meta)
                Just snapshot ->
                    Store.createSessionWithInitialPromptEpoch
                        pool
                        storedMeta
                        (toStoredPromptSnapshot snapshot)
            cleanupCreated =
                cleanupForkDatabaseIfOwned pool storedMeta
                    `finally` cleanupDirectory
            cleanupDirectory = do
                _ <- tryIO (removePathForcibly dir)
                pure ()
            publish = do
                -- The pending-to-active handoff remains masked after durable
                -- creation.
                case publication of
                    Nothing -> pure ()
                    Just handoff -> do
                        handoff handle
                        removeSessionMaterializationMeta
                            spec.createRoot
                            sessionId
                pure handle
            publishConfirmed = case publication of
                Nothing -> publish
                Just _ -> restore afterStored >> publish
            failStored err =
                fail
                    ("could not create PostgreSQL session: "
                        <> Text.unpack (renderStoreError err))
            failConflict =
                fail "could not allocate a unique PostgreSQL session id"
            adoptStored onMissing onConflict =
                restore
                    (Store.loadSessionMetadata pool sessionId) >>= \case
                        Right (Just actual)
                            | actual == storedMeta -> publishConfirmed
                        Right Nothing -> onMissing
                        Right (Just _) -> onConflict
                        Left err -> failStored err
            createAndPublish = do
                let exceptionCleanup = case publication of
                        Nothing -> cleanupCreated
                        Just _ -> pure ()
                created <-
                    restore
                        (do
                            setFileMode (unsafeToFilePath dir) 0o700
                            createStored)
                        `onException` exceptionCleanup
                case created of
                    Left err -> case publication of
                        Nothing -> cleanupCreated >> failStored err
                        Just _ ->
                            adoptStored
                                (failStored err)
                                (cleanupDirectory >> failConflict)
                    Right False -> case publication of
                        Nothing -> do
                            cleanupDirectory
                            failConflict
                        Just _ ->
                            adoptStored
                                (cleanupDirectory >> failConflict)
                                (cleanupDirectory >> failConflict)
                    Right True -> publishConfirmed
        case recoveredMeta of
            Nothing ->
                tryIO (createDirectory dir) >>= \case
                    Left err -> do
                        removeSessionMaterializationMeta
                            spec.createRoot
                            sessionId
                        ioError err
                    Right () -> createAndPublish
            Just _ -> do
                -- A previous attempt may have committed before interruption.
                -- Reuse its exact metadata so a missing row can be retried and
                -- an existing exact-match row can be adopted.
                ensureMaterializationDirectory dir
                adoptStored
                    createAndPublish
                    (cleanupDirectory >> failConflict)

sessionMaterializationMetaPath :: OsPath -> Text -> OsPath
sessionMaterializationMetaPath root sessionId =
    sessionTempsRoot root
        </> unsafeEncodeUtf
            (".materialization-" <> Text.unpack sessionId <> ".json")

removeSessionMaterializationMeta :: OsPath -> Text -> IO ()
removeSessionMaterializationMeta root sessionId = do
    _ <- tryIO (removeFile (sessionMaterializationMetaPath root sessionId))
    pure ()

ensureMaterializationDirectory :: OsPath -> IO ()
ensureMaterializationDirectory dir =
    symbolicLinkStatusMaybe dir >>= \case
        Nothing -> do
            createDirectory dir
            setFileMode (unsafeToFilePath dir) 0o700
        Just status
            | isDirectory status && not (isSymbolicLink status) ->
                setFileMode (unsafeToFilePath dir) 0o700
            | otherwise ->
                fail "reserved session path is not a private directory"

loadMaterializationMeta
    :: OsPath
    -> Text
    -> IO (Maybe SessionMeta)
loadMaterializationMeta root sessionId = do
    let path = sessionMaterializationMetaPath root sessionId
    symbolicLinkStatusMaybe path >>= \case
        Nothing -> pure Nothing
        Just status
            | isRegularFile status && not (isSymbolicLink status) -> do
                bytes <-
                    retryOnFileBusy (LBS.readFile (unsafeToFilePath path))
                meta <- either (fail . Text.unpack) pure
                    (decodeLazy sessionMetaDecoder bytes)
                unless (meta.metaId == sessionId) $
                    fail
                        "materialization metadata session id does not match reservation"
                unless (meta.metaVersion == sessionSchemaVersion) $
                    fail "unsupported materialization metadata version"
                pure (Just meta)
            | otherwise ->
                fail "materialization metadata is not a private regular file"

validateSessionCreateGatewayBoundary :: SessionCreate -> IO ()
validateSessionCreateGatewayBoundary spec =
    unless (targetsGateway == hasGatewayIdentity) $
        fail
            "session gateway identity does not match its routing connection"
  where
    targetsGateway =
        spec.createTarget.targetConnectionId == organizationGatewayConnectionId
    hasGatewayIdentity = isJust spec.createGatewayIdentity

-- | Create the session directory on first use when persistence is still pending.
ensureSession :: IORef PersistenceState -> IO SessionHandle
ensureSession = ensureSessionWithMaterializationHook (pure ())

ensureSessionWithMaterializationHook
    :: IO ()
    -> IORef PersistenceState
    -> IO SessionHandle
ensureSessionWithMaterializationHook afterStored slotRef = do
    slot <- readIORef slotRef
    case slot of
        PersistenceActive handle -> pure handle
        PersistencePending spec sessionId tempDir ->
            createReservedSessionWithHandoff
                spec
                sessionId
                tempDir
                Nothing
                afterStored
                (Just (writeIORef slotRef . PersistenceActive))

-- | Return a durable, resumable session ID, materializing pending persistence.
ensurePersistenceSessionId :: Persistence -> IO (Maybe Text)
ensurePersistenceSessionId =
    ensurePersistenceSessionIdWithMaterializationHook (pure ())

-- | Instrument the boundary after durable ownership is confirmed and before
-- the masked pending-to-active handoff.
ensurePersistenceSessionIdWithMaterializationHook
    :: IO ()
    -> Persistence
    -> IO (Maybe Text)
ensurePersistenceSessionIdWithMaterializationHook _ PersistenceDisabled =
    pure Nothing
ensurePersistenceSessionIdWithMaterializationHook
        afterStored
        (PersistenceEnabled slotRef) = do
    handle <- ensureSessionWithMaterializationHook afterStored slotRef
    pure (Just handle.sessionMeta.metaId)

-- | Ensure the durable session exists and atomically persist the
-- provider-visible request prefix before it can be sent. Subsequent calls only
-- append an immutable epoch when the prefix or pending generated context
-- actually changes.
ensureSessionWithPromptSnapshot
    :: IORef PersistenceState
    -> SessionPromptSnapshot
    -> IO SessionHandle
ensureSessionWithPromptSnapshot slotRef candidate = do
    slot <- readIORef slotRef
    handle <- case slot of
        PersistencePending spec sessionId tempDir ->
            createReservedSessionWithHandoff
                spec
                sessionId
                tempDir
                (Just candidate)
                (pure ())
                (Just (writeIORef slotRef . PersistenceActive))
        PersistenceActive active -> pure active
    let snapshot =
            maybe candidate
                (`mergePromptSnapshotContext` candidate)
                handle.sessionMeta.metaPromptSnapshot
    case handle.sessionMeta.metaPromptSnapshot of
        Just previous
            | promptSnapshotsEquivalent previous snapshot ->
                pure handle
        _ -> do
            Store.appendSessionPromptEpoch
                handle.sessionPool
                handle.sessionMeta.metaId
                (toStoredPromptSnapshot snapshot) >>= \case
                    Left err ->
                        fail
                            ("could not persist PostgreSQL prompt epoch: "
                                <> Text.unpack (renderStoreError err))
                    Right Nothing ->
                        fail
                            ("session not found: "
                                <> Text.unpack handle.sessionMeta.metaId)
                    Right (Just _) -> do
                        let next = handle
                                { sessionMeta = handle.sessionMeta
                                    { metaPromptSnapshot = Just snapshot
                                    }
                                }
                        writeIORef slotRef (PersistenceActive next)
                        pure next

mergePromptSnapshotContext
    :: SessionPromptSnapshot
    -> SessionPromptSnapshot
    -> SessionPromptSnapshot
mergePromptSnapshotContext previous candidate
    | promptSnapshotsSharePrefix previous candidate =
        candidate
            { promptSnapshotGeneratedContext =
                candidate.promptSnapshotGeneratedContext
                    <|> previous.promptSnapshotGeneratedContext
            , promptSnapshotGrokContext =
                candidate.promptSnapshotGrokContext
                    <|> previous.promptSnapshotGrokContext
            }
    | otherwise = candidate

promptSnapshotsSharePrefix
    :: SessionPromptSnapshot
    -> SessionPromptSnapshot
    -> Bool
promptSnapshotsSharePrefix left right =
    left.promptSnapshotVersion == right.promptSnapshotVersion
        && left.promptSnapshotProvider == right.promptSnapshotProvider
        && left.promptSnapshotConnection == right.promptSnapshotConnection
        && left.promptSnapshotModel == right.promptSnapshotModel
        && left.promptSnapshotDialect == right.promptSnapshotDialect
        && left.promptSnapshotCwd == right.promptSnapshotCwd
        && left.promptSnapshotInstructions == right.promptSnapshotInstructions
        && left.promptSnapshotTools == right.promptSnapshotTools
        && left.promptSnapshotCacheKey == right.promptSnapshotCacheKey

promptSnapshotsEquivalent
    :: SessionPromptSnapshot
    -> SessionPromptSnapshot
    -> Bool
promptSnapshotsEquivalent left right =
    promptSnapshotsSharePrefix left right
        && left.promptSnapshotGeneratedContext
            == right.promptSnapshotGeneratedContext
        && left.promptSnapshotGrokContext == right.promptSnapshotGrokContext

appendTurn :: SessionHandle -> SessionTurn -> IO SessionHandle
appendTurn handle turn =
    appendTurnWithMetaUpdate handle turn id

appendTurnIndexed :: SessionHandle -> SessionTurn -> IO (SessionHandle, Int64)
appendTurnIndexed handle turn =
    appendTurnWithMetaUpdateIndexed handle turn id

-- | Append one transcript turn, then apply an additional metadata transition
-- before the append's single metadata write.
appendTurnWithMetaUpdate
    :: SessionHandle
    -> SessionTurn
    -> (SessionMeta -> SessionMeta)
    -> IO SessionHandle
appendTurnWithMetaUpdate handle turn updateMeta =
    appendTurnWithMetaTransition handle turn
        (updateMeta . applyTurnMetadata turn)

appendTurnWithMetaUpdateIndexed
    :: SessionHandle
    -> SessionTurn
    -> (SessionMeta -> SessionMeta)
    -> IO (SessionHandle, Int64)
appendTurnWithMetaUpdateIndexed handle turn updateMeta =
    appendTurnWithMetaTransitionIndexed handle turn
        (updateMeta . applyTurnMetadata turn)

-- | Append a transcript turn and persist one metadata transition. Timestamp
-- and response-id updates are common to every kind of turn; the supplied
-- transition controls whether title, usage, or caller-specific metadata is
-- changed.
appendTurnWithMetaTransition
    :: SessionHandle
    -> SessionTurn
    -> (SessionMeta -> SessionMeta)
    -> IO SessionHandle
appendTurnWithMetaTransition handle turn transition = do
    fst <$> appendTurnWithMetaTransitionIndexed handle turn transition

appendTurnWithMetaTransitionIndexed
    :: SessionHandle
    -> SessionTurn
    -> (SessionMeta -> SessionMeta)
    -> IO (SessionHandle, Int64)
appendTurnWithMetaTransitionIndexed =
    appendTurnWithMetaTransitionIndexedUsing Store.appendSessionTurnIndexed

-- | Append a turn while atomically retiring the current provider-visible
-- prompt epoch. The returned handle has no prompt snapshot, so its next
-- request establishes a fresh stable prefix.
appendTurnWithPromptResetIndexed
    :: SessionHandle
    -> SessionTurn
    -> (SessionMeta -> SessionMeta)
    -> IO (SessionHandle, Int64)
appendTurnWithPromptResetIndexed handle turn transition =
    appendTurnWithMetaTransitionIndexedUsing
        Store.appendSessionTurnIndexedWithPromptReset
        handle
        turn
        (\meta ->
            (transition meta)
                { metaPromptSnapshot = Nothing
                })

appendTurnWithMetaTransitionIndexedUsing
    :: ( StorePool
        -> Store.SessionTurn
        -> Store.SessionMetadata
        -> IO (Either StoreError (Maybe Int64))
       )
    -> SessionHandle
    -> SessionTurn
    -> (SessionMeta -> SessionMeta)
    -> IO (SessionHandle, Int64)
appendTurnWithMetaTransitionIndexedUsing
    appendStoredTurn
    handle
    turn
    transition = do
    let pool = handle.sessionPool
    now <- normalizePostgresTimestamp <$> getCurrentTime
    let meta0 = handle.sessionMeta
        meta = meta0
            { metaUpdatedAt = now
            , metaLastResponseId = turn.turnResponseId <|> meta0.metaLastResponseId
            }
        finalMeta = transition meta
    appendStoredTurn
        pool
        (toStoredTurn turn)
        (toStoredMetadata finalMeta) >>= \case
            Left err ->
                fail
                    ("could not append PostgreSQL session turn: "
                        <> Text.unpack (renderStoreError err))
            Right Nothing ->
                fail ("session not found: " <> Text.unpack finalMeta.metaId)
            Right (Just turnIndex) ->
                pure (handle { sessionMeta = finalMeta }, turnIndex)

applyTurnMetadata :: SessionTurn -> SessionMeta -> SessionMeta
applyTurnMetadata turn meta =
    meta
        { metaTitle =
            if meta.metaTitle == "untitled" && not (Text.null turn.turnUserText)
                then sessionTitleFromPrompt turn.turnUserText
                else meta.metaTitle
        , metaInputTokens =
            meta.metaInputTokens + maybe 0 (.inputTokens) turn.turnUsage
        , metaOutputTokens =
            meta.metaOutputTokens + maybe 0 (.outputTokens) turn.turnUsage
        , metaCachedTokens =
            meta.metaCachedTokens + maybe 0 (.cachedTokens) turn.turnUsage
        }

-- | Persist provider usage that is not represented by its own transcript
-- turn, such as an inline compaction request. Session metadata is the
-- authoritative aggregate used when resuming.
addSessionUsage :: TokenUsage -> SessionHandle -> IO SessionHandle
addSessionUsage usage handle = do
    let pool = handle.sessionPool
    now <- normalizePostgresTimestamp <$> getCurrentTime
    let meta0 = handle.sessionMeta
        meta = meta0
            { metaUpdatedAt = now
            , metaInputTokens =
                meta0.metaInputTokens + usage.inputTokens
            , metaOutputTokens =
                meta0.metaOutputTokens + usage.outputTokens
            , metaCachedTokens =
                meta0.metaCachedTokens + usage.cachedTokens
            }
    Store.replaceSessionMetadata
        pool
        "session.usage_added"
        (toStoredMetadata meta) >>= \case
            Left err ->
                fail
                    ("could not update PostgreSQL session usage: "
                        <> Text.unpack (renderStoreError err))
            Right False ->
                fail ("session not found: " <> Text.unpack meta.metaId)
            Right True ->
                pure handle { sessionMeta = meta }

-- | The original root target under which metadata-less child transcripts may
-- have been created. Old session files derive it from their persisted root
-- target; once written, it remains stable across root model/provider changes.
sessionLegacySubagentTarget :: SessionMeta -> LegacySubagentTarget
sessionLegacySubagentTarget meta =
    fromMaybe
        LegacySubagentTarget
            { legacyTargetProvider = meta.metaProvider
            , legacyTargetConnection = meta.metaConnection
            , legacyTargetEffectiveModel =
                fromMaybe meta.metaModel meta.metaTransportModel
            , legacyTargetDialect = meta.metaDialect
            }
        meta.metaLegacySubagentTarget

sessionConversationText :: [SessionTurn] -> Text
sessionConversationText =
    Text.intercalate "\n\n" . foldMap renderTurn
  where
    renderTurn turn =
        [ "User:\n" <> turn.turnUserText ]
            <> case turn.turnAssistantText of
                Just text | not (Text.null (Text.strip text)) ->
                    ["Assistant:\n" <> text]
                _ -> []

sessionTitleTurnCountFromSlot
    :: Persistence
    -> IO Int
sessionTitleTurnCountFromSlot = \case
    PersistenceDisabled -> pure 0
    PersistenceEnabled slotRef ->
        readIORef slotRef >>= \case
            PersistencePending _ _ _ -> pure 0
            PersistenceActive handle -> pure handle.sessionMeta.metaTitleUserTurns

-- | Append a synthetic marker without deriving a title or aggregating usage.
-- Used for markers such as @/new@ and @/clear@.
appendTurnKeepTitle :: SessionHandle -> SessionTurn -> IO SessionHandle
appendTurnKeepTitle handle turn =
    appendTurnWithMetaTransition handle turn id

appendTurnKeepTitleIndexed
    :: SessionHandle
    -> SessionTurn
    -> IO (SessionHandle, Int64)
appendTurnKeepTitleIndexed handle turn =
    appendTurnWithMetaTransitionIndexed handle turn id

-- | Prompts in the current immutable transcript branch, paired with the turns
-- that should remain when rewinding to immediately before that prompt.
sessionRewindChoices :: [SessionTurn] -> [(SessionTurn, [SessionTurn])]
sessionRewindChoices turns =
    [ (turn, take turnIndex active)
    | (turnIndex, turn) <- zip [0 ..] active
    , isRewindPromptTurn turn
    ]
  where
    active =
        reverse
            (takeWhile
                ((/= TranscriptReset) . (.turnEffect))
                (reverse turns))

-- | Publish a rewound conversation branch without mutating historical turns.
--
-- The reset marker and retained prefix are appended atomically. Replayed
-- compaction checkpoints keep their replace effect, so model context resumes
-- from the correct compacted suffix while the full visual prefix stays
-- scrollable.
rewindSession
    :: SessionHandle
    -> [SessionTurn]
    -> IO (Either Text SessionHandle)
rewindSession handle retained = do
    now <- normalizePostgresTimestamp <$> getCurrentTime
    let promptCount = length (filter isRewindPromptTurn retained)
        meta0 = handle.sessionMeta
        meta = meta0
            { metaUpdatedAt = now
            , metaLastResponseId = retainedLastResponseId retained
            , metaTitleRefreshIndex =
                min
                    meta0.metaTitleRefreshIndex
                    (titleRefreshIndex promptCount)
            , metaTitleUserTurns = promptCount
            , metaLastRecap = Nothing
            , metaLastTurnSummary = Nothing
            , metaLastRecapMainTurns = 0
            }
        marker = SessionTurn
            { turnAt = now
            , turnUserText = rewindSessionUserText
            , turnAssistantText = Just "Conversation rewound."
            , turnError = Nothing
            , turnResponseId = Nothing
            , turnEffect = TranscriptReset
            , turnItems = []
            , turnDisplayItems = []
            , turnUsage = Nothing
            , turnProviderTelemetry = []
            }
    Store.appendSessionTurns
        handle.sessionPool
        (map toStoredTurn (marker : retained))
        (toStoredMetadata meta) >>= \case
            Left err ->
                pure
                    (Left
                        ("could not rewind PostgreSQL session: "
                            <> renderStoreError err))
            Right False ->
                pure (Left ("session not found: " <> meta.metaId))
            Right True ->
                pure (Right handle { sessionMeta = meta })

isRewindPromptTurn :: SessionTurn -> Bool
isRewindPromptTurn turn =
    turn.turnEffect == TranscriptAppend
        && not (Text.null (Text.strip turn.turnUserText))

retainedLastResponseId :: [SessionTurn] -> Maybe Text
retainedLastResponseId = foldl' step Nothing
  where
    step responseId turn = case turn.turnEffect of
        TranscriptAppend -> turn.turnResponseId <|> responseId
        TranscriptReplace -> turn.turnResponseId
        TranscriptReset -> turn.turnResponseId


loadSession
    :: StorePool
    -> OsPath
    -> Text
    -> IO (Either Text (SessionMeta, [SessionTurn]))
loadSession pool root sessionId = runExceptT do
    _ <- except (sessionDirForId root sessionId)
    stored <- loadWithLegacyImport root pool sessionId Store.loadSession
    storedPrompt <- loadStoredPromptEpoch pool sessionId
    decodeStoredSession
        sessionSchemaVersion
        isValidSessionId
        sessionId
        storedPrompt
        stored

loadActiveSession
    :: StorePool
    -> OsPath
    -> Text
    -> IO (Either Text (SessionMeta, [SessionTurn]))
loadActiveSession pool root sessionId = runExceptT do
    _ <- except (sessionDirForId root sessionId)
    stored <- loadWithLegacyImport root pool sessionId Store.loadActiveSession
    storedPrompt <- loadStoredPromptEpoch pool sessionId
    decodeStoredSession
        sessionSchemaVersion
        isValidSessionId
        sessionId
        storedPrompt
        stored

loadStoredPromptEpoch
    :: StorePool
    -> Text
    -> ExceptT Text IO (Maybe Store.SessionPromptEpoch)
loadStoredPromptEpoch pool sessionId =
    lift (Store.loadLatestSessionPromptEpoch pool sessionId)
        >>= either (throwE . renderStoreError) pure

loadSessionMeta
    :: StorePool
    -> OsPath
    -> Text
    -> IO (Either Text SessionMeta)
loadSessionMeta pool root sessionId = runExceptT do
    _ <- except (sessionDirForId root sessionId)
    stored <- loadWithLegacyImport root pool sessionId Store.loadSessionMetadata
    meta <- except (fromStoredMetadata stored)
    validateSessionMeta sessionSchemaVersion isValidSessionId sessionId meta
    pure meta

loadRecentSessionTurns
    :: StorePool
    -> OsPath
    -> Text
    -> Int
    -> IO (Either Text SessionTurnPage)
loadRecentSessionTurns pool root sessionId limit =
    loadSessionTurnPage root pool sessionId
        (\pool' key -> Store.loadRecentSessionTurns pool' key limit)

loadRecentSessionHistoryTurns
    :: StorePool
    -> OsPath
    -> Text
    -> Int
    -> IO (Either Text SessionTurnPage)
loadRecentSessionHistoryTurns pool root sessionId limit =
    loadSessionTurnPage root pool sessionId
        (\pool' key -> Store.loadRecentSessionHistoryTurns pool' key limit)

loadSessionTurnsBefore
    :: StorePool
    -> OsPath
    -> Text
    -> Int64
    -> Int
    -> IO (Either Text SessionTurnPage)
loadSessionTurnsBefore pool root sessionId cursor limit =
    loadSessionTurnPage root pool sessionId
        (\pool' key ->
            Store.loadSessionTurnsBefore
                pool' key cursor (boundedPageLimit limit))

loadSessionHistoryTurnsBefore
    :: StorePool
    -> OsPath
    -> Text
    -> Int64
    -> Int
    -> IO (Either Text SessionTurnPage)
loadSessionHistoryTurnsBefore pool root sessionId cursor limit =
    loadSessionTurnPage root pool sessionId
        (\pool' key ->
            Store.loadSessionHistoryTurnsBefore
                pool' key cursor (boundedPageLimit limit))

loadSessionTurnsAfter
    :: StorePool
    -> OsPath
    -> Text
    -> Int64
    -> Int
    -> IO (Either Text SessionTurnPage)
loadSessionTurnsAfter pool root sessionId cursor limit =
    loadSessionTurnPage root pool sessionId
        (\pool' key ->
            Store.loadSessionTurnsAfter
                pool' key cursor (boundedPageLimit limit))

loadSessionHistoryTurnsRange
    :: StorePool
    -> OsPath
    -> Text
    -> Int64
    -> Int
    -> IO (Either Text SessionTurnPage)
loadSessionHistoryTurnsRange pool root sessionId start limit =
    loadSessionTurnPage root pool sessionId
        (\pool' key ->
            Store.loadSessionHistoryTurnsRange
                pool' key start (boundedPageLimit limit))

loadSessionHistoryTurnsRangeBounded
    :: StorePool
    -> OsPath
    -> Text
    -> Int64
    -> Int64
    -> Int
    -> IO (Either Text SessionTurnPage)
loadSessionHistoryTurnsRangeBounded
        pool root sessionId start endExclusive limit =
    loadSessionTurnPage root pool sessionId
        (\pool' key ->
            Store.loadSessionHistoryTurnsRangeBounded
                pool' key start endExclusive (boundedPageLimit limit))

boundedPageLimit :: Int -> Int
boundedPageLimit = min 1000 . max 1

-- | Return a bounded full-history window centered on a durable turn index.
-- The requested turn is included when it exists. Near either edge the result
-- is intentionally not rebalanced; callers can use the page flags to continue
-- in either direction without an eager full-session load.
loadSessionHistoryTurnsAround
    :: StorePool
    -> OsPath
    -> Text
    -> Int64
    -> Int
    -> IO (Either Text SessionTurnPage)
loadSessionHistoryTurnsAround pool root sessionId center radius
    | center < 0 = pure (Left "turn index must be non-negative")
    | radius < 0 = pure (Left "turn radius must be non-negative")
    | otherwise =
        loadSessionHistoryTurnsRange
            pool
            root
            sessionId
            (max 0 (center - fromIntegral boundedRadius))
            (boundedRadius * 2 + 1)
  where
    boundedRadius = min 500 radius

loadSessionHistorySnapshot
    :: StorePool
    -> OsPath
    -> Text
    -> IO (Either Text (SessionMeta, Int64, Int64))
loadSessionHistorySnapshot pool root sessionId = runExceptT do
    _ <- except (sessionDirForId root sessionId)
    stored <- loadWithLegacyImport
        root pool sessionId Store.loadSessionHistorySnapshot
    meta <- except (fromStoredMetadata stored.sessionSnapshotMetadata)
    validateSessionMeta sessionSchemaVersion isValidSessionId sessionId meta
    pure
        ( meta
        , stored.sessionSnapshotStart
        , stored.sessionSnapshotTotal
        )

loadSessionResumeStats
    :: StorePool
    -> OsPath
    -> Text
    -> IO (Either Text SessionResumeStats)
loadSessionResumeStats pool root sessionId = runExceptT do
    _ <- except (sessionDirForId root sessionId)
    stored <- loadWithLegacyImport root pool sessionId Store.loadSessionResumeStats
    pure SessionResumeStats
        { resumeStatsTurnCount = fromIntegral stored.sessionResumeTurnCount
        , resumeStatsMessageCount = fromIntegral stored.sessionResumeMessageCount
        , resumeStatsToolCount = fromIntegral stored.sessionResumeToolCount
        , resumeStatsFirstPrompt =
            fmap Text.strip stored.sessionResumeFirstPrompt
        }

loadSessionTurnPage
    :: OsPath
    -> StorePool
    -> Text
    -> (StorePool -> Text
        -> IO (Either StoreError (Maybe Store.SessionTurnPage)))
    -> IO (Either Text SessionTurnPage)
loadSessionTurnPage root pool sessionId loader = runExceptT do
    _ <- except (sessionDirForId root sessionId)
    stored <- loadWithLegacyImport root pool sessionId loader
    turns <- except $ traverse
        (\storedTurn -> do
            turn <- fromStoredTurn storedTurn.storedTurn
            pure (storedTurn.storedTurnIndex, turn))
        (Vector.toList stored.sessionPageTurns)
    pure SessionTurnPage
        { pageTurns = turns
        , pageGenerationStart = stored.sessionPageGenerationStart
        , pageTotalTurns = stored.sessionPageTotal
        , pageHasOlder = stored.sessionPageHasOlder
        , pageHasNewer = stored.sessionPageHasNewer
        }

loadWithLegacyImport
    :: OsPath
    -> StorePool
    -> Text
    -> (StorePool -> Text -> IO (Either StoreError (Maybe a)))
    -> ExceptT Text IO a
loadWithLegacyImport root pool sessionId loader = do
    stored <- lift (loader pool sessionId)
        >>= either (throwE . renderStoreError) pure
    stored' <- case stored of
        Just value -> pure (Just value)
        Nothing -> do
            _ <- importLegacySession
                sessionSchemaVersion
                isValidSessionId
                (sessionDirForId root)
                pool
                sessionId
            -- Another process may win the import race and return False from
            -- its idempotent insert. Always reload the canonical row.
            lift (loader pool sessionId)
                >>= either (throwE . renderStoreError) pure
    maybe (throwE ("session not found: " <> sessionId)) pure stored'

-- | Load several sessions with one batched PostgreSQL read while preserving
-- request order. A missing database row still takes the legacy import path.
loadSessions
    :: StorePool
    -> OsPath
    -> [Text]
    -> IO [Either Text (SessionMeta, [SessionTurn])]
loadSessions pool root sessionIds = do
    let validated =
            [ sessionDirForId root sessionId
                >> Right sessionId
            | sessionId <- sessionIds
            ]
        validIds = [sessionId | Right sessionId <- validated]
    stored <- Store.loadSessions pool validIds
    restoreResults validated stored
  where
    restoreResults [] [] = pure []
    restoreResults (Left err : rest) results =
        (Left err :) <$> restoreResults rest results
    restoreResults (Right sessionId : rest) (result : results) = do
        loaded <- case result of
            Left err -> pure (Left (renderStoreError err))
            Right Nothing -> loadSession pool root sessionId
            Right (Just value) ->
                runExceptT
                    (decodeStoredSession
                        sessionSchemaVersion
                        isValidSessionId
                        sessionId
                        Nothing
                        value)
        (loaded :) <$> restoreResults rest results
    restoreResults _ _ =
        pure [Left "batched session load returned an unexpected result count"]

loadSessionHandle
    :: StorePool
    -> OsPath
    -> Text
    -> IO (Either Text (SessionHandle, [SessionTurn]))
loadSessionHandle pool root sessionId =
    loadActiveSession pool root sessionId >>= \case
        Left err -> pure (Left err)
        Right (meta, turns) ->
            pure do
                dir <- sessionDirForId root sessionId
                tempDir <- sessionTempDirForId root sessionId
                Right
                    ( SessionHandle
                        { sessionPool = pool
                        , sessionDir = dir
                        , sessionTempDir = tempDir
                        , sessionMetaPath = dir </> unsafeEncodeUtf "meta.json"
                        , sessionTranscriptPath =
                            dir </> unsafeEncodeUtf "transcript.jsonl"
                        , sessionMeta = meta
                        }
                    , turns
                    )

-- | Import a transferred session under its existing id and optional cwd.
importSessionTransfer
    :: StorePool
    -> OsPath
    -> Maybe OsPath
    -> SessionTransfer
    -> IO (Either Text Text)
importSessionTransfer pool root cwd transfer = runExceptT do
    let sessionId = transfer.transferMeta.metaId
    dir <- except (sessionDirForId root sessionId)
    exists <- lift (doesDirectoryExist dir)
    when exists (throwE ("session already exists: " <> sessionId))
    lift (ensurePrivateDir root)
    lift (createDirectory dir)
    lift (setFileMode (unsafeToFilePath dir) 0o700)
    _ <- lift (ensureSessionTemp root sessionId) >>= except
    let meta = transfer.transferMeta
            { metaCwd = fromMaybe transfer.transferMeta.metaCwd cwd }
        bytes = Aeson.encode (SessionTransfer meta transfer.transferTurns)
        legacy = Store.LegacySession
            { legacySourcePath = "afk:" <> transfer.transferMeta.metaId
            , legacyContentHash = contentFingerprint bytes
            , legacyMetadata = toStoredMetadata meta
            , legacyTurns = map toStoredTurn transfer.transferTurns
            , legacyPromptSnapshot =
                toStoredPromptSnapshot <$> meta.metaPromptSnapshot
            }
    lift (Store.importLegacySession pool legacy) >>= \case
        Left err -> do
            lift (cleanupTransfer dir sessionId)
            throwE (renderStoreError err)
        Right False -> do
            lift (cleanupTransfer dir sessionId)
            throwE ("session already exists: " <> sessionId)
        Right True -> pure sessionId
  where
    cleanupTransfer dir sessionId = do
        _ <- tryIO (removePathForcibly dir)
        _ <- removeSessionTemp root sessionId
        pure ()

-- | Import a validated transfer as a new session. The source id is never
-- reused, which makes importing the same file safe and preserves the source
-- transcript as an immutable object.
importSessionTransferRemapped
    :: StorePool
    -> OsPath
    -> Maybe OsPath
    -> SessionTransferEnvelope
    -> IO (Either Text Text)
importSessionTransferRemapped pool root cwd rawEnvelope =
    case validateSessionTransferEnvelope rawEnvelope of
        Left err -> pure (Left err)
        Right envelope -> do
            (sessionId, _) <- allocateSessionTemp root
            now <- normalizePostgresTimestamp <$> getCurrentTime
            let transfer = envelope.transferSession
                sourceMeta = transfer.transferMeta
                meta = sourceMeta
                    { metaId = sessionId
                    , metaCreatedAt = now
                    , metaUpdatedAt = now
                    }
            importSessionTransfer pool root cwd
                transfer { transferMeta = meta }

exportSessionTransfer
    :: StorePool
    -> OsPath
    -> Text
    -> IO (Either Text SessionTransferEnvelope)
exportSessionTransfer pool root sessionId =
    loadSession pool root sessionId <&> \result -> do
        (meta, turns) <- result
        validateSessionTransferEnvelope SessionTransferEnvelope
            { transferFormat = sessionTransferFormatName
            , transferFormatVersion = sessionTransferFormatVersion
            , transferSession = SessionTransfer meta turns
            }

-- | Stream a transfer document in bounded turn pages. Callback bytes are
-- valid only for the duration of the callback. The export captures the turn
-- count from its first page and ignores later appends, yielding an immutable
-- prefix without hydrating the full transcript.
streamSessionTransfer
    :: StorePool
    -> OsPath
    -> Text
    -> (BS.ByteString -> IO ())
    -> IO (Either Text ())
streamSessionTransfer pool root sessionId emit = runExceptT do
    (meta, snapshotStart, snapshotTotal) <-
        lift (loadSessionHistorySnapshot pool root sessionId) >>= except
    let snapshotEnd = snapshotStart + max 0 snapshotTotal
    lift $ emitLazy
        ("{\"format\":\"haskell-agent.session-transfer\","
            <> "\"version\":1,\"session\":{\"meta\":"
            <> Aeson.encode meta
            <> ",\"turns\":[")
    firstPage <- lift
        (loadSessionHistoryTurnsRangeBounded
            pool root sessionId snapshotStart snapshotEnd 256)
        >>= except
    let total = max 0 snapshotTotal
    emitted <- lift (emitPage True total 0 firstPage)
    when (emitted < total) $
        streamRest snapshotEnd total emitted firstPage
    lift (emitLazy "]}}")
  where
    emitLazy = mapM_ emit . LBS.toChunks

    emitPage first total emitted page = do
        let remaining = max 0 (total - emitted)
            selected = take (fromIntegral remaining) page.pageTurns
        _ <- foldl'
            (\action (_, turn) -> do
                isFirst <- action
                unless isFirst (emit ",")
                emitLazy (Aeson.encode turn)
                pure False)
            (pure first)
            selected
        pure (emitted + fromIntegral (length selected))

    streamRest snapshotEnd total emitted previous =
        case reverse previous.pageTurns of
            [] -> throwE "session export paging made no progress"
            (lastIndex, _) : _ -> do
                page <- lift
                    (loadSessionHistoryTurnsRangeBounded
                        pool root sessionId (lastIndex + 1) snapshotEnd 256)
                    >>= except
                next <- lift (emitPage False total emitted page)
                when (next <= emitted) $
                    throwE "session export paging made no progress"
                when (next < total) (streamRest snapshotEnd total next page)

-- | Fork a source session through an inclusive durable turn index. The source
-- remains untouched. Transcript effects are copied verbatim, while derived
-- usage and continuation metadata are recomputed from the selected prefix.
forkSessionAtTurn
    :: StorePool
    -> OsPath
    -> Text
    -> Int64
    -> IO (Either Text Text)
forkSessionAtTurn pool root sourceId throughIndex = runExceptT do
    when (throughIndex < 0) (throwE "turn index must be non-negative")
    (sourceMeta, snapshotStart, snapshotTotal) <-
        lift (loadSessionHistorySnapshot pool root sourceId) >>= except
    let snapshotEnd = snapshotStart + max 0 snapshotTotal
    when
        (throughIndex < snapshotStart || throughIndex >= snapshotEnd) $
        throwE "turn index is outside the session transcript"
    turns <- loadPrefix snapshotStart (throughIndex + 1) id
    let
        usage = foldl' addUsage
            (TokenUsage
                { inputTokens = 0
                , outputTokens = 0
                , cachedTokens = 0
                })
            (mapMaybe (.turnUsage) turns)
        meta = sourceMeta
            { metaLastResponseId = continuationResponseId turns
            , metaInputTokens = usage.inputTokens
            , metaOutputTokens = usage.outputTokens
            , metaCachedTokens = usage.cachedTokens
            , metaLastRecap = Nothing
            , metaLastTurnSummary = Nothing
            , metaLastRecapMainTurns = 0
            , metaTitleUserTurns =
                length (filter (not . Text.null . Text.strip . (.turnUserText)) turns)
            }
        envelope = SessionTransferEnvelope
            { transferFormat = sessionTransferFormatName
            , transferFormatVersion = sessionTransferFormatVersion
            , transferSession = SessionTransfer meta turns
            }
    lift (importSessionTransferRemapped pool root Nothing envelope) >>= except
  where
    loadPrefix cursor endExclusive append
        | cursor >= endExclusive = pure (append [])
        | otherwise = do
            page <- lift
                (loadSessionHistoryTurnsRangeBounded
                    pool root sourceId cursor endExclusive 256)
                >>= except
            case page.pageTurns of
                [] -> throwE "session fork paging made no progress"
                values -> do
                    let lastIndex = fst (last values)
                        pageTurns = map snd values
                    loadPrefix
                        (lastIndex + 1)
                        endExclusive
                        (append . (pageTurns <>))

    addUsage left right = TokenUsage
        { inputTokens = left.inputTokens + right.inputTokens
        , outputTokens = left.outputTokens + right.outputTokens
        , cachedTokens = left.cachedTokens + right.cachedTokens
        }

    continuationResponseId = foldl' step Nothing
    step previous turn =
        let base = case turn.turnEffect of
                TranscriptAppend -> previous
                TranscriptReplace -> Nothing
                TranscriptReset -> Nothing
        in turn.turnResponseId <|> base

deleteSession :: StorePool -> OsPath -> Text -> IO (Either Text ())
deleteSession pool root sessionId = mask \restore -> runExceptT do
    dir <- except (sessionDirForId root sessionId)
    exists <- lift (restore (doesDirectoryExist dir))
    now <- lift (restore getCurrentTime)
    lock <- if exists
        then lift (acquireSessionLock dir sessionId) >>= \case
            Left _ -> throwE "cannot delete a running session"
            Right lock -> pure (Just lock)
        else pure Nothing
    deleted <- lift $
        restore (Store.deleteSession pool sessionId now)
            `finally` maybe (pure ()) releaseSessionLock lock
    case deleted of
        Left err -> throwE (renderStoreError err)
        Right False -> throwE ("session not found: " <> sessionId)
        Right True
            | not exists -> pure ()
            | otherwise ->
                lift (restore (tryIO (removePathForcibly dir))) >>= \case
                    Left err ->
                        throwE
                            ("session deleted but artifacts could not be removed: "
                                <> Text.pack (displayException err))
                    Right () -> pure ()
    tempRemoved <- lift (restore (removeSessionTemp root sessionId))
    except tempRemoved

renameSession
    :: StorePool
    -> OsPath
    -> Text
    -> Text
    -> IO (Either Text SessionMeta)
renameSession pool root sessionId rawTitle = runExceptT do
    let title = Text.unwords (Text.words (Text.strip rawTitle))
    when (Text.null title) (throwE "session title cannot be empty")
    _ <- except (sessionDirForId root sessionId)
    now <- lift getCurrentTime
    renamed <- lift $
        Store.setSessionTitle pool sessionId title True 2 now
    case renamed of
        Left err -> throwE (renderStoreError err)
        Right False -> throwE ("session not found: " <> sessionId)
        Right True -> lift (loadSessionMeta pool root sessionId) >>= except

setSessionArchived
    :: StorePool
    -> OsPath
    -> Text
    -> Bool
    -> IO (Either Text ())
setSessionArchived pool root sessionId archived = runExceptT do
    _ <- except (sessionDirForId root sessionId)
    now <- lift getCurrentTime
    changed <- lift $
        Store.setSessionArchived pool sessionId archived now
    case changed of
        Left err -> throwE (renderStoreError err)
        Right False -> throwE ("session not found: " <> sessionId)
        Right True -> pure ()

-- | Session ids are single path components. Keep this deliberately broader
-- than the current date-plus-hex allocator so older ids remain resumable.
isValidSessionId :: Text -> Bool
isValidSessionId sessionId =
    not (Text.null sessionId)
        && sessionId /= "."
        && sessionId /= ".."
        && Text.all (\char -> char /= '/' && char /= '\\' && char /= '\NUL') sessionId

sessionDirForId :: OsPath -> Text -> Either Text OsPath
sessionDirForId root sessionId
    | isValidSessionId sessionId =
        Right (root </> unsafeEncodeUtf (Text.unpack sessionId))
    | otherwise = Left "invalid session id"

sessionTempDirForId :: OsPath -> Text -> Either Text OsPath
sessionTempDirForId root sessionId
    | isValidSessionId sessionId =
        Right
            (sessionTempsRoot root
                </> unsafeEncodeUtf (Text.unpack sessionId))
    | otherwise = Left "invalid session id"

listSessions :: StorePool -> OsPath -> IO ([SessionMeta], [Text])
listSessions pool _root = do
    Store.listSessionMetadata pool >>= \case
        Left err ->
            fail
                ("could not list PostgreSQL sessions: "
                    <> Text.unpack (renderStoreError err))
        Right values ->
            let decoded = map decodeListedSessionMeta values
            in pure
                ( [meta | Right meta <- decoded]
                , [err | Left err <- decoded]
                )

-- | Decode one persisted session for listing. Corrupt or incompatible
-- metadata becomes an error string instead of disappearing from the picker.
decodeListedSessionMeta :: Store.SessionMetadata -> Either Text SessionMeta
decodeListedSessionMeta value = do
    meta <- fromStoredMetadata value
    unless (meta.metaVersion == sessionSchemaVersion) $
        Left $
            "unsupported session schema version "
                <> Text.pack (show meta.metaVersion)
                <> " for session "
                <> meta.metaId
                <> " (expected "
                <> Text.pack (show sessionSchemaVersion)
                <> ")"
    pure meta

listArchivedSessionIds :: StorePool -> IO (Either Text [Text])
listArchivedSessionIds pool =
    Store.listSessionArchiveKeys pool >>= \case
        Left err -> pure (Left (renderStoreError err))
        Right sessionIds -> pure (Right sessionIds)

writeSessionMeta :: StorePool -> OsPath -> SessionMeta -> IO ()
writeSessionMeta pool _path meta = do
    Store.replaceSessionMetadata
        pool
        "session.metadata_replaced"
        (toStoredMetadata meta) >>= \case
            Left err ->
                fail
                    ("could not update PostgreSQL session metadata: "
                        <> Text.unpack (renderStoreError err))
            Right False ->
                fail ("session not found: " <> Text.unpack meta.metaId)
            Right True -> pure ()

sessionTitleFromPrompt :: Text -> Text
sessionTitleFromPrompt prompt =
    let title = case take 10 (Text.words (Text.strip prompt)) of
            [] -> "New session"
            words' -> Text.unwords words'
    in if Text.length title <= 72
        then title
        else Text.take 69 title <> "..."

setGeneratedSessionTitle :: Int -> Text -> SessionHandle -> IO SessionHandle
setGeneratedSessionTitle refreshIndex rawTitle handle
    | handle.sessionMeta.metaTitleIsManual = pure handle
    | otherwise = do
        let title = Text.unwords (Text.words (Text.strip rawTitle))
        now <- getCurrentTime
        Store.setGeneratedSessionTitle
            handle.sessionPool
            handle.sessionMeta.metaId
            title
            (fromIntegral refreshIndex)
            now >>= \case
                Left err ->
                    fail
                        ("could not update PostgreSQL session title: "
                            <> Text.unpack (renderStoreError err))
                Right True ->
                    pure handle
                        { sessionMeta = handle.sessionMeta
                            { metaTitle = title
                            , metaTitleIsManual = False
                            , metaTitleRefreshIndex = refreshIndex
                            }
                        }
                Right False ->
                    loadSessionMeta
                        handle.sessionPool
                        handle.sessionDir
                        handle.sessionMeta.metaId >>= \case
                            Left err -> fail (Text.unpack err)
                            Right meta -> pure handle { sessionMeta = meta }

setManualSessionTitle :: Text -> SessionHandle -> IO SessionHandle
setManualSessionTitle = writeTitle True 2

writeTitle :: Bool -> Int -> Text -> SessionHandle -> IO SessionHandle
writeTitle manual refreshIndex rawTitle handle = do
    let title = Text.unwords (Text.words (Text.strip rawTitle))
        meta = handle.sessionMeta
            { metaTitle = title
            , metaTitleIsManual = manual
            , metaTitleRefreshIndex = refreshIndex
            }
    now <- getCurrentTime
    Store.setSessionTitle
        handle.sessionPool
        handle.sessionMeta.metaId
        title
        manual
        (fromIntegral refreshIndex)
        now >>= \case
            Left err ->
                fail
                    ("could not update PostgreSQL session title: "
                        <> Text.unpack (renderStoreError err))
            Right False ->
                fail ("session not found: " <> Text.unpack handle.sessionMeta.metaId)
            Right True -> pure handle { sessionMeta = meta }

resetSessionTitleToAuto :: SessionHandle -> IO SessionHandle
resetSessionTitleToAuto handle = do
    let meta = handle.sessionMeta
            { metaTitleIsManual = False
            , metaTitleRefreshIndex = 0
            }
    now <- getCurrentTime
    Store.setSessionTitle
        handle.sessionPool
        handle.sessionMeta.metaId
        meta.metaTitle
        False
        0
        now >>= \case
            Left err ->
                fail
                    ("could not reset PostgreSQL session title: "
                        <> Text.unpack (renderStoreError err))
            Right False ->
                fail ("session not found: " <> Text.unpack handle.sessionMeta.metaId)
            Right True -> pure handle { sessionMeta = meta }

setSessionRecap :: Text -> Int -> SessionHandle -> IO SessionHandle
setSessionRecap summary mainTurns handle = do
    let meta = handle.sessionMeta
            { metaLastRecap = Just summary
            , metaLastRecapMainTurns = max 0 mainTurns
            }
    writeSessionMeta handle.sessionPool handle.sessionMetaPath meta
    pure handle { sessionMeta = meta }

setSessionTurnSummary :: Text -> SessionHandle -> IO SessionHandle
setSessionTurnSummary summary handle = do
    let meta = handle.sessionMeta { metaLastTurnSummary = Just summary }
    writeSessionMeta handle.sessionPool handle.sessionMetaPath meta
    pure handle { sessionMeta = meta }

sessionUsageFromMeta :: SessionMeta -> TokenUsage
sessionUsageFromMeta meta = TokenUsage
    { inputTokens = meta.metaInputTokens
    , outputTokens = meta.metaOutputTokens
    , cachedTokens = meta.metaCachedTokens
    }

-- | Session totals come from meta. Older sessions without those fields
-- decode as zero and start accumulating from new turns.
sessionUsageFromTurns :: SessionMeta -> [SessionTurn] -> TokenUsage
sessionUsageFromTurns meta _turns = sessionUsageFromMeta meta

-- | Copy-pasteable resume line printed on Ctrl-C, matching grok build.
resumeHint :: String -> Text -> Text
resumeHint progName sessionId =
    "Resume this session with: "
        <> shellSingleQuote progName
        <> " --resume "
        <> sessionId

-- | POSIX single-quote so paths with spaces stay one shell word.
shellSingleQuote :: String -> Text
shellSingleQuote s =
    "'" <> Text.replace "'" "'\\''" (Text.pack s) <> "'"

-- | Reserve a unique session id by atomically creating its private scratch
-- directory. The durable session directory remains deferred until first use.
allocateSessionTemp :: OsPath -> IO (Text, OsPath)
allocateSessionTemp root = do
    let tempRoot = sessionTempsRoot root
    ensurePrivateDir tempRoot
    now <- getCurrentTime
    go tempRoot now (0 :: Int)
  where
    go tempRoot now attempt
        | attempt >= 32 = fail "could not allocate a unique session temp directory"
        | otherwise = do
            let sessionId = sessionIdForAttempt now attempt
                durableDir =
                    root </> unsafeEncodeUtf (Text.unpack sessionId)
                tempDir =
                    tempRoot </> unsafeEncodeUtf (Text.unpack sessionId)
            durableExists <-
                maybe False (const True)
                    <$> symbolicLinkStatusMaybe durableDir
            recoveryExists <-
                maybe False (const True)
                    <$> symbolicLinkStatusMaybe
                        (sessionMaterializationMetaPath root sessionId)
            if durableExists || recoveryExists
                then go tempRoot now (attempt + 1)
                else mask \restore ->
                    tryIO (createDirectory tempDir) >>= \case
                        Left _ -> restore (go tempRoot now (attempt + 1))
                        Right () ->
                            restore
                                (do
                                    setFileMode
                                        (unsafeToFilePath tempDir)
                                        0o700
                                    pure (sessionId, tempDir))
                                `onException` do
                                    _ <- removeSessionTemp root sessionId
                                    pure ()

-- | Take a shared lease for a session's scratch directory. Automatic cleanup
-- requires the matching exclusive lock, so a live process cannot lose its
-- temporary files even before its durable session lock has been acquired.
acquireSessionTempLease
    :: OsPath
    -> OsPath
    -> IO (Either Text (Maybe SessionTempLease))
acquireSessionTempLease root path =
    case sessionTempId root path of
        Nothing -> pure (Right Nothing)
        Just sessionId -> mask \restore -> do
            let lockPath = sessionTempLockPath root sessionId
            prepared <- tryAny $
                restore (ensurePrivateDir (takeDirectory lockPath))
            result <- case prepared of
                Left exception -> pure (Left exception)
                Right () -> tryAny do
                    -- Keep async exceptions masked from successful acquisition
                    -- until the lock is wrapped in its owning lease.
                    FileLock.tryLockFile
                        (unsafeToFilePath lockPath)
                        FileLock.Shared
            pure case result of
                Left exception ->
                    Left
                        ("failed to lease session scratch directory "
                            <> toText path
                            <> ": "
                            <> Text.pack (displayException exception))
                Right Nothing ->
                    Left
                        ("session scratch directory is being cleaned up: "
                            <> toText path)
                Right (Just lock) ->
                    Right (Just (SessionTempLease lock))

releaseSessionTempLease :: SessionTempLease -> IO ()
releaseSessionTempLease (SessionTempLease lock) = do
    _ <- tryAny (FileLock.unlockFile lock)
    pure ()

-- | Remove old session scratch directories after retaining the newest entries.
-- Only allocator-shaped names are considered, and a directory with a live
-- shared lease is skipped. Failures are reported per path and never stop the
-- rest of the best-effort cleanup. Directories allocated on the current UTC
-- day are always kept, closing the startup interval before a lease is taken.
cleanupStaleSessionTemps
    :: OsPath
    -> Int
    -> [OsPath]
    -> IO SessionTempCleanupReport
cleanupStaleSessionTemps root requestedKeep protected = do
    let tempRoot = sessionTempsRoot root
    exists <- doesDirectoryExist tempRoot
    if not exists
        then pure mempty
        else do
            today <- utctDay <$> getCurrentTime
            listed <- tryAny (listDirectory tempRoot)
            case listed of
                Left exception ->
                    pure $ tempCleanupFailure tempRoot exception
                Right entries -> do
                    directories <- foldM
                        (collectDirectory tempRoot)
                        ([], [])
                        entries
                    case directories of
                        (managed, discoveryFailures) -> do
                            let candidates =
                                    filter
                                        (isBefore today . takeFileName)
                                        (drop (max 1 requestedKeep) $
                                            sortOn
                                                (Down
                                                    . unsafeToFilePath
                                                    . takeFileName)
                                                managed)
                            cleaned <- foldM cleanupOne mempty candidates
                            pure $
                                cleaned
                                    <> mempty
                                        { tempCleanupFailures =
                                            discoveryFailures
                                        }
  where
    protectedPaths = map normalise protected
    isBefore today path =
        maybe False (< today) (allocatedSessionDay path)

    collectDirectory tempRoot (managed, failures) entry = do
        let path = tempRoot </> entry
        checked <- tryAny (doesDirectoryExist path)
        pure case checked of
            Left exception ->
                ( managed
                , failures
                    <> [(path, Text.pack (displayException exception))]
                )
            Right True
                | isAllocatedSessionId entry ->
                    (managed <> [path], failures)
            Right _ -> (managed, failures)

    cleanupOne report candidate
        | any
            (\protectedPath ->
                equalFilePath protectedPath (normalise candidate))
            protectedPaths =
                pure report
        | otherwise = do
            result <- tryAny (cleanupStaleSessionTemp root candidate)
            pure $ report <> case result of
                Left exception ->
                    tempCleanupFailure candidate exception
                Right candidateReport -> candidateReport

cleanupStaleSessionTemp
    :: OsPath
    -> OsPath
    -> IO SessionTempCleanupReport
cleanupStaleSessionTemp root candidate = mask \restore ->
    case sessionTempId root candidate of
        Nothing -> pure mempty
        Just sessionId -> do
            let durableDir = root </> sessionId
            durableExists <- restore (doesDirectoryExist durableDir)
            durableLock <-
                if durableExists
                    then fmap (fmap Just) $
                        acquireSessionLock durableDir (toText sessionId)
                    else pure (Right Nothing)
            case durableLock of
                -- A running or otherwise un-lockable durable session owns the
                -- scratch directory. Treat either case conservatively.
                Left _ -> pure mempty
                Right lock ->
                    restore (cleanupWithSessionLock sessionId)
                        `finally` mapM_ releaseSessionLock lock
  where
    cleanupWithSessionLock sessionId = mask \restore -> do
        let lockPath = sessionTempLockPath root sessionId
        prepared <- tryAny $
            restore (ensurePrivateDir (takeDirectory lockPath))
        locked <- case prepared of
            Left exception -> pure (Left exception)
            Right () -> tryAny $
                FileLock.tryLockFile
                    (unsafeToFilePath lockPath)
                    FileLock.Exclusive
        case locked of
            Left exception ->
                pure $ tempCleanupFailure candidate exception
            Right Nothing ->
                pure mempty
            Right (Just lock) -> do
                removed <- tryAny $
                    restore (do
                        symbolicLinkStatusMaybe candidate >>= \case
                            -- Another startup cleaner may have removed the
                            -- candidate before this process acquired its
                            -- exclusive lock.
                            Nothing -> pure False
                            Just status
                                | isSymbolicLink status -> pure False
                                | otherwise ->
                                    removePathForcibly candidate
                                        >> removeSessionMaterializationMeta
                                            root
                                            (toText sessionId)
                                        >> pure True)
                        `finally` FileLock.unlockFile lock
                pure case removed of
                    Left exception ->
                        tempCleanupFailure candidate exception
                    Right True ->
                        mempty { tempCleanupRemoved = [candidate] }
                    Right False -> mempty

sessionTempId :: OsPath -> OsPath -> Maybe OsPath
sessionTempId root rawPath =
    let tempRoot = normalise (sessionTempsRoot root)
        path = normalise rawPath
    in if equalFilePath tempRoot (takeDirectory path)
            && isAllocatedSessionId (takeFileName path)
        then Just (takeFileName path)
        else Nothing

sessionTempLockPath :: OsPath -> OsPath -> OsPath
sessionTempLockPath root sessionId =
    sessionTempsRoot root
        </> unsafeEncodeUtf ".locks"
        </> (sessionId <> unsafeEncodeUtf ".lock")

isAllocatedSessionId :: OsPath -> Bool
isAllocatedSessionId path = case allocatedSessionDay path of
    Just _ -> True
    Nothing -> False

allocatedSessionDay :: OsPath -> Maybe Day
allocatedSessionDay path =
    case unsafeToFilePath path of
        year1 : year2 : year3 : year4 : '-' :
                month1 : month2 : '-' : day1 : day2 : '-' : suffix ->
            let date =
                    [ year1, year2, year3, year4, '-'
                    , month1, month2, '-', day1, day2
                    ]
            in if length suffix == 8 && all isHexDigit suffix
                then parseTimeM True defaultTimeLocale "%Y-%m-%d" date
                else Nothing
        _ -> Nothing

tempCleanupFailure
    :: OsPath
    -> SomeException
    -> SessionTempCleanupReport
tempCleanupFailure path exception =
    mempty
        { tempCleanupFailures =
            [(path, Text.pack (displayException exception))]
        }

ensureSessionTemp :: OsPath -> Text -> IO (Either Text OsPath)
ensureSessionTemp root sessionId =
    case sessionTempDirForId root sessionId of
        Left err -> pure (Left err)
        Right tempDir -> do
            result <- tryIO (ensurePrivateDir tempDir)
            pure $ case result of
                Left err ->
                    Left
                        ("could not create session temp directory: "
                            <> Text.pack (displayException err))
                Right () -> Right tempDir

sessionIdForAttempt :: UTCTime -> Int -> Text
sessionIdForAttempt now attempt =
    let day = formatTime defaultTimeLocale "%Y-%m-%d" now
        start =
            floor
                (nominalDiffTimeToSeconds
                    (utcTimeToPOSIXSeconds now)
                    * 1000000) :: Integer
        hex = hex8 (start + fromIntegral attempt)
    in Text.pack (day <> "-" <> hex)

removeSessionTemp :: OsPath -> Text -> IO (Either Text ())
removeSessionTemp root sessionId =
    case sessionTempDirForId root sessionId of
        Left err -> pure (Left err)
        Right tempDir -> do
            removeSessionMaterializationMeta root sessionId
            exists <- doesDirectoryExist tempDir
            if not exists
                then pure (Right ())
                else tryIO (removePathForcibly tempDir) >>= \case
                    Left err ->
                        pure $ Left
                            ("could not delete session temp directory: "
                                <> Text.pack (displayException err))
                    Right () -> pure (Right ())

removeReservedTemp :: OsPath -> Text -> IO ()
removeReservedTemp root sessionId = do
    _ <- removeSessionTemp root sessionId
    pure ()

hex8 :: Integer -> String
hex8 n =
    let s = showHex (n `mod` 0x100000000) ""
    in replicate (8 - length s) '0' <> s

ensurePrivateDir :: OsPath -> IO ()
ensurePrivateDir path = do
    createDirectoryIfMissing True path
    _ <- tryIO (setFileMode (unsafeToFilePath path) 0o700)
    pure ()
