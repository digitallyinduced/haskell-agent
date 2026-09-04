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
    , appendTurnWithPromptResetAndTaskPlanClearIndexed
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
    , loadCurrentTaskPlan
    , taskPlanHooksForPersistence
    , resumeHint
    , sessionUsageFromTurns
    ) where

import Agent.FileRetry (retryOnFileBusy, writeLazyFileAtomically)
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
import Agent.CLI.Session.Storage
    ( deleteSession
    , listArchivedSessionIds
    , listSessions
    , loadActiveSession
    , loadRecentSessionHistoryTurns
    , loadRecentSessionTurns
    , loadSession
    , loadSessionHandle
    , loadSessionHistorySnapshot
    , loadSessionHistoryTurnsAround
    , loadSessionHistoryTurnsBefore
    , loadSessionHistoryTurnsRange
    , loadSessionHistoryTurnsRangeBounded
    , loadSessionMeta
    , loadSessionResumeStats
    , loadSessions
    , loadSessionTurnsAfter
    , loadSessionTurnsBefore
    , renameSession
    , sessionSchemaVersion
    , setSessionArchived
    , writeSessionMeta
    )
import Agent.CLI.Session.TaskPlan
    ( fromStoredTaskPlan
    , toStoredTaskPlanItem
    )
import Agent.CLI.Session.TempWorkspace
    ( SessionTempCleanupReport(..)
    , SessionTempLease
    , acquireSessionTempLease
    , allocateSessionTemp
    , cleanupStaleSessionTemps
    , defaultSessionTempKeepCount
    , ensurePrivateDir
    , ensureSessionTemp
    , isValidSessionId
    , releaseSessionTempLease
    , removeReservedTemp
    , removeSessionMaterializationMeta
    , removeSessionTemp
    , sessionDirForId
    , sessionMaterializationMetaPath
    , sessionTempDirForId
    , sessionTempsRoot
    , sessionsRoot
    , symbolicLinkStatusMaybe
    )
import Agent.CLI.Session.Transfer
    ( SessionTransferEnvelope(..)
    , exportSessionTransfer
    , forkSessionAtTurn
    , importSessionTransfer
    , importSessionTransferRemapped
    , sessionTransferFormatVersion
    , streamSessionTransfer
    , validateSessionTransferEnvelope
    )
import Agent.CLI.Session.Codec
    ( toStoredMetadata
    , toStoredPromptSnapshot
    , toStoredTurn
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
import Agent.Tools.TaskPlan
    ( CurrentTaskPlan(..)
    , TaskPlan(..)
    , TaskPlanHooks(..)
    )
import Control.Applicative ((<|>))
import Control.Exception.Safe
    ( displayException
    , finally
    , mask
    , onException
    , tryAny
    , tryIO
    )
import Control.Monad (guard, unless, when)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.IORef
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock
    ( UTCTime
    , getCurrentTime
    )
import System.Directory.OsPath
    ( copyFile
    , createDirectory
    , doesFileExist
    , listDirectory
    , removePathForcibly
    , removeFile
    )
import System.OsPath
    ( OsPath
    , unsafeEncodeUtf
    , (</>)
    )
import System.Posix.Files
    ( getSymbolicLinkStatus
    , isDirectory
    , isRegularFile
    , isSymbolicLink
    , setFileMode
    )

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


newPendingPersistence :: SessionCreate -> IO Persistence
newPendingPersistence spec = do
    validateSessionCreateGatewayBoundary spec
    (sessionId, tempDir) <- allocateSessionTemp spec.createRoot
    newPendingPersistenceReserved spec sessionId tempDir

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
                                    Right True -> do
                                        copied <- restore
                                            (Store.copySessionTaskPlan
                                                source.sessionPool
                                                source.sessionMeta.metaId
                                                meta.metaId)
                                            `onException` cleanupOwned
                                        case copied of
                                            Left err -> do
                                                cleanupOwned
                                                pure (Left (renderStoreError err))
                                            Right _ -> pure (Right handle)
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

-- | Load the authoritative current plan for an already-active persistence
-- slot. Pending sessions cannot yet have durable plan state.
loadCurrentTaskPlan
    :: Persistence
    -> IO (Either Text (Maybe CurrentTaskPlan))
loadCurrentTaskPlan PersistenceDisabled = pure (Right Nothing)
loadCurrentTaskPlan (PersistenceEnabled slotRef) =
    readIORef slotRef >>= \case
        PersistencePending{} -> pure (Right Nothing)
        PersistenceActive handle ->
            mapStoreException "could not load session task plan" $
                fmap (fmap (fmap fromStoredTaskPlan))
                    (Store.loadSessionTaskPlan
                        handle.sessionPool
                        handle.sessionMeta.metaId)

-- | Construct write-through hooks for the task-plan environment. A write
-- materializes pending persistence before mutating plan state.
taskPlanHooksForPersistence :: Persistence -> Maybe TaskPlanHooks
taskPlanHooksForPersistence PersistenceDisabled = Nothing
taskPlanHooksForPersistence (PersistenceEnabled slotRef) =
    Just TaskPlanHooks
        { taskPlanPersistReplace = \plan ->
            mapStoreException "could not persist session task plan" do
                handle <- ensureSession slotRef
                Store.replaceSessionTaskPlan
                    handle.sessionPool
                    handle.sessionMeta.metaId
                    plan.taskPlanExplanation
                    (map toStoredTaskPlanItem plan.taskPlanItems) >>= \case
                        Left err -> pure (Left err)
                        Right Nothing ->
                            fail
                                ("session not found: "
                                    <> Text.unpack handle.sessionMeta.metaId)
                        Right (Just revision) -> pure (Right revision)
        , taskPlanPersistClear =
            mapStoreException "could not clear session task plan" do
                readIORef slotRef >>= \case
                    PersistencePending{} -> pure (Right ())
                    PersistenceActive handle ->
                        Store.clearSessionTaskPlan
                            handle.sessionPool
                            handle.sessionMeta.metaId >>= \case
                                Left err -> pure (Left err)
                                Right _ -> pure (Right ())
        }

mapStoreException
    :: Text
    -> IO (Either StoreError a)
    -> IO (Either Text a)
mapStoreException label action =
    tryAny action <&> \case
        Left err -> Left (label <> ": " <> Text.pack (displayException err))
        Right result -> either (Left . renderStoreError) Right result


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

-- | Append a reset turn while atomically retiring the prompt epoch and
-- clearing the session's current task plan.
appendTurnWithPromptResetAndTaskPlanClearIndexed
    :: SessionHandle
    -> SessionTurn
    -> (SessionMeta -> SessionMeta)
    -> IO (SessionHandle, Int64)
appendTurnWithPromptResetAndTaskPlanClearIndexed handle turn transition =
    appendTurnWithMetaTransitionIndexedUsing
        Store.appendSessionTurnIndexedWithPromptResetAndTaskPlanClear
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
    Store.appendSessionTurnsClearingTaskPlan
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
