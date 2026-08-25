-- | Persist REPL conversations under @~/.haskell-agent/sessions@.
module Agent.CLI.Session
    ( SessionHandle(..)
    , SessionMeta(..)
    , LegacySubagentTarget(..)
    , SessionTurn(..)
    , SessionActivity(..)
    , SessionTransfer(..)
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
    , appendTurn
    , appendTurnWithMetaUpdate
    , appendTurnKeepTitle
    , addSessionUsage
    , deleteSession
    , loadSession
    , importSessionTransfer
    , loadSessionHandle
    , isValidSessionId
    , listSessions
    , sessionDirForId
    , sessionTempDirForId
    , sessionTempsRoot
    , allocateSessionTemp
    , ensureSessionTemp
    , removeSessionTemp
    , sessionsRoot
    , sessionTitleFromPrompt
    , setGeneratedSessionTitle
    , setManualSessionTitle
    , resetSessionTitleToAuto
    , sessionConversationText
    , sessionLegacySubagentTarget
    , sessionTitleTurnCountFromSlot
    , writeSessionMeta
    , ensureSession
    , resumeHint
    , sessionUsageFromTurns
    ) where

import Agent.FileRetry (retryOnFileBusy, writeLazyFileAtomically)
import Agent.CLI.SessionLock
    ( acquireSessionLock
    , releaseSessionLock
    )
import Agent.CLI.Session.StoreCodec
    ( fromStoredResponseItem
    , toStoredResponseItem
    )
import Agent.CLI.Models (ModelTarget(..))
import Agent.Loop (TokenUsage(..))
import Agent.Dialect
    ( DialectId
    , dialectSlug
    , legacyDialectIdForProvider
    , parseDialect
    , providerSupportsDialect
    )
import Agent.OsPath (toText, unsafeToFilePath)
import Agent.Responses.Types (ResponseItem)
import Agent.Provider (Provider(..), parseProvider, providerSlug)
import Agent.Store.Postgres (normalizePostgresTimestamp)
import Agent.Store.Postgres.Connection (StorePool)
import qualified Agent.Store.Postgres.Session as Store
import Agent.Store.Types (renderStoreError)
import Control.Applicative ((<|>))
import Control.Exception.Safe (displayException, finally, onException, tryIO)
import Control.Monad (unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
    ( ExceptT(..)
    , except
    , runExceptT
    , throwE
    )
import Data.Aeson (FromJSON(..), ToJSON(..), object, withObject, (.:), (.:?), (.!=), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Bits (xor)
import Data.IORef
import Data.Functor ((<&>))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (UTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Word (Word64)
import Numeric (showHex)
import System.Directory.OsPath
    ( createDirectory
    , createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , removePathForcibly
    , removeFile
    )
import System.OsPath (OsPath, takeDirectory, unsafeEncodeUtf, (</>))
import System.Posix.Files (setFileMode)

sessionSchemaVersion :: Int
sessionSchemaVersion = 1

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

data SessionMeta = SessionMeta
    { metaVersion :: !Int
    , metaId :: !Text
    , metaCreatedAt :: !UTCTime
    , metaUpdatedAt :: !UTCTime
    , metaProvider :: !Provider
    , metaConnection :: !Text
    , metaModel :: !Text
    , metaTransportModel :: !(Maybe Text)
    , metaDialect :: !DialectId
    , metaLegacySubagentTarget :: !(Maybe LegacySubagentTarget)
    , metaCwd :: !OsPath
    , metaEffort :: !Text
    , metaTitle :: !Text
    , metaTitleIsManual :: !Bool
    , metaTitleRefreshIndex :: !Int
    , metaTitleUserTurns :: !Int
    , metaLastResponseId :: !(Maybe Text)
    , metaInputTokens :: !Int
    , metaOutputTokens :: !Int
    , metaCachedTokens :: !Int
    } deriving (Eq, Show)

data SessionTransfer = SessionTransfer
    { transferMeta :: !SessionMeta
    , transferTurns :: ![SessionTurn]
    } deriving (Eq, Show)

instance ToJSON SessionTransfer where
    toJSON transfer = object
        [ "meta" .= transfer.transferMeta
        , "turns" .= transfer.transferTurns
        ]

instance FromJSON SessionTransfer where
    parseJSON = withObject "SessionTransfer" \o ->
        SessionTransfer <$> o .: "meta" <*> o .: "turns"

-- | Durable provenance for subagent transcripts written before child target
-- metadata was persisted. Keeping this target separate from the mutable root
-- target prevents a later reopen from treating stale legacy children as
-- compatible merely because the root metadata has already been retargeted.
data LegacySubagentTarget = LegacySubagentTarget
    { legacyTargetProvider :: !Provider
    , legacyTargetConnection :: !Text
    , legacyTargetEffectiveModel :: !Text
    , legacyTargetDialect :: !DialectId
    } deriving (Eq, Show)

instance ToJSON LegacySubagentTarget where
    toJSON target = object
        [ "provider" .= providerSlug target.legacyTargetProvider
        , "connection" .= target.legacyTargetConnection
        , "effectiveModel" .= target.legacyTargetEffectiveModel
        , "dialect" .= dialectSlug target.legacyTargetDialect
        ]

instance FromJSON LegacySubagentTarget where
    parseJSON = withObject "LegacySubagentTarget" \o -> do
        providerText <- o .: "provider"
        provider <- case parseProvider providerText of
            Just parsed -> pure parsed
            Nothing ->
                fail
                    ("unknown legacy subagent provider: "
                        <> Text.unpack providerText)
        dialectText <- o .: "dialect"
        dialect <- case parseDialect dialectText of
            Just parsed -> pure parsed
            Nothing ->
                fail
                    ("unknown legacy subagent dialect: "
                        <> Text.unpack dialectText)
        unless (providerSupportsDialect provider dialect) $
            fail
                ( "legacy subagent dialect "
                    <> Text.unpack (dialectSlug dialect)
                    <> " is incompatible with provider "
                    <> Text.unpack (providerSlug provider)
                )
        connection <- fromMaybe (providerSlug provider) <$> o .:? "connection"
        when (Text.null (Text.strip connection)) $
            fail "legacy subagent connection must not be empty"
        LegacySubagentTarget provider connection
            <$> o .: "effectiveModel"
            <*> pure dialect

instance ToJSON SessionMeta where
    toJSON meta = object
        [ "version" .= meta.metaVersion
        , "id" .= meta.metaId
        , "createdAt" .= meta.metaCreatedAt
        , "updatedAt" .= meta.metaUpdatedAt
        , "provider" .= providerSlug meta.metaProvider
        , "connection" .= meta.metaConnection
        , "model" .= meta.metaModel
        , "transportModel" .= meta.metaTransportModel
        , "dialect" .= dialectSlug meta.metaDialect
        , "legacySubagentTarget" .= meta.metaLegacySubagentTarget
        , "cwd" .= unsafeToFilePath meta.metaCwd
        , "effort" .= meta.metaEffort
        , "title" .= meta.metaTitle
        , "titleIsManual" .= meta.metaTitleIsManual
        , "titleRefreshIndex" .= meta.metaTitleRefreshIndex
        , "titleUserTurns" .= meta.metaTitleUserTurns
        , "lastResponseId" .= meta.metaLastResponseId
        , "inputTokens" .= meta.metaInputTokens
        , "outputTokens" .= meta.metaOutputTokens
        , "cachedTokens" .= meta.metaCachedTokens
        ]

instance FromJSON SessionMeta where
    parseJSON = withObject "SessionMeta" \o -> do
        version <- o .: "version"
        providerText <- o .: "provider"
        provider <- case parseProvider providerText of
            Just p -> pure p
            Nothing -> fail ("unknown provider: " <> Text.unpack providerText)
        model <- o .: "model"
        connection <- fromMaybe (providerSlug provider) <$> o .:? "connection"
        when (Text.null (Text.strip connection)) $
            fail "session connection must not be empty"
        dialectText <- o .:? "dialect"
        dialect <- case dialectText of
            Nothing -> pure (legacyDialectIdForProvider provider)
            Just text -> case parseDialect text of
                Just parsed -> pure parsed
                Nothing -> fail ("unknown dialect: " <> Text.unpack text)
        unless (providerSupportsDialect provider dialect) $
            fail
                ( "dialect "
                    <> Text.unpack (dialectSlug dialect)
                    <> " is incompatible with provider "
                    <> Text.unpack (providerSlug provider)
                )
        SessionMeta version
            <$> o .: "id"
            <*> o .: "createdAt"
            <*> o .: "updatedAt"
            <*> pure provider
            <*> pure connection
            <*> pure model
            <*> o .:? "transportModel"
            <*> pure dialect
            <*> o .:? "legacySubagentTarget"
            <*> (unsafeEncodeUtf <$> o .: "cwd")
            <*> o .: "effort"
            <*> o .: "title"
            <*> (o .:? "titleIsManual" .!= False)
            <*> (o .:? "titleRefreshIndex" .!= 2)
            <*> (o .:? "titleUserTurns" .!= 6)
            <*> o .:? "lastResponseId"
            <*> (o .:? "inputTokens" .!= 0)
            <*> (o .:? "outputTokens" .!= 0)
            <*> (o .:? "cachedTokens" .!= 0)

data SessionTurn = SessionTurn
    { turnAt :: !UTCTime
    , turnUserText :: !Text
    , turnAssistantText :: !(Maybe Text)
    , turnError :: !(Maybe Text)
    , turnResponseId :: !(Maybe Text)
    , turnItems :: ![ResponseItem]
    , turnUsage :: !(Maybe TokenUsage)
    } deriving (Eq, Show)

instance ToJSON SessionTurn where
    toJSON turn = object
        [ "at" .= turn.turnAt
        , "userText" .= turn.turnUserText
        , "assistantText" .= turn.turnAssistantText
        , "error" .= turn.turnError
        , "responseId" .= turn.turnResponseId
        , "items" .= turn.turnItems
        , "usage" .= turn.turnUsage
        ]

instance FromJSON SessionTurn where
    parseJSON = withObject "SessionTurn" \o ->
        SessionTurn
            <$> o .: "at"
            <*> o .: "userText"
            <*> o .:? "assistantText"
            <*> o .:? "error"
            <*> o .:? "responseId"
            <*> o .: "items"
            <*> o .:? "usage"

-- | Ephemeral progress for a running persisted session. This lives in the
-- session temp directory rather than the transcript so polling clients can
-- explain long waits without adding synthetic conversation turns.
data SessionActivity = SessionActivity
    { activityKind :: !Text
    , activityMessage :: !Text
    , activityRetryAt :: !(Maybe UTCTime)
    , activityUpdatedAt :: !UTCTime
    } deriving (Eq, Show)

instance ToJSON SessionActivity where
    toJSON activity = object
        [ "kind" .= activity.activityKind
        , "message" .= activity.activityMessage
        , "retry_at" .= activity.activityRetryAt
        , "updated_at" .= activity.activityUpdatedAt
        ]

instance FromJSON SessionActivity where
    parseJSON = withObject "SessionActivity" \o ->
        SessionActivity
            <$> o .: "kind"
            <*> o .: "message"
            <*> o .:? "retry_at"
            <*> o .: "updated_at"

data SessionHandle = SessionHandle
    { sessionPool :: !StorePool
    , sessionDir :: !OsPath
    , sessionTempDir :: !OsPath
    , sessionMetaPath :: !OsPath
    , sessionTranscriptPath :: !OsPath
    , sessionMeta :: !SessionMeta
    }

-- | Parameters for creating a session on the first persisted turn.
data SessionCreate = SessionCreate
    { createPool :: !StorePool
    , createRoot :: !OsPath
    , createTarget :: !ModelTarget
    , createCwd :: !OsPath
    , createEffort :: !Text
    , createTitleHint :: !(Maybe Text)
    , createTitleIsManual :: !Bool
    }

-- | Whether conversation state is persisted.
data Persistence
    = PersistenceDisabled
    | PersistenceEnabled (IORef PersistenceState)

-- | An enabled persistence slot, before or after its first use.
data PersistenceState
    = PersistencePending SessionCreate Text OsPath
    | PersistenceActive SessionHandle

newPendingPersistence :: SessionCreate -> IO Persistence
newPendingPersistence spec = do
    (sessionId, tempDir) <- allocateSessionTemp spec.createRoot
    newPendingPersistenceReserved spec sessionId tempDir

newPendingPersistenceReserved
    :: SessionCreate
    -> Text
    -> OsPath
    -> IO Persistence
newPendingPersistenceReserved spec sessionId tempDir = do
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
                                    (Aeson.eitherDecode bytes)

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
    (sessionId, tempDir) <- allocateSessionTemp spec.createRoot
    createReservedSession spec sessionId tempDir
        `onException` removeReservedTemp spec.createRoot sessionId

createReservedSession
    :: SessionCreate
    -> Text
    -> OsPath
    -> IO SessionHandle
createReservedSession spec sessionId tempDir = do
    let pool = spec.createPool
    ensurePrivateDir spec.createRoot
    dir <- either (fail . Text.unpack) pure
        (sessionDirForId spec.createRoot sessionId)
    createDirectory dir
    setFileMode (unsafeToFilePath dir) 0o700
    now <- normalizePostgresTimestamp <$> getCurrentTime
    let title = case spec.createTitleHint of
            Just hint | not (Text.null hint) -> hint
            _ -> "untitled"
        meta = SessionMeta
            { metaVersion = sessionSchemaVersion
            , metaId = sessionId
            , metaCreatedAt = now
            , metaUpdatedAt = now
            , metaProvider = spec.createTarget.targetProvider
            , metaConnection = spec.createTarget.targetConnectionId
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
            }
        handle = SessionHandle
            { sessionPool = pool
            , sessionDir = dir
            , sessionTempDir = tempDir
            , sessionMetaPath = dir </> unsafeEncodeUtf "meta.json"
            , sessionTranscriptPath = dir </> unsafeEncodeUtf "transcript.jsonl"
            , sessionMeta = meta
            }
    Store.createSession pool (toStoredMetadata meta) >>= \case
        Left err -> do
            _ <- tryIO (removePathForcibly dir)
            fail
                ("could not create PostgreSQL session: "
                    <> Text.unpack (renderStoreError err))
        Right False -> do
            _ <- tryIO (removePathForcibly dir)
            fail "could not allocate a unique PostgreSQL session id"
        Right True -> pure handle

-- | Create the session directory on first use when persistence is still pending.
ensureSession :: IORef PersistenceState -> IO SessionHandle
ensureSession slotRef = do
    slot <- readIORef slotRef
    case slot of
        PersistenceActive handle -> pure handle
        PersistencePending spec sessionId tempDir -> do
            handle <- createReservedSession spec sessionId tempDir
            writeIORef slotRef (PersistenceActive handle)
            pure handle

appendTurn :: SessionHandle -> SessionTurn -> IO SessionHandle
appendTurn handle turn =
    appendTurnWithMetaUpdate handle turn id

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
    let pool = handle.sessionPool
    now <- normalizePostgresTimestamp <$> getCurrentTime
    let meta0 = handle.sessionMeta
        meta = meta0
            { metaUpdatedAt = now
            , metaLastResponseId = turn.turnResponseId <|> meta0.metaLastResponseId
            }
        finalMeta = transition meta
    Store.appendSessionTurn
        pool
        (toStoredTurn turn)
        (toStoredMetadata finalMeta) >>= \case
            Left err ->
                fail
                    ("could not append PostgreSQL session turn: "
                        <> Text.unpack (renderStoreError err))
            Right False ->
                fail ("session not found: " <> Text.unpack finalMeta.metaId)
            Right True ->
                pure handle { sessionMeta = finalMeta }

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


loadSession
    :: StorePool
    -> OsPath
    -> Text
    -> IO (Either Text (SessionMeta, [SessionTurn]))
loadSession pool root sessionId = runExceptT do
    _ <- except (sessionDirForId root sessionId)
    stored <- lift (Store.loadSession pool sessionId)
        >>= either (throwE . renderStoreError) pure
    stored' <- case stored of
        Just value -> pure (Just value)
        Nothing -> do
            _ <- importLegacySession root pool sessionId
            -- Another process may win the import race and return False from
            -- its idempotent insert. Always reload the canonical row.
            lift (Store.loadSession pool sessionId)
                >>= either (throwE . renderStoreError) pure
    case stored' of
        Nothing -> throwE ("session not found: " <> sessionId)
        Just value -> decodeStoredSession sessionId value

loadSessionHandle
    :: StorePool
    -> OsPath
    -> Text
    -> IO (Either Text (SessionHandle, [SessionTurn]))
loadSessionHandle pool root sessionId =
    loadSession pool root sessionId >>= \case
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

deleteSession :: StorePool -> OsPath -> Text -> IO (Either Text ())
deleteSession pool root sessionId = runExceptT do
    dir <- except (sessionDirForId root sessionId)
    exists <- lift (doesDirectoryExist dir)
    lock <- if exists
        then lift (acquireSessionLock dir sessionId) >>= \case
            Left _ -> throwE "cannot delete a running session"
            Right lock -> pure (Just lock)
        else pure Nothing
    now <- lift getCurrentTime
    deleted <- lift $
        Store.deleteSession pool sessionId now
            `finally` maybe (pure ()) releaseSessionLock lock
    case deleted of
        Left err -> throwE (renderStoreError err)
        Right False -> throwE ("session not found: " <> sessionId)
        Right True
            | not exists -> pure ()
            | otherwise ->
                lift (tryIO (removePathForcibly dir)) >>= \case
                    Left err ->
                        throwE
                            ("session deleted but artifacts could not be removed: "
                                <> Text.pack (displayException err))
                    Right () -> pure ()
    tempRemoved <- lift (removeSessionTemp root sessionId)
    except tempRemoved

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

listSessions :: StorePool -> OsPath -> IO [SessionMeta]
listSessions pool _root = do
    Store.listSessionMetadata pool >>= \case
        Left err ->
            fail
                ("could not list PostgreSQL sessions: "
                    <> Text.unpack (renderStoreError err))
        Right values -> pure (catMaybes (map decodeMetaQuiet values))

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
    | otherwise = writeTitle False refreshIndex rawTitle handle

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
    writeSessionMeta handle.sessionPool handle.sessionMetaPath meta
    pure handle { sessionMeta = meta }

resetSessionTitleToAuto :: SessionHandle -> IO SessionHandle
resetSessionTitleToAuto handle = do
    let meta = handle.sessionMeta
            { metaTitleIsManual = False
            , metaTitleRefreshIndex = 0
            }
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
            durableExists <- doesDirectoryExist durableDir
            if durableExists
                then go tempRoot now (attempt + 1)
                else tryIO (createDirectory tempDir) >>= \case
                    Left _ -> go tempRoot now (attempt + 1)
                    Right () -> do
                        setFileMode (unsafeToFilePath tempDir) 0o700
                        pure (sessionId, tempDir)

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

loadTranscript :: OsPath -> ExceptT Text IO [SessionTurn]
loadTranscript path = do
    exists <- lift (doesFileExist path)
    if not exists
        then pure []
        else do
            raw <- lift (retryOnFileBusy (Text.readFile (unsafeToFilePath path)))
            let linesOf = filter (not . Text.null) (Text.lines raw)
            except (mapM decodeTurnLine linesOf)

decodeTurnLine :: Text -> Either Text SessionTurn
decodeTurnLine line =
    case Aeson.eitherDecodeStrict' (Text.encodeUtf8 line) of
        Left err -> Left ("invalid transcript line: " <> Text.pack err)
        Right turn -> Right turn

decodeFileEither :: FromJSON a => OsPath -> ExceptT Text IO a
decodeFileEither path = do
    exists <- lift (doesFileExist path)
    unless exists $
        throwE ("missing file: " <> toText path)
    bytes <- lift (retryOnFileBusy (LBS.readFile (unsafeToFilePath path)))
    case Aeson.eitherDecode' bytes of
        Left err -> throwE (toText path <> ": " <> Text.pack err)
        Right value -> pure value

decodeStoredSession
    :: Text
    -> Store.StoredSession
    -> ExceptT Text IO (SessionMeta, [SessionTurn])
decodeStoredSession sessionId stored = do
    meta <- except (fromStoredMetadata stored.storedMetadata)
    validateSessionMeta sessionId meta
    turns <- except $ mapM (fromStoredTurn . (.storedTurn)) stored.storedTurns
    pure (meta, turns)

toStoredMetadata :: SessionMeta -> Store.SessionMetadata
toStoredMetadata meta = Store.SessionMetadata
    { sessionMetadataKey = meta.metaId
    , sessionMetadataVersion = fromIntegral meta.metaVersion
    , sessionMetadataCreatedAt = meta.metaCreatedAt
    , sessionMetadataUpdatedAt = meta.metaUpdatedAt
    , sessionMetadataProvider = providerSlug meta.metaProvider
    , sessionMetadataConnection = meta.metaConnection
    , sessionMetadataModel = meta.metaModel
    , sessionMetadataTransportModel = meta.metaTransportModel
    , sessionMetadataDialect = dialectSlug meta.metaDialect
    , sessionMetadataLegacyTarget =
        toStoredLegacyTarget <$> meta.metaLegacySubagentTarget
    , sessionMetadataCwd = Text.pack (unsafeToFilePath meta.metaCwd)
    , sessionMetadataEffort = meta.metaEffort
    , sessionMetadataTitle = meta.metaTitle
    , sessionMetadataTitleIsManual = meta.metaTitleIsManual
    , sessionMetadataTitleRefreshIndex =
        fromIntegral meta.metaTitleRefreshIndex
    , sessionMetadataTitleUserTurns =
        fromIntegral meta.metaTitleUserTurns
    , sessionMetadataLastResponseId = meta.metaLastResponseId
    , sessionMetadataInputTokens = fromIntegral meta.metaInputTokens
    , sessionMetadataOutputTokens = fromIntegral meta.metaOutputTokens
    , sessionMetadataCachedTokens = fromIntegral meta.metaCachedTokens
    }

fromStoredMetadata :: Store.SessionMetadata -> Either Text SessionMeta
fromStoredMetadata stored = do
    when (Text.null (Text.strip stored.sessionMetadataConnection)) $
        Left "stored session connection must not be empty"
    provider <- maybe
        (Left ("unknown stored provider: " <> stored.sessionMetadataProvider))
        Right
        (parseProvider stored.sessionMetadataProvider)
    dialect <- maybe
        (Left ("unknown stored dialect: " <> stored.sessionMetadataDialect))
        Right
        (parseDialect stored.sessionMetadataDialect)
    unless (providerSupportsDialect provider dialect) $
        Left
            ( "stored dialect "
                <> stored.sessionMetadataDialect
                <> " is incompatible with provider "
                <> stored.sessionMetadataProvider
            )
    legacyTarget <-
        traverse fromStoredLegacyTarget stored.sessionMetadataLegacyTarget
    pure SessionMeta
        { metaVersion = fromIntegral stored.sessionMetadataVersion
        , metaId = stored.sessionMetadataKey
        , metaCreatedAt = stored.sessionMetadataCreatedAt
        , metaUpdatedAt = stored.sessionMetadataUpdatedAt
        , metaProvider = provider
        , metaConnection = stored.sessionMetadataConnection
        , metaModel = stored.sessionMetadataModel
        , metaTransportModel = stored.sessionMetadataTransportModel
        , metaDialect = dialect
        , metaLegacySubagentTarget = legacyTarget
        , metaCwd =
            unsafeEncodeUtf (Text.unpack stored.sessionMetadataCwd)
        , metaEffort = stored.sessionMetadataEffort
        , metaTitle = stored.sessionMetadataTitle
        , metaTitleIsManual = stored.sessionMetadataTitleIsManual
        , metaTitleRefreshIndex =
            fromIntegral stored.sessionMetadataTitleRefreshIndex
        , metaTitleUserTurns =
            fromIntegral stored.sessionMetadataTitleUserTurns
        , metaLastResponseId = stored.sessionMetadataLastResponseId
        , metaInputTokens = fromIntegral stored.sessionMetadataInputTokens
        , metaOutputTokens = fromIntegral stored.sessionMetadataOutputTokens
        , metaCachedTokens = fromIntegral stored.sessionMetadataCachedTokens
        }

toStoredLegacyTarget
    :: LegacySubagentTarget
    -> Store.SessionLegacyTarget
toStoredLegacyTarget target = Store.SessionLegacyTarget
    { sessionLegacyProvider = providerSlug target.legacyTargetProvider
    , sessionLegacyConnection = target.legacyTargetConnection
    , sessionLegacyEffectiveModel = target.legacyTargetEffectiveModel
    , sessionLegacyDialect = dialectSlug target.legacyTargetDialect
    }

fromStoredLegacyTarget
    :: Store.SessionLegacyTarget
    -> Either Text LegacySubagentTarget
fromStoredLegacyTarget stored = do
    when (Text.null (Text.strip stored.sessionLegacyConnection)) $
        Left "stored legacy session connection must not be empty"
    when (Text.null (Text.strip stored.sessionLegacyEffectiveModel)) $
        Left "stored legacy session effective model must not be empty"
    provider <- maybe
        (Left ("unknown stored legacy provider: " <> stored.sessionLegacyProvider))
        Right
        (parseProvider stored.sessionLegacyProvider)
    dialect <- maybe
        (Left ("unknown stored legacy dialect: " <> stored.sessionLegacyDialect))
        Right
        (parseDialect stored.sessionLegacyDialect)
    unless (providerSupportsDialect provider dialect) $
        Left
            ( "stored legacy dialect "
                <> stored.sessionLegacyDialect
                <> " is incompatible with provider "
                <> stored.sessionLegacyProvider
            )
    pure LegacySubagentTarget
        { legacyTargetProvider = provider
        , legacyTargetConnection = stored.sessionLegacyConnection
        , legacyTargetEffectiveModel = stored.sessionLegacyEffectiveModel
        , legacyTargetDialect = dialect
        }

toStoredTurn :: SessionTurn -> Store.SessionTurn
toStoredTurn turn = Store.SessionTurn
    { sessionTurnOccurredAt = turn.turnAt
    , sessionTurnUserText = turn.turnUserText
    , sessionTurnAssistantText = turn.turnAssistantText
    , sessionTurnError = turn.turnError
    , sessionTurnResponseId = turn.turnResponseId
    , sessionTurnItems = map toStoredResponseItem turn.turnItems
    , sessionTurnUsage = toStoredUsage <$> turn.turnUsage
    }

fromStoredTurn :: Store.SessionTurn -> Either Text SessionTurn
fromStoredTurn stored = do
    items <- traverse fromStoredResponseItem stored.sessionTurnItems
    pure SessionTurn
        { turnAt = stored.sessionTurnOccurredAt
        , turnUserText = stored.sessionTurnUserText
        , turnAssistantText = stored.sessionTurnAssistantText
        , turnError = stored.sessionTurnError
        , turnResponseId = stored.sessionTurnResponseId
        , turnItems = items
        , turnUsage = fromStoredUsage <$> stored.sessionTurnUsage
        }

toStoredUsage :: TokenUsage -> Store.SessionUsage
toStoredUsage usage = Store.SessionUsage
    { sessionUsageInputTokens = fromIntegral usage.inputTokens
    , sessionUsageOutputTokens = fromIntegral usage.outputTokens
    , sessionUsageCachedTokens = fromIntegral usage.cachedTokens
    }

fromStoredUsage :: Store.SessionUsage -> TokenUsage
fromStoredUsage usage = TokenUsage
    { inputTokens = fromIntegral usage.sessionUsageInputTokens
    , outputTokens = fromIntegral usage.sessionUsageOutputTokens
    , cachedTokens = fromIntegral usage.sessionUsageCachedTokens
    }

decodeMetaQuiet :: Store.SessionMetadata -> Maybe SessionMeta
decodeMetaQuiet value =
    case fromStoredMetadata value of
        Right meta | meta.metaVersion == sessionSchemaVersion -> Just meta
        _ -> Nothing

validateSessionMeta :: Text -> SessionMeta -> ExceptT Text IO ()
validateSessionMeta sessionId meta = do
    unless (isValidSessionId meta.metaId) $
        throwE "invalid session id in metadata"
    unless (meta.metaId == sessionId) $
        throwE "session id does not match requested session"
    unless (meta.metaVersion == sessionSchemaVersion) $
        throwE $
            "unsupported session schema version "
                <> Text.pack (show meta.metaVersion)
                <> " (expected "
                <> Text.pack (show sessionSchemaVersion)
                <> ")"

-- | Import the old @meta.json@ + @transcript.jsonl@ representation on first
-- access.  PostgreSQL remains canonical after a successful import; the files
-- are retained as rollback/export artifacts and are never dual-written.
importLegacySession :: OsPath -> StorePool -> Text -> ExceptT Text IO Bool
importLegacySession root pool sessionId = do
    dir <- except (sessionDirForId root sessionId)
    let
        metaPath = dir </> unsafeEncodeUtf "meta.json"
        transcriptPath = dir </> unsafeEncodeUtf "transcript.jsonl"
    exists <- lift (doesDirectoryExist dir)
    if not exists
        then pure False
        else do
            meta <- decodeFileEither metaPath
            validateSessionMeta sessionId meta
            turns <- loadTranscript transcriptPath
            metaBytes <- lift (retryOnFileBusy (LBS.readFile (unsafeToFilePath metaPath)))
            transcriptExists <- lift (doesFileExist transcriptPath)
            transcriptBytes <- if transcriptExists
                then lift (retryOnFileBusy
                    (LBS.readFile (unsafeToFilePath transcriptPath)))
                else pure mempty
            let legacy = Store.LegacySession
                    { legacySourcePath = toText dir
                    , legacyContentHash =
                        contentFingerprint (metaBytes <> transcriptBytes)
                    , legacyMetadata = toStoredMetadata meta
                    , legacyTurns = map toStoredTurn turns
                    }
            lift (Store.importLegacySession pool legacy) >>= \case
                Left err -> throwE (renderStoreError err)
                Right imported -> pure imported

-- A deterministic import key without another crypto dependency.  It is used
-- only for idempotency, not authentication or corruption detection.
contentFingerprint :: LBS.ByteString -> Text
contentFingerprint =
    Text.pack . pad16 . (`showHex` "") . LBS.foldl' step fnvOffset
  where
    fnvOffset :: Word64
    fnvOffset = 14695981039346656037
    fnvPrime :: Word64
    fnvPrime = 1099511628211
    step hash byte = (hash `xor` fromIntegral byte) * fnvPrime
    pad16 text = replicate (16 - length text) '0' <> text
