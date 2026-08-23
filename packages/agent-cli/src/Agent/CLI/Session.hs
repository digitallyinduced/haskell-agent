-- | Persist REPL conversations under @~/.haskell-agent/sessions@.
module Agent.CLI.Session
    ( SessionHandle(..)
    , SessionMeta(..)
    , LegacySubagentTarget(..)
    , SessionTurn(..)
    , SessionCreate(..)
    , Persistence(..)
    , PersistenceState(..)
    , newPendingPersistence
    , newPendingPersistenceReserved
    , newActivePersistence
    , persistenceTempDir
    , cleanupPendingPersistence
    , createSession
    , appendTurn
    , appendTurnWithMetaUpdate
    , appendTurnKeepTitle
    , addSessionUsage
    , deleteSession
    , loadSession
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

import Agent.FileRetry
    ( appendLazyFileRetryingOpen
    , retryOnFileBusy
    , writeLazyFileAtomically
    )
import Agent.CLI.SessionLock
    ( acquireSessionLock
    , releaseSessionLock
    )
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
import Control.Applicative ((<|>))
import Control.Exception.Safe (displayException, finally, onException, tryIO)
import Control.Monad (unless)
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
import Data.IORef
import Data.Functor ((<&>))
import Data.List (sortOn)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (Down(..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (UTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Numeric (showHex)
import System.Directory.OsPath
    ( createDirectory
    , createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , listDirectory
    , removePathForcibly
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

-- | Durable provenance for subagent transcripts written before child target
-- metadata was persisted. Keeping this target separate from the mutable root
-- target prevents a later reopen from treating stale legacy children as
-- compatible merely because the root metadata has already been retargeted.
data LegacySubagentTarget = LegacySubagentTarget
    { legacyTargetProvider :: !Provider
    , legacyTargetEffectiveModel :: !Text
    , legacyTargetDialect :: !DialectId
    } deriving (Eq, Show)

instance ToJSON LegacySubagentTarget where
    toJSON target = object
        [ "provider" .= providerSlug target.legacyTargetProvider
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
        LegacySubagentTarget provider
            <$> o .: "effectiveModel"
            <*> pure dialect

instance ToJSON SessionMeta where
    toJSON meta = object
        [ "version" .= meta.metaVersion
        , "id" .= meta.metaId
        , "createdAt" .= meta.metaCreatedAt
        , "updatedAt" .= meta.metaUpdatedAt
        , "provider" .= providerSlug meta.metaProvider
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

data SessionHandle = SessionHandle
    { sessionDir :: !OsPath
    , sessionTempDir :: !OsPath
    , sessionMetaPath :: !OsPath
    , sessionTranscriptPath :: !OsPath
    , sessionMeta :: !SessionMeta
    } deriving (Eq, Show)

-- | Parameters for creating a session on the first persisted turn.
data SessionCreate = SessionCreate
    { createRoot :: !OsPath
    , createProvider :: !Provider
    , createModel :: !Text
    , createTransportModel :: !Text
    , createDialect :: !DialectId
    , createCwd :: !OsPath
    , createEffort :: !Text
    , createTitleHint :: !(Maybe Text)
    , createTitleIsManual :: !Bool
    } deriving (Eq, Show)

-- | Whether conversation state is stored on disk.
data Persistence
    = PersistenceDisabled
    | PersistenceEnabled (IORef PersistenceState)

-- | An enabled persistence slot, before or after its first use.
data PersistenceState
    = PersistencePending SessionCreate Text OsPath
    | PersistenceActive SessionHandle
    deriving (Eq, Show)

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
    PersistenceEnabled <$> newIORef (PersistenceActive handle)

persistenceTempDir :: Persistence -> IO (Maybe OsPath)
persistenceTempDir = \case
    PersistenceDisabled -> pure Nothing
    PersistenceEnabled slotRef ->
        readIORef slotRef <&> \case
            PersistencePending _ _ tempDir -> Just tempDir
            PersistenceActive handle -> Just handle.sessionTempDir

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
    ensurePrivateDir spec.createRoot
    dir <- either (fail . Text.unpack) pure
        (sessionDirForId spec.createRoot sessionId)
    createDirectory dir
    setFileMode (unsafeToFilePath dir) 0o700
    now <- getCurrentTime
    let title = case spec.createTitleHint of
            Just hint | not (Text.null hint) -> hint
            _ -> "untitled"
        meta = SessionMeta
            { metaVersion = sessionSchemaVersion
            , metaId = sessionId
            , metaCreatedAt = now
            , metaUpdatedAt = now
            , metaProvider = spec.createProvider
            , metaModel = spec.createModel
            , metaTransportModel = Just spec.createTransportModel
            , metaDialect = spec.createDialect
            , metaLegacySubagentTarget = Just LegacySubagentTarget
                { legacyTargetProvider = spec.createProvider
                , legacyTargetEffectiveModel = spec.createTransportModel
                , legacyTargetDialect = spec.createDialect
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
            { sessionDir = dir
            , sessionTempDir = tempDir
            , sessionMetaPath = dir </> unsafeEncodeUtf "meta.json"
            , sessionTranscriptPath = dir </> unsafeEncodeUtf "transcript.jsonl"
            , sessionMeta = meta
            }
    writeSessionMeta handle.sessionMetaPath meta
    pure handle

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
    let path = handle.sessionTranscriptPath
    existed <- doesFileExist path
    appendLazyFileRetryingOpen path (Aeson.encode turn <> "\n")
    if existed then pure () else setFileMode (unsafeToFilePath path) 0o600
    now <- getCurrentTime
    let meta0 = handle.sessionMeta
        meta = meta0
            { metaUpdatedAt = now
            , metaLastResponseId = turn.turnResponseId <|> meta0.metaLastResponseId
            }
        finalMeta = transition meta
    writeSessionMeta handle.sessionMetaPath finalMeta
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
    now <- getCurrentTime
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
    writeSessionMeta handle.sessionMetaPath meta
    pure handle { sessionMeta = meta }

-- | The original root target under which metadata-less child transcripts may
-- have been created. Old session files derive it from their persisted root
-- target; once written, it remains stable across root model/provider changes.
sessionLegacySubagentTarget :: SessionMeta -> LegacySubagentTarget
sessionLegacySubagentTarget meta =
    fromMaybe
        LegacySubagentTarget
            { legacyTargetProvider = meta.metaProvider
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


loadSession :: OsPath -> Text -> IO (Either Text (SessionMeta, [SessionTurn]))
loadSession root sessionId = runExceptT do
    dir <- except (sessionDirForId root sessionId)
    let
        metaPath = dir </> unsafeEncodeUtf "meta.json"
        transcriptPath = dir </> unsafeEncodeUtf "transcript.jsonl"
    exists <- lift (doesDirectoryExist dir)
    unless exists $
        throwE ("session not found: " <> sessionId)
    meta <- decodeFileEither metaPath
    unless (isValidSessionId meta.metaId) $
        throwE "invalid session id in metadata"
    unless (meta.metaId == sessionId) $
        throwE "session id does not match directory"
    unless (meta.metaVersion == sessionSchemaVersion) $
        throwE $
            "unsupported session schema version "
                <> Text.pack (show meta.metaVersion)
                <> " (expected "
                <> Text.pack (show sessionSchemaVersion)
                <> ")"
    turns <- loadTranscript transcriptPath
    pure (meta, turns)

deleteSession :: OsPath -> Text -> IO (Either Text ())
deleteSession root sessionId = runExceptT do
    dir <- except (sessionDirForId root sessionId)
    exists <- lift (doesDirectoryExist dir)
    unless exists $
        throwE ("session not found: " <> sessionId)
    lock <- lift (acquireSessionLock dir sessionId) >>= \case
        Left _ -> throwE "cannot delete a running session"
        Right lock -> pure lock
    removed <- lift $
        tryIO (removePathForcibly dir) `finally` releaseSessionLock lock
    case removed of
        Left err ->
            throwE ("could not delete session: " <> Text.pack (displayException err))
        Right () -> do
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

listSessions :: OsPath -> IO [SessionMeta]
listSessions root = do
    exists <- doesDirectoryExist root
    if not exists
        then pure []
        else do
            names <- listDirectory root
            metas <- mapM (readMetaQuiet root) names
            pure (sortOn (Down . (.metaUpdatedAt)) (catMaybes metas))

writeSessionMeta :: OsPath -> SessionMeta -> IO ()
writeSessionMeta path meta =
    writeLazyFileAtomically path 0o600 (Aeson.encode meta)

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
    writeSessionMeta handle.sessionMetaPath meta
    pure handle { sessionMeta = meta }

resetSessionTitleToAuto :: SessionHandle -> IO SessionHandle
resetSessionTitleToAuto handle = do
    let meta = handle.sessionMeta
            { metaTitleIsManual = False
            , metaTitleRefreshIndex = 0
            }
    writeSessionMeta handle.sessionMetaPath meta
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

readMetaQuiet :: OsPath -> OsPath -> IO (Maybe SessionMeta)
readMetaQuiet root name = do
    let path = root </> name </> unsafeEncodeUtf "meta.json"
    result <- tryIO (runExceptT (decodeFileEither path))
    pure $ case result of
        Left _ -> Nothing
        Right (Left _) -> Nothing
        Right (Right meta)
            | meta.metaVersion == sessionSchemaVersion -> Just meta
            | otherwise -> Nothing
