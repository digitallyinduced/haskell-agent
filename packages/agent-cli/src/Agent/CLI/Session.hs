-- | Persist REPL conversations under @~/.haskell-agent/sessions@.
module Agent.CLI.Session
    ( SessionHandle(..)
    , SessionMeta(..)
    , SessionTurn(..)
    , SessionCreate(..)
    , Persistence(..)
    , PersistenceState(..)
    , newPendingPersistence
    , newActivePersistence
    , createSession
    , appendTurn
    , appendTurnKeepTitle
    , loadSession
    , isValidSessionId
    , listSessions
    , sessionsRoot
    , sessionTitleFromPrompt
    , setGeneratedSessionTitle
    , setManualSessionTitle
    , resetSessionTitleToAuto
    , sessionConversationText
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
import Agent.Loop (TokenUsage(..))
import Agent.OsPath (toText)
import Agent.Responses.Types (ResponseItem)
import Agent.Provider (Provider(..), parseProvider, providerSlug)
import Control.Applicative ((<|>))
import Control.Exception.Safe (impureThrow, tryIO)
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
import Data.List (sortOn)
import Data.Maybe (catMaybes)
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
    )
import System.OsPath (OsPath, decodeUtf, unsafeEncodeUtf, (</>))
import System.Posix.Files (setFileMode)

sessionSchemaVersion :: Int
sessionSchemaVersion = 1

-- | @~/.haskell-agent/sessions@ given the user's home directory.
sessionsRoot :: OsPath -> OsPath
sessionsRoot home =
    home </> unsafeEncodeUtf ".haskell-agent" </> unsafeEncodeUtf "sessions"

data SessionMeta = SessionMeta
    { metaVersion :: !Int
    , metaId :: !Text
    , metaCreatedAt :: !UTCTime
    , metaUpdatedAt :: !UTCTime
    , metaProvider :: !Provider
    , metaModel :: !Text
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

instance ToJSON SessionMeta where
    toJSON meta = object
        [ "version" .= meta.metaVersion
        , "id" .= meta.metaId
        , "createdAt" .= meta.metaCreatedAt
        , "updatedAt" .= meta.metaUpdatedAt
        , "provider" .= providerSlug meta.metaProvider
        , "model" .= meta.metaModel
        , "cwd" .= decodeUtfPath meta.metaCwd
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
        SessionMeta version
            <$> o .: "id"
            <*> o .: "createdAt"
            <*> o .: "updatedAt"
            <*> pure provider
            <*> o .: "model"
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
    , sessionMetaPath :: !OsPath
    , sessionTranscriptPath :: !OsPath
    , sessionMeta :: !SessionMeta
    } deriving (Eq, Show)

-- | Parameters for creating a session on the first persisted turn.
data SessionCreate = SessionCreate
    { createRoot :: !OsPath
    , createProvider :: !Provider
    , createModel :: !Text
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
    = PersistencePending SessionCreate
    | PersistenceActive SessionHandle
    deriving (Eq, Show)

newPendingPersistence :: SessionCreate -> IO Persistence
newPendingPersistence spec =
    PersistenceEnabled <$> newIORef (PersistencePending spec)

newActivePersistence :: SessionHandle -> IO Persistence
newActivePersistence handle =
    PersistenceEnabled <$> newIORef (PersistenceActive handle)

createSession :: SessionCreate -> IO SessionHandle
createSession spec = do
    ensurePrivateDir spec.createRoot
    now <- getCurrentTime
    (sessionId, dir) <- allocateSessionDir spec.createRoot now
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
        PersistencePending spec -> do
            handle <- createSession spec
            writeIORef slotRef (PersistenceActive handle)
            pure handle

appendTurn :: SessionHandle -> SessionTurn -> IO SessionHandle
appendTurn handle turn = do
    let path = handle.sessionTranscriptPath
    existed <- doesFileExist path
    appendLazyFileRetryingOpen path (Aeson.encode turn <> "\n")
    if existed then pure () else setFileMode (decodeUtfPath path) 0o600
    now <- getCurrentTime
    let meta0 = handle.sessionMeta
        meta = meta0
            { metaUpdatedAt = now
            , metaLastResponseId = turn.turnResponseId <|> meta0.metaLastResponseId
            , metaTitle =
                if meta0.metaTitle == "untitled" && not (Text.null turn.turnUserText)
                    then sessionTitleFromPrompt turn.turnUserText
                    else meta0.metaTitle
            , metaInputTokens =
                meta0.metaInputTokens + maybe 0 (.inputTokens) turn.turnUsage
            , metaOutputTokens =
                meta0.metaOutputTokens + maybe 0 (.outputTokens) turn.turnUsage
            , metaCachedTokens =
                meta0.metaCachedTokens + maybe 0 (.cachedTokens) turn.turnUsage
            }
    writeSessionMeta handle.sessionMetaPath meta
    pure handle { sessionMeta = meta }

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
            PersistencePending _ -> pure 0
            PersistenceActive handle -> pure handle.sessionMeta.metaTitleUserTurns

-- | Like 'appendTurn', but never derives the session title from this turn.
-- Used for synthetic markers such as @/new@ and @/clear@.
appendTurnKeepTitle :: SessionHandle -> SessionTurn -> IO SessionHandle
appendTurnKeepTitle handle turn = do
    let path = handle.sessionTranscriptPath
    existed <- doesFileExist path
    appendLazyFileRetryingOpen path (Aeson.encode turn <> "\n")
    if existed then pure () else setFileMode (decodeUtfPath path) 0o600
    now <- getCurrentTime
    let meta0 = handle.sessionMeta
        meta = meta0
            { metaUpdatedAt = now
            , metaLastResponseId = turn.turnResponseId <|> meta0.metaLastResponseId
            }
    writeSessionMeta handle.sessionMetaPath meta
    pure handle { sessionMeta = meta }


loadSession :: OsPath -> Text -> IO (Either Text (SessionMeta, [SessionTurn]))
loadSession root sessionId = runExceptT do
    unless (isValidSessionId sessionId) $
        throwE "invalid session id"
    let dir = root </> unsafeEncodeUtf (Text.unpack sessionId)
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

-- | Session ids are single path components. Keep this deliberately broader
-- than the current date-plus-hex allocator so older ids remain resumable.
isValidSessionId :: Text -> Bool
isValidSessionId sessionId =
    not (Text.null sessionId)
        && sessionId /= "."
        && sessionId /= ".."
        && Text.all (\char -> char /= '/' && char /= '\\' && char /= '\NUL') sessionId

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

allocateSessionDir :: OsPath -> UTCTime -> IO (Text, OsPath)
allocateSessionDir root now = go (0 :: Int)
  where
    day = formatTime defaultTimeLocale "%Y-%m-%d" now
    start = floor (nominalDiffTimeToSeconds (utcTimeToPOSIXSeconds now) * 1000000) :: Integer
    go attempt
        | attempt >= 32 = fail "could not allocate a unique session id"
        | otherwise = do
            let hex = hex8 (start + fromIntegral attempt)
                sessionId = Text.pack (day <> "-" <> hex)
                dir = root </> unsafeEncodeUtf (Text.unpack sessionId)
            result <- tryIO (createDirectory dir)
            case result of
                Left _ -> go (attempt + 1)
                Right () -> do
                    setFileMode (decodeUtfPath dir) 0o700
                    pure (sessionId, dir)

hex8 :: Integer -> String
hex8 n =
    let s = showHex (n `mod` 0x100000000) ""
    in replicate (8 - length s) '0' <> s

ensurePrivateDir :: OsPath -> IO ()
ensurePrivateDir path = do
    createDirectoryIfMissing True path
    _ <- tryIO (setFileMode (decodeUtfPath path) 0o700)
    pure ()

loadTranscript :: OsPath -> ExceptT Text IO [SessionTurn]
loadTranscript path = do
    exists <- lift (doesFileExist path)
    if not exists
        then pure []
        else do
            raw <- lift (retryOnFileBusy (Text.readFile (decodeUtfPath path)))
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
    bytes <- lift (retryOnFileBusy (LBS.readFile (decodeUtfPath path)))
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

decodeUtfPath :: OsPath -> FilePath
decodeUtfPath = either impureThrow id . decodeUtf
