-- | Persist REPL conversations under @~/.haskell-agent/sessions@.
module Agent.CLI.Session
    ( SessionHandle(..)
    , SessionMeta(..)
    , SessionTurn(..)
    , SessionCreate(..)
    , createSession
    , appendTurn
    , appendTurnKeepTitle
    , loadSession
    , listSessions
    , sessionsRoot
    , sessionTitleFromPrompt
    , writeSessionMeta
    , ensureSession
    , devResumePointerPath
    , writeDevResumePointer
    , readDevResumePointer
    , clearDevResumePointer
    , resumeHint
    , sessionUsageFromTurns
    ) where

import Agent.Loop (TokenUsage(..))
import Agent.OsPath (OsPath, fromFilePath, toFilePath)
import Agent.OpenAI.Responses.Types (ResponseItem)
import Agent.Provider (Provider(..), parseProvider, providerSlug)
import Control.Applicative ((<|>))
import Control.Exception (try)
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
    , removeFile
    , renameFile
    )
import System.OsPath ((</>))
import System.Posix.Files (setFileMode)

sessionSchemaVersion :: Int
sessionSchemaVersion = 1

-- | @~/.haskell-agent/sessions@ given the user's home directory.
sessionsRoot :: OsPath -> OsPath
sessionsRoot home =
    home </> fromFilePath ".haskell-agent" </> fromFilePath "sessions"

-- | Pointer written before a GHCi @:reload@ so @devMain@ can resume.
devResumePointerPath :: OsPath -> OsPath
devResumePointerPath home =
    home </> fromFilePath ".haskell-agent" </> fromFilePath "dev-resume"

writeDevResumePointer :: OsPath -> Text -> IO ()
writeDevResumePointer home sessionId = do
    let root = home </> fromFilePath ".haskell-agent"
        path = devResumePointerPath home
    ensurePrivateDir root
    Text.writeFile (toFilePath path) (sessionId <> "\n")
    setFileMode (toFilePath path) 0o600

readDevResumePointer :: OsPath -> IO (Maybe Text)
readDevResumePointer home = do
    let path = devResumePointerPath home
    exists <- doesFileExist path
    if not exists
        then pure Nothing
        else do
            raw <- Text.strip <$> Text.readFile (toFilePath path)
            pure (if Text.null raw then Nothing else Just raw)

clearDevResumePointer :: OsPath -> IO ()
clearDevResumePointer home = do
    let path = devResumePointerPath home
    _ <- try @IOError (removeFile path)
    pure ()

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
        , "cwd" .= toFilePath meta.metaCwd
        , "effort" .= meta.metaEffort
        , "title" .= meta.metaTitle
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
            <*> (fromFilePath <$> o .: "cwd")
            <*> o .: "effort"
            <*> o .: "title"
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
    } deriving (Eq, Show)

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
            , metaLastResponseId = Nothing
            , metaInputTokens = 0
            , metaOutputTokens = 0
            , metaCachedTokens = 0
            }
        handle = SessionHandle
            { sessionDir = dir
            , sessionMetaPath = dir </> fromFilePath "meta.json"
            , sessionTranscriptPath = dir </> fromFilePath "transcript.jsonl"
            , sessionMeta = meta
            }
    writeSessionMeta handle.sessionMetaPath meta
    pure handle

-- | Create the session directory on first use when the slot is still pending.
ensureSession :: IORef (Either SessionCreate SessionHandle) -> IO SessionHandle
ensureSession slotRef = do
    slot <- readIORef slotRef
    case slot of
        Right handle -> pure handle
        Left spec -> do
            handle <- createSession spec
            writeIORef slotRef (Right handle)
            pure handle

appendTurn :: SessionHandle -> SessionTurn -> IO SessionHandle
appendTurn handle turn = do
    let path = handle.sessionTranscriptPath
    existed <- doesFileExist path
    LBS.appendFile (toFilePath path) (Aeson.encode turn <> "\n")
    if existed then pure () else setFileMode (toFilePath path) 0o600
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

-- | Like 'appendTurn', but never derives the session title from this turn.
-- Used for synthetic markers such as @/new@ and @/clear@.
appendTurnKeepTitle :: SessionHandle -> SessionTurn -> IO SessionHandle
appendTurnKeepTitle handle turn = do
    let path = handle.sessionTranscriptPath
    existed <- doesFileExist path
    LBS.appendFile (toFilePath path) (Aeson.encode turn <> "\n")
    if existed then pure () else setFileMode (toFilePath path) 0o600
    now <- getCurrentTime
    let meta0 = handle.sessionMeta
        meta = meta0
            { metaUpdatedAt = now
            , metaLastResponseId = turn.turnResponseId <|> meta0.metaLastResponseId
            }
    writeSessionMeta handle.sessionMetaPath meta
    pure handle { sessionMeta = meta }


loadSession :: OsPath -> Text -> IO (Either String (SessionMeta, [SessionTurn]))
loadSession root sessionId = do
    let dir = root </> fromFilePath (Text.unpack sessionId)
        metaPath = dir </> fromFilePath "meta.json"
        transcriptPath = dir </> fromFilePath "transcript.jsonl"
    exists <- doesDirectoryExist dir
    if not exists
        then pure (Left ("session not found: " <> Text.unpack sessionId))
        else do
            metaResult <- decodeFileEither metaPath
            case metaResult of
                Left err -> pure (Left err)
                Right meta
                    | meta.metaVersion /= sessionSchemaVersion ->
                        pure $ Left $
                            "unsupported session schema version "
                                <> show meta.metaVersion
                                <> " (expected "
                                <> show sessionSchemaVersion
                                <> ")"
                    | otherwise -> do
                        turnsResult <- loadTranscript transcriptPath
                        pure ((meta,) <$> turnsResult)

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
writeSessionMeta path meta = do
    let tmp = path <> fromFilePath ".tmp"
    LBS.writeFile (toFilePath tmp) (Aeson.encode meta)
    setFileMode (toFilePath tmp) 0o600
    renameOrReplace tmp path
    setFileMode (toFilePath path) 0o600

sessionTitleFromPrompt :: Text -> Text
sessionTitleFromPrompt prompt =
    let oneLine = Text.unwords (Text.words (Text.strip prompt))
    in if Text.length oneLine <= 72
        then oneLine
        else Text.take 69 oneLine <> "..."

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
                dir = root </> fromFilePath (Text.unpack sessionId)
            result <- try @IOError (createDirectory dir)
            case result of
                Left _ -> go (attempt + 1)
                Right () -> do
                    setFileMode (toFilePath dir) 0o700
                    pure (sessionId, dir)

hex8 :: Integer -> String
hex8 n =
    let s = showHex (n `mod` 0x100000000) ""
    in replicate (8 - length s) '0' <> s

ensurePrivateDir :: OsPath -> IO ()
ensurePrivateDir path = do
    createDirectoryIfMissing True path
    _ <- try @IOError (setFileMode (toFilePath path) 0o700)
    pure ()

loadTranscript :: OsPath -> IO (Either String [SessionTurn])
loadTranscript path = do
    exists <- doesFileExist path
    if not exists
        then pure (Right [])
        else do
            raw <- Text.readFile (toFilePath path)
            let linesOf = filter (not . Text.null) (Text.lines raw)
            pure (mapM decodeTurnLine linesOf)

decodeTurnLine :: Text -> Either String SessionTurn
decodeTurnLine line =
    case Aeson.eitherDecodeStrict' (Text.encodeUtf8 line) of
        Left err -> Left ("invalid transcript line: " <> err)
        Right turn -> Right turn

decodeFileEither :: FromJSON a => OsPath -> IO (Either String a)
decodeFileEither path = do
    exists <- doesFileExist path
    if not exists
        then pure (Left ("missing file: " <> toFilePath path))
        else do
            bytes <- LBS.readFile (toFilePath path)
            pure (case Aeson.eitherDecode' bytes of
                Left err -> Left (toFilePath path <> ": " <> err)
                Right value -> Right value)

readMetaQuiet :: OsPath -> OsPath -> IO (Maybe SessionMeta)
readMetaQuiet root name = do
    let path = root </> name </> fromFilePath "meta.json"
    result <- try @IOError (decodeFileEither path)
    pure $ case result of
        Left _ -> Nothing
        Right (Left _) -> Nothing
        Right (Right meta)
            | meta.metaVersion == sessionSchemaVersion -> Just meta
            | otherwise -> Nothing

renameOrReplace :: OsPath -> OsPath -> IO ()
renameOrReplace tmp path = do
    result <- try @IOError (renameFile tmp path)
    case result of
        Right () -> pure ()
        Left _ -> do
            bytes <- LBS.readFile (toFilePath tmp)
            LBS.writeFile (toFilePath path) bytes
            _ <- try @IOError (removeFile tmp)
            pure ()
