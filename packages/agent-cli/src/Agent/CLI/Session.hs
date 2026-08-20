-- | Persist REPL conversations under @~/.haskell-agent/sessions@.
module Agent.CLI.Session
    ( SessionHandle(..)
    , SessionMeta(..)
    , SessionTurn(..)
    , SessionCreate(..)
    , createSession
    , appendTurn
    , loadSession
    , listSessions
    , sessionsRoot
    , sessionTitleFromPrompt
    , writeSessionMeta
    , ensureSession
    , resumeHint
    ) where

import Agent.OpenAI.Responses.Types (ResponseItem)
import Agent.Provider (Provider(..), parseProvider, providerSlug)
import Control.Applicative ((<|>))
import Control.Exception (try)
import Data.Aeson (FromJSON(..), ToJSON(..), object, withObject, (.:), (.:?), (.=))
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
import System.Directory
    ( createDirectory
    , createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , listDirectory
    , removeFile
    , renameFile
    )
import System.FilePath ((</>))
import System.Posix.Files (setFileMode)

sessionSchemaVersion :: Int
sessionSchemaVersion = 1

-- | @~/.haskell-agent/sessions@ given the user's home directory.
sessionsRoot :: FilePath -> FilePath
sessionsRoot home = home </> ".haskell-agent" </> "sessions"

data SessionMeta = SessionMeta
    { metaVersion :: !Int
    , metaId :: !Text
    , metaCreatedAt :: !UTCTime
    , metaUpdatedAt :: !UTCTime
    , metaProvider :: !Provider
    , metaModel :: !Text
    , metaCwd :: !FilePath
    , metaEffort :: !Text
    , metaTitle :: !Text
    , metaLastResponseId :: !(Maybe Text)
    } deriving (Eq, Show)

instance ToJSON SessionMeta where
    toJSON meta = object
        [ "version" .= meta.metaVersion
        , "id" .= meta.metaId
        , "createdAt" .= meta.metaCreatedAt
        , "updatedAt" .= meta.metaUpdatedAt
        , "provider" .= providerSlug meta.metaProvider
        , "model" .= meta.metaModel
        , "cwd" .= meta.metaCwd
        , "effort" .= meta.metaEffort
        , "title" .= meta.metaTitle
        , "lastResponseId" .= meta.metaLastResponseId
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
            <*> o .: "cwd"
            <*> o .: "effort"
            <*> o .: "title"
            <*> o .:? "lastResponseId"

data SessionTurn = SessionTurn
    { turnAt :: !UTCTime
    , turnUserText :: !Text
    , turnAssistantText :: !(Maybe Text)
    , turnResponseId :: !(Maybe Text)
    , turnItems :: ![ResponseItem]
    } deriving (Eq, Show)

instance ToJSON SessionTurn where
    toJSON turn = object
        [ "at" .= turn.turnAt
        , "userText" .= turn.turnUserText
        , "assistantText" .= turn.turnAssistantText
        , "responseId" .= turn.turnResponseId
        , "items" .= turn.turnItems
        ]

instance FromJSON SessionTurn where
    parseJSON = withObject "SessionTurn" \o ->
        SessionTurn
            <$> o .: "at"
            <*> o .: "userText"
            <*> o .:? "assistantText"
            <*> o .:? "responseId"
            <*> o .: "items"

data SessionHandle = SessionHandle
    { sessionDir :: !FilePath
    , sessionMetaPath :: !FilePath
    , sessionTranscriptPath :: !FilePath
    , sessionMeta :: !SessionMeta
    } deriving (Eq, Show)

-- | Parameters for creating a session on the first persisted turn.
data SessionCreate = SessionCreate
    { createRoot :: !FilePath
    , createProvider :: !Provider
    , createModel :: !Text
    , createCwd :: !FilePath
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
            }
        handle = SessionHandle
            { sessionDir = dir
            , sessionMetaPath = dir </> "meta.json"
            , sessionTranscriptPath = dir </> "transcript.jsonl"
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
    LBS.appendFile path (Aeson.encode turn <> "\n")
    if existed then pure () else setFileMode path 0o600
    now <- getCurrentTime
    let meta0 = handle.sessionMeta
        meta = meta0
            { metaUpdatedAt = now
            , metaLastResponseId = turn.turnResponseId <|> meta0.metaLastResponseId
            , metaTitle =
                if meta0.metaTitle == "untitled" && not (Text.null turn.turnUserText)
                    then sessionTitleFromPrompt turn.turnUserText
                    else meta0.metaTitle
            }
    writeSessionMeta handle.sessionMetaPath meta
    pure handle { sessionMeta = meta }

loadSession :: FilePath -> Text -> IO (Either String (SessionMeta, [SessionTurn]))
loadSession root sessionId = do
    let dir = root </> Text.unpack sessionId
        metaPath = dir </> "meta.json"
        transcriptPath = dir </> "transcript.jsonl"
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

listSessions :: FilePath -> IO [SessionMeta]
listSessions root = do
    exists <- doesDirectoryExist root
    if not exists
        then pure []
        else do
            names <- listDirectory root
            metas <- mapM (readMetaQuiet root) names
            pure (sortOn (Down . (.metaUpdatedAt)) (catMaybes metas))

writeSessionMeta :: FilePath -> SessionMeta -> IO ()
writeSessionMeta path meta = do
    let tmp = path <> ".tmp"
    LBS.writeFile tmp (Aeson.encode meta)
    setFileMode tmp 0o600
    renameOrReplace tmp path
    setFileMode path 0o600

sessionTitleFromPrompt :: Text -> Text
sessionTitleFromPrompt prompt =
    let oneLine = Text.unwords (Text.words (Text.strip prompt))
    in if Text.length oneLine <= 72
        then oneLine
        else Text.take 69 oneLine <> "..."

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

allocateSessionDir :: FilePath -> UTCTime -> IO (Text, FilePath)
allocateSessionDir root now = go (0 :: Int)
  where
    day = formatTime defaultTimeLocale "%Y-%m-%d" now
    start = floor (nominalDiffTimeToSeconds (utcTimeToPOSIXSeconds now) * 1000000) :: Integer
    go attempt
        | attempt >= 32 = fail "could not allocate a unique session id"
        | otherwise = do
            let hex = hex8 (start + fromIntegral attempt)
                sessionId = Text.pack (day <> "-" <> hex)
                dir = root </> Text.unpack sessionId
            result <- try @IOError (createDirectory dir)
            case result of
                Left _ -> go (attempt + 1)
                Right () -> do
                    setFileMode dir 0o700
                    pure (sessionId, dir)

hex8 :: Integer -> String
hex8 n =
    let s = showHex (n `mod` 0x100000000) ""
    in replicate (8 - length s) '0' <> s

ensurePrivateDir :: FilePath -> IO ()
ensurePrivateDir path = do
    createDirectoryIfMissing True path
    _ <- try @IOError (setFileMode path 0o700)
    pure ()

loadTranscript :: FilePath -> IO (Either String [SessionTurn])
loadTranscript path = do
    exists <- doesFileExist path
    if not exists
        then pure (Right [])
        else do
            raw <- Text.readFile path
            let linesOf = filter (not . Text.null) (Text.lines raw)
            pure (mapM decodeTurnLine linesOf)

decodeTurnLine :: Text -> Either String SessionTurn
decodeTurnLine line =
    case Aeson.eitherDecodeStrict' (Text.encodeUtf8 line) of
        Left err -> Left ("invalid transcript line: " <> err)
        Right turn -> Right turn

decodeFileEither :: FromJSON a => FilePath -> IO (Either String a)
decodeFileEither path = do
    exists <- doesFileExist path
    if not exists
        then pure (Left ("missing file: " <> path))
        else do
            bytes <- LBS.readFile path
            pure (case Aeson.eitherDecode' bytes of
                Left err -> Left (path <> ": " <> err)
                Right value -> Right value)

readMetaQuiet :: FilePath -> FilePath -> IO (Maybe SessionMeta)
readMetaQuiet root name = do
    let path = root </> name </> "meta.json"
    result <- try @IOError (decodeFileEither path)
    pure $ case result of
        Left _ -> Nothing
        Right (Left _) -> Nothing
        Right (Right meta)
            | meta.metaVersion == sessionSchemaVersion -> Just meta
            | otherwise -> Nothing

renameOrReplace :: FilePath -> FilePath -> IO ()
renameOrReplace tmp path = do
    result <- try @IOError (renameFile tmp path)
    case result of
        Right () -> pure ()
        Left _ -> do
            bytes <- LBS.readFile tmp
            LBS.writeFile path bytes
            _ <- try @IOError (removeFile tmp)
            pure ()
