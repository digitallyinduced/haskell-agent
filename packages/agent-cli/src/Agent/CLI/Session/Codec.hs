-- | Persistence codecs and legacy on-disk session import.
module Agent.CLI.Session.Codec
    ( contentFingerprint
    , loadTranscript
    , decodeFileEither
    , decodeStoredSession
    , toStoredMetadata
    , fromStoredMetadata
    , toStoredTurn
    , fromStoredTurn
    , toStoredUsage
    , fromStoredUsage
    , validateSessionMeta
    , importLegacySession
    ) where

import Agent.CLI.Session.StoreCodec (fromStoredResponseItem, toStoredResponseItem)
import Agent.CLI.Session.Types
import Agent.CLI.Json (decodeLazy)
import qualified Agent.Json.Decode as Hermes
import Agent.Dialect (dialectSlug, parseDialect, providerSupportsDialect)
import Agent.FileRetry (retryOnFileBusy)
import Agent.Loop (TokenUsage(..))
import Agent.OsPath (toText, unsafeToFilePath)
import Agent.Provider (parseProvider, providerSlug)
import Agent.Store.Postgres.Connection (StorePool)
import qualified Agent.Store.Postgres.Session as Store
import Agent.Store.Types (renderStoreError)
import Control.Monad (unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, except, throwE)
import qualified Data.ByteString.Lazy as LBS
import Data.Bits (xor)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.IO as Text
import Data.Word (Word64)
import qualified Data.Vector as Vector
import Numeric (showHex)
import System.Directory.OsPath (doesDirectoryExist, doesFileExist)
import System.OsPath (OsPath, (</>), unsafeEncodeUtf)

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
    case Hermes.decodeEither sessionTurnDecoder
            (TextEncoding.encodeUtf8 line) of
        Left err ->
            Left ("invalid transcript line: "
                <> Hermes.jsonErrorMessage err)
        Right turn -> Right turn

decodeFileEither
    :: Hermes.Decoder a
    -> OsPath
    -> ExceptT Text IO a
decodeFileEither decoder path = do
    exists <- lift (doesFileExist path)
    unless exists $
        throwE ("missing file: " <> toText path)
    bytes <- lift (retryOnFileBusy (LBS.readFile (unsafeToFilePath path)))
    case decodeLazy decoder bytes of
        Left err -> throwE (toText path <> ": " <> err)
        Right value -> pure value

decodeStoredSession
    :: Int
    -> (Text -> Bool)
    -> Text
    -> Store.StoredSession
    -> ExceptT Text IO (SessionMeta, [SessionTurn])
decodeStoredSession schemaVersion validId sessionId stored = do
    meta <- except (fromStoredMetadata stored.storedMetadata)
    validateSessionMeta schemaVersion validId sessionId meta
    turns <- except $
        traverse
            (fromStoredTurn . (.storedTurn))
            (Vector.toList stored.storedTurns)
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
    , sessionMetadataLastRecap = meta.metaLastRecap
    , sessionMetadataLastTurnSummary = meta.metaLastTurnSummary
    , sessionMetadataLastRecapMainTurns =
        fromIntegral meta.metaLastRecapMainTurns
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
        , metaLastRecap = stored.sessionMetadataLastRecap
        , metaLastTurnSummary = stored.sessionMetadataLastTurnSummary
        , metaLastRecapMainTurns =
            fromIntegral stored.sessionMetadataLastRecapMainTurns
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
    , sessionTurnEffect = turn.turnEffect
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
        , turnEffect = stored.sessionTurnEffect
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

validateSessionMeta
    :: Int
    -> (Text -> Bool)
    -> Text
    -> SessionMeta
    -> ExceptT Text IO ()
validateSessionMeta schemaVersion validId sessionId meta = do
    unless (validId meta.metaId) $
        throwE "invalid session id in metadata"
    unless (meta.metaId == sessionId) $
        throwE "session id does not match requested session"
    unless (meta.metaVersion == schemaVersion) $
        throwE $
            "unsupported session schema version "
                <> Text.pack (show meta.metaVersion)
                <> " (expected "
                <> Text.pack (show schemaVersion)
                <> ")"

-- | Import the old @meta.json@ + @transcript.jsonl@ representation on first
-- access.  PostgreSQL remains canonical after a successful import; the files
-- are retained as rollback/export artifacts and are never dual-written.
importLegacySession
    :: Int
    -> (Text -> Bool)
    -> (Text -> Either Text OsPath)
    -> StorePool
    -> Text
    -> ExceptT Text IO Bool
importLegacySession schemaVersion validId sessionDirForId pool sessionId = do
    dir <- except (sessionDirForId sessionId)
    let
        metaPath = dir </> unsafeEncodeUtf "meta.json"
        transcriptPath = dir </> unsafeEncodeUtf "transcript.jsonl"
    exists <- lift (doesDirectoryExist dir)
    if not exists
        then pure False
        else do
            meta <- decodeFileEither sessionMetaDecoder metaPath
            validateSessionMeta schemaVersion validId sessionId meta
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
