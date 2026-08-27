-- | Persist per-subagent transcripts under a parent session directory.
--
-- Layout:
--
-- @
--   <sessionDir>/agents/<agentId>/meta.json
--   <sessionDir>/agents/<agentId>/transcript.json
-- @
module Agent.CLI.SubagentStore
    ( SubagentDiskFields(..)
    , SubagentTarget(..)
    , LegacySubagentTargetFields(..)
    , SubagentDiskMeta(..)
    , SubagentStateSnapshot(..)
    , subagentDiskFields
    , isValidSubagentStoreId
    , subagentStoreDir
    , forkSubagentTranscript
    , saveSubagentState
    , loadSubagentState
    ) where

import Agent.CLI.Btw (trimDanglingToolSuffix)
import Agent.Dialect
    ( DialectId
    , dialectSlug
    , parseDialect
    )
import Agent.FileRetry (retryOnFileBusy, writeLazyFileAtomically)
import qualified Agent.Json.Decoder as JsonDecoder
import qualified Agent.Json.Encoder as JsonEncoder
import Agent.OsPath (toText, unsafeToFilePath)
import Agent.Provider (Provider, parseProvider, providerSlug)
import Agent.Responses.Types
import Agent.Subagents (SubagentId(..), SubagentIdentity(..), SubagentStatus(..))
import Agent.Subagents.TaskPath (taskPathText)
import Agent.ToolArgs (readExactInt)
import Control.Applicative ((<|>))
import Control.Exception.Safe (tryAny)
import Data.Aeson (FromJSON(..), ToJSON(..), object, withObject, (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory.OsPath
    ( createDirectoryIfMissing
    , doesFileExist
    )
import System.OsPath (OsPath, unsafeEncodeUtf, (</>))
import System.Posix.Files (setFileMode)

-- | Metadata shared by current and legacy subagent stores.
--
-- Current writers always persist a status, while older stores may omit it.
-- The target fields are kept out of this record because their completeness is
-- what distinguishes current metadata from legacy metadata.
data SubagentDiskFields = SubagentDiskFields
    { diskPreviousResponseId :: !(Maybe Text)
    , diskStatus :: !(Maybe SubagentStatus)
    , diskAgentType :: !(Maybe Text)
    , diskAgentModel :: !(Maybe Text)
    , diskReasoningEffort :: !(Maybe Text)
    , diskCwd :: !(Maybe OsPath)
    , diskTaskPath :: !(Maybe Text)
    , diskParentId :: !(Maybe SubagentId)
    , diskDepth :: !(Maybe Int)
    } deriving (Eq, Show)

-- | Complete target metadata written by current versions.
data SubagentTarget = SubagentTarget
    { targetProvider :: !Provider
    , targetConnection :: !Text
    , targetEffectiveModel :: !Text
    , targetDialect :: !DialectId
    } deriving (Eq, Show)

-- | Partial target metadata accepted from older @meta.json@ files.
data LegacySubagentTargetFields = LegacySubagentTargetFields
    { legacyDiskProvider :: !(Maybe Provider)
    , legacyDiskConnection :: !(Maybe Text)
    , legacyDiskEffectiveModel :: !(Maybe Text)
    , legacyDiskDialect :: !(Maybe DialectId)
    } deriving (Eq, Show)

-- | Persisted metadata is either current, with a complete restoration target,
-- or legacy, with a target that must be normalized against the parent
-- session's durable legacy target before use.
data SubagentDiskMeta
    = CurrentSubagentDiskMeta !SubagentDiskFields !SubagentTarget
    | LegacySubagentDiskMeta !SubagentDiskFields !LegacySubagentTargetFields
    deriving (Eq, Show)

-- | Named inputs for a subagent snapshot save.
data SubagentStateSnapshot = SubagentStateSnapshot
    { snapshotItems :: ![ResponseItem]
    , snapshotPreviousResponseId :: !(Maybe Text)
    , snapshotStatus :: !SubagentStatus
    , snapshotTarget :: !SubagentTarget
    , snapshotAgentType :: !(Maybe Text)
    , snapshotAgentModel :: !(Maybe Text)
    , snapshotReasoningEffort :: !(Maybe Text)
    , snapshotCwd :: !(Maybe OsPath)
    , snapshotIdentity :: !(Maybe SubagentIdentity)
    }

subagentDiskFields :: SubagentDiskMeta -> SubagentDiskFields
subagentDiskFields = \case
    CurrentSubagentDiskMeta fields _ -> fields
    LegacySubagentDiskMeta fields _ -> fields

instance ToJSON SubagentDiskMeta where
    toJSON meta = object
        [ "previousResponseId" .= fields.diskPreviousResponseId
        , "status" .= fmap encodeDiskStatus fields.diskStatus
        , "provider" .= fmap providerSlug provider
        , "connection" .= connection
        , "effectiveModel" .= effectiveModel
        , "dialect" .= fmap dialectSlug dialect
        , "agentType" .= fields.diskAgentType
        , "agentModel" .= fields.diskAgentModel
        , "reasoningEffort" .= fields.diskReasoningEffort
        , "cwd" .= fmap unsafeToFilePath fields.diskCwd
        , "taskPath" .= fields.diskTaskPath
        , "parentId" .= fields.diskParentId
        , "depth" .= fields.diskDepth
        ]
      where
        fields = subagentDiskFields meta
        (provider, connection, effectiveModel, dialect) = case meta of
            CurrentSubagentDiskMeta _ target ->
                ( Just target.targetProvider
                , Just target.targetConnection
                , Just target.targetEffectiveModel
                , Just target.targetDialect
                )
            LegacySubagentDiskMeta _ legacy ->
                ( legacy.legacyDiskProvider
                , legacy.legacyDiskConnection
                , legacy.legacyDiskEffectiveModel
                , legacy.legacyDiskDialect
                )

instance FromJSON SubagentDiskMeta where
    parseJSON = withObject "SubagentDiskMeta" \o -> do
        statusValue <- o .:? "status"
        diskStatus <- traverse decodeDiskStatus statusValue
        providerText <- o .:? "provider"
        diskProvider <- traverse parseDiskProvider providerText
        storedConnection <- o .:? "connection"
        let diskConnection =
                storedConnection <|> (providerSlug <$> diskProvider)
        dialectText <- o .:? "dialect"
        diskDialect <- traverse parseDiskDialect dialectText
        diskEffectiveModel <- o .:? "effectiveModel"
        fields <- SubagentDiskFields
            <$> o .:? "previousResponseId"
            <*> pure diskStatus
            <*> o .:? "agentType"
            <*> o .:? "agentModel"
            <*> o .:? "reasoningEffort"
            <*> (fmap unsafeEncodeUtf <$> o .:? "cwd")
            <*> o .:? "taskPath"
            <*> o .:? "parentId"
            <*> o .:? "depth"
        pure $ case
                (diskProvider, diskConnection, diskEffectiveModel, diskDialect)
                of
            (Just provider, Just connection, Just effectiveModel, Just dialect) ->
                CurrentSubagentDiskMeta fields SubagentTarget
                    { targetProvider = provider
                    , targetConnection = connection
                    , targetEffectiveModel = effectiveModel
                    , targetDialect = dialect
                    }
            _ ->
                LegacySubagentDiskMeta fields LegacySubagentTargetFields
                    { legacyDiskProvider = diskProvider
                    , legacyDiskConnection = diskConnection
                    , legacyDiskEffectiveModel = diskEffectiveModel
                    , legacyDiskDialect = diskDialect
                    }

parseDiskDialect :: Text -> Parser DialectId
parseDiskDialect text =
    case parseDialect text of
        Just dialect -> pure dialect
        Nothing -> fail ("unknown persisted subagent dialect: " <> Text.unpack text)

parseDiskProvider :: Text -> Parser Provider
parseDiskProvider text =
    case parseProvider text of
        Just provider -> pure provider
        Nothing -> fail ("unknown persisted subagent provider: " <> Text.unpack text)

encodeDiskStatus :: SubagentStatus -> Aeson.Value
encodeDiskStatus = \case
    Pending -> Aeson.String "pending"
    Running -> Aeson.String "running"
    Interrupted -> Aeson.String "interrupted"
    Closed -> Aeson.String "closed"
    NotFound -> Aeson.String "not_found"
    Completed finalText -> Aeson.object ["completed" .= finalText]
    Errored err -> Aeson.object ["errored" .= err]

decodeDiskStatus :: Aeson.Value -> Parser SubagentStatus
decodeDiskStatus = \case
    Aeson.String "pending" -> pure Pending
    Aeson.String "pending_init" -> pure Pending
    Aeson.String "running" -> pure Running
    Aeson.String "interrupted" -> pure Interrupted
    Aeson.String "closed" -> pure Closed
    Aeson.String "shutdown" -> pure Closed
    Aeson.String "not_found" -> pure NotFound
    Aeson.Object object
        | Just value <- KeyMap.lookup "completed" object ->
            Completed <$> Aeson.parseJSON value
        | Just value <- KeyMap.lookup "errored" object ->
            Errored <$> Aeson.parseJSON value
    value -> fail ("invalid persisted subagent status: " <> show value)

-- | Generated ids look like @agent-<hex>-<n>@. Reject path separators and
-- traversal so resume paths cannot escape @agents/@.
isValidSubagentStoreId :: SubagentId -> Bool
isValidSubagentStoreId (SubagentId text) =
    let name = Text.unpack text
    in not (null name)
        && all isSafeNameChar name
        && name /= "."
        && name /= ".."
        && "agent-" `Text.isPrefixOf` text
  where
    isSafeNameChar c = isAlphaNum c || c == '-' || c == '_'

subagentStoreDir :: OsPath -> SubagentId -> Either Text OsPath
subagentStoreDir sessionDir agentId
    | not (isValidSubagentStoreId agentId) =
        Left ("invalid subagent id for store path: " <> agentId.unSubagentId)
    | otherwise =
        Right
            ( sessionDir
                </> unsafeEncodeUtf "agents"
                </> unsafeEncodeUtf (Text.unpack agentId.unSubagentId)
            )

forkSubagentTranscript :: Maybe Text -> [ResponseItem] -> [ResponseItem]
forkSubagentTranscript forkTurns items =
    let completeItems = trimDanglingToolSuffix items
        normalized = Text.toLower . Text.strip <$> forkTurns
    in case normalized of
        Just "none" -> []
        Just turns
            | Just count <- readExactInt turns
            , count > 0 -> takeRecentTurns count completeItems
        _ -> completeItems

takeRecentTurns :: Int -> [ResponseItem] -> [ResponseItem]
takeRecentTurns count items =
    case drop (max 0 (length starts - count)) starts of
        start : _ -> drop start items
        [] -> items
  where
    starts =
        [ index
        | (index, MessageItem message) <- zip [0 :: Int ..] items
        , message.role == RoleUser
        ]

saveSubagentState
    :: OsPath
    -> SubagentId
    -> SubagentStateSnapshot
    -> IO (Either Text ())
saveSubagentState sessionDir agentId snapshot =
    case subagentStoreDir sessionDir agentId of
        Left err -> pure (Left err)
        Right dir -> do
            let metaPath = dir </> unsafeEncodeUtf "meta.json"
                transcriptPath = dir </> unsafeEncodeUtf "transcript.json"
            createDirectoryIfMissing True dir
            _ <- tryAny (setFileMode (unsafeToFilePath dir) 0o700)
            let fields = SubagentDiskFields
                    { diskPreviousResponseId =
                        snapshot.snapshotPreviousResponseId
                    , diskStatus = Just snapshot.snapshotStatus
                    , diskAgentType = snapshot.snapshotAgentType
                    , diskAgentModel = snapshot.snapshotAgentModel
                    , diskReasoningEffort = snapshot.snapshotReasoningEffort
                    , diskCwd = snapshot.snapshotCwd
                    , diskTaskPath =
                        taskPathText . (.identityTaskPath)
                            <$> snapshot.snapshotIdentity
                    , diskParentId =
                        snapshot.snapshotIdentity >>= (.identityParent)
                    , diskDepth =
                        (.identityDepth) <$> snapshot.snapshotIdentity
                    }
                meta =
                    CurrentSubagentDiskMeta fields snapshot.snapshotTarget
            writeLazyFileAtomically metaPath 0o600 (Aeson.encode meta)
            writeLazyFileAtomically
                transcriptPath
                0o600
                (LBS.fromStrict
                    (JsonEncoder.encode
                        (JsonEncoder.list responseItemEncoder)
                        snapshot.snapshotItems))
            pure (Right ())

loadSubagentState
    :: OsPath
    -> SubagentId
    -> IO (Either Text (Maybe ([ResponseItem], SubagentDiskMeta)))
loadSubagentState sessionDir agentId =
    case subagentStoreDir sessionDir agentId of
        Left err -> pure (Left err)
        Right dir -> do
            let metaPath = dir </> unsafeEncodeUtf "meta.json"
                transcriptPath = dir </> unsafeEncodeUtf "transcript.json"
            hasMeta <- doesFileExist metaPath
            hasTranscript <- doesFileExist transcriptPath
            if not (hasMeta || hasTranscript)
                then pure (Right Nothing)
                else do
                    metaResult <- if hasMeta
                        then decodeFile metaPath
                        else pure (Right (LegacySubagentDiskMeta
                            (SubagentDiskFields
                                Nothing Nothing Nothing Nothing Nothing
                                Nothing Nothing Nothing Nothing)
                            (LegacySubagentTargetFields
                                Nothing Nothing Nothing Nothing)))
                    itemsResult <- if hasTranscript
                        then decodeTranscriptFile transcriptPath
                        else pure (Right [])
                    pure $ case (metaResult, itemsResult) of
                        (Left err, _) -> Left err
                        (_, Left err) -> Left err
                        (Right meta, Right items) ->
                            Right (Just (items, meta))
  where
    decodeFile path = do
        raw <- retryOnFileBusy (LBS.readFile (unsafeToFilePath path))
        case Aeson.eitherDecode raw of
            Left err ->
                pure $ Left $
                    "failed to decode "
                        <> toText path
                        <> ": "
                        <> Text.pack err
            Right value -> pure (Right value)

    decodeTranscriptFile path = do
        raw <- retryOnFileBusy (LBS.readFile (unsafeToFilePath path))
        case JsonDecoder.decode
                (JsonDecoder.list responseItemDecoder)
                (LBS.toStrict raw) of
            Left err ->
                pure $ Left $
                    "failed to decode "
                        <> toText path
                        <> ": "
                        <> JsonDecoder.renderDecodeError err
            Right value -> pure (Right value)
