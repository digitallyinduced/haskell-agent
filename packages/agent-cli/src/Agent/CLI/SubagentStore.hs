-- | Persist per-subagent transcripts under a parent session directory.
--
-- Layout:
--
-- @
--   <sessionDir>/agents/<agentId>/meta.json
--   <sessionDir>/agents/<agentId>/transcript.json
-- @
module Agent.CLI.SubagentStore
    ( SubagentDiskMeta(..)
    , isValidSubagentStoreId
    , subagentStoreDir
    , forkSubagentTranscript
    , saveSubagentState
    , loadSubagentState
    ) where

import Agent.CLI.Btw (trimDanglingToolSuffix)
import Agent.FileRetry (retryOnFileBusy, writeLazyFileAtomically)
import Agent.OsPath (toText)
import Agent.Responses.Types
import Agent.Subagents (SubagentId(..), SubagentIdentity(..), SubagentStatus(..))
import Agent.Subagents.TaskPath (taskPathText)
import Control.Exception.Safe (impureThrow, tryAny)
import Data.Aeson (FromJSON(..), ToJSON(..), object, withObject, (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Read (readMaybe)
import System.Directory.OsPath
    ( createDirectoryIfMissing
    , doesFileExist
    )
import System.OsPath (OsPath, decodeUtf, unsafeEncodeUtf, (</>))
import System.Posix.Files (setFileMode)

data SubagentDiskMeta = SubagentDiskMeta
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

instance ToJSON SubagentDiskMeta where
    toJSON meta = object
        [ "previousResponseId" .= meta.diskPreviousResponseId
        , "status" .= fmap encodeDiskStatus meta.diskStatus
        , "agentType" .= meta.diskAgentType
        , "agentModel" .= meta.diskAgentModel
        , "reasoningEffort" .= meta.diskReasoningEffort
        , "cwd" .= fmap decodeUtfPath meta.diskCwd
        , "taskPath" .= meta.diskTaskPath
        , "parentId" .= meta.diskParentId
        , "depth" .= meta.diskDepth
        ]

instance FromJSON SubagentDiskMeta where
    parseJSON = withObject "SubagentDiskMeta" \o -> do
        statusValue <- o .:? "status"
        diskStatus <- traverse decodeDiskStatus statusValue
        SubagentDiskMeta
            <$> o .:? "previousResponseId"
            <*> pure diskStatus
            <*> o .:? "agentType"
            <*> o .:? "agentModel"
            <*> o .:? "reasoningEffort"
            <*> (fmap unsafeEncodeUtf <$> o .:? "cwd")
            <*> o .:? "taskPath"
            <*> o .:? "parentId"
            <*> o .:? "depth"

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
            | Just count <- readMaybe (Text.unpack turns)
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
    -> [ResponseItem]
    -> Maybe Text
    -> SubagentStatus
    -> Maybe Text
    -> Maybe Text
    -> Maybe Text
    -> Maybe OsPath
    -> Maybe SubagentIdentity
    -> IO (Either Text ())
saveSubagentState
        sessionDir agentId items previous status agentType agentModel
        reasoningEffort cwd identity =
    case subagentStoreDir sessionDir agentId of
        Left err -> pure (Left err)
        Right dir -> do
            let metaPath = dir </> unsafeEncodeUtf "meta.json"
                transcriptPath = dir </> unsafeEncodeUtf "transcript.json"
            createDirectoryIfMissing True dir
            _ <- tryAny (setFileMode (decodeUtfPath dir) 0o700)
            writeLazyFileAtomically metaPath 0o600 $ Aeson.encode SubagentDiskMeta
                { diskPreviousResponseId = previous
                , diskStatus = Just status
                , diskAgentType = agentType
                , diskAgentModel = agentModel
                , diskReasoningEffort = reasoningEffort
                , diskCwd = cwd
                , diskTaskPath = taskPathText . (.identityTaskPath) <$> identity
                , diskParentId = identity >>= (.identityParent)
                , diskDepth = (.identityDepth) <$> identity
                }
            writeLazyFileAtomically transcriptPath 0o600 (Aeson.encode items)
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
                        else pure (Right (SubagentDiskMeta
                            Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing))
                    itemsResult <- if hasTranscript
                        then decodeFile transcriptPath
                        else pure (Right [])
                    pure $ case (metaResult, itemsResult) of
                        (Left err, _) -> Left err
                        (_, Left err) -> Left err
                        (Right meta, Right items) ->
                            Right (Just (items, meta))
  where
    decodeFile path = do
        raw <- retryOnFileBusy (LBS.readFile (decodeUtfPath path))
        case Aeson.eitherDecode raw of
            Left err ->
                pure $ Left $
                    "failed to decode "
                        <> toText path
                        <> ": "
                        <> Text.pack err
            Right value -> pure (Right value)

decodeUtfPath :: OsPath -> FilePath
decodeUtfPath = either impureThrow id . decodeUtf
