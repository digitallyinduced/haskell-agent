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
    , saveSubagentState
    , loadSubagentState
    ) where

import Agent.FileRetry (retryOnFileBusy, writeLazyFileAtomically)
import Agent.OsPath (OsPath, fromFilePath, toFilePath, toText)
import Agent.OpenAI.Responses.Types (ResponseItem)
import Agent.Subagents (SubagentId(..), SubagentIdentity(..))
import Agent.Subagents.TaskPath (taskPathText)
import Control.Exception.Safe (tryAny)
import Data.Aeson (FromJSON(..), ToJSON(..), object, withObject, (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory.OsPath
    ( createDirectoryIfMissing
    , doesFileExist
    )
import System.OsPath ((</>))
import System.Posix.Files (setFileMode)

data SubagentDiskMeta = SubagentDiskMeta
    { diskPreviousResponseId :: !(Maybe Text)
    , diskAgentType :: !(Maybe Text)
    , diskAgentModel :: !(Maybe Text)
    , diskCwd :: !(Maybe OsPath)
    , diskTaskPath :: !(Maybe Text)
    , diskParentId :: !(Maybe SubagentId)
    , diskDepth :: !(Maybe Int)
    } deriving (Eq, Show)

instance ToJSON SubagentDiskMeta where
    toJSON meta = object
        [ "previousResponseId" .= meta.diskPreviousResponseId
        , "agentType" .= meta.diskAgentType
        , "agentModel" .= meta.diskAgentModel
        , "cwd" .= fmap toFilePath meta.diskCwd
        , "taskPath" .= meta.diskTaskPath
        , "parentId" .= meta.diskParentId
        , "depth" .= meta.diskDepth
        ]

instance FromJSON SubagentDiskMeta where
    parseJSON = withObject "SubagentDiskMeta" \o ->
        SubagentDiskMeta
            <$> o .:? "previousResponseId"
            <*> o .:? "agentType"
            <*> o .:? "agentModel"
            <*> (fmap fromFilePath <$> o .:? "cwd")
            <*> o .:? "taskPath"
            <*> o .:? "parentId"
            <*> o .:? "depth"

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
                </> fromFilePath "agents"
                </> fromFilePath (Text.unpack agentId.unSubagentId)
            )

saveSubagentState
    :: OsPath
    -> SubagentId
    -> [ResponseItem]
    -> Maybe Text
    -> Maybe Text
    -> Maybe Text
    -> Maybe OsPath
    -> Maybe SubagentIdentity
    -> IO (Either Text ())
saveSubagentState sessionDir agentId items previous agentType agentModel cwd identity =
    case subagentStoreDir sessionDir agentId of
        Left err -> pure (Left err)
        Right dir -> do
            let metaPath = dir </> fromFilePath "meta.json"
                transcriptPath = dir </> fromFilePath "transcript.json"
            createDirectoryIfMissing True dir
            _ <- tryAny (setFileMode (toFilePath dir) 0o700)
            writeLazyFileAtomically metaPath 0o600 $ Aeson.encode SubagentDiskMeta
                { diskPreviousResponseId = previous
                , diskAgentType = agentType
                , diskAgentModel = agentModel
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
            let metaPath = dir </> fromFilePath "meta.json"
                transcriptPath = dir </> fromFilePath "transcript.json"
            hasMeta <- doesFileExist metaPath
            hasTranscript <- doesFileExist transcriptPath
            if not (hasMeta || hasTranscript)
                then pure (Right Nothing)
                else do
                    metaResult <- if hasMeta
                        then decodeFile metaPath
                        else pure (Right (SubagentDiskMeta
                            Nothing Nothing Nothing Nothing Nothing Nothing Nothing))
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
        raw <- retryOnFileBusy (LBS.readFile (toFilePath path))
        case Aeson.eitherDecode raw of
            Left err ->
                pure $ Left $
                    "failed to decode "
                        <> toText path
                        <> ": "
                        <> Text.pack err
            Right value -> pure (Right value)
