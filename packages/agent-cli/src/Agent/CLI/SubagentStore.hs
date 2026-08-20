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

import Agent.OpenAI.Responses.Types (ResponseItem)
import Agent.Subagents (SubagentId(..))
import Control.Exception.Safe (tryAny)
import Data.Aeson (FromJSON(..), ToJSON(..), object, withObject, (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory
    ( createDirectoryIfMissing
    , doesFileExist
    , renameFile
    )
import System.FilePath ((</>))
import System.Posix.Files (setFileMode)

data SubagentDiskMeta = SubagentDiskMeta
    { diskPreviousResponseId :: !(Maybe Text)
    , diskAgentType :: !(Maybe Text)
    } deriving (Eq, Show)

instance ToJSON SubagentDiskMeta where
    toJSON meta = object
        [ "previousResponseId" .= meta.diskPreviousResponseId
        , "agentType" .= meta.diskAgentType
        ]

instance FromJSON SubagentDiskMeta where
    parseJSON = withObject "SubagentDiskMeta" \o ->
        SubagentDiskMeta
            <$> o .:? "previousResponseId"
            <*> o .:? "agentType"

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

subagentStoreDir :: FilePath -> SubagentId -> Either Text FilePath
subagentStoreDir sessionDir agentId
    | not (isValidSubagentStoreId agentId) =
        Left ("invalid subagent id for store path: " <> agentId.unSubagentId)
    | otherwise =
        Right (sessionDir </> "agents" </> Text.unpack agentId.unSubagentId)

saveSubagentState
    :: FilePath
    -> SubagentId
    -> [ResponseItem]
    -> Maybe Text
    -> Maybe Text
    -> IO (Either Text ())
saveSubagentState sessionDir agentId items previous agentType =
    case subagentStoreDir sessionDir agentId of
        Left err -> pure (Left err)
        Right dir -> do
            let metaPath = dir </> "meta.json"
                transcriptPath = dir </> "transcript.json"
                metaTmp = metaPath <> ".tmp"
                transcriptTmp = transcriptPath <> ".tmp"
            createDirectoryIfMissing True dir
            _ <- tryAny (setFileMode dir 0o700)
            LBS.writeFile metaTmp $ Aeson.encode SubagentDiskMeta
                { diskPreviousResponseId = previous
                , diskAgentType = agentType
                }
            _ <- tryAny (setFileMode metaTmp 0o600)
            LBS.writeFile transcriptTmp (Aeson.encode items)
            _ <- tryAny (setFileMode transcriptTmp 0o600)
            renameFile metaTmp metaPath
            renameFile transcriptTmp transcriptPath
            pure (Right ())

loadSubagentState
    :: FilePath
    -> SubagentId
    -> IO (Either Text (Maybe ([ResponseItem], SubagentDiskMeta)))
loadSubagentState sessionDir agentId =
    case subagentStoreDir sessionDir agentId of
        Left err -> pure (Left err)
        Right dir -> do
            let metaPath = dir </> "meta.json"
                transcriptPath = dir </> "transcript.json"
            hasMeta <- doesFileExist metaPath
            hasTranscript <- doesFileExist transcriptPath
            if not (hasMeta || hasTranscript)
                then pure (Right Nothing)
                else do
                    metaResult <- if hasMeta
                        then decodeFile metaPath
                        else pure (Right (SubagentDiskMeta Nothing Nothing))
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
        raw <- LBS.readFile path
        case Aeson.eitherDecode raw of
            Left err ->
                pure $ Left $
                    "failed to decode "
                        <> Text.pack path
                        <> ": "
                        <> Text.pack err
            Right value -> pure (Right value)
