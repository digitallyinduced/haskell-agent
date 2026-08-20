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
    , subagentStoreDir
    , saveSubagentState
    , loadSubagentState
    ) where

import Agent.OpenAI.Responses.Types (ResponseItem)
import Agent.Subagents (SubagentId(..))
import Control.Exception (try)
import Data.Aeson (FromJSON(..), ToJSON(..), object, withObject, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory
    ( createDirectoryIfMissing
    , doesFileExist
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

subagentStoreDir :: FilePath -> SubagentId -> FilePath
subagentStoreDir sessionDir agentId =
    sessionDir </> "agents" </> Text.unpack agentId.unSubagentId

saveSubagentState
    :: FilePath
    -> SubagentId
    -> [ResponseItem]
    -> Maybe Text
    -> Maybe Text
    -> IO ()
saveSubagentState sessionDir agentId items previous agentType = do
    let dir = subagentStoreDir sessionDir agentId
        metaPath = dir </> "meta.json"
        transcriptPath = dir </> "transcript.json"
    createDirectoryIfMissing True dir
    _ <- try @IOError (setFileMode dir 0o700)
    LBS.writeFile metaPath $ Aeson.encode SubagentDiskMeta
        { diskPreviousResponseId = previous
        , diskAgentType = agentType
        }
    _ <- try @IOError (setFileMode metaPath 0o600)
    LBS.writeFile transcriptPath (Aeson.encode items)
    _ <- try @IOError (setFileMode transcriptPath 0o600)
    pure ()

loadSubagentState
    :: FilePath
    -> SubagentId
    -> IO (Maybe ([ResponseItem], SubagentDiskMeta))
loadSubagentState sessionDir agentId = do
    let dir = subagentStoreDir sessionDir agentId
        metaPath = dir </> "meta.json"
        transcriptPath = dir </> "transcript.json"
    hasMeta <- doesFileExist metaPath
    hasTranscript <- doesFileExist transcriptPath
    if not (hasMeta || hasTranscript)
        then pure Nothing
        else do
            meta <- if hasMeta
                then do
                    raw <- LBS.readFile metaPath
                    pure $ fromMaybe emptyMeta (Aeson.decode raw)
                else pure emptyMeta
            items <- if hasTranscript
                then do
                    raw <- LBS.readFile transcriptPath
                    pure $ fromMaybe [] (Aeson.decode raw)
                else pure []
            pure $ Just (items, meta)
  where
    emptyMeta = SubagentDiskMeta Nothing Nothing
