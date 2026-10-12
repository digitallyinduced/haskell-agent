-- | Public types shared by subagent registries and provider runtimes.
module Agent.Subagents.Types
    ( SubagentId(..)
    , subagentIdDecoder
    , SubagentIdentity(..)
    , RootTurnId(..)
    , SubagentStatus(..)
    , SuspensionPersistence(..)
    , SubagentConfig(..)
    , SubagentSpawnEnv(..)
    , RunSubagent
    , defaultSubagentConfig
    , defaultMaxConcurrent
    , defaultMaxDepth
    , defaultMaxSpawnedPerTurn
    , defaultWaitTimeoutMs
    , minWaitTimeoutMs
    , maxWaitTimeoutMs
    ) where

import Agent.Cancel (CancelFlag)
import qualified Agent.Json.Decode as Json
import Agent.InterAgentMessage (InterAgentMessage)
import Agent.Loop (LoopError, LoopEvent, LoopResult)
import System.OsPath (OsPath)
import Agent.Subagents.TaskPath (TaskPath)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Word (Word64)

newtype SubagentId = SubagentId { unSubagentId :: Text }
    deriving (Eq, Ord, Show)

newtype RootTurnId = RootTurnId { unRootTurnId :: Word64 }
    deriving (Eq, Ord, Show)

instance Aeson.ToJSON SubagentId where
    toJSON (SubagentId text) = Aeson.String text

subagentIdDecoder :: Json.Decoder SubagentId
subagentIdDecoder = SubagentId <$> Json.text

data SubagentIdentity = SubagentIdentity
    { identityParent :: Maybe SubagentId
    , identityDepth :: Int
    , identityTaskPath :: TaskPath
    }
    deriving (Eq, Show)

-- | Storage operations run after the runner checkpoints its transcript and
-- before suspended status is published, or before resumption is acknowledged.
-- The host must durably retain the agent identity and transcript, fence writes
-- against cancellation, and atomically record a runnable handoff on resume.
-- These callbacks must not call back into the registry.
data SuspensionPersistence = SuspensionPersistence
    { persistSuspension :: SubagentId -> UTCTime -> Text -> IO (Either Text ())
    , persistSuspensionResume :: SubagentId -> InterAgentMessage -> IO (Either Text ())
    , persistSuspensionCancellation :: SubagentId -> IO (Either Text ())
    }

data SubagentStatus
    = Pending
    | Running
    | Completed !(Maybe Text)
    | Errored !Text
    | Interrupted
    | Suspended !UTCTime !Text
    | Closed
    | NotFound
    deriving (Eq, Show)

data SubagentConfig = SubagentConfig
    { maxConcurrent :: !Int
      -- | 'Nothing' means unlimited nesting depth.
    , maxDepth :: !(Maybe Int)
      -- | Cumulative new-agent admissions owned by one root turn.
      -- 'Nothing' disables the budget.
    , maxSpawnedPerTurn :: !(Maybe Int)
    } deriving (Eq, Show)

defaultMaxConcurrent :: Int
defaultMaxConcurrent = 8

defaultMaxDepth :: Int
defaultMaxDepth = 1

defaultMaxSpawnedPerTurn :: Int
defaultMaxSpawnedPerTurn = 16

defaultSubagentConfig :: SubagentConfig
defaultSubagentConfig = SubagentConfig
    { maxConcurrent = defaultMaxConcurrent
    , maxDepth = Just defaultMaxDepth
    , maxSpawnedPerTurn = Just defaultMaxSpawnedPerTurn
    }

minWaitTimeoutMs :: Int
minWaitTimeoutMs = 10000

maxWaitTimeoutMs :: Int
maxWaitTimeoutMs = 3600 * 1000

defaultWaitTimeoutMs :: Int
defaultWaitTimeoutMs = 30000

data SubagentSpawnEnv = SubagentSpawnEnv
    { subId :: !SubagentId
    , subDepth :: !Int
    , subParentId :: !(Maybe SubagentId)
    , subCwd :: !OsPath
    , subCancel :: !CancelFlag
    , subRootTurnId :: !(Maybe RootTurnId)
    }

-- | CLI/provider callback that runs one child agent loop for a prompt.
-- The optional text is the child's previous response id from an earlier turn.
type RunSubagent =
    SubagentSpawnEnv
    -> Maybe Text
    -> InterAgentMessage
    -> (LoopEvent -> IO ())
    -> IO (Either LoopError LoopResult)
