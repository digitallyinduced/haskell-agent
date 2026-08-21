-- | Public types shared by subagent registries and provider runtimes.
module Agent.Subagents.Types
    ( SubagentId(..)
    , RootTurnId(..)
    , SubagentStatus(..)
    , SubagentConfig(..)
    , SubagentSpawnEnv(..)
    , RunSubagent
    , defaultSubagentConfig
    , defaultMaxConcurrent
    , defaultWaitTimeoutMs
    , minWaitTimeoutMs
    , maxWaitTimeoutMs
    ) where

import Agent.Cancel (CancelFlag)
import Agent.InterAgentMessage (InterAgentMessage)
import Agent.Loop (LoopError, LoopEvent, LoopResult)
import Agent.OsPath (OsPath)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Data.Word (Word64)

newtype SubagentId = SubagentId { unSubagentId :: Text }
    deriving (Eq, Ord, Show)

newtype RootTurnId = RootTurnId { unRootTurnId :: Word64 }
    deriving (Eq, Ord, Show)

instance Aeson.ToJSON SubagentId where
    toJSON (SubagentId text) = Aeson.String text

instance Aeson.FromJSON SubagentId where
    parseJSON = Aeson.withText "SubagentId" (pure . SubagentId)

data SubagentStatus
    = Pending
    | Running
    | Completed !(Maybe Text)
    | Errored !Text
    | Interrupted
    | Closed
    | NotFound
    deriving (Eq, Show)

data SubagentConfig = SubagentConfig
    { maxConcurrent :: !Int
      -- | 'Nothing' means unlimited nesting depth.
    , maxDepth :: !(Maybe Int)
    } deriving (Eq, Show)

defaultMaxConcurrent :: Int
defaultMaxConcurrent = 6

defaultSubagentConfig :: SubagentConfig
defaultSubagentConfig = SubagentConfig
    { maxConcurrent = defaultMaxConcurrent
    , maxDepth = Nothing
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
