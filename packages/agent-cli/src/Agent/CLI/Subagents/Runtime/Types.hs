-- | Shared state for CLI-owned child-agent runtimes.
module Agent.CLI.Subagents.Runtime.Types
    ( PreparedChild(..)
    , SubagentResidency(..)
    , SubagentRuntime(..)
    , SubagentSession(..)
    , SubagentStoreRoot
    ) where

import Agent.CLI.Compaction (OccupancySnapshot)
import Agent.CLI.Options (ApprovalPolicy, CliOptions)
import Agent.CLI.Session (LegacySubagentTarget)
import Agent.GrokBuild.Dialect.Task (GrokSubagentSpecs)
import Agent.Loop (BackendSnapshot)
import Agent.Provider (Provider, TokenProvider)
import Agent.Responses.Types (ResponseCreateParams)
import Agent.Subagents (SubagentId, SubagentRegistry)
import Agent.Tools.MultiAgents (MultiAgentContext, SubagentWorktree)
import Agent.Tools.PlanMode (PlanModeHooks)
import Agent.Tools.Types (AppTool, ToolEnv)
import Agent.Dialect (DialectId)
import Control.Concurrent.MVar (MVar)
import Data.IORef (IORef)
import Data.Map.Strict (Map)
import Data.Text (Text)
import System.OsPath (OsPath)

-- | Lifecycle state for an in-memory child session.
--
-- A pinned session is necessarily resident. Keeping these states in one
-- lock prevents the impossible combination of an evicted, pinned session.
data SubagentResidency
    = SessionEvicted
    | SessionResident
    | SessionPinned
    deriving (Eq, Show)

data SubagentSession = SubagentSession
    { subSessionTranscript :: !(IORef BackendSnapshot)
    , subSessionContextTokens :: !(IORef (Maybe OccupancySnapshot))
    , subSessionProvider :: !Provider
    , subSessionConnection :: !Text
    , subSessionEffectiveModel :: !Text
    , subSessionDialect :: !DialectId
    , subSessionResidency :: !(MVar SubagentResidency)
    }

-- | Optional on-disk root for child transcripts (@sessionDir/agents/<id>@).
type SubagentStoreRoot = IORef (Maybe OsPath)

-- | Provider-neutral dependencies shared by all child-agent backends.
data SubagentRuntime = SubagentRuntime
    { subagentOptions :: !CliOptions
    , subagentGhciEnabled :: !(IORef Bool)
    , subagentBashEnabled :: !(IORef Bool)
    , subagentPolicy :: !(IORef ApprovalPolicy)
    , subagentPlanHooks :: !PlanModeHooks
    , subagentSkillRoots :: !(IORef [OsPath])
    , subagentAllowedRoots :: !(IORef [OsPath])
    , subagentRootAccessRequest :: !(IORef (Maybe (OsPath -> IO Bool)))
    , subagentSessionTmp :: !(IORef (Maybe OsPath))
    , subagentMcpTools :: ![AppTool]
    , subagentParams :: !(IORef ResponseCreateParams)
    , subagentRegistry :: !SubagentRegistry
    , subagentSessions :: !(IORef (Map SubagentId SubagentSession))
    , subagentStoreRoot :: !SubagentStoreRoot
    , subagentTypes :: !GrokSubagentSpecs
    , subagentLegacyTarget :: !(Maybe LegacySubagentTarget)
    , subagentConnection :: !Text
    , subagentMapModel :: !(Text -> Text)
    , subagentCreateWorktree
        :: !(Maybe (OsPath -> IO (Either Text SubagentWorktree)))
    , subagentSpawnModelGuidance :: !(Maybe Text)
    , subagentAllowedChildModels :: !(Maybe [Text])
    , subagentChildModelAllowed :: !(Maybe (Text -> IO Bool))
    , subagentOpenAiChild :: !(Maybe TokenProvider)
    }

data PreparedChild = PreparedChild
    { preparedParentParams :: !ResponseCreateParams
    , preparedSession :: !SubagentSession
    , preparedToolEnv :: !ToolEnv
    , preparedMultiContext :: !MultiAgentContext
    }
