module Agent.CLI.Subagents.Types
    ( SubagentRuntime(..)
    , SubagentSession(..)
    , SubagentStoreRoot
    ) where

import Agent.CLI.Options (ApprovalPolicy, CliOptions)
import Agent.CLI.Session (LegacySubagentTarget)
import Agent.Dialect (DialectId)
import Agent.GrokBuild.Dialect.Task (GrokSubagentSpecs)
import Agent.Provider (Provider)
import Agent.Responses.Types (ResponseCreateParams, ResponseItem)
import Agent.Subagents (SubagentId, SubagentRegistry)
import Agent.Tools.PlanMode (PlanModeHooks)
import Agent.Tools.Types (AppTool)
import Control.Concurrent.MVar (MVar)
import Data.IORef (IORef)
import Data.Map.Strict (Map)
import Data.Text (Text)
import System.OsPath (OsPath)

data SubagentSession = SubagentSession
    { subSessionTranscript :: !(IORef [ResponseItem])
    , subSessionContextTokens :: !(IORef (Maybe (Int, Int)))
    , subSessionProvider :: !Provider
    , subSessionConnection :: !Text
    , subSessionEffectiveModel :: !Text
    , subSessionDialect :: !DialectId
    , subSessionPinned :: !(IORef Bool)
    , subSessionHydrated :: !(MVar Bool)
    }

-- | Optional on-disk root for child transcripts (@sessionDir/agents/<id>@).
type SubagentStoreRoot = IORef (Maybe OsPath)

-- | Provider-neutral dependencies shared by all child-agent backends.
data SubagentRuntime = SubagentRuntime
    { subagentOptions :: !CliOptions
    , subagentGhciEnabled :: !(IORef Bool)
    , subagentBashEnabled :: !(IORef Bool)
    , subagentPolicy :: !ApprovalPolicy
    , subagentPlanHooks :: !PlanModeHooks
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
    , subagentSpawnModelGuidance :: !(Maybe Text)
    }
