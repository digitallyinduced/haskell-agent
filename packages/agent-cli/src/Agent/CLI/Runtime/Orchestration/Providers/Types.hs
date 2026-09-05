module Agent.CLI.Runtime.Orchestration.Providers.Types
    ( ProviderConfig(..)
    , OpenAiConfig(..)
    , OpenAiAccounts(..)
    , OpenRouterConfig(..)
    , ClaudeConfig(..)
    , ProviderHost(..)
    , ProviderCompaction(..)
    , ProviderRuntime(..)
    , ProviderAccountSelection(..)
    , ProviderSubagents(..)
    ) where

import Agent.CLI.Compaction (CompactOutcome, CompactionInstall, OccupancySnapshot)
import Agent.CLI.Session.History (LiveConversation)
import Agent.CLI.Session.Runtime.Types (SessionBackend)
import Agent.Claude (ClaudeCodeAuth)
import Agent.Claude.Control (ClaudeCodeHostHandlers)
import Agent.Connectivity.NetworkPath (NetworkRecovery)
import Agent.Error (ApiError)
import Agent.Loop (Backend, TokenUsage, TurnInput)
import Agent.OpenAI.Auth (Pool)
import Agent.OpenRouter.Options (ClientOptions)
import Agent.Provider (Credential, TokenProvider)
import Agent.Responses.Types (ResponseCreateParams)
import Agent.Tools.TaskPlan (TaskPlanEnv)
import Data.IORef (IORef)
import Data.Text (Text)
import System.OsPath (OsPath)
import qualified Agent.Responses.GenericClient as GenericResponses

-- | Only the selected provider's configuration crosses the provider boundary.
data ProviderConfig
    = OpenAiProviderConfig OpenAiConfig
    | XaiProviderConfig TokenProvider Bool
    | GeminiProviderConfig TokenProvider
    | OpenRouterProviderConfig OpenRouterConfig
    | ClaudeProviderConfig ClaudeConfig

data OpenAiConfig = OpenAiConfig
    { tokenProvider :: TokenProvider
    , showRawReasoning :: Bool
    , transportModel :: Text -> Text
    , accounts :: OpenAiAccounts
    }

-- | Account switching owns connections; the host owns account presentation
-- and preferences. Keep those mutations behind operations.
data OpenAiAccounts = OpenAiAccounts
    { selectablePool :: Maybe Pool
    , readActiveAccountId :: IO Text
    , resolveAccountLabel :: Credential -> IO Text
    , installAccount :: Credential -> Text -> IO ()
    , preferAccount :: Text -> IO ()
    }

data OpenRouterConfig = OpenRouterConfig
    { tokenProvider :: TokenProvider
    , clientOptions :: ClientOptions
    , genericOptions :: Maybe GenericResponses.GenericClientOptions
    , model :: Text
    , transportModel :: Text -> Text
    }

data ClaudeConfig = ClaudeConfig
    { withAuth :: forall a. (ClaudeCodeAuth -> IO a) -> IO a
    , cwd :: OsPath
    , initialPrevious :: Maybe Text
    , transportModel :: Text -> Text
    , hostHandlers :: ClaudeCodeHostHandlers
    , onConnected :: Text -> IO ()
    }

-- | Shared services used to construct a provider backend. Session startup,
-- persistence, UI, and subagent registration belong to the caller.
data ProviderHost = ProviderHost
    { compaction :: ProviderCompaction
    , networkRecovery :: Maybe NetworkRecovery
    }

-- | The live compaction state is shared with the session. Providers supply
-- transport-specific summarization and context limits; installation stays
-- behind the shared compaction helpers and the session's automatic hook.
data ProviderCompaction = ProviderCompaction
    { paramsRef :: IORef ResponseCreateParams
    , contextTokensRef :: IORef (Maybe OccupancySnapshot)
    , contextWindowForParams :: (Text -> Text) -> Int -> ResponseCreateParams -> Int
    , currentModelContextWindow :: (Text -> Text) -> IO (Maybe Int)
    , conversationRef :: IORef LiveConversation
    , installAutomaticCompact :: CompactOutcome -> [TurnInput] -> IO CompactionInstall
    , taskPlan :: Maybe TaskPlanEnv
    , recordCompactionUsage :: TokenUsage -> IO ()
    , compactThreshold :: Maybe Int
    }

-- | Valid only inside the continuation passed to 'withProviderRuntime'.
-- Its backend and account operations must not be used after that scope closes.
data ProviderRuntime = ProviderRuntime
    { sessionBackend :: SessionBackend
    , currentContextWindow :: IO (Maybe Int)
    , compactRunner :: Maybe Text -> IO (Either Text CompactOutcome)
    , accountSelection :: ProviderAccountSelection
    , subagents :: ProviderSubagents
    }

data ProviderAccountSelection
    = HttpAccountSelection
    | OpenAiAccountSelection (Maybe Pool) (Maybe (Text -> IO (Either ApiError Text)))
    | NoAccountSelection

-- | Describe child transport support without accessing the session registry.
data ProviderSubagents
    = HttpSubagents (ResponseCreateParams -> Backend)
    | XaiSubagents
        (ResponseCreateParams -> Int)
        (ResponseCreateParams -> Int)
        (ResponseCreateParams -> Backend)
    | CodexSubagents Bool
    | NoProviderSubagents
