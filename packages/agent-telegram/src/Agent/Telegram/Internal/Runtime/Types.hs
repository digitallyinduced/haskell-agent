module Agent.Telegram.Internal.Runtime.Types (TelegramRuntime(..)) where


import Agent.CLI.AgentSessions (SessionProcessManager)
import Agent.CLI.Options (ApprovalPolicy)
import Agent.CLI.Models (ModelTarget)
import Agent.Telegram.Types
import Agent.Store.Postgres.Connection (StorePool)
import Control.Concurrent.Chan (Chan)
import Control.Concurrent.MVar (MVar)
import Data.Set (Set)
import Data.Text (Text)
import System.OsPath (OsPath)

data TelegramRuntime = TelegramRuntime
    { runtimeClient :: !TelegramClient
    , runtimeBot :: !TelegramUser
    , runtimeRespondToAllGroupMessages :: !Bool
    , runtimeWorkerCount :: !Int
    , runtimeGatewayDirectory :: !OsPath
    , runtimePool :: !StorePool
    , runtimeSessionsRoot :: !OsPath
    , runtimeStatePath :: !OsPath
    , runtimeStateVar :: !(MVar TelegramState)
    , runtimeWorkQueue :: !(Chan TelegramChatKey)
    , runtimeScheduled :: !(MVar (Set TelegramChatKey))
    , runtimeProcessManager :: !SessionProcessManager
    , runtimeTarget :: !ModelTarget
    , runtimeCwd :: !OsPath
    , runtimeEffort :: !Text
    , runtimePolicy :: !ApprovalPolicy
    }
