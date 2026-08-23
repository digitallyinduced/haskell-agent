-- | Translate the harness's subscription-specific configuration into the
-- provider-neutral options exposed by @claude-agent-sdk-haskell@.
module Agent.Claude.Options
    ( ClaudeCodePermission(..)
    , ClaudeCodeOptions(..)
    , ClaudeCodeToolMode(..)
    , defaultClaudeCodeOptions
    , toClaudeAgentOptions
    ) where

import Agent.Claude.Internal.Environment
    ( sanitizedClaudeEnvironment )
import Claude.Agent.SDK.Types
    ( ClaudeAgentOptions(..)
    , PermissionMode(..)
    , defaultClaudeAgentOptions
    )
import qualified Data.Aeson as Aeson
import Data.Text (Text)

-- | Claude's non-interactive permission policies. Neither policy can pause
-- the hidden subprocess to ask the terminal user for confirmation.
data ClaudeCodePermission
    = ClaudeCodeDontAsk
    | ClaudeCodeBypass
    deriving (Eq, Show)

data ClaudeCodeToolMode
    = ClaudeCodeDefaultTools
    | ClaudeCodeNoTools
    deriving (Eq, Show)

-- | Subscription-provider options that are stable for a backend's lifetime.
data ClaudeCodeOptions = ClaudeCodeOptions
    { executable :: !FilePath
    , cwd :: !FilePath
    , permission :: !ClaudeCodePermission
    , safeMode :: !Bool
    , promptWriteTimeoutMicros :: !Int
    } deriving (Eq, Show)

defaultClaudeCodeOptions :: FilePath -> FilePath -> ClaudeCodeOptions
defaultClaudeCodeOptions executable cwd = ClaudeCodeOptions
    { executable
    , cwd
    , permission = ClaudeCodeDontAsk
    , safeMode = True
    , promptWriteTimeoutMicros = 60 * 1_000_000
    }

-- | Keep subscription/auth policy in the adapter. The reusable SDK supports
-- every authentication method accepted by Claude Code; this adapter supplies
-- an environment with API/provider overrides removed.
toClaudeAgentOptions
    :: ClaudeCodeToolMode
    -> ClaudeCodeOptions
    -> IO ClaudeAgentOptions
toClaudeAgentOptions toolMode options = do
    baseEnvironment <- sanitizedClaudeEnvironment
    let environment =
            setEnvironmentVariable
                "ENABLE_CLAUDEAI_MCP_SERVERS"
                "0"
                baseEnvironment
    pure $
        (defaultClaudeAgentOptions options.executable options.cwd)
            { tools = case toolMode of
                ClaudeCodeDefaultTools -> Nothing
                ClaudeCodeNoTools -> Just []
            , disallowedTools = ["AskUserQuestion"]
            , permissionMode = Just case options.permission of
                ClaudeCodeDontAsk -> PermissionDontAsk
                ClaudeCodeBypass -> PermissionBypassPermissions
            , allowDangerouslySkipPermissions =
                options.permission == ClaudeCodeBypass
            , settingSources = Just []
            , mcpServers = Just emptyMcpConfiguration
            , strictMcpConfig = True
            , safeMode = options.safeMode
            , disableSlashCommands = options.safeMode
            , noChrome = True
            , environment = Just environment
            , clientApplication = Just clientApplicationName
            , promptWriteTimeoutMicros =
                options.promptWriteTimeoutMicros
            }

setEnvironmentVariable
    :: String
    -> String
    -> [(String, String)]
    -> [(String, String)]
setEnvironmentVariable name value environment =
    (name, value) : filter ((/= name) . fst) environment

emptyMcpConfiguration :: Aeson.Value
emptyMcpConfiguration =
    Aeson.object
        [ "mcpServers" Aeson..= Aeson.object []
        ]

clientApplicationName :: Text
clientApplicationName = "haskell-agent"
