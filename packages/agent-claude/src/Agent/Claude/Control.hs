-- | Host-facing control callbacks for a structured Claude Code session.
--
-- This module deliberately mirrors the useful control-protocol fields without
-- exposing the lower-level SDK package through the adapter's public API.
module Agent.Claude.Control
    ( ClaudeCodeBackendHandle(..)
    , ClaudeCodeHostHandlers(..)
    , ClaudeCodeMcpRequest(..)
    , ClaudeCodePermissionRequest(..)
    , ClaudeCodePermissionResult(..)
    , defaultClaudeCodeHostHandlers
    , configureClaudeCodeHostTools
    , toClaudeAgentHandlers
    ) where

import Agent.Loop (Backend)
import Claude.Agent.SDK.Control
    ( ClaudeAgentHandlers(..)
    , McpMessageRequest
    , ToolPermissionRequest
    , ToolPermissionResult
    , defaultClaudeAgentHandlers
    )
import qualified Claude.Agent.SDK.Control as SDKControl
import Claude.Agent.SDK.Types (ClaudeAgentOptions(..))
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import Data.Text (Text)

-- | A running backend plus an in-band interrupt action for its active turn.
data ClaudeCodeBackendHandle = ClaudeCodeBackendHandle
    { loopBackend :: !Backend
    , interruptActiveTurn :: !(IO ())
    }

-- | Permission details supplied by Claude Code. Unknown future fields remain
-- available in 'raw'.
data ClaudeCodePermissionRequest = ClaudeCodePermissionRequest
    { toolName :: !Text
    , input :: !Value
    , permissionSuggestions :: ![Value]
    , blockedPath :: !(Maybe Text)
    , decisionReason :: !(Maybe Text)
    , title :: !(Maybe Text)
    , displayName :: !(Maybe Text)
    , description :: !(Maybe Text)
    , toolUseId :: !(Maybe Text)
    , agentId :: !(Maybe Text)
    , raw :: !Value
    } deriving (Eq, Show)

data ClaudeCodePermissionResult
    = ClaudeCodePermissionAllow
        { updatedInput :: !(Maybe Value)
        , updatedPermissions :: ![Value]
        }
    | ClaudeCodePermissionDeny
        { message :: !Text
        , interrupt :: !Bool
        }
    deriving (Eq, Show)

data ClaudeCodeMcpRequest = ClaudeCodeMcpRequest
    { serverName :: !Text
    , message :: !Value
    , raw :: !Value
    } deriving (Eq, Show)

-- | Optional host services. When configured, Claude Code performs the SDK
-- initialize handshake and keeps its bidirectional control stream alive.
data ClaudeCodeHostHandlers = ClaudeCodeHostHandlers
    { canUseTool
        :: !(Maybe
            (ClaudeCodePermissionRequest -> IO ClaudeCodePermissionResult))
    , handleMcpMessage
        :: !(Maybe (ClaudeCodeMcpRequest -> IO Value))
    , mcpServerName :: !Text
    , mcpToolNames :: ![Text]
    }

defaultClaudeCodeHostHandlers :: ClaudeCodeHostHandlers
defaultClaudeCodeHostHandlers = ClaudeCodeHostHandlers
    { canUseTool = Nothing
    , handleMcpMessage = Nothing
    , mcpServerName = "haskell-agent"
    , mcpToolNames = []
    }

-- | Install the synthetic SDK MCP server in Claude Code's strict MCP
-- configuration. MCP tools are pre-approved by Claude itself because every
-- call is independently approved again by the host bridge before dispatch.
configureClaudeCodeHostTools
    :: ClaudeCodeHostHandlers
    -> ClaudeAgentOptions
    -> ClaudeAgentOptions
configureClaudeCodeHostTools handlers options =
    case handlers.handleMcpMessage of
        Nothing -> options
        Just _ ->
            options
                { allowedTools =
                    options.allowedTools
                        <> map
                            (mcpQualifiedName handlers.mcpServerName)
                            handlers.mcpToolNames
                , mcpServers =
                    Just $
                        Aeson.object
                            [ "mcpServers" Aeson..=
                                Aeson.object
                                    [ AesonKey.fromText handlers.mcpServerName
                                        Aeson..=
                                            Aeson.object
                                                [ "type" Aeson..=
                                                    ("sdk" :: Text)
                                                , "name" Aeson..=
                                                    handlers.mcpServerName
                                                ]
                                    ]
                            ]
                , strictMcpConfig = True
                }

toClaudeAgentHandlers :: ClaudeCodeHostHandlers -> ClaudeAgentHandlers
toClaudeAgentHandlers handlers =
    defaultClaudeAgentHandlers
        { canUseTool =
            fmap
                (\callback request ->
                    toSdkPermissionResult
                        <$> callback (fromSdkPermissionRequest request))
                handlers.canUseTool
        , handleMcpMessage =
            fmap
                (\callback request ->
                    callback (fromSdkMcpRequest request))
                handlers.handleMcpMessage
        }

fromSdkPermissionRequest
    :: ToolPermissionRequest
    -> ClaudeCodePermissionRequest
fromSdkPermissionRequest request =
    ClaudeCodePermissionRequest
        { toolName = request.toolName
        , input = request.input
        , permissionSuggestions = request.permissionSuggestions
        , blockedPath = request.blockedPath
        , decisionReason = request.decisionReason
        , title = request.title
        , displayName = request.displayName
        , description = request.description
        , toolUseId = request.toolUseId
        , agentId = request.agentId
        , raw = request.raw
        }

toSdkPermissionResult
    :: ClaudeCodePermissionResult
    -> ToolPermissionResult
toSdkPermissionResult = \case
    ClaudeCodePermissionAllow{updatedInput, updatedPermissions} ->
        SDKControl.ToolPermissionAllow
            { updatedInput
            , updatedPermissions
            }
    ClaudeCodePermissionDeny{message, interrupt} ->
        SDKControl.ToolPermissionDeny
            { message
            , interrupt
            }

fromSdkMcpRequest :: McpMessageRequest -> ClaudeCodeMcpRequest
fromSdkMcpRequest request =
    ClaudeCodeMcpRequest
        { serverName = request.serverName
        , message = request.message
        , raw = request.raw
        }

mcpQualifiedName :: Text -> Text -> Text
mcpQualifiedName server tool =
    "mcp__" <> server <> "__" <> tool
