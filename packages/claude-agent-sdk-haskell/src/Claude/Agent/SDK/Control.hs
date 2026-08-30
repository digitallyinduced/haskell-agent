-- | Typed values used by Claude Code's bidirectional SDK control protocol.
--
-- Every decoded request retains its original JSON value so callers can
-- forward fields added by newer Claude Code releases.
module Claude.Agent.SDK.Control
    ( ClaudeAgentHandlers(..)
    , defaultClaudeAgentHandlers
    , ControlRequest(..)
    , ControlResponse(..)
    , ControlCancelRequest(..)
    , ToolPermissionRequest(..)
    , ToolPermissionResult(..)
    , McpMessageRequest(..)
    ) where

import Data.Aeson (Value)
import Data.Text (Text)

-- | Host callbacks served while Claude Code is running.
--
-- Missing handlers fail closed: permission requests are denied and MCP or
-- unknown requests receive a protocol error.
data ClaudeAgentHandlers = ClaudeAgentHandlers
    { canUseTool
        :: !(Maybe
            (ToolPermissionRequest -> IO ToolPermissionResult))
    , handleMcpMessage
        :: !(Maybe (McpMessageRequest -> IO Value))
    -- | Forward-compatible escape hatch for request subtypes not understood
    -- by this SDK version. The returned object is placed in the successful
    -- control response.
    , handleUnknownControl
        :: !(Maybe (ControlRequest -> IO Value))
    -- | Extra fields merged into the @initialize@ request. Reserved fields
    -- (@subtype@ and @hooks@) are always supplied by the SDK.
    , initializeOptions :: !(Maybe Value)
    , controlRequestTimeoutMicros :: !Int
    , initializeTimeoutMicros :: !Int
    , shutdownTimeoutMicros :: !Int
    }

defaultClaudeAgentHandlers :: ClaudeAgentHandlers
defaultClaudeAgentHandlers = ClaudeAgentHandlers
    { canUseTool = Nothing
    , handleMcpMessage = Nothing
    , handleUnknownControl = Nothing
    , initializeOptions = Nothing
    , controlRequestTimeoutMicros = 60 * 1_000_000
    , initializeTimeoutMicros = 60 * 1_000_000
    , shutdownTimeoutMicros = 5 * 1_000_000
    }

data ToolPermissionRequest = ToolPermissionRequest
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

data ToolPermissionResult
    = ToolPermissionAllow
        { updatedInput :: !(Maybe Value)
        , updatedPermissions :: ![Value]
        }
    | ToolPermissionDeny
        { message :: !Text
        , interrupt :: !Bool
        }
    deriving (Eq, Show)

data McpMessageRequest = McpMessageRequest
    { serverName :: !Text
    , message :: !Value
    , raw :: !Value
    } deriving (Eq, Show)

-- | A decoded control request payload. 'ControlUnknown' is deliberately
-- retained rather than rejected during decoding.
data ControlRequest
    = ControlCanUseTool !ToolPermissionRequest
    | ControlMcpMessage !McpMessageRequest
    | ControlInterrupt !Value
    | ControlInitialize !Value
    | ControlSetPermissionMode !Text !Value
    | ControlSetModel !(Maybe Text) !Value
    | ControlGetContextUsage !Value
    | ControlStopTask !Text !Value
    | ControlUnknown !(Maybe Text) !Value
    deriving (Eq, Show)

data ControlResponse
    = ControlSuccess
        { requestId :: !Text
        , response :: !Value
        , raw :: !Value
        }
    | ControlError
        { requestId :: !Text
        , error :: !Text
        , raw :: !Value
        }
    deriving (Eq, Show)

data ControlCancelRequest = ControlCancelRequest
    { requestId :: !Text
    , raw :: !Value
    } deriving (Eq, Show)
