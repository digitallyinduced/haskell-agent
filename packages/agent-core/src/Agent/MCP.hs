-- | Local Model Context Protocol clients over the stdio transport.
--
-- Each configured server is started once, initialized, and queried for its
-- read-only tools. The returned 'AppTool' handlers share the retained client;
-- 'closeMcpFleet' must run after all loops and subagents using those handlers
-- have stopped.
module Agent.MCP
    ( McpServerConfig(..)
    , McpInitState(..)
    , McpServerStatus(..)
    , McpToolRegistration(..)
    , McpFleet(..)
    , McpSupervisor
    , McpFleetLease(..)
    , newMcpSupervisor
    , acquireMcpFleet
    , acquireMcpFleetWithProgress
    , acquireMcpFleetProgressive
    , releaseMcpFleetLease
    , closeMcpSupervisor
    , startMcpFleet
    , startMcpFleetWithProgress
    , startMcpFleetProgressive
    , closeMcpFleet
    , mcpFleetTools
    , mcpFleetMetaTools
    , mcpFleetGrokMetaTools
    , mcpFleetStatuses
    , normalizeMcpToolResult
    ) where

import Agent.MCP.Client (normalizeMcpToolResult)
import Agent.MCP.Fleet
    ( closeMcpFleet
    , mcpFleetGrokMetaTools
    , mcpFleetMetaTools
    , mcpFleetStatuses
    , mcpFleetTools
    , startMcpFleet
    , startMcpFleetProgressive
    , startMcpFleetWithProgress
    )
import Agent.MCP.Supervisor
    ( acquireMcpFleet
    , acquireMcpFleetProgressive
    , acquireMcpFleetWithProgress
    , closeMcpSupervisor
    , newMcpSupervisor
    , releaseMcpFleetLease
    )
import Agent.MCP.Types
    ( McpFleet(..)
    , McpFleetLease(..)
    , McpInitState(..)
    , McpServerConfig(..)
    , McpServerStatus(..)
    , McpSupervisor
    , McpToolRegistration(..)
    )
