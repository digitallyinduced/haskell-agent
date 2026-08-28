-- | Local Model Context Protocol clients over the stdio transport.
--
-- Each configured server is started once, initialized, and queried for its
-- read-only tools and (when negotiated) Skills-over-MCP metadata. The
-- returned handlers share the retained client; 'closeMcpFleet' must run
-- after all loops and subagents using those handlers have stopped.
module Agent.MCP
    ( McpServerConfig(..)
    , McpInitState(..)
    , McpServerStatus(..)
    , McpToolRegistration(..)
    , McpSkillsCapability(..)
    , McpSkillRegistration(..)
    , McpSkillEntry(..)
    , McpSkillResources(..)
    , McpSkillResource(..)
    , McpResourceContent(..)
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
    , mcpFleetSkillRegistrations
    , mcpFleetGetSkill
    , mcpFleetReadResource
    , normalizeMcpToolResult
    ) where

import Agent.MCP.Client (normalizeMcpToolResult)
import Agent.MCP.Fleet
    ( closeMcpFleet
    , mcpFleetGrokMetaTools
    , mcpFleetMetaTools
    , mcpFleetStatuses
    , mcpFleetSkillRegistrations
    , mcpFleetGetSkill
    , mcpFleetReadResource
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
    , McpSkillsCapability(..)
    , McpSkillRegistration(..)
    , McpSkillEntry(..)
    , McpSkillResources(..)
    , McpSkillResource(..)
    , McpResourceContent(..)
    )
