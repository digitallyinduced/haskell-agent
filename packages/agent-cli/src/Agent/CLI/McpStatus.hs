module Agent.CLI.McpStatus
    ( formatMcpModelNotice
    , formatMcpModelNoticeFor
    , formatMcpProgress
    , summarizeMcpStatuses
    ) where

import Agent.Dialect (DialectId(..))
import qualified Agent.MCP as MCP
import Data.Text (Text)
import qualified Data.Text as Text

summarizeMcpStatuses :: [MCP.McpServerStatus] -> (Int, Int, Int)
summarizeMcpStatuses statuses =
    ( length (filter isConnecting statuses)
    , length (filter isReady statuses)
    , length (filter isFailed statuses)
    )
  where
    isConnecting status = case status.mcpStatusState of
        MCP.McpPending -> True
        MCP.McpInitializing -> True
        _ -> False
    isReady status = status.mcpStatusState == MCP.McpReady
    isFailed status = case status.mcpStatusState of
        MCP.McpFailed _ -> True
        _ -> False

formatMcpProgress :: [MCP.McpServerStatus] -> Text
formatMcpProgress statuses =
    let (connecting, ready, failed) = summarizeMcpStatuses statuses
    in if null statuses || connecting == 0
        then
            "Loading built-in tools…"
                <> if ready + failed == 0
                    then ""
                    else
                        " MCP: "
                            <> Text.pack (show ready)
                            <> " ready"
                            <> if failed == 0
                                then ""
                                else
                                    ", "
                                        <> Text.pack (show failed)
                                        <> " unavailable"
        else
            "Loading built-in tools… MCP: "
                <> Text.pack (show connecting)
                <> " connecting, "
                <> Text.pack (show ready)
                <> " ready"

formatMcpModelNotice :: [MCP.McpServerStatus] -> Text
formatMcpModelNotice =
    formatMcpModelNoticeFor CodexDialect

formatMcpModelNoticeFor
    :: DialectId
    -> [MCP.McpServerStatus]
    -> Text
formatMcpModelNoticeFor activeDialect statuses =
    let connecting =
            [ status.mcpStatusName
            | status <- statuses
            , status.mcpStatusState
                `elem` [MCP.McpPending, MCP.McpInitializing]
            ]
        ready =
            [ status.mcpStatusName
            | status <- statuses
            , status.mcpStatusState == MCP.McpReady
            ]
        failed =
            [ status.mcpStatusName
            | status <- statuses
            , case status.mcpStatusState of
                MCP.McpFailed _ -> True
                _ -> False
            ]
    in "<system-reminder>MCP status changed. "
        <> statusPart "Connecting" connecting
        <> statusPart "Ready" ready
        <> statusPart "Unavailable" failed
        <> if activeDialect == GrokBuildDialect
            then
                "Use search_tool to discover currently available MCP tools and "
                    <> "use_tool to invoke one by its server__tool name.</system-reminder>"
            else
                "Use mcp_search to discover currently available MCP tools and "
                    <> "mcp_call to invoke one by its server__tool name.</system-reminder>"
  where
    statusPart _ [] = ""
    statusPart label names =
        label <> ": " <> Text.intercalate ", " names <> ". "
