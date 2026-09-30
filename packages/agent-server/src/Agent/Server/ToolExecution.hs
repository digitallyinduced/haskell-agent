-- | Tool surface for tenants without model-controlled execution.
module Agent.Server.ToolExecution
    ( composeHostOnlyTools
    , hostOnlyExecutionToolNames
    ) where

import Agent.Tools.Types
    ( AppTool(..)
    , AppToolGroup(..)
    )
import Data.Text (Text)

-- | Keep every host service and withhold execution tools rather than run
-- them on the host. Delegated agents compose through the same function, so
-- no agent of such a tenant can reach a shell, local processes or the
-- workspace. Network access remains with host services such as MCP and the
-- configured @web_fetch@.
composeHostOnlyTools :: [AppToolGroup] -> [AppTool]
composeHostOnlyTools = concatMap \case
    HostToolGroup tools -> tools
    ExecutionToolGroup tools ->
        filter ((`elem` hostOnlyExecutionToolNames) . (.appToolName)) tools

-- | Execution-group tools whose handles resolve only to harness-owned
-- artifacts, such as oversized MCP results, never to tenant files.
hostOnlyExecutionToolNames :: [Text]
hostOnlyExecutionToolNames = ["read_tool_output", "search_tool_output"]
