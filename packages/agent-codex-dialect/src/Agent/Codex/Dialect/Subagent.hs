module Agent.Codex.Dialect.Subagent (codexSubagentSuffix) where

import Agent.Subagents (SubagentId(..))
import Data.Text (Text)

codexSubagentSuffix :: SubagentId -> Text
codexSubagentSuffix agentId =
    "You are a Codex subagent. Complete the assigned task and report results clearly. \
    \Your agent id is "
        <> agentId.unSubagentId
        <> "."
