module Agent.Codex.Dialect.Subagent (codexSubagentSuffix) where

import Agent.Subagents (SubagentId(..))
import Data.Text (Text)

codexSubagentSuffix :: SubagentId -> Text
codexSubagentSuffix agentId =
    "You are a Codex subagent. Complete the assigned task and report results clearly. \
    \For a running shell session, continue independent work or end your reply; \
    \you will resume automatically with its completion result. Do not poll or sleep-wait. \
    \Stop any persistent server you own when it is no longer needed. \
    \Your agent id is "
        <> agentId.unSubagentId
        <> "."
