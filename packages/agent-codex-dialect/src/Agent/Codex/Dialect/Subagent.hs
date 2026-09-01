module Agent.Codex.Dialect.Subagent (codexSubagentSuffix) where

import Agent.Subagents (SubagentId(..))
import Data.Text (Text)

codexSubagentSuffix :: SubagentId -> Text
codexSubagentSuffix agentId =
    "You are a Codex subagent. Complete the assigned task and report results clearly. \
    \Before finishing, perform one bounded wait for any shell session you started that is still running. \
    \Your agent id is "
        <> agentId.unSubagentId
        <> "."
