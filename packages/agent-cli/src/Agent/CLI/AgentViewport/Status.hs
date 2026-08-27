-- | Provider-neutral labels and glyphs for agent status.
module Agent.CLI.AgentViewport.Status
    ( agentStatusGlyph
    , formatAgentStatus
    ) where

import Agent.Subagents (SubagentStatus(..))
import Data.Text (Text)
import qualified Data.Text as Text

formatAgentStatus :: SubagentStatus -> Text
formatAgentStatus status = case status of
    Pending -> "pending"
    Running -> "running"
    Completed _ -> "done"
    Errored _ -> "error"
    Interrupted -> "interrupted"
    Closed -> "closed"
    NotFound -> "missing"

agentStatusGlyph :: Text -> Text
agentStatusGlyph status = case Text.toLower status of
    "active" -> "●"
    "running" -> "●"
    "ready" -> "○"
    "pending" -> "○"
    "done" -> "✓"
    "error" -> "✕"
    "interrupted" -> "■"
    "cancelled" -> "■"
    "closed" -> "×"
    "missing" -> "?"
    _ -> "·"
