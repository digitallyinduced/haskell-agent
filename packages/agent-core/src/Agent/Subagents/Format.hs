-- | Pure status predicates and wire/user-facing subagent formatting.
module Agent.Subagents.Format
    ( encodeStatus
    , formatCompletionNotice
    , isFinalStatus
    ) where

import Agent.Subagents.Types (SubagentId(..), SubagentStatus(..))
import Data.Aeson ((.=), object)
import qualified Data.Aeson as Aeson
import Data.Text (Text)

isFinalStatus :: SubagentStatus -> Bool
isFinalStatus = \case
    Completed _ -> True
    Errored _ -> True
    Interrupted -> True
    Closed -> True
    NotFound -> True
    Pending -> False
    Running -> False

encodeStatus :: SubagentStatus -> Aeson.Value
encodeStatus = \case
    Pending -> Aeson.String "pending_init"
    Running -> Aeson.String "running"
    Interrupted -> Aeson.String "interrupted"
    Closed -> Aeson.String "shutdown"
    NotFound -> Aeson.String "not_found"
    Completed text -> object ["completed" .= text]
    Errored err -> object ["errored" .= err]

-- | Parent-facing notice when a child finishes without an active wait_agent.
formatCompletionNotice :: SubagentId -> SubagentStatus -> Text
formatCompletionNotice agentId status =
    "<subagent_notification>\n"
        <> "agent_id: "
        <> agentId.unSubagentId
        <> "\nstatus: "
        <> statusSummary status
        <> "\n</subagent_notification>"

statusSummary :: SubagentStatus -> Text
statusSummary = \case
    Completed (Just text) -> "completed\nfinal: " <> text
    Completed Nothing -> "completed"
    Errored err -> "errored\nerror: " <> err
    Interrupted -> "interrupted"
    Closed -> "shutdown"
    Pending -> "pending_init"
    Running -> "running"
    NotFound -> "not_found"
