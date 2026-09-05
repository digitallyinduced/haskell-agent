-- | Execution facts retained locally alongside transport output. Provider JSON
-- deliberately omits these facts; durable session storage encodes them separately.
module Agent.ToolOutcome
    ( ToolOutcome(..)
    , toolOutcomeSucceeded
    , toolOutcomeDecoder
    ) where

import qualified Agent.Json.Decode as Json
import Data.Aeson (ToJSON(..), Value, object, (.=))
import Data.Text (Text)

data ToolOutcome
    = ToolSucceeded
    | ToolFailed
    | ToolDenied
    | ToolCancelled
    | ShellRunning !Int
    | ShellExited !Int
    | ShellCancelled
    | ShellTimedOut
    deriving (Eq, Show)

toolOutcomeSucceeded :: ToolOutcome -> Bool
toolOutcomeSucceeded = \case
    ToolSucceeded -> True
    ShellRunning _ -> True
    ShellExited 0 -> True
    _ -> False

instance ToJSON ToolOutcome where
    toJSON = \case
        ToolSucceeded -> tagged "succeeded"
        ToolFailed -> tagged "failed"
        ToolDenied -> tagged "denied"
        ToolCancelled -> tagged "cancelled"
        ShellRunning sessionId -> object ["kind" .= ("shell_running" :: Text), "session_id" .= sessionId]
        ShellExited code -> object ["kind" .= ("shell_exited" :: Text), "exit_code" .= code]
        ShellCancelled -> tagged "shell_cancelled"
        ShellTimedOut -> tagged "shell_timed_out"
      where
        tagged :: Text -> Value
        tagged kind = object ["kind" .= kind]

toolOutcomeDecoder :: Json.Decoder ToolOutcome
toolOutcomeDecoder = Json.discriminatedObject "kind" \case
    "succeeded" -> pure ToolSucceeded
    "failed" -> pure ToolFailed
    "denied" -> pure ToolDenied
    "cancelled" -> pure ToolCancelled
    "shell_running" -> Json.object $ ShellRunning <$> Json.atKey "session_id" Json.int
    "shell_exited" -> Json.object $ ShellExited <$> Json.atKey "exit_code" Json.int
    "shell_cancelled" -> pure ShellCancelled
    "shell_timed_out" -> pure ShellTimedOut
    _ -> fail "unknown tool outcome"
