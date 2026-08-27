module Agent.CLI.Command.Instructions
    ( deepResearchInstruction
    , goalInstruction
    , loopScheduleInstruction
    , workflowInstruction
    ) where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)

loopScheduleInstruction :: Text -> Text
loopScheduleInstruction args =
    Text.unlines
        [ "# /loop -- schedule a recurring prompt"
        , ""
        , "Turn the input below into a scheduler_create call."
        , "Each fire runs in a detached background subagent, not in this conversation, so the stored prompt must stand on its own."
        , "Inline every path, job/PR/branch id, status command, success condition, and stop condition that a fresh fire needs."
        , "Keep one fire bounded: it must report a short status and stop rather than polling inline."
        , "The parent session or user owns cancellation; the detached child cannot modify the schedule. Tasks otherwise expire after seven days."
        , ""
        , "Convert the user's cadence, wherever phrased, into a compact <number><unit> interval using s, m, h, or d."
        , "The minimum is 60 seconds. If no cadence is present, ask how often to run and never invent a default."
        , ""
        , "Call scheduler_create with the interval, the self-contained prompt, and fire_immediately: true."
        , "Do not execute the scheduled prompt inline. Confirm the cadence, stop condition, seven-day expiry, and task_id."
        , ""
        , "Input:"
        , args
        ]

goalInstruction :: Text -> Text
goalInstruction objective =
    Text.unlines
        [ "# /goal -- pursue an objective"
        , ""
        , "A goal has been set: " <> objective
        , ""
        , "Work directly on this goal and carry it as far as possible. Deliver everything requested without leaving manual steps for the user."
        , "Break the objective into concrete tracked steps and verify changes on the real path as you go."
        , "Call update_goal(completed: true, message: \"summary\") only when the goal is fully achieved."
        , "Call update_goal(blocked_reason: \"reason\") only when truly stuck after at least three consecutive failed attempts at the same problem."
        , "Call update_goal(message: \"status note\") to record useful progress. If update_goal errors, continue and report status in the reply."
        , ""
        , "Start now."
        ]

workflowInstruction :: Text -> Text -> Text
workflowInstruction name input =
    let argsJson =
            decodeUtf8
                (LazyByteString.toStrict
                    (Aeson.encode (Aeson.object ["query" .= input])))
    in Text.unlines
        [ "# /workflow -- launch a named workflow"
        , ""
        , "Call the workflow tool immediately with exactly the name and args below."
        , "The args value is a JSON object whose query field contains the verbatim input; do not omit args or flatten query into a top-level tool argument."
        , "Do not imitate the workflow inline or inspect the workspace before launching it."
        , "If the workflow tool rejects an unsupported option, report that error honestly rather than silently changing semantics."
        , ""
        , "name: " <> name
        , "args: " <> argsJson
        ]

deepResearchInstruction :: Text -> Text
deepResearchInstruction = workflowInstruction "deep-research"
