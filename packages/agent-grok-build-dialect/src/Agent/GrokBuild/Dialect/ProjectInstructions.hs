module Agent.GrokBuild.Dialect.ProjectInstructions
    ( formatGrokAgentsMd
    ) where

import Agent.OsPath (toText)
import Agent.ProjectInstructions
    ( InstructionFile(..)
    , LoadedAgentsMd
    , loadedInstructionFiles
    )
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

formatGrokAgentsMd :: LoadedAgentsMd -> Maybe Text
formatGrokAgentsMd loaded =
    case mapMaybe withContent (loadedInstructionFiles loaded) of
        [] -> Nothing
        kept ->
            Just $ Text.concat $
                [ "\n\n<system-reminder>\n"
                , "As you answer the user's questions, you can use the following context"
                , " (ordered from repo root to current directory - deeper files take precedence on conflicts):\n"
                ]
                <> concatMap renderFile kept
                <>
                [ "\nFollow these instructions exactly. When working in subdirectories not listed above, "
                , "check for additional project instruction files (AGENTS.md, Claude.md, etc.)."
                , "\n</system-reminder>"
                ]
  where
    withContent file =
        (\content -> (file, content)) <$> nonEmptyInstructionContent file
    renderFile :: (InstructionFile, Text) -> [Text]
    renderFile (file, content) =
        [ "\n## From: "
        , neutralizeReminderTags (toText file.instructionPath)
        , "\n"
        , neutralizeReminderTags content
        , "\n"
        ]

nonEmptyInstructionContent :: InstructionFile -> Maybe Text
nonEmptyInstructionContent file =
    let text = file.instructionContent
    in if Text.null (Text.strip text) then Nothing else Just text

neutralizeReminderTags :: Text -> Text
neutralizeReminderTags =
    Text.replace "<system-reminder" "&lt;system-reminder"
        . Text.replace "</system-reminder" "&lt;/system-reminder"
        . Text.replace "<system_reminder" "&lt;system_reminder"
        . Text.replace "</system_reminder" "&lt;/system_reminder"
