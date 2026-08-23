module Agent.Codex.Dialect.ProjectInstructions
    ( formatCodexAgentsMd
    ) where

import Agent.OsPath (toText)
import Agent.ProjectInstructions
    ( LoadedAgentsMd(..)
    , nonEmptyInstructionContent
    )
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import System.OsPath (OsPath)

formatCodexAgentsMd :: OsPath -> LoadedAgentsMd -> Maybe Text
formatCodexAgentsMd cwd loaded =
    case bodies of
        Nothing -> Nothing
        Just body ->
            Just $ Text.concat
                [ "# AGENTS.md instructions for "
                , toText cwd
                , "\n\n<INSTRUCTIONS>\n"
                , body
                , "\n</INSTRUCTIONS>"
                ]
  where
    bodies = case
        ( loaded.loadedGlobal >>= nonEmptyInstructionContent
        , mapMaybe nonEmptyInstructionContent loaded.loadedProject
        ) of
        (Nothing, []) -> Nothing
        (Nothing, project) -> Just (Text.intercalate "\n\n" project)
        (Just global, []) -> Just global
        (Just global, project) ->
            Just $ global <> "\n\n--- project-doc ---\n\n"
                <> Text.intercalate "\n\n" project
