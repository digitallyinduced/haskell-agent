-- | Dialect-specific system prompt closed over by the transport backend.
module Agent.CLI.Prompt
    ( defaultModelFor
    , systemPrompt
    , systemPromptForTools
    ) where

import Agent.CLI.Timestamp (timeContextGuidance)
import Agent.Codex.Dialect.Prompt
    ( codexSystemPrompt
    , codexSystemPromptForTools
    )
import Agent.Dialect
    ( Dialect
    , PromptStyle(..)
    , dialectPromptStyle
    )
import Agent.GrokBuild.Dialect.Prompt
    ( codingGrokPromptTools
    , grokSystemPrompt
    , grokSystemPromptForTools
    )
import Agent.OsPath (toText)
import Agent.Provider (Provider(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.OsPath (OsPath)

defaultModelFor :: Provider -> Text
defaultModelFor = \case
    XAIProvider -> "grok-4.6"
    OpenAIProvider -> "gpt-5.6-luna"
    OpenRouterProvider -> "openai/gpt-5.1"

-- | @isNonInteractive@ is True for one-shot @-p@ (no human in the loop).
systemPrompt :: Dialect -> OsPath -> Day -> Bool -> Text
systemPrompt dialect cwd today isNonInteractive =
    base <> "\n\n" <> ghciGuidance <> "\n" <> timeContextGuidance
  where
    base = case dialectPromptStyle dialect of
        GrokBuildPromptStyle ->
            grokSystemPrompt codingGrokPromptTools cwd today isNonInteractive
        CodexPromptStyle -> codexSystemPrompt cwd today
        GenericResponsesPromptStyle ->
            genericSystemPrompt cwd today isNonInteractive

-- | Render a child prompt against the final filtered application-tool set.
-- @web_search@ is server-side and remains available independently.
systemPromptForTools :: Dialect -> [Text] -> OsPath -> Day -> Bool -> Text
systemPromptForTools dialect toolNames cwd today isNonInteractive =
    Text.intercalate "\n\n" $
        filter (not . Text.null)
            [ base
            , ghciGuidanceForTools available
            , timeContextGuidance
            ]
  where
    available = "web_search" : toolNames
    base = case dialectPromptStyle dialect of
        GrokBuildPromptStyle ->
            grokSystemPromptForTools
                codingGrokPromptTools
                available
                cwd
                today
                isNonInteractive
        CodexPromptStyle ->
            codexSystemPromptForTools available cwd today
        GenericResponsesPromptStyle ->
            genericSystemPromptForTools
                available
                cwd
                today
                isNonInteractive

-- | Prefer GHCI as the general-purpose scripting environment.
ghciGuidance :: Text
ghciGuidance =
    Text.unlines
        [ "Prefer ghci for scripting."
        , "When you need a short program, calculation, or one-off script, use the run_ghci tool rather than Python, Node, bash, or compiling a binary."
        , "run_ghci keeps a persistent GHCi session: bindings and loaded modules stick across calls."
        , "The session enables GHC2021 plus BlockArguments, OverloadedStrings, OverloadedRecordDot, DuplicateRecordFields, NoFieldSelectors, LambdaCase, and RecordWildCards."
        , "Pure expressions do not need user approval; IO and side-effecting GHCi commands do."
        , "Prefer shell tools (run_terminal_cmd or shell_command) for OS commands, package installs, servers, and anything that is not Haskell evaluation."
        , "Drive GHCi with complete expressions; do not expect interactive human input."
        ]

ghciGuidanceForTools :: [Text] -> Text
ghciGuidanceForTools available
    | "run_ghci" `notElem` available = ""
    | otherwise =
        Text.unlines $
            [ "Prefer ghci for scripting."
            , "When you need a short program, calculation, or one-off script, use the run_ghci tool rather than Python, Node, bash, or compiling a binary."
            , "run_ghci keeps a persistent GHCi session: bindings and loaded modules stick across calls."
            , "The session enables GHC2021 plus BlockArguments, OverloadedStrings, OverloadedRecordDot, DuplicateRecordFields, NoFieldSelectors, LambdaCase, and RecordWildCards."
            , "Pure expressions do not need user approval; IO and side-effecting GHCi commands do."
            ]
                <> shellGuidance
                <> [ "Drive GHCi with complete expressions; do not expect interactive human input."
                   ]
  where
    shellNames =
        filter (`elem` available) ["run_terminal_cmd", "shell_command"]
    shellGuidance = case shellNames of
        [] -> []
        names ->
            [ "Prefer shell tools ("
                <> Text.intercalate " or " names
                <> ") for OS commands, package installs, servers, and anything that is not Haskell evaluation."
            ]

genericSystemPrompt :: OsPath -> Day -> Bool -> Text
genericSystemPrompt cwd today isNonInteractive =
    Text.unlines
        [ identity
        , "Today's date is " <> formattedToday <> "."
        , ""
        , "Use the registered tools to inspect and modify the workspace."
        , "- Prefer read_file, grep, and list_dir for codebase exploration."
        , "- Use search_replace for focused file edits."
        , "- Use run_terminal_cmd for commands that require a shell."
        , "- Use run_ghci for Haskell evaluation and short typed scripts."
        , "- Use web_search for current public information."
        , "- Use enter_plan_mode for genuinely ambiguous architectural work."
        , "- Do not mention tools this session does not register."
        , ""
        , "Work directly in " <> toText cwd <> "."
        , "Keep changes scoped to the request and report unverified work plainly."
        ]
  where
    formattedToday =
        Text.pack (formatTime defaultTimeLocale "%Y-%m-%d" today)
    identity
        | isNonInteractive =
            "You are an autonomous coding agent. There is no human operator in this session."
        | otherwise =
            "You are an interactive coding agent helping the user complete software engineering work."

genericSystemPromptForTools :: [Text] -> OsPath -> Day -> Bool -> Text
genericSystemPromptForTools available cwd today isNonInteractive =
    Text.unlines $
        [ identity
        , "Today's date is " <> formattedToday <> "."
        , ""
        , "Use the registered tools to work in the workspace."
        ]
            <> toolLine "read_file" "- Use read_file to inspect files."
            <> toolLine "grep" "- Use grep to search file contents."
            <> toolLine "list_dir" "- Use list_dir to list directories."
            <> toolLine "search_replace"
                "- Use search_replace for focused file edits."
            <> toolLine "run_terminal_cmd"
                "- Use run_terminal_cmd for commands that require a shell."
            <> toolLine "run_ghci"
                "- Use run_ghci for Haskell evaluation and short typed scripts."
            <> toolLine "web_search"
                "- Use web_search for current public information."
            <> toolLine "enter_plan_mode"
                "- Use enter_plan_mode for genuinely ambiguous architectural work."
            <> [ "- Do not mention tools this session does not register."
               , ""
               , "Work directly in " <> toText cwd <> "."
               , "Keep changes scoped to the request and report unverified work plainly."
               ]
  where
    formattedToday =
        Text.pack (formatTime defaultTimeLocale "%Y-%m-%d" today)
    identity
        | isNonInteractive =
            "You are an autonomous coding agent. There is no human operator in this session."
        | otherwise =
            "You are an interactive coding agent helping the user complete software engineering work."
    toolLine name line
        | name `elem` available = [line]
        | otherwise = []
