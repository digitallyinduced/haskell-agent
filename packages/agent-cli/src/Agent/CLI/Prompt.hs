-- | Dialect-specific system prompt closed over by the transport backend.
module Agent.CLI.Prompt
    ( defaultModelFor
    , systemPrompt
    , systemPromptForTools
    ) where

import Agent.CLI.Timestamp (timeContextGuidance)
import Agent.Dialect
    ( Dialect
    , PromptStyle(..)
    , dialectPromptStyle
    )
import Agent.OsPath (toText)
import Agent.Provider (Provider(..))
import Agent.Tools.Grok.Prompt
    ( codingGrokPromptTools
    , grokSystemPrompt
    , grokSystemPromptForTools
    )
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
    ClaudeCodeProvider -> "sonnet"

-- | @isNonInteractive@ is True for one-shot @-p@ (no human in the loop).
systemPrompt :: Dialect -> OsPath -> Day -> Bool -> Text
systemPrompt dialect cwd today isNonInteractive =
    base <> "\n\n" <> ghciGuidanceForDialect dialect
        <> "\n" <> timeContextGuidance
  where
    base = case dialectPromptStyle dialect of
        GrokBuildPromptStyle ->
            grokSystemPrompt codingGrokPromptTools cwd today isNonInteractive
        CodexPromptStyle -> codexSystemPrompt cwd today
        GenericResponsesPromptStyle ->
            genericSystemPrompt cwd today isNonInteractive
        ClaudeCodePromptStyle ->
            claudeCodeSystemPrompt cwd today

-- | Render a child prompt against the final filtered application-tool set.
-- @web_search@ is server-side and remains available independently.
systemPromptForTools :: Dialect -> [Text] -> OsPath -> Day -> Bool -> Text
systemPromptForTools dialect toolNames cwd today isNonInteractive =
    Text.intercalate "\n\n" $
        filter (not . Text.null)
            [ base
            , ghciGuidanceForTools dialect available
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
        ClaudeCodePromptStyle ->
            claudeCodeSystemPrompt cwd today

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

ghciGuidanceForDialect :: Dialect -> Text
ghciGuidanceForDialect dialect =
    case dialectPromptStyle dialect of
        GrokBuildPromptStyle ->
            Text.replace "run_terminal_cmd" "run_terminal_command"
                ghciGuidance
        CodexPromptStyle -> ghciGuidance
        GenericResponsesPromptStyle -> ghciGuidance
        ClaudeCodePromptStyle -> ""

ghciGuidanceForTools :: Dialect -> [Text] -> Text
ghciGuidanceForTools dialect available
    | dialectPromptStyle dialect == ClaudeCodePromptStyle = ""
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
    shellNames = case dialectPromptStyle dialect of
        GrokBuildPromptStyle ->
            [ "run_terminal_command"
            | "run_terminal_cmd" `elem` available
                || "run_terminal_command" `elem` available
            ]
        CodexPromptStyle ->
            filter (`elem` available) ["run_terminal_cmd", "shell_command"]
        GenericResponsesPromptStyle ->
            filter (`elem` available) ["run_terminal_cmd", "shell_command"]
        ClaudeCodePromptStyle -> []
    shellGuidance = case shellNames of
        [] -> []
        names ->
            [ "Prefer shell tools ("
                <> Text.intercalate " or " names
                <> ") for OS commands, package installs, servers, and anything that is not Haskell evaluation."
            ]

codexSystemPrompt :: OsPath -> Day -> Text
codexSystemPrompt cwd today =
    Text.unlines
        [ "You are a coding agent working in " <> toText cwd <> "."
        , "Today's date is " <> Text.pack (formatTime defaultTimeLocale "%Y-%m-%d" today) <> "."
        , ""
        , "Use these tools:"
        , "- Inspect files with read_file, grep, and list_dir. Outside Plan Mode, shell_command is also available; always set workdir."
        , "- Edit files with apply_patch. Never call applypatch or apply-patch."
        , "- Track multi-step work with update_plan (progress checklist; unavailable in Plan Mode)."
        , "- Evaluate Haskell with run_ghci (persistent GHCi; pure expressions auto-approve)."
        , "- Look up current public information with web_search."
        , "- Enter Plan Mode with enter_plan_mode; write its plan with write_plan; ask planning questions with ask_user_question."
        , ""
        , "Web search citation references such as turn2search5 are internal."
        , "Use them only when interacting with web_search; never expose internal reference IDs"
        , "or citation-marker syntax such as <cite|...> in user-visible output."
        , "Cite web sources using descriptive Markdown links instead."
        , ""
        , "Plan Mode: use it for software implementation or architectural planning when code"
        , "changes are expected after approval, or when the user explicitly requests Plan Mode."
        , "Do not enter it for conversational plans such as business, writing, or travel plans."
        , "The transition requires user approval."
        , "When a developer reminder says plan mode is active, explore read-only,"
        , "write the design with write_plan only, and present the final plan in a <proposed_plan>"
        , "block (opening and closing tags on their own lines). Do not implement until plan"
        , "mode ends. update_plan is not Plan Mode."
        , "Be concise. Do not mention tools this session does not register."
        ]

claudeCodeSystemPrompt :: OsPath -> Day -> Text
claudeCodeSystemPrompt cwd today =
    Text.unlines
        [ "You are Claude Code running as the model and tool runtime for an independent agent harness."
        , "Work in " <> toText cwd <> "."
        , "Today's date is " <> Text.pack (formatTime defaultTimeLocale "%Y-%m-%d" today) <> "."
        , ""
        , "Use Claude Code's built-in tools directly. The outer harness renders Claude Code's"
        , "validated structured output and does not execute tool calls on your behalf."
        , "Follow any AGENTS.md instructions supplied in user context."
        , "Be concise in user-visible responses."
        ]

codexSystemPromptForTools :: [Text] -> OsPath -> Day -> Text
codexSystemPromptForTools available cwd today =
    Text.unlines $
        [ "You are a coding agent working in " <> toText cwd <> "."
        , "Today's date is " <> formattedToday <> "."
        , ""
        , "Use the registered tools:"
        ]
            <> toolLine "shell_command"
                "- Inspect the repo and run system commands with shell_command. Always set workdir."
            <> toolLine "apply_patch"
                "- Edit files with apply_patch. Never call applypatch or apply-patch."
            <> toolLine "update_plan"
                "- Track multi-step work with update_plan."
            <> toolLine "run_ghci"
                "- Evaluate Haskell with run_ghci."
            <> toolLine "web_search"
                "- Look up current public information with web_search."
            <> planLines
            <> ["Be concise. Do not mention tools this session does not register."]
  where
    formattedToday =
        Text.pack (formatTime defaultTimeLocale "%Y-%m-%d" today)
    toolLine name line
        | name `elem` available = [line]
        | otherwise = []
    planLines =
        toolLine "enter_plan_mode"
            "- Enter Plan Mode with enter_plan_mode for genuinely ambiguous architectural work."
            <> toolLine "ask_user_question"
                "- Ask planning questions with ask_user_question."

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
