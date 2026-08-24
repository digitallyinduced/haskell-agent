-- | Dialect-specific system prompt closed over by the transport backend.
module Agent.CLI.Prompt
    ( secretInputGuidance
    , subscriptionSubagentModelGuidance
    , sessionTempGuidance
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
import Agent.Provider (BillingMode(..), Provider(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.OsPath (OsPath)

-- | Nudge subscription-backed OpenAI sessions toward the inexpensive model
-- for bounded delegation without encouraging unnecessary or risky spawning.
subscriptionSubagentModelGuidance :: Provider -> BillingMode -> Maybe Text
subscriptionSubagentModelGuidance provider billing
    | provider == OpenAIProvider
    , billing == SubscriptionBilled =
        Just $ Text.unwords
            [ "When OpenAI subscription billing is active, prefer"
            , "`gpt-5.6-luna` for small, bounded tasks such as codebase searches,"
            , "locating definitions, straightforward reviews, test investigation,"
            , "and concise summaries. Keep the parent model for complex debugging,"
            , "architecture, security-sensitive analysis, or work requiring"
            , "substantial judgment. Do not spawn a subagent when doing the work"
            , "directly would be faster."
            ]
    | otherwise = Nothing

-- | @isNonInteractive@ is True for one-shot @-p@ (no human in the loop).
systemPrompt :: Dialect -> OsPath -> Maybe OsPath -> Day -> Bool -> Text
systemPrompt dialect cwd sessionTmp today isNonInteractive =
    Text.intercalate "\n\n" $
        filter (not . Text.null)
            [ base
            , sessionTempGuidance sessionTmp
            , ghciGuidanceForDialect dialect
            , timeContextGuidance
            ]
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
systemPromptForTools
    :: Dialect
    -> [Text]
    -> OsPath
    -> Maybe OsPath
    -> Day
    -> Bool
    -> Text
systemPromptForTools
        dialect toolNames cwd sessionTmp today isNonInteractive =
    Text.intercalate "\n\n" $
        filter (not . Text.null)
            [ base
            , sessionTempGuidance sessionTmp
            , secretInputGuidance available
            , learnedSkillGuidance available
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

-- | Tell the model about its second filesystem sandbox root without changing
-- the project/worktree used for relative paths.
sessionTempGuidance :: Maybe OsPath -> Text
sessionTempGuidance = \case
    Nothing -> ""
    Just path ->
        Text.unlines
            [ "Session temporary directory: " <> toText path
            , "Use this private directory for clones, downloads, extracted files, generated assets, and other scratch work."
            , "Filesystem tools may access both the workspace and this directory; relative paths still resolve against the workspace."
            , "HASKELL_AGENT_TMPDIR and TMPDIR point to this directory for shell commands."
            ]

-- | Keep sensitive values outside model-visible text and tool arguments when
-- the host exposes the dedicated secret-entry capability.
secretInputGuidance :: [Text] -> Text
secretInputGuidance available
    | "ask_secret" `notElem` available = ""
    | otherwise =
        Text.unlines
            [ "Secret handling:"
            , "- Never ask the user to paste a token, API key, password, or other secret into chat or a normal tool argument."
            , "- Use ask_secret to request sensitive values. It returns a private temporary file path, never the secret value."
            , "- Pass that path to a consumer that supports file input and delete the file promptly after use."
            , "- Never read, print, summarize, or otherwise expose the secret file contents."
            ]

learnedSkillGuidance :: [Text] -> Text
learnedSkillGuidance available
    | "skill_search" `notElem` available = ""
    | otherwise =
        Text.unlines
            [ "Learned skills:"
            , "- Use skill_search and skill_read when reusable guidance from earlier sessions may apply."
            , "- When the user establishes a durable preference, decision, lesson, or repeatable procedure that will help future sessions, consider promoting it with skill_create or skill_update."
            , "- Store actionable reusable guidance, not ordinary facts or transient task state. Search before creating, prefer updating an existing skill, and choose the narrowest correct scope."
            ]

-- | Prefer GHCI as the general-purpose scripting environment.
ghciGuidance :: Text
ghciGuidance =
    Text.unlines
        [ "Prefer ghci for scripting."
        , "When you need a short program, calculation, or one-off script, use the run_ghci tool rather than Python, Node, bash, or compiling a binary."
        , "run_ghci keeps a persistent GHCi session: bindings and loaded modules stick across calls."
        , "The session enables GHC2021 plus BlockArguments, OverloadedStrings, OverloadedRecordDot, DuplicateRecordFields, NoFieldSelectors, LambdaCase, and RecordWildCards."
        , "Pure expressions do not need user approval; IO and side-effecting GHCi commands do."
        , "Useful built-ins: cmd/cmdIn capture argv-based commands, cmd_/cmdIn_ print their output, and readText/writeText/appendText/listFiles/pathExists cover common file operations."
        , "Examples: cmd \"git\" [\"status\",\"--short\"]; cmdIn \"/repo\" \"git\" [\"diff\",\"--check\"]; text <- readText \"input.txt\"."
        , "Imports are standalone GHCi statements, never lines inside a do block. Split imports, reusable bindings, and execution across calls instead of building one giant expression."
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
            , "Useful built-ins: cmd/cmdIn capture argv-based commands, cmd_/cmdIn_ print their output, and readText/writeText/appendText/listFiles/pathExists cover common file operations."
            , "Examples: cmd \"git\" [\"status\",\"--short\"]; cmdIn \"/repo\" \"git\" [\"diff\",\"--check\"]; text <- readText \"input.txt\"."
            , "Imports are standalone GHCi statements, never lines inside a do block. Split imports, reusable bindings, and execution across calls instead of building one giant expression."
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
