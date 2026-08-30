-- | Dialect-specific system prompt closed over by the transport backend.
module Agent.CLI.Prompt
    ( appendMcpInstructions
    , codexEnvironmentContext
    , mcpInstructionsForRequest
    , mcpInstructionsGuidance
    , secretInputGuidance
    , imageDisplayGuidance
    , subscriptionSubagentModelGuidance
    , sessionTempGuidance
    , systemPrompt
    , systemPromptForCatalogModel
    , systemPromptForTools
    ) where

import Agent.CLI.Timestamp (timeContextGuidance)
import Agent.Codex.Dialect.Prompt
    ( codexSystemPrompt
    , codexSystemPromptForTools
    )
import Agent.OpenAI.Models
    ( ModelInfo
    , ModelPersonality(..)
    , renderModelInstructions
    )
import Agent.Dialect
    ( Dialect
    , PromptStyle(..)
    , dialectPromptStyle
    )
import Agent.CLI.Tools (hostedSearchToolNames)
import Agent.GrokBuild.Dialect.Prompt
    ( codingGrokPromptTools
    , grokSystemPrompt
    , grokSystemPromptForTools
    )
import Agent.OsPath (toText)
import Agent.Provider (BillingMode(..), Provider(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.OsPath (OsPath)

-- | Help subscription-backed OpenAI sessions choose a child model by the
-- delegated outcome. Keep this as guidance rather than a hard-coded override:
-- the parent has the goal and context needed to judge ambiguity and risk.
subscriptionSubagentModelGuidance :: Provider -> BillingMode -> Maybe Text
subscriptionSubagentModelGuidance provider billing
    | provider == OpenAIProvider
    , billing == SubscriptionBilled =
        Just $ Text.unwords
            [ "Choose the subagent model from the delegated goal's complexity,"
            , "ambiguity, and risk—not"
            , "just its apparent size. Consider how much the parent outcome depends"
            , "on the result and how easily errors can be detected. Use"
            , "`gpt-5.6-luna` (fast · low cost) only"
            , "for mechanical, bounded, easily verified work such as locating"
            , "definitions or summarizing known material. Use `gpt-5.6-terra`"
            , "(balanced) for routine implementation, debugging, test investigation,"
            , "and reviews needing moderate judgment. Use `gpt-5.6-sol` (frontier)"
            , "for complex or ambiguous debugging, architecture, security- or"
            , "correctness-sensitive analysis, cross-cutting work, or substantial"
            , "judgment. Do not use Luna as a blanket default; when uncertain,"
            , "prefer the stronger tier. Honor an explicitly requested model."
            , "Omit the override when the parent model already matches the chosen"
            , "tier. Do not spawn a subagent when doing the work directly would"
            , "be faster."
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
-- Hosted search tools are server-side and remain available independently.
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
            , imageDisplayGuidance available
            , learnedSkillGuidance available
            , ghciGuidanceForTools dialect available
            , timeContextGuidance
            ]
  where
    availableNames = hostedSearchToolNames dialect ++ toolNames
    available = Set.fromList availableNames
    base = case dialectPromptStyle dialect of
        GrokBuildPromptStyle ->
            grokSystemPromptForTools
                codingGrokPromptTools
                availableNames
                cwd
                today
                isNonInteractive
        CodexPromptStyle ->
            codexSystemPromptForTools availableNames cwd today
        GenericResponsesPromptStyle ->
            genericSystemPromptForTools
                availableNames
                cwd
                today
                isNonInteractive
        ClaudeCodePromptStyle ->
            claudeCodeSystemPrompt cwd today

-- | Render instructions for an OpenAI model whose catalog entry carries an
-- instructions template. The template is emitted verbatim (Codex sends it
-- unmodified as the base-instructions developer item); harness-specific
-- runtime guidance follows in the same developer text. Working directory and
-- date deliberately stay out of the instructions — catalog models receive
-- them through 'codexEnvironmentContext', matching upstream placement.
systemPromptForCatalogModel
    :: Dialect
    -> ModelInfo
    -> [Text]
    -> Maybe OsPath
    -> Text
systemPromptForCatalogModel dialect info toolNames sessionTmp =
    Text.intercalate "\n\n" $
        filter (not . Text.null)
            [ Text.strip
                (renderModelInstructions ModelPersonalityDefault info)
            , sessionTempGuidance sessionTmp
            , secretInputGuidance available
            , imageDisplayGuidance available
            , learnedSkillGuidance available
            , ghciGuidanceForTools dialect available
            , timeContextGuidance
            ]
  where
    available =
        Set.fromList (hostedSearchToolNames dialect ++ toolNames)

-- | The upstream @<environment_context>@ user fragment: working directory,
-- shell, date, and timezone. Sent as conversation context rather than inside
-- the instructions, matching where Codex-trained models expect it.
codexEnvironmentContext
    :: OsPath
    -> Day
    -> Maybe Text
    -> Maybe Text
    -> Text
codexEnvironmentContext cwd today shell timezone =
    Text.intercalate "\n" $
        [ "<environment_context>"
        , "  <cwd>" <> escapeXml (toText cwd) <> "</cwd>"
        ]
            <> [ "  <shell>" <> escapeXml value <> "</shell>"
               | Just value <- [shell]
               ]
            <> [ "  <current_date>" <> formattedToday <> "</current_date>" ]
            <> [ "  <timezone>" <> escapeXml value <> "</timezone>"
               | Just value <- [timezone]
               ]
            <> [ "</environment_context>" ]
  where
    formattedToday =
        Text.pack (formatTime defaultTimeLocale "%Y-%m-%d" today)
    escapeXml =
        Text.replace ">" "&gt;"
            . Text.replace "<" "&lt;"
            . Text.replace "&" "&amp;"

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
            , "Filesystem-tool paths under /tmp or /private/tmp are redirected into this directory."
            , "HASKELL_AGENT_TMPDIR and TMPDIR point to this directory for shell commands; use $TMPDIR instead of a literal /tmp or /private/tmp path."
            ]

-- | Keep sensitive values outside model-visible text and tool arguments when
-- the host exposes the dedicated secret-entry capability.
secretInputGuidance :: Set Text -> Text
secretInputGuidance available
    | "ask_secret" `Set.notMember` available = ""
    | otherwise =
        Text.unlines
            [ "Secret handling:"
            , "- Never ask the user to paste a token, API key, password, or other secret into chat or a normal tool argument."
            , "- Use ask_secret to request sensitive values. It returns a private temporary file path, never the secret value."
            , "- Pass that path to a consumer that supports file input and delete the file promptly after use."
            , "- Never read, print, summarize, or otherwise expose the secret file contents."
            ]

-- | Point the model at inline image display when the host can present one.
imageDisplayGuidance :: Set Text -> Text
imageDisplayGuidance available
    | "show_image" `Set.notMember` available = ""
    | otherwise =
        Text.unlines
            [ "Image display:"
            , "- Use show_image to present an image file (PNG, JPEG, GIF, BMP, TIFF) inline to the user, for example screenshots, rendered previews, charts, or icons."
            , "- Convert other formats such as SVG or PDF to PNG first."
            , "- The user sees the image; it is not added to your own context."
            ]

-- | Natural-language guidance advertised by MCP servers, appended verbatim
-- under a heading per server.
mcpInstructionsGuidance :: [(Text, Text)] -> Text
mcpInstructionsGuidance [] = ""
mcpInstructionsGuidance instructions =
    Text.intercalate "\n\n" $
        "MCP server instructions:"
            : [ "## " <> name <> "\n" <> body
              | (name, body) <- instructions
              ]

appendMcpInstructions :: [(Text, Text)] -> Text -> Text
appendMcpInstructions [] base = base
appendMcpInstructions instructions base =
    base <> "\n\n" <> mcpInstructionsGuidance instructions

-- | Progressive MCP readiness is timing-dependent: a cold fleet may have no
-- server instructions when the request is built, while a reused fleet may
-- already have all of them. Keep that content in the MCP conversation notice
-- so the request prefix does not change merely because startup was faster.
-- Blocking startup has a complete snapshot and can safely include it.
mcpInstructionsForRequest
    :: Bool
    -> [(Text, Text)]
    -> [(Text, Text)]
mcpInstructionsForRequest progressive instructions
    | progressive = []
    | otherwise = instructions

learnedSkillGuidance :: Set Text -> Text
learnedSkillGuidance available
    | "skill_search" `Set.notMember` available = ""
    | otherwise =
        Text.unlines
            [ "Learned skills:"
            , "- Use skill_search and view_skill when reusable guidance from earlier sessions may apply."
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

ghciGuidanceForTools :: Dialect -> Set Text -> Text
ghciGuidanceForTools dialect available
    | dialectPromptStyle dialect == ClaudeCodePromptStyle = ""
    | "run_ghci" `Set.notMember` available = ""
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
