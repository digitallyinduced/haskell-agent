-- | Short, provider-specific system prompt closed over by the transport backend.
module Agent.CLI.Prompt
    ( defaultModelFor
    , systemPrompt
    ) where

import Agent.Provider (Provider(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, formatTime)

defaultModelFor :: Provider -> Text
defaultModelFor = \case
    XAIProvider -> "grok-4.5"
    OpenAIProvider -> "gpt-5.1-codex"

systemPrompt :: Provider -> FilePath -> Day -> Text
systemPrompt provider cwd today =
    Text.unlines
        [ "You are a coding agent working in " <> Text.pack cwd <> "."
        , "Today's date is " <> Text.pack (formatTime defaultTimeLocale "%Y-%m-%d" today) <> "."
        , ""
        , toolRules provider
        , "Be concise. Do not mention tools this session does not register."
        ]

toolRules :: Provider -> Text
toolRules = \case
    XAIProvider ->
        "Use grok-build tools only:\n\
        \- Prefer read_file, grep, and list_dir before editing.\n\
        \- Edit with search_replace. An empty old_string creates a new file.\n\
        \- Run commands with run_terminal_cmd."
    OpenAIProvider ->
        "Use Codex tools only:\n\
        \- Inspect the repo with shell_command (rg, cat, ls). Always set workdir.\n\
        \- Edit files with apply_patch. Never call applypatch or apply-patch.\n\
        \- Track multi-step work with update_plan."
