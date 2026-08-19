-- | Provider-specific system prompt closed over by the transport backend.
module Agent.CLI.Prompt
    ( defaultModelFor
    , systemPrompt
    ) where

import Agent.Provider (Provider(..))
import Agent.Tools.Grok.Prompt (codingGrokPromptTools, grokSystemPrompt)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, formatTime)

defaultModelFor :: Provider -> Text
defaultModelFor = \case
    XAIProvider -> "grok-4.5"
    OpenAIProvider -> "gpt-5.1-codex"
    OpenRouterProvider -> "openai/gpt-5.1"

-- | @isNonInteractive@ is True for one-shot @-p@ (no human in the loop).
systemPrompt :: Provider -> FilePath -> Day -> Bool -> Text
systemPrompt provider cwd today isNonInteractive = case provider of
    XAIProvider -> grokSystemPrompt codingGrokPromptTools cwd today isNonInteractive
    OpenRouterProvider -> grokSystemPrompt codingGrokPromptTools cwd today isNonInteractive
    OpenAIProvider -> codexSystemPrompt cwd today

codexSystemPrompt :: FilePath -> Day -> Text
codexSystemPrompt cwd today =
    Text.unlines
        [ "You are a coding agent working in " <> Text.pack cwd <> "."
        , "Today's date is " <> Text.pack (formatTime defaultTimeLocale "%Y-%m-%d" today) <> "."
        , ""
        , "Use Codex tools only:"
        , "- Inspect the repo with shell_command (rg, cat, ls). Always set workdir."
        , "- Edit files with apply_patch. Never call applypatch or apply-patch."
        , "- Track multi-step work with update_plan."
        , "Be concise. Do not mention tools this session does not register."
        ]
