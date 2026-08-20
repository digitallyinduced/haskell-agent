-- | Provider-specific system prompt closed over by the transport backend.
module Agent.CLI.Prompt
    ( defaultModelFor
    , systemPrompt
    ) where

import Agent.CLI.Timestamp (timeContextGuidance)
import Agent.Provider (Provider(..))
import Agent.Tools.Grok.Prompt (codingGrokPromptTools, grokSystemPrompt)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, formatTime)

defaultModelFor :: Provider -> Text
defaultModelFor = \case
    XAIProvider -> "grok-4.5"
    OpenAIProvider -> "gpt-5.6-luna"
    OpenRouterProvider -> "openai/gpt-5.1"

-- | @isNonInteractive@ is True for one-shot @-p@ (no human in the loop).
systemPrompt :: Provider -> FilePath -> Day -> Bool -> Text
systemPrompt provider cwd today isNonInteractive =
    base <> "\n\n" <> ghciGuidance <> "\n" <> timeContextGuidance
  where
    base = case provider of
        XAIProvider -> grokSystemPrompt codingGrokPromptTools cwd today isNonInteractive
        OpenRouterProvider -> grokSystemPrompt codingGrokPromptTools cwd today isNonInteractive
        OpenAIProvider -> codexSystemPrompt cwd today

-- | Prefer GHCI as the general-purpose scripting environment.
ghciGuidance :: Text
ghciGuidance =
    Text.unlines
        [ "Prefer ghci for scripting."
        , "When you need a short program, calculation, or one-off script, use the run_ghci tool rather than Python, Node, bash, or compiling a binary."
        , "run_ghci keeps a persistent GHCi session: bindings and loaded modules stick across calls."
        , "Pure expressions do not need user approval; IO and side-effecting GHCi commands do."
        , "Prefer shell tools (run_terminal_cmd or shell_command) for OS commands, package installs, servers, and anything that is not Haskell evaluation."
        , "Drive GHCi with complete expressions; do not expect interactive human input."
        ]

codexSystemPrompt :: FilePath -> Day -> Text
codexSystemPrompt cwd today =
    Text.unlines
        [ "You are a coding agent working in " <> Text.pack cwd <> "."
        , "Today's date is " <> Text.pack (formatTime defaultTimeLocale "%Y-%m-%d" today) <> "."
        , ""
        , "Use these tools:"
        , "- Inspect the repo with shell_command (rg, cat, ls). Always set workdir."
        , "- Edit files with apply_patch. Never call applypatch or apply-patch."
        , "- Track multi-step work with update_plan (progress checklist; unavailable in Plan Mode)."
        , "- Evaluate Haskell with run_ghci (persistent GHCi; pure expressions auto-approve)."
        , "- Look up current public information with web_search."
        , ""
        , "Plan Mode: when a developer reminder says plan mode is active, explore read-only,"
        , "write the design to plan.md only, and present the final plan in a <proposed_plan>"
        , "block (opening and closing tags on their own lines). Do not implement until plan"
        , "mode ends. update_plan is not Plan Mode."
        , "Be concise. Do not mention tools this session does not register."
        ]
