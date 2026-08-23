-- | Provider-specific system prompt closed over by the transport backend.
module Agent.CLI.Prompt
    ( defaultModelFor
    , systemPrompt
    , systemPromptWithHaskellProgram
    , systemPromptWithToolAccess
    ) where

import Agent.CLI.Timestamp (timeContextGuidance)
import Agent.OsPath (toText)
import Agent.Provider (Provider(..))
import Agent.Tools.Grok.Prompt (codingGrokPromptTools, grokSystemPrompt)
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
systemPrompt :: Provider -> OsPath -> Day -> Bool -> Text
systemPrompt provider cwd today isNonInteractive =
    systemPromptWithHaskellProgram
        True provider cwd today isNonInteractive

systemPromptWithHaskellProgram
    :: Bool
    -> Provider
    -> OsPath
    -> Day
    -> Bool
    -> Text
systemPromptWithHaskellProgram
        haskellProgramEnabled =
    systemPromptWithToolAccess haskellProgramEnabled True

systemPromptWithToolAccess
    :: Bool
    -> Bool
    -> Provider
    -> OsPath
    -> Day
    -> Bool
    -> Text
systemPromptWithToolAccess
        haskellProgramEnabled directShellEnabled provider cwd today isNonInteractive =
    base
        <> "\n\n"
        <> ghciGuidance haskellProgramEnabled directShellEnabled
        <> "\n"
        <> timeContextGuidance
  where
    base = case provider of
        XAIProvider -> grokSystemPrompt codingGrokPromptTools cwd today isNonInteractive
        OpenRouterProvider -> grokSystemPrompt codingGrokPromptTools cwd today isNonInteractive
        OpenAIProvider -> codexSystemPrompt directShellEnabled cwd today

-- | Prefer GHCI as the general-purpose scripting environment.
ghciGuidance :: Bool -> Bool -> Text
ghciGuidance haskellProgramEnabled directShellEnabled =
    Text.unlines
        ( [ "Prefer ghci for scripting."
          , "When you need a short program, calculation, or one-off script, use the run_ghci tool rather than Python, Node, bash, or compiling a binary."
          , "run_ghci keeps a persistent GHCi session: bindings and loaded modules stick across calls."
          ]
            <> (if haskellProgramEnabled
                then
                    [ "For multi-tool orchestration or filtering large intermediate results, use run_haskell_program. Its callTool helper accepts registered nested tools, returns their formatted Text results, and routes nested calls through normal approvals while only the Haskell program's selected output returns to the model."
                    , "For independent nested calls, run_haskell_program preimports Concurrently(..) and runConcurrently. Applicative Concurrently callTool actions overlap only when their registered tools are parallel-safe; stateful tools remain serialized."
                    , "For isolated model calls inside run_haskell_program, prefer callLLMText for assistant text or callLLMJson for typed JSON. Use encodeJsonText instead of manually concatenating or showing final JSON. Use raw callLLM only when the complete Responses value is required. Identical successful callLLM requests are reused when a corrected Haskell program retries during the same parent turn."
                    , "run_haskell_program uses a fresh dedicated GHCi process for each call and is not OS-sandboxed, so the outer program always requires approval."
                    ]
                else [])
            <> (if directShellEnabled
                then
                    [ "Prefer shell tools (run_terminal_cmd or shell_command) for OS commands, package installs, servers, and anything that is not Haskell evaluation."
                    ]
                else
                    [ "Direct shell tools are unavailable to the parent agent. Use callTool from run_haskell_program for OS commands and file reads."
                    , "If a Haskell program fails to compile or run, inspect the returned error, repair the complete program, and call run_haskell_program again. Do not fall back to a direct shell tool."
                    ])
            <> [ "The session enables GHC2021 plus BlockArguments, OverloadedStrings, OverloadedRecordDot, DuplicateRecordFields, NoFieldSelectors, LambdaCase, and RecordWildCards."
               , "Pure expressions do not need user approval; IO and side-effecting GHCi commands do."
               , "Drive GHCi with complete expressions; do not expect interactive human input."
               ]
        )

codexSystemPrompt :: Bool -> OsPath -> Day -> Text
codexSystemPrompt directShellEnabled cwd today =
    Text.unlines
        [ "You are a coding agent working in " <> toText cwd <> "."
        , "Today's date is " <> Text.pack (formatTime defaultTimeLocale "%Y-%m-%d" today) <> "."
        , ""
        , "Use these tools:"
        ]
        <> Text.unlines
            (if directShellEnabled
                then
                    [ "- Inspect files with read_file, grep, and list_dir. Outside Plan Mode, shell_command is also available; always set workdir."
                    ]
                else
                    [ "- Inspect files with read_file, grep, and list_dir. Run OS commands through `callTool \"shell_command\"` inside run_haskell_program. Pass an object with `command` and `workdir`; optional fields are `timeout_ms` or `yield_time_ms`."
                    ])
        <> Text.unlines
        [ "- Edit files with apply_patch. Never call applypatch or apply-patch."
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
