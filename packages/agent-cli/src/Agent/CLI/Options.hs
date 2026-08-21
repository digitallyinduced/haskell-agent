-- | Command-line flags for @agent-cli@.
module Agent.CLI.Options
    ( ApprovalAnswer(..)
    , ApprovalPolicy(..)
    , CliOptions(..)
    , Command(..)
    , defaultCliOptions
    , defaultEffortFor
    , isOneShot
    , parseApprovalAnswer
    , parseArgs
    , parseEffort
    , reasoningEfforts
    , resolveApprovalPolicy
    , usage
    ) where

import Agent.OsPath (OsPath, fromFilePath)
import Agent.Provider (Provider(..), parseProvider)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text

data Command
    = ShowHelp
    | ShowVersion
    | Login
    | ListSessions
    | ShowSession Text
    | RunAgent CliOptions
    deriving (Eq, Show)

data ApprovalPolicy
    = ApproveAll
    | DenyMutating
    | PromptMutating
    deriving (Eq, Show)

data ApprovalAnswer
    = AllowOnce
    | AllowAlways
    | Deny
    deriving (Eq, Show)

-- | Parse a mutating-tool approval reply. Matching is case-insensitive
-- and ignores surrounding whitespace.
parseApprovalAnswer :: Text -> ApprovalAnswer
parseApprovalAnswer raw = case Text.toLower (Text.strip raw) of
    "y" -> AllowOnce
    "yes" -> AllowOnce
    "a" -> AllowAlways
    "always" -> AllowAlways
    "yolo" -> AllowAlways
    _ -> Deny

data CliOptions = CliOptions
    { optProvider :: !(Maybe Provider)
    , optModel :: !(Maybe Text)
    , optCwd :: !(Maybe OsPath)
    , optWorktree :: !Bool
    , optYolo :: !Bool
    , optNoYolo :: !Bool
    , optMaxTurns :: !Int
    , optEffort :: !(Maybe Text)
      -- ^ 'Nothing' means use 'defaultEffortFor' once the provider is known.
    , optPrompt :: !(Maybe Text)
    , optPromptFile :: !(Maybe OsPath)
    , optResume :: !(Maybe Text)
    , optSaveSession :: !Bool
    , optAgentsMd :: !Bool
      -- ^ Discover and inject AGENTS.md at session start (default: True).
    } deriving (Eq, Show)

defaultCliOptions :: CliOptions
defaultCliOptions = CliOptions
    { optProvider = Nothing
    , optModel = Nothing
    , optCwd = Nothing
    , optWorktree = False
    , optYolo = False
    , optNoYolo = False
    , optMaxTurns = 500
    , optEffort = Nothing
    , optPrompt = Nothing
    , optPromptFile = Nothing
    , optResume = Nothing
    , optSaveSession = False
    , optAgentsMd = True
    }

-- | Provider default when @--effort@ is omitted. Grok runs at high effort.
defaultEffortFor :: Provider -> Text
defaultEffortFor = \case
    XAIProvider -> "high"
    OpenAIProvider -> "medium"
    OpenRouterProvider -> "medium"

isOneShot :: CliOptions -> Bool
isOneShot options = isJust options.optPrompt || isJust options.optPromptFile

-- | One-shot without a TTY auto-approves so scripts do not hang, unless
-- @--no-yolo@ is set. Interactive sessions prompt on mutating tools, unless
-- project settings already enabled auto-approve (and @--no-yolo@ was not set).
resolveApprovalPolicy :: CliOptions -> Bool -> Bool -> ApprovalPolicy
resolveApprovalPolicy options isTty projectAutoApprove
    | options.optYolo && not options.optNoYolo = ApproveAll
    | options.optNoYolo && not isTty = DenyMutating
    | not isTty && isOneShot options = ApproveAll
    | not isTty = DenyMutating
    | options.optNoYolo = PromptMutating
    | projectAutoApprove = ApproveAll
    | otherwise = PromptMutating

parseArgs :: [String] -> Either String Command
parseArgs args
    | any (`elem` ["--help", "-h"]) args = Right ShowHelp
    | "--version" `elem` args = Right ShowVersion
    | take 1 args == ["login"] =
        if length args == 1
            then Right Login
            else Left "usage: agent-cli login"
    | take 1 args == ["sessions"] = parseSessionsCommand (drop 1 args)
    | otherwise = RunAgent <$> parseOptions defaultCliOptions args

parseSessionsCommand :: [String] -> Either String Command
parseSessionsCommand = \case
    [] -> Right ListSessions
    ["list"] -> Right ListSessions
    ["show", sessionId] -> Right (ShowSession (Text.pack sessionId))
    ["show"] -> Left "usage: agent-cli sessions show <session-id>"
    other ->
        Left ("unknown sessions command: " <> unwords other
            <> "\nusage: agent-cli sessions [list|show <id>]")

parseOptions :: CliOptions -> [String] -> Either String CliOptions
parseOptions options = \case
    [] -> validate options
    "-h" : _ -> Left usage
    "--help" : _ -> Left usage
    "--version" : _ -> Left "agent-cli 0.1.0.0"
    "--provider" : value : rest -> do
        provider <- case parseProvider (Text.pack value) of
            Just parsed -> Right parsed
            Nothing -> Left ("unknown provider: " <> value <> " (use openai, xai, or openrouter)")
        parseOptions options { optProvider = Just provider } rest
    "--model" : value : rest ->
        parseOptions options { optModel = Just (Text.pack value) } rest
    "--cwd" : value : rest ->
        parseOptions options { optCwd = Just (fromFilePath value) } rest
    "--worktree" : rest ->
        parseOptions options { optWorktree = True } rest
    "--yolo" : rest ->
        parseOptions options { optYolo = True, optNoYolo = False } rest
    "--no-yolo" : rest ->
        parseOptions options { optNoYolo = True, optYolo = False } rest
    "--max-turns" : value : rest -> do
        turns <- parseInt "--max-turns" value
        parseOptions options { optMaxTurns = turns } rest
    "--effort" : value : rest -> do
        effort <- parseEffort (Text.pack value)
        parseOptions options { optEffort = Just effort } rest
    "-p" : value : rest ->
        parseOptions options { optPrompt = Just (Text.pack value) } rest
    "--prompt" : value : rest ->
        parseOptions options { optPrompt = Just (Text.pack value) } rest
    "--prompt-file" : value : rest ->
        parseOptions options { optPromptFile = Just (fromFilePath value) } rest
    "--resume" : value : rest ->
        parseOptions options { optResume = Just (Text.pack value) } rest
    "--save-session" : rest ->
        parseOptions options { optSaveSession = True } rest
    "--agents-md" : rest ->
        parseOptions options { optAgentsMd = True } rest
    "--no-agents-md" : rest ->
        parseOptions options { optAgentsMd = False } rest
    flag : _
        | flag == "openai-base-url" ->
            Left "openai-base-url was removed; run agent-cli --help"
        | "-" `Text.isPrefixOf` Text.pack flag ->
            Left ("unknown flag: " <> flag <> "\n" <> usage)
        | otherwise ->
            Left ("unexpected argument: " <> flag <> "\n" <> usage)

validate :: CliOptions -> Either String CliOptions
validate options
    | isJust options.optPrompt && isJust options.optPromptFile =
        Left "use either -p/--prompt or --prompt-file, not both"
    | options.optMaxTurns < 1 =
        Left "--max-turns must be at least 1"
    | isJust options.optResume && options.optWorktree =
        Left "use either --resume or --worktree, not both"
    | otherwise = Right options

parseInt :: String -> String -> Either String Int
parseInt flag value = case reads value of
    [(n, "")] | n >= 1 -> Right n
    _ -> Left (flag <> " expects a positive integer, got " <> value)

reasoningEfforts :: [Text]
reasoningEfforts = ["none", "low", "medium", "high", "xhigh", "max"]

parseEffort :: Text -> Either String Text
parseEffort raw =
    let effort = Text.toLower (Text.strip raw)
    in if effort `elem` reasoningEfforts
        then Right effort
        else Left
            ( "effort must be none, low, medium, high, xhigh, or max (got "
                <> Text.unpack effort
                <> ")"
            )

usage :: String
usage = unlines
    [ "Usage: agent-cli [OPTIONS]"
    , "       agent-cli login"
    , "       agent-cli sessions [list]"
    , "       agent-cli sessions show <session-id>"
    , ""
    , "  -p, --prompt TEXT       Run one prompt and exit"
    , "      --prompt-file FILE  Read the one-shot prompt from a file"
    , "      --provider NAME     openai, xai, or openrouter (default: detect from auth)"
    , "      --model NAME        Override the provider default model"
    , "      --cwd DIR           Working directory for tools (default: current)"
    , "      --worktree          Create a new git worktree under ~/.haskell-agent/worktrees"
    , "      --resume ID         Resume a persisted session from ~/.haskell-agent/sessions"
    , "      --save-session      Persist a one-shot (-p) run as a session"
    , "      --agents-md         Discover and inject AGENTS.md (default)"
    , "      --no-agents-md      Skip AGENTS.md discovery"
    , "      --yolo              Auto-approve every tool"
    , "      --no-yolo           Never auto-approve; deny mutating tools without a TTY"
    , "      --max-turns N       Stop after N model turns (default: 500)"
    , "      --effort LEVEL      Reasoning effort: none, low, medium, high, xhigh, max"
    , "                          (default: high for xai/grok, medium otherwise)"
    , "      --version           Print the agent-cli version"
    , "      --help              Show this help"
    , ""
    , "Without -p/--prompt-file, start a REPL. Interactive REPL sessions are"
    , "persisted under ~/.haskell-agent/sessions. /effort [LEVEL] changes"
    , "reasoning effort. /model (alias /m) opens the model picker; /model NAME"
    , "sets it. /help [NAME] lists slash commands. Tab completes / commands."
    , "Shift+Tab cycles idle mode: ask (normal) → plan → always-approve → ask."
    , "/compact [FOCUS] summarizes history (OpenAI remote compact;"
    , "xAI/OpenRouter local summary) to free context."
    , "/plan [description] enters plan mode (read-only except plan.md);"
    , "when a plan is presented, approve (a), request changes (s), or cancel (q)."
    , "/btw <QUESTION> asks a one-shot side question without changing or"
    , "persisting the main conversation."
    , "/always-approve (or :yolo) toggles auto-approve and saves it under"
    , "<project>/.haskell-agent/settings.json. Permission prompts offer Allow once"
    , "or Always this tool this session; /always-approve still enables project yolo."
    , "/resume [ID] resumes a persisted session (TTY: two-pane picker)."
    , "/paste [TEXT] attaches a clipboard image to the next"
    , "message and draws an in-terminal preview (Kitty/Ghostty/WezTerm/iTerm2);"
    , "/paste --send [TEXT] sends immediately. Cmd+V of a Finder image path also"
    , "attaches and previews. /attachments lists queued images; /clear-attachments"
    , "drops them. Linux uses wl-paste/xclip."
    , "with an optional caption. /session prints the current session id."
    , "/clear resets the live conversation; /new starts a fresh session id."
    , "/reload-auth forces a re-read of xAI/OpenRouter credentials;"
    , "auth failures also reload once and retry automatically."
    , "Ctrl-D or :q exits."
    , "Ctrl-C cancels the current turn (or warns at the idle prompt);"
    , "a second Ctrl-C exits and prints a --resume command when a"
    , "session has been persisted."
    , "Under `repl` (nix develop), first open passes --worktree unless the"
    , "cwd is already under ~/.haskell-agent/worktrees. :reload returns to"
    , "GHCi, reloads modules, and resumes the same session."

    ]
