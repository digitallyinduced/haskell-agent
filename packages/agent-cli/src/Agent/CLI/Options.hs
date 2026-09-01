-- | Command-line flags for @agent-cli@.
module Agent.CLI.Options
    ( ApprovalAnswer(..)
    , ApprovalPolicy(..)
    , CliOptions(..)
    , Command(..)
    , GatewayCommand(..)
    , McpCommand(..)
    , ScreenMode(..)
    , SessionOutputFormat(..)
    , SessionPageRequest(..)
    , StorageCommand(..)
    , defaultCliOptions
    , defaultEffortFor
    , freshSessionOptions
    , isOneShot
    , parseApprovalAnswer
    , parseArgs
    , parseEffort
    , normalizeReasoningEffortForDialect
    , reasoningEfforts
    , reasoningEffortsForDialect
    , resolveApprovalPolicy
    , usage
    ) where

import System.OsPath (OsPath, unsafeEncodeUtf)
import Agent.Dialect (DialectId(..))
import Agent.Loop (defaultLoopMaxTurns)
import Agent.Provider (Provider(..), parseProvider)
import Agent.ReasoningEffort
    ( ReasoningEffort(..)
    , parseReasoningEffort
    )
import qualified Agent.ReasoningEffort as ReasoningEffort
import Agent.CLI.Runtime.Options
    ( ApprovalPolicy(..)
    , GatewayCommand(..)
    , defaultEffortFor
    )
import Agent.TUI.Motion (MotionMode(..))
import Data.Foldable (asum)
import Data.Int (Int64)
import qualified Data.List as List
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Options.Applicative as Options

data Command
    = ShowHelp
    | ShowVersion
    | Login
    | Gateway GatewayCommand
    | Mcp McpCommand
    | ListSessions SessionOutputFormat
    | ShowSession Text SessionOutputFormat (Maybe SessionPageRequest)
    | WaitSession Text
    | ImportSession (Maybe OsPath)
    | Storage StorageCommand
    | RunAgent CliOptions
    deriving (Eq, Show)

data SessionPageRequest
    = SessionRecent !Int
    | SessionBefore !Int64 !Int
    deriving (Eq, Show)

-- | Output intended either for people at a terminal or for native clients.
data SessionOutputFormat
    = SessionHuman
    | SessionJSON
    deriving (Eq, Show)

data McpCommand
    = McpLogin Text [Text]
    | McpLogout Text
    deriving (Eq, Show)

-- | Administrative commands for the harness-managed PostgreSQL server.
data StorageCommand
    = StorageStatus
    | StorageStart
    | StorageStop
    | StorageMigrate
    | StorageDoctor
    deriving (Eq, Show)

data ScreenMode
    = ScreenAuto
    | ScreenFullscreen
    | ScreenMinimal
    deriving (Eq, Show)

data ApprovalAnswer
    = AllowOnce
    | AllowAlways
    | AllowAll
    | Deny
    deriving (Eq, Show)

-- | Parse a mutating-tool approval reply. Matching is case-insensitive
-- and ignores surrounding whitespace, except uppercase @A@ is the short
-- spelling for project-wide auto-approval while lowercase @a@ remembers only
-- the current tool for this session.
parseApprovalAnswer :: Text -> ApprovalAnswer
parseApprovalAnswer raw
    | Text.strip raw == "A" = AllowAll
    | otherwise = case Text.toLower (Text.strip raw) of
        "y" -> AllowOnce
        "yes" -> AllowOnce
        "a" -> AllowAlways
        "always" -> AllowAlways
        "all" -> AllowAll
        "yolo" -> AllowAll
        _ -> Deny

data CliOptions = CliOptions
    { optProvider :: !(Maybe Provider)
    , optModel :: !(Maybe Text)
    , optCwd :: !(Maybe OsPath)
    , optWorktree :: !Bool
    , optYolo :: !Bool
    , optNoYolo :: !Bool
    , optManagedDenyMutations :: !Bool
    , optMaxTurns :: !Int
    , optMaxConcurrentAgents :: !(Maybe Int)
      -- ^ Concurrent subagent cap. 'Nothing' uses project, then harness, then
      -- 'defaultMaxConcurrent'.
    , optCompactThreshold :: !(Maybe Int)
      -- ^ OpenAI automatic-compaction threshold in estimated context tokens.
    , optEffort :: !(Maybe ReasoningEffort)
      -- ^ 'Nothing' means use 'defaultEffortFor' once the provider is known.
    , optShowRawReasoning :: !Bool
      -- ^ Show raw OpenAI reasoning text instead of summaries only.
    , optPrompt :: !(Maybe Text)
    , optPromptFile :: !(Maybe OsPath)
    , optManagedTurnFile :: !(Maybe OsPath)
    , optResume :: !(Maybe Text)
    , optSaveSession :: !Bool
    , optAgentsMd :: !Bool
      -- ^ Discover and inject AGENTS.md at session start (default: True).
    , optSkills :: !Bool
      -- ^ Discover and expose filesystem skills (default: True).
    , optGhci :: !Bool
      -- ^ Expose the persistent run_ghci tool (default: False).
    , optBash :: !Bool
      -- ^ Expose the provider's explicit shell execution tool (default: True).
    , optComputerUse :: !Bool
      -- ^ Allow the model to control the local macOS desktop (default: False).
    , optCodeMode :: !Bool
      -- ^ Honor catalog-selected JavaScript code mode (default: False).
    , optScreenMode :: !ScreenMode
    , optMotionMode :: !MotionMode
    } deriving (Eq, Show)

defaultCliOptions :: CliOptions
defaultCliOptions = CliOptions
    { optProvider = Nothing
    , optModel = Nothing
    , optCwd = Nothing
    , optWorktree = False
    , optYolo = False
    , optNoYolo = False
    , optManagedDenyMutations = False
    , optMaxTurns = defaultLoopMaxTurns
    , optMaxConcurrentAgents = Nothing
    , optCompactThreshold = Nothing
    , optEffort = Nothing
    , optShowRawReasoning = False
    , optPrompt = Nothing
    , optPromptFile = Nothing
    , optManagedTurnFile = Nothing
    , optResume = Nothing
    , optSaveSession = False
    , optAgentsMd = True
    , optSkills = True
    , optGhci = False
    , optBash = True
    , optComputerUse = False
    , optCodeMode = False
    , optScreenMode = ScreenAuto
    , optMotionMode = MotionFull
    }

-- | Drop provider-visible routing and conversation inputs when a gateway
-- credential change creates a new trust boundary.
freshSessionOptions :: CliOptions -> OsPath -> CliOptions
freshSessionOptions options cwd =
    options
        { optProvider = Nothing
        , optModel = Nothing
        , optCwd = Just cwd
        , optWorktree = False
        , optEffort = Nothing
        , optPrompt = Nothing
        , optPromptFile = Nothing
        , optManagedTurnFile = Nothing
        , optResume = Nothing
        }

isOneShot :: CliOptions -> Bool
isOneShot options =
    isJust options.optPrompt
        || isJust options.optPromptFile
        || isJust options.optManagedTurnFile

-- | One-shot without a TTY auto-approves so scripts do not hang, unless
-- @--no-yolo@ is set. Interactive sessions prompt on mutating tools, unless
-- project settings already enabled auto-approve (and @--no-yolo@ was not set).
resolveApprovalPolicy :: CliOptions -> Bool -> Bool -> ApprovalPolicy
resolveApprovalPolicy options isTty projectAutoApprove
    | options.optYolo && not options.optNoYolo = ApproveAll
    | options.optManagedDenyMutations = DenyMutating
    | isJust options.optManagedTurnFile && options.optNoYolo =
        PromptMutating
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
    | isRunInvocation args
    , "openai-base-url" `elem` args =
        Left "openai-base-url was removed; run agent-cli --help"
    | otherwise =
        case Options.execParserPure parserPreferences commandParserInfo args of
            Options.Success command -> validateCommand command
            Options.Failure failure ->
                Left (fst (Options.renderFailure failure "agent-cli"))
            Options.CompletionInvoked _ -> Right ShowHelp

isRunInvocation :: [String] -> Bool
isRunInvocation = \case
    command : _ ->
        command `notElem` ["gateway", "login", "mcp", "sessions", "storage"]
    [] -> True

parserPreferences :: Options.ParserPrefs
parserPreferences =
    Options.prefs Options.showHelpOnError

commandParserInfo :: Options.ParserInfo Command
commandParserInfo =
    Options.info commandParser
        ( Options.fullDesc
            <> Options.progDesc
                "Run an agent or administer persisted sessions and storage"
        )

sessionShowParser :: Options.Parser Command
sessionShowParser =
    makeSessionShow
        <$> (Text.pack
            <$> Options.argument Options.str
                (Options.metavar "SESSION_ID"))
        <*> sessionOutputFormatParser
        <*> Options.optional
            (Options.option sessionCursorReader
                ( Options.long "before"
                    <> Options.metavar "TURN_INDEX"
                    <> Options.help "Load turns older than this cursor"
                ))
        <*> Options.optional
            (Options.option (positiveIntReader "--limit")
                ( Options.long "limit"
                    <> Options.metavar "N"
                    <> Options.help "Bound JSON transcript turns (maximum 500)"
                ))
  where
    makeSessionShow sessionId outputFormat before limit =
        ShowSession sessionId outputFormat $
            case (before, limit) of
                (Nothing, Nothing) -> Nothing
                (Nothing, Just value) -> Just (SessionRecent value)
                (Just cursor, maybeLimit) ->
                    Just (SessionBefore cursor (fromMaybe 50 maybeLimit))

commandParser :: Options.Parser Command
commandParser =
    Options.hsubparser
        ( Options.command "login"
            (Options.info (pure Login)
                (Options.progDesc "Manage provider credentials"))
            <> Options.command "gateway"
                (Options.info gatewayParser
                    (Options.progDesc "Connect this agent to an LLM gateway"))
            <> Options.command "sessions"
                (Options.info sessionsParser
                    (Options.progDesc "Administer persisted sessions"))
            <> Options.command "mcp"
                (Options.info mcpParser
                    (Options.progDesc "Authorize remote MCP servers"))
            <> Options.command "storage"
                (Options.info storageParser
                    (Options.progDesc "Administer managed PostgreSQL storage"))
        )
        Options.<|> (RunAgent <$> runOptionsParser)

gatewayParser :: Options.Parser Command
gatewayParser = Gateway <$> Options.hsubparser
    ( Options.command "connect"
        (Options.info
            (GatewayConnect . Text.pack
                <$> Options.strOption
                    ( Options.long "url"
                        <> Options.metavar "HTTPS_URL"
                        <> Options.help "Gateway base URL"
                    ))
            (Options.progDesc "Authorize this installation with a gateway user"))
    <> Options.command "status"
        (Options.info (pure GatewayStatus)
            (Options.progDesc "Show the connected gateway"))
    <> Options.command "disconnect"
        (Options.info (pure GatewayDisconnect)
            (Options.progDesc "Remove the saved gateway credential"))
    )

sessionsParser :: Options.Parser Command
sessionsParser =
    maybe (ListSessions SessionHuman) id
        <$> Options.optional
            (Options.hsubparser
                ( Options.command "list"
                    (Options.info
                        (ListSessions <$> sessionOutputFormatParser)
                        (Options.progDesc "List persisted sessions"))
                    <> Options.command "show"
                        (Options.info sessionShowParser
                            (Options.progDesc "Show a persisted session"))
                    <> Options.command "wait"
                        (Options.info
                            (WaitSession . Text.pack
                                <$> Options.argument Options.str
                                    (Options.metavar "SESSION_ID"))
                            (Options.progDesc "Wait for a managed session"))
                    <> Options.command "import"
                        (Options.info importSessionParser
                            (Options.progDesc
                                "Import a session from the current process"))
                )
            )

sessionOutputFormatParser :: Options.Parser SessionOutputFormat
sessionOutputFormatParser =
    Options.flag SessionHuman SessionJSON
        ( Options.long "json"
            <> Options.help "Emit stable machine-readable JSON"
        )

importSessionParser :: Options.Parser Command
importSessionParser =
    ImportSession
        <$> Options.optional
            (unsafeEncodeUtf
                <$> Options.strOption
                    ( Options.long "cwd"
                        <> Options.metavar "DIR"
                        <> Options.help "Working directory recorded for the import"
                    ))

storageParser :: Options.Parser Command
storageParser =
    Options.hsubparser
        ( storageCommand "status" StorageStatus "Show storage status"
            <> storageCommand "start" StorageStart "Start managed PostgreSQL"
            <> storageCommand "stop" StorageStop "Stop managed PostgreSQL"
            <> storageCommand "migrate" StorageMigrate "Apply storage migrations"
            <> storageCommand "doctor" StorageDoctor "Check storage health"
        )
  where
    storageCommand name command description =
        Options.command name
            (Options.info (pure (Storage command))
                (Options.progDesc description))

mcpParser :: Options.Parser Command
mcpParser = Mcp <$> Options.hsubparser
    ( Options.command "login"
        (Options.info
            (McpLogin
                <$> (Text.pack <$> Options.argument Options.str (Options.metavar "URL"))
                <*> Options.many
                    (Text.pack
                        <$> Options.strOption
                            (Options.long "scope"
                                <> Options.metavar "SCOPE"
                                <> Options.help "Additional OAuth scope to request (repeatable; unioned with granted scopes)")))
            (Options.progDesc "Authorize an MCP server with OAuth PKCE"))
    <> Options.command "logout"
        (Options.info
            (McpLogout . Text.pack <$> Options.argument Options.str (Options.metavar "URL"))
            (Options.progDesc "Remove saved MCP OAuth credentials"))
    )

type OptionUpdate = CliOptions -> CliOptions

runOptionsParser :: Options.Parser CliOptions
runOptionsParser =
    List.foldl' (\options update -> update options) defaultCliOptions
        <$> Options.many optionUpdateParser

optionUpdateParser :: Options.Parser OptionUpdate
optionUpdateParser = asum
    [ optionUpdate "provider" "NAME"
        "Provider: openai, xai, openrouter, gemini, or claude-code"
        providerReader (\value options -> options { optProvider = Just value })
    , optionUpdate "model" "NAME" "Override the saved last model"
        textReader (\value options -> options { optModel = Just value })
    , optionUpdate "cwd" "DIR" "Working directory for tools"
        pathReader (\value options -> options { optCwd = Just value })
    , flagUpdate "worktree" "Create a new git worktree"
        (\options -> options { optWorktree = True })
    , flagUpdate "yolo" "Auto-approve every tool"
        (\options -> options { optYolo = True, optNoYolo = False })
    , flagUpdate "no-yolo" "Deny mutating tools without a TTY"
        (\options -> options { optNoYolo = True, optYolo = False })
    , flagUpdate "managed-deny-mutations" "Deny mutations in a managed turn"
        (\options -> options
            { optManagedDenyMutations = True
            , optNoYolo = True
            , optYolo = False
            })
    , optionUpdate "max-turns" "N" "Stop after N model turns"
        (positiveIntReader "--max-turns")
        (\value options -> options { optMaxTurns = value })
    , optionUpdate "max-concurrent-agents" "N"
        "Concurrent subagent cap"
        (positiveIntReader "--max-concurrent-agents")
        (\value options -> options { optMaxConcurrentAgents = Just value })
    , optionUpdate "compact-threshold" "N"
        "Automatic compaction threshold in tokens"
        (positiveIntReader "--compact-threshold")
        (\value options -> options { optCompactThreshold = Just value })
    , optionUpdate "effort" "LEVEL" "Reasoning effort"
        effortReader (\value options -> options { optEffort = Just value })
    , flagUpdate "show-raw-reasoning" "Show raw OpenAI reasoning"
        (\options -> options { optShowRawReasoning = True })
    , (\value options -> options { optPrompt = Just value })
        <$> Options.option textReader
            ( Options.short 'p'
                <> Options.long "prompt"
                <> Options.metavar "TEXT"
                <> Options.help "Run one prompt and exit"
            )
    , optionUpdate "prompt-file" "FILE"
        "Read the one-shot prompt from a file"
        pathReader (\value options -> options { optPromptFile = Just value })
    , optionUpdate "managed-turn-file" "FILE"
        "Read an internal managed-turn request"
        pathReader
        (\value options -> options { optManagedTurnFile = Just value })
    , optionUpdate "resume" "ID" "Resume a persisted session"
        textReader (\value options -> options { optResume = Just value })
    , flagUpdate "save-session" "Persist a one-shot run as a session"
        (\options -> options { optSaveSession = True })
    , boolFlagUpdate "agents-md" True "Discover and inject AGENTS.md"
        (\value options -> options { optAgentsMd = value })
    , boolFlagUpdate "no-agents-md" False "Skip AGENTS.md discovery"
        (\value options -> options { optAgentsMd = value })
    , boolFlagUpdate "skills" True "Discover filesystem skills"
        (\value options -> options { optSkills = value })
    , boolFlagUpdate "no-skills" False "Disable skill discovery"
        (\value options -> options { optSkills = value })
    , boolFlagUpdate "ghci" True "Enable the persistent GHCi tool"
        (\value options -> options { optGhci = value })
    , boolFlagUpdate "no-ghci" False "Disable the persistent GHCi tool"
        (\value options -> options { optGhci = value })
    , boolFlagUpdate "bash" True "Enable shell execution tools"
        (\value options -> options { optBash = value })
    , boolFlagUpdate "no-bash" False "Disable shell execution tools"
        (\value options -> options { optBash = value })
    , boolFlagUpdate "computer-use" True "Enable local macOS computer use"
        (\value options -> options { optComputerUse = value })
    , boolFlagUpdate "no-computer-use" False "Disable local computer use"
        (\value options -> options { optComputerUse = value })
    , boolFlagUpdate "code-mode" True "Enable catalog-selected code mode"
        (\value options -> options { optCodeMode = value })
    , boolFlagUpdate "no-code-mode" False "Use conventional tool calling"
        (\value options -> options { optCodeMode = value })
    , screenFlagUpdate "fullscreen" ScreenFullscreen
        "Use the retained full-screen TUI"
    , screenFlagUpdate "minimal" ScreenMinimal
        "Use terminal-native append-only rendering"
    , optionUpdate "motion" "MODE" "Animation policy: full, reduced, or off"
        motionReader (\value options -> options { optMotionMode = value })
    ]

optionUpdate
    :: String
    -> String
    -> String
    -> Options.ReadM value
    -> (value -> OptionUpdate)
    -> Options.Parser OptionUpdate
optionUpdate name metavar description reader update =
    update
        <$> Options.option reader
            ( Options.long name
                <> Options.metavar metavar
                <> Options.help description
            )

flagUpdate
    :: String
    -> String
    -> OptionUpdate
    -> Options.Parser OptionUpdate
flagUpdate name description update =
    Options.flag' update
        (Options.long name <> Options.help description)

boolFlagUpdate
    :: String
    -> Bool
    -> String
    -> (Bool -> OptionUpdate)
    -> Options.Parser OptionUpdate
boolFlagUpdate name value description update =
    flagUpdate name description (update value)

screenFlagUpdate
    :: String
    -> ScreenMode
    -> String
    -> Options.Parser OptionUpdate
screenFlagUpdate name value description =
    flagUpdate name description
        (\options -> options { optScreenMode = value })

textReader :: Options.ReadM Text
textReader = Text.pack <$> Options.str

pathReader :: Options.ReadM OsPath
pathReader = unsafeEncodeUtf <$> Options.str

providerReader :: Options.ReadM Provider
providerReader = Options.eitherReader \value ->
    case parseProvider (Text.pack value) of
        Just provider -> Right provider
        Nothing -> Left
            ("unknown provider: " <> value
                <> " (use openai, xai, openrouter, gemini, or claude-code)")

positiveIntReader :: String -> Options.ReadM Int
positiveIntReader flag =
    Options.eitherReader (parseInt flag)

sessionCursorReader :: Options.ReadM Int64
sessionCursorReader =
    Options.eitherReader \value ->
        case reads value of
            [(cursor, "")] | cursor >= 0 -> Right cursor
            _ ->
                Left
                    ("--before expects a non-negative integer, got "
                        <> value)

effortReader :: Options.ReadM ReasoningEffort
effortReader =
    Options.eitherReader (parseEffort . Text.pack)

motionReader :: Options.ReadM MotionMode
motionReader =
    Options.eitherReader parseMotionMode

validateCommand :: Command -> Either String Command
validateCommand = \case
    RunAgent options -> RunAgent <$> validate options
    ShowSession _ SessionHuman (Just _) ->
        Left "session pagination requires --json"
    command@(ShowSession _ SessionJSON (Just page))
        | sessionPageLimit page > 500 ->
            Left "--limit must not exceed 500"
        | otherwise -> Right command
    command -> Right command
  where
    sessionPageLimit = \case
        SessionRecent limit -> limit
        SessionBefore _ limit -> limit

validate :: CliOptions -> Either String CliOptions
validate options
    | length
        (filter id
            [ isJust options.optPrompt
            , isJust options.optPromptFile
            , isJust options.optManagedTurnFile
            ]) > 1 =
        Left "use only one of -p/--prompt, --prompt-file, or --managed-turn-file"
    | options.optMaxTurns < 1 =
        Left "--max-turns must be at least 1"
    | maybe False (< 1) options.optMaxConcurrentAgents =
        Left "--max-concurrent-agents must be at least 1"
    | isJust options.optResume && options.optWorktree =
        Left "use either --resume or --worktree, not both"
    | otherwise = Right options

parseInt :: String -> String -> Either String Int
parseInt flag value = case reads value of
    [(n, "")] | n >= 1 -> Right n
    _ -> Left (flag <> " expects a positive integer, got " <> value)

parseMotionMode :: String -> Either String MotionMode
parseMotionMode raw = case Text.toLower (Text.pack raw) of
    "full" -> Right MotionFull
    "reduced" -> Right MotionReduced
    "off" -> Right MotionOff
    _ -> Left ("--motion expects full, reduced, or off (got " <> raw <> ")")

reasoningEfforts :: [ReasoningEffort]
reasoningEfforts = ReasoningEffort.reasoningEfforts

-- | Efforts exposed by the active model-facing protocol. Grok accepts
-- @xhigh@ but rejects the OpenAI-only @max@ value.
reasoningEffortsForDialect :: DialectId -> [ReasoningEffort]
reasoningEffortsForDialect = \case
    GrokBuildDialect -> filter (/= EffortMax) reasoningEfforts
    _ -> reasoningEfforts

-- | Replace an effort unsupported by the active model-facing protocol with
-- its closest supported value. This also cleans up resumed sessions and
-- provider switches that inherited an effort from another dialect.
normalizeReasoningEffortForDialect
    :: DialectId
    -> ReasoningEffort
    -> ReasoningEffort
normalizeReasoningEffortForDialect dialect effort
    | effort `elem` reasoningEffortsForDialect dialect = effort
    | dialect == GrokBuildDialect = EffortHigh
    | otherwise = effort

parseEffort :: Text -> Either String ReasoningEffort
parseEffort = either (Left . Text.unpack) Right . parseReasoningEffort

usage :: String
usage = unlines
    [ "Usage: agent-cli [OPTIONS]"
    , "       agent-cli login"
    , "       agent-cli gateway connect --url <https-url>"
    , "       agent-cli gateway <status|disconnect>"
    , "       agent-cli sessions [list]"
    , "       agent-cli sessions show <session-id>"
    , "       agent-cli mcp login <url> [--scope SCOPE]..."
    , "       agent-cli mcp logout <url>"
    , "       agent-cli storage <status|start|stop|migrate|doctor>"
    , ""
    , "  -p, --prompt TEXT       Run one prompt and exit"
    , "      --prompt-file FILE  Read the one-shot prompt from a file"
    , "      --provider NAME     openai, xai, openrouter, gemini, or claude-code"
    , "                          (default: detect from API/OAuth auth)"
    , "      --model NAME        Override the saved last model"
    , "      --cwd DIR           Working directory for tools (default: current)"
    , "      --worktree          Create a new git worktree under ~/.haskell-agent/worktrees"
    , "      --resume ID         Resume a persisted session from ~/.haskell-agent/sessions"
    , "      --save-session      Persist a one-shot (-p) run as a session"
    , "      --limit N           Bound JSON session transcript turns (max 500)"
    , "      --before INDEX      Load JSON turns older than a turn cursor"
    , "      --agents-md         Discover and inject AGENTS.md (default)"
    , "      --no-agents-md      Skip AGENTS.md discovery"
    , "      --skills            Discover Agent Skills (default)"
    , "      --no-skills         Disable skill discovery and invocation"
    , "      --ghci              Enable the persistent GHCi tool"
    , "      --no-ghci           Disable the persistent GHCi tool (default)"
    , "      --bash              Enable explicit shell execution tools (default)"
    , "      --no-bash           Disable explicit shell execution tools"
    , "      --computer-use      Enable local macOS desktop control (opt-in)"
    , "      --no-computer-use   Disable local desktop control (default)"
    , "      --fullscreen        Use the retained full-screen TUI"
    , "      --minimal           Use terminal-native append-only rendering"
    , "      --motion MODE       Animation policy: full, reduced, or off"
    , "      --yolo              Auto-approve every tool"
    , "      --no-yolo           Never auto-approve; deny mutating tools without a TTY"
    , "      --max-turns N       Stop after N model turns (default: "
        <> show defaultLoopMaxTurns <> ")"
    , "      --max-concurrent-agents N"
    , "                          Concurrent subagent cap (default: 32;"
    , "                          project settings, then ~/.haskell-agent/config.json)"
    , "      --compact-threshold N"
    , "                          Automatic compaction threshold in tokens"
    , "                          (default: provider/model-specific)"
    , "      --effort LEVEL      Reasoning effort: none, low, medium, high, xhigh, max"
    , "                          (default: xhigh for Claude Code, high for xai/grok,"
    , "                          medium otherwise)"
    , "      --show-raw-reasoning"
    , "                          Show raw OpenAI reasoning (default: summaries only)"
    , "      --version           Print the agent-cli version"
    , "      --help              Show this help"
    , ""
    , "Without -p/--prompt-file, start a REPL. Interactive REPL sessions are"
    , "persisted under ~/.haskell-agent/sessions. /effort [LEVEL] changes"
    , "reasoning effort. /model (alias /m) opens the model picker; /model NAME"
    , "sets it. /help [NAME] lists slash commands. Tab completes / commands."
    , "Shift+Tab cycles idle mode: ask (normal) → plan → always-approve → ask."
    , "In fullscreen mode the composer stays editable during a turn; Enter steers"
    , "the active turn at its next model boundary without cancelling current work."
    , "Ctrl+Enter sends the current draft next by cancelling the active turn;"
    , "Ctrl+O is the fallback when the terminal cannot distinguish Ctrl+Enter."
    , "With an empty composer, send-now promotes the oldest queued prompt."
    , "/compact [FOCUS] summarizes history (OpenAI remote compact;"
    , "Claude Code/xAI/OpenRouter/Gemini local summary) to free context."
    , "/plan [description] enters plan mode (read-only except plan.md);"
    , "when a plan is presented, approve (a), request changes (s), or cancel (q)."
    , "/btw <QUESTION> asks a one-shot side question without changing or"
    , "persisting the main conversation."
    , "/skills lists discovered SKILL.md workflows; /skills reload rescans."
    , "Invoke one with /NAME [ARGS] or mention it as $NAME in a prompt."
    , "/agents opens the agent viewport; /agents limit [N] shows or sets"
    , "the live concurrent subagent cap and saves it to project settings."
    , "/shell shows the active shell tools; /shell ghci or /shell bash switches"
    , "the current session. /shell both and /shell none are also available."
    , "/always-approve (or :yolo) toggles auto-approve and saves it under"
    , "<project>/.haskell-agent/settings.json. Permission prompts offer Allow once"
    , "or Always this tool this session; /always-approve still enables project yolo."
    , "/resume [ID] resumes a persisted session (TTY: two-pane picker)."
    , "/desktop opens the current persisted conversation in the macOS app."
    , "/paste [TEXT] attaches a clipboard image to the next"
    , "message and draws an in-terminal preview (Kitty/Ghostty/WezTerm/iTerm2);"
    , "/paste --send [TEXT] sends immediately. Cmd+V of a Finder image path also"
    , "attaches and previews. /attachments lists queued images; /clear-attachments"
    , "drops them. Linux uses wl-paste/xclip."
    , "with an optional caption. /session prints the current session id."
    , "/clear resets the live conversation; /new starts a fresh session id."
    , "/reload-auth forces a re-read of xAI/OpenRouter/Gemini credentials;"
    , "auth failures also reload once and retry automatically."
    , "Ctrl-D or :q exits. Graceful exits print a --resume command when a"
    , "session has been persisted."
    , "Ctrl-C cancels the current turn (or warns at the idle prompt);"
    , "a second Ctrl-C exits."
    , "Under `repl` (nix develop), first open passes --worktree unless the"
    , "cwd is already under ~/.haskell-agent/worktrees. :reload returns to"
    , "GHCi, reloads modules, and resumes the same session."

    ]
