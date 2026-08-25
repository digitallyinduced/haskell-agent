-- | Interactive REPL slash commands.
module Agent.CLI.Command
    ( ReplAction(..)
    , ShellMode(..)
    , SkillCommand(..)
    , SlashCatalog(..)
    , SlashCommand(..)
    , SlashMenu(..)
    , SlashSuggestion(..)
    , currentEffort
    , currentModel
    , deepResearchInstruction
    , defaultSlashCatalog
    , formatSlashHelp
    , formatSlashHelpWithCatalog
    , formatSlashHelpWithSkills
    , goalInstruction
    , lookupSlashCommand
    , lookupSlashCommandIn
    , loopScheduleInstruction
    , mkSlashCatalog
    , parseReplLine
    , parseReplLineWithCatalog
    , parseReplLineWithSkills
    , setModel
    , setReasoningEffort
    , slashCommands
    , slashCompletionCandidates
    , slashCompletionCandidatesWithCatalog
    , slashCompletionCandidatesWithModels
    , slashCompletionCandidatesWithSkills
    , slashCompletionCandidatesWithSkillsAndModels
    , slashMenuFor
    , slashMenuForCatalog
    , slashMenuForWithModels
    , slashMenuForWithSkills
    , slashMenuForWithSkillsAndModels
    , workflowInstruction
    ) where

import Agent.CLI.Options (parseEffort, reasoningEfforts)
import Agent.CLI.Style (roleMuted, rolePrompt)
import Agent.Dialect (DialectId(..))
import Agent.Responses.Types

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson ((.=))
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Char (isAlphaNum, isDigit, isSpace)
import Data.List (find, isPrefixOf, sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (Down(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)

data ReplAction
    = ReplQuit
    | ReplReload
    | ReplPrompt Text
    | ReplExpandedPrompt !Text !Text
      -- ^ Original user-visible text and the model-visible expansion.
    | ReplShowEffort
    | ReplSetEffort Text
    | ReplShowModel
    | ReplSetModel Text
    | ReplToggleAlwaysApprove
    | ReplPlan (Maybe Text)
    -- ^ Enter plan mode. @Just@ starts a turn with that description.
    | ReplBtw Text
    -- ^ Ask an isolated one-shot question over the current context.
    | ReplRecap
    -- ^ Generate a display-only "where was I" recap of the current session.
    | ReplShowSession
    | ReplShowSessionInfo
    | ReplAfk (Maybe Text)
    -- ^ Hand the active session to tmux, optionally on @host:path@.
    | ReplWorktree
    | ReplRename Text
    | ReplRenameAuto
    | ReplLogin
    | ReplReloadAuth
    | ReplPaste !Bool !Text
    | ReplClearAttachments
    | ReplShowAttachments
    | ReplCopyLast
    | ReplCopyCode Int
    | ReplCopyDiff
    | ReplCopyPath
    | ReplCopySession
    | ReplShowTerminal
    | ReplAgents
    | ReplMcp
    | ReplGoalStatus
    | ReplGoalPause
    | ReplGoalResume
    | ReplGoalClear
    | ReplGoalSet !Text !Text !(Maybe Int) !Text
      -- ^ Original text, objective, optional token budget, and expansion.
    | ReplWorkflowRuns
    | ReplWorkflowManage !Text !(Maybe Text)
      -- ^ Operation and optional run id/display name.
    | ReplSkills !Bool
    | ReplShowShell
    | ReplSetShell !ShellMode
    | ReplInvokeSkill !Text !Text
    | ReplHelp (Maybe Text)
    -- ^ @Nothing@ lists every command; @Just@ is a canonical name without @/@.
    | ReplResume (Maybe Text)
    -- ^ @Nothing@ opens the session picker; @Just@ is a session id.
    | ReplSearch !Text
    -- ^ Search persisted conversation turns and open matching sessions.
    | ReplCompact (Maybe Text)
    -- ^ Optional focus note for what to keep while compacting history.
    | ReplClear
      -- ^ Soft-reset live transcript; keep the same session id.
    | ReplNew
      -- ^ Start a fresh persisted session id with empty history.
    | ReplUsage
    | ReplCommandError Text
    deriving (Eq, Show)

data ShellMode
    = ShellGhci
    | ShellBash
    | ShellBoth
    | ShellNone
    deriving (Eq, Show)

data SkillCommand = SkillCommand
    { skillCommandName :: !Text
    , skillCommandSummary :: !Text
    , skillCommandArgumentHint :: !(Maybe Text)
    , skillCommandSource :: !Text
    }
    deriving (Eq, Show)

-- | One REPL slash command. @slashName@ is the canonical name without a
-- leading @/@; aliases are also stored without @/@.
data SlashCommand = SlashCommand
    { slashName :: !Text
    , slashAliases :: ![Text]
    , slashUsage :: !Text
    , slashSummary :: !Text
    , slashTakesArguments :: !Bool
    , slashDialects :: !(Maybe [DialectId])
    , slashRequiredTools :: ![Text]
    }
    deriving (Eq, Show)

-- | Session-scoped slash-command capabilities and presentation data.
--
-- Tool-backed commands are filtered when the catalog is built, so the same
-- catalog must be used for parsing, help, and both completion UIs.
data SlashCatalog = SlashCatalog
    { slashCatalogDialect :: !DialectId
    , slashCatalogToolNames :: !(Set Text)
    , slashCatalogCommands :: ![SlashCommand]
    , slashCatalogSkills :: ![SkillCommand]
    , slashCatalogModelIds :: ![Text]
    }
    deriving (Eq, Show)

slashCommands :: [SlashCommand]
slashCommands =
    [ cmd "help" [] "/help [NAME]" "List slash commands, or describe one" True
    , cmd "model" ["m"] "/model [NAME]" "Open the model picker, or set a model" True
    , cmd "effort" [] "/effort [none|low|medium|high|xhigh|max]" "Show or set reasoning effort" True
    , cmd "plan" [] "/plan [description]" "Enter plan mode (or Shift+Tab)" True
    , cmd "btw" [] "/btw <QUESTION>" "Ask a side question without changing the conversation" True
    , cmd "recap" ["summarize"] "/recap" "Summarize the session so far" False
    , cmd "session" [] "/session" "Print the current session id" False
    , cmd "session-info" ["status", "info"] "/session-info" "Show session details (model, tools, and context usage)" False
    , cmd "afk" [] "/afk [HOST:PATH]" "Move this session into tmux, locally or over SSH" True
    , cmd "worktree" [] "/worktree" "Start a fresh session in a new git worktree" False
    , cmd "rename" ["title"] "/rename <TITLE>|--auto" "Rename the current session, or restore automatic titles" True
    , cmd "login" ["accounts"] "/login" "Manage provider credentials and usage" False
    , cmd "resume" [] "/resume [ID]" "Pick a session to resume, or resume ID" True
    , cmd "search" [] "/search <QUERY>" "Search past conversations and resume a match" True
    , cmd "compact" [] "/compact [FOCUS]" "Summarize history to free context" True
    , cmd "clear" [] "/clear" "Reset the live conversation (same session id)" False
    , cmd "new" [] "/new" "Start a fresh persisted session id" False
    , cmd "usage" [] "/usage" "Show usage, pacing, and reset times for connected accounts" False
    , cmd "reload-auth" [] "/reload-auth" "Re-read provider credentials" False
    , cmd "paste" [] "/paste [--send] [TEXT]" "Attach a clipboard image (Cmd+V / Ctrl+V) and preview it in the terminal" True
    , cmd "attachments" [] "/attachments" "List queued clipboard images" False
    , cmd "clear-attachments" [] "/clear-attachments" "Drop queued clipboard images" False
    , cmd "copy" ["copy-last"] "/copy" "Copy the last assistant response" False
    , cmd "copy-code" [] "/copy-code [N]" "Copy fenced code block N from the last response" True
    , cmd "copy-diff" [] "/copy-diff" "Copy the last diff block" False
    , cmd "copy-path" [] "/copy-path" "Copy the active worktree path" False
    , cmd "copy-session" [] "/copy-session" "Copy the current session id" False
    , cmd "terminal" ["ghostty"] "/terminal" "Show detected terminal capabilities" False
    , cmd "agents" ["a"] "/agents" "Browse the agent hierarchy and switch viewport" False
    , cmd "mcp" ["mcps"] "/mcp" "Manage local MCP servers" False
    , grokToolCmd "scheduler_create"
        "loop" [] "/loop [interval] <prompt>"
        "Run a prompt on a recurring interval" True
    , grokToolCmd "update_goal"
        "goal" [] "/goal <objective> [--budget N] | status | pause | resume | clear"
        "Set, manage, or check an autonomous goal" True
    , grokToolCmd "workflow"
        "workflow" [] "/workflow runs | <name> [input]"
        "Launch a named workflow or list workflow runs" True
    , grokToolCmd "workflow"
        "deep-research" [] "/deep-research <query>"
        "Run bounded background research, cross-check evidence, and write a cited report" True
    , cmd "skills" [] "/skills [reload]" "List discovered skills or reload them from disk" True
    , cmd "shell" [] "/shell [ghci|bash|both|none]" "Show or select the allowed shell tools" True
    , cmd "always-approve" ["yolo"] "/always-approve" "Toggle project auto-approve (or Shift+Tab)" False
    , cmd "quit" ["exit"] "/quit" "Exit the current session" False
    ]
  where
    cmd name aliases usage summary takesArguments =
        SlashCommand
            { slashName = name
            , slashAliases = aliases
            , slashUsage = usage
            , slashSummary = summary
            , slashTakesArguments = takesArguments
            , slashDialects = Nothing
            , slashRequiredTools = []
            }
    grokToolCmd requiredTool name aliases usage summary takesArguments =
        (cmd name aliases usage summary takesArguments)
            { slashDialects = Just [GrokBuildDialect]
            , slashRequiredTools = [requiredTool]
            }

-- | Legacy/default catalog. It intentionally contains only always-on commands;
-- callers with a live session should use 'mkSlashCatalog'.
defaultSlashCatalog :: SlashCatalog
defaultSlashCatalog =
    mkSlashCatalog CodexDialect [] [] []

mkSlashCatalog
    :: DialectId
    -> [Text]
    -> [SkillCommand]
    -> [Text]
    -> SlashCatalog
mkSlashCatalog dialect toolNames skills modelIds =
    let tools =
            Set.fromList
                (map (Text.toLower . Text.strip) toolNames)
    in SlashCatalog
        { slashCatalogDialect = dialect
        , slashCatalogToolNames = tools
        , slashCatalogCommands =
            filter (commandAvailable dialect tools) slashCommands
        , slashCatalogSkills = skills
        , slashCatalogModelIds = modelIds
        }

commandAvailable :: DialectId -> Set Text -> SlashCommand -> Bool
commandAvailable dialect tools command =
    maybe True (dialect `elem`) command.slashDialects
        && all
            ((`Set.member` tools) . Text.toLower)
            command.slashRequiredTools

lookupSlashCommand :: Text -> Maybe SlashCommand
lookupSlashCommand =
    lookupSlashCommandFrom slashCommands

lookupSlashCommandIn :: SlashCatalog -> Text -> Maybe SlashCommand
lookupSlashCommandIn catalog =
    lookupSlashCommandFrom catalog.slashCatalogCommands

lookupSlashCommandFrom :: [SlashCommand] -> Text -> Maybe SlashCommand
lookupSlashCommandFrom commands raw =
    let name = Text.toLower (Text.dropWhile (== '/') (Text.strip raw))
    in find (\cmd -> cmd.slashName == name || name `elem` cmd.slashAliases)
        commands

parseReplLine :: Text -> ReplAction
parseReplLine =
    parseReplLineWithCatalog defaultSlashCatalog

parseReplLineWithSkills :: [SkillCommand] -> Text -> ReplAction
parseReplLineWithSkills skills =
    parseReplLineWithCatalog
        defaultSlashCatalog
            { slashCatalogSkills = skills
            }

parseReplLineWithCatalog :: SlashCatalog -> Text -> ReplAction
parseReplLineWithCatalog catalog raw =
    let line = Text.strip raw
    in if line == ":q" || line == ":quit"
        then ReplQuit
        else if line == ":reload"
            then ReplReload
            else case Text.uncons line of
                Just ('/', _)
                    | looksLikeAbsolutePath line -> ReplPrompt raw
                    | otherwise -> parseSlash catalog raw line
                Just (':', _) -> parseColon raw
                _ -> ReplPrompt raw

-- Absolute paths share slash commands' leading slash. A path with at least
-- one further separator is unambiguously path-like, so leave it as prompt
-- text instead of reporting its first component as an unknown command.
looksLikeAbsolutePath :: Text -> Bool
looksLikeAbsolutePath line =
    case Text.words line of
        first : _ -> Text.any (== '/') (Text.drop 1 first)
        [] -> False

parseColon :: Text -> ReplAction
parseColon raw
    | isAlwaysApproveAlias (Text.drop 1 (Text.strip raw)) = ReplToggleAlwaysApprove
    | otherwise = ReplPrompt raw

parseSlash :: SlashCatalog -> Text -> Text -> ReplAction
parseSlash catalog raw line = case Text.words line of
    [] -> unknownCommand "/"
    command : args -> case lookupSlashCommandIn catalog command of
        Nothing -> case lookupSkillCommand catalog.slashCatalogSkills command of
            Just skill ->
                ReplInvokeSkill
                    skill.skillCommandName
                    (Text.strip (Text.drop (Text.length command) line))
            Nothing -> unknownCommand command
        Just spec -> case spec.slashName of
            "help" -> parseHelpCommand catalog args
            "effort" -> parseEffortCommand args
            "model" -> parseModelCommand args
            "plan" ->
                let description =
                        Text.strip (Text.drop (Text.length command) line)
                in ReplPlan
                    (if Text.null description then Nothing else Just description)
            "btw" ->
                let question =
                        Text.strip (Text.drop (Text.length command) line)
                in if Text.null question
                    then ReplCommandError "usage: /btw <QUESTION>"
                    else ReplBtw question
            "recap" ->
                if null args
                    then ReplRecap
                    else ReplCommandError "usage: /recap"
            "session" ->
                if null args
                    then ReplShowSession
                    else ReplCommandError "usage: /session"
            "session-info" ->
                if null args
                    then ReplShowSessionInfo
                    else ReplCommandError "usage: /session-info"
            "afk" -> case args of
                [] -> ReplAfk Nothing
                [target] -> ReplAfk (Just target)
                _ -> ReplCommandError "usage: /afk [HOST:PATH]"
            "worktree" ->
                if null args
                    then ReplWorktree
                    else ReplCommandError "usage: /worktree"
            "rename" ->
                let title = Text.strip (Text.drop (Text.length command) line)
                in if title == "--auto"
                    then ReplRenameAuto
                    else if Text.null title
                        then ReplCommandError "usage: /rename <TITLE>|--auto"
                        else if Text.length title > 100
                            then ReplCommandError
                                "session titles must be at most 100 characters"
                            else ReplRename title
            "login" ->
                if null args
                    then ReplLogin
                    else ReplCommandError "usage: /login"
            "resume" -> parseResumeCommand args
            "search" ->
                let query =
                        Text.strip (Text.drop (Text.length command) line)
                in if Text.null query
                    then ReplCommandError "usage: /search <QUERY>"
                    else ReplSearch query
            "compact" ->
                let focus =
                        Text.strip (Text.drop (Text.length command) line)
                in ReplCompact
                    (if Text.null focus then Nothing else Just focus)
            "clear" ->
                if null args
                    then ReplClear
                    else ReplCommandError "usage: /clear"
            "new" ->
                if null args
                    then ReplNew
                    else ReplCommandError "usage: /new"
            "usage" ->
                if null args
                    then ReplUsage
                    else ReplCommandError "usage: /usage"
            "reload-auth" ->
                if null args
                    then ReplReloadAuth
                    else ReplCommandError "usage: /reload-auth"
            "paste" ->
                parsePasteCommand (Text.strip (Text.drop (Text.length command) line))
            "attachments" ->
                if null args
                    then ReplShowAttachments
                    else ReplCommandError "usage: /attachments"
            "clear-attachments" ->
                if null args
                    then ReplClearAttachments
                    else ReplCommandError "usage: /clear-attachments"
            "copy" ->
                if null args
                    then ReplCopyLast
                    else ReplCommandError "usage: /copy"
            "copy-code" -> parseCopyCodeCommand args
            "copy-diff" ->
                if null args
                    then ReplCopyDiff
                    else ReplCommandError "usage: /copy-diff"
            "copy-path" ->
                if null args
                    then ReplCopyPath
                    else ReplCommandError "usage: /copy-path"
            "copy-session" ->
                if null args
                    then ReplCopySession
                    else ReplCommandError "usage: /copy-session"
            "terminal" ->
                if null args
                    then ReplShowTerminal
                    else ReplCommandError "usage: /terminal"
            "agents" ->
                if null args
                    then ReplAgents
                    else ReplCommandError "usage: /agents"
            "mcp" ->
                if null args
                    then ReplMcp
                    else ReplCommandError "usage: /mcp"
            "loop" ->
                parseLoopCommand raw
                    (Text.strip (Text.drop (Text.length command) line))
            "goal" ->
                parseGoalCommand raw
                    (Text.strip (Text.drop (Text.length command) line))
            "workflow" ->
                parseWorkflowCommand raw
                    (Text.strip (Text.drop (Text.length command) line))
            "deep-research" ->
                parseDeepResearchCommand raw
                    (Text.strip (Text.drop (Text.length command) line))
            "skills" -> case args of
                [] -> ReplSkills False
                ["reload"] -> ReplSkills True
                _ -> ReplCommandError "usage: /skills [reload]"
            "shell" -> parseShellCommand args
            "always-approve" ->
                if null args
                    then ReplToggleAlwaysApprove
                    else ReplCommandError "usage: /always-approve"
            "quit" ->
                if null args
                    then ReplQuit
                    else ReplCommandError "usage: /quit"
            other -> unknownCommand ("/" <> other)

unknownCommand :: Text -> ReplAction
unknownCommand command =
    ReplCommandError ("unknown command: " <> command <> " (try /help)")

lookupSkillCommand :: [SkillCommand] -> Text -> Maybe SkillCommand
lookupSkillCommand skills raw =
    let name = Text.toLower (Text.dropWhile (== '/') (Text.strip raw))
    in find ((== name) . Text.toLower . (.skillCommandName)) skills

parseHelpCommand :: SlashCatalog -> [Text] -> ReplAction
parseHelpCommand catalog = \case
    [] -> ReplHelp Nothing
    [name] -> case lookupSlashCommandIn catalog name of
        Just spec -> ReplHelp (Just spec.slashName)
        Nothing -> case lookupSkillCommand catalog.slashCatalogSkills name of
            Just skill -> ReplHelp (Just skill.skillCommandName)
            Nothing -> unknownCommand name
    _ -> ReplCommandError "usage: /help [NAME]"

parseLoopCommand :: Text -> Text -> ReplAction
parseLoopCommand original args
    | Text.null args = ReplCommandError loopUsageMessage
    | otherwise =
        ReplExpandedPrompt original (loopScheduleInstruction args)

parseGoalCommand :: Text -> Text -> ReplAction
parseGoalCommand original args =
    case Text.toLower args of
        "" -> ReplGoalStatus
        "status" -> ReplGoalStatus
        "pause" -> ReplGoalPause
        "resume" -> ReplGoalResume
        "clear" -> ReplGoalClear
        _ ->
            case parseTrailingGoalBudget args of
                Left err -> ReplCommandError err
                Right (objective, budget) ->
                    ReplGoalSet
                        original
                        objective
                        budget
                        (goalInstruction objective
                            <> maybe
                                ""
                                (\tokens ->
                                    "\nThe host records an advisory scope budget of "
                                        <> Text.pack (show tokens)
                                        <> " tokens, but does not hard-enforce it. Keep the work proportionate and report if the objective cannot fit.")
                                budget)

parseTrailingGoalBudget :: Text -> Either Text (Text, Maybe Int)
parseTrailingGoalBudget input =
    let trimmed = Text.strip input
        flag = "--budget"
        (throughFlag, afterFlag) = Text.breakOnEnd flag trimmed
        beforeFlag = Text.dropEnd (Text.length flag) throughFlag
        value = Text.strip afterFlag
        ownToken =
            not (Text.null throughFlag)
                && not (Text.null beforeFlag)
                && isSpace (Text.last beforeFlag)
                && not (Text.null afterFlag)
                && isSpace (Text.head afterFlag)
                && not (Text.any isSpace value)
        parsed = case reads (Text.unpack value) of
            [(budget, "")]
                | budget > 0
                , budget <= maxBound ->
                    Just budget
            _ -> Nothing
        objective = Text.stripEnd beforeFlag
        hasBudgetToken = flag `elem` Text.words trimmed
    in case parsed of
        Just budget
            | ownToken
            , not (Text.null objective)
            , Text.all isDigit value ->
                Right (objective, Just budget)
        _
            | hasBudgetToken ->
                Left
                    "usage: /goal <objective> [--budget POSITIVE_INTEGER]"
            | otherwise -> Right (trimmed, Nothing)

parseWorkflowCommand :: Text -> Text -> ReplAction
parseWorkflowCommand original args =
    case Text.words args of
        [] -> ReplWorkflowRuns
        [single]
            | Text.toLower single == "runs" ->
                ReplWorkflowRuns
        first : rest
            | isWorkflowOperation first ->
                ReplWorkflowManage
                    (Text.toLower first)
                    (nonEmptyText
                        (Text.strip
                            (Text.drop (Text.length first) args)))
            | [operation] <- rest
            , isWorkflowOperation operation ->
                ReplWorkflowManage
                    (Text.toLower operation)
                    (Just first)
            | otherwise ->
                let input =
                        Text.strip
                            (Text.drop (Text.length first) args)
                in ReplExpandedPrompt
                    original
                    (workflowInstruction first input)
  where
    isWorkflowOperation value =
        Text.toLower value `elem` ["pause", "resume", "stop", "save"]

parseDeepResearchCommand :: Text -> Text -> ReplAction
parseDeepResearchCommand original query
    | Text.null query =
        ReplCommandError "usage: /deep-research <query>"
    | otherwise =
        ReplExpandedPrompt original (deepResearchInstruction query)

nonEmptyText :: Text -> Maybe Text
nonEmptyText value
    | Text.null value = Nothing
    | otherwise = Just value

loopUsageMessage :: Text
loopUsageMessage =
    Text.unlines
        [ "usage: /loop [interval] <prompt>"
        , "example: /loop 30m check deploy status"
        , "example: /loop check deploy status every hour"
        , ""
        , "Tell me how often it should run (for example 30m, 1 hour, or every 2 days)."
        ]

-- | Canonical model instruction for the detached scheduler implemented by this
-- host. Cadence parsing stays with the model so natural language remains valid.
loopScheduleInstruction :: Text -> Text
loopScheduleInstruction args =
    Text.unlines
        [ "# /loop -- schedule a recurring prompt"
        , ""
        , "Turn the input below into a scheduler_create call."
        , "Each fire runs in a detached background subagent, not in this conversation, so the stored prompt must stand on its own."
        , "Inline every path, job/PR/branch id, status command, success condition, and stop condition that a fresh fire needs."
        , "Keep one fire bounded: it must report a short status and stop rather than polling inline."
        , "The parent session or user owns cancellation; the detached child cannot modify the schedule. Tasks otherwise expire after seven days."
        , ""
        , "Convert the user's cadence, wherever phrased, into a compact <number><unit> interval using s, m, h, or d."
        , "The minimum is 60 seconds. If no cadence is present, ask how often to run and never invent a default."
        , ""
        , "Call scheduler_create with the interval, the self-contained prompt, and fire_immediately: true."
        , "Do not execute the scheduled prompt inline. Confirm the cadence, stop condition, seven-day expiry, and task_id."
        , ""
        , "Input:"
        , args
        ]

-- | Model instruction used after the host has activated goal state.
goalInstruction :: Text -> Text
goalInstruction objective =
    Text.unlines
        [ "# /goal -- pursue an objective"
        , ""
        , "A goal has been set: " <> objective
        , ""
        , "Work directly on this goal and carry it as far as possible. Deliver everything requested without leaving manual steps for the user."
        , "Break the objective into concrete tracked steps and verify changes on the real path as you go."
        , "Call update_goal(completed: true, message: \"summary\") only when the goal is fully achieved."
        , "Call update_goal(blocked_reason: \"reason\") only when truly stuck after at least three consecutive failed attempts at the same problem."
        , "Call update_goal(message: \"status note\") to record useful progress. If update_goal errors, continue and report status in the reply."
        , ""
        , "Start now."
        ]

workflowInstruction :: Text -> Text -> Text
workflowInstruction name input =
    let argsJson =
            decodeUtf8
                (LazyByteString.toStrict
                    (Aeson.encode
                        (Aeson.object ["query" .= input])))
    in Text.unlines
        [ "# /workflow -- launch a named workflow"
        , ""
        , "Call the workflow tool immediately with exactly the name and args below."
        , "The args value is a JSON object whose query field contains the verbatim input; do not omit args or flatten query into a top-level tool argument."
        , "Do not imitate the workflow inline or inspect the workspace before launching it."
        , "If the workflow tool rejects an unsupported option, report that error honestly rather than silently changing semantics."
        , ""
        , "name: " <> name
        , "args: " <> argsJson
        ]

deepResearchInstruction :: Text -> Text
deepResearchInstruction query =
    workflowInstruction "deep-research" query

parseResumeCommand :: [Text] -> ReplAction
parseResumeCommand = \case
    [] -> ReplResume Nothing
    [sessionId]
        | Text.null (Text.strip sessionId) ->
            ReplCommandError "usage: /resume [ID]"
        | otherwise -> ReplResume (Just sessionId)
    _ -> ReplCommandError "usage: /resume [ID]"

parseCopyCodeCommand :: [Text] -> ReplAction
parseCopyCodeCommand = \case
    [] -> ReplCopyCode 1
    [raw] -> case reads (Text.unpack raw) of
        [(n, "")] | n > 0 -> ReplCopyCode n
        _ -> ReplCommandError "usage: /copy-code [N]"
    _ -> ReplCommandError "usage: /copy-code [N]"

isAlwaysApproveAlias :: Text -> Bool
isAlwaysApproveAlias name =
    Text.toLower name `elem` ["always-approve", "yolo"]

-- | @/paste@ queues a clipboard image on the next prompt.
-- @/paste --send [caption]@ sends immediately (old behavior).
parsePasteCommand :: Text -> ReplAction
parsePasteCommand rest =
    let (immediate, caption) = case Text.words rest of
            ("--send":xs) -> (True, Text.unwords xs)
            ("-s":xs) -> (True, Text.unwords xs)
            _ -> (False, rest)
    in ReplPaste immediate (Text.strip caption)

parseEffortCommand :: [Text] -> ReplAction
parseEffortCommand = \case
    [] -> ReplShowEffort
    [level] -> case parseEffort level of
        Right effort -> ReplSetEffort effort
        Left err -> ReplCommandError (Text.pack err)
    _ -> ReplCommandError "usage: /effort [none|low|medium|high|xhigh|max]"

parseShellCommand :: [Text] -> ReplAction
parseShellCommand = \case
    [] -> ReplShowShell
    [raw] -> case Text.toLower raw of
        "ghci" -> ReplSetShell ShellGhci
        "bash" -> ReplSetShell ShellBash
        "both" -> ReplSetShell ShellBoth
        "none" -> ReplSetShell ShellNone
        _ -> ReplCommandError "usage: /shell [ghci|bash|both|none]"
    _ -> ReplCommandError "usage: /shell [ghci|bash|both|none]"

parseModelCommand :: [Text] -> ReplAction
parseModelCommand = \case
    [] -> ReplShowModel
    [name]
        | Text.null (Text.strip name) ->
            ReplCommandError "usage: /model [NAME]"
        | otherwise -> ReplSetModel name
    _ -> ReplCommandError "usage: /model [NAME]"

-- | Rebuild from the constructor: 'input' is also a field on 'CustomToolCall'.
setReasoningEffort :: Text -> ResponseCreateParams -> ResponseCreateParams
setReasoningEffort level ResponseCreateParams{..} =
    ResponseCreateParams
        { reasoning = Just updated
        , ..
        }
  where
    updated = case reasoning of
        Just ReasoningConfig{..} -> ReasoningConfig { effort = Just level, .. }
        Nothing -> ReasoningConfig
            { context = Nothing
            , effort = Just level
            , generateSummary = Nothing
            , reasoningMode = Nothing
            , summary = Nothing
            , extraFields = KeyMap.empty
            }

currentEffort :: ResponseCreateParams -> Text
currentEffort params =
    fromMaybe "low" (params.reasoning >>= (.effort))

-- | Rebuild from the constructor: 'input' is also a field on 'CustomToolCall'.
setModel :: Text -> ResponseCreateParams -> ResponseCreateParams
setModel name ResponseCreateParams{..} =
    ResponseCreateParams
        { model = Just name
        , ..
        }

currentModel :: ResponseCreateParams -> Text
currentModel params =
    fromMaybe "(unset)" params.model

-- | Help text for @/help@ / @/help NAME@.
formatSlashHelp :: Bool -> Maybe Text -> Text
formatSlashHelp color =
    formatSlashHelpWithCatalog color defaultSlashCatalog

formatSlashHelpWithSkills :: Bool -> [SkillCommand] -> Maybe Text -> Text
formatSlashHelpWithSkills color skills =
    formatSlashHelpWithCatalog color
        defaultSlashCatalog
            { slashCatalogSkills = skills
            }

formatSlashHelpWithCatalog
    :: Bool
    -> SlashCatalog
    -> Maybe Text
    -> Text
formatSlashHelpWithCatalog color catalog = \case
    Nothing ->
        Text.intercalate "\n"
            (map (formatSlashHelpRow color) catalog.slashCatalogCommands
                <> map
                    (formatSkillHelpRow color)
                    catalog.slashCatalogSkills)
    Just name ->
        case lookupSlashCommandIn catalog name of
            Just spec -> formatSlashHelpRow color spec
            Nothing -> case lookupSkillCommand catalog.slashCatalogSkills name of
                Just skill -> formatSkillHelpRow color skill
                Nothing -> roleMuted color ("unknown command: " <> name <> " (try /help)")

formatSlashHelpRow :: Bool -> SlashCommand -> Text
formatSlashHelpRow color spec =
    let aliases =
            if null spec.slashAliases
                then ""
                else
                    " ("
                        <> Text.intercalate ", "
                            (map ("/" <>) spec.slashAliases)
                        <> ")"
    in rolePrompt color spec.slashUsage
        <> aliases
        <> "\n  "
        <> roleMuted color spec.slashSummary

formatSkillHelpRow :: Bool -> SkillCommand -> Text
formatSkillHelpRow color skill =
    let usage =
            "/"
                <> skill.skillCommandName
                <> maybe "" (" " <>) skill.skillCommandArgumentHint
    in rolePrompt color usage
        <> "\n  "
        <> roleMuted color
            (skill.skillCommandSummary <> " · skill · " <> skill.skillCommandSource)

-- | Haskeline replacements for the word being completed.
-- @reversedPrev@ is the text before that word, reversed (haskeline's
-- 'completeWordWithPrev' convention). Empty when the buffer is not a slash
-- line.
slashCompletionCandidates :: String -> String -> [String]
slashCompletionCandidates =
    slashCompletionCandidatesWithCatalog defaultSlashCatalog

slashCompletionCandidatesWithModels
    :: [Text]
    -> String
    -> String
    -> [String]
slashCompletionCandidatesWithModels modelIds =
    slashCompletionCandidatesWithCatalog
        defaultSlashCatalog
            { slashCatalogModelIds = modelIds
            }

slashCompletionCandidatesWithSkills
    :: [SkillCommand]
    -> String
    -> String
    -> [String]
slashCompletionCandidatesWithSkills skills =
    slashCompletionCandidatesWithCatalog
        defaultSlashCatalog
            { slashCatalogSkills = skills
            }

slashCompletionCandidatesWithSkillsAndModels
    :: [SkillCommand]
    -> [Text]
    -> String
    -> String
    -> [String]
slashCompletionCandidatesWithSkillsAndModels
        skills modelIds =
    slashCompletionCandidatesWithCatalog
        defaultSlashCatalog
            { slashCatalogSkills = skills
            , slashCatalogModelIds = modelIds
            }

slashCompletionCandidatesWithCatalog
    :: SlashCatalog
    -> String
    -> String
    -> [String]
slashCompletionCandidatesWithCatalog catalog reversedPrev word =
    let prev = reverse reversedPrev
    in if not (isSlashLine prev word)
        then []
        else case words prev of
            [] -> completeSlashNames catalog word
            cmd : _ -> completeSlashArgs catalog cmd word

isSlashLine :: String -> String -> Bool
isSlashLine prev word = case dropWhile isSpace prev of
    [] -> "/" `isPrefixOf` word
    rest -> "/" `isPrefixOf` rest

completeSlashNames :: SlashCatalog -> String -> [String]
completeSlashNames catalog word =
    let needle = Text.toLower (Text.dropWhile (== '/') (Text.pack word))
        names =
            concatMap
                (\cmd -> ("/" <> cmd.slashName) : map ("/" <>) cmd.slashAliases)
                catalog.slashCatalogCommands
        skillNames =
            map
                (("/" <>) . (.skillCommandName))
                catalog.slashCatalogSkills
    in filter (\name -> needle `Text.isPrefixOf` Text.drop 1 (Text.toLower (Text.pack name)))
        (map Text.unpack (names <> skillNames))

completeSlashArgs :: SlashCatalog -> String -> String -> [String]
completeSlashArgs catalog cmd word =
    case lookupSlashCommandIn catalog (Text.pack cmd) of
        Nothing -> []
        Just spec ->
            let needle = Text.toLower (Text.pack word)
                options = argCompletions catalog spec
            in map Text.unpack $
                filter (Text.isPrefixOf needle . Text.toLower) options

argCompletions :: SlashCatalog -> SlashCommand -> [Text]
argCompletions catalog spec = case spec.slashName of
    "effort" -> reasoningEfforts
    "model" -> catalog.slashCatalogModelIds
    "shell" -> ["ghci", "bash", "both", "none"]
    "help" ->
        map (.slashName) catalog.slashCatalogCommands
            <> map (.skillCommandName) catalog.slashCatalogSkills
    "rename" -> ["--auto"]
    "paste" -> ["--send"]
    "goal" -> ["status", "pause", "resume", "clear"]
    "workflow" -> ["runs"]
    _ -> []

-- | One row in the live slash-command dropdown.
data SlashSuggestion = SlashSuggestion
    { slashSuggestionDisplay :: !Text
    , slashSuggestionReplacement :: !Text
    , slashSuggestionSummary :: !Text
    , slashSuggestionTakesArguments :: !Bool
    , slashSuggestionMatchPositions :: ![Int]
    }
    deriving (Eq, Show)

-- | Current live slash menu and the character range replaced on acceptance.
data SlashMenu = SlashMenu
    { slashMenuReplaceStart :: !Int
    , slashMenuReplaceEnd :: !Int
    , slashMenuSuggestions :: ![SlashSuggestion]
    }
    deriving (Eq, Show)

-- | Derive a live menu from a leading slash command at the cursor.
slashMenuFor :: Text -> Int -> Maybe SlashMenu
slashMenuFor =
    slashMenuForCatalog defaultSlashCatalog

slashMenuForWithModels :: [Text] -> Text -> Int -> Maybe SlashMenu
slashMenuForWithModels modelIds =
    slashMenuForCatalog
        defaultSlashCatalog
            { slashCatalogModelIds = modelIds
            }

slashMenuForWithSkills :: [SkillCommand] -> Text -> Int -> Maybe SlashMenu
slashMenuForWithSkills skills =
    slashMenuForCatalog
        defaultSlashCatalog
            { slashCatalogSkills = skills
            }

slashMenuForWithSkillsAndModels
    :: [SkillCommand]
    -> [Text]
    -> Text
    -> Int
    -> Maybe SlashMenu
slashMenuForWithSkillsAndModels skills modelIds =
    slashMenuForCatalog
        defaultSlashCatalog
            { slashCatalogSkills = skills
            , slashCatalogModelIds = modelIds
            }

slashMenuForCatalog
    :: SlashCatalog
    -> Text
    -> Int
    -> Maybe SlashMenu
slashMenuForCatalog catalog text cursor
    | cursor < 1 = Nothing
    | Text.isPrefixOf "/" text =
        let commandToken = Text.takeWhile (not . isSpace) text
            commandEnd = Text.length commandToken
        in if cursor <= commandEnd
            then commandMenu catalog (Text.take cursor text) commandEnd
            else argumentMenu catalog commandToken commandEnd text cursor
    | otherwise =
        skillMentionMenu catalog.slashCatalogSkills text cursor

skillMentionMenu :: [SkillCommand] -> Text -> Int -> Maybe SlashMenu
skillMentionMenu skills text cursor = do
    let before = Text.take cursor text
        token = Text.takeWhileEnd (not . isSpace) before
        replaceStart = cursor - Text.length token
    queryToken <- Text.stripPrefix "$" token
    if Text.any (not . mentionNameChar) queryToken
        then Nothing
        else do
            let query = Text.toLower queryToken
                scored =
                    mapMaybe
                        (\(order, skill) -> do
                            (score, positions) <-
                                fuzzyMatch query
                                    (Text.toLower skill.skillCommandName)
                            pure (score, order, skill, positions))
                        (zip [0 :: Int ..] skills)
                ordered
                    | Text.null query = scored
                    | otherwise =
                        sortOn
                            (\(score, order, _, _) -> (Down score, order))
                            scored
                rows =
                    [ SlashSuggestion
                        { slashSuggestionDisplay =
                            "$" <> skill.skillCommandName
                        , slashSuggestionReplacement =
                            "$" <> skill.skillCommandName <> " "
                        , slashSuggestionSummary =
                            skill.skillCommandSummary
                                <> " · skill · "
                                <> skill.skillCommandSource
                        , slashSuggestionTakesArguments = True
                        , slashSuggestionMatchPositions = map (+ 1) positions
                        }
                    | (_, _, skill, positions) <- ordered
                    ]
                replaceEnd =
                    cursor
                        + Text.length
                            (Text.takeWhile mentionNameChar (Text.drop cursor text))
            if null rows
                then Nothing
                else Just SlashMenu
                    { slashMenuReplaceStart = replaceStart
                    , slashMenuReplaceEnd = replaceEnd
                    , slashMenuSuggestions = rows
                    }
  where
    mentionNameChar char =
        not (isSpace char) && (char == '-' || char == ':' || isAlphaNum char)

commandMenu :: SlashCatalog -> Text -> Int -> Maybe SlashMenu
commandMenu catalog token replaceEnd =
    let query = Text.toLower (Text.drop 1 token)
        commands =
            catalog.slashCatalogCommands
                <> map skillAsSlashCommand catalog.slashCatalogSkills
        scored = mapMaybe (scoreCommand query) (zip [0 :: Int ..] commands)
        ordered
            | Text.null query = scored
            | otherwise = sortOn (\(score, order, _, _) -> (Down score, order)) scored
        rows =
            [ SlashSuggestion
                { slashSuggestionDisplay = "/" <> command.slashName
                , slashSuggestionReplacement =
                    "/" <> command.slashName
                        <> if command.slashTakesArguments then " " else ""
                , slashSuggestionSummary = command.slashSummary
                , slashSuggestionTakesArguments = command.slashTakesArguments
                , slashSuggestionMatchPositions = map (+ 1) positions
                }
            | (_, _, command, positions) <- ordered
            ]
    in if Text.any (== '/') query || null rows
        then Nothing
        else Just SlashMenu
            { slashMenuReplaceStart = 0
            , slashMenuReplaceEnd = replaceEnd
            , slashMenuSuggestions = rows
            }

skillAsSlashCommand :: SkillCommand -> SlashCommand
skillAsSlashCommand skill =
    SlashCommand
        { slashName = skill.skillCommandName
        , slashAliases = []
        , slashUsage =
            "/"
                <> skill.skillCommandName
                <> maybe "" (" " <>) skill.skillCommandArgumentHint
        , slashSummary =
            skill.skillCommandSummary <> " · skill · " <> skill.skillCommandSource
        , slashTakesArguments = True
        , slashDialects = Nothing
        , slashRequiredTools = []
        }

scoreCommand
    :: Text
    -> (Int, SlashCommand)
    -> Maybe (Int, Int, SlashCommand, [Int])
scoreCommand query (order, command)
    | Text.null query = Just (0, order, command, [])
    | otherwise =
        case sortOn (Down . fst) $
            mapMaybe (fuzzyMatch query . Text.toLower)
                (command.slashName : command.slashAliases) of
            [] -> Nothing
            (score, positions) : _ ->
                Just (score, order, command, positions)

argumentMenu :: SlashCatalog -> Text -> Int -> Text -> Int -> Maybe SlashMenu
argumentMenu catalog commandToken commandEnd text cursor = do
    command <- lookupSlashCommandIn catalog commandToken
    let before = Text.take cursor text
        suffix = Text.takeWhileEnd (not . isSpace) before
        argStart = Text.length before - Text.length suffix
        tokenEnd =
            cursor
                + Text.length
                    (Text.takeWhile (not . isSpace) (Text.drop cursor text))
        precedingArgs =
            Text.words
                (Text.take (argStart - commandEnd) (Text.drop commandEnd text))
        options
            | null precedingArgs = argCompletions catalog command
            | otherwise = []
        query = Text.toLower suffix
        ordered = sortOn (\(score, option, _) -> (Down score, option))
            [ (score, option, positions)
            | option <- options
            , Just (score, positions) <- [fuzzyMatch query (Text.toLower option)]
            ]
        rows =
            [ SlashSuggestion
                { slashSuggestionDisplay = option
                , slashSuggestionReplacement = option
                , slashSuggestionSummary = ""
                , slashSuggestionTakesArguments = False
                , slashSuggestionMatchPositions = positions
                }
            | (_, option, positions) <- ordered
            ]
    if null rows
        then Nothing
        else Just SlashMenu
            { slashMenuReplaceStart = argStart
            , slashMenuReplaceEnd = tokenEnd
            , slashMenuSuggestions = rows
            }

-- | Small deterministic fuzzy matcher for the short command catalog.
fuzzyMatch :: Text -> Text -> Maybe (Int, [Int])
fuzzyMatch needle haystack
    | Text.null needle = Just (0, [])
    | needle == haystack =
        Just (10000, [0 .. Text.length needle - 1])
    | needle `Text.isPrefixOf` haystack =
        Just (8000 - Text.length haystack, [0 .. Text.length needle - 1])
    | otherwise = do
        positions@(firstPos:_) <- subsequencePositions needle haystack
        lastPos <- safeLast positions
        let
            gaps = lastPos - firstPos + 1 - length positions
            boundaryBonus =
                sum
                    [ if pos == 0 || Text.index haystack (pos - 1) == '-'
                        then 40
                        else 0
                    | pos <- positions
                    ]
        pure
            ( 4000
                + boundaryBonus
                - firstPos * 10
                - gaps * 20
                - Text.length haystack
            , positions
            )
  where
    safeLast = \case
        [] -> Nothing
        first : rest -> Just (foldl (\_ item -> item) first rest)

subsequencePositions :: Text -> Text -> Maybe [Int]
subsequencePositions needle haystack =
    go 0 (Text.unpack needle) (Text.unpack haystack)
  where
    go _ [] _ = Just []
    go _ _ [] = Nothing
    go index wanted@(n:ns) (h:hs)
        | n == h = (index :) <$> go (index + 1) ns hs
        | otherwise = go (index + 1) wanted hs
