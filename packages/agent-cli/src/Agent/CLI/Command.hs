-- | Interactive REPL slash commands.
module Agent.CLI.Command
    ( CopyRequest(..)
    , ForkRequest(..)
    , ReplAction(..)
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
    , initInstruction
    , lookupSlashCommand
    , lookupSlashCommandIn
    , loopScheduleInstruction
    , mkSlashCatalog
    , slashCatalogWithSkills
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

import Agent.CLI.Options
    ( parseEffort
    , reasoningEffortsForDialect
    )
import Agent.CLI.Command.Types
import Agent.CLI.Command.Catalog (slashCommands)
import Agent.CLI.Command.Instructions
    ( deepResearchInstruction
    , goalInstruction
    , initInstruction
    , loopScheduleInstruction
    , workflowInstruction
    )
import Agent.CLI.Style (roleMuted, rolePrompt)
import Agent.Dialect (DialectId(..))
import Agent.ReasoningEffort
    ( ReasoningEffort(..)
    , parseReasoningEffort
    , reasoningEffortText
    )
import Agent.Responses.Types

import Data.Char (isAlphaNum, isDigit, isSpace)
import Data.List (isPrefixOf, sortOn)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (Down(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

-- | Legacy/default catalog. It intentionally contains only always-on commands;
-- callers with a live session should use 'mkSlashCatalog'.
defaultSlashCatalog :: SlashCatalog
defaultSlashCatalog =
    mkSlashCatalog False CodexDialect [] [] []

mkSlashCatalog
    :: Bool
    -> DialectId
    -> [Text]
    -> [SkillCommand]
    -> [Text]
    -> SlashCatalog
mkSlashCatalog fastMode dialect toolNames skills modelIds =
    let tools =
            Set.fromList
                (map (Text.toLower . Text.strip) toolNames)
        commands =
            filter
                (\command -> command.slashName /= "fast" || fastMode)
                (filter
                    (commandAvailable dialect tools)
                    (map (commandForDialect dialect) slashCommands))
    in SlashCatalog
        { slashCatalogDialect = dialect
        , slashCatalogToolNames = tools
        , slashCatalogCommands = commands
        , slashCatalogCommandByName = indexSlashCommands commands
        , slashCatalogSkills = skills
        , slashCatalogSkillByName = indexSkillCommands skills
        , slashCatalogModelIds = modelIds
        }

commandForDialect :: DialectId -> SlashCommand -> SlashCommand
commandForDialect dialect command
    | command.slashName == "effort" =
        command
            { slashUsage =
                "/effort ["
                    <> Text.intercalate
                        "|"
                        (map reasoningEffortText
                            (reasoningEffortsForDialect dialect))
                    <> "]"
            }
    | otherwise = command

slashCatalogWithSkills :: [SkillCommand] -> SlashCatalog -> SlashCatalog
slashCatalogWithSkills skills catalog =
    catalog
        { slashCatalogSkills = skills
        , slashCatalogSkillByName = indexSkillCommands skills
        }

indexSlashCommands :: [SlashCommand] -> Map Text SlashCommand
indexSlashCommands commands =
    Map.fromList
        [ (name, command)
        | command <- commands
        , name <- command.slashName : command.slashAliases
        ]

indexSkillCommands :: [SkillCommand] -> Map Text SkillCommand
indexSkillCommands skills =
    Map.fromList
        [ (Text.toLower skill.skillCommandName, skill)
        | skill <- skills
        ]

normalizeSlashName :: Text -> Text
normalizeSlashName raw =
    Text.toLower (Text.dropWhile (== '/') (Text.strip raw))

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
lookupSlashCommandIn catalog raw =
    Map.lookup (normalizeSlashName raw) catalog.slashCatalogCommandByName

lookupSlashCommandFrom :: [SlashCommand] -> Text -> Maybe SlashCommand
lookupSlashCommandFrom commands raw =
    Map.lookup (normalizeSlashName raw) (indexSlashCommands commands)

parseReplLine :: Text -> ReplAction
parseReplLine =
    parseReplLineWithCatalog defaultSlashCatalog

parseReplLineWithSkills :: [SkillCommand] -> Text -> ReplAction
parseReplLineWithSkills skills =
    parseReplLineWithCatalog
        (slashCatalogWithSkills skills defaultSlashCatalog)

parseReplLineWithCatalog :: SlashCatalog -> Text -> ReplAction
parseReplLineWithCatalog catalog raw =
    let line = Text.strip raw
    in if isExitAlias line
        then ReplQuit
        else if line == ":reload"
            then ReplReload
            else case Text.uncons line of
                Just ('/', _)
                    | looksLikeAbsolutePath line -> ReplPrompt raw
                    | otherwise -> parseSlash catalog raw line
                Just (':', _) -> parseColon raw
                _ -> ReplPrompt raw

-- | Bare shell- and Vim-style input that exits the interactive session.
-- Keep this exact so ordinary prompts such as @exiting@ still reach the model.
isExitAlias :: Text -> Bool
isExitAlias raw =
    Text.toLower (Text.strip raw)
        `elem` ["exit", "quit", ":q", ":q!", ":quit", ":wq", ":wq!"]

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
        Nothing -> case lookupSkillCommandIn catalog command of
            Just skill ->
                ReplInvokeSkill
                    skill.skillCommandName
                    (Text.strip (Text.drop (Text.length command) line))
            Nothing -> unknownCommand command
        Just spec -> case spec.slashName of
            "help" -> parseHelpCommand catalog args
            "init" ->
                if null args
                    then ReplInit
                    else ReplCommandError "usage: /init"
            "review" ->
                let instructions =
                        Text.strip (Text.drop (Text.length command) line)
                in ReplReview
                    (if Text.null instructions then Nothing else Just instructions)
            "diff" ->
                if null args
                    then ReplDiff
                    else ReplCommandError "usage: /diff"
            "fork" ->
                parseForkCommand
                    (Text.strip (Text.drop (Text.length command) line))
            "export" ->
                let path = Text.strip (Text.drop (Text.length command) line)
                in ReplExport
                    (if Text.null path then Nothing else Just path)
            "history" ->
                if null args
                    then ReplHistory
                    else ReplCommandError "usage: /history"
            "find" ->
                ReplFind
                    (nonEmptyText
                        (Text.strip (Text.drop (Text.length command) line)))
            "permissions" ->
                if null args
                    then ReplPermissions
                    else ReplCommandError "usage: /permissions"
            "effort" -> parseEffortCommand args
            "fast" ->
                if null args
                    then ReplToggleFast
                    else ReplCommandError "usage: /fast"
            "model" -> parseModelCommand args
            "plan" ->
                let description =
                        Text.strip (Text.drop (Text.length command) line)
                in ReplPlan
                    (if Text.null description then Nothing else Just description)
            "view-plan" ->
                if null args
                    then ReplViewPlan
                    else ReplCommandError "usage: /view-plan"
            "queue" ->
                if null args
                    then ReplQueue
                    else ReplCommandError "usage: /queue"
            "transcript" ->
                if null args
                    then ReplTranscript
                    else ReplCommandError "usage: /transcript"
            "edit-prompt" ->
                if null args
                    then ReplEditPrompt
                    else ReplCommandError "usage: /edit-prompt"
            "context" ->
                if null args
                    then ReplContext
                    else ReplCommandError "usage: /context"
            "btw" ->
                let question =
                        Text.strip (Text.drop (Text.length command) line)
                in if Text.null question
                    then ReplCommandError "usage: /btw <QUESTION>"
                    else ReplBtw question
            "meta" ->
                let request =
                        Text.strip (Text.drop (Text.length command) line)
                in if Text.null request
                    then ReplCommandError "usage: /meta <REQUEST>"
                    else ReplMetaConsole request
            "recap" ->
                if null args
                    then ReplRecap
                    else ReplCommandError "usage: /recap"
            "retry" ->
                if null args
                    then ReplRetry
                    else ReplCommandError "usage: /retry"
            "session" ->
                if null args
                    then ReplShowSession
                    else ReplCommandError "usage: /session"
            "session-info" ->
                if null args
                    then ReplShowSessionInfo
                    else ReplCommandError "usage: /session-info"
            "desktop" ->
                if null args
                    then ReplDesktop
                    else ReplCommandError "usage: /desktop"
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
            "home" ->
                if null args
                    then ReplHome
                    else ReplCommandError "usage: /home"
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
            "rewind" ->
                if null args
                    then ReplRewind
                    else ReplCommandError "usage: /rewind"
            "clear" ->
                if null args
                    then ReplClear
                    else ReplCommandError "usage: /clear"
            "new" ->
                if null args
                    then ReplNew
                    else ReplCommandError "usage: /new"
            "delete" ->
                if null args
                    then ReplDelete
                    else ReplCommandError "usage: /delete"
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
                parseCopyCommand
                    (Text.strip (Text.drop (Text.length command) line))
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
            "agents" -> parseAgentsCommand args
            "mcp" -> case args of
                [] -> ReplMcp
                ("prompt" : server : name : rest) ->
                    ReplMcpPrompt server name (map parsePromptArgument rest)
                _ ->
                    ReplCommandError
                        "usage: /mcp [prompt <server> <prompt> [key=value ...]]"
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
            "codemod" ->
                if null args
                    then ReplEnableCodeMode
                    else ReplCommandError "usage: /codemod"
            "always-approve" ->
                if null args
                    then ReplToggleAlwaysApprove
                    else ReplCommandError "usage: /always-approve"
            "update-and-restart" ->
                if null args
                    then ReplUpdateAndRestart
                    else ReplCommandError "usage: /update-and-restart"
            "quit" ->
                if null args
                    then ReplQuit
                    else ReplCommandError "usage: /quit"
            other -> unknownCommand ("/" <> other)

unknownCommand :: Text -> ReplAction
unknownCommand command =
    ReplCommandError ("unknown command: " <> command <> " (try /help)")

lookupSkillCommandIn :: SlashCatalog -> Text -> Maybe SkillCommand
lookupSkillCommandIn catalog raw =
    Map.lookup (normalizeSlashName raw) catalog.slashCatalogSkillByName

parseHelpCommand :: SlashCatalog -> [Text] -> ReplAction
parseHelpCommand catalog = \case
    [] -> ReplHelp Nothing
    [name] -> case lookupSlashCommandIn catalog name of
        Just spec -> ReplHelp (Just spec.slashName)
        Nothing -> case lookupSkillCommandIn catalog name of
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

parseForkCommand :: Text -> ReplAction
parseForkCommand input =
    either ReplCommandError ReplFork (go Nothing (Text.stripStart input))
  where
    go worktree rest
        | Text.null rest =
            Right ForkRequest
                { forkWorktree = worktree
                , forkDirective = Nothing
                }
        | otherwise =
            let (token, suffix) = Text.break isSpace rest
                remaining = Text.stripStart suffix
                finish =
                    Right ForkRequest
                        { forkWorktree = worktree
                        , forkDirective = nonEmptyText (Text.strip rest)
                        }
            in case token of
                "--worktree" -> case worktree of
                    Nothing -> go (Just True) remaining
                    Just True -> Left "--worktree specified twice"
                    Just False ->
                        Left
                            "--worktree and --no-worktree are mutually exclusive"
                "--no-worktree" -> case worktree of
                    Nothing -> go (Just False) remaining
                    Just False -> Left "--no-worktree specified twice"
                    Just True ->
                        Left
                            "--worktree and --no-worktree are mutually exclusive"
                "--at" -> Left "--at is not supported in this version"
                _ -> finish

parseCopyCommand :: Text -> ReplAction
parseCopyCommand input
    | Text.null stripped = copy 1 Nothing
    | Text.all isDigit first =
        case reads (Text.unpack first) :: [(Integer, String)] of
            [(number, "")]
                | number > 0
                , number <= toInteger (maxBound :: Int) ->
                    copy
                        (fromInteger number)
                        (nonEmptyText (Text.stripStart rest))
            _ -> usage
    | otherwise = copy 1 (Just stripped)
  where
    stripped = Text.strip input
    (first, rest) = Text.break isSpace stripped
    copy index destination =
        ReplCopy CopyRequest
            { copyResponseIndex = index
            , copyDestination = destination
            }
    usage =
        ReplCommandError
            "usage: /copy [N] [PATH] where N is 1 (latest), 2, 3, ..."

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

parseAgentsCommand :: [Text] -> ReplAction
parseAgentsCommand = \case
    [] -> ReplAgents
    ["limit"] -> ReplShowAgentLimit
    ["limit", raw] -> case reads (Text.unpack raw) of
        [(n, "")] | n >= 1 -> ReplSetAgentLimit n
        _ -> ReplCommandError "usage: /agents [limit [N]]"
    _ -> ReplCommandError "usage: /agents [limit [N]]"

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
setReasoningEffort
    :: ReasoningEffort
    -> ResponseCreateParams
    -> ResponseCreateParams
setReasoningEffort level ResponseCreateParams{..} =
    ResponseCreateParams
        { reasoning = Just updated
        , ..
        }
  where
    updated = case reasoning of
        Just ReasoningConfig{..} ->
            ReasoningConfig { effort = Just (reasoningEffortText level), .. }
        Nothing -> ReasoningConfig
            { context = Nothing
            , effort = Just (reasoningEffortText level)
            , generateSummary = Nothing
            , reasoningMode = Nothing
            , summary = Nothing
            }

currentEffort :: ResponseCreateParams -> ReasoningEffort
currentEffort params =
    fromMaybe EffortLow $
        params.reasoning
            >>= (.effort)
            >>= either (const Nothing) Just . parseReasoningEffort

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
        (slashCatalogWithSkills skills defaultSlashCatalog)

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
            Nothing -> case lookupSkillCommandIn catalog name of
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
        (slashCatalogWithSkills skills defaultSlashCatalog)

slashCompletionCandidatesWithSkillsAndModels
    :: [SkillCommand]
    -> [Text]
    -> String
    -> String
    -> [String]
slashCompletionCandidatesWithSkillsAndModels
        skills modelIds =
    slashCompletionCandidatesWithCatalog
        ((slashCatalogWithSkills skills defaultSlashCatalog)
            { slashCatalogModelIds = modelIds
            })

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
    "agents" -> ["limit"]
    "effort" ->
        map reasoningEffortText
            (reasoningEffortsForDialect catalog.slashCatalogDialect)
    "model" -> catalog.slashCatalogModelIds
    "shell" -> ["ghci", "bash", "both", "none"]
    "help" ->
        map (.slashName) catalog.slashCatalogCommands
            <> map (.skillCommandName) catalog.slashCatalogSkills
    "rename" -> ["--auto"]
    "fork" -> ["--worktree", "--no-worktree"]
    "paste" -> ["--send"]
    "goal" -> ["status", "pause", "resume", "clear"]
    "workflow" -> ["runs"]
    _ -> []

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
        (slashCatalogWithSkills skills defaultSlashCatalog)

slashMenuForWithSkillsAndModels
    :: [SkillCommand]
    -> [Text]
    -> Text
    -> Int
    -> Maybe SlashMenu
slashMenuForWithSkillsAndModels skills modelIds =
    slashMenuForCatalog
        ((slashCatalogWithSkills skills defaultSlashCatalog)
            { slashCatalogModelIds = modelIds
            })

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

-- | Split a @key=value@ prompt argument; a bare word is a key with an empty
-- value.
parsePromptArgument :: Text -> (Text, Text)
parsePromptArgument argument =
    let (key, value) = Text.breakOn "=" argument
    in (key, Text.drop 1 value)
